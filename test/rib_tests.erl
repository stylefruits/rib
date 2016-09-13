-module(rib_tests).

-behaviour(supervisor).
-behaviour(elli_handler).

%% elli_handler callbacks
-export([handle/2, handle_event/3]).

%% supervisor callbacks
-export([init/1]).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% supervisor callbacks
%% ===================================================================

init([]) ->
    ElliOpts = [{callback, ?MODULE}, {port, 47812}],
    ElliSpec = {
      fancy_http,
      {elli, start_link, [ElliOpts]},
      permanent,
      5000,
      worker,
      [elli]},

    {ok, { {one_for_one, 5, 10}, [ElliSpec]} }.

%% ===================================================================
%% elli_handler callbacks
%% ===================================================================

handle(_Req, _Args) ->
    {200,
     [{"content-type", "application/json"}],
     jiffy:encode(#{foo => bar})}.

handle_event(_Event, _Data, _Args) ->
    ok.

%% ===================================================================
%% Integration tests
%% ===================================================================

integration_test_() ->
    [{setup, fun setup/0, fun teardown/1, fun test/0},
     {setup, fun setup/0, fun teardown/1, fun test_metrics_auth_401/0},
     {setup, fun setup/0, fun teardown/1, fun test_metrics_auth_200/0},
     {setup, fun setup/0, fun teardown/1, fun test_gzip_response/0},
     {setup, fun setup/0, fun teardown/1, fun test_options_cors_without_origin/0},
     {setup, fun setup/0, fun teardown/1, fun test_options_cors_with_origin/0},
     {setup, fun setup/0, fun teardown/1, fun test_post_cors_without_origin/0},
     {setup, fun setup/0, fun teardown/1, fun test_post_cors_with_origin/0},
     {setup, fun setup/0, fun teardown/1, fun test_domain_violation/0}].

test() ->
    Batch = [#{method => get, uri => <<"/">>, name => x},
             #{method => get, uri => <<"http://0:47812/">>},
             #{method => options, uri => <<"/options">>},
             #{method => head, uri => <<"http://0:47812/hd-bar">>},
             #{method => head, uri => <<"/hd-{result=x:$.foo}">>},
             #{method => post, uri => <<"/{result=x:$.foo}">>}],
    RequestBody = jiffy:encode(Batch),
    Request = {uri(), [], "application/json", RequestBody},
    {ok, {{"HTTP/1.1", 200, "OK"}, _, ResponseBody}} = httpc:request(post, Request, [], []),
    #{<<"results">> := Results} = jiffy:decode(ResponseBody, [return_maps]),
    6 = length(Results),
    lists:foreach(fun(#{<<"response">> := Resp, <<"request">> := Req}) ->
                          Body = maps:get(<<"body">>, Resp, undefined),
                          #{<<"method">> := Method, <<"uri">> := Uri} = Req,
                          case Uri of
                              <<"/">>       -> <<"get">>  = Method;
                              <<"/options">>-> <<"options">> = Method;
                              <<"/hd-bar">> -> <<"head">> = Method;
                              _             -> <<"post">> = Method
                          end,
                          case Method of
                              <<"head">> -> undefined = Body;
                              _          -> #{<<"foo">> := <<"bar">>} = Body
                          end
                  end,
                  Results).

test_domain_violation() ->
    Batch = [#{method => get, uri => <<"http://fail:47812/">>}],
    RequestBody = jiffy:encode(Batch),
    Request = {uri(), [], "application/json", RequestBody},
    {ok, {{"HTTP/1.1", 500, _}, _, _}} = httpc:request(post, Request, [], []).

test_gzip_response() ->
    RequestBody = jiffy:encode([]),
    AcceptEncoding = {"accept-encoding", "gzip"},
    Request = {uri(), [AcceptEncoding], "application/json", RequestBody},
    {ok, {{_, 200, _}, _, Body}} = httpc:request(post, Request, [], []),
    zlib:gunzip(Body).

test_options_cors_without_origin() ->
    Request = {uri(), []},
    {ok, {{_, 204, _}, Headers, _}} = httpc:request(options, Request, [], []),
    [{"connection", "Keep-Alive"}, {"content-length", "0"},
     {"access-control-allow-headers", "Content-Type, Accept-Encoding"},
     {"access-control-max-age", "86400"},
     {"access-control-allow-origin", "*"},
     {"access-control-allow-methods", "POST"}] = Headers.

test_options_cors_with_origin() ->
    Request = {uri(), [{"origin", "http://example.org"}]},
    {ok, {{_, 204, _}, Headers, _}} = httpc:request(options, Request, [], []),
    [{"connection", "Keep-Alive"}, {"content-length", "0"},
     {"access-control-allow-headers", "Content-Type, Accept-Encoding"},
     {"access-control-max-age", "86400"},
     {"access-control-allow-origin", "http://example.org"},
     {"access-control-allow-methods", "POST"}] = Headers.

test_post_cors_without_origin() ->
    Request = {uri(), [], "application/json", "[]"},
    {ok, {{_, 200, _}, Headers, _}} = httpc:request(post, Request, [], []),
    HeadersSubset = proplists:delete("content-length", Headers),
    [{"connection", "Keep-Alive"},
     {"content-type","application/json; charset=utf-8"},
     {"access-control-allow-headers", "Content-Type, Accept-Encoding"},
     {"access-control-max-age", "86400"},
     {"access-control-allow-origin", "*"},
     {"access-control-allow-methods", "POST"}] = HeadersSubset.

test_post_cors_with_origin() ->
    Request = {uri(), [{"origin", "foo"}], "application/json", "[]"},
    {ok, {{_, 200, _}, Headers, _}} = httpc:request(post, Request, [], []),
    HeadersSubset = proplists:delete("content-length", Headers),
    [{"connection", "Keep-Alive"},
     {"content-type","application/json; charset=utf-8"},
     {"access-control-allow-headers", "Content-Type, Accept-Encoding"},
     {"access-control-max-age", "86400"},
     {"access-control-allow-origin", "foo"},
     {"access-control-allow-methods", "POST"}] = HeadersSubset.


%% $ echo -n foo:bar | base64
metrics_auth_header() ->
    {"Authorization", "Basic Zm9vOmJhcg=="}.

metrics_uri() -> "http://0:47811/metrics".

test_metrics_auth_401() ->
    Request = {metrics_uri(), []},
    {ok, {{_, 401, _}, _, _}} = httpc:request(get, Request, [], []).

test_metrics_auth_200() ->
    Request = {metrics_uri(), [metrics_auth_header()]},
    {ok, {{_, 200, _}, _, _}} = httpc:request(get, Request, [], []).

uri() -> "http://0:47811/v1/batch".

setup() ->
    case application:load(rib) of
        ok -> ok;
        {error, {already_loaded, rib}} -> ok
    end,
    ok = application:set_env(rib, port, 47811),
    ok = application:set_env(rib, backend, "http://0:47812"),
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    ok = rib:start(),
    Pid.

teardown(Pid) ->
    process_flag(trap_exit, true),
    true = exit(Pid, shutdown),
    receive
        {'EXIT', Pid, _Reason} -> ok
    after 1000 ->
              error(exit_timeout)
    end,
    ok = application:stop(rib).
