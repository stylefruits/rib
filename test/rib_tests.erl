-module(rib_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Integration tests
%% ===================================================================

integration_test_() ->
    RawTests = [fun test/1,
                fun test_metrics_auth_401/1,
                fun test_metrics_auth_200/1,
                fun test_health_check_200/1,
                fun test_gzip_response/1,
                fun test_options_cors_without_origin/1,
                fun test_options_cors_with_origin/1,
                fun test_post_cors_without_origin/1,
                fun test_post_cors_with_origin/1,
                fun test_domain_violation/1],
    lists:map(fun (Test) ->
                      {setup,
                       fun setup/0,
                       fun teardown/1,
                       fun (TestState) ->
                               [fun () -> Test(TestState) end]
                       end}
              end,
              RawTests).

test(TestState) ->
    Batch = [#{method => get, uri => <<"/">>, name => x},
             #{method => get, uri => binary_backend_uri(TestState, "/")},
             #{method => options, uri => <<"/options">>},
             #{method => head, uri => binary_backend_uri(TestState, "/hd-bar")},
             #{method => head, uri => <<"/hd-{result=x:$.foo}">>},
             #{method => post, uri => <<"/{result=x:$.foo}">>}],
    RequestBody = jiffy:encode(Batch),
    Request = {batch_uri(TestState), [], "application/json", RequestBody},
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

test_domain_violation(TestState) ->
    Batch = [#{method => get, uri => <<"http://fail:47812/">>}],
    RequestBody = jiffy:encode(Batch),
    Request = {batch_uri(TestState), [], "application/json", RequestBody},
    {ok, {{"HTTP/1.1", 500, _}, _, _}} = httpc:request(post, Request, [], []).

test_gzip_response(TestState) ->
    RequestBody = jiffy:encode([]),
    AcceptEncoding = {"accept-encoding", "gzip"},
    Request = {batch_uri(TestState), [AcceptEncoding], "application/json", RequestBody},
    {ok, {{_, 200, _}, _, Body}} = httpc:request(post, Request, [], []),
    zlib:gunzip(Body).

test_options_cors_without_origin(TestState) ->
    Request = {batch_uri(TestState), []},
    {ok, {{_, 204, _}, Headers, _}} = httpc:request(options, Request, [], []),
    [{"connection", "Keep-Alive"}, {"content-length", "0"},
     {"access-control-allow-headers", "Content-Type, Accept-Encoding"},
     {"access-control-max-age", "86400"},
     {"access-control-allow-origin", "*"},
     {"access-control-allow-methods", "POST"}] = Headers.

test_options_cors_with_origin(TestState) ->
    Request = {batch_uri(TestState), [{"origin", "http://example.org"}]},
    {ok, {{_, 204, _}, Headers, _}} = httpc:request(options, Request, [], []),
    [{"connection", "Keep-Alive"}, {"content-length", "0"},
     {"access-control-allow-headers", "Content-Type, Accept-Encoding"},
     {"access-control-max-age", "86400"},
     {"access-control-allow-origin", "http://example.org"},
     {"access-control-allow-methods", "POST"}] = Headers.

test_post_cors_without_origin(TestState) ->
    Request = {batch_uri(TestState), [], "application/json", "[]"},
    {ok, {{_, 200, _}, Headers, _}} = httpc:request(post, Request, [], []),
    HeadersSubset = proplists:delete("content-length", Headers),
    [{"connection", "Keep-Alive"},
     {"content-type","application/json; charset=utf-8"},
     {"access-control-allow-headers", "Content-Type, Accept-Encoding"},
     {"access-control-max-age", "86400"},
     {"access-control-allow-origin", "*"},
     {"access-control-allow-methods", "POST"}] = HeadersSubset.

test_post_cors_with_origin(TestState) ->
    Request = {batch_uri(TestState), [{"origin", "foo"}], "application/json", "[]"},
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

test_metrics_auth_401(TestState) ->
    Request = {metrics_uri(TestState), []},
    {ok, {{_, 401, _}, _, _}} = httpc:request(get, Request, [], []).

test_metrics_auth_200(TestState) ->
    Request = {metrics_uri(TestState), [metrics_auth_header()]},
    {ok, {{_, 200, _}, _, _}} = httpc:request(get, Request, [], []).

test_health_check_200(TestState) ->
    Request = {health_uri(TestState), []},
    {ok, {{_, 200, _}, _, _}} = httpc:request(get, Request, [], []).

%% ===================================================================
%% Setup/Teardown
%% ===================================================================
random_port() ->
    Min = 1024,
    Max = 65534,
    Min + rand:uniform(Max-Min).

format_base_url(Port) ->
    "http://0:" ++ integer_to_list(Port).

uri({_, RibBase, _}, Path) ->
    RibBase ++ Path.

binary_backend_uri({_, _, BackendBase}, Path) ->
    list_to_binary(BackendBase ++ Path).

health_uri(TestState) ->
    uri(TestState, "/health-check").

metrics_uri(TestState) ->
    uri(TestState, "/metrics").

batch_uri(TestState) ->
    uri(TestState, "/v1/batch").

setup() ->
    RibPort = random_port(),
    RibBase = format_base_url(RibPort),
    BackendPort = RibPort + 1,
    BackendBase = format_base_url(BackendPort),
    case application:load(rib) of
        ok -> ok;
        {error, {already_loaded, rib}} -> ok
    end,
    {ok, Pid} = elli:start_link([{callback, rib_backend_callback},
                                 {port, BackendPort},
                                 {reuseaddr, true}]),
    ok = application:set_env(rib, port, RibPort),
    ok = application:set_env(rib, backend, BackendBase),
    ok = rib:start(),
    {Pid, RibBase, BackendBase}.

teardown({Pid, _, _}) ->
    Ref = monitor(process, Pid),
    exit(Pid, normal),
    receive
        {'DOWN', Ref, process, Pid, _Reason} -> ok
    after 1000
          -> error(exit_timeout)
    end,
    ok = application:stop(rib),
    ok = application:unload(rib).
