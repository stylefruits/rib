-module(rib_callback).

%% elli_handler callbacks
-export([handle/2, handle_event/3, auth_fun/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

%% elli_handler callbacks

handle(Req, _Args) ->
    process_flag(trap_exit, true),
    Fun = fun() ->
                  Resp = handle(Req#req.method, elli_request:path(Req), Req),
                  exit({ok, Resp})
          end,
    Child = spawn_link(Fun),
    {Taken, Resp} = timer:tc(fun() ->
                                     receive
                                         {'EXIT', Child, {ok, Resp}} -> Resp;
                                         Err -> error_response(Err)
                                     end
                             end),
    log_response(Taken, Resp),
    with_cors_headers(Resp, Req).

handle_event(Event, _Data, _Args)
  when Event == request_complete;
       Event == request_timeout;
       Event == request_closed -> ok;

handle_event(Event, Data, Args) ->
  error_logger:info_msg("~s:handle_event: ~p~n",
                        [?MODULE, {Event, Data, Args}]).

auth_fun(Req, User, Password) ->
    case elli_request:path(Req) of
        [<<"metrics">>] -> authenticate_metrics(User, Password);
        _ -> ok
    end.

%% Implementation

handle('GET',[<<"health-check">>], _Req) ->
    {200, [], <<"Ok\n">>};

handle('GET',[<<"metrics">>], _Req) ->
    {200, [], rib_metrics:report()};

handle('OPTIONS',[<<"v1">>, <<"batch">>], _Req) ->
    {204, [], <<>>};

handle('POST',[<<"v1">>, <<"batch">>], Req) ->
    Requests = jiffy:decode(elli_request:body(Req), [return_maps]),
    {Taken, {ok, Responses}} = timer:tc(rib_fetch, fetch_all, [Requests]),
    CacheHeaders = lists:map(fun(R) ->
      {AllHeaders} = maps:get(headers, maps:get(response, R)),
      case proplists:get_value(<<"cache-control">>, AllHeaders, undefined) of
          undefined -> undefined;
          Rest -> rib_parse:parse_cache_control(binary_to_list(Rest))
      end
    end, Responses),
    Minimal = rib_parse:minimal_cache(CacheHeaders),
    Cache = rib_parse:format_cache_control(Minimal),
    Data = #{results => Responses, time_taken => Taken / 1000},
    encode_response(Req, Cache, Data);

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

with_cors_headers({Status, Headers, Body}, Req) ->
    Origin = elli_request:get_header(<<"Origin">>, Req, <<"*">>),
    CORSHeaders = [{<<"Access-Control-Allow-Headers">>,
                    <<"Content-Type, Accept-Encoding">>},
                   {<<"Access-Control-Max-Age">>, <<"86400">>},
                   {<<"Access-Control-Allow-Origin">>, Origin},
                   {<<"Access-Control-Allow-Methods">>, <<"POST">>}],
    {Status, Headers ++ CORSHeaders, Body}.

error_response(Err) ->
    {500,
     [{<<"Content-type">>, <<"text/plain; charset=utf-8">>}],
     io_lib:format("~p~n", [Err])}.

encode_response(Req, Cache, Data) ->
    JSON = jiffy:encode(Data),
    Headers = [{<<"Content-Type">>, <<"application/json; charset=utf-8">>}|
               case Cache of
                 undefined -> [];
                 _ -> [{<<"Cache-Control">>, list_to_binary(Cache)}]
               end],
    case accepted_encoding(Req) of
        none -> {200, Headers, JSON};
        gzip -> {200,
                 [{<<"Content-Encoding">>, <<"gzip">>}|Headers],
                 zlib:gzip(JSON)}
    end.

accepted_encoding(Req) ->
    case elli_request:get_header(<<"Accept-Encoding">>, Req, undefined) of
        undefined -> none;
        Header ->
            case lists:member(<<"gzip">>, re:split(Header, ", +")) of
                true  -> gzip;
                false -> none
            end
    end.

authenticate_metrics(User, Password) ->
    {ok, ValidUser} = application:get_env(rib, metrics_user),
    {ok, ValidPass} = application:get_env(rib, metrics_pass),
    case {User, Password} of
        {ValidUser, ValidPass} -> ok;
        _ -> unauthorized
    end.

log_response(TimeTakenUsec, Response) ->
    {Status, _, _} = Response,
    TimeTakenSec = TimeTakenUsec / 1.0e6,
    TimeTakenMsec = TimeTakenUsec / 1.0e3,
    ok = rib_metrics:observe(rib_callback_time_taken_msec,
                             [Status],
                             TimeTakenMsec),
    error_logger:info_report(#{status => Status,
                               elapsed => TimeTakenSec}).
