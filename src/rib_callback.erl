-module(rib_callback).

%% elli_handler callbacks
-export([handle/2, handle_event/3]).

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
    {Status, _, _} = Resp,
    error_logger:info_report(#{status => Status,
                               elapsed => Taken / 1.0e6}),
    Resp.

handle_event(Event, _Data, _Args)
  when Event == request_complete;
       Event == request_timeout;
       Event == request_closed -> ok;

handle_event(Event, Data, Args) ->
  error_logger:info_msg("~s:handle_event: ~p~n",
                        [?MODULE, {Event, Data, Args}]).

%% Implementation

handle('POST',[<<"v1">>, <<"batch">>], Req) ->
    Requests = jiffy:decode(elli_request:body(Req), [return_maps]),
    {Taken, {ok, Responses}} = timer:tc(rib_fetch, fetch_all, [Requests]),
    Data = #{results => Responses, time_taken => Taken / 1000},
    encode_response(Req, Data);

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

error_response(Err) ->
    {500,
     [{<<"Content-type">>, <<"text/plain; charset=utf-8">>}],
     io_lib:format("~p~n", [Err])}.

encode_response(Req, Data) ->
    JSON = jiffy:encode(Data),
    Headers = [{<<"Content-Type">>, <<"application/json; charset=utf-8">>}],
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
