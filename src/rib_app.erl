-module(rib_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case inets:start(httpc, [{profile, rib}]) of
      {ok, _} -> ok;
      {error, {already_started, _}} -> ok
    end,
    {ok, _} = rib_conn_killer_sup:start_link(),
    {ok, _} = rib_metrics:start_link(),
    {ok, _} = rib_sup:start_link().

stop(_State) ->
    inets:stop(httpc, rib),
    ok.
