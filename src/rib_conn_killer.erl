-module(rib_conn_killer).

%% API
-export([start_link/0]).

%% API functions

start_link() ->
    {ok, spawn_link(fun go/0)}.

%% Implementation

go() ->
    {ok, Base} = application:get_env(rib, backend),
    Url = Base ++ "/",
    Headers = [{"connection", "close"}],
    {ok, _} = httpc:request(get, {Url, Headers}, [], [], rib),
    receive
    after
        30000 -> go()
    end.
