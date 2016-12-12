-module(rib_conn_killer).

%% API
-export([start_link/0]).

%% API functions

start_link() ->
    {ok, spawn_link(fun go/0)}.

%% Implementation

go() ->
    {ok, Base} = application:get_env(rib, backend),
    TimeoutSecs = application:get_env(rib, timeout, 10),

    Url = Base ++ "/",
    Headers = [{"connection", "close"}],
    Opts = [{timeout, timer:seconds(TimeoutSecs)}],
    {ok, _} = httpc:request(get, {Url, Headers}, Opts, [], rib),
    receive
    after
        30000 -> go()
    end.
