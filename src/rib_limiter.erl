-module(rib_limiter).

-behaviour(gen_server).

%% API
-export([start_link/1, subtract/2]).

%% gen_server API
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, code_change/3, terminate/2]).

%% API

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

subtract(ServerRef, N) ->
    gen_server:cast(ServerRef, {subtract, N}).

%% gen_server API

init([{max, N}]) ->
    {ok, N}.

handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

handle_call(_Req, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast({subtract, N}, State) ->
    NewState = State - N,
    case NewState < 0 of
        true -> {stop, limit_exceeded, NewState};
        false -> {noreply, NewState}
    end.

terminate(_Arg, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
