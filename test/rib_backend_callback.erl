-module(rib_backend_callback).
-export([handle/2, handle_event/3]).
-behaviour(elli_handler).

%% ===================================================================
%% Test Backend for Integration Tests
%% ===================================================================
handle(_Req, _Args) ->
    {200,
     [{"content-type", "application/json"}],
     jiffy:encode(#{foo => bar})}.

handle_event(_Event, _Data, _Args) ->
    ok.
