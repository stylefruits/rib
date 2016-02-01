-module(rib_limiter_tests).

-include_lib("eunit/include/eunit.hrl").

within_limits_test_() ->
    {foreach,
     setup(3),
     fun teardown/1,
     [subtract(Nums) || Nums <- [[3], [1, 2], [1, 1], [1, 1, 1]]]}.

limits_exceeded_test_() ->
    {foreach,
     setup(3, limit_exceeded),
     fun teardown/1,
     [subtract(Nums) || Nums <- [[4], [2, 2], [1, 1, 1, 1], [3, 2]]]}.

setup(Max) ->
    fun() ->
            process_flag(trap_exit, true),
            {ok, Pid} = rib_limiter:start_link([{max, Max}]),
            true = register(?MODULE, Pid)
    end.

setup(Max, limit_exceeded) ->
    Setup = setup(Max),
    fun() ->
            Setup(),
            handle_exit
    end.

teardown(handle_exit) ->
    receive
        {'EXIT', _Pid, limit_exceeded} -> ok
    end;

teardown(_) ->
    true = unregister(?MODULE).

subtract(Numbers) ->
    fun() ->
            Pid = whereis(?MODULE),
            [ok = rib_limiter:subtract(Pid, N) || N <- Numbers]
    end.
