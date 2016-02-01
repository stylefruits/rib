-module(rib_depend_tests).

-import(rib_depend, [requests_into_groups/1]).

-include_lib("eunit/include/eunit.hrl").

requests_into_groups_test_() ->
    Cases = [{[#{name => <<>>, parents => []},
               #{name => <<"a">>, parents => []},
               #{name => <<"b">>, parents => [<<"a">>]},
               #{name => <<>>, parents => [<<"b">>]},
               #{name => <<>>, parents => [<<"b">>, <<"a">>]}],
              [[#{name => <<"a">>, parents => []},
                #{name => <<>>, parents => []}],
               [#{name => <<"b">>, parents => [<<"a">>]}],
               [#{name => <<>>, parents => [<<"b">>, <<"a">>]},
                #{name => <<>>, parents => [<<"b">>]}]]},

             {[#{name => <<"a">>, parents => []}],
              [[#{name => <<"a">>, parents => []}]]},

             {[#{name => <<"a">>, parents => []},
               #{name => <<"b">>, parents => []}],
              [[#{name => <<"b">>, parents => []},
                #{name => <<"a">>, parents => []}]]},

             {[], []}],
    [?_assertEqual(Val, requests_into_groups(Reqs)) || {Reqs, Val} <- Cases].
