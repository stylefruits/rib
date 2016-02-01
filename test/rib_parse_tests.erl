-module(rib_parse_tests).

-include_lib("eunit/include/eunit.hrl").

parse_uri_test_() ->
    Cases = [{<<"/foo">>,
              [{binary, <<"/foo">>}]},

             {<<"/foo/{result=prev:$.data.id}-bar">>,
              [{binary, <<"/foo/">>},
               {reference, #{parent => <<"prev">>,
                             path => <<"$.data.id">>}},
               {binary, <<"-bar">>}]},

             {<<"foo{result=prev:$.data.id}bar{result=next:$.error}">>,
              [{binary, <<"foo">>},
               {reference, #{parent => <<"prev">>,
                             path => <<"$.data.id">>}},
               {binary, <<"bar">>},
               {reference, #{parent => <<"next">>,
                             path => <<"$.error">>}}]}],
    [?_assertEqual(Value, rib_parse:parse_uri(Arg)) || {Arg, Value} <- Cases].
