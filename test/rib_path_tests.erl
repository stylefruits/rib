-module(rib_path_tests).

-import(rib_path, [chunks_to_paths/2]).

-include_lib("eunit/include/eunit.hrl").

chunks_to_paths_test_() ->
    Cases = [{[{binary, <<"a">>}, {binary, <<"b">>}],
              [],
              [<<"ab">>]},

             {[{reference, #{parent => parent,
                             path => <<"$[*].subj">>}},
               {binary, <<" is ">>},
               {reference, #{parent => parent,
                             path => <<"$[*].adj">>}}],
              [#{request => #{name => parent},
                 response => #{body => [{[{<<"adj">>, <<"good">>}]},
                                        {[{<<"x">>, <<"bad">>}]},
                                        {[{<<"subj">>, <<"all">>}]},
                                        {[{<<"subj">>, <<"everything">>}]},
                                        {[{<<"adj">>, <<"fine">>}]}] }}],
              [<<"all is good">>,
               <<"everything is good">>,
               <<"all is fine">>,
               <<"everything is fine">>]},

             {[{binary, <<"foo ">>},
               {reference, #{parent => req,
                             path => <<"$.x">>}}],
              [#{request => #{name => req},
                 response => #{body => {[{<<"x">>, <<"bar">>}]}}},
               #{request => #{name => req},
                 response => #{body => {[{<<"x">>, <<"baz">>}]}}}],
              [<<"foo bar">>,
               <<"foo baz">>]}],
    [?_assertEqual(Expected, chunks_to_paths(Chunks, History))
     || {Chunks, History, Expected} <- Cases].
