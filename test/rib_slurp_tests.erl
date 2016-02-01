-module(rib_slurp_tests).

-import(rib_slurp, [slurp_response/1]).

-include_lib("eunit/include/eunit.hrl").

slurp_response_test_() ->
    Cases = [{{status(),
               [{"Content-type", "application/json"},
                {"Content-Encoding", "gzip"}],
               zlib:gzip(<<"{\"foo\":[1,2,3]}">>)},
              {[{<<"foo">>, [1, 2, 3]}]}},

             {{status(),
               [{"Content-Type", "application/json;charset=UTF-8"}],
               <<"{\"foo\":[1,2,3]}">>},
              {[{<<"foo">>, [1, 2, 3]}]}},

            {{status(),
              [{"Content-Type", "application/json;charset=UTF-8"}],
              ""},
              undefined}],
    [?_assertEqual(Body, slurp_response(Req)) || {Req, Body} <- Cases].

status() -> {x, y, z}.
