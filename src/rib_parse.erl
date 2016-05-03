-module(rib_parse).
-export([parse_uri/1, parse_cache_control/1, minimal_cache/1, format_cache_control/1]).

%% API

parse_uri(Uri) ->
    case re:run(Uri, "{result=([^}]*)}", [global]) of
        nomatch          -> split_uri(Uri, []);
        {match, Matches} -> split_uri(Uri, Matches)
    end.

parse_cache_control(Value) ->
    {ok, R} = re:compile("(\".*?\"|[^\",\\s]+)(?=\\s*,|\\s*$)"),
    {match, Matches} = re:run(Value, R, [{capture,[1],list},global]),
    lists:map(fun([S]) ->
      V = re:split(S, "=", [{return,list}]),
      elem_to_prop(V)
    end, Matches).

format_cache_control(Value) ->
    Private = proplists:get_value(private, Value, false),
    Public = proplists:get_value(public, Value, false),
    MaxAge = proplists:get_value('max-age', Value),
    Privacy = case {Public, Private} of
      {_, true} -> "private";
      _ -> "public"
    end,
    lists:concat([Privacy, ", max-age=", MaxAge]).

minimal_cache(Headers) ->
    lists:foldl(fun(Elem, AccIn) ->
        case Elem of
          undefined -> undefined;
          Elem ->
            PrivateNew = proplists:get_value(private, Elem, false),
            PrivateOld = proplists:get_value(private, AccIn, false),
            MaxAgeNew = proplists:get_value('max-age', Elem, "3600"),
            MaxAgeOld = proplists:get_value('max-age', AccIn, "3600"),
            MaxAge = minimal_max_age(MaxAgeOld, MaxAgeNew),
            case {PrivateNew, PrivateOld} of
              {false, false} ->  [{public, true}, {'max-age', MaxAge}];
              _ -> [{private, true}, {'max-age', MaxAge}]
            end
         end
      end,
      [{public, true},
       {'max-age', "3600"}],
      Headers).

minimal_max_age(Age1, Age2) ->
    {A1, []} = string:to_integer(Age1),
    {A2, []} = string:to_integer(Age2),
    integer_to_list(min(A1, A2)).

%% Implementation

split_uri(Uri, Matches) ->
    split_uri(Uri, Matches, 0, []).

split_uri(Uri, [Match|Matches], Offset, Chunks) ->
    [{S1, L1}, {S2, L2}] = Match,
    Fragment = binary:part(Uri, S2, L2),
    [Parent, Path] = re:split(Fragment, ":"),
    Binary = {binary, binary:part(Uri, Offset, S1 - Offset)},
    Reference = {reference, #{parent => Parent,
                              path => Path}},
    split_uri(Uri, Matches, S1 + L1, Chunks ++ [Binary, Reference]);

split_uri(Uri, [], Offset, Chunks) ->
    Size = size(Uri),
    case Size - Offset of
        0   -> Chunks;
        Rem -> Chunks ++ [{binary, binary:part(Uri, Size, -Rem)}]
    end.

elem_to_prop([Key]) -> {list_to_atom(Key), true};
elem_to_prop([Key|[Value]]) -> {list_to_atom(Key), Value}.
