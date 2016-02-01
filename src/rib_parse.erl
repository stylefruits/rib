-module(rib_parse).
-export([parse_uri/1]).

%% API

parse_uri(Uri) ->
    case re:run(Uri, "{result=([^}]*)}", [global]) of
        nomatch          -> split_uri(Uri, []);
        {match, Matches} -> split_uri(Uri, Matches)
    end.

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
