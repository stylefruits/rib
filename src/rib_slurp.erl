-module(rib_slurp).
-export([slurp_response/1]).

%% API

slurp_response({_, _, ""}) ->
    undefined;

slurp_response({{_, _, _}, Headers, Body}) ->
    Decoded = case content_encoding(Headers) of
                  <<"gzip">> -> zlib:gunzip(Body);
                  _ -> Body
              end,
    case hd(binary:split(content_type(Headers), <<";">>)) of
        <<"application/json">> -> jiffy:decode(Decoded, [])
    end.

%% Implementation

content_type(Headers) ->
    header_value(Headers, "content-type").

content_encoding(Headers) ->
    header_value(Headers, "content-encoding").

header_value(Headers, Name) ->
    Found = [Val || {Key, Val} <- Headers, is_same_key(Name, Key)],
    case Found of
        [Val|_] -> binary:list_to_bin(Val);
        _ -> undefined
    end.

is_same_key(Expected, Found) when is_binary(Found) ->
    string:to_lower(binary:bin_to_list(Found)) == Expected;
is_same_key(Expected, Found) when is_list(Found) ->
    string:to_lower(Found) == Expected.
