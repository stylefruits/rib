-module(rib_fetch).
-export([fetch_all/1]).

%% API

fetch_all(Requests) ->
    Limit = application:get_env(rib, max_requests, 64),
    {ok, Limiter} = rib_limiter:start_link([{max, Limit}]),
    Processed = [preprocess(Req) || Req <- Requests],
    Groups = rib_depend:requests_into_groups(Processed),
    FoldFun = fun (Group, History) ->
                      History ++ fetch_concurrently(Group, {History, Limiter})
              end,
    {ok, lists:foldl(FoldFun, [], Groups)}.

%% Implementation

fetch_concurrently(Requests, State) ->
    F = fun(Req) -> fetch(Req, State) end,
    lists:concat(pmap(F, Requests)).

preprocess(Req) ->
    #{<<"method">> := MethodBin, <<"uri">> := Path} = Req,
    Chunks = rib_parse:parse_uri(Path),
    #{chunks  => Chunks,
      parents => chunks_to_parents(Chunks),
      method  => binary_to_http_method(MethodBin),
      name    => maps:get(<<"name">>, Req, <<>>)}.

chunks_to_parents(Chunks) ->
    MaybeParents = [case Chunk of
                        {reference, #{parent := Parent}} -> Parent;
                        _                                -> none
                    end || Chunk <- Chunks],
    [Parent || Parent <- MaybeParents, Parent /= none].

fetch(Req, {History, Limiter}) ->
    #{chunks := Chunks} = Req,
    Paths = maybe_distinct(Req, rib_path:chunks_to_paths(Chunks, History)),
    ok = rib_limiter:subtract(Limiter, length(Paths)),
    pmap(fun(Path) -> fetch_one(Path, Req) end, Paths).

maybe_distinct(#{method := get}, Paths) ->
  lists:usort(Paths);

maybe_distinct(_Req, Paths) ->
  Paths.

fetch_one(RawPath, Req) ->
    #{method := Method, name := Name} = Req,
    {Url, Path} = resolve_path(RawPath),
    RequestArgs = [Method, case Method of
                               get -> {Url, headers()};
                               head -> {Url, headers()};
                               options -> {Url, headers()};
                               post -> {Url, headers(), "", <<>>}
                           end, [], [], rib],
    {Taken, {ok, Resp}} = timer:tc(httpc, request, RequestArgs),
    Body = try
               rib_slurp:slurp_response(Resp)
           catch
               error:Reason -> error(#{request => Req,
                                       raw_path => RawPath,
                                       response => Resp,
                                       reason => Reason})
           end,
    {{_, Status, _}, Headers, _} = Resp,
    BaseResponse = #{status => Status,
                     time_taken => Taken / 1000,
                     headers => headers_to_ejson(Headers)},
    #{request => #{uri => Path,
                   name => Name,
                   method => Method},
      response => case Body of
                      undefined -> BaseResponse;
                      _         -> maps:put(body, Body, BaseResponse)
                  end}.

resolve_path(RawPath) ->
    {ok, Base} = application:get_env(rib, backend),
    Path = binary:bin_to_list(RawPath),
    case string:str(Path, Base) of
        1 -> {Path, binary:part(RawPath, length(Base),
                                byte_size(RawPath) - length(Base))};
        _ -> {Base ++ Path, RawPath}
    end.

headers_to_ejson(Headers) ->
    {[{list_to_binary(Key), list_to_binary(Val)} || {Key, Val} <- Headers]}.

binary_to_http_method(<<"get">>)    -> get;
binary_to_http_method(<<"head">>)   -> head;
binary_to_http_method(<<"options">>)-> options;
binary_to_http_method(<<"post">>)   -> post.

headers() ->
    [{"accept", "application/json"},
     {"user-agent", "rib/" ++ version()},
     {"accept-encoding", "gzip"}].

version() ->
    hd([Version || {rib, _, Version} <- application:loaded_applications()]).

pmap(Function, List) ->
    Parent = self(),
    Work = fun(Elem) ->
                   fun() ->
                           Parent ! {self(), Function(Elem)}
                   end
           end,
    Workers = [spawn_link(Work(Elem)) || Elem <- List],
    [receive {Worker, Resp} -> Resp end || Worker <- Workers].
