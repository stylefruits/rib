-module(rib_path).
-export([chunks_to_paths/2]).

%% API

chunks_to_paths(Chunks, History) ->
    Binaries = [case Chunk of
                    {binary, Bin} -> [Bin];
                    {reference, Ref} -> from_history(Ref, History)
                end || Chunk <- Chunks],
    Fun = fun (Postfixes, Accums) ->
              [<<Accum/binary, Postfix/binary>> || Postfix <- Postfixes,
                                                   Accum <- Accums]
          end,
    lists:foldl(Fun, [<<>>], Binaries).

%% Implementation

from_history(Ref, History) ->
    #{parent := Parent, path := Path} = Ref,
    Entries = [Entry || Entry <- History,
                        Parent == maps:get(name, maps:get(request, Entry))],
    Values = lists:map(fun (Entry) ->
                           #{response := Resp} = Entry,
                           #{body := Body} = Resp,
                           ejsonpath:execute(binary:bin_to_list(Path), Body)
                       end, Entries),
    lists:concat(Values).
