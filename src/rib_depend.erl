-module(rib_depend).
-export([requests_into_groups/1]).

%% API

requests_into_groups(Reqs) ->
    requests_into_groups(Reqs, []).

%% Implementation

requests_into_groups([], _) ->
    [];

requests_into_groups(Reqs, FetchedNames) ->
    {Satisfied, Unsatisfied} = group_by_satisfaction(Reqs, FetchedNames),
    AllNames = FetchedNames ++ [maps:get(name, Req) || Req <- Satisfied],
    case Satisfied of
        [] -> error(badarg);
        _  -> [Satisfied|requests_into_groups(Unsatisfied, AllNames)]
    end.

group_by_satisfaction(Reqs, FetchedNames) ->
    lists:foldl(fun (Req, {Sat, Unsat}) ->
                    case satisfied(Req, FetchedNames) of
                        true  -> {[Req|Sat], Unsat};
                        false -> {Sat, [Req|Unsat]}
                    end
                end,
                {[], []},
                Reqs).

satisfied(Req, Names) ->
    #{parents := Parents} = Req,
    lists:all(fun(Parent) -> lists:member(Parent, Names) end, Parents).
