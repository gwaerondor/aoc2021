-module(day07).
-export([main/1]).

main(Input) ->
    Crabs = parse(Input),
    Id = fun(X) -> X end,
    Gauss = fun(X) -> X * (X+1) div 2 end,
    io:format("Part 1: ~p~nPart 2: ~p~n", [run(Id, Crabs), run(Gauss, Crabs)]).

run(F, Crabs) ->
    Range = lists:seq(lists:min(Crabs), lists:max(Crabs)),
    lists:min([lists:sum([F(abs(Crab - Pos)) || Crab <- Crabs]) || Pos <- Range]).

parse(Input) ->
    Tokens = string:tokens(Input, ","),
    [list_to_integer(X) || X <- Tokens].
