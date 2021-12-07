-module(day01).
-export([main/1]).

main(Input) ->
    Parsed = [list_to_integer(S) || S <- string:tokens(Input, "\n")],
    io:format("Part 1 ~p~nPart 2 ~p~n", [countDescending1(Parsed), countDescending2(Parsed)]).

countDescending1([A, B | Rest]) when B > A -> 1 + countDescending1([B | Rest]);
countDescending1([_, B | Rest]) -> countDescending1([B | Rest]);
countDescending1(_) -> 0.

countDescending2([A, B, C, D | Rest]) when B+C+D > A+B+C -> 1 + countDescending2([B, C, D | Rest]);
countDescending2([_, B, C, D | Rest]) -> countDescending2([B, C, D | Rest]);
countDescending2(_) -> 0.
