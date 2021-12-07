-module(day06).
-export([main/1]).

main(Input) ->
    Init = parse(Input),
    io:format("Part 1: ~p~nPart 2: ~p~n", [run(Init, 80), run(Init, 256)]).

run(Fish, 0) -> lists:sum(maps:values(Fish));
run(Fish, Days) -> run(tick(Fish), Days - 1).

tick(Fish) ->
    Spawning = maps:get(0, Fish, 0),
    Decremented = lists:foldl(fun shift_down/2, Fish, lists:seq(1, 8)),
    Decremented#{8 => Spawning,
                 6 => maps:get(6, Decremented, 0) + Spawning}.

shift_down(Ix, Map) ->
    Map#{Ix - 1 => maps:get(Ix, Map, 0)}.

parse(Input) ->
    freq([list_to_integer(Token) || Token <- string:tokens(Input, ",")]).

freq(Fish) ->
    lists:foldl(fun increment_at/2, #{}, Fish).

increment_at(Ix, Map) ->
    maps:update_with(Ix, fun(N) -> N + 1 end, 1, Map).
