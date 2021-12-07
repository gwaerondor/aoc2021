-module(main).
-export([run/1]).

run(Day) ->
    PaddedDay = lists:flatten(io_lib:format("day~2..0B", [Day])),
    Module = list_to_atom(PaddedDay),
    Module:main(read_input(PaddedDay)).

read_input(Day) ->
    Path = lists:flatten(io_lib:format("../input/~s.txt", [Day])),
    case file:read_file(Path) of
        {ok, Bin} ->
            string:trim(binary_to_list(Bin));
        _ ->
            io:format("File ~p not found.~n", [Path]),
            erlang:halt(1)
    end.


