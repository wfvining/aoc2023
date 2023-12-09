-module(day6).

-compile(export_all).

races() ->
    [{56, 334},
     {71, 1135},
     {79, 1350},
     {99, 2430}].

distances(TotalTime) ->
    [distance(T, TotalTime - T) || T <- lists:seq(0, TotalTime)].

distance(ChargeTime, MovementTime) ->
    ChargeTime * MovementTime.

numwinners({Time, Record}) ->
    length([D || D <- distances(Time), D > Record]).

part_one() ->
    lists:foldl(fun(X, Acc) -> numwinners(X) * Acc end,
                1, races()).
