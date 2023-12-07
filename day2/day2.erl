-module(day2).

-export([load_input/1, game_possible/2, part2/1]).

load_input(Path) ->
    {ok, File} = file:open(Path, [read]),
    load_input(File, []).

load_input(File, Input) ->
    case file:read_line(File) of
        eof ->
            Input;
        {ok, Data} ->
            load_input(File, [parse_game(Data)|Input])
    end.

parse_game(GameStr) ->
    GameNumber = game_number(GameStr),
    Rounds = parse_rounds(GameStr),
    {GameNumber, Rounds}.

game_number("Game " ++ Rest) ->
    [Number|_] = string:split(Rest, ":"),
    list_to_integer(Number).

parse_rounds(GameStr) ->
    [_|Rounds] = string:split(GameStr, ":"),
    AllRounds = string:split(Rounds, ";", all),
    [parse_round(Round) || Round <- AllRounds].

parse_round(RoundStr) ->
    Selected = string:split(RoundStr, ",", all),
    [parse_selection(Selection) || Selection <- Selected].

parse_selection(Selection) ->
    S = string:trim(Selection),
    [N, Color] = string:split(S, " "),
    {list_to_atom(Color), list_to_integer(N)}.

round_possible(Bag, Round) ->
    lists:all(
      fun({Color, N}) ->
              Max = proplists:get_value(Color, Bag, 0),
              N =< Max
      end, Round).

game_possible(Bag, {_, Rounds}) ->
    lists:all(fun (Round) -> round_possible(Bag, Round) end, Rounds).

part2(Games) ->
    MinSets = [min_set(Game) || Game <- Games],
    lists:sum([power(Set) || Set <- MinSets]).

min_set({_, Rounds}) ->
    AllDraws = lists:concat(Rounds),
    Sorted = lists:reverse(lists:keysort(2, AllDraws)),
    {proplists:get_value(red, Sorted, 0),
     proplists:get_value(green, Sorted, 0),
     proplists:get_value(blue, Sorted, 0)}.

power({X, Y, Z}) ->
    X * Y * Z.
