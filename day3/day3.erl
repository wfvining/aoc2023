-module(day3).

-compile(export_all).

load_input(Path) ->
    {ok, File} = file:open(Path, [read]),
    Input = load_schematic(File),
    parse_input(Input).

load_schematic(File) ->
    case file:read_line(File) of
        eof ->
            [];
        {ok, Data} ->
            [string:trim(Data)|load_schematic(File)]
    end.

parse_input(Input) ->
    SymbolCoords = symbol_coords(Input),
    Numbers = numbers(Input),
    {SymbolCoords, Numbers}.

symbol_coords(Input) ->
    symbol_coords(Input, 1, 1, []).

symbol_coords([], _, _, Coords) ->
    Coords;
symbol_coords([[C|RestRow]|RestLines], X, Y, Coords)
  when C =:= $.;
       ((C >= $0) and (C =< $9)) ->
    symbol_coords([RestRow|RestLines], X + 1, Y, Coords);
symbol_coords([[S|RestRow]|RestLines], X, Y, Coords) ->
    symbol_coords([RestRow|RestLines], X + 1, Y, [{{X, Y}, S}|Coords]);
symbol_coords([[]|RestLines], _, Y, Coords) ->
    symbol_coords(RestLines, 1, Y + 1, Coords).

numbers(Input) ->
    numbers(Input, 1, 1, []).

numbers([], _, _, Numbers) ->
    Numbers;
numbers([[]|RestLines], _, Y, Numbers) ->
    numbers(RestLines, 1, Y + 1, Numbers);
numbers([Line|RestLines], X, Y, Numbers) ->
    case lists:splitwith(fun (C) -> C >= $0 andalso C =< $9 end, Line) of
        {[], [_|Rest]} ->
            numbers([Rest|RestLines], X + 1, Y, Numbers);
        {Digits, Rest} ->
            L = length(Digits),
            N = list_to_integer(Digits),
            Coords = {X, X + L - 1, Y},
            numbers([Rest|RestLines], X + L, Y, [{N, Coords}|Numbers])
    end.

part_numbers({SymbolCoords, Numbers}) ->
    %% Filter Numbers by adjacency to Symbols
    F = fun ({_Number, {MinX, MaxX, Y}}) ->
                CheckCoords = check_coords(MinX, MaxX, Y),
                lists:any(
                  fun(Coord) ->
                          lists:keymember(Coord, 1, SymbolCoords)
                  end, CheckCoords)
        end,
    [N || {N, _} <- lists:filter(F, Numbers)].

check_coords(MinX, MaxX, Y) ->
    lists:flatten(
      [{MinX - 1, Y}, {MaxX + 1, Y},
       [{X, Y - 1} || X <- lists:seq(MinX - 1, MaxX + 1)],
       [{X, Y + 1} || X <- lists:seq(MinX - 1, MaxX + 1)]]).

gear_ratios({SymbolCoords, Numbers}) ->
    StarCoords = [Coord || {Coord, C} <- SymbolCoords, C =:= $*],
    NumberBoundaries = [{N, check_coords(MinX, MaxX, Y)}
                        || {N, {MinX, MaxX, Y}} <- Numbers],
    F = fun (Star) ->
                [N || {N, Coords} <- NumberBoundaries,
                      lists:member(Star, Coords)]
        end,
    AdjacentNumbers = [F(Star) || Star <- StarCoords],
    lists:filtermap(
      fun ([X, Y]) ->
              {true, X * Y};
          (_) ->
              false
      end,
      AdjacentNumbers).
