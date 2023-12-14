-module(day14).

-compile(export_all).

load_input(Path) ->
    {ok, File} = file:open(Path, [read]),
    Input = load_input(File, []),
    file:close(File),
    Input.

load_input(File, Input) ->
    case file:read_line(File) of
        eof ->
            lists:reverse(Input);
        {ok, Data} ->
            load_input(File, [string:trim(Data) | Input])
    end.

columns([[]|_]) ->
    [];
columns(Rows) ->
    Col = [hd(Row) || Row <- Rows],
    [Col | columns([tl(Row) || Row <- Rows])].

parse_column(Col) ->
    parse_column(Col, {[], []}, 1).

parse_column([], {Squares, Circles}, _) ->
    {lists:reverse(Squares), lists:reverse(Circles)};
parse_column([$.|Rest], Acc, N) ->
    parse_column(Rest, Acc, N + 1);
parse_column([$#|Rest], {Squares, Circles}, N) ->
    parse_column(Rest, {[N|Squares], Circles}, N + 1);
parse_column([$O|Rest], {Squares, Circles}, N) ->
    parse_column(Rest, {Squares, [N|Circles]}, N + 1).

roll_stones({Squares, Circles}) ->
    roll_stones(Squares, Circles, 1).

roll_stones(_Squares, [], _NextSlot) ->
    [];
roll_stones([], Circles, NextSlot) ->
    lists:seq(NextSlot, NextSlot + length(Circles) - 1);
roll_stones([S|_] = Squares, [C|MoreCircles], NextSlot)
  when C < S ->
    [NextSlot | roll_stones(Squares, MoreCircles, NextSlot + 1)];
roll_stones([S|MoreSquares], [C|_] = Circles, _NextSlot)
  when C > S ->
    roll_stones(MoreSquares, Circles, S + 1).

column_weight(Column, ColLen) ->
    lists:sum([ColLen - C + 1 || C <- Column]).

total_load(Input) ->
    Columns = columns(Input),
    Stones = [parse_column(Col) || Col <- Columns],
    Rolled = [roll_stones(S) || S <- Stones],
    lists:sum([column_weight(Col, length(hd(Columns))) || Col <- Rolled]).
