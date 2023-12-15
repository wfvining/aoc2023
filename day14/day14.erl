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
    Height = length(hd(Columns)),
    lists:sum([column_weight(Col, Height) || Col <- Rolled]).

part2(Input) ->
    Columns = columns(Input),
    Height = length(hd(Columns)),
    Width = length(Columns),
    Stones = [parse_column(Col) || Col <- Columns],
    L = do_cycles(Stones, Height, Width),
    {PrefixLen, LoopLen} = describe_loop(L),
    {_, Loop} = lists:split(PrefixLen, L),
    N = ((1000000000 - PrefixLen) rem LoopLen) + 1,
    Cfg = lists:nth(N, Loop),
    {_, Circles} = lists:unzip(Cfg),
    lists:sum([column_weight(Col, Height) || Col <- Circles]).

do_cycles(Stones, Height, Width) ->
    {Squares, _} = lists:unzip(Stones),
    Circles = do_cycle(Stones, Height, Width),
    NewStones = lists:zip(Squares, Circles),
    visualize(NewStones, Height, Width),
    do_cycles(
      gb_sets:from_list([Stones]),
      [Stones], NewStones, Height, Width).

do_cycles(Seen, Prev, Stones, Height, Width) ->
    {Squares, _} = lists:unzip(Stones),
    Circles = do_cycle(Stones, Height, Width),
    NewStones = lists:zip(Squares, Circles),
    case gb_sets:is_member(NewStones, Seen) of
        true ->
            lists:reverse([NewStones|Prev]);
        false ->
            visualize(NewStones, Height, Width),
            do_cycles(gb_sets:add_element(NewStones, Seen),
                      [Stones|Prev], NewStones, Height, Width)
    end.

do_cycle(Stones, Height, Width) ->
    {NorthSquares, _} = lists:unzip(Stones),
    NewCircles = [roll_stones(S) || S <- Stones],
    WestCircles = transpose(NewCircles, Height),
    WestSquares = transpose(NorthSquares, Height),
    WestStones = lists:zip(WestSquares, WestCircles),
    WestRolled = [roll_stones(S) || S <- WestStones],
    SouthStones =
        lists:zip(
          flip(NorthSquares, Height),
          flip(transpose(WestRolled, Width), Height)),
    SouthRolled = [roll_stones(S) || S <- SouthStones],
    EastStones =
        lists:zip(
          flip(WestSquares, Width),
          flip(transpose(flip(SouthRolled, Height), Height), Width)),
    EastRolled = [roll_stones(S) || S <- EastStones],
    transpose(flip(EastRolled, Width), Height).

describe_loop(L) ->
    {Prefix, Loop} = lists:split(prefix_len(L), L),
    {length(Prefix), length(Loop)}.

prefix_len(L) ->
    Last = lists:last(L),
    prefix_len(L, Last, 1).

prefix_len([H|_], Last, N) when H =:= Last ->
    N - 1;
prefix_len([_|T], Last, N) ->
    prefix_len(T, Last, N + 1).

visualize(Stones, Height, Width) ->
    {Squares, Circles} = lists:unzip(Stones),
    SquareRows = transpose(Squares, Height),
    CircleRows = transpose(Circles, Height),
    visualize1(SquareRows, CircleRows, Width).

visualize1([], [], _) -> io:format("~n");
visualize1([RSquare|Squares], [RCirc|Circles], Width) ->
    Symbol =
        fun(X) ->
                case {lists:member(X, RSquare),
                      lists:member(X, RCirc)}
                of
                    {true, _} -> io:format("#");
                    {_, true} -> io:format("O");
                    _ -> io:format(".")
                end
        end,
    [Symbol(X) || X <- lists:seq(1, Width)],
    io:format("~n"),
    visualize1(Squares, Circles, Width).

flip(Xss, Size) ->
    [flip_vec(Xs, Size) || Xs <- Xss].

flip_vec(Vec, Size) ->
    lists:sort([Size - X + 1 || X <- Vec]).

transpose(Cols, NumRows) ->
    columns_to_rows(Cols, 1, NumRows).

columns_to_rows([], _, NumRows) ->
    lists:duplicate(NumRows, []);
columns_to_rows([C|Cols], ColNum, NumRows) ->
    Rows = columns_to_rows(Cols, ColNum + 1, NumRows),
    NewRows = column_to_rows(C, ColNum, NumRows),
    lists:zipwith(fun lists:append/2, NewRows, Rows).

column_to_rows(C, ColNum, NumRows) ->
    [case lists:member(N, C) of
         true -> [ColNum];
         false -> []
     end || N <- lists:seq(1, NumRows)].
