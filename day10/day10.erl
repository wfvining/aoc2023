-module(day10).

-compile(export_all).

load_input(Path) ->
    {ok, File} = file:open(Path, [read]),
    Array = load_array(File),
    file:close(File),
    Array.

load_array(File) ->
    Rows = [array:from_list(Line) || Line <- lines(File)],
    array:from_list(Rows).

lines(File) ->
    case file:read_line(File) of
        eof ->
            [];
        {ok, Data} ->
            [string:trim(Data) | lines(File)]
    end.

find_start(Array) ->
    find_start(0, 0, Array).

find_start(X, Y, Array) ->
    case get(X, Y, Array) of
        $S ->
            {X, Y};
        _ ->
            RowLen = array:size(array:get(0, Array)),
            NextX = (X + 1) rem RowLen,
            NextY = if NextX < X -> Y + 1; true -> Y end,
            find_start(NextX, NextY, Array)
    end.

get(X, Y, Array) ->
    array:get(X, array:get(Y, Array)).

set(X, Y, Value, Array) ->
    array:set(Y, array:set(X, Value, array:get(Y, Array)), Array).

loop_mask(X, Y, Maze) ->
    Visited =
        array:new(
          array:size(Maze),
          {default, array:new(array:size(array:get(0, Maze)),
                              {default, false})}),
    {NewVisited, {NewX, NewY}} = step(X, Y, Maze, Visited),
    {_, FinalVisited} = loop_length(NewX, NewY, Maze, NewVisited, 1),
    FinalVisited.

extract_loop(LoopMask, Maze) ->
    Width = array:size(array:get(0, Maze)),
    Height = array:size(Maze),
    NewMaze = array:new(Height, {default, array:new(Width, {default, $.})}),
    Coords = [{X, Y} || X <- lists:seq(0, Width - 1),
                        Y <- lists:seq(0, Height - 1)],
    F = fun ({X, Y}, Acc) ->
                case get(X, Y, LoopMask) of
                    true -> set(X, Y, get(X, Y, Maze), Acc);
                    false -> Acc
                end
        end,
    lists:foldl(F, NewMaze, Coords).

count_interior(Loop) ->
    Width = array:size(array:get(0, Loop)),
    Height = array:size(Loop),
    Coords = [{X, Y} || X <- lists:seq(1, Width - 2),
                        Y <- lists:seq(1, Height - 2)],
    Count = fun (true, Acc)  -> Acc + 1;
                (false, Acc) -> Acc
            end,
    lists:foldl(
      Count, 0,
      [is_contained(X, Y, Loop) || {X, Y} <- Coords,
                                   get(X, Y, Loop) =:= $.]).

is_contained(X, Y, Loop) ->
    Coords = [{X1, Y} || X1 <- lists:seq(0, X)],
    CountCrossings = fun({X1, Y1}, Acc) ->
                             case is_crossing(X1, Y1, Loop) of
                                 true -> Acc + 1;
                                 false -> Acc
                             end
                     end,
    Crossings = lists:foldl(CountCrossings, 0, Coords),
    odd(Crossings).

is_crossing(X, Y, Loop) ->
    C = get(X, Y, Loop),
    lists:member(C, "JL|").

odd(X) ->
    (X rem 2) =:= 1.

%% Replace the S tile with its actual value.
replace_start(X, Y, Loop) ->
    Candidates = [set(X, Y, C, Loop) || C <- "-7JLF|"],
    F = fun(Arr) -> num_connections(X, Y, Arr) =:= 2 end,
    hd(lists:filter(F, Candidates)).

num_connections(X, Y, Loop) ->
    Coords = [{X - 1, Y}, {X + 1, Y}, {X, Y - 1}, {X, Y + 1}],
    Connections = [connected(X, Y, X1, Y1, Loop) || {X1, Y1} <- Coords],
    lists:foldl(fun (true, Acc) -> Acc + 1; (_, Acc) -> Acc end,
                0, Connections).

loop_length(X, Y, Maze) ->
    Visited =
        array:new(
          array:size(Maze),
          {default, array:new(array:size(array:get(0, Maze)),
                              {default, false})}),
    {NewVisited, {NewX, NewY}} = step(X, Y, Maze, Visited),
    element(1, loop_length(NewX, NewY, Maze, NewVisited, 1)).

loop_length(X, Y, Maze, Visited, N) ->
    case get(X, Y, Maze) of
        $S ->
            {N, Visited};
        _ ->
            {NewVisited, {NewX, NewY}} = step(X, Y, Maze, Visited),
            loop_length(NewX, NewY, Maze, NewVisited, N + 1)
    end.

step(X, Y, Maze, Visited) ->
    case next_step(X, Y, Maze, Visited) of
        [Next|_] ->
            {set(X, Y, true, Visited), Next};
        [] ->
            {set(X, Y, true, Visited), find_start(Maze)}
    end.

next_step(X, Y, Maze, Visited) ->
    NESW = [{X, Y - 1}, {X + 1, Y}, {X, Y + 1}, {X - 1, Y}],
    [{X1, Y1} || {X1, Y1} <- NESW,
                 (is_valid(X1, Y1, Maze)
                  andalso connected(X, Y, X1, Y1, Maze)
                  andalso not get(X1, Y1, Visited))].

is_valid(X, Y1, Maze) ->
    YSize = array:size(Maze),
    XSize = array:size(array:get(0, Maze)),
    X >= 0 andalso Y1 >= 0 andalso X < YSize andalso X < XSize.

connected(X1, Y1, X2, Y2, Maze) when X1 < X2 ->
    %% Connected to the right
    Src = get(X1, Y1, Maze),
    Dest = get(X2, Y2, Maze),
    case {Src, Dest} of
        {$-, $-} -> true;
        {$-, $7} -> true;
        {$-, $J} -> true;
        {$L, $-} -> true;
        {$L, $7} -> true;
        {$L, $J} -> true;
        {$F, $-} -> true;
        {$F, $7} -> true;
        {$F, $J} -> true;
        {$S, $7} -> true;
        {$S, $J} -> true;
        {$S, $-} -> true;
        {_, _}   -> false
    end;
connected(X1, Y1, X2, Y2, Maze) when X1 > X2 ->
    %% Connected to the left
    Src = get(X1, Y1, Maze),
    Dest = get(X2, Y2, Maze),
    case {Src, Dest} of
        {$-, $-} -> true;
        {$-, $F} -> true;
        {$-, $L} -> true;
        {$J, $-} -> true;
        {$J, $F} -> true;
        {$J, $L} -> true;
        {$7, $-} -> true;
        {$7, $F} -> true;
        {$7, $L} -> true;
        {$S, $-} -> true;
        {$S, $F} -> true;
        {$S, $L} -> true;
        {_, _}   -> false
    end;
connected(X1, Y1, X2, Y2, Maze) when Y1 > Y2 ->
    %% connected up
    Src = get(X1, Y1, Maze),
    Dest = get(X2, Y2, Maze),
    case {Src, Dest} of
        {$|, $|} -> true;
        {$|, $F} -> true;
        {$|, $7} -> true;
        {$J, $|} -> true;
        {$J, $F} -> true;
        {$J, $7} -> true;
        {$L, $|} -> true;
        {$L, $F} -> true;
        {$L, $7} -> true;
        {$S, $|} -> true;
        {$S, $F} -> true;
        {$S, $7} -> true;
        {_, _}   -> false
    end;
connected(X1, Y1, X2, Y2, Maze) when Y1 < Y2 ->
    %% connected down: | F 7
    Src = get(X1, Y1, Maze),
    Dest = get(X2, Y2, Maze),
    case {Src, Dest} of
        {$|, $|} -> true;
        {$|, $J} -> true;
        {$|, $L} -> true;
        {$F, $|} -> true;
        {$F, $J} -> true;
        {$F, $L} -> true;
        {$7, $|} -> true;
        {$7, $J} -> true;
        {$7, $L} -> true;
        {$S, $|} -> true;
        {$S, $J} -> true;
        {$S, $L} -> true;
        {_, _}   -> false
    end.
