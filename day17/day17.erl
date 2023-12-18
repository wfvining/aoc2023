-module(day17).

-compile(export_all).

load_input(Path) ->
    {ok, File} = file:open(Path, [read]),
    Grid = parse_grid(File),
    file:close(File),
    array:from_list(Grid).

parse_grid(File) ->
    case file:read_line(File) of
        eof ->
            [];
        {ok, Line} ->
            L = [list_to_integer([X]) || X <- string:trim(Line)],
            [array:from_list(L) | parse_grid(File)]
    end.

width(Grid) ->
    array:size(array:get(0, Grid)).

height(Grid) ->
    array:size(Grid).

get(X, Y, Grid) ->
    array:get(X, array:get(Y, Grid)).

get_neighbors({{X, Y}, {DX, DY} = Dir, N}, Grid) when N =/= 0, N < 4 ->
    Coords = [{{X + DX, Y + DY}, Dir, N + 1}],
    filter_legal(Grid, Coords);
get_neighbors({{X, Y}, Dir, N}, Grid) when (N >= 4) and (N < 10); N =:= 0 ->
    Dirs = [{0, 1}, {0, -1}, {1, 0}, {-1, 0}],
    Coords = [{{X + NDX, Y + NDY}, {NDX, NDY},
               if D =/= Dir -> 1; D =:= Dir -> N + 1 end}
              || {NDX, NDY} = D <- Dirs,
                 Dir =/= {-NDX, -NDY}],
    filter_legal(Grid, Coords);
get_neighbors({{X, Y}, Dir, N}, Grid) when N =:= 10 ->
    Dirs = [{0, 1}, {0, -1}, {1, 0}, {-1, 0}],
    Coords = [{{X + NDX, Y + NDY}, {NDX, NDY}, 1}
              || {NDX, NDY} = D <- Dirs,
                 Dir =/= {-NDX, -NDY},
                 Dir =/= D],
    filter_legal(Grid, Coords).

dijkstra(Grid, Source, Target) ->
    Q = pq:insert(0, {Source, {0, 0}, 0}, pq:new()),
    Parents = #{},
    {D, _P} = loop(Grid, Q, Parents, sets:new(), Target),
    D.

loop(Grid, Q, Parents, Seen, Target) ->
    {Dist, {Coord, _, N} = Node, Q1} = pq:take_smallest(Q),
    if
        Target =:= Coord, N >= 4 ->
            {Dist, Parents};
        true ->
            case sets:is_element(Node, Seen) of
                true ->
                    loop(Grid, Q1, Parents, Seen, Target);
                false ->
                    Neighbors = get_neighbors(Node, Grid),
                    LegalCoords =
                        [Neighbor || Neighbor <- Neighbors,
                                     not sets:is_element(Neighbor, Seen)],
                    {Q2, Parents2} =
                        relax_edges(Grid, Node, Dist,
                                    LegalCoords, Q1, Parents),
                    NewSeen = sets:add_element(Node, Seen),
                    loop(Grid, Q2, Parents2, NewSeen, Target)
            end
    end.

relax_edges(Grid, Parent, D, Adj, Q, Parents) ->
    F = fun({{X, Y}, _, _} = Node, {Q1, Parents1}) ->
                Dist = get(X, Y, Grid) + D,
                {pq:insert(Dist, Node, Q1),
                 Parents1#{Node => Parent}}
        end,
    lists:foldl(F, {Q, Parents}, Adj).

filter_legal(Grid, Coords) ->
    Width = width(Grid),
    Height = height(Grid),
    F = fun ({{X, Y}, _, _}) ->
                (X >= 0) and (Y >= 0) and (X < Width) and (Y < Height)
        end,
    lists:filter(F, Coords).
