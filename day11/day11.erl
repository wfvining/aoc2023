-module(day11).

-compile(export_all).

load_input(Path) ->
    {ok, File} = file:open(Path, [read]),
    Img = load_image(File),
    file:close(File),
    Img.

load_image(File) ->
    case file:read_line(File) of
        eof ->
            [];
        {ok, Data} ->
            [string:trim(Data) | load_image(File)]
    end.

expand(Img) ->
    Img1 = expand_columns(Img),
    expand_rows(Img1).

expand_columns(Img) ->
    case lists:all(fun(R) -> R =:= [] end, Img) of
        true ->
            Img;
        false ->
            Col = [hd(R) || R <- Img],
            Img1 = expand_columns([tl(R) || R <- Img]),
            case lists:all(fun(C) -> C =:= $. end, Col) of
                true ->
                    [[$., C | R] || {R, C} <- lists:zip(Img1, Col)];
                false ->
                    [[C | R] || {R, C} <- lists:zip(Img1, Col)]
            end
    end.

expand_rows([]) ->
    [];
expand_rows([Row|Img]) ->
    Img1 = expand_rows(Img),
    case lists:all(fun(C) -> C =:= $. end, Row) of
        true ->
            [lists:duplicate(length(Row), $.), Row | Img1];
        false ->
            [Row | Img1]
    end.

galaxy_coords(Universe) ->
    galaxy_coords(0, 0, Universe).

galaxy_coords(_, _, []) ->
    [];
galaxy_coords(_, Y, [[]|Rest]) ->
    galaxy_coords(0, Y + 1, Rest);
galaxy_coords(X, Y, [[$#|Rest1]|Rest]) ->
    [{X, Y} | galaxy_coords(X + 1, Y, [Rest1|Rest])];
galaxy_coords(X, Y, [[_|Rest1]|Rest]) ->
    galaxy_coords(X + 1, Y, [Rest1|Rest]).

distances(Coords) ->
    [manhattan_distance(C1, C2) || C1 <- Coords, C2 <- Coords, C1 < C2].

manhattan_distance({X1, Y1}, {X2, Y2}) ->
    abs(X1 - X2) + abs(Y1 - Y2).

expand_coords(Coords, ExpansionFactor) ->
    {Xs, Ys} = lists:unzip(Coords),
    MaxX = lists:max(Xs),
    MaxY = lists:max(Ys),
    NewXs = expand_coords(0, MaxX, Xs, ExpansionFactor),
    NewYs = expand_coords(0, MaxY, Ys, ExpansionFactor),
    lists:zip(NewXs, NewYs).

expand_coords(X, Max, Coords, _Factor) when X =:= Max ->
    Coords;
expand_coords(X, Max, Coords, ExpansionFactor) ->
    NewCoords = expand_coords(X + 1, Max, Coords, ExpansionFactor),
    case lists:member(X, NewCoords) of
        true ->
            NewCoords;
        false ->
            [if C > X -> C + ExpansionFactor - 1; C < X -> C end
             || C <- NewCoords]
    end.
