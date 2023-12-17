-module(day16).

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
            [array:from_list(string:trim(Line)) | parse_grid(File)]
    end.

energized(Grid, Start) ->
    {Seen, _} = follow_beam(Grid, Start, sets:new(), sets:new()),
    sets:size(Seen).

mazimize_energy(Grid) ->
    Width = array:size(array:get(0, Grid)),
    Height = array:size(Grid),
    Top = [{{X, 0}, down} || X <- lists:seq(0, Width - 1)],
    Left = [{{0, Y}, right} || Y <- lists:seq(0, Height - 1)],
    Bottom = [{{X, Height - 1}, up} || X <- lists:seq(0, Width - 1)],
    Right = [{{Width - 1, Y}, left} || Y <- lists:seq(0, Height - 1)],
    lists:max([energized(Grid, Start) || Start <- Top ++ Left ++ Bottom ++ Right]).

get(X, Y, Arr) ->
    array:get(X, array:get(Y, Arr)).

off_grid({X, Y}, Grid) ->
    (X < 0)
        or (Y < 0)
        or (X >= array:size(array:get(0, Grid)))
        or (Y >= array:size(Grid)).

follow_beam(Grid, {Coord, Dir} = Pos, Seen, SeenWithDir) ->
    case off_grid(Coord, Grid) or sets:is_element(Pos, SeenWithDir)
    of
        true ->
            {Seen, SeenWithDir};
        false ->
            NewSeen = sets:add_element(Coord, Seen),
            NewSeenWithDir = sets:add_element(Pos, SeenWithDir),
            move_beam(Grid, Coord, Dir, NewSeen, NewSeenWithDir)
    end.

move_beam(Grid, {X, Y}, Dir, Seen, SeenWithDir) ->
    Cell = get(X, Y, Grid),
    case {Cell, Dir} of
        {$., right} ->
            follow_beam(Grid, {{X + 1, Y}, Dir}, Seen, SeenWithDir);
        {$., left} ->
            follow_beam(Grid, {{X - 1, Y}, Dir}, Seen, SeenWithDir);
        {$., up} ->
            follow_beam(Grid, {{X, Y - 1}, Dir}, Seen, SeenWithDir);
        {$., down} ->
            follow_beam(Grid, {{X, Y + 1}, Dir}, Seen, SeenWithDir);

        {$/, right} ->
            follow_beam(Grid, {{X, Y - 1}, up}, Seen, SeenWithDir);
        {$/, left} ->
            follow_beam(Grid, {{X, Y + 1}, down}, Seen, SeenWithDir);
        {$/, up} ->
            follow_beam(Grid, {{X + 1, Y}, right}, Seen, SeenWithDir);
        {$/, down} ->
            follow_beam(Grid, {{X - 1, Y}, left}, Seen, SeenWithDir);

        {$\\, right} ->
            follow_beam(Grid, {{X, Y + 1}, down}, Seen, SeenWithDir);
        {$\\, left} ->
            follow_beam(Grid, {{X, Y - 1}, up}, Seen, SeenWithDir);
        {$\\, up} ->
            follow_beam(Grid, {{X - 1, Y}, left}, Seen, SeenWithDir);
        {$\\, down} ->
            follow_beam(Grid, {{X + 1, Y}, right}, Seen, SeenWithDir);

        {$-, right} ->
            follow_beam(Grid, {{X + 1, Y}, right}, Seen, SeenWithDir);
        {$-, left} ->
            follow_beam(Grid, {{X - 1, Y}, left}, Seen, SeenWithDir);
        {$-, D} when D =:= up; D =:= down ->
            {Seen1, SeenWithDir1} = follow_beam(Grid, {{X - 1, Y}, left}, Seen, SeenWithDir),
            follow_beam(Grid, {{X + 1, Y}, right}, Seen1, SeenWithDir1);

        {$|, D} when D =:= left; D =:= right ->
            {Seen1, SeenWithDir1} = follow_beam(Grid, {{X, Y - 1}, up}, Seen, SeenWithDir),
            follow_beam(Grid, {{X, Y + 1}, down}, Seen1, SeenWithDir1);
        {$|, up} ->
            follow_beam(Grid, {{X, Y - 1}, up}, Seen, SeenWithDir);
        {$|, down} ->
            follow_beam(Grid, {{X, Y + 1}, down}, Seen, SeenWithDir)
    end.
