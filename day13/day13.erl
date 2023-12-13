-module(day13).

-compile(export_all).

load_input(Path) ->
    {ok, File} = file:open(Path, [read]),
    Input = load_input(File, []),
    file:close(File),
    Input.

load_input(File, Input) ->
    case load_map(File, []) of
        {ok, Map} ->
            load_input(File, [Map|Input]);
        eof ->
            Input
    end.

load_map(File, Map) ->
    case file:read_line(File) of
        eof when length(Map) > 0 ->
            {ok, lists:reverse(Map)};
        eof when length(Map) =:= 0 ->
            eof;
        {ok, "\n"} ->
            {ok, lists:reverse(Map)};
        {ok, Line} ->
            load_map(File, [string:trim(Line)|Map])
    end.

columns([[]|_]) ->
    [];
columns(Map) ->
    Col = [hd(Row) || Row <- Map],
    [Col | columns([tl(Row) || Row <- Map])].

find_symmetry(Map) ->
    Z = [{[hd(R)], tl(R)} || R <- Map],
    find_symmetry1(Z).

find_symmetry1([{_, []}|_]) ->
    [];
find_symmetry1(Z) ->
    F = fun({L1, L2}) when length(L1) =< length(L2) ->
                lists:prefix(L1, L2);
           ({L1, L2}) ->
                lists:prefix(L2, L1)
        end,
    case lists:all(F, Z) of
        true ->
            [Z|find_symmetry1([{[X|L], R} || {L, [X|R]} <- Z])];
        false ->
            find_symmetry1([{[X|L], R} || {L, [X|R]} <- Z])
    end.

find_new_symmertry(Map) ->
    [Original] = symmetries(Map),
    Smudges = smudges(Map),
    Syms = lists:flatten([symmetries(S) || S <- Smudges]),
    [New] = lists:uniq([X || X <- Syms, X =/= Original]),
    New.

symmetries(Map) ->
    RowSym = find_symmetry(Map),
    ColSym = find_symmetry(columns(Map)),
    RowSummary = [length(Before) || [{Before, _}|_] <- RowSym],
    ColSummary = [length(Before) * 100 || [{Before, _}|_] <- ColSym],
    lists:uniq(RowSummary ++ ColSummary).

smudges(Map) ->
    Width = length(hd(Map)),
    Height = length(Map),
    [smudge(Map, X, Y) || X <- lists:seq(1, Width),
                          Y <- lists:seq(1, Height)].

smudge([Row|Rest], X, 1) ->
    [smudge_x(Row, X)|Rest];
smudge([Row|Map], X, Y) ->
    [Row|smudge(Map, X, Y - 1)].

smudge_x([$.|Row], 1) ->
    [$#|Row];
smudge_x([$#|Row], 1) ->
    [$.|Row];
smudge_x([X|Row], N) ->
    [X|smudge_x(Row, N - 1)].

summarize(Input) ->
    RowSym = lists:concat([find_symmetry(Map) || Map <- Input]),
    ColSym = lists:concat([find_symmetry(columns(Map)) || Map <- Input]),
    RowSummary = [length(Before) || [{Before, _}|_] <- RowSym],
    ColSummary = [length(Before) * 100 || [{Before, _}|_] <- ColSym],
    lists:sum(RowSummary) + lists:sum(ColSummary).
