-module(day9).

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
            Line = string:split(string:trim(Data), " ", all),
            Readings = [list_to_integer(I) || I <- Line],
            load_input(File, [Readings|Input])
    end.

extrapolate_sequence(Seq) ->
    Reduced = [lists:reverse(S) || S <- reduce_sequence(Seq)],
    extrapolate(tl(Reduced), 0).

extrapolate_back(Seq) ->
    Reduced = reduce_sequence(Seq),
    extrapolate_back(tl(Reduced), 0).

extrapolate([], X) ->
    X;
extrapolate([[X|_]|Rest], Goal) ->
    extrapolate(Rest, Goal + X).

extrapolate_back([], X) ->
    X;
extrapolate_back([[X|_]|Rest], Goal) ->
    extrapolate_back(Rest, X - Goal).

reduce_sequence(Seq) ->
    reduce_sequence(Seq, []).

reduce_sequence(Seq, Acc) ->
    case lists:all(fun(X) -> X =:= 0 end, Seq) of
        true ->
            [Seq|Acc];
        false ->
            reduce_sequence(reduce(Seq), [Seq|Acc])
    end.

reduce([_]) -> [];
reduce([X, Y|Rest]) ->
    [Y - X | reduce([Y|Rest])].

extrapolate_all(Sequences) ->
    [extrapolate_sequence(Seq) || Seq <- Sequences].

extrapolate_all_back(Sequences) ->
    [extrapolate_back(Seq) || Seq <- Sequences].
