-module(day18).

-compile(export_all).

load_input(Path) ->
    {ok, File} = file:open(Path, [read]),
    DigPlan = load_dig_plan(File),
    file:close(File),
    DigPlan.

load_dig_plan(File) ->
    RE = "(L|U|R|D) (\\d+) \\((.*)\\)",
    case file:read_line(File) of
        eof ->
            [];
        {ok, Data} ->
            {match, [D, N, C]} =
                re:run(Data, RE, [{capture, [1, 2, 3], list}]),
            [{direction(D), list_to_integer(N), C}|load_dig_plan(File)]
    end.

direction("L") -> left;
direction("U") -> up;
direction("R") -> right;
direction("D") -> down.

volume(DigPlan) ->
    Instructions = parse_instructions(DigPlan),
    io:format("~p~n", [Instructions]),
    Perimiter = perimiter(Instructions),
    PerimiterLength = lists:sum([X || {_, X, _} <- Instructions]),
    A = area(Perimiter),
    InteriorPoints = interior(A, PerimiterLength),
    trunc(PerimiterLength + InteriorPoints).

parse_instructions([]) -> [];
parse_instructions([H|Rest]) ->
    [parse_instruction(H)|parse_instructions(Rest)].

parse_instruction({_, _, [_|Instr]}) ->
    {Dist, Dir} = lists:split(5, Instr),
    {parse_dir(Dir), erlang:list_to_integer(Dist, 16), nil}.

parse_dir("0") -> right;
parse_dir("1") -> down;
parse_dir("2") -> left;
parse_dir("3") -> up.

interior(Area, PerimiterLength) ->
    Area - PerimiterLength div 2 + 1.

perimiter(DigPlan) ->
    perimiter(DigPlan, [{0, 0}]).

perimiter([], Perimiter) ->
    Perimiter;
perimiter([{left, N, _}|Rest], [{X, Y}|_] = Perimiter) ->
    perimiter(Rest, [{X - N, Y}|Perimiter]);
perimiter([{up, N, _}|Rest], [{X, Y}|_] = Perimiter) ->
    perimiter(Rest, [{X, Y - N}|Perimiter]);
perimiter([{right, N, _}|Rest], [{X, Y}|_] = Perimiter) ->
    perimiter(Rest, [{X + N, Y}|Perimiter]);
perimiter([{down, N, _}|Rest], [{X, Y}|_] = Perimiter) ->
    perimiter(Rest, [{X, Y + N}|Perimiter]).

area(Points) ->
    area(Points, 0).

area([_], Acc) ->
    abs(Acc div 2);
area([{X1, Y1}, {X2, Y2}|Rest], Acc) ->
    area([{X2, Y2}|Rest], Acc + ((Y1 + Y2) * (X1 - X2))).
