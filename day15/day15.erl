-module(day15).

-compile(export_all).

load_input(Path) ->
    {ok, Data} = file:read_file(Path),
    string:split(string:trim(binary_to_list(Data)), ",", all).

part1(Input) ->
    lists:sum([hash(Step) || Step <- Input]).

part2(Input) ->
    Boxes = run_initialization(Input),
    F = fun(BoxNum, Box, Acc) ->
                Acc + focus_power(BoxNum, Box)
        end,
    maps:fold(F, 0, Boxes).

parse_instruction(Str) ->
    RE = "(^[[:alpha:]]+)(-|=\\d+)",
    {match, [Label, Op]} = re:run(Str, RE, [{capture, [1, 2], list}]),
    case Op of
        "-" ->
            {Label, delete};
        "=" ++ FLen ->
            {Label, {install, list_to_integer(FLen)}}
    end.

parse_instructions(Input) ->
    [parse_instruction(I) || I <- Input].

hash(Str) ->
    hash(0, Str).

hash(Val, []) ->
    Val;
hash(Val, [C|Rest]) ->
    Val1 = Val + C,
    Val2 = Val1 * 17,
    Val3 = Val2 rem 256,
    hash(Val3, Rest).

focus_power(BoxNumber, Box) ->
    F = fun({Ix, {_Label, FocalLength}}, Acc) ->
                ((1 + BoxNumber) * Ix * FocalLength) + Acc
        end,
    lists:foldl(F, 0, lists:enumerate(Box)).

run_initialization(Input) ->
    Instructions = parse_instructions(Input),
    run_initialization(Instructions, #{}).

run_initialization([], Boxes) ->
    Boxes;
run_initialization([{Label, delete}|Instructions], Boxes) ->
    BoxNum = hash(Label),
    NewBoxes = remove_lens(BoxNum, Label, Boxes),
    run_initialization(Instructions, NewBoxes);
run_initialization([{Label, {install, FLen}}|Instructions], Boxes) ->
    BoxNum = hash(Label),
    NewBoxes = install_lens(BoxNum, Label, FLen, Boxes),
    run_initialization(Instructions, NewBoxes).

remove_lens(BoxNum, _Label, Boxes)
  when not is_map_key(BoxNum, Boxes) ->
    Boxes;
remove_lens(BoxNum, Label, Boxes) ->
    Box = maps:get(BoxNum, Boxes),
    NewBox = lists:keydelete(Label, 1, Box),
    Boxes#{BoxNum => NewBox}.

install_lens(BoxNum, Label, FLen, Boxes)
  when not is_map_key(BoxNum, Boxes) ->
    Boxes#{BoxNum => [{Label, FLen}]};
install_lens(BoxNum, Label, FLen, Boxes) ->
    Box = maps:get(BoxNum, Boxes),
    case lists:keyfind(Label, 1, Box) of
        false ->
            Boxes#{BoxNum => Box ++ [{Label, FLen}]};
        {_, _} ->
            NewBox = lists:keyreplace(Label, 1, Box, {Label, FLen}),
            Boxes#{BoxNum => NewBox}
    end.
