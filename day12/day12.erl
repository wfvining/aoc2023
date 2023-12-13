-module(day12).

-compile(export_all).

load_input(Path) ->
    {ok, File} = file:open(Path, [read]),
    Input = load_input(File, []),
    file:close(File),
    Input.

load_input(File, Input) ->
    case file:read_line(File) of
        eof ->
            Input;
        {ok, Data} ->
            I = parse_input(string:trim(Data)),
            load_input(File, [I|Input])
    end.

parse_input(Input) ->
    [Pattern, Counts] = string:split(Input, " ", all),
    {Pattern, parse_numbers(Counts)}.

parse_numbers(Numbers) ->
    Ns = string:split(Numbers, ",", all),
    [list_to_integer(N) || N <- Ns].

count_options(Input) ->
    F = fun ({Str, CheckSum} = I, Acc) ->
                ValidOptions = valid_options(Str, CheckSum),
                Acc + if length(ValidOptions) =:= 0 ->
                              io:format("~p~n", [I]),
                              0;
                         true ->
                              length(ValidOptions)
                      end
        end,
    lists:foldl(F, 0, Input).

valid_options(Str, CheckSum) ->
    Valid = fun (Row) -> is_valid(Row, CheckSum) end,
    lists:filter(Valid, expand(Str)).

expand([]) ->
    [[]];
expand([$?|Rest]) ->
    Suffixes = expand(Rest),
    [[$#|S] || S <- Suffixes] ++ [[$.|S] || S <- Suffixes];
expand([C|Rest]) ->
    Suffixes = expand(Rest),
    [[C|S] || S <- Suffixes].

is_valid(Row, Check) ->
    Check1 = checksum(Row),
    Check1 =:= Check.

checksum(Row) ->
    checksum(Row, 0, []).

checksum([$#|Rest], N, Counts) ->
    checksum(Rest, N + 1, Counts);
checksum([$.|Rest], N, Counts) when N > 0 ->
    checksum(Rest, 0, [N|Counts]);
checksum([$.|Rest], 0, Counts) ->
    checksum(Rest, 0, Counts);
checksum([], N, Counts) when N > 0 ->
    lists:reverse([N|Counts]);
checksum([], 0, Counts) ->
    lists:reverse(Counts).
