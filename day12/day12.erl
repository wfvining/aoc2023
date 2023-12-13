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

count_valid(Str, CheckSum) ->
    F = fun(C) -> C =:= $? end,
    case lists:any(F, Str) of
        true ->
            A = lists:flatten(string:replace(Str, "?", ".")),
            B = lists:flatten(string:replace(Str, "?", "#")),
            CountA = count_valid1(A, CheckSum),
            CountB = count_valid1(B, CheckSum),
            Total = CountA + CountB,
            store({Str, CheckSum}, Total),
            Total;
        false ->
            case is_valid(Str, CheckSum) of
                true ->
                    1;
                false ->
                    0
            end
    end.

count_valid1(Str, CSum) ->
    try
        ets:lookup_element(cache, {Str, CSum}, 2)
    catch error:badarg ->
            count_valid(Str, CSum)
    end.

store(Key, Value) ->
    ets:insert(cache, {Key, Value}).

init_cache() ->
    ets:new(cache, [set, named_table]).

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
