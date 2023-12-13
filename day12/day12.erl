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

count([], [], Cache) -> {1, Cache};
count([], _, Cache) -> {0, Cache};
count(Cfg, [], Cache) ->
    case lists:member($#, Cfg) of
        true -> {0, Cache};
        false -> {1, Cache}
    end;
count([$.|Rest] = Cfg, Nums, Cache) ->
    mcount(Rest, Nums, Cache);
count([$#|_] = Cfg, [N|Nums], Cache) ->
    try lists:split(N, Cfg) of
        {Block, []} ->
            case lists:member($., Block) of
                false when length(Cfg) =:= N ->
                    mcount([], Nums, Cache);
                _ ->
                    {0, Cache}
            end;
        {Block, [C|Rest]} when C =/= $# ->
            case lists:member($., Block) of
                false ->
                    mcount(Rest, Nums, Cache);
                true ->
                    {0, Cache}
            end;
        {_Block, [$#|_Rest]} ->
            {0, Cache}
    catch error:badarg ->
            {0, Cache}
    end;
count([$?|Rest] = Cfg, Nums, Cache) ->
    {A, Cache1} = mcount(Rest, Nums, Cache),
    {B, Cache2} = mcount([$#|Rest], Nums, Cache1),
    Total = A + B,
    {Total, Cache2}.

mcount(Cfg, Nums, Cache) when is_map_key({Cfg, Nums}, Cache) ->
    {maps:get({Cfg, Nums}, Cache), Cache};
mcount(Cfg, Nums, Cache) ->
    {C, Cache1} = count(Cfg, Nums, Cache),
    Cache2 = maps:put({Cfg, Nums}, C, Cache1),
    {C, Cache2}.

count_all(Input) ->
    F = fun({Cfg, Nums}, Acc) -> {C, _} = mcount(Cfg, Nums, #{}), Acc + C end,
    lists:foldl(F, 0, [unfold(I) || I <- Input]).

unfold({Cfg, Nums}) ->
    Nums1 = lists:flatten(lists:duplicate(5, Nums)),
    Cfg1 = lists:flatten(string:join(lists:duplicate(5, Cfg), "?")),
    {Cfg1, Nums1}.
