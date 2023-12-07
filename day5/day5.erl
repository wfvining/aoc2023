-module(day5).

-compile(export_all).

load_input(Path) ->
    {ok, File} = file:open(Path, [read]),
    Seeds = load_seeds(File),
    file:read_line(File),
    SeedToSoil = load_map(File),
    SoilToFertilizer = load_map(File),
    FertilizerToWater = load_map(File),
    WaterToLight = load_map(File),
    LightToTemp = load_map(File),
    TempToHumidity = load_map(File),
    HumidityToLocation = load_map(File),
    file:close(File),
    {Seeds, [SeedToSoil, SoilToFertilizer,
             FertilizerToWater, WaterToLight,
             LightToTemp, TempToHumidity,
             HumidityToLocation]}.

load_seeds(File) ->
    {ok, L} = file:read_line(File),
    [_|Seeds] = string:split(L, " ", all),
    [list_to_integer(string:trim(string:strip(Seed))) || Seed <- Seeds].

load_map(File) ->
    file:read_line(File), %% Header
    load_map(File, []).

load_map(File, Map) ->
    case file:read_line(File) of
        {ok, "\n"} ->
            Map;
        eof ->
            Map;
        {ok, Data} ->
            load_map(File, [parse_map_range(Data)|Map])
    end.

parse_map_range(Data) ->
    [DestStart, SrcStart, Len] =
        string:split(string:trim(string:strip(Data)), " ", all),
    {list_to_integer(DestStart),
     list_to_integer(SrcStart),
     list_to_integer(Len)}.

translate(X, []) -> X;
translate(X, [Map|Maps]) ->
    translate(apply_map(X, Map), Maps).

apply_map(X, Map) ->
    F = fun({DestStart, SrcStart, Len}) ->
                case in_range(X, SrcStart, Len) of
                    true ->
                        {true, DestStart + (X - SrcStart)};
                    false ->
                        false
                end
        end,
    case lists:filtermap(F, Map) of
        [NewValue] ->
            NewValue;
        [] ->
            X
    end.

in_range(X, Start, Len) when X >= Start ->
    X < (Start + Len);
in_range(_, _, _) ->
    false.

seeds_to_locations({Seeds, Maps}) ->
    [translate(S, Maps) || S <- Seeds].
