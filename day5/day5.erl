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

seeds_to_ranges([]) ->
    [];
seeds_to_ranges([Start, Len|Rest]) ->
    [{Start, Len}|seeds_to_ranges(Rest)].

seeds_to_location_ranges({Seeds, Maps}) ->
    SeedRanges = seeds_to_ranges(Seeds),
    lists:flatten([translate_range(S, Maps) || S <- SeedRanges]).

translate_range(Range, []) -> [Range];
translate_range(Range, [Map|Maps]) ->
    NewRanges = apply_map_range(Range, Map),
    lists:concat([translate_range(R, Maps) || R <- NewRanges]).

apply_map_range(Range, []) ->
    [Range];
apply_map_range(Range, [M|Map]) ->
    {Output, RemainingRanges} = apply_map_range1(M, Range),
    Outputs = [apply_map_range(R, Map) || R <- RemainingRanges],
    lists:flatten(Output ++ Outputs).

apply_map_range1({DestStart, SrcStart, Len}, {Start, SrcLen})
  when Start >= SrcStart,
       Start + SrcLen =< SrcStart + Len ->
    %% The source range is fully containted
    StartOffset = Start - SrcStart,
    {[{DestStart + StartOffset, SrcLen}], []};
apply_map_range1({DestStart, SrcStart, Len}, {Start, SrcLen})
  when Start >= SrcStart,
       Start < SrcStart + Len,
       Start + SrcLen > SrcStart + Len ->
    %% The start of the source range overlaps
    SrcRangeEnd = SrcStart + Len - 1,
    CoveredLen = SrcRangeEnd - Start + 1,
    StartOffset = Start - SrcStart,
    {[{DestStart + StartOffset, CoveredLen}],
     [{SrcRangeEnd + 1, SrcLen - CoveredLen}]};
apply_map_range1({DestStart, SrcStart, Len}, {Start, SrcLen})
  when Start < SrcStart,
       Start + SrcLen =< SrcStart + Len,
       Start + SrcLen >= SrcStart ->
    %% the end of the source range overlaps
    SrcEnd = Start + SrcLen - 1,
    CoveredLen = SrcEnd - SrcStart + 1,
    {[{DestStart, CoveredLen}], [{Start, SrcLen - CoveredLen}]};
apply_map_range1({DestStart, SrcStart, Len}, {Start, SrcLen})
  when Start < SrcStart,
       SrcStart + Len < Start + SrcLen ->
    %% The source range contains the whole map range
    PreLen = SrcStart - Start,
    PostStart = SrcStart + Len,
    PostLen = (Start + SrcLen) - PostStart,
    {[{DestStart, Len}], [{Start, PreLen}, {PostStart, PostLen}]};
apply_map_range1(_, Range) ->
    %% Not covered
    {[], [Range]}.
