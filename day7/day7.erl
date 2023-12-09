-module(day7).

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
            load_input(File, [parse_hand(string:trim(Data))|Input])
    end.

parse_hand(Data) ->
    [Hand, Bid] = string:split(Data, " "),
    {Hand, list_to_integer(Bid)}.

partition(Hand) ->
    F = fun(X, Acc) ->
                maps:update_with(X, fun(C) -> C + 1 end, 1, Acc)
        end,
    lists:foldl(F, #{}, Hand).

strengthen(#{$J := J} = P) ->
    H = maps:without([$J], P),
    %% get the max key and add all the jokers to that key
    MaxVal = fun(K1, V1, {K, V}) ->
                      if V1 > V ->
                              {K1, V1};
                         true ->
                              {K, V}
                      end
              end,
    case maps:fold(MaxVal, {nil, 0}, H) of
        {nil, 0} ->
            P;
        {K, V} ->
            H#{K => V + J}
    end;
strengthen(P) ->
    P.

classify_hand(Hand) ->
    P = strengthen(partition(Hand)),
    Counts = lists:sort(maps:values(P)),
    case Counts of
        [5]             -> 1;
        [1, 4]          -> 2;
        [2, 3]          -> 3;
        [1, 1, 3]       -> 4;
        [1, 2, 2]       -> 5;
        [1, 1, 1, 2]    -> 6;
        [1, 1, 1, 1, 1] -> 7
    end.

compare_hands(HandA, HandB) ->
    HandAClass = classify_hand(HandA),
    HandBClass = classify_hand(HandB),
    if HandAClass < HandBClass ->
            true;
       HandAClass > HandBClass ->
            false;
       HandAClass =:= HandBClass ->
            order_by_card(HandA, HandB)
    end.

order_by_card([CardA|RestA], [CardB|RestB]) ->
    case compare_cards(CardA, CardB) of
        lt ->
            true;
        eq ->
            order_by_card(RestA, RestB);
        gt ->
            false
    end.

compare_cards(A, B) ->
    RankA = card_rank(A),
    RankB = card_rank(B),
    if RankA < RankB ->
            lt;
       RankA =:= RankB ->
            eq;
       RankA > RankB ->
            gt
    end.

card_rank($A) -> 1;
card_rank($K) -> 2;
card_rank($Q) -> 3;
card_rank($T) -> 5;
card_rank($9) -> 6;
card_rank($8) -> 7;
card_rank($7) -> 8;
card_rank($6) -> 9;
card_rank($5) -> 10;
card_rank($4) -> 11;
card_rank($3) -> 12;
card_rank($2) -> 13;
card_rank($J) -> 14.

winnings(Hands) ->
    F = fun({HandA, _}, {HandB, _}) -> compare_hands(HandA, HandB) end,
    OrderedHands = lists:reverse(lists:sort(F, Hands)),
    {_, OrderedBets} = lists:unzip(OrderedHands),
    Multipliers = lists:seq(1, length(OrderedBets)),
    lists:sum(
      lists:zipwith(fun(X, Y) -> X * Y end, OrderedBets, Multipliers)).
