-module(day4).

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
            load_input(File, [parse_card(string:trim(Data))|Input])
    end.

parse_card(Data) ->
    ["Card " ++ N, AllNumbers] = string:split(Data, ":"),
    [NumbersStr, WinnersStr] = string:split(AllNumbers, "|"),
    CardNumber = list_to_integer(string:strip(N)),
    {CardNumber, {number_list(NumbersStr), number_list(WinnersStr)}}.

number_list(Str) ->
    Numbers = string:split(Str, " ", all),
    [list_to_integer(N) || N <- Numbers, length(N) > 0].

score_cards(Cards) ->
    [score_card(Card) || Card <- Cards].

score_card({_, {Numbers, Winners}}) ->
    Losers = Winners -- Numbers,
    length(Winners) - length(Losers).

process_cards(Cards) ->
    Inventory = maps:from_list(
                  [{N, 1} || N <- lists:seq(1, length(Cards))]),
    ToProcess = [N || {N, _} <- Cards],
    process_cards(ToProcess, maps:from_list(Cards), Inventory).

process_cards([], _, Inventory) -> Inventory;
process_cards([Card|ToProcess], Cards, Inventory) ->
    NewCards = process_card(Card, Cards),
    NewInventory = update_inventory(NewCards, Inventory),
    process_cards(NewCards ++ ToProcess, Cards, NewInventory).

process_card(N, Cards) ->
    Card = maps:get(N, Cards),
    Matches = score_card({N, Card}),
    if Matches =:= 0 ->
            [];
       Matches > 0 ->
            Copies = lists:seq(N + 1, N + Matches),
            [X || X <- Copies, is_map_key(X, Cards)]
    end.

update_inventory(NewCards, Inventory) ->
    F = fun(X, Acc) ->
                maps:update_with(X, fun(Y) -> Y + 1 end, Acc)
        end,
    lists:foldl(F, Inventory, NewCards).
