-module(pq).

-export([new/0, insert/3, take_smallest/1]).

new() ->
    [].

insert(Pri, K, Q) ->
    insert_sorted({K, Pri}, Q).

take_smallest(Q) ->
    Q1 = tl(Q),
    {K, Pri} = hd(Q),
    {Pri, K, Q1}.

insert_sorted({K, V}, []) ->
    [{K, V}];
insert_sorted({K, V}, [{_, V1} = X|Rest]) ->
    if V >= V1 ->
            [X|insert_sorted({K, V}, Rest)];
       V < V1 ->
            [{K, V}, X|Rest]
    end.
