-module(day8).

-compile(export_all).

load_input(Path) ->
    {ok, File} = file:open(Path, [read]),
    Directions = load_directions(File),
    {ok, "\n"} = file:read_line(File),
    Network = load_network(File),
    file:close(File),
    {Directions, Network}.

load_directions(File) ->
    {ok, Line} = file:read_line(File),
    string:trim(Line).

load_network(File) ->
    load_network(File, #{}).

load_network(File, Adj) ->
    RE = "([A-Z][A-Z][A-Z]) = \\(([A-Z][A-Z][A-Z]), ([A-Z][A-Z][A-Z])\\)",
    case file:read_line(File) of
        eof ->
            Adj;
        {ok, Data} ->
            Data1 = string:trim(Data),
            {match, [Src, L, R]} =
                re:run(Data1, RE, [{capture, [1, 2, 3], list}]),
            load_network(File, Adj#{Src => {L, R}})
    end.

path_length({Directions, Network}) ->
    path_length(Directions, "AAA", Directions, Network, 0).

path_length(_, [_, _, $Z], _, _, N) ->
    N;
path_length([], Node, Directions, Network, N) ->
    path_length(Directions, Node, Directions, Network, N);
path_length([$L|Rest], Node, Directions, Network, N) ->
    {L, _} = maps:get(Node, Network),
    path_length(Rest, L, Directions, Network, N + 1);
path_length([$R|Rest], Node, Directions, Network, N) ->
    {_, R} = maps:get(Node, Network),
    path_length(Rest, R, Directions, Network, N + 1).

all_ending_in_a(Network) ->
    [N || [_, _, C] = N <- maps:keys(Network), C =:= $A].

ghost_path_length({Directions, Network}) ->
    AllAs = all_ending_in_a(Network),
    Cycles = [path_length(Directions, N, Directions, Network, 0)
              || N <- AllAs],
    lcm(Cycles).

lcm([X|Rest]) ->
    lists:foldl(fun lcm/2, X, Rest).

lcm(A, B) -> (A * B) div gcd(A, B).

gcd(A, B) when A < B ->
    gcd(B, A);
gcd(A, 0) ->
    A;
gcd(A, B) ->
    gcd(B, A rem B).

%% ghost_path_length({Directions, Network}) ->
%%     AllAs = all_ending_in_a(Network),
%%     ghost_path_length(Directions, AllAs, Directions, Network, 0).

%% ghost_path_length([], Nodes, Directions, Network, N) ->
%%     ghost_path_length(Directions, Nodes, Directions, Network, N);
%% ghost_path_length([Dir|Rest], Nodes, Directions, Network, N) ->
%%     case all([C =:= $Z || [_, _, C] <- Nodes]) of
%%         true ->
%%             N;
%%         false ->
%%             NewNodes = [go(Dir, Node, Network) || Node <- Nodes],
%%             ghost_path_length(Rest, NewNodes, Directions, Network, N + 1)
%%     end.

%% all(List) ->
%%     lists:all(fun(X) -> X end, List).

%% go($L, Node, Network) ->
%%     {L, _} = maps:get(Node, Network),
%%     L;
%% go($R, Node, Network) ->
%%     {_, R} = maps:get(Node, Network),
%%     R.
