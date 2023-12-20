-module(day19).

-compile(export_all).

load_input(Path) ->
    {ok, Data} = file:read_file(Path),
    [Workflows, Parts] = binary:split(Data, <<"\n\n">>),
    {parse_workflows(Workflows), parse_parts(Parts)}.

parse_parts(PartsStr) ->
    Parts = binary:split(PartsStr, <<"\n">>, [global, trim]),
    [parse_part(binary_to_list(Part)) || Part <- Parts].

parse_part(Part) ->
    RE = "\\{x=(\\d+),m=(\\d+),a=(\\d+),s=(\\d+)\\}",
    {match, [X, M, A, S]} =
        re:run(Part, RE, [{capture, [1, 2, 3, 4], list}]),
    #{x => list_to_integer(X),
      m => list_to_integer(M),
      a => list_to_integer(A),
      s => list_to_integer(S)}.

parse_workflows(Workflows) ->
    Workflows1 = binary:split(Workflows, <<"\n">>, [global, trim]),
    maps:from_list([parse_workflow(binary_to_list(Workflow))
                    || Workflow <- Workflows1]).

parse_workflow(WorkflowStr) ->
    RE = "([[:alpha:]]+)\\{(.*)\\}",
    {match, [Name, RuleStr]} =
        re:run(WorkflowStr, RE, [{capture, [1, 2], list}]),
    Rules = parse_rules(RuleStr),
    {Name, Rules}.

parse_rules(RuleStr) ->
    Rules = string:split(RuleStr, ",", all),
    [parse_rule(Rule) || Rule <- Rules].

parse_rule(R) ->
    RE = "([xmas])([<>])(\\d+):([[:alpha:]]+)",
    case re:run(R, RE, [{capture, [1, 2, 3, 4], list}]) of
        {match, [Var, Op, N, Dest]} ->
            {parse_op(Var, Op, N), Dest};
        nomatch ->
            {default, R}
    end.

parse_op(Var, Op, N) ->
    {list_to_atom(Var), list_to_atom(Op), list_to_integer(N)}.

summarize_accepted({Workflows, Parts}) ->
    Accepted = [Part || Part <- Parts, accepted(Workflows, Part)],
    Summarize = fun(#{x := X, m := M, a := A, s := S}, Acc) ->
                        Acc + X + M + A + S
                end,
    lists:foldl(Summarize, 0, Accepted).

accepted(Workflows, Part) ->
    accepted("in", Workflows, Part).

accepted(Cur, Workflows, Part) ->
    W = maps:get(Cur, Workflows),
    case apply_workflow(W, Part) of
        "A" ->
            true;
        "R" ->
            false;
        W1 ->
            accepted(W1, Workflows, Part)
    end.

apply_workflow([{default, Default}], _) ->
    Default;
apply_workflow([{{Var, Op, N}, O}|Rest], Part) ->
    Val = maps:get(Var, Part),
    case apply(erlang, Op, [Val, N]) of
        true ->
            O;
        false ->
            apply_workflow(Rest, Part)
    end.
