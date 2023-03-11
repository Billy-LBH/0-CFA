%%%-------------------------------------------------------------------
%% @doc parser public API
%% @end
%%%-------------------------------------------------------------------

-module(parser).

-export([parse/1, newlabel/1, getLambdas/2, congen/2, recall/3]).

% E ::= x | (E1 E2) | (/x.E)

parse(Input) ->
    {Expr, Rest} = parse_expr(Input),
    case Rest of
        [] -> Expr;
        _ -> {error, "empty input"}
    end.

parse_expr([F | Rest]) when F >= $a, F =< $z; F >= $A, F =< $Z ->
    % check if the first character of a string is a letter, ASCII
    parse_var([F | Rest]);
parse_expr([$\s | Rest]) ->
    % when space appear, skip the space
    parse_expr(Rest);
parse_expr([$(, $/ | Rest]) ->
    % check if it is abstraction
    {V1, [$. | Rest1]} = parse_vars(Rest),
    % parse the bound variable
    {E1, [$) | Rest2]} = parse_expr(Rest1),
    % parse the main body
    {{lam, V1, E1}, Rest2};
parse_expr([$( | Rest]) ->
    {E1, Rest1} = parse_expr(Rest),
    % check if it is application
    {E2, [$) | Rest2]} = parse_expr(Rest1),
    {{app, E1, E2}, Rest2};
parse_expr([]) ->
    {error, "empty input"}.

parse_var([F | Rest]) when F >= $a, F =< $z; F >= $A, F =< $Z ->
    % if satisfy the condition, add var before the tuple
    {{var, F}, Rest};
parse_var([$\s | Rest]) ->
    % skip space
    parse_var(Rest);
parse_var(Input) ->
    {error, Input}.

parse_vars(Input) ->
    case parse_var(Input) of
        {{var, F}, Rest} ->
            {Vars, Rest2} = parse_vars(Rest),
            {[{var, F} | Vars], Rest2};
        {error, Rest1} ->
            {[], Rest1}
    end.

%----------- Label -----------%

% label([First | Rest], N) ->
%     if
%         Rest /= [] ->
%             {[LabelledE1], N1} = label(Rest, N + 1),
%             {[{First, N}, LabelledE1], N1};
%         true ->
%             []
%     end.

% label([First | Rest], N) ->
%     {Labelled, N1} = label(Rest, N + 1),
%     {[{First, N} | Labelled], N1};
% label([], N) ->
%     {[], N}.

label({var, Var}, N) ->
    {{var, N, Var}, N + 1};
label([First | Rest], N) ->
    {Labelled, N1} = label(Rest, N + 1),
    {[{First, N} | Labelled], N1};
label([], N) ->
    {[], N};
label({lam, Var, E}, N) ->
    {LabelledVar, N2} = label(Var, N + 1),
    {LabelledE, N3} = label(E, N2),
    {{lam, N, LabelledVar, LabelledE}, N3};
label({app, E1, E2}, N) ->
    {LabelledE1, N2} = label(E1, N + 1),
    {LabelledE2, N3} = label(E2, N2),
    {{app, N, LabelledE1, LabelledE2}, N3}.

newlabel(Tree) ->
    {LabelledTree, _} = label(Tree, 1),
    LabelledTree.

% label(Tree, N) ->
%     case Tree of
%         {var, Var} ->
%             {{var, Var}, N};
%         [First | Rest] ->
%             {Labelled, N1} = label(Rest, N + 1),
%             {[{First, N} | Labelled], N1};
%         [] ->
%             {[], N};
%         {lam, Var, E} ->
%             LabelledVar = label(Var, N + 1),
%             LabelledE = label(E, N + 2),
%             {{lam, N, LabelledVar, LabelledE}, N + 3};
%         {app, E1, E2} ->
%             LabelledE1 = label(E1, N + 1),
%             LabelledE2 = label(E2, N + 2),
%             {{app, N, LabelledE1, LabelledE2}, N + 3}
%     end.

% ----------------congen------------------ %

% getLambdas(LabelledTree, FinalList) ->
%     case LabelledTree of
%         {lam, L, V, E} ->
%             [[{lam, L, V, E}] | FinalList];
%         {app, L, E1, E2} ->
%             List1 = getLambdas(E1, FinalList),
%             List2 = getLambdas(E2, FinalList ++ List1),
%             List2;
%         _ ->
%             FinalList
%     end.

getLambdas({lam, L, V, E}, FinalList) ->
    E1 = getLambdas(E, FinalList),
    [{lam, L, V, E} | E1];
% getLambdas({{lam, L, V, E}, _}, FinalList) ->
%     E1 = getLambdas(E, FinalList),
%     [{lam, L, V, E} | E1];
% getLambdas({{app, _, E1, E2}, _}, FinalList) ->
%     List1 = getLambdas(E1, FinalList),
%     List2 = getLambdas(E2, List1),
%     List2;
getLambdas({app, _, E1, E2}, FinalList) ->
    List1 = getLambdas(E1, FinalList),
    List2 = getLambdas(E2, List1),
    List2;
getLambdas({_, _, _}, FinalList) ->
    FinalList.

congen({var, L, Var}, _) ->
    [{flow, Var, L}];
congen({lam, L, V, E}, Lams) ->
    Cs = congen(E, Lams),
    [{flow, {lam, L, V, E}, L} | Cs];
congen({app, L, E1, E2}, Lams) ->
    C1 = congen(E1, Lams),
    C2 = congen(E2, Lams),
    recall(Lams, L, E2) ++ C1 ++ C2.

recall([], _, _) ->
    [];
recall([F | Rest], L, E2) ->
    LE1 = element(2, F),
    LE2 = element(2, E2),
    LV1 = element(3, F),
    LE = element(2, element(4, F)),
    C1 = {implies, {flow, F, LE1}, {flow, LE2, LV1}},
    C2 = {implies, {flow, F, LE1}, {flow, LE, LE1}},
    [C1, C2 | recall(Rest, L, E2)].

%     C1 = congen(E1),
    %     C2 = congen(E2),
    %     LE1 = element(2, E1),
    %     LE2 = element(2, E2),
    %     LV1 = element(3, E1),
    %     LE = element(2, element(4, E1)),
    % lists:foreach(
    %     fun(E) ->
    %         E1 = E,
    %         LE1 = element(2, E1),
    %         LE2 = element(2, E2),
    %         LV1 = element(3, E1),
    %         LE = element(2, element(4, E1)),
    %         [
    %             {implies, {flow, Lam, LE1}, {flow, LE2, LV1}},
    %             {implies, {flow, Lam, LE1}, {flow, LE, LE1}}
    %         ] ++ C1 ++ C2
    %     end,
    %     Lams
    % ).

% case E1 of
    %     Lam when lists:member(Lam, Lams) ->
    %         [
    %             {implies, {flow, Lam, LE1}, {flow, LE2, LV1}},
    %             {implies, {flow, Lam, LE1}, {flow, LE, LE1}}
    %         ] ++ C1 ++ C2;
    %     _ ->
    %         C1 ++ C2
    % end.

% use uniq to avoid duplicated situation

%% internal functions