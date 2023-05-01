%%%-------------------------------------------------------------------
%% @doc parser public API
%% @end
%%%-------------------------------------------------------------------

-module(parser).

-export([cfa/1, parse_proc/1, newlabel/1, parse_expr/1, getPid/2]).

% E ::= x | (E1 E2) | (/x.E) | n | (!e1 e2) | (#x.E) ｜ (&e)

% Pi ::= 0 | <M>n || Pi
% 0 is the empty list of processes,
% and <M>n || Pi is a list of processes with one process <M>n in the head followed by Pi more processes

cfa(Input) ->
    Ptree = parse_proc(Input),
    LabelledPtree = newlabel(Ptree),
    LabelledTerm = reverse(LabelledPtree),
    Map = newmap(Ptree),
    Keys = getMessagekey(LabelledPtree, []),
    News = getNew(LabelledPtree),
    Newsnumber = length(News),
    Keysnumber = length(Keys),
    Newmap = generate(Keysnumber, Keysnumber + Newsnumber),
    Keymap = messagebox(Keys),
    Allmap = maps:merge(Map, Keymap),
    Totalmap = maps:merge(Allmap, Newmap),
    Lams = getLambdas(LabelledPtree, []),
    Pids = getPid(LabelledPtree, []),
    Conlist = congen(LabelledPtree, Pids, Lams),
    io:fwrite("~s~n", [LabelledTerm]),
    solve(Conlist, Totalmap).

% ---------- Seq ---------- %

% -------------------- Parse ------------------ %

% parse(Input) ->
%     {Expr, Rest} = parse_proc(Input),
%     case Rest of
%         [] -> Expr;
%         _ -> {error, "empty input"}
%     end.

parse_expr([F | Rest]) when F >= $a, F =< $z; F >= $A, F =< $Z ->
    % check if the first character of a string is a letter, ASCII
    parse_var([F | Rest]);
parse_expr([$\s | Rest]) ->
    % when space appear, skip the space
    parse_expr(Rest);
parse_expr([$(, $& | Rest]) ->
    {P, [$) | Rest1]} = parse_expr(Rest),
    {{new, P}, Rest1};
parse_expr([$(, $! | Rest]) ->
    % check if it is send
    {E, Rest1} = parse_expr(Rest),
    {M, [$) | Rest2]} = parse_expr(Rest1),
    {{send, E, M}, Rest2};
parse_expr([$(, $# | Rest]) ->
    % check if it is receive
    {M, [$. | Rest1]} = parse_vars(Rest),
    {E, [$) | Rest2]} = parse_expr(Rest1),
    {{rec, M, E}, Rest2};
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
% parse_expr([$< | Rest]) ->
%     {T1, [$> | Rest1]} = parse_expr(Rest),
%     {P1, Rest2} = parse_expr(Rest1),
%     {{proc, T1, P1}, Rest2};
% parse_expr([$|, $| | Rest]) ->
%     parse_expr(Rest);
parse_expr([F | Rest]) when is_integer(F) ->
    {{pid, F}, Rest};
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

parse_proc([$< | Rest]) ->
    {T1, [$> | Rest1]} = parse_expr(Rest),
    {P1, Rest2} = parse_proc(Rest1),
    [{proc, T1, P1} | parse_proc(Rest2)];
parse_proc([$|, $| | Rest]) ->
    parse_proc(Rest);
parse_proc([F | Rest]) when is_integer(F) ->
    {{procid, F}, Rest};
parse_proc([]) ->
    [].
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
label({pid, P}, N) ->
    {{pid, N, P}, N + 1};
label({procid, P}, N) ->
    {{procid, P}, N};
label({new, P}, N) ->
    {LabelledP, N1} = label(P, N + 1),
    {{new, N, LabelledP}, N1};
label({proc, T, P}, N) ->
    {LabelledT, N1} = label(T, N),
    {{proc, LabelledT, P}, N1};
% label({conc, P1, P2}, N) ->
%     {LabelledP1, N1} = label(P1, N),
%     {LabelledP2, N2} = label(P2, N1),
%     {{conc, LabelledP1, LabelledP2}, N2};
label([{var, Var} | Rest], N) ->
    {Labelled, N1} = label(Rest, N + 1),
    {[{var, N, Var} | Labelled], N1};
label([{proc, T, P} | Rest], N) ->
    {LabelledT, N1} = label(T, N),
    {LabelledP, N2} = label(P, N1),
    {Labelled, N3} = label(Rest, N2),
    {[{proc, LabelledT, LabelledP} | Labelled], N3};
label([], N) ->
    {[], N};
label({lam, Var, E}, N) ->
    {LabelledVar, N2} = label(Var, N + 1),
    {LabelledE, N3} = label(E, N2),
    {{lam, N, LabelledVar, LabelledE}, N3};
label({app, E1, E2}, N) ->
    {LabelledE1, N2} = label(E1, N + 1),
    {LabelledE2, N3} = label(E2, N2),
    {{app, N, LabelledE1, LabelledE2}, N3};
label({rec, M, E}, N) ->
    {LabelledMes, N2} = label(M, N + 1),
    {LabelledE, N3} = label(E, N2),
    {{rec, N, LabelledMes, LabelledE}, N3};
label({send, E1, E2}, N) ->
    {LabelledE1, N2} = label(E1, N + 1),
    {LabelledE2, N3} = label(E2, N2),
    {{send, N, LabelledE1, LabelledE2}, N3}.

newlabel(Tree) ->
    {LabelledTree, _} = label(Tree, 1),
    LabelledTree.

newmap(Tree) ->
    {_, N} = label(Tree, 1),
    Keys = lists:seq(1, N - 1),
    KeyValueTuples = [{Key, []} || Key <- Keys],
    maps:from_list(KeyValueTuples).

messagebox(Mkeys) ->
    KeyValueTuples = [{Key, []} || Key <- Mkeys],
    maps:from_list(KeyValueTuples).

generate(In, Nn) ->
    Keys = lists:seq(In + 1, Nn),
    KeyValueTuples = [{{procid, I}, []} || I <- Keys],
    maps:from_list(KeyValueTuples).

% based on the label number, make corresponding maps

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

% -------------------- Parse reverse ------------------ %

reverse({var, L, Var}) ->
    String = io_lib:format("~c'~p", [Var, L]),
    String;
reverse({pid, L, P}) ->
    String = io_lib:format("~c'~p", [P, L]),
    String;
reverse({procid, P}) ->
    String = io_lib:format("~c", [P]),
    String;
reverse({new, L, P}) ->
    T1 = reverse(P),
    String = io_lib:format("(&~s)'~p", [T1, L]),
    String;
% reverse({conc, P1, P2}) ->
%     T1 = reverse(P1),
%     T2 = reverse(P2),
%     String = io_lib:format("~s||~s", [T1, T2]),
%     String;
reverse([{proc, T, P} | Rest]) ->
    T1 = reverse(T),
    T2 = reverse(P),
    T3 = reverse(Rest),
    case T3 of
        ok ->
            String = io_lib:format("<~s>~s", [T1, T2]);
        _ ->
            String = io_lib:format("<~s>~s||~s", [T1, T2, T3])
    end,
    String;
reverse([{var, L, Var} | Rest]) ->
    T1 = reverse(Rest),
    case T1 of
        ok ->
            String = io_lib:format("~c'~p", [Var, L]);
        _ ->
            String = io_lib:format("~c'~p~s", [Var, L, T1])
    end,
    String;
reverse({lam, L, V, E}) ->
    T1 = reverse(V),
    T2 = reverse(E),
    String = io_lib:format("(/~s.~s)'~p", [T1, T2, L]),
    String;
reverse({app, L, E1, E2}) ->
    T1 = reverse(E1),
    T2 = reverse(E2),
    String = io_lib:format("(~s ~s)'~p", [T1, T2, L]),
    String;
reverse({send, L, E, M}) ->
    T1 = reverse(E),
    T2 = reverse(M),
    String = io_lib:format("(!~s ~s)'~p", [T1, T2, L]),
    String;
reverse({rec, L, M, E}) ->
    T1 = reverse(M),
    T2 = reverse(E),
    String = io_lib:format("(#~s.~s)'~p", [T1, T2, L]),
    String;
reverse([]) ->
    String = io:format(""),
    String.

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

% getLambdas({{lam, L, V, E}, _}, FinalList) ->
%     E1 = getLambdas(E, FinalList),
%     [{lam, L, V, E} | E1];
% getLambdas({{app, _, E1, E2}, _}, FinalList) ->
%     List1 = getLambdas(E1, FinalList),
%     List2 = getLambdas(E2, List1),
%     List2;
% getLambdas({conc, P1, P2}, FinalList) ->
%     List1 = getLambdas(P1, FinalList),
%     List2 = getLambdas(P2, List1),
%     List2;

getLambdas({lam, L, V, E}, FinalList) ->
    List = getLambdas(E, FinalList),
    [{lam, L, V, E} | List];
getLambdas([{proc, T, _} | Rest], FinalList) ->
    List1 = getLambdas(T, FinalList),
    List2 = getLambdas(Rest, List1),
    List2;
getLambdas({new, _, P}, FinalList) ->
    List = getLambdas(P, FinalList),
    List;
getLambdas({app, _, E1, E2}, FinalList) ->
    List1 = getLambdas(E1, FinalList),
    List2 = getLambdas(E2, List1),
    List2;
getLambdas({send, _, E, M}, FinalList) ->
    List1 = getLambdas(E, FinalList),
    List2 = getLambdas(M, List1),
    List2;
getLambdas({rec, _, _, E}, FinalList) ->
    List = getLambdas(E, FinalList),
    List;
getLambdas({_, _, _}, FinalList) ->
    FinalList;
getLambdas([], FinalList) ->
    FinalList.

getPid({pid, L, P}, FinalList) ->
    [{pid, L, P} | FinalList];
getPid({new, _, P}, FinalList) ->
    List = getPid(P, FinalList),
    List;
getPid({lam, _, _, E}, FinalList) ->
    List = getPid(E, FinalList),
    List;
getPid([{proc, T, _} | Rest], FinalList) ->
    List1 = getPid(T, FinalList),
    List2 = getPid(Rest, List1),
    List2;
getPid({app, _, E1, E2}, FinalList) ->
    List1 = getPid(E1, FinalList),
    List2 = getPid(E2, List1),
    List2;
getPid({send, _, E, M}, FinalList) ->
    List1 = getPid(E, FinalList),
    List2 = getPid(M, List1),
    List2;
getPid({rec, _, _, E}, FinalList) ->
    List = getPid(E, FinalList),
    List;
getPid({_, _, _}, FinalList) ->
    FinalList;
getPid([], FinalList) ->
    FinalList.

getMessagekey({procid, P}, FinalList) ->
    [{procid, P} | FinalList];
getMessagekey({new, _, _}, FinalList) ->
    FinalList;
getMessagekey({pid, _, _}, FinalList) ->
    FinalList;
getMessagekey({lam, _, _, E}, FinalList) ->
    List = getMessagekey(E, FinalList),
    List;
getMessagekey([{proc, T, P} | Rest], FinalList) ->
    List1 = getMessagekey(T, FinalList),
    List2 = getMessagekey(P, List1),
    List3 = getMessagekey(Rest, List2),
    List3;
getMessagekey({app, _, E1, E2}, FinalList) ->
    List1 = getMessagekey(E1, FinalList),
    List2 = getMessagekey(E2, List1),
    List2;
getMessagekey({send, _, E, M}, FinalList) ->
    List1 = getMessagekey(E, FinalList),
    List2 = getMessagekey(M, List1),
    List2;
getMessagekey({rec, _, _, E}, FinalList) ->
    List = getMessagekey(E, FinalList),
    List;
getMessagekey({_, _, _}, FinalList) ->
    FinalList;
getMessagekey([], FinalList) ->
    FinalList.

getNew(Input) ->
    case Input of
        {new, L, P} -> [{new, L, P}];
        {rec, _, _, E} -> getNew(E);
        {lam, _, _, E} -> getNew(E);
        {app, _, E1, E2} -> getNew(E1) ++ getNew(E2);
        {send, _, E, M} -> getNew(E) ++ getNew(M);
        [{proc, T, _} | Rest] -> getNew(T) ++ getNew(Rest);
        _ -> []
    end.

check(V, {var, L, Var}) ->
    List = [X || X <- V, element(3, X) =:= Var],
    case List of
        [] ->
            [];
        _ ->
            [{flow, element(2, lists:nth(1, List)), L}]
    end;
check(_, {lam, _, _, _}) ->
    [];
check(V, {app, _, E1, E2}) ->
    L1 = check(V, E1),
    L2 = check(V, E2),
    L1 ++ L2;
check(_, {procid, _}) ->
    [];
check(_, {pid, _, _}) ->
    [];
check(V, {send, _, E, M}) ->
    L1 = check(V, E),
    L2 = check(V, M),
    L1 ++ L2;
check(_, {rec, _, _, _}) ->
    [].

checkatom(Input) ->
    case Input of
        {rec, L, M, E} -> [{rec, L, M, E}];
        {new, _, P} -> checkatom(P);
        {lam, _, _, E} -> checkatom(E);
        {app, _, E1, E2} -> checkatom(E1) ++ checkatom(E2);
        {send, _, E, M} -> checkatom(E) ++ checkatom(M);
        _ -> []
    end.

congen({var, L, Var}, _, _) ->
    [{flow, {var, L, Var}, L}];
congen({pid, L, P}, _, _) ->
    [{flow, {pid, L, P}, L}];
congen({procid, _}, _, _) ->
    [];
congen({new, L, P}, Pids, Lams) ->
    C1 = congen(P, Pids, Lams),
    [{flow, {new, L, P}, L}] ++ C1;
congen([{proc, T, P} | Rest], Pids, Lams) ->
    Recs = checkatom(T),
    case Recs of
        [{rec, _, M, _} | _] ->
            C1 = congen(T, Pids, Lams),
            C2 = congen(P, Pids, Lams),
            C3 = congen(Rest, Pids, Lams),
            [{flow, {procid, element(2, P)}, element(2, lists:nth(1, M))}] ++ C1 ++ C2 ++ C3;
        [] ->
            C1 = congen(T, Pids, Lams),
            C2 = congen(P, Pids, Lams),
            C3 = congen(Rest, Pids, Lams),
            C1 ++ C2 ++ C3
    end;
% congen({conc, P1, P2}, Lams) ->
%     C1 = congen(P1, Lams),
%     C2 = congen(P2, Lams),
%     C1 ++ C2;
congen({lam, L, V, E}, Pids, Lams) ->
    Cs = congen(E, Pids, Lams),
    Cs2 = check(V, E),
    [{flow, {lam, L, V, E}, L}] ++ Cs ++ Cs2;
congen({app, L, E1, E2}, Pids, Lams) ->
    C1 = congen(E1, Pids, Lams),
    C2 = congen(E2, Pids, Lams),
    recall(Lams, L, E1, E2) ++ C1 ++ C2;
congen({send, _, E, M}, Pids, Lams) ->
    C1 = congen(E, Pids, Lams),
    C2 = congen(M, Pids, Lams),
    C3 = repid(Pids, E, M),
    C1 ++ C2 ++ C3;
congen({rec, L, M, E}, Pids, Lams) ->
    Cs = congen(E, Pids, Lams),
    Cs2 = check(M, E),
    [{flow, {rec, L, M, E}, L}] ++ Cs ++ Cs2;
congen([], _, _) ->
    [].

recall([], _, _, _) ->
    [];
recall([F | Rest], L, E1, E2) ->
    LE1 = element(2, E1),
    LE2 = element(2, E2),
    LV1 = element(2, lists:nth(1, element(3, F))),
    LE = element(2, element(4, F)),
    C1 = {implies, {flow, F, LE1}, {flow, LE2, LV1}},
    C2 = {implies, {flow, F, LE1}, {flow, LE, L}},
    [C1, C2 | recall(Rest, L, E1, E2)].

repid([], _, _) ->
    [];
repid([F | Rest], E, M) ->
    LE = element(2, E),
    LM = element(2, M),
    Pid = element(3, F),
    C = {implies, {flow, F, LE}, {flow, LM, {procid, Pid}}},
    [C | repid(Rest, E, M)].

process_flow(From, To, Map) ->
    case From of
        {lam, L, V, E} ->
            Fun = fun(I) -> lists:uniq(I ++ [{lam, L, V, E}]) end,
            Newmap = maps:update_with(To, Fun, [{lam, L, V, E}], Map);
        {var, _, _} ->
            % Fun = fun(I) -> lists:uniq(I ++ [Var]) end,
            % Newmap = maps:update_with(To, Fun, [Var], Map);
            Newmap = Map;
        {pid, L, P} ->
            Fun = fun(I) -> lists:uniq(I ++ [{pid, L, P}]) end,
            Newmap = maps:update_with(To, Fun, [{pid, L, P}], Map);
        {new, L, P} ->
            Fun = fun(I) -> lists:uniq(I ++ [{new, L, P}]) end,
            Newmap = maps:update_with(To, Fun, [{new, L, P}], Map);
        {rec, L, M, E} ->
            Fun = fun(I) -> lists:uniq(I ++ [{rec, L, M, E}]) end,
            Newmap = maps:update_with(To, Fun, [{rec, L, M, E}], Map);
        Label ->
            Fun = fun(I) -> lists:uniq(I ++ maps:get(Label, Map)) end,
            Newmap = maps:update_with(To, Fun, maps:get(Label, Map), Map)
    end,
    Newmap.

process_imp(F1, F2, Map) ->
    case element(2, F1) of
        {lam, L, V, E} ->
            case lists:member({lam, L, V, E}, maps:get(element(3, F1), Map)) of
                true ->
                    process_flow(element(2, F2), element(3, F2), Map);
                false ->
                    Map
            end;
        {var, _, Var} ->
            case lists:member(Var, maps:get(element(3, F1), Map)) of
                true ->
                    process_flow(element(2, F2), element(3, F2), Map);
                false ->
                    Map
            end;
        {pid, L, P} ->
            case lists:member({pid, L, P}, maps:get(element(3, F1), Map)) of
                true ->
                    process_flow(element(2, F2), element(3, F2), Map);
                false ->
                    Map
            end;
        Label ->
            case lists:subset(maps:get(Label, Map), maps:get(element(3, F1), Map)) of
                true ->
                    process_flow(element(2, F2), element(3, F2), Map);
                false ->
                    Map
            end
    end.

solve_aux([{flow, From, To} | Rest], Map) ->
    Newmap = process_flow(From, To, Map),
    solve_aux(Rest, Newmap);
solve_aux([{implies, F1, F2} | Rest], Map) ->
    Newmap = process_imp(F1, F2, Map),
    solve_aux(Rest, Newmap);
solve_aux([], Map) ->
    Map.

solve(ConList, Map) ->
    Newmap = solve_aux(ConList, Map),
    case Newmap == Map of
        true ->
            Map;
        false ->
            solve(ConList, Newmap)
    end.

% check(T1, T2) ->
%     case T2 of
%         {flow, _, _} when element(3, T1) == element(3, T2) ->
%             [T2];
%         _ ->
%             []
%     end.

% flowfilter([First | Rest]) ->
%     lists:map(
%         fun(T) -> check(First, T) end,
%         Rest
%     ).

% solve([First | Rest]) ->
%     List = [],
%     case First of
%         {flow, T1, T2} ->
%             [{T2}, {T1}];
%         _ ->
%             []
%     end,
%     List ++ solve(Rest);
% solve([]) ->
%     [].

% solve([First | Rest]) ->
%     case First of
%         {flow, T1, T2} ->
%             [{T2}, {T1}];
%         {implies, F1, F2} when lists:member(F1, Flowlist)->

        




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

% update_with function

%% internal functions
