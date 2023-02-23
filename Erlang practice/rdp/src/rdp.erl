%%%-------------------------------------------------------------------
%% @doc rdp public API
%% @end
%%%-------------------------------------------------------------------

-module(rdp).

-export([exp/1]).

-define(GRAMMAR, [lambda, term]).
-define(lambda, ["(", "\\", var, ".", term, ")"]).
-define(term, [variable, lambda_application]).
-define(variable, [var]).
-define(lambda_application, [term, term]).
add_tuple(Tuple, List) -> [Tuple | List].

rec(Tokens, Output) ->
    case Tokens of
        variable ->
            add_tuple({'var', [var]}, Output),
            rec(rest);
        lambda ->
            add_tuple({'lam', [var], ".", term}, Output),
            rec(rest)
    end.

exp(Input) ->
    Tokens = string:tokens(Input, " "),
    Output = [],

    rec(Tokens, Output).

% convert({var, _, X}) ->
%     {variable, X};
% convert({lambda, _, {var, _, X}, E}) ->
%     {abstraction, X, convert(E)};
% convert({app, _, E1, E2}) ->
%     {application, convert(E1), convert(E2)}.

%% internal functions
