%%%-------------------------------------------------------------------
%% @doc parser public API
%% @end
%%%-------------------------------------------------------------------

-module(parser).

-export([parse/1]).

parse(Input) ->
    {Expr, Rest} = parse_expr(Input),
    case Rest of
        [] -> {Expr};
        _ -> {error, "empty input"}
    end.

parse_expr([F | Rest]) when F >= $a, F =< $z; F >= $A, F =< $Z ->
    % check if the first character of a string is a letter, ASCII
    parse_var([F | Rest]);
parse_expr([$\s | Rest]) ->
    parse_expr(Rest);
parse_expr([$(, $/ | Rest]) ->
    {V1, [$. | Rest1]} = parse_expr(Rest),
    {E1, [$) | Rest2]} = parse_expr(Rest1),
    {{lam, V1, '.', E1}, Rest2};
parse_expr([$( | Rest]) ->
    {E1, Rest1} = parse_expr(Rest),
    {E2, [$) | Rest2]} = parse_expr(Rest1),
    {{app, E1, E2}, Rest2};
parse_expr([]) ->
    {error, "empty input"}.

parse_var([F | Rest]) when F >= $a, F =< $z; F >= $A, F =< $Z ->
    % if satisfy the condition, add var before the tuple
    {[var, F], Rest};
parse_var([$\s | Rest]) ->
    parse_var(Rest);
parse_var(_) ->
    {error, "empty input"}.

%% internal functions
