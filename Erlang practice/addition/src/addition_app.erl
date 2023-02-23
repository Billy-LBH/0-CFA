%%%-------------------------------------------------------------------
%% @doc addition public API
%% @end
%%%-------------------------------------------------------------------

-module(addition_app).

-export([add/2, start/0]).

add(X, Y) ->
    Sum = X+Y,
    io:format("~w~n",[Sum]).

start() ->
    add(3,5).

%% internal functions
