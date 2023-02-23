%%%-------------------------------------------------------------------
%% @doc test public API
%% @end
%%%-------------------------------------------------------------------

-module(test_app).

-export([start/0]).

start() ->
    Input = "Tomorrow ;is Saturday",
    Lines = string:tokens(Input, ";"),
    io:format("~p~n", [Lines]).

%% internal functions
