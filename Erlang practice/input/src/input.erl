%%%-------------------------------------------------------------------
%% @doc input public API
%% @end
%%%-------------------------------------------------------------------

-module(input).
-export([parse_input/1]).

parse_input(Input) ->
    [Command | Args] = string:tokens(Input, " "),
    process_command(Command, Args).

process_command("add", [Arg1, Arg2]) ->
    Arg1Int = list_to_integer(Arg1),
    Arg2Int = list_to_integer(Arg2),
    Result = Arg1Int + Arg2Int,
    io:format("Result: ~p~n", [Result]).

%% internal functions
