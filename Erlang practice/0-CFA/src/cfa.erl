%%%-------------------------------------------------------------------
%% @doc 0-CFA public API
%% @end
%%%-------------------------------------------------------------------

-module(cfa).

-export([main/0]).

-type var() :: string().
-type lit() :: atom() | integer() | float() | [].
-type pat() :: var() | lit() | [pat() | pat()].

main() ->
    % Define the input code as a string
    Input = "",

    % Split the string into separate lines
    Lines = string:tokens(Input, ";"),

    % Process each line to perform the 0-CFA analysis
    lists:foreach(fun process_line/1, Lines).

process_line(Line) ->
    % Trim whitespaces from the beginning and end of the line
    Line = string:strip(Line),

    % Skip empty lines
    if
        Line == "" ->
            ok;
        true ->
            % if the line is not empty just go through it
            % Analyze the line using the 0-CFA algorithm
            % (Implementation left as an exercise for the reader)
            io:format("0-CFA analysis: ~p~n", [Line])
    end.

%% internal functions
