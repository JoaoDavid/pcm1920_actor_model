%% @author PC
%% @doc @todo Add description to main.


-module(main).

-export([main/0]).

-record(actor_node,{left,right}).
-record(binary_search,{root=#actor_node{}}).

returnsValue() ->
	40.

main() ->
	Var = returnsValue(),
	io:format("The value is: ~p.", [Var]).

actor_node() ->
	receive
        {insert, Value} ->
            io:format("Handling: ~s~n", [Value]),
            actor_node();
		{delete, Value} ->
            io:format("Handling: ~s~n", [Value]),
            actor_node();
		{contains} ->
            actor_node()
    end.

