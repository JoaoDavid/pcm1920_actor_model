%% @author PC
%% @doc @todo Add description to main.

%  c(main), main:main().
-module(main).

-export([main/0,client/1,binary_search/3,contains/5,binary_search_handle_response/3]).

-record(actor_node,{left,right}).
-record(binary_search,{root=#actor_node{}}).



main() ->
	register(binary_search_root, spawn(main, binary_search, [99,undefined,undefined])),
	register(client, spawn(main, client, [3])),
	io:format("Main finished\n", []).


client(N) ->
	io:format("Entering Client\n", []),
	binary_search_root ! {contains,99},
	receive
        Response ->
            io:format("Client Response: ~p", [Response])
    end,
	if 
      N > 0 ->
		  client(N-1);
      true -> 
         io:format("Finished Client\n", [])
   end.
	


binary_search(Value, Left, Right) ->
	io:format("value:~p left:~p right:~p \n", [Value,Left,Right]),
	receive
        {insert, Value} ->
            io:format("Handling: ~s~n", [Value]),
            binary_search(Value, Left, Right);
		{delete, Value} ->
            io:format("Handling: ~s~n", [Value]),
            binary_search(Value, Left, Right);
		{contains, ValueToFind} ->
			io:format("contains?: ~p\n", [ValueToFind]),
            contains(Value, Left, Right, binary_search, ValueToFind)
    end,
	binary_search_handle_response(Value, Left, Right).

binary_search_handle_response(Value, Left, Right) ->
	io:format("handling response ~p \n", []),
	receive
		{contains, ValueToFind, Response} ->
			io:format("contains?: ~p\n", [ValueToFind]),
			client ! Response
    end,
	binary_search(Value, Left, Right).

actor_node(Value, Left, Right) ->
	receive
        {insert, Value} ->
			1;
		{delete, Value} ->
			1;
		{contains, ValueToFind} ->
            contains(Value, Left, Right, actor_node, ValueToFind)
    end.

contains(Value, Left, Right, F, ValueToFind) ->
	case ValueToFind of 
     	Value ->
			io:format("inside contains yes, contains: ~p\n", [ValueToFind]),
			binary_search_root ! {contains, ValueToFind, true};
      	N when N < Value ->
			if 
      		Left == undefined -> 
        		binary_search_root ! {contains, ValueToFind, false};  
      		true -> 
         		Left ! {contains, ValueToFind}
   			end;
		N when N > Value ->
			if 
      		Right == undefined -> 
        		binary_search_root ! {contains, ValueToFind, false};
      		true -> 
         		Right ! {contains, ValueToFind}
   			end
	end.
	%F(Value, Left, Right).


