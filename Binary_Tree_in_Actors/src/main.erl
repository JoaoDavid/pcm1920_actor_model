%% @author PC
%% @doc @todo Add description to main.

%  c(main), main:main().
-module(main).

-export([main/0,client/1,binary_search/0,binary_search/3,insert/4,contains/4,binary_search_handle_response/0,client_handle_response/0,node_handle_request/3]).


main() ->
	register(binary_search_root, spawn(main, binary_search, [3,undefined,undefined])),
	register(client, spawn(main, client, [3])).


client(N) ->
	io:format("Entering Client\n", []),
	binary_search_root ! {contains,3},
	client_handle_response(),
	binary_search_root ! {contains,1},
	client_handle_response(),
	binary_search_root ! {insert,1},
	client_handle_response(),
	binary_search_root ! {contains,1},
	client_handle_response().


client_handle_response() ->
	receive
        {Op, Value, Response} ->
            io:format("Client Response: ~p ~p ~p\n", [Op, Value, Response])
    end.
	

binary_search() -> binary_search(undefined,undefined,undefined).
binary_search(Value, Left, Right) ->
	node_handle_request(Value, Left, Right),
	binary_search_handle_response(),
	binary_search(Value, Left, Right).

node_handle_request(Value, Left, Right) ->
	receive
        {insert, ValueToInsert} ->
            io:format("Insert: ~p\n", [ValueToInsert]),
			insert(Value, Left, Right, ValueToInsert);
		{delete, ValueToDelete} ->
            io:format("Delete: ~p\n", [ValueToDelete]);
		{contains, ValueToFind} ->
			io:format("Contains: ~p\n", [ValueToFind]),
            contains(Value, Left, Right, ValueToFind)
    end.

binary_search_handle_response() ->
	io:format("handling response ~p \n", []),
	receive
		{contains, ValueToFind, Response} ->
			io:format("binary_search_handle_response, contains?: ~p\n", [ValueToFind]),
			client ! {contains, ValueToFind, Response};
		{insert, ValueToInsert, Response} ->
			io:format("binary_search_handle_response, insert?: ~p\n", [ValueToInsert]),
			client ! {insert, ValueToInsert, Response}
    end.

actor_node(Value, Left, Right) ->
	node_handle_request(Value, Left, Right).

insert(Value, Left, Right, ValueToInsert) ->
	RootPid = whereis(binary_search_root),
	case ValueToInsert of 
     	Value ->
			io:format("inside insert already exists: ~p\n", [ValueToInsert]),
			binary_search_root ! {insert, ValueToInsert, already_exists};
      	N when N < Value ->
			if 
      		Left == undefined ->
				io:format("Left == undefined\n", []),
				NewNodePid = spawn(main, actor_node, [ValueToInsert,undefined,undefined]),
        		binary_search_root ! {insert, ValueToInsert, true},
				if
					self() == RootPid ->
						io:format("self() == binary_search_root: ~p\n", [self() == RootPid]),
						binary_search(Value, NewNodePid, Right);
					true ->
						io:format("self() != binary_search_root\n", []),
						actor_node(Value, NewNodePid, Right)
				end;
      		true -> 
         		Left ! {insert, ValueToInsert}
   			end;
		N when N > Value ->
			if 
      		Right == undefined -> 
        		NewNodePid = spawn(main, actor_node, [ValueToInsert,undefined,undefined]),
        		binary_search_root ! {insert, ValueToInsert, true},
				if
					self() == RootPid ->
						binary_search(Value, Left, NewNodePid);
					true ->
						actor_node(Value, Left, NewNodePid)
				end;
      		true -> 
         		Right ! {insert, ValueToInsert}
   			end
	end.

contains(Value, Left, Right, ValueToFind) ->
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


