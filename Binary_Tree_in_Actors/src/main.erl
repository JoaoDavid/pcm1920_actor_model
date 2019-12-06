%% @author PC
%% @doc @todo Add description to main.

%  c(main), main:main().
-module(main).

-export([main/0,client/0,client_handle_response/0,binary_search/0,binary_search/1,actor_node/3,insert/4,contains/4]).


main() ->
	register(binary_tree_api, spawn(main, binary_search, [])),
	register(client, spawn(main, client, [])).


client() ->
	io:format("Entering Client\n", []),
	binary_tree_api ! {contains,3},
	client_handle_response(),
	binary_tree_api ! {contains,1},
	client_handle_response(),
	binary_tree_api ! {insert,1},
	client_handle_response(),
	binary_tree_api ! {contains,1},
	client_handle_response().


client_handle_response() ->
	receive
        {Op, Value, Response} ->
            io:format("Client Response: ~p ~p ~p\n", [Op, Value, Response])
    end.

binary_search_handle_response() -> 
	receive
		{Op, Value, Response} ->
			client ! {Op,Value,Response}
	end.

binary_search() -> 
	receive
        {insert, ValueToInsert} ->
			Root = spawn(main, actor_node, [ValueToInsert,undefined,undefined]),
			client ! {insert,ValueToInsert,true},
			io:format("Root pid: ~p\n", [Root]),
			binary_search(Root);
		{Op, Value} ->
			client ! {Op,Value,false},
			binary_search()
	end.
			
binary_search(Root) ->
	io:format("Entrei no binary_search root\n", []),
	receive
        {insert, ValueToInsert} ->
			io:format("API: insert ~p\n", [ValueToInsert]),
			Root ! {insert, ValueToInsert};
		{delete, ValueToDelete} ->
			io:format("API: delete ~p\n", [ValueToDelete]),
			Root ! {delete, ValueToDelete};
		{contains, ValueToFind} ->
			io:format("API: contains ~p\n", [ValueToFind]),
			Root ! {contains, ValueToFind}
    end,
	binary_search_handle_response(),
	binary_search(Root).

actor_node(Value, Left, Right) ->
	receive
        {insert, ValueToInsert} ->
            io:format("Node Insert: ~p\n", [ValueToInsert]),
			insert(Value, Left, Right, ValueToInsert);
		{delete, ValueToDelete} ->
            io:format("Node Delete: ~p\n", [ValueToDelete]);
		{contains, ValueToFind} ->
			io:format("Node Contains: ~p\n", [ValueToFind]),
            contains(Value, Left, Right, ValueToFind)
    end.

insert(Value, Left, Right, ValueToInsert) ->
	case ValueToInsert of 
     	Value ->
			io:format("inside insert already exists: ~p\n", [ValueToInsert]),
			binary_tree_api ! {insert, ValueToInsert, already_exists};
      	N when N < Value ->
			if 
      		Left == undefined ->
				NewNodePid = spawn(main, actor_node, [ValueToInsert,undefined,undefined]),
        		binary_tree_api ! {insert, ValueToInsert, true},
				actor_node(Value, NewNodePid, Right);
      		true -> 
         		Left ! {insert, ValueToInsert},
				actor_node(Value, Left, Right)
   			end;
		N when N > Value ->
			if 
      		Right == undefined -> 
        		NewNodePid = spawn(main, actor_node, [ValueToInsert,undefined,undefined]),
        		binary_tree_api ! {insert, ValueToInsert, true},
				actor_node(Value, Left, NewNodePid);
      		true -> 
         		Right ! {insert, ValueToInsert},
				actor_node(Value, Left, Right)
   			end
	end.

contains(Value, Left, Right, ValueToFind) ->
	case ValueToFind of 
     	Value ->
			io:format("inside contains yes, contains: ~p\n", [ValueToFind]),
			binary_tree_api ! {contains, ValueToFind, true};
      	N when N < Value ->
			if 
      		Left == undefined -> 
        		binary_tree_api ! {contains, ValueToFind, false};  
      		true -> 
         		Left ! {contains, ValueToFind}
   			end;
		N when N > Value ->
			if 
      		Right == undefined -> 
        		binary_tree_api ! {contains, ValueToFind, false};
      		true -> 
         		Right ! {contains, ValueToFind}
   			end
	end.

