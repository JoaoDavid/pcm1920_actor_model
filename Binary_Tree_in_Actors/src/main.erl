%% @author PC
%% @doc @todo Add description to main.

%  c(main), main:start().
-module(main).

-export([start/0,client/0,client_handle_response/0,binary_search/0,binary_search/1,actor_node/3,insert/4,contains/4, delete/5]).


start() ->
	register(binary_tree_api, spawn(main, binary_search, [])),
	register(client, spawn(main, client, [])).


client() ->
	io:format("Entering Client\n", []),
	binary_tree_api ! {insert,8},
	binary_tree_api ! {insert,4},
	binary_tree_api ! {insert,12},
	binary_tree_api ! {contains,8},
	binary_tree_api ! {contains,4},
	binary_tree_api ! {contains,12},

	binary_tree_api ! {insert,2},
	binary_tree_api ! {insert,6},
	binary_tree_api ! {insert,10},
	binary_tree_api ! {insert,14},

	binary_tree_api ! {contains,8},
	binary_tree_api ! {contains,4},
	binary_tree_api ! {contains,12},
	binary_tree_api ! {contains,2},
	binary_tree_api ! {contains,6},
	binary_tree_api ! {contains,10},
	binary_tree_api ! {contains,14},

	binary_tree_api ! {delete,4},
	binary_tree_api ! {insert,5},
	binary_tree_api ! {contains,5},
	binary_tree_api ! {contains,6},
	binary_tree_api ! {contains,4},
	binary_tree_api ! {contains,2},

	binary_tree_api ! {delete,10},
	binary_tree_api ! {insert,10},
	binary_tree_api ! {contains,13},
	binary_tree_api ! {contains,10},
	binary_tree_api ! {contains,14},
	
	
	
	client_handle_response_forever().
	
client_handle_response_forever() ->
	client_handle_response(),
	client_handle_response_forever().

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
			%io:format("Root pid: ~p\n", [Root]),
			binary_search(Root);
		{Op, Value} ->
			client ! {Op,Value,false},
			binary_search()
	end.
			
binary_search(Root) ->
	receive
        {insert, ValueToInsert} ->
			%io:format("API: insert ~p\n", [ValueToInsert]),
			Root ! {insert, ValueToInsert};
		{delete, ValueToDelete} ->
			%io:format("API: delete ~p\n", [ValueToDelete]),
			Root ! {delete, ValueToDelete, self()};
		{contains, ValueToFind} ->
			%io:format("API: contains ~p\n", [ValueToFind]),
			Root ! {contains, ValueToFind}
    end,
	binary_search_handle_response(),
	binary_search(Root).

actor_node(Value, Left, Right) ->
	%Father whose child died
	receive
        {insert, ValueToInsert} ->
			insert(Value, Left, Right, ValueToInsert);
		{delete, ValueToDelete, Father} ->
			delete(Value, Left, Right, ValueToDelete, Father);
		{contains, ValueToFind} ->
            contains(Value, Left, Right, ValueToFind);
		{die, Father} ->			
            Father ! {reinsert, Value};
		{reinsert, ValueToReInserted} ->			
            reinsert(Value, Left, Right, ValueToReInserted);
		{child_died, Pid} ->			
            if
				Pid == Left ->
					actor_node(Value, undefined, Right);
				true ->
					actor_node(Value, Left, undefined)
			end
    end.


delete(Value, Left, Right, ValueToDelete, Father) ->
	case ValueToDelete of 
     	Value ->
			Father ! {child_died, self()},
			if
				Left /= undefined ->
					Left ! {die, Father};
				true ->
					ok
			end,
			if
				Right /= undefined ->
					Right ! {die, Father};
				true ->
					ok
			end,			
			binary_tree_api ! {delete, ValueToDelete, true};	
      	N when N < Value ->
			if 
      		Left == undefined ->
        		binary_tree_api ! {delete, ValueToDelete, does_not_exist},
				actor_node(Value, Left, Right);
      		true -> 
         		Left ! {delete, ValueToDelete, self()},
				actor_node(Value, Left, Right)
   			end;
		N when N > Value ->
			if 
      		Right == undefined -> 
        		binary_tree_api ! {delete, ValueToDelete, does_not_exist},
				actor_node(Value, Left, Right);
      		true -> 
         		Right ! {delete, ValueToDelete, self()},
				actor_node(Value, Left, Right)
   			end
	end.

reinsert(Value, Left, Right, ValueToInsert) ->
	case ValueToInsert of 
     	Value ->
			actor_node(Value, Left, Right);
      	N when N < Value ->
			if 
      		Left == undefined ->
				NewNodePid = spawn(main, actor_node, [ValueToInsert,undefined,undefined]),
				actor_node(Value, NewNodePid, Right);
      		true -> 
         		Left ! {reinsert, ValueToInsert},
				actor_node(Value, Left, Right)
   			end;
		N when N > Value ->
			if 
      		Right == undefined -> 
        		NewNodePid = spawn(main, actor_node, [ValueToInsert,undefined,undefined]),
				actor_node(Value, Left, NewNodePid);
      		true -> 
         		Right ! {reinsert, ValueToInsert},
				actor_node(Value, Left, Right)
   			end
	end.

insert(Value, Left, Right, ValueToInsert) ->
	case ValueToInsert of 
     	Value ->
			binary_tree_api ! {insert, ValueToInsert, already_exists},
			actor_node(Value, Left, Right);
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
	end,
	actor_node(Value, Left, Right).


