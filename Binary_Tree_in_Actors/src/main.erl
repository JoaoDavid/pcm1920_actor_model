%% @author PC
%% @doc @todo Add description to main.

%  c(main), main:start().
-module(main).

-export([start/0,client/0,client_handle_response/0,binary_search/0,binary_search/1,actor_node/5,insert/6,contains/6, delete/6]).


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
	
	binary_tree_api ! {insert,8},
	binary_tree_api ! {delete,8},
	binary_tree_api ! {contains,8},
	binary_tree_api ! {insert,8},
	binary_tree_api ! {contains,8},
	
	
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
			Root = spawn(main, actor_node, [ValueToInsert,undefined,undefined,self(),true]),
			client ! {insert,ValueToInsert,true},
			binary_search(Root);
		{Op, Value} ->
			client ! {Op,Value,false},
			binary_search()
	end.
			
binary_search(Root) ->
	receive
        {insert, ValueToInsert} ->
			Root ! {insert, ValueToInsert};
		{delete, ValueToDelete} ->
			Root ! {delete, ValueToDelete};
		{contains, ValueToFind} ->
			Root ! {contains, ValueToFind}
    end,
	binary_search_handle_response(),
	binary_search(Root).

actor_node(Value, Left, Right, Father, State) ->
	receive
        {insert, ValueToInsert} ->
			insert(Value, Left, Right, ValueToInsert, Father, State);
		{delete, ValueToDelete} ->
			delete(Value, Left, Right, ValueToDelete, Father, State);
		{contains, ValueToFind} ->
            contains(Value, Left, Right, ValueToFind, Father, State);
		{reincarnate, NewFather} ->			
            NewFather ! {reinsert, Value};
		{reinsert, ValueToReInsert} ->			
            reinsert(Value, Left, Right, ValueToReInsert, Father, State);
		{child_died, Pid} ->			
            if
				Pid == Left ->
					actor_node(Value, undefined, Right, Father, State);
				true ->
					actor_node(Value, Left, undefined, Father, State)
			end
    end.


delete(Value, Left, Right, ValueToDelete, Father, State) ->
	PidInterface = whereis(binary_tree_api),
	case ValueToDelete of 
     	Value ->
			if
				PidInterface == Father ->
					if
						State == true ->							
							binary_tree_api ! {delete, ValueToDelete, true},
							actor_node(Value, Left, Right, Father, false);
						State /= false ->
							binary_tree_api ! {delete, ValueToDelete, does_not_exist},
							actor_node(Value, Left, Right, Father, State)
					end;
				true ->
					Father ! {child_died, self()}
			end,			
			if
				Left /= undefined ->
					Left ! {reincarnate, Father};
				true ->
					ok
			end,
			if
				Right /= undefined ->
					Right ! {reincarnate, Father};
				true ->
					ok
			end,			
			binary_tree_api ! {delete, ValueToDelete, true};	
      	N when N < Value ->
			if 
      		Left == undefined ->
        		binary_tree_api ! {delete, ValueToDelete, does_not_exist},
				actor_node(Value, Left, Right, Father, State);
      		true -> 
         		Left ! {delete, ValueToDelete},
				actor_node(Value, Left, Right, Father, State)
   			end;
		N when N > Value ->
			if 
      		Right == undefined -> 
        		binary_tree_api ! {delete, ValueToDelete, does_not_exist},
				actor_node(Value, Left, Right, Father, State);
      		true -> 
         		Right ! {delete, ValueToDelete},
				actor_node(Value, Left, Right, Father, State)
   			end
	end.

reinsert(Value, Left, Right, ValueToInsert, Father, State) ->
	case ValueToInsert of 
     	Value ->
			actor_node(Value, Left, Right, Father, State);
      	N when N < Value ->
			if 
      		Left == undefined ->
				NewNodePid = spawn(main, actor_node, [ValueToInsert,undefined,undefined,self(),true]),
				actor_node(Value, NewNodePid, Right, Father, State);
      		true -> 
         		Left ! {reinsert, ValueToInsert},
				actor_node(Value, Left, Right, Father, State)
   			end;
		N when N > Value ->
			if 
      		Right == undefined -> 
        		NewNodePid = spawn(main, actor_node, [ValueToInsert,undefined,undefined,self(),true]),
				actor_node(Value, Left, NewNodePid, Father, State);
      		true -> 
         		Right ! {reinsert, ValueToInsert},
				actor_node(Value, Left, Right, Father, State)
   			end
	end.

insert(Value, Left, Right, ValueToInsert, Father, State) ->
	case ValueToInsert of 
     	Value ->
			if
				State == true ->							
					binary_tree_api ! {insert, ValueToInsert, already_exists},
					actor_node(Value, Left, Right, Father, State);
				State == false ->
					binary_tree_api ! {insert, ValueToInsert, true},
					actor_node(Value, Left, Right, Father, true)
			end;
      	N when N < Value ->
			if 
      		Left == undefined ->
				NewNodePid = spawn(main, actor_node, [ValueToInsert,undefined,undefined,self(),true]),
        		binary_tree_api ! {insert, ValueToInsert, true},
				actor_node(Value, NewNodePid, Right, Father, State);
      		true -> 
         		Left ! {insert, ValueToInsert},
				actor_node(Value, Left, Right, Father, State)
   			end;
		N when N > Value ->
			if 
      		Right == undefined -> 
        		NewNodePid = spawn(main, actor_node, [ValueToInsert,undefined,undefined,self(),true]),
        		binary_tree_api ! {insert, ValueToInsert, true},
				actor_node(Value, Left, NewNodePid, Father, State);
      		true -> 
         		Right ! {insert, ValueToInsert},
				actor_node(Value, Left, Right, Father, State)
   			end
	end.

contains(Value, Left, Right, ValueToFind, Father, State) ->
	case ValueToFind of 
     	Value ->
			binary_tree_api ! {contains, ValueToFind, State};
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
	actor_node(Value, Left, Right, Father, State).


