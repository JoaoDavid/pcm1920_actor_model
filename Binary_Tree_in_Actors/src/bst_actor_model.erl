%% @author Joao David n49448

%  c(bst_actor_model), bst_actor_model:start().
%  regs().
-module(bst_actor_model).

-export([start/0,binary_search/0,actor_node/5]).

%----------------------------- CLIENT -----------------------------

start() ->
	register(binary_tree_interface, spawn(main, binary_search, [])),
	register(client, self()),
	
	io:format("Client starting to send messages\n", []),
	binary_tree_interface ! {insert,8},
	binary_tree_interface ! {insert,4},
	binary_tree_interface ! {insert,12},
	binary_tree_interface ! {contains,8},
	binary_tree_interface ! {contains,4},
	binary_tree_interface ! {contains,12},

	binary_tree_interface ! {insert,2},
	binary_tree_interface ! {insert,6},
	binary_tree_interface ! {insert,10},
	binary_tree_interface ! {insert,14},

	binary_tree_interface ! {contains,8},
	binary_tree_interface ! {contains,4},
	binary_tree_interface ! {contains,12},
	binary_tree_interface ! {contains,2},
	binary_tree_interface ! {contains,6},
	binary_tree_interface ! {contains,10},
	binary_tree_interface ! {contains,14},

	binary_tree_interface ! {delete,4},
	binary_tree_interface ! {insert,5},
	binary_tree_interface ! {contains,5},
	binary_tree_interface ! {contains,6},
	binary_tree_interface ! {contains,4},
	binary_tree_interface ! {contains,2},

	binary_tree_interface ! {delete,10},
	binary_tree_interface ! {insert,10},
	binary_tree_interface ! {contains,13},
	binary_tree_interface ! {contains,10},
	binary_tree_interface ! {contains,14},
	
	binary_tree_interface ! {insert,8},
	binary_tree_interface ! {delete,8},
	binary_tree_interface ! {contains,8},
	binary_tree_interface ! {insert,8},
	binary_tree_interface ! {contains,8},
	%timer:sleep(3000),
	binary_tree_interface ! {destroy},
	client_handle_response().
	
client_handle_response() ->
	Timeout = 6000,
	receive
        {Op, Value, Response} ->
            io:format("Client Response: ~p ~p ~p\n", [Op, Value, Response]),
			client_handle_response();
		{destroyed} ->
			io:format("Binary Search Tree destroyed\n", [])
	after
    	Timeout ->
      		io:format("Client ending after ~p miliseconds without new messages\n", [Timeout])
    end.

%----------------------------- BINARY SEARCH TREE -----------------------------

binary_search_handle_response() -> 
	receive
		{Op, Value, Response} ->
			client ! {Op,Value,Response}
	end.

binary_search() -> 
	receive
        {insert, ValueToInsert} ->
			Root = spawn(main, actor_node, [ValueToInsert,undefined,undefined,self(),true]),
			register(list_to_atom("node"++integer_to_list(ValueToInsert)), Root),
			client ! {insert,ValueToInsert,true},
			binary_search(Root);
		{destroy} ->
			client ! {destroyed};
		{Op, Value} ->
			client ! {Op,Value,does_not_exist},
			binary_search()
	end.
			
binary_search(Root) ->
	receive
        {insert, ValueToInsert} ->
			Root ! {insert, ValueToInsert},
			binary_search_handle_response(),
			binary_search(Root);
		{delete, ValueToDelete} ->
			Root ! {delete, ValueToDelete},
			binary_search_handle_response(),
			binary_search(Root);
		{contains, ValueToFind} ->
			Root ! {contains, ValueToFind},
			binary_search_handle_response(),
			binary_search(Root);
		{destroy} ->
			Root ! {destroy},
			client ! {destroyed}					
    end.

%----------------------------- ACTOR NODE -----------------------------

actor_node(Value, Left, Right, Father, State) ->
	receive
        {insert, ValueToInsert} ->
			insert(Value, Left, Right, ValueToInsert, Father, State);
		{delete, ValueToDelete} ->
			delete(Value, Left, Right, ValueToDelete, Father, State);
		{contains, ValueToFind} ->
            contains(Value, Left, Right, ValueToFind, State),
			actor_node(Value, Left, Right, Father, State);
		{reincarnate, NewFather} ->			
            NewFather ! {reinsert, Value};
		{reinsert, ValueToReInsert} ->			
            reinsert(Value, Left, Right, ValueToReInsert, Father, State);
		{child_died, Pid} ->			
            if
				Pid == Left ->
					actor_node(Value, undefined, Right, Father, State);
				Pid == Right ->
					actor_node(Value, Left, undefined, Father, State)
			end;
		{destroy} ->
			actor_node_destroy(Left, Right)
    end.

actor_node_destroy(Left, Right) ->
	if
		Left /= undefined ->
			Left ! {destroy};
		true ->
			no_left_child
	end,
	if
		Right /= undefined ->
			Right ! {destroy};
		true ->
			no_right_child
	end.

delete(Value, Left, Right, ValueToDelete, Father, State) ->
	PidInterface = whereis(binary_tree_interface),
	case ValueToDelete of 
     	Value ->
			if
				PidInterface == Father ->
					if
						State == true ->							
							binary_tree_interface ! {delete, ValueToDelete, true},
							actor_node(Value, Left, Right, Father, false);
						State == false ->
							binary_tree_interface ! {delete, ValueToDelete, does_not_exist},
							actor_node(Value, Left, Right, Father, State)
					end;
				PidInterface /= Father ->
					Father ! {child_died, self()},
					binary_tree_interface ! {delete, ValueToDelete, true}
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
			end;
      	N when N < Value ->
			if 
      		Left == undefined ->
        		binary_tree_interface ! {delete, ValueToDelete, does_not_exist},
				actor_node(Value, Left, Right, Father, State);
      		Left /= undefined -> 
         		Left ! {delete, ValueToDelete},
				actor_node(Value, Left, Right, Father, State)
   			end;
		N when N > Value ->
			if 
      		Right == undefined -> 
        		binary_tree_interface ! {delete, ValueToDelete, does_not_exist},
				actor_node(Value, Left, Right, Father, State);
      		Right /= undefined -> 
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
				register(list_to_atom("node"++integer_to_list(ValueToInsert)), NewNodePid),
				actor_node(Value, NewNodePid, Right, Father, State);
      		Left /= undefined -> 
         		Left ! {reinsert, ValueToInsert},
				actor_node(Value, Left, Right, Father, State)
   			end;
		N when N > Value ->
			if 
      		Right == undefined -> 
        		NewNodePid = spawn(main, actor_node, [ValueToInsert,undefined,undefined,self(),true]),
				register(list_to_atom("node"++integer_to_list(ValueToInsert)), NewNodePid),
				actor_node(Value, Left, NewNodePid, Father, State);
      		Right /= undefined -> 
         		Right ! {reinsert, ValueToInsert},
				actor_node(Value, Left, Right, Father, State)
   			end
	end.

insert(Value, Left, Right, ValueToInsert, Father, State) ->
	case ValueToInsert of 
     	Value ->
			if
				State == true ->							
					binary_tree_interface ! {insert, ValueToInsert, already_exists},
					actor_node(Value, Left, Right, Father, State);
				State == false ->
					binary_tree_interface ! {insert, ValueToInsert, true},
					actor_node(Value, Left, Right, Father, true)
			end;
      	N when N < Value ->
			if 
      		Left == undefined ->
				NewNodePid = spawn(main, actor_node, [ValueToInsert,undefined,undefined,self(),true]),
				register(list_to_atom("node"++integer_to_list(ValueToInsert)), NewNodePid),
        		binary_tree_interface ! {insert, ValueToInsert, true},
				actor_node(Value, NewNodePid, Right, Father, State);
      		Left /= undefined -> 
         		Left ! {insert, ValueToInsert},
				actor_node(Value, Left, Right, Father, State)
   			end;
		N when N > Value ->
			if 
      		Right == undefined -> 
        		NewNodePid = spawn(main, actor_node, [ValueToInsert,undefined,undefined,self(),true]),
				register(list_to_atom("node"++integer_to_list(ValueToInsert)), NewNodePid),
        		binary_tree_interface ! {insert, ValueToInsert, true},
				actor_node(Value, Left, NewNodePid, Father, State);
      		Right /= undefined -> 
         		Right ! {insert, ValueToInsert},
				actor_node(Value, Left, Right, Father, State)
   			end
	end.

contains(Value, Left, Right, ValueToFind, State) ->
	case ValueToFind of 
     	Value ->
			binary_tree_interface ! {contains, ValueToFind, State};
      	N when N < Value ->
			if 
      		Left == undefined -> 
        		binary_tree_interface ! {contains, ValueToFind, false};  
      		Left /= undefined -> 
         		Left ! {contains, ValueToFind}
   			end;
		N when N > Value ->
			if 
      		Right == undefined -> 
        		binary_tree_interface ! {contains, ValueToFind, false};
      		Right /= undefined -> 
         		Right ! {contains, ValueToFind}
   			end
	end.