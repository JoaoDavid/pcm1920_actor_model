%% @author Joao David n49448

%  c(bst_actor_model), bst_actor_model:start().
%  regs().
-module(bst_actor_model).

-export([start/0,binary_search_tree/0,actor_node/5]).

%----------------------------- CLIENT -----------------------------

start() ->
	register(bst_node, spawn(bst_actor_model, binary_search_tree, [])),
	register(client_node, self()),	
	io:format("Client starting to send messages\n", []),
	client_send_random_ops(1000),
	bst_node ! {destroy},
	client_handle_response().

client_send_random_ops(Iteration) ->
	OpNumber = rand:uniform(3),
	Value = rand:uniform(1000),
	case OpNumber of 
     	1 -> bst_node ! {insert,Value};
		2 -> bst_node ! {contains,Value};
		3 -> bst_node ! {delete,Value}
	end,
	if
		Iteration > 0 ->
		  client_send_random_ops(Iteration - 1);
		true ->
			done
	end.

			
	
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

binary_search_tree_handle_response() -> 
	receive
		{Op, Value, Response} ->
			client_node ! {Op,Value,Response}
	end.

binary_search_tree() -> 
	receive
        {insert, ValueToInsert} ->
			Root = spawn(bst_actor_model, actor_node, [ValueToInsert,undefined,undefined,self(),true]),
			register(list_to_atom("node"++integer_to_list(ValueToInsert)), Root),
			client_node ! {insert,ValueToInsert,true},
			binary_search_tree(Root);
		{destroy} ->
			client_node ! {destroyed};
		{Op, Value} ->
			client_node ! {Op,Value,does_not_exist},
			binary_search_tree()
	end.
			
binary_search_tree(Root) ->
	receive
        {insert, ValueToInsert} ->
			Root ! {insert, ValueToInsert},
			binary_search_tree_handle_response(),
			binary_search_tree(Root);
		{delete, ValueToDelete} ->
			Root ! {delete, ValueToDelete},
			binary_search_tree_handle_response(),
			binary_search_tree(Root);
		{contains, ValueToFind} ->
			Root ! {contains, ValueToFind},
			binary_search_tree_handle_response(),
			binary_search_tree(Root);
		{destroy} ->
			Root ! {destroy},
			client_node ! {destroyed}					
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
	PidInterface = whereis(bst_node),
	case ValueToDelete of 
     	Value ->
			if
				PidInterface == Father ->
					if
						State == true ->							
							bst_node ! {delete, ValueToDelete, true},
							actor_node(Value, Left, Right, Father, false);
						State == false ->
							bst_node ! {delete, ValueToDelete, does_not_exist},
							actor_node(Value, Left, Right, Father, State)
					end;
				PidInterface /= Father ->
					Father ! {child_died, self()},
					bst_node ! {delete, ValueToDelete, true}
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
        		bst_node ! {delete, ValueToDelete, does_not_exist},
				actor_node(Value, Left, Right, Father, State);
      		Left /= undefined -> 
         		Left ! {delete, ValueToDelete},
				actor_node(Value, Left, Right, Father, State)
   			end;
		N when N > Value ->
			if 
      		Right == undefined -> 
        		bst_node ! {delete, ValueToDelete, does_not_exist},
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
				NewNodePid = spawn(bst_actor_model, actor_node, [ValueToInsert,undefined,undefined,self(),true]),
				%register(list_to_atom("node"++integer_to_list(ValueToInsert)), NewNodePid),
				actor_node(Value, NewNodePid, Right, Father, State);
      		Left /= undefined -> 
         		Left ! {reinsert, ValueToInsert},
				actor_node(Value, Left, Right, Father, State)
   			end;
		N when N > Value ->
			if 
      		Right == undefined -> 
        		NewNodePid = spawn(bst_actor_model, actor_node, [ValueToInsert,undefined,undefined,self(),true]),
				%register(list_to_atom("node"++integer_to_list(ValueToInsert)), NewNodePid),
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
					bst_node ! {insert, ValueToInsert, already_exists},
					actor_node(Value, Left, Right, Father, State);
				State == false ->
					bst_node ! {insert, ValueToInsert, true},
					actor_node(Value, Left, Right, Father, true)
			end;
      	N when N < Value ->
			if 
      		Left == undefined ->
				NewNodePid = spawn(bst_actor_model, actor_node, [ValueToInsert,undefined,undefined,self(),true]),
				%register(list_to_atom("node"++integer_to_list(ValueToInsert)), NewNodePid),
        		bst_node ! {insert, ValueToInsert, true},
				actor_node(Value, NewNodePid, Right, Father, State);
      		Left /= undefined -> 
         		Left ! {insert, ValueToInsert},
				actor_node(Value, Left, Right, Father, State)
   			end;
		N when N > Value ->
			if 
      		Right == undefined -> 
        		NewNodePid = spawn(bst_actor_model, actor_node, [ValueToInsert,undefined,undefined,self(),true]),
				%register(list_to_atom("node"++integer_to_list(ValueToInsert)), NewNodePid),
        		bst_node ! {insert, ValueToInsert, true},
				actor_node(Value, Left, NewNodePid, Father, State);
      		Right /= undefined -> 
         		Right ! {insert, ValueToInsert},
				actor_node(Value, Left, Right, Father, State)
   			end
	end.

contains(Value, Left, Right, ValueToFind, State) ->
	case ValueToFind of 
     	Value ->
			bst_node ! {contains, ValueToFind, State};
      	N when N < Value ->
			if 
      		Left == undefined -> 
        		bst_node ! {contains, ValueToFind, false};  
      		Left /= undefined -> 
         		Left ! {contains, ValueToFind}
   			end;
		N when N > Value ->
			if 
      		Right == undefined -> 
        		bst_node ! {contains, ValueToFind, false};
      		Right /= undefined -> 
         		Right ! {contains, ValueToFind}
   			end
	end.