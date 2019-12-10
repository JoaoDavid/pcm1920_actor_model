%% @author Joao David n49448
%% @doc @todo Add description to bst_actors.

%  c(bst_actors), bst_actors:start().
%  regs().

-module(bst_actors).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,bst/3,tree_node/6]).



%% ====================================================================
%% Internal functions
%% ====================================================================

%This constant sets the maximum number of tree actors that had their
%value deleted, but are still alive
-define(MAX_GARBAGE_NODES, 100).

start() ->
	StartPids = erlang:processes(),
	InterfaceNode = spawn(bst_actors, bst, [undefined,self(),0]),
	%client_send_random_ops(101,InterfaceNode),
	client_send_insert_ops(1000,InterfaceNode),
	client_send_delete_ops(1000,InterfaceNode),
	%InterfaceNode ! {garbage_collection},
	Messages = erlang:process_info(self(), messages),
	io:format("Client Messages: ~p\n", [Messages]),	

	
	Messages2 = erlang:process_info(self(), messages),
	io:format("Client Messages: ~p\n", [Messages2]),	
	client_handle_response(0),

	MessagesBst = erlang:process_info(InterfaceNode, messages),
	io:format("Messages in bst: ~p\n", [MessagesBst]),
	EndPids = erlang:processes(),
	io:format("~p StartPids: ~p\n", [length(StartPids),StartPids]),
	io:format("~p EndPids: ~p\n", [length(EndPids),EndPids]).

client_send_delete_ops(Iteration,InterfaceNode) ->
	InterfaceNode ! {delete,Iteration},
	if
		Iteration > 1 ->
			client_send_delete_ops(Iteration-1,InterfaceNode);
		true ->
			done
	end.

client_send_insert_ops(Iteration,InterfaceNode) ->
	InterfaceNode ! {insert,Iteration},
	if
		Iteration > 1 ->
			client_send_insert_ops(Iteration-1,InterfaceNode);
		true ->
			done
	end.
	

client_send_random_ops(Iteration,InterfaceNode) ->
	%OpNumber = rand:uniform(3),
	OpNumber = 1,
	Value = rand:uniform(99),
	case OpNumber of 
     	1 -> InterfaceNode ! {insert,Iteration};
		2 -> InterfaceNode ! {contains,Value};
		3 -> InterfaceNode ! {delete,Value}
	end,
	if
		Iteration > 1 ->
		  client_send_random_ops(Iteration - 1,InterfaceNode);
		true ->
			done
	end.

client_handle_response(Counter) ->
	Timeout = 0,
	receive
        {Op, Value, Response} ->
            io:format("Client Response: ~p ~p ~p\n", [Op, Value, Response]),
			client_handle_response(Counter + 1);
		{destroyed} ->
			io:format("Binary Search Tree destroyed\n", [])
	after
    	Timeout ->
      		io:format("Client ending after ~p miliseconds without new messages\n", [Timeout]),
			io:format("N msgs: ~p\n", [Counter])
    end.

bst(Root,ClientPid,NumDeletes) ->
	receive
		{Op, Value, Response} ->
			ClientPid ! {Op,Value,Response},
			if
				(Op == delete) and (Response == true) ->
					if
						NumDeletes + 1 >= ?MAX_GARBAGE_NODES ->
							self() ! {garbage_collection};
						true ->
							garbage_not_full
					end,
					bst(Root,ClientPid,NumDeletes + 1);
				true ->
					bst(Root,ClientPid,NumDeletes)
			end;			
        {insert, ValueToInsert} ->
			if
				Root == undefined ->
					NewRoot = create_tree_node(ValueToInsert,self()),
					self() ! {insert, ValueToInsert, true},
					bst(NewRoot,ClientPid,NumDeletes);
				true ->
					Root ! {insert, ValueToInsert},
					bst(Root,ClientPid,NumDeletes)
			end;			
		{contains, ValueToFind} ->
			if
				Root == undefined ->
					self() ! {contains, ValueToFind, false};
				true ->
					Root ! {contains, ValueToFind}
			end,
			bst(Root,ClientPid,NumDeletes);
		{delete, ValueToDelete} ->
			if
				Root == undefined ->
					self() ! {delete, ValueToDelete, does_not_exist};
				true ->
					Root ! {delete, ValueToDelete}
			end,
			bst(Root,ClientPid,NumDeletes);	
		{garbage_collection} ->
			if
				Root == undefined ->
					no_root;
				true ->
					Root ! {garbage_collection}
			end,			
			NewRoot = garbage_collection(Root,undefined),
			bst(NewRoot,ClientPid,0);
		{die} ->
			if
				Root == undefined ->
					die;
				true ->
					Root ! {die}
			end
	end.

create_tree_node(Value,InterfaceNode) ->
	Pid = spawn(bst_actors, tree_node, [Value,undefined,undefined,self(),true,InterfaceNode]),
	%register(list_to_atom("node"++integer_to_list(Value)), Pid).
	Pid.

gc_tree_node(Value,Left,Right,IsActive,InterfaceNode) ->
	if
		IsActive ->							
			InterfaceNode ! {reincarnate, Value};
		true ->
			collect_garbage
	end,
	if 
      	Left == undefined ->
			no_left_child;
      	Left /= undefined -> 
         	Left ! {garbage_collection}
   	end,
	if 
      	Right == undefined ->
			no_right_child;
      	Right /= undefined -> 
         	Right ! {garbage_collection}
   	end.

die_tree_node(Left,Right) ->
	if 
      	Left == undefined ->
			no_left_child;
      	Left /= undefined -> 
         	Left ! {die}
   	end,
	if 
      	Right == undefined ->
			no_right_child;
      	Right /= undefined -> 
         	Right ! {die}
   	end.

tree_node(Value,Left,Right,Father,IsActive,InterfaceNode) ->
	receive
    	{insert, ValueToInsert} ->
			{L, R, IsA} = insert(Value, Left, Right, IsActive, InterfaceNode, ValueToInsert),
			tree_node(Value,L,R,Father,IsA,InterfaceNode);
		{contains, ValueToFind} ->
            contains(Value, Left, Right, IsActive, InterfaceNode, ValueToFind),
			tree_node(Value,Left,Right,Father,IsActive,InterfaceNode);
		{delete, ValueToDelete} ->
			{L, R, IsA} = delete(Value, Left, Right, IsActive, InterfaceNode, ValueToDelete),
			tree_node(Value,L,R,Father,IsA,InterfaceNode);
		{garbage_collection} ->
			gc_tree_node(Value,Left,Right,IsActive,InterfaceNode);
		{die} ->
			die_tree_node(Left,Right)
    end.


insert(Value, Left, Right, IsActive, InterfaceNode, ValueToInsert) ->
	case ValueToInsert of 
     	Value ->
			if
			IsActive ->							
				InterfaceNode ! {insert, ValueToInsert, already_exists},
				{Left, Right, IsActive};
			true ->
				InterfaceNode ! {insert, ValueToInsert, true},
				{Left, Right, true}
			end;
      	N when N < Value ->
			if 
      		Left == undefined ->
				NewNodePid = create_tree_node(ValueToInsert, InterfaceNode),
        		InterfaceNode ! {insert, ValueToInsert, true},
				{NewNodePid, Right, IsActive};
      		Left /= undefined -> 
         		Left ! {insert, ValueToInsert},
				{Left, Right, IsActive}
   			end;
		N when N > Value ->
			if 
      		Right == undefined -> 
        		NewNodePid = create_tree_node(ValueToInsert, InterfaceNode),
        		InterfaceNode ! {insert, ValueToInsert, true},
				{Left, NewNodePid, IsActive};
      		Right /= undefined -> 
         		Right ! {insert, ValueToInsert},
				{Left, Right, IsActive}
   			end
	end.

contains(Value, Left, Right, IsActive, InterfaceNode, ValueToFind) ->
	case ValueToFind of 
     	Value ->
			InterfaceNode ! {contains, ValueToFind, IsActive};
      	N when N < Value ->
			if 
      		Left == undefined -> 
        		InterfaceNode ! {contains, ValueToFind, false};  
      		Left /= undefined -> 
         		Left ! {contains, ValueToFind}
   			end;
		N when N > Value ->
			if 
      		Right == undefined -> 
        		InterfaceNode ! {contains, ValueToFind, false};
      		Right /= undefined -> 
         		Right ! {contains, ValueToFind}
   			end
	end.

delete(Value, Left, Right, IsActive, InterfaceNode, ValueToDelete) ->
	case ValueToDelete of 
     	Value ->
			if
			IsActive ->							
				InterfaceNode ! {delete, ValueToDelete, true},
				{Left, Right, false};
			true ->
				InterfaceNode ! {delete, ValueToDelete, does_not_exist},
				{Left, Right, IsActive}
			end;
      	N when N < Value ->
			if 
      		Left == undefined ->
				InterfaceNode ! {delete, ValueToDelete, does_not_exist},
				{Left, Right, IsActive};
      		Left /= undefined -> 
         		Left ! {delete, ValueToDelete},
				{Left, Right, IsActive}
   			end;
		N when N > Value ->
			if 
      		Right == undefined -> 
        		InterfaceNode ! {delete, ValueToDelete, does_not_exist},
				{Left, Right, IsActive};
      		Right /= undefined -> 
         		Right ! {delete, ValueToDelete},
				{Left, Right, IsActive}
   			end
	end.

die(Left, Right) ->
	if 
    	Left == undefined ->
			no_left_child;
      	Left /= undefined -> 
         	Left ! {die}
   	end,
	if 
    	Right == undefined ->
			no_left_child;
      	Right /= undefined -> 
         	Right ! {die}
   	end.

garbage_collection(OldRoot,Root) ->
	receive
        {reincarnate, ValueToInsert} ->
			if
				Root == undefined ->
					NewRoot = create_tree_node(ValueToInsert,self()),
					garbage_collection(OldRoot,NewRoot);
				true ->
					Root ! {insert, ValueToInsert},
					garbage_collection(OldRoot,Root)
			end
	after
    	0 ->
      		Root
    end.