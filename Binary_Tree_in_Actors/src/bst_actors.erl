%% @author PC
%% @doc @todo Add description to bst_actors.

%  c(bst_actors), bst_actors:start().
%  regs().

-module(bst_actors).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,bst/2,tree_node/6]).



%% ====================================================================
%% Internal functions
%% ====================================================================

start() ->
	InterfaceNode = spawn(bst_actors, bst, [undefined,self()]),
	%client_send_random_ops(10,InterfaceNode),
	Messages = erlang:process_info(self(), messages),
	io:format("Client Messages: ~p\n", [Messages]),	

	InterfaceNode ! {insert,8},
	InterfaceNode ! {insert,4},
	InterfaceNode ! {insert,12},
	InterfaceNode ! {insert,2},
	InterfaceNode ! {insert,14},
	InterfaceNode ! {delete,12},
	InterfaceNode ! {delete,2},
	InterfaceNode ! {garbage_collection},
	InterfaceNode ! {contains,8},
	InterfaceNode ! {contains,4},
	InterfaceNode ! {contains,12},
	InterfaceNode ! {contains,2},
	InterfaceNode ! {contains,14},
	
	Messages2 = erlang:process_info(self(), messages),
	io:format("Client Messages: ~p\n", [Messages2]),	
	client_handle_response(),

	MessagesBst = erlang:process_info(InterfaceNode, messages),
	io:format("Messages in bst: ~p\n", [MessagesBst]).
	
client_send_random_ops(Iteration,InterfaceNode) ->
	%OpNumber = rand:uniform(3),
	OpNumber = 1,
	Value = rand:uniform(99),
	case OpNumber of 
     	1 -> InterfaceNode ! {insert,Value};
		2 -> InterfaceNode ! {contains,Value};
		3 -> InterfaceNode ! {delete,Value}
	end,
	if
		Iteration > 0 ->
		  client_send_random_ops(Iteration - 1,InterfaceNode);
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

bst(Root,ClientPid) ->
	receive
		{Op, Value, Response} ->
			ClientPid ! {Op,Value,Response},
			bst(Root,ClientPid);
        {insert, ValueToInsert} ->
			if
				Root == undefined ->
					NewRoot = create_tree_node(ValueToInsert,self()),
					self() ! {insert, ValueToInsert, true},
					bst(NewRoot,ClientPid);
				true ->
					Root ! {insert, ValueToInsert},
					bst(Root,ClientPid)
			end;			
		{contains, ValueToFind} ->
			if
				Root == undefined ->
					self() ! {contains, ValueToFind, does_not_exist};
				true ->
					Root ! {contains, ValueToFind}
			end,
			bst(Root,ClientPid);
		{delete, ValueToDelete} ->
			if
				Root == undefined ->
					self() ! {delete, ValueToDelete, does_not_exist};
				true ->
					Root ! {delete, ValueToDelete}
			end,
			bst(Root,ClientPid);	
		{garbage_collection} ->
			Root ! {garbage_collection},
			NewRoot = garbage_collection(Root,undefined),
			bst(NewRoot,ClientPid)
	end.

create_tree_node(Value,InterfaceNode) ->
	spawn(bst_actors, tree_node, [Value,undefined,undefined,self(),true,InterfaceNode]).

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
			Left ! die,
			%TODO check if different than undefined
			Right ! die
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
    	2000 ->
      		Root
    end.