%% @author Joao David n49448
%% @doc @todo Add description to bst_actors.

%  c(bst_actors), bst_actors:start().
%  regs().

-module(bst_actors).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,bst/6,tree_node/6]).



%% ====================================================================
%% Internal functions
%% ====================================================================

%This constant is the trigger value to start a garbage collection
%when the number of deleted tree actors that are still running
%reach this value, the tree starts garbage collecting
-define(MAX_GARBAGE_NODES, -1).

start() ->
	StartPids = erlang:processes(),
	InterfaceNode = spawn(bst_actors, bst, [undefined,self(),0,0,0,0]),
	%client_send_random_ops(101,InterfaceNode,99),
	client_send_ops(insert,20000,InterfaceNode),	
	client_send_ops(delete,500,InterfaceNode),
	InterfaceNode ! {gc},
	client_send_ops(delete,20000,InterfaceNode),
	InterfaceNode ! {gc},
	InterfaceNode ! {insert,3},
	%InterfaceNode ! {die},
	Messages = erlang:process_info(self(), messages),
	io:format("Client Messages: ~p\n", [Messages]),	
	
	
	
	client_handle_response(0),
	Messages2 = erlang:process_info(self(), messages),
	io:format("Client Messages: ~p\n", [Messages2]),
	MessagesBst = erlang:process_info(InterfaceNode, messages),
	io:format("Messages in bst: ~p\n", [MessagesBst]),
	
	%processes
	EndPids = erlang:processes(),
	%io:format("~p StartPids: ~p\n", [length(StartPids),StartPids]),
	%io:format("~p EndPids: ~p\n", [length(EndPids),EndPids]),
	io:format("Diff: ~p\n", [length(EndPids)-length(StartPids)]).

client_send_ops(Op,Iteration,InterfaceNode) ->
	InterfaceNode ! {Op,Iteration},
	if
		Iteration > 1 ->
			client_send_ops(Op,Iteration-1,InterfaceNode);
		true ->
			done
	end.


client_send_random_ops(Iteration,InterfaceNode,MaxRandomValue) ->
	OpNumber = rand:uniform(3),
	Value = rand:uniform(MaxRandomValue),	
	if
		Iteration > 0 ->
			case OpNumber of 
				1 -> InterfaceNode ! {insert,Iteration};
				2 -> InterfaceNode ! {contains,Value};
				3 -> InterfaceNode ! {delete,Value}
			end,
			client_send_random_ops(Iteration - 1,InterfaceNode,MaxRandomValue);
		true ->
			done
	end.

client_handle_response(Counter) ->
	Timeout = 120000,
	receive
		{Op, Value, Response} ->
			io:format("Client Response: ~p ~p ~p\n", [Op, Value, Response]),
			client_handle_response(Counter + 1);
		{destroyed} ->
			io:format("Binary Search Tree destroyed\n", [])
	after
		Timeout ->
		io:format("Client ending...\n", []),
		io:format("N msgs: ~p\n", [Counter])
	end.


bst_gc(Root) ->
	receive
		{copy, ValueToInsert} ->			
			Root ! {copy, ValueToInsert},
			bst_gc(Root);
		{resume} ->			
			ok
	end.


bst_gc_new(Root) ->
	receive
		{copy, ValueToInsert} ->
			if
				Root == undefined -> 
					bst_gc_new(create_tree_node(ValueToInsert, self()));
				true -> 
					Root ! {copy, ValueToInsert},
					bst_gc_new(Root)
			end;
		{resume} ->			
			Root
	end.


bst_gc_two(Root,0) -> ok;
bst_gc_two(Root,N) ->
	receive
		{copy, ValueToInsert} ->			
			Root ! {copy, ValueToInsert},
			bst_gc_two(Root,N-1);
		{resume} ->			
			bst_gc_two(Root,0)
	end.

bst_gc_one(Root) ->
	receive
		{copy, ValueToInsert} ->			
			Root ! {copy, ValueToInsert},
			bst_gc_one(Root)
	after
		0 ->
		ok
	end.

bst(Root,ClientPid,NumDeletes,RecSeq,SentSeq,NumActive) ->
	receive
		{insert, _, _, -1} ->
			bst(Root,ClientPid,NumDeletes,RecSeq,SentSeq,NumActive);
		{insert, Value, true, Seq} when Seq == SentSeq ->
			ClientPid ! {insert,Value,true},
			bst(Root,ClientPid,NumDeletes,RecSeq,SentSeq+1,NumActive+1);
		{delete, Value, true, Seq} when Seq == SentSeq ->
			ClientPid ! {delete,Value,true},
			bst(Root,ClientPid,NumDeletes,RecSeq,SentSeq+1,NumActive-1);
		{Op, Value, Response, Seq} when Seq == SentSeq ->
			ClientPid ! {Op,Value,Response},
			bst(Root,ClientPid,NumDeletes,RecSeq,SentSeq+1,NumActive);		
		{insert, ValueToInsert} ->
			if
				Root == undefined ->
					NewRoot = create_tree_node(ValueToInsert,self()),
					self() ! {insert, ValueToInsert, true, RecSeq},
					bst(NewRoot,ClientPid,NumDeletes,RecSeq+1,SentSeq,NumActive);
				true ->
					Root ! {insert, ValueToInsert,RecSeq},
					bst(Root,ClientPid,NumDeletes,RecSeq+1,SentSeq,NumActive)
			end;			
		{contains, ValueToFind} ->
			if
				Root == undefined ->
					self() ! {contains, ValueToFind, false, RecSeq};
				true ->
					Root ! {contains, ValueToFind, RecSeq}
			end,
			bst(Root,ClientPid,NumDeletes,RecSeq+1,SentSeq,NumActive);
		{delete, ValueToDelete} ->
			if
				Root == undefined ->
					self() ! {delete, ValueToDelete, does_not_exist, RecSeq};
				true ->
					Root ! {delete, ValueToDelete, RecSeq}
			end,
			bst(Root,ClientPid,NumDeletes,RecSeq+1,SentSeq,NumActive);	
		{gc} ->
			self() ! {collecting,garbage,true, RecSeq},
			io:format("Lets collect garbage: RecSeq ~p  SentSeq ~p\n", [RecSeq,SentSeq]),
			if
				Root == undefined ->
					no_root;
				true ->
					Root ! {gc}
			end,			
			%NewRoot = create_tree_node(1,self()),
			io:format("NumActive ~p  \n", [NumActive]),
			NewRoot = bst_gc_new(undefined),
			%bst_gc(NewRoot),
			%bst_gc_one(NewRoot),
			%bst_gc_two(NewRoot,1500),
			%bst_gc(NewRoot),
			%NewRoot = bst_gc_new(undefined),
			io:format("after bst_gc(NewRoot), \n", []),
			io:format("after self() ! {resume},, \n", []),
			bst(NewRoot,ClientPid,NumDeletes,RecSeq+1,SentSeq,NumActive);
		{die} ->
			if
				Root == undefined ->
					die;
				true ->
					Root ! {die}
			end,
			ClientPid ! {destroyed};
		{resume} ->	bst(Root,ClientPid,NumDeletes,RecSeq,SentSeq,NumActive)
	end.

create_tree_node(Value,InterfaceNode) ->
	spawn(bst_actors, tree_node, [Value,undefined,undefined,self(),true,InterfaceNode]).

gc_tree_node(Value,undefined,undefined,IsActive,InterfaceNode) ->
	if
		IsActive ->							
			InterfaceNode ! {copy, Value};
		true ->
			collect_garbage
	end,
	InterfaceNode ! {resume};
gc_tree_node(Value,Left,Right,IsActive,InterfaceNode) ->
	if
		IsActive ->							
			InterfaceNode ! {copy, Value};
		true ->
			collect_garbage
	end,
	if 
		Left == undefined ->
			no_left_child;
		Left /= undefined -> 
			Left ! {gc}
	end,
	if 
		Right == undefined ->
			no_right_child;
		Right /= undefined -> 
			Right ! {gc}
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
		{insert, ValueToInsert, Seq} ->
			{L, R, IsA, Res} = insert(Value, Left, Right, IsActive, InterfaceNode, ValueToInsert, Seq),
			if
				(Res /= undefined) and (Seq /= -1) -> InterfaceNode ! {insert, ValueToInsert, Res, Seq};
				true -> dk
			end,
			tree_node(Value,L,R,Father,IsA,InterfaceNode);
		{contains, ValueToFind, Seq} ->
			contains(Value, Left, Right, IsActive, InterfaceNode, ValueToFind, Seq),
			tree_node(Value,Left,Right,Father,IsActive,InterfaceNode);
		{delete, ValueToDelete, Seq} ->
			{L, R, IsA} = delete(Value, Left, Right, IsActive, InterfaceNode, ValueToDelete, Seq),
			tree_node(Value,L,R,Father,IsA,InterfaceNode);
		{gc} ->
			gc_tree_node(Value,Left,Right,IsActive,InterfaceNode);
		{die} ->
			die_tree_node(Left,Right);
		{copy, ValueToInsert} ->
			{L, R, IsA, Res} = insert(Value, Left, Right, IsActive, InterfaceNode, ValueToInsert, -1),
			tree_node(Value,L,R,Father,IsA,InterfaceNode)
	end.

copy(Value, Left, Right, IsActive, InterfaceNode, ValueToInsert, Seq) ->
	case ValueToInsert of 
		Value ->
			if
				IsActive ->
					{Left, Right, IsActive, already_exists};
				true ->
					{Left, Right, true, true}
			end;
		N when N < Value ->
			if 
				Left == undefined ->
					NewNodePid = create_tree_node(ValueToInsert, InterfaceNode),
					{NewNodePid, Right, IsActive, true};
				Left /= undefined -> 
					Left ! {insert, ValueToInsert, Seq},
					{Left, Right, IsActive, undefined}
			end;
		N when N > Value ->
			if 
				Right == undefined -> 
					NewNodePid = create_tree_node(ValueToInsert, InterfaceNode),
					{Left, NewNodePid, IsActive, true};
				Right /= undefined -> 
					Right ! {insert, ValueToInsert, Seq},
					{Left, Right, IsActive, undefined}
			end
	end.

insert(Value, Left, Right, IsActive, InterfaceNode, ValueToInsert, Seq) ->
	case ValueToInsert of 
		Value ->
			if
				IsActive ->
					{Left, Right, IsActive, already_exists};
				true ->
					{Left, Right, true, true}
			end;
		N when N < Value ->
			if 
				Left == undefined ->
					NewNodePid = create_tree_node(ValueToInsert, InterfaceNode),
					{NewNodePid, Right, IsActive, true};
				Left /= undefined -> 
					Left ! {insert, ValueToInsert, Seq},
					{Left, Right, IsActive, undefined}
			end;
		N when N > Value ->
			if 
				Right == undefined -> 
					NewNodePid = create_tree_node(ValueToInsert, InterfaceNode),
					{Left, NewNodePid, IsActive, true};
				Right /= undefined -> 
					Right ! {insert, ValueToInsert, Seq},
					{Left, Right, IsActive, undefined}
			end
	end.

contains(Value, Left, Right, IsActive, InterfaceNode, ValueToFind, Seq) ->
	case ValueToFind of 
		Value ->
			InterfaceNode ! {contains, ValueToFind, IsActive, Seq};
		N when N < Value ->
			if 
				Left == undefined -> 
					InterfaceNode ! {contains, ValueToFind, false, Seq};  
				Left /= undefined -> 
					Left ! {contains, ValueToFind, Seq}
			end;
		N when N > Value ->
			if 
				Right == undefined -> 
					InterfaceNode ! {contains, ValueToFind, false, Seq};
				Right /= undefined -> 
					Right ! {contains, ValueToFind, Seq}
			end
	end.

delete(Value, Left, Right, IsActive, InterfaceNode, ValueToDelete, Seq) ->
	case ValueToDelete of 
		Value ->
			if
				IsActive ->							
					InterfaceNode ! {delete, ValueToDelete, true, Seq},
					{Left, Right, false};
				true ->
					InterfaceNode ! {delete, ValueToDelete, does_not_exist, Seq},
					{Left, Right, IsActive}
			end;
		N when N < Value ->
			if 
				Left == undefined ->
					InterfaceNode ! {delete, ValueToDelete, does_not_exist, Seq},
					{Left, Right, IsActive};
				Left /= undefined -> 
					Left ! {delete, ValueToDelete, Seq},
					{Left, Right, IsActive}
			end;
		N when N > Value ->
			if 
				Right == undefined -> 
					InterfaceNode ! {delete, ValueToDelete, does_not_exist, Seq},
					{Left, Right, IsActive};
				Right /= undefined -> 
					Right ! {delete, ValueToDelete, Seq},
					{Left, Right, IsActive}
			end
	end.
