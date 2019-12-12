%% @author Joao David n49448
%% @doc Binary Search Tree using the Actor Model

% HOW TO RUN
%
% Open the console in the same directory where this file is
% 1) start erlang using the command: erl
% 2) run the following command to compile, then run start function
%     c(bst_actors), bst_actors:start().

% OPERATION MESSAGES
%
% insert:               {insert, Value}
% contains:             {contains, Value}
% delete:               {delete, Value}
% garbage collector:    {gc}
% destroy tree:         {die}

-module(bst_actors).
-export([start/0,bst/6,tree_node/6]).

% --------------------------- Client ---------------------------

start() ->
	StartPids = erlang:processes(),
	InterfaceNode = spawn(bst_actors, bst, [undefined,self(),0,0,0,0]),


	client_send_ops(insert,20,InterfaceNode),	
	client_send_ops(delete,10,InterfaceNode),
	client_send_ops(contains,20,InterfaceNode),
	
	%collect garbage
	InterfaceNode ! {gc},
	client_send_ops(contains,20,InterfaceNode),
	
	%random operations
	client_send_random_ops(5000,InterfaceNode,500),	
	
	%destroy tree
	InterfaceNode ! {die}, 
	
	
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

client_send_ops(_,0,_) -> done;
client_send_ops(Op,Iteration,InterfaceNode) ->
	InterfaceNode ! {Op,Iteration},
	client_send_ops(Op,Iteration-1,InterfaceNode).


client_send_random_ops(0,_,_) -> done;
client_send_random_ops(Iteration,InterfaceNode,MaxRandomValue) ->
	OpNumber = rand:uniform(4),
	Value = rand:uniform(MaxRandomValue),	
	case OpNumber of 
		1 -> InterfaceNode ! {insert,Iteration};
		2 -> InterfaceNode ! {contains,Value};
		3 -> InterfaceNode ! {delete,Value};
		4 -> InterfaceNode ! {gc}
	end,
	client_send_random_ops(Iteration - 1,InterfaceNode,MaxRandomValue).


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

% --------------------------- BST ---------------------------

bst_gc(Root) ->
	receive
		{copy, ValueToInsert} ->
			if
				Root == undefined -> 
					bst_gc(create_tree_node(ValueToInsert, self()));
				true -> 
					Root ! {copy, ValueToInsert},
					bst_gc(Root)
			end;
		{resume} ->			
			Root
	end.

bst(Root,ClientPid,NumDeletes,RecSeq,SentSeq,NumActive) ->
	receive
		{die, tree_destroyed, true, Seq} when Seq == SentSeq ->
			ClientPid ! {destroyed};
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
			self() ! {garbage,collector,true, RecSeq},
			if
				Root == undefined ->
					no_root;
				true ->
					Root ! {gc}
			end,
			NewRoot = bst_gc(undefined),
			bst(NewRoot,ClientPid,NumDeletes,RecSeq+1,SentSeq,NumActive);
		{die} ->
			if
				Root == undefined ->
					die;
				true ->
					Root ! {die}
			end,
			self() ! {die, tree_destroyed, true, RecSeq},
			bst(Root,ClientPid,NumDeletes,RecSeq+1,SentSeq,NumActive);
		{resume} ->	bst(Root,ClientPid,NumDeletes,RecSeq,SentSeq,NumActive)
	end.

% --------------------------- Tree Node ---------------------------

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
			{L, R, IsA, _} = insert(Value, Left, Right, IsActive, InterfaceNode, ValueToInsert, -1),
			tree_node(Value,L,R,Father,IsA,InterfaceNode)
	end.

% --------------------------- Operations ---------------------------

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