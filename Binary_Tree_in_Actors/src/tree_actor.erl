%% @author Joao David n49448

% Cluster and Multicore Programming
% Department of Informatics
% Faculty of Sciences
% University of Lisbon
% December 11, 2019


-module(tree_actor).

-export([gc_tree_node/5,die_tree_node/2,tree_node/6]).
-import(operations, [create_tree_node/2,insert/7,contains/7,delete/7]).


% --------------------------- Tree Node ---------------------------



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