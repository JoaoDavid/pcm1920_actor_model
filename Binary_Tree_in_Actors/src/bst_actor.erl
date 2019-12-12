%% @author Joao David n49448

% Cluster and Multicore Programming
% Department of Informatics
% Faculty of Sciences
% University of Lisbon
% December 11, 2019

-module(bst_actor).


-export([bst/6]).
-import(operations, [create_tree_node/2]).


% --------------------------- BST Interface ---------------------------

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