%% @author PC
%% @doc @todo Add description to op_handler.


-module(op_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([insert/7,contains/7,delete/7,create_tree_node/2]).


%% ====================================================================
%% Internal functions
%% ====================================================================

create_tree_node(Value,InterfaceNode) ->
	spawn(bst_actors, tree_node, [Value,undefined,undefined,self(),true,InterfaceNode]).

%Value, Left, Right, IsActive, InterfaceNode, ValueToInsert, Seq
insert(X, Left, Right, IsActive, _, X, _) -> {Left, Right, not IsActive, already_exists};
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