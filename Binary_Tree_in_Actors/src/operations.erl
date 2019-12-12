%% @author Joao David n49448

% Cluster and Multicore Programming
% Department of Informatics
% Faculty of Sciences
% University of Lisbon
% December 11, 2019


-module(operations).

-export([create_tree_node/2,insert/7,contains/7,delete/7]).


% --------------------------- Operations ---------------------------

create_tree_node(Value,InterfaceNode) ->
	spawn(tree_actor, tree_node, [Value,undefined,undefined,self(),true,InterfaceNode]).


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