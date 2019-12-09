%% @author PC
%% @doc @todo Add description to bst_actors.

%  c(bst_actors), bst_actors:start().
%  regs().

-module(bst_actors).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,bst/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================

start() ->
	InterfaceNode = spawn(bst_actors, bst, [undefined,self()]),
	client_send_random_ops(10,InterfaceNode),
	Messages = erlang:process_info(self(), messages),
	io:format("Client Response: ~p\n", [Messages]),
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

bst_handle_response(ClientPid) -> 
	receive
		{client, Op, Value, Response} ->
			ClientPid ! {Op,Value,Response}
	end.

bst(Root,ClientPid) ->
	receive
        {insert, ValueToInsert} ->
			if
				Root == undefined ->
					NewRoot = spawn(bst_actors, tree_node, [ValueToInsert,undefined,undefined,self(),true]),
					self() ! {client, insert, ValueToInsert, true},
					bst(NewRoot,ClientPid);
				true ->
					Root ! {insert, ValueToInsert}
			end,
			bst_handle_response(ClientPid),
			bst(Root,ClientPid);
		{contains, ValueToFind} ->
			if
				Root == undefined ->
					self() ! {client, contains, ValueToFind, does_not_exist};
				true ->
					Root ! {contains, ValueToFind}
			end,
			bst_handle_response(ClientPid),
			bst(Root,ClientPid);
		{delete, ValueToDelete} ->
			if
				Root == undefined ->
					self() ! {client, contains, ValueToDelete, does_not_exist};
				true ->
					Root ! {delete, ValueToDelete}
			end,
			bst_handle_response(ClientPid),
			bst(Root,ClientPid);	
		{destroy} ->
			Root ! {destroy},
			self() ! {destroy, all, destroyed}
	end.

tree_node(Value,Left,Right,Father,Active) ->
	io:format("tree_node: ~p ~p ~p ~p ~p\n", [Value,Left,Right,Father,Active]),
	tree_node(Value,Left,Right,Father,Active).
