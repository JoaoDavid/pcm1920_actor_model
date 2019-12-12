%% @author Joao David n49448

% Cluster and Multicore Programming
% Department of Informatics
% Faculty of Sciences
% University of Lisbon
% December 11, 2019


% HOW TO RUN
%
% Open the console in the same directory where the src files are
% 1) start erlang using the command: erl
% 2) run the following command to compile, then run start function
%     c(client_actor), c(operations), c(tree_actor), c(bst_actor), client_actor:start().

% OPERATION MESSAGES
%
% insert:               {insert, Value}
% contains:             {contains, Value}
% delete:               {delete, Value}
% garbage collector:    {gc}
% destroy tree:         {die}

-module(client_actor).
-export([start/0]).
-import(bst_actor, [bst/6]).


% --------------------------- Client ---------------------------

start() ->
	InterfaceNode = spawn(bst_actor, bst, [undefined,self(),0,0,0,0]),
	
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
	
	%print response from bst actor
	client_handle_response().


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


client_handle_response() ->
	Timeout = 120000,
	receive
		{Op, Value, Response} ->
			io:format("Client Response: ~p ~p ~p\n", [Op, Value, Response]),
			client_handle_response();
		{destroyed} ->
			io:format("Binary Search Tree destroyed\n", [])
	after
		Timeout ->
		io:format("Client ending...\n", [])
	end.