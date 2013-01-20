%%%        File : node.erl
%%%      Author : Carl Witt
%%% Description : Contains the logic for participants in the DHT
%%%     Created : 01 Aug 2011 by Carl Witt

-module(node).
-include("include/node.hrl").

-include("include/kademliaGlobal.hrl").
-include("include/systemMonitor.hrl").
-include("include/routingTable.hrl").
-include("include/metric.hrl").

-import(kademliaGlobal).

-export([
			start/1,			% spawn a new node, return PID
			store/3,
			findValue/2,
			findNode/2
		]).

%% =================================================================================================
%% MAIN LOOP
%% =================================================================================================

mainLoop(#nodeState{
			thisNode = ThisNode,
			data = Data, 
			routingTable = RoutingTable} = S) ->

    receive
	
	%% =============================================================================================
	%% JOIN THE KADEMLIA NETWORK
	%% =============================================================================================
	
	% the first node in the network
	{boot, undefined} ->	
		%?LOG("First node on the kademlia net", ?JOINING_THE_NETWORK),
		NewThisNode = ThisNode#peer{pid = self()},
		mainLoop(S#nodeState{ 
		    			thisNode = NewThisNode,
		    			% add node to its own routing table
		    			routingTable = routingTable:add(routingTable:create(NewThisNode),NewThisNode)
		    		});
		
	% ----------------------------------------------------------------------------------------------    
	{boot, InitialNode} -> 
		% the initial node is never the node itself.
	    %?LOG("Received bootstrap node ~p~n", [InitialNode], ?JOINING_THE_NETWORK),
	
		NewThisNode = ThisNode#peer{pid = self()},
		
		% announce arrival in the neighbourhood via a lookup
		InitialNode#peer.pid ! ?FIND_NODE(	
											ThisNode#peer.key,	% target key
											NewThisNode,		% requestor node
											0					% current recursion depth
										),
		
		NewRoutingTable = routingTable:addAll(routingTable:create(NewThisNode),[NewThisNode,InitialNode]),
	    mainLoop(S#nodeState{ 
	    			routingTable = NewRoutingTable,
	    			thisNode = NewThisNode,
	    			fNS = initialFNS([InitialNode], ThisNode#peer.key)
	    			});
	
	%% =============================================================================================
	%% STORE data in the DHT
	%% =============================================================================================
	
	% for the passive node: add tuple to data
	{?STORE, #key{} = Key, Value} ->
	    NewData = lists:keystore(Key, 			% key to compare against
	    						 1, 			% tuple position
	    						 Data, 			% original list
	    						 {Key,Value}),	% new tuple
	    mainLoop(S#nodeState{data=NewData});
	
	% ----------------------------------------------------------------------------------------------    
	% for the active node: retrieve closest nodes and store on them
	{?STORE, KeyString, ValueString} ->
		Key = metric:calculateKey(KeyString),
		Store = fun(#peer{pid = PID}) -> PID ! ?STORE(Key, ValueString) end,
		Send = fun(ResultNodes) -> 
					?LOG("Storing value on ~n~p~n",[ResultNodes], store),
					lists:foreach(Store, ResultNodes)
			   end,
		io:format("~nInitiating store: ~p",[Key]),
		ThisNode#peer.pid ! ?FIND_NODE(Key, Send),
	    mainLoop(S);
	
	%% =============================================================================================
	%% FIND_VALUE
	%% =============================================================================================
	
	% for the passive node: return the value if possible otherwise return close nodes
	{?FIND_VALUE, #key{} = K, #peer{pid = Requester}=RP, RecursionDepth} -> 
		% attempt to add all node contacts the node can get
		NewRoutingTable = routingTable:add(RoutingTable, RP),
		
		Result = lists:keyfind(K, 1, Data),
		if Result /= false ->
			{_,Value} = Result,
			Requester ! ?FIND_VALUE_RESPONSE(Result, ThisNode),
			?LOG("Retrieved value ~p for key ~p (for ~p) ~n", [Value, K, RP], ?FIND_VALUE_PROCEDURE);
		?ELSE ->
			ResultNodes = routingTable:getClosestNNodes(RoutingTable, K, ?BUCKET_SIZE),
			Requester ! ?FIND_VALUE_RESPONSE(ResultNodes, RecursionDepth, ThisNode),
			?LOG("Retrieved close neighbours (for ~s) ~n", [senderFormat(RP)], ?FIND_VALUE_PROCEDURE)
		end,
		mainLoop(S#nodeState{routingTable = NewRoutingTable});

	% ---------------------------------------------------------------------------------------------	
	% for the active node: initiate 
	{?FIND_VALUE, Key} ->
		?LOG("\t\t\t\t->Initiating value lookup for ~p~n",[Key], ?FIND_VALUE_PROCEDURE),
		
		% send the best matches to ask for the key to the node itself
		NodeSelection = routingTable:getClosestNNodes(RoutingTable, Key, ?CONCURRENCY),
		self() ! ?FIND_VALUE_RESPONSE(NodeSelection, 0, ThisNode),
		Fail = fun(ResultNodes) -> 
					io:format("~nFind value: closest nodes processed. If no answer is delayed, the key cannot be found.~n"),
					io:format("~nFound nodes: ~p.~n",[ResultNodes])
			   end,
		InitialFNS = initialFNS(NodeSelection, Key),
		NewFNS = InitialFNS#findNodeState{resultProcessing=Fail},
		mainLoop(S#nodeState{fNS=NewFNS});

	% ---------------------------------------------------------------------------------------------
	% for the active node: proceed
	{?FIND_VALUE_RESPONSE, Nodes, RecursionDepth, Sender} when
	S#nodeState.fNS#findNodeState.recursionDepth /= result_processed ->
		{NewFNS, NewRoutingTable} = handleResponse(?FIND_VALUE_RESPONSE, Nodes, RecursionDepth, S, Sender),
		mainLoop(S#nodeState{fNS=NewFNS, routingTable = NewRoutingTable});
	
	% ---------------------------------------------------------------------------------------------
	% for the active node: abort
	{?FIND_VALUE_RESPONSE, {Key, Value}, Sender} ->
		FNS = S#nodeState.fNS,
		Results = FNS#findNodeState.results,
		if FNS#findNodeState.recursionDepth /= result_processed ->
			io:format("~nValue found: ~p~n",[Value]),
			% remove sender from results
			SenderDist = metric:distance(Sender#peer.key, Key),
			NewResults = gb_trees:delete_any(SenderDist, Results),
			% chose closest to cache value
			{_Distance, {Node, _Queried, _Responded}} = gb_trees:smallest(NewResults),
			Node#peer.pid ! ?STORE(Key, Value),
			?LOG("Caching <~p,~p> on ~s~n",[Key, Value, senderFormat(Node)],?FIND_VALUE_PROCEDURE),
			NewFNS = FNS#findNodeState{recursionDepth = result_processed},
			mainLoop(S#nodeState{fNS=NewFNS});
		?ELSE ->
			%io:format("Result already processed!"),
			mainLoop(S)
		end;
		
	%% =============================================================================================
	%% FIND_NODE (kademlia core procedure)
	%% =============================================================================================
	
	% for the passive node: return the k closest nodes to key this nodes knows
	{?FIND_NODE, #key{} = K, #peer{pid = Requester}=RP, RecursionDepth} ->
		%?LOG("Searching nodes close to ~p for ~p (Round ~p)~n",[K,RP,RecursionDepth], ?FIND_NODE_PROCEDURE),
		Result = routingTable:getClosestNNodes(RoutingTable, K, ?BUCKET_SIZE),

		if Result == [] ->
			io:format("~p~n~p~n",[RoutingTable, Result]),
			?DEBUG("Searching nodes close to ~p for ~p (Round ~p)~n",[K,RP,RecursionDepth]);
		?ELSE -> false end,
		
		% attempt to add all node contacts the node can get
		NewRoutingTable = routingTable:add(RoutingTable, RP),
		
		Requester ! ?FIND_NODE_RESPONSE(Result, RecursionDepth,ThisNode),
		mainLoop(S#nodeState{routingTable = NewRoutingTable});
	
	% ---------------------------------------------------------------------------------------------	
	% for the active node: initiate 
	{?FIND_NODE, Key, ResultProcessing} ->
		?LOG("\t\t\t\t->Initiating node lookup for ~p~n",[Key], ?FIND_NODE_PROCEDURE),
		
		% send the best matches to ask for the key to the node itself
		NodeSelection = routingTable:getClosestNNodes(RoutingTable, Key, ?CONCURRENCY),
		self() ! ?FIND_NODE_RESPONSE(NodeSelection, 0, ThisNode),
		
		InitialFNS = initialFNS(NodeSelection, Key),
		NewFNS = InitialFNS#findNodeState{resultProcessing=ResultProcessing},
		mainLoop(S#nodeState{fNS=NewFNS});

	% ---------------------------------------------------------------------------------------------
	% for the active node: proceed
	{?FIND_NODE_RESPONSE, Nodes, RecursionDepth, Sender} ->
		{NewFNS, NewRoutingTable} = handleResponse(?FIND_NODE_RESPONSE, Nodes, RecursionDepth, S, Sender),
		mainLoop(S#nodeState{fNS=NewFNS, routingTable = NewRoutingTable});
	
	%% =============================================================================================
	%% PING and PONG
	%% =============================================================================================
	
	{ping, Sender, Token} ->
		Sender ! {pong, Token},
		mainLoop(S);
	
	%% =============================================================================================
	%% UTILITIES
	%% =============================================================================================
	{get_routing_table, Sender, Token} ->
		Sender ! {routing_table, routingTable:entries(RoutingTable), Token},
	    mainLoop(S);
	{get_routing_table, Sender} ->
		Sender ! {routing_table, ThisNode, routingTable:entries(RoutingTable)},
		mainLoop(S);
		
	{get_data, Sender, Token} ->
		Sender ! {Data, Token},
	    mainLoop(S);
	    
	print ->
	    printInfo(S),
	    mainLoop(S);
	
	{rtg, Filename} ->
		io:format("~n~p~n",[RoutingTable]),
		graphml:routingTableGraph(RoutingTable, Filename++".graphml"),
		mainLoop(S);
		
	{rpc, Function} ->
		Function(),
		mainLoop(S);
		
	Any -> 
		?LOG("main loop, rejected message/response ~p", [Any], misc),
	    mainLoop(S)

    end. % of mainloop

%% =================================================================================================
%% PRIVATE FUNCTIONS
%% =================================================================================================
	
start(Me) ->
%io:format("Spawning new node ~p with ~p ~n",[Name,Data]),
    PID = spawn(fun() -> 
    				mainLoop(#nodeState{
    									thisNode = Me,	% the pid is inserted on boot
    									data = []
    									}
    						) 
    			end ),
    PID.

initialFNS(NodeSelection, Key) ->
	#findNodeState{
		searchKey = Key,
		results = gb_trees:empty(),
		queried = NodeSelection,		% this must be sorted (and it is because of getClosest...)
		recursionDepth = 1,
		expectedResponses = ?CONCURRENCY,
		closestSoFar = undefined
	}.

%% =================================================================================================
%% Handle responses
%% =================================================================================================

handleResponse(	ResponseType, 
				Nodes, 
				RecursionDepth, 
				#nodeState{routingTable = RoutingTable, fNS = FNS, thisNode = ThisNode}=S, Sender) ->
	
	% Create some alias variables
	SearchKey = FNS#findNodeState.searchKey,
	Results = FNS#findNodeState.results,
	BestSoFar = FNS#findNodeState.closestSoFar,
	NewExpectedResponses = FNS#findNodeState.expectedResponses - 1,

	% some helper functions 
	Strip = fun({Node, _, _}) -> Node end,
	% attach flags unqueried, not yet responded to a node
	Fresh = fun(Node) -> {Node, false, false} end,
		
	if length(Nodes) > 0 ->	
		BestOffered = hd(Nodes),						% returned list must always be sorted!
		NewRoutingTable = routingTable:addAll(RoutingTable, Nodes),
		
		% add nodes to selection and 
		% mark sender as alive (to adress the sender, recalculate the distance)
		SenderDist = metric:distance(Sender#peer.key, SearchKey),
		ExtendedResults = heapInsert(Results, SearchKey, lists:map(Fresh,Nodes)),
		SenderAppears = gb_trees:is_defined(SenderDist, ExtendedResults),
		% update crashes if key is not present, so check before. 
		if SenderAppears ->
			NewResults = gb_trees:update(SenderDist, 						% sender entry
										{Sender,true,true}, 				% mark Queried and Responded
										ExtendedResults						% in this tree
										);
		?ELSE -> % sender is not in the selection (but the selection might have changed due to the reported results)
			NewResults = ExtendedResults
		end;
	?ELSE ->
		NewResults = Results,
		NewRoutingTable = RoutingTable,
		BestOffered = none
	end,

	% show progress (selected nodes with queried/responded flags) 
	if (ThisNode#peer.name == node0) ->
		?LOG("~s",[resultsString(NewResults)],find_node_result_selection);
	?ELSE -> 
		false
	end,
	
	% check whether the result selection is good enough now
	Stable = fun({_, Queried, Responded}) -> Queried and Responded end,
	AllDone = lists:all(Stable, gb_trees:values(NewResults)) or
			  (erlang:is_atom(RecursionDepth)) and 
			  (ResponseType == ?FIND_VALUE_RESPONSE),
	
	if not AllDone ->
		
		% check new results against best node so far
		if BestSoFar == undefined ->																
			  Improves = true;
		   BestOffered /= none ->	% logic or is not lazy in erlang so it crashes when trying to calculate distances.
			  Improves = ( metric:distance(BestOffered#peer.key, SearchKey) < 
				  		   metric:distance(BestSoFar#peer.key, SearchKey) );
		   ?ELSE ->
			  Improves = false
		end,
		
		% check whether this is the last result of the current round
		CloseRound = (RecursionDepth == FNS#findNodeState.recursionDepth) and 
						(NewExpectedResponses == 0),%(NewResponseCount == ?CONCURRENCY),
		
		% if process stagnates try to stabilize/finish result by quering all open contacts
		if not Improves and CloseRound ->
			%Recipients = lists:map(Strip, lists:filter(Unqueried, gb_trees:values(NewResults)));
			Amount = ?BUCKET_SIZE;			
		?ELSE -> % normal proceed
			%BestEntries = lists:filter(Unqueried, gb_trees:values(NewResults)),
			%Recipients = utils:takeIfPossible(lists:map(Strip, BestEntries), ?CONCURRENCY)
			Amount = ?CONCURRENCY
		end,
		
		% retrieve closer nodes
		UpdatedResults = queryBest(
									ResponseType,
									gb_trees:iterator(NewResults), %heapInsert(gb_trees:empty(),SearchKey,Recipients),
									S#nodeState{fNS=FNS#findNodeState{recursionDepth = RecursionDepth}}, % update recursion depth
									Amount,
									gb_trees:empty()
									),
		%io:format("~n~p/~p",[RecursionDepth,ResponseType]),
		NewFNS = FNS#findNodeState{
									results = UpdatedResults,
									recursionDepth = RecursionDepth+1,
									expectedResponses = Amount
								};
	?ELSE -> % all done
	
		Processing = FNS#findNodeState.resultProcessing,
		%io:format("Postprocessing:~n\t~p~n",[Processing]),
		if (erlang:is_function(Processing)) and 
		   (FNS#findNodeState.recursionDepth /= done) ->
			%io:format("Post processing result: ~p",[Processing(lists:map(Strip,gb_trees:values(NewResults)))]),		
			Processing(lists:map(Strip,gb_trees:values(NewResults))),
			NewFNS = FNS#findNodeState{
										recursionDepth = result_processed, 
										resultProcessing = fun(_X)->false end
									   };
		?ELSE ->
			NewFNS = FNS
		end
	end,
	{NewFNS, NewRoutingTable}.
		
	

%% =================================================================================================
%% Forward FIND_NODE to unqueried nodes
%% =================================================================================================


queryBest(_, [], _, _, Accumulator) ->
	Accumulator;
	
% sends a find node or find value query (depending on response type) to the best
% entries in the results selection references by the iterator 
% returns a new result selection with the queried entries marked
queryBest(ResponseType, Iterator, S, Amount, Accumulator) ->
	%?LOG("SendQuery cand=~p howmany=~p",[Candidates,HowMany]),		
	FNS = S#nodeState.fNS,
	% fetch next entry
	{Distance, {Node, Queried, Responded}, NewIterator} = gb_trees:next(Iterator),
	% if candidate was already queried, leave entry unchanged
	% if enough queries have been send, just copy the rest of the table
	if Queried or (Amount == 0) ->
		NewEntry = {Node, Queried, Responded},
		NewAmount = Amount;
	?ELSE ->
		NewEntry = {Node, true, false},
		% send query
		%?LOG("Sending out new query to ~p (Round ~p)",[senderFormat(Node),FNS#findNodeState.recursionDepth+1],ResponseType),
		if ResponseType == ?FIND_VALUE_RESPONSE ->
			Node#peer.pid ! ?FIND_VALUE(FNS#findNodeState.searchKey,
										S#nodeState.thisNode,
										FNS#findNodeState.recursionDepth+1);
		?ELSE ->
			Node#peer.pid ! ?FIND_NODE(FNS#findNodeState.searchKey,
										S#nodeState.thisNode,
										FNS#findNodeState.recursionDepth+1)
		end,
		NewAmount = Amount-1
	end,
	queryBest(ResponseType, NewIterator, S, NewAmount, gb_trees:insert(Distance, NewEntry, Accumulator)).

%% =================================================================================================
%% HELPER FUNCTIONS
%% =================================================================================================

% add the remaining nodes to the results
% Heap			gb_tree structure / priority queue / heap to insert into (result selection)
% SearchKey		reference SearchKey for the distances measured in the queue
% Elements		to insert (if called from handleResponse, a list of {Node, Queried, Responded} tuples)
heapInsert(Heap, SearchKey, Elements) ->
																kademliaGlobal:assert("gb_trees:size(Heap) >= 0",[{'Heap',Heap}]),
																kademliaGlobal:assert("erlang:is_list(Elements)",[{'Elements',Elements}]),
	Insert = fun({Node,_,_}=Elem, AccIn) -> 
																%try kademliaGlobal:assert("{peer, _, _, Key} = Node",[{'Node',Node}]) of
																%	_ -> ok catch error:Error -> io:format("~p / not a peer~n",[Error]) end,
		Key = Node#peer.key,
		Prio = metric:distance(SearchKey,Key),
		ElementExists = gb_trees:is_defined(Prio, AccIn),
		if ElementExists ->
			AccIn;
		?ELSE ->
			gb_trees:insert(Prio, Elem, AccIn)
		end
	end,
	NewHeap = lists:foldl(Insert, Heap, Elements),
	AvailableSpace = ?BUCKET_SIZE - gb_trees:size(NewHeap),
	if (AvailableSpace < 0) ->
		RemoveWorst = fun(Queue) -> 
						{_,_,NewQueue} = gb_trees:take_largest(Queue), 
						NewQueue
					  end,
		iterate(RemoveWorst, NewHeap, abs(AvailableSpace));
	?ELSE ->
		NewHeap
	end.

% applies Function N times to Value, using the output of Function as new input to the next application
iterate(_, Value, 0) -> Value;
iterate(Function, Value, N) -> iterate(Function, Function(Value), N-1).

%addResult(#peer{key=Key}=Peer, AccIn) -> 
%	Prio = metric:distance(SearchKey,Key),
%	ElementExists = gb_trees:is_defined(Prio, AccIn),
%	if ElementExists ->
%		AccIn;
%	?ELSE ->
%		gb_trees:insert(Prio, Peer, AccIn)
%	end.
%addResult(Any,AccIn) ->
%	io:format("Trying to add ~p but should be #peer{}",[Any]),
%	false.
	

printInfo(#nodeState{
			thisNode = ThisNode,
			data = Data, 
			routingTable = RoutingTable}) ->
	Header  = io_lib:format("~n~n============ node ~s INFO ============~n~n", [utils:to_upper(atom_to_list(ThisNode#peer.name))]),
	DataStr = io_lib:format("Data: ~p~n~n", [Data]),
	PeerStr = io_lib:format("Known Peers: ~p~n~n", [RoutingTable]),
	SelfStr = io_lib:format("Me: ~p~n", [ThisNode]),
	Footer  = io_lib:format("~n~n=========================================~n~n", []),
	
	Str = Header ++ DataStr ++ PeerStr ++ SelfStr ++ Footer, 
	
	io:format("~s",[Str]).

% how to display peers
senderFormat(Peer) ->
	io_lib:format("~p",[Peer#peer.name]).

% formats a result selection to a string (gb_tree as priority list)
resultsString(Results) ->
	Header = io_lib:format("~-20s | ~-15s | ~-5s | ~-5s ~n",["Node", "Distance", "Q", "R"]),
	Bar = io_lib:format("\t~55.55.=s~n",[""]),
	Header ++ Bar ++ temporaryResults(Results).
	
temporaryResults(Results) ->
	Empty = gb_trees:size(Results) == 0,
	if Empty ->
		io_lib:format("~n",[]);
	?ELSE ->
		{Distance, Entry, Rest} = gb_trees:take_smallest(Results),
		{Node, Queried, Responded} = Entry,
		Cross = fun(Var) -> if Var -> "X"; ?ELSE -> " " end end,
		ThisLine = io_lib:format("\t~-20s | ~15.15s | ~-5s | ~-5s ~n",[senderFormat(Node), erlang:integer_to_list(Distance), Cross(Queried), Cross(Responded)]),
		(ThisLine ++ temporaryResults(Rest))
	end.
							 
%% =================================================================================================
%% WRAPPER FUNCTIONS for testing purposes on terminal
%% =================================================================================================

store(PID, Key, Value) ->
	PID ! ?STORE(Key, Value).

findValue(PID, String) ->
	PID ! {?FIND_VALUE, metric:calculateKey(String)}.
	
findNode(PID, String) ->
	PID ! ?FIND_NODE(metric:calculateKey(String), fun(Results) -> io:format("~nFound nodes ~n~p",[Results]) end).