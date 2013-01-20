%% ----------------------------------------------------
%% CONFIGURATION / MESSAGE TYPES
%% ----------------------------------------------------

-define(PING_FREQUENCY, 120000). 	% automatic pings every 2 minutes
-define(DEFAULT_TTL, 5). 			% default time to live is 3 hops
-define(DROP_TTL, 1).				% queries with lower or equal ttl are dropped

-define(CONCURRENCY, 3).			% how many nodes are concurrently queried when looking up something?

-define(FIND_NODE, find_node).		% requires a key as parameter
-define(FIND_NODE_RESPONSE, find_node_response).
-define(FIND_NODE(Key,ResultProcessing), 
		{?FIND_NODE, Key, ResultProcessing}).
-define(FIND_NODE(Key,Requestor,RecursionDepth), 
		{?FIND_NODE, Key, Requestor, RecursionDepth}).
-define(FIND_NODE_RESPONSE(Result, RecursionDepth, Sender), 
		{?FIND_NODE_RESPONSE, Result, RecursionDepth, Sender}).
-define(AWAITING_FIND_NODE_RESULTS,awaiting_find_node_results).

-define(FIND_VALUE, find_value).
-define(FIND_VALUE_RESPONSE, find_value_response).
-define(FIND_VALUE(Key, Requester, RecursionDepth), {?FIND_VALUE, Key, Requester, RecursionDepth}).
-define(FIND_VALUE_RESPONSE(Result, Sender), 
		{?FIND_VALUE_RESPONSE, Result, Sender}).
-define(FIND_VALUE_RESPONSE(Nodes, RecursionDepth, Sender), 
		{?FIND_VALUE_RESPONSE, Nodes, RecursionDepth, Sender}).
-define(AWAITING_FIND_VALUE_RESULTS,awaiting_find_value_results).

-define(STORE, store).				% requires an #entry as parameter
-define(STORE(Key, Value), 
		{?STORE, Key, Value}).

-define(PING, ping).

%% ----------------------------------------------------
%% DATA TYPES
%% ----------------------------------------------------

% peer
% - information needed to identify and send messages to another node
% pid		PID			as erlang built-in
% name		atom		descriptor for node
% key		#key{}		as defined in metric.hrl
% spawnO.	integer		ordered sequence (index) of creation, starting at zero
-record(peer,{pid,name,key,spawnOrder}).

%-define(THIS, #peer{pid=self(),name=Name}).			% creating a peer record for this process
-define(THIS, S#nodeState.thisNode).					% 

% nodeState
% - data that is carried along the main loop
% thisNode			#peer record that contains all e.g. PID and Key
% data				List of countries and capitals [{country,capital}]
% routingTable		List of known nodes [{#peer{},Number of Data Items(Integer)}]
% fNS				#findNodeState record for node lookup procedure
-record(nodeState,{thisNode, data, routingTable, fNS}).

% findNodeState
% - state data for of the FIND_NODE recursive algorithm
% searchKey			#key record for the initial search key (needed in comparisons later)
% results			Priority queue (see gb_trees) of #peer records that have been returned from other nodes
% recursionDepth	current stage of the recursive process (the number of the most recent round of requests)
% closestSoFar		#peer that has been closest to the desired key during FIND_NODE procedure
% resultProcessing	a function that takes the result nodes (the ordered, closest #peer records) and does something, i.e. send them STORE messages.
-record(findNodeState,{searchKey, results, queried, recursionDepth, expectedResponses, closestSoFar, resultProcessing}).