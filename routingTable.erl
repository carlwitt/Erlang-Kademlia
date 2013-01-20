%%%      Module : routingData
%%%      Author : Carl Witt
%%% Description : offers the abstract data type to store routing information
%%%     Created : 04 Aug 2011 by Carl Witt
%%%
%%% Abstract: Routing Information is organized in a binary tree which leafs are k-buckets.
%%% The path (1/0 for left/right) to a bucket is the prefix all nodes in this bucket share.

-module(routingTable).
-include("include/routingTable.hrl").
-include("include/kademliaGlobal.hrl").
-include("include/systemMonitor.hrl").
-include("include/metric.hrl").
-include("include/node.hrl").

-export([	
			create/1, 
			add/2, 
			addAll/2,
			getClosestN/3,
			getClosestNNodes/3,
			refresh/2,
			send/2,
			entries/1
		]).


%% ----------------------------------------------------
%% IMPLEMENTATION
%% ----------------------------------------------------

% returns an empty routingData structure
% in the beginning all nodes are stored in a single bucket, sharing no common prefix (length = 0)
create(#peer{}=Owner) -> 
	Root = #leaf{bucket = []},
	#routingData{owner = Owner, root = Root}.
	
% -------------------------------------------------------------------------------------------------
addAll(	RD, ListOfRoutingEntries ) ->
	
	Insert = fun(RoutingEntry, AccIn) -> add(AccIn, RoutingEntry) end,
	lists:foldl(Insert, RD, ListOfRoutingEntries).
	

% -------------------------------------------------------------------------------------------------
add(	#routingData{owner = Owner, root = Root} = RD,
		RoutingEntry	) ->
	% replace the tree by the result of the insert operation
	RD#routingData{root = insertHelper(Root, fresh(RoutingEntry), Owner, 0)}.

% creates an updated tree with the routing entry inserted, if allowed
% subtree			#leaf or #innerNode
% RoutingEntry		#routingEntry record to insert
% Owner				#peer record of the owner (to determine splittability)
% prefixLength		length of the already common prefix while moving down the tree
insertHelper(#leaf{bucket = Bucket}=Leaf, RoutingEntry, Owner, PrefixLength) -> 
	Full = length(Bucket) == ?BUCKET_SIZE,
	Split = bucketContainsNode(Bucket, Owner),	
	if
		Full and Split ->
			if Owner#peer.name == node0 ->
				%io:format("~nSplit to insert ~p bucket ~p~n",[RoutingEntry,Bucket]),
				%io:format("Result: ~p~n",[insertHelper(split(Leaf, PrefixLength), RoutingEntry, Owner, PrefixLength+1)]),
				insertHelper(split(Leaf, PrefixLength), RoutingEntry, Owner, PrefixLength);
			true -> insertHelper(split(Leaf, PrefixLength), RoutingEntry, Owner, PrefixLength)
			end;
		Full ->
			OldestPeer = (hd(Bucket))#routingEntry.peer,
			PID = OldestPeer#peer.pid,
			%io:format("Full, ping ~s/~p~n",[kademliaGlobal:senderFormat(Oldest#routingEntry.peer),PID]),
			Token = make_ref(),
			PID!{ping, self(), Token},
			receive
				{pong, Token} ->
					%io:format("   "),
					insertSorted(Bucket, fresh(OldestPeer))		% update responder to most recently seen (but drop new contact)
				after 100 ->
					%Time out! Replace contact
					%io:format(" X "),
					insertSorted(Bucket, RoutingEntry)
			end,
			Leaf;
		?ELSE -> % not full
			Leaf#leaf{bucket = insertSorted(Bucket, RoutingEntry)}
	end;

insertHelper(	#innerNode{zero = Zero, one = One} = IN, 
				RoutingEntry, 
				Owner, 
				PrefixLength	) ->
	Key = RoutingEntry#routingEntry.peer#peer.key#key.hash,
	NextBitIndex = ?KEY_BITS - (PrefixLength + 1),
	NextBit = metric:nthBit(Key, NextBitIndex),
	if NextBit -> % one
		IN#innerNode{one = insertHelper(One, RoutingEntry, Owner, PrefixLength+1)};
	?ELSE -> % zero
		IN#innerNode{zero = insertHelper(Zero, RoutingEntry, Owner, PrefixLength+1)}
	end.	

% -------------------------------------------------------------------------------------------------
% splits a leaf and sorts the bucket contents to the newly available prefixes
% returns #innerNode
split(	#leaf{bucket = Bucket} = Leaf,
		PrefixLength	) ->
	NextBit = ?KEY_BITS - (PrefixLength + 1),
	ContinuesWithZero = fun(#routingEntry{peer=#peer{key=#key{hash=Hash}}}) -> metric:nthBit(Hash,NextBit) == false end,
	{ZeroBucket, OneBucket} = lists:partition(ContinuesWithZero, Bucket),
	#innerNode{ 
				zero = Leaf#leaf{bucket = ZeroBucket}, 
				one = Leaf#leaf{bucket = OneBucket}
				}.

% -------------------------------------------------------------------------------------------------
% return the N closest entries of the routing table,
% according to the metric distance to the key. 
% the results MUST be sorted with the best at the top of the list.
getClosestN(	#routingData{root = Root}, 
			Key, N 	) ->
	{_, Results } = retrieveHelper(Root, 	% tree
									 Key, 	% search key
									 N, 	% how many
									 0 ),	% prefix length
	Results.

% creates an updated tree with the routing entry inserted, if allowed
% subtree			#leaf or #innerNode
% SearchKey			#target ID
% Amount			how many to fetch left
% prefix length		current level of the tree
% returns: {HowManyLeft, Results} where HowManyLeft is the remaining count of nodes to fetch
% and results is the list of #peer records which were already fetched
retrieveHelper(	#leaf{bucket = Bucket}, 
				_, 
				Amount, 
				_) -> 
	% best matches are at the bottom
	Hits = utils:takeIfPossible(lists:reverse(Bucket), Amount),
	{Amount - length(Hits), Hits};

retrieveHelper(	#innerNode{zero = Zero, one = One}, 
				SearchKey, 
				Amount, 
				PrefixLength	) ->
	Hash = SearchKey#key.hash,
	NextBitIndex = ?KEY_BITS - (PrefixLength + 1),
	NextBit = metric:nthBit(Hash, NextBitIndex),
	if NextBit -> % one
		FirstChoice = One, SecondChoice = Zero;
	?ELSE -> % zero
		FirstChoice = Zero, SecondChoice = One
	end,
	{HowManyLeft, Results} = retrieveHelper(FirstChoice, SearchKey, Amount, PrefixLength+1),
	if HowManyLeft > 0 ->
		{OtherHowManyLeft, OtherResults} = retrieveHelper(SecondChoice, SearchKey, HowManyLeft, PrefixLength+1),
		{OtherHowManyLeft, Results++OtherResults};
	?ELSE ->
		{HowManyLeft, Results}
	end.

% as getClosestN but return only the #peer records
getClosestNNodes(RoutingTable, Key, N) -> 
	RoutingEntries = getClosestN(RoutingTable, Key, N),
	Strip = fun(#routingEntry{peer=Peer}) -> Peer end,
	lists:map(Strip,RoutingEntries).

refresh(_RoutingTable, _ForNode) -> false.

send(RE, Message) ->
	RE#routingEntry.peer#peer.pid ! Message.

% -------------------------------------------------------------------------------------------------
% returns all #routingEntry records from the structure
entries( #routingData{root = Root} ) ->
	traverseHelper(Root).

% retrieves all #routingEntry records from the tree
%
% and results is the list of #peer records which were already fetched
traverseHelper(	#leaf{bucket = Bucket} ) -> Bucket;

traverseHelper(	#innerNode{zero = Zero, one = One} ) ->
	ZeroEntries = traverseHelper(Zero),
	OneEntries = traverseHelper(One),
	ZeroEntries ++ OneEntries.

%% ----------------------------------------------------
%% PRIVATE HELPER FUNCTIONS
%% ----------------------------------------------------
	
%bucketContains(Bucket, TargetPeer) ->
%	kademliaGlobal:assert("erlang:is_list(Bucket)",[{'Bucket',Bucket}]),
%	Match = fun(#routingEntry{peer=Peer}) -> Peer == TargetPeer end,
%	{Matching, _} = lists:partition(Match, Bucket),
%%?DEBUG_RAW("Matching elements in bucketContains: ~p",[Matching]),
%	Matching =/= [].
%
%sortListByCloseness(RoutingEntries, Key) ->
%	Criterion = fun(RE1, RE2) -> 
%					metric:distance(RE1#routingEntry.peer#peer.key,Key) =<
%						metric:distance(RE2#routingEntry.peer#peer.key,Key)
%				end,
%	lists:sort(Criterion, RoutingEntries).
%	
%	
%sortBucket(Bucket) ->
%	kademliaGlobal:assert("erlang:is_list(B)",[{'B',Bucket}]),
%	% sort bucket entries to contain the most recent at the bottom
%	% i.e. the ones with the "smaller dates" are "smaller".
%	Criterion = fun(#routingEntry{lastSeen=LS1}, 
%					#routingEntry{lastSeen=LS2}) -> 
%						LS1 =< LS2 
%				end,
%	lists:sort(Criterion, Bucket).
	

%% -------------------------------------------------------------------------------------------------
%% HELPER FUNCTIONS
%% -------------------------------------------------------------------------------------------------

% bucket		List of #routingEntry
bucketContainsNode(Bucket, Node) ->
	Strip = fun(#routingEntry{peer = Peer}) -> Peer end,
	lists:member(Node, lists:map(Strip, Bucket)).
insertSorted(	Bucket, 
				#routingEntry{peer = Peer, lastSeen=LastSeen}=RoutingEntry	) ->
	
	SameNode = fun(#routingEntry{peer = P}) -> P == Peer end,
	{_OldEntry, Rest} = lists:partition(SameNode, Bucket),
	GotThisAlready = lists:any(SameNode, Bucket),
	if GotThisAlready ->
		ModBucket = Rest;
	?ELSE ->
		ModBucket = Bucket
	end,
	IsFresher = fun(#routingEntry{lastSeen = LS}) -> LS > LastSeen end,
	{Fresher, Older} = lists:partition(IsFresher, ModBucket),
	Older ++ [RoutingEntry] ++ Fresher.

bucketDelete(	Bucket,
				Peer	) ->
	NotSameNode = fun(#routingEntry{peer = P}) -> P /= Peer end,
	lists:filter(NotSameNode, Bucket).
	
fresh(Peer) ->
	#routingEntry{peer = Peer, lastSeen = erlang:now()}.

%print(#routingData{owner = Owner, root = Root}) ->
%	io:format("Routing Data of ~p~n",[Owner]),
%	printTree(Root,0).
	
%printTree(#leaf{bucket = Bucket}, Indent) ->
%	io:format("~s Entries: ",[replicate("    ",Indent)]),
%	lists:foreach(fun(#routingEntry{peer=#peer{name=Name}})->io:format("~p, ",[Name	]) end, Bucket);

%printTree(#innerNode{zero = Zero, one = One}, Indent) ->
%	printTree(Zero,Indent+1),
%	printTree(One, Indent+1).

%replicate(String,N) ->
%	lists:concat(lists:map(fun(_X)->String end, lists:seq(1,N))).

