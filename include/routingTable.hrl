%%%        File : routingTable.erl
%%%      Author : Carl Witt
%%% Description : An abstract data type for storing known contacts to other nodes
%%%     Created : 30 Jun 2009 by Karl Marklund 
%%%
%%% Abstract: Routing information (a contact) is stored in buckets of a certain size.
%%% Each node sorts its known contacts into buckets according to the "distance"
%%% between that contact's ID and the node's ID. Buckets are allocated dynamically
%%% and can be refreshed to sort stale contacts out.
%%% The most important operation is to retrieve contacts close to a certain ID (getClosestN)
%%% which is used in searching the network and storing data.

%% ----------------------------------------------------
%% GLOBAL CONSTANTS / CONFIGURATION
%% ----------------------------------------------------

-define(BUCKET_SIZE, 10).		 	% each node can record this much nodes of a certain distance

%% ----------------------------------------------------
%% DATA TYPES
%% ----------------------------------------------------

% routingEntry - assigns last valid response time to a peer
% peer			#peer{}
% lastSeen		{megasecs, secs, microsecs} as produced by erlang:now()
-record(routingEntry,{peer, lastSeen}).

% routingData the abstract data type
% owner		peer that uses this structure (to allow more contacts in its neighbourhood)
% root		the routing data tree
-record(routingData, {owner, root}).

% innerNodes refer to two subtrees, containing all the peer contacts
% that have a prefix continuing with zero or one, respectively
-record(innerNode, {zero, one}).

% leaf - routing information
% bucket				list of #routingEntry records / INVARIANT: sorted with most recently seen contact at the end.
-record(leaf, {bucket}).

