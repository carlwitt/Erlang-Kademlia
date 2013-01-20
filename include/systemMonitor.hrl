%%%        File : systemMonitor.erl
%%%      Author : Carl Witt
%%% Description : Responsible for logging all info from the system.
%%%     Created : 01 Aug 2011 by Carl Witt

%% ----------------------------------------------------
%% PROTOCOL MESSAGE CLASSES 
%% ----------------------------------------------------

-define(JOINING_THE_NETWORK, joining_the_network).
-define(FIND_NODE_PROCEDURE, find_node_procedure).
-define(FIND_VALUE_PROCEDURE, find_value_procedure).
-define(MISC_EVENT, misc_event).
-define(PINGS, pings).

%% ----------------------------------------------------
%% MONITOR MESSAGE MACROS
%% ----------------------------------------------------

-define( 	DEBUG(String), 
			global:send(
						kademliaMonitor,
						{debug, ?THIS, io_lib:format(String,[]), erlang:now()}
						)
		). 
		
-define( 	DEBUG(String, Args), 
			global:send(
						kademliaMonitor,
						{debug, ?THIS, io_lib:format(String,Args), erlang:now()}
						)
		). 
%% doesn't require the node information used in ?THIS to be available
-define(	DEBUG_RAW(String, Args),
			global:send(
						kademliaMonitor, 
						{debug, io_lib:format(String,Args), erlang:now()}
						)
		). 


% Group is an atom that represents the "class" (e.g. find node) of the protocol message and is used for filtering
-define(	LOG(String, Group),
			global:send(
						kademliaMonitor,
						{protocol, Group, ?THIS, io_lib:format(String,[]), erlang:now()}
						)
		). 
		
-define(	LOG(String, Args, Group),
			global:send(
						kademliaMonitor,
						{protocol, Group, ?THIS, io_lib:format(String,Args), erlang:now()}
						)
		). 
%% doesn't require the node information used in ?THIS to be available		
-define(	LOG_RAW(String, Args, Group),
			global:send(
						kademliaMonitor,
						{protocol, Group, io_lib:format(String,Args), erlang:now()}
						)
		). 

%% ----------------------------------------------------
%% SERVER RUNTIME CONFIGURATION OPTIONS
%% ----------------------------------------------------
    
% Options for the monitor - true/false
% mute			Do not display any messages
% showDebug		display debug messages
% showProtocol	display protocol messages
% filter		list of atoms that lead to suppression from the display
-record(monitorState,{mute, showDebug, showProtocol, filter}).