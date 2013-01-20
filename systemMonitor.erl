-module(systemMonitor).
-include("include/systemMonitor.hrl").
-include("include/node.hrl").
-include("include/kademliaGlobal.hrl").
-include("include/metric.hrl").
-export([
			start/1
		]).
		
%% ----------------------------------------------------
%% START
%% ----------------------------------------------------

% boot monitor and connect with master node
start(MasterNode) ->
    GlobalName = kademliaMonitor,
    LocalName = mon,

    Connect = net_kernel:connect(MasterNode),
    io:format("~nConnect to master node ~p: ~p~n",[MasterNode,Connect]),
	%if (not Connect) ->
	%	io:format("~n~n\t!!!\tPlease start master node first (make master), then start this monitor.~n~n");
	%?ELSE ->
	    PID = spawn(fun() -> mainLoop(initialMonitorState()) end),

		% make process known to all nodes
		Register = global:register_name(GlobalName,PID),								
		io:format("Registering monitor to all connected nodes as ~p: ~p~n",[GlobalName, Register]),
		
		register(mon, PID),
	    Now = httpd_util:rfc1123_date(erlang:universaltime()),
	    io:format("~n~n== System Monitor running ~s (~p/~p) == ~n~n", [Now,PID,LocalName]).
	%end.
		
initialMonitorState() ->
	#monitorState{mute = false, showDebug = true, showProtocol = true, filter=[]}.

% how to display peers
senderFormat(Peer) ->
	io_lib:format("~p (~p)",[Peer#peer.name,
							 Peer#peer.key#key.hash]).
	
mainLoop(S) ->

    receive
	
	%% ----------------------------------------------------
	%% DEBUG MESSAGES
	%% ----------------------------------------------------
	
	{debug, Sender, Message, TimeStamp} ->
		if ( not S#monitorState.mute and S#monitorState.showDebug) ->
			SenderString = senderFormat(Sender),
			io:format("~n-------------------------------- ! ----------~p--------~n",[TimeStamp]),
		    io:format("~s:~n\t~s~n", [SenderString, lists:flatten(Message)]),
	    	io:format("-------------------------------- ! --------------------------------~n");
	    ?ELSE -> false
		end,
	    mainLoop(S);
	    
	{debug, Message, TimeStamp} ->
		if ( not S#monitorState.mute and S#monitorState.showDebug) ->
			io:format("~n-------------------------------- ! ----------~p--------~n",[TimeStamp]),
	    	io:format("~s ~n", [lists:flatten(Message)]),
			io:format("-------------------------------- ! --------------------------------~n");
	    ?ELSE -> false
		end,
	    mainLoop(S);
	
	%% ----------------------------------------------------
	%% PROTOCOL MESSAGES
	%% ----------------------------------------------------
	
	{protocol, Group, Sender, Message, TimeStamp} ->
		Allowed = not S#monitorState.mute and 
			 		S#monitorState.showProtocol and 
			 		not lists:member(Group, S#monitorState.filter),
		if Allowed ->
			SenderString = senderFormat(Sender),
		    io:format("~n~p\t~s\\~p:~n\t~s~n", [TimeStamp, SenderString, Group, lists:flatten(Message)]);
		?ELSE -> false
		end,
	    mainLoop(S);
	    
	{protocol, Group, Message, TimeStamp} ->
		Allowed = not S#monitorState.mute and 
			 		S#monitorState.showProtocol and 
			 		not lists:member(Group, S#monitorState.filter),
		if Allowed ->
			SenderString = "*master",
		    io:format("~n~p\t~s\\~p:~n\t~s~n", [TimeStamp, SenderString, Group, lists:flatten(Message)]);
		?ELSE -> false
		end,
	    mainLoop(S);
	
	%% ----------------------------------------------------
	%% STATE MANIPULATION MESSAGES
	%% ----------------------------------------------------
	
	mute ->
		io:format("== MONITOR MUTE ==~n"),
		mainLoop(S#monitorState{mute=true});
	unmute ->
		io:format("== MONITOR NOT MUTE ==~n"),
		mainLoop(S#monitorState{mute=false});
	debugOff ->
		io:format("== SUPPRESSING DEBUG MESSAGES ==~n"),
		mainLoop(S#monitorState{showDebug=false});
	debugOn ->
		io:format("== ALLOWING DEBUG MESSAGES ==~n"),
		mainLoop(S#monitorState{showDebug=true});
	protocolOff ->
		io:format("== SUPPRESSING PROTOCOL MESSAGES ==~n"),
		mainLoop(S#monitorState{showProtocol=false});
	protocolOn ->
		io:format("== ALLOWING PROTOCOL MESSAGES ==~n"),
		mainLoop(S#monitorState{showProtocol=true});
	{suppress, Group} ->
		io:format("== SUPPRESSING CLASS ~p MESSAGES ==~n",[Group]),
		NewFilter = S#monitorState.filter++[Group],
		mainLoop(S#monitorState{filter=NewFilter});
	{allow, Group} ->
		io:format("== ALLOWING CLASS ~p MESSAGES ==",[Group]),
		NewFilter = lists:filter(fun(X) -> X /= Group end,
								S#monitorState.filter),
		mainLoop(S#monitorState{filter=NewFilter});
	state ->
		io:format("Current state ~p~n",[S]),
		mainLoop(S);
	reset ->
		io:format("== SETTINGS RESET =="),
		mainLoop(initialMonitorState());
	Any ->
	    if (erlang:is_list(Any)) ->
	    	io:format("\tMonitor received: ~s~n", [Any]);
	    ?ELSE ->
	    	io:format("\tMonitor received: ~p~n", [Any])
	    end,
	    mainLoop(S)
    
	after 10000 ->
		%io:format("Monitor is still alive."),
		mainLoop(S)
	end.
