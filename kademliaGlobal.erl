-module(kademliaGlobal).
-include("include/kademliaGlobal.hrl").
-include("include/node.hrl").

-export([
			eval/2,
			assert/2,
			senderFormat/1
		]).

eval(S,Environ) -> 
	{ok,Scanned,_} = erl_scan:string(S),
	{ok,Parsed} = erl_parse:parse_exprs(Scanned),
	erl_eval:exprs(Parsed,Environ).

%% ----------------------------------------------------
%% ASSERTIONS
%% ----------------------------------------------------

% could be nicer, but automatically generating the bindings
% e.g. from the variable name doesn't work. The macro receives
% the actual value rather than the string. So passing kademliaGlobal:assert("...", N)
% would not make the variable name N available (as necessary), only its value.
% Maybe one could parse the expression for variable names but then again
% the order of assignment (if passing a list of values) should be clear and
% maybe problems arise when processing a list within the macro?
assert(ExprString, Bindings) ->
	{_,Value,_} = eval(ExprString++".",Bindings),
	if (not Value) ->
		io:format("~n----! Assertion Failed !----~n"),
		io:format("in ~p line ~p~n",[?MODULE, ?LINE]),
		io:format("Required ~s, but ~p~n",[ExprString, Bindings]),
		io:format("----! Assertion Failed !----~n");
	?ELSE -> false
	end.
	
% how to display peers
senderFormat(Peer) ->
	io_lib:format("~p",[Peer#peer.name]).
