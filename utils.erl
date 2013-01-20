-module(utils).
-include("include/kademliaGlobal.hrl").
-include("include/metric.hrl").

-export([
			%fcp/1,
			%cp/1,
			%print/1,
			%keystore/4,
			to_upper/1,
			%to_lower/1,
			%uniforms/2,
			%uniforms/3,
			selectRandom/1,
			takeIfPossible/2,
			decInt2BinStr/1
		]).


takeIfPossible(List, N) ->
	kademliaGlobal:assert("erlang:is_list(List)",[{'List',List}]),
	kademliaGlobal:assert("N >= 0",[{'N',N}]),
	L = length(List),
	if N > L ->
		{Result, _ } = lists:split(L, List);
	?ELSE ->
		{Result, _ } = lists:split(N, List)
	end,
	Result.

selectRandom(List) when length(List) == 0 -> undefined;
selectRandom(List) ->
	Index = random:uniform(length(List)),
	lists:nth(Index, List).
	
%% K random elements of the List 
%selectRandomN(List, N) -> 
%    uniforms( N, length(List), List, [] ). 

%% returns a random List with k unique values between [1,..,N] 
%uniforms ( K, N ) -> 
%    uniforms ( K, N, lists:seq( 1, N ), [] ). 
 
%% returns a random List with k unique values between [From,..,To] 
%uniforms ( K, From, To ) -> 
%    uniforms ( K, To-From, lists:seq( From, To ), [] ). 
     
%% help functions, which moves K elements from 'From' to 'To' 
%uniforms ( K, N, From, To ) -> 
%    case K of 
%	0 -> To; 
%	_ when N > 0 -> 
%	    E = lists:nth( random:uniform(N), From) ,
%	    uniforms ( K-1, N-1,  lists:delete (E, From ), [E|To] ) 
 %   end.


% Forward composition. A list of function is converted into one function, 
% executing them in the order as passed
% example FCP([F1,F2,F3]) = 
%	fun(X) -> F3(F2(F1(X))) end.
%fcp(FunctionList) -> 
%	cp(lists:reverse(FunctionList)).
	
% Composition. A list of function is converted into one function,
% using normal composition operator order
% example FCP([F1,F2,F3]) = 
%	fun(X) -> F1(F2(F3(X))) end.
%cp(FunctionList) ->
%	Compose = fun(Acc,F) -> fun(X) -> Acc(F(X)) end end,
%	Identity = fun(X) -> X end,
%	Composed = lists:foldl(Compose, Identity, FunctionList),
%	fun(X) -> Composed(X) end.
	

%print(Prefix) ->
%    fun(Format, Data) ->
%	    io:format("~w ~s: ~s~n", [self(), Prefix, io_lib:format(Format, Data)])
 %      
  %     end.



%% http://www.trapexit.org/String_Case

to_upper(S) -> lists:map(fun char_to_upper/1, S).
%to_lower(S) -> lists:map(fun char_to_lower/1, S).

char_to_upper(C) when C >= $a, C =< $z -> C bxor $\s;
char_to_upper(C) -> C.
%char_to_lower(C) when C >= $A, C =< $Z -> C bxor $\s;
%char_to_lower(C) -> C.

%%
%% STDLIB 1.15.1: The functions keystore/4 and keytake/3 are new in the lists module.
%% http://www.erlang.org/doc/apps/stdlib/notes.html
%%
%% Types:
%% Key = term()
%% N = 1..tuple_size(Tuple)
%%  TupleList1 = TupleList2 = [Tuple]
%%  NewTuple = Tuple = tuple()
%%  
%% Returns a copy of TupleList1 where the first occurrence of a tuple
%% T whose Nth element compares equal to Key is replaced with
%% NewTuple, if there is such a tuple T. If there is no such tuple T a
%% copy of TupleList1 where [NewTuple] has been appended to the end is
%% returned.
%keystore(Key, N, TupleList1, NewTuple) ->		       
 %   case lists:keysearch(Key, N, TupleList1) of
%	{value, _} ->
%	    lists:keyreplace(Key, N, TupleList1, NewTuple);
%	false ->
%	    lists:append(TupleList1, [NewTuple])
 %   end.
    
%unique_add(Element, List) ->		       
%    case lists:member(Element, List) of
%	true ->
%	    List;
%	false ->
%	    [Element,List]
%   end.

% converts an integer in decimal representation to a string containing 
% its binary representation
decInt2BinStr(Int) ->
	BinaryString = hd(io_lib:format("~.2B", [Int])),
	lists:flatten(io_lib:format("~"++erlang:integer_to_list(?KEY_BITS)++"."++erlang:integer_to_list(?KEY_BITS)++".0s",[BinaryString])).


