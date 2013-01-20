-module(metric).
-include("include/metric.hrl").

-include("include/kademliaGlobal.hrl").

-export([
			distance/2,
			randomKey/0,
			randomKey/1,
			calculateKey/1,
			nthBit/2
		]).

% distance - returns the distance between two elements in the key space according to this metric
distance(K1, K2) ->
		K1#key.hash bxor K2#key.hash.

% randomKey - returns a random element of the key space
randomKey() ->
	Hash = erlang:phash2(erlang:now(), 1 bsl ?KEY_BITS),
	Readable = io_lib:format("random key generated ~p ~p",[erlang:date(),erlang:time()]),
	#key{hash = Hash, readable = io_lib:format("~s",[lists:flatten(Readable)])}.

% randomKey(Descriptor) - returns a random element of the key space
% uses descriptor as "seed", salted with the current time
% descriptor survives as #key.readable
randomKey(Descriptor) ->
	Hash = erlang:phash2({Descriptor,erlang:now()}, 1 bsl ?KEY_BITS),
	Readable = Descriptor,
	#key{hash = Hash, readable = Readable}.

% hashes some value to determine an associated element in the key space
calculateKey(Data) ->
	#key{hash = erlang:phash2(Data, 1 bsl ?KEY_BITS), readable = Data}.
	
% nthBit - zero based access function to the bit representation of data
nthBit(Data, N) ->
	kademliaGlobal:assert("erlang:is_integer(N)",[{'N',N}]),
	kademliaGlobal:assert("erlang:is_integer(Data)",[{'Data',Data}]),
	Data band (1 bsl N) > 0.