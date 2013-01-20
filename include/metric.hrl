%%%      Module : metric
%%%      Author : Carl Witt
%%% Description : defines the metric used to compare keys
%%%     Created : 30 Jun 2009 by Karl Marklund 
%%%
%%% Abstract: Values are associated with a key. Nodes also have a key.
%%% When storing data, the system puts <key,value> pairs on nodes with low
%%% distance between node and data key, according to this metric.

-define(KEY_BITS, 24).

% keys have a hash value to compare and a readable value 
% (the origin of the hash value) to allow easier debugging.
% hash		Integer value
% readable	any term
-record(key, {hash, readable}).