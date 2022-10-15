-module(ets_test).
-export([start/0]).

start() ->
	list:foreach(fun test_ets/1,
		[set, ordered_set, bag, duplicate_bag]).


test_ets() ->
	TableId = ets:new	
