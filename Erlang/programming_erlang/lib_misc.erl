-module(lib_misc).
-export([qsort/1, on_exit/2]).

qsort([]) -> [];
qsort([Pivot | T]) ->
	qsort([X || X <- T, X < Pivot])
	++ [Pivot] ++
	qsort([X || X <- T, X >= Pivot]).


on_exit(Pid, Fun) -> 
	spawn(fun() ->
			      Ref = monitor(process, Pid),
			      receive
				      { 'DOWN', Ref, process, Pid, Why } ->
					      Fun(Why)
			      end
	      end).

