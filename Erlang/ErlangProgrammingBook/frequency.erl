%
% Exercise 5-2: Changing the Frequency Server
%

-module (frequency).
-export ([start/0,stop/0,allocate/0,deallocate/1]).
-export ([init/0]).

% These are the start functions used to create and initialize the server

start() ->
	register(frequency, spawn(?MODULE, init, [])).

init() ->
	Frequencies = {get_frequencies(), []},
	loop(Frequencies).

% Hard coded
get_frequencies() -> [10,11,12,13,14,15].

% The client functions
stop() -> call(stop).
allocate() -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).

% Hide all message passing and the message protocol in a functional interface
call(Message) ->
	frequency ! {request, self(), Message},
	receive
		{reply, Reply} -> Reply
	end.

% The main loop
loop(Frequencies) ->
	receive
		{request, Pid, allocate} ->
			{NewFrequencies, Reply} = allocate(Frequencies, Pid),
			reply(Pid, Reply),
			loop(NewFrequencies);
		{request, Pid, {deallocate, Freq}} ->
			NewFrequencies = deallocate(Frequencies, Freq),
			reply(Pid, ok),
			loop(NewFrequencies);
		{request, Pid, stop} ->
			reply(Pid, ok)
	end.

reply(Pid, Reply) ->
	Pid ! {reply, Reply}.

% Internal help functions

allocate({[], Allocated}, _Pid) ->
	{{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
	{{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
	NewAllocated=lists:keydelete(Freq, 1, Allocated),
	{[Freq|Free], NewAllocated}.
	

