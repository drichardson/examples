%
% Erlang Programming Exercise 4-2: The Process Ring
%

-module(process_ring).
-export([start/1, stop/0, process/1, send_messages/2]).		

% send_messages/2: send MessageCount messages around the process ring followed by a quit message.
send_messages(0, _) ->
	ok;
send_messages(MessageCount, Message) ->
	io:format("send_messages: sending {message, ~s} current count ~w to starter~n", [Message, MessageCount]),
	starter ! {message, Message},
	send_messages(MessageCount - 1, Message).

% start/1: start up the process ring
start(ProcessCount) ->
	io:format("Starting~n"),
	StarterPid = spawn(?MODULE, process, [ProcessCount-1]),
	register(starter, StarterPid),
	io:format("Registered starter with pid ~w~n", [StarterPid]).
	
stop() ->
	io:format("Stopping~n"),
	starter ! quit.

% process/1: start the next process in the ring (or point back to starter) and then start the process loop.
process(ProcessCount) ->
	io:format("process started with count ~w~n", [ProcessCount]),
	case ProcessCount of
		Count when Count >= 1 ->
			NextPid = spawn(?MODULE, process, [ProcessCount-1]);
		Count when Count == 0 ->
			NextPid = starter
	end,
	io:format("In process: I'm ~w and nextpid is ~w~n", [self(), NextPid]),
	process_loop(NextPid).

% process_loop/1: run a process loop that processes mesages
process_loop(NextPid) ->
	receive
		quit ->
			io:format("Pid ~w received quit message~n", [self()]),
			% Don't send quit to the starter process, because it already was quit if we got here
			if
				NextPid /= starter ->
					NextPid ! quit;
				true ->
					ok
			end,
			ok;
		{message, Message} ->
			io:format("Pid ~w received message ~s. Forwarding to ~w~n", [self(), Message, NextPid]),
			NextPid ! {message, Message},
			timer:sleep(1000), % Sleep so that this doesn't go so fast you can't tell what's happening (for debugging only)
			process_loop(NextPid)
	end.
		