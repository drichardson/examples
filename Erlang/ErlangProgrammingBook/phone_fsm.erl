% Erlang Programming Book, exercise 5-5

-module (phone_fsm).
-export ([idle/0]).

idle() ->
	receive
		{Number, incoming} ->
			start_ringing(),
			ringing(Number);
		off_hook ->
			start_tone(),
			dial()
	end.

ringing(Number) ->
	receive
		{Number, other_on_hook} ->
			stop_ringing(),
			idle();
		{Number, off_hook} ->
			stop_ringing(),
			connected(Number)
	end.

dial() ->
	receive
		on_hook ->
			stop_tone(),
			idle()
	end.

connected(_Number) ->
	receive
		on_hook ->
			stop_tone(),
			idle()
	end.

start_ringing() ->
	io:format("Start ringing~n").

stop_ringing() ->
	io:format("Stop ringing.~n").

start_tone() ->
	io:format("Start tone.~n").

stop_tone() ->
	io:format("Stop tone.~n").
