(
// Send the ar message to the SinOsc class. This means make an audio rate instance (calculate
// a signal value for each sample in the block).
{
	// Open the function
	SinOsc.ar( // Make an audio rate SinOsc
		440, // frequency of 440 Hz
		0, // initial phase in radians
		0.2) // multiply amplitude by 0.2
}.play; // close the Function and send the 'play' message to it, which means evaluate yourself and play the results on the server, which if not specified, is the default server that is stored in the variable 's'.
)

(
// An example of polymorphism that plugs the a control rate SinOsc into the mul parameter for the audio rate SinOsc.
{ var ampOsc;
	ampOsc = SinOsc.kr(0.5, 1.5pi, 0.5, 0.5);
	SinOsc.ar(440, 0, ampOsc);
}.play;
)