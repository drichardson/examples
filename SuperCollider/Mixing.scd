// Mix using addition. Note mul parameter makes sure amplitied values don't clip (i.e., fall outside [-1,1]) when added together.
{ PinkNoise.ar(0.2) + SinOsc.ar(440, 0, 0.2) + Saw.ar(660, 0.2) }.play;

{ Saw.ar([440,440], 0.2) }.play;
{ SinOsc.ar() }.play

// Use Mix to mix 2 channels into 1 channel
{ Mix.new([SinOsc.ar(440,0,0.2),Saw.ar(660,0.2)]).postln }.play;

// combine two stereo arrays
(
{
	var a, b;
	a = [SinOsc.ar(440, 0, 0.2), Saw.ar(662, 0.2)];
	b = [SinOsc.ar(442, 0, 0.2), Saw.ar(660, 0.2)];
	Mix([a, b]).postln;
}.play;
)

// Mix.fill calls the function n times and sums the results.
(
var n = 8;
{ Mix.fill(n, {SinOsc.ar(500 + 500.0.rand, 0, 1 / n) }) }.play;
)


// Mix.fill calls the function with an index of it's order in the execution.
// Look at the post window for frequences and indices
(
var n = 8;
{
	Mix.fill(n, { arg index;
		var freq;
		index.postln;
		freq = 440 + index;
		freq.postln;
		SinOsc.ar(freq, 0, 1 / n)
	})
}.play;
)


//
// From A Gentle Introduction to SuperCollider
//

// Mix and Splay

// 5 channels output (watch Meter window)
a = { SinOsc.ar([100, 300, 500, 700, 900], mul: 0.1) }.play;
a.free;

// Mix it down to mono:
b = { Mix(SinOsc.ar([100, 300, 500, 700, 900], mul: 0.1)) }.play;
b.free;

// Mix it down to stereo (spread evenly from left to right)
c = { Splay.ar(SinOsc.ar([100, 300, 500, 700, 900], mul: 0.1)) }.play;
c.free;

// Fun with Splay:
(
d = {arg fundamental = 110;
	var harmonics = (1..12);
	var snd = BPF.ar(
		in: Saw.ar(32, LFPulse.ar(harmonics, width: 0.1)),
		freq: harmonics * fundamental,
		rq: 0.01,
		mul: 20);
	Splay.ar(snd);
}.play;
)
d.set(\fundamental, 100); // change fundamental just for fun
d.set(\fundamental, 110); // change fundamental just for fun
d.set(\fundamental, 120); // change fundamental just for fun
d.free;