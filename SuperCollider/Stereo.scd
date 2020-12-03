(
// arrays
a = ["foo", "bar"];
a.at(0).postln; // foo
a.at(1).postln; // bar
a.at(2).postln; // returns nil, out of bounds

// shorthand for at
a[0].postln;
a[1].postln;
a[2].postln;

);

// A function that returns an array of UGens is played as output with that number of channels.
// So in the example with 2 entries, you get stereo.
{ [SinOsc.ar(440, 0, 0.2), SinOsc.ar(442,0, 0.2)] }.play;

// multichannel expantion. Plugging an array into one of a UGen's arguments produces an array of that UGen instead of a single one.
{ SinOsc.ar([440, 442], 0, 0.2) }.play;


(
{ var freq;
	// randomly select frequency. Since some are mono and some are stereo, you will also randomly play stereo and mono output.
	freq = [[660,880], [440,660], 1320, 880].choose;
	freq.postln;
	SinOsc.ar(freq, 0, 0.2);
}.play;
)


{ Pan2.ar(PinkNoise.ar(0.2), SinOsc.kr(0.5)) }.play;