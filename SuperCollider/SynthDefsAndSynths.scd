// Functions vs. SynthDefs

{ SinOsc.ar(440, 0, 0.2) }.play;

SynthDef.new("tutorial-SinOsc", { Out.ar(0, SinOsc.ar(440, 0, 0.2)) }).play;

// Getting and freeing the Synth
x = { SinOsc.ar(440, 0, 0.2) }.play;
y = SynthDef.new("tutorial-SinOsc", { Out.ar(0, SinOsc.ar(440, 0, 0.2)) }).play;
x.free; // receiver notation
free(y); // function notation

// execute first, by itself
SynthDef.new("tutorial-PinkNoise", { Out.ar(0, PinkNoise.ar(0.3)) }).add;

// then:
x = Synth.new("tutorial-PinkNoise");
y = Synth.new("tutorial-PinkNoise");
x.free; y.free;`

// first with a Function. Note the random frequency each time 'play' is called.
f = { SinOsc.ar(440 + 200.rand, 0, 0.2) };
x = f.play;
y = f.play;
z = f.play;
x.free; y.free; z.free;

// Now with a SynthDef. No randomness!
SynthDef("tutorial-NoRand", { Out.ar(0, SinOsc.ar(440 + 200.rand, 0, 0.2)) }).add;
x = Synth("tutorial-NoRand");
y = Synth("tutorial-NoRand");
z = Synth("tutorial-NoRand");
x.free; y.free; z.free;




// With Rand, it works!
SynthDef("tutorial-Rand", { Out.ar(0, SinOsc.ar(Rand(440, 660), 0, 0.2)) }).add;
x = Synth("tutorial-Rand");
y = Synth("tutorial-Rand");
z = Synth("tutorial-Rand");
x.free; y.free; z.free;



(
SynthDef("tutorial-args", { arg freq = 440, out = 0;
	Out.ar(out, SinOsc.ar(freq, 0, 0.2));
}).add;
)
x = Synth("tutorial-args");
y = Synth("tutorial-args", ["freq", 660]);
z = Synth("tutorial-args", ["freq", 880, "out", 1]);
x.free; y.free; z.free;

// Changing values on a Synth after is has been created.
(
SynthDef.new(\tutorial_args, { arg freq = 440, out = 0;
	Out.ar(out, SinOsc.ar(freq, 0, 0.2));
}).add;
)
s.scope; // scope so you can see the effect
x = Synth.new(\tutorial_args);
x.set(\freq, 660);
x.set(\freq, 880, "out", 1);
x.free;

