~

// Reading from Busses
In.ar(0, 1); // this will return an OutputProxy
In.ar(0, 4); // this will return an Array of 4 OutputProxys.


// Writing to Busses

// This throws an error. Can't write a control rate signal to an audio rate bus.
{ Out.ar(0, SinOsc.kr)}.play;

// The first busses are the output busses. On a stereo system, they are 0 and 1.
{ Out.ar(0, SinOsc.ar)}.play;
{ Out.ar(1, SinOsc.ar)}.play;


// Redirecting

// Randomly picking bus 18 (use Bus object to allocate for you).
// Note the order matters. If you run them in reverse order, you hear nothing.
{ Out.ar(0, In.ar(18)); }.play;
{ Out.ar(18, SinOsc.ar); }.play;

~s1 = SynthDef(\redirect_test, { Out.ar(0, In.ar(18)) });
~s1.add;
r = Synth(\redirect_test);

~s2 = SynthDef(\redirect_sound, { Out.ar(18, SinOsc.ar) });
~s2.after(~s1);
s = Synth(\redirect_sound);



//
// From a Gentle Introduction to SuperCollider
//


// Warning: use headphones to avoid feedback
{SoundIn.ar(0)}.play; // same as In.ar(8): takes sound from the first input bus

// Stereo version
{SoundIn.ar([0, 1])}.play; // first and second inputs

// Some reverb just for fun?
{FreeVerb.ar(SoundIn.ar([0, 1]), mix: 0.5, room: 0.9)}.play;



// The Bus object


// Run this first ('turn reverb on'−−you won't hear anything at first)
(
r = {
	Out.ar(0, FreeVerb.ar(
		In.ar(18, 2),
		mix: 0.5,
		room: 0.9,
		mul: 0.4)
)}.play;
)

// Now run this second ('feed the busy tone into the reverb bus')
(
a = {
	Out.ar(18,
		SinOsc.ar([800, 880],
			mul: LFPulse.ar(2)))
}.play;
)
a.free;


// now the same thing using a Bus object so we don't have to keep track of the Bus numbers ourself

// create the bus
~myBus = Bus.audio(s, 2);
// turn on the reverb: read fro myBus (source sound)
(
r = {
	Out.ar(0, FreeVerb.ar(
		In.ar(~myBus, 2),
		mix: 0.5,
		room: 0.9,
		mul: 0.4))
}.play;
)
// feed the busy tone into ~myBus
(
b = {
	Out.ar(~myBus, SinOsc.ar(
		[800, 880],
		mul: LFPulse.ar(2)))
}.play;
)
// free both synths
r.free; b.free;





// Reading from Busses
// The next busses are the input. On a stereo output system, the first two input indicies are 2 and 3.
{ [In.ar(2), In.ar(3)] }.scope;


// This will work as the audio rate signal is downsampled to control rate.
{ Out.kr(0, SinOsc.ar)}.scope;



//
// Using a bus to apply an effect to a signal coming from a bus.
//
// start the effect
(
f = {
	Out.ar(0, BPF.ar(
		in: In.ar(55),
		freq: MouseY.kr(1000, 5000),
		rq: 0.1)
	)
}.play;
)

// start the source
~n1 = {Out.ar(55, WhiteNoise.ar(0.5))}.play();
~n2 = {Out.ar(55, SinOsc.ar(880, 0.5))}.play;
~n1.free
~n2.free


// When multiple Synths write to the same bus, their output is summed (i.e. mixed).

(
SynthDef("tutorial-args", { arg freq = 440, out = 0;
	Out.ar(out, SinOsc.ar(freq, 0, 0.2));
}).add;
)

// both write to bus 1, and their output is mixed
x = Synth("tutorial-args", ["out", 1, "freq", 660]);
y = Synth("tutorial-args", ["out", 1, "freq", 770]);
free(x); free(y);



// Creating a Bus Object
// Using Bus objects frees you having having to manage bus indicies yourself.

b = Bus.control(s, 2); // Get a two channel control Bus
c = Bus.audio(s); // Get a one channel private audio Bus (one is the default)

(
SynthDef.new("tutorial-SinOsc-stereo", { var outArray;
	outArray = [SinOsc.ar(440, 0, 0.2), SinOsc.ar(442, 0, 0.2)];
	Out.ar(0, outArray); // writes to busses 0 and 1
}).play;
)

s.reboot; // restart the server and thus reset bus allocators
b = Bus.control(s, 2); // Get a two channel control Bus
b.index; // should be zero
b.numChannels;
c = Bus.control(s);
c.index; // should be 2, since b used 0 and 1 (it had 2 channels).
c.numChannels; // the default number of channels is 1
b.free; // free the indicies.


// Busses in Action

(
SynthDef("tutorial-Infreq", { arg bus, freqOffset = 0;
	// this will add freqOffset to whatever is read in from the bus
	Out.ar(0, SinOsc.ar(In.kr(bus) + freqOffset, 0, 0.5));
}).add;

SynthDef("tutorial-Outfreq", { arg freq = 400, bus;
	Out.kr(bus, SinOsc.kr(1, 0, freq/40, freq));
}).add;

b = Bus.control(s, 1);
)

(
x = Synth.new("tutorial-Outfreq", [\bus, b]);
y = Synth.after(x, "tutorial-Infreq", [\bus, b]);
//z = Synth.after(x, "tutorial-Infreq", [\bus, b, \freqOffset, 200]);
)
x.free; y.free; z.free; b.free;



(
// the arg direct will control the proportion of direct to processed signal
SynthDef("tutorial-DecayPink", { arg outBus = 0, effectBus, direct = 0.5;
    var source;
    // Decaying pulses of PinkNoise. We'll add reverb later.
    source = Decay2.ar(Impulse.ar(1, 0.25), 0.01, 0.2, PinkNoise.ar);
    // this will be our main output
    Out.ar(outBus, source * direct);
    // this will be our effects output
    Out.ar(effectBus, source * (1 - direct));
}).add;

SynthDef("tutorial-DecaySin", { arg outBus = 0, effectBus, direct = 0.5;
    var source;
    // Decaying pulses of a modulating sine wave. We'll add reverb later.
    source = Decay2.ar(Impulse.ar(0.3, 0.25), 0.3, 1, SinOsc.ar(SinOsc.kr(0.2, 0, 110, 440)));
    // this will be our main output
    Out.ar(outBus, source * direct);
    // this will be our effects output
    Out.ar(effectBus, source * (1 - direct));
}).add;

SynthDef("tutorial-Reverb", { arg outBus = 0, inBus;
    var input;
    input = In.ar(inBus, 1);

    // a low-rent reverb
    // aNumber.do will evaluate its function argument a corresponding number of times
    // {}.dup(n) will evaluate the function n times, and return an Array of the results
    // The default for n is 2, so this makes a stereo reverb
    16.do({ input = AllpassC.ar(input, 0.04, { Rand(0.001,0.04) }.dup, 3)});

    Out.ar(outBus, input);
}).add;

b = Bus.audio(s,1); // this will be our effects bus
)

(
x = Synth.new("tutorial-Reverb", [\inBus, b]);
y = Synth.before(x, "tutorial-DecayPink", [\effectBus, b]);
z = Synth.before(x, "tutorial-DecaySin", [\effectBus, b, \outBus, 1]);
)

// Change the balance of wet to dry
y.set(\direct, 1); // only direct PinkNoise
z.set(\direct, 1); // only direct Sine wave
y.set(\direct, 0); // only reverberated PinkNoise
z.set(\direct, 0); // only reverberated Sine wave
x.free; y.free; z.free; b.free;


//
// Doug's Bus tests
//
s.reboot;
b = Bus.audio(s, 1);
{ Out.ar(b, [SinOsc.ar(440, 0, 0.5)]) }.play;
b.scope;
{ Out.ar(0, [In.ar(b)]) }.play;
b.free;


// Routing one bus to another

s.reboot;
(
SynthDef("doug-writer", { arg bus;
	Out.ar(bus, SinOsc.ar());
}).add;

SynthDef("doug-reader-writer", { arg busRead, busWrite;
	Out.ar(busWrite, In.ar(busRead));
}).add;
)

b = Bus.audio(s);
w = Synth.new("doug-writer", [\bus, b]);
r = Synth.new("doug-reader-writer", [\busRead, b, \busWrite, 0]);
w.free; r.free;


{ Out.ar(b, SinOsc.ar()); Out.ar(b); }.play;



{ Line.kr(); }.plot(1);
{ XLine.kr(); }.plot(1);
{ LFDNoise3.ar(XLine.ar(1000, 100, 0.1) ! 3) }.plot(0.1);

plot({ Line.kr(200,17000,10)});
plot({ XLine.kr(200,17000,10)});
play({ SinOsc.ar(XLine.kr(200,17000,10),0,0.1) });



{ LFPar.ar(XLine.kr(100,15000,6),0,0.1) }.scope(1, zoom: 4);
{ SinOsc.ar(SinOsc.kr(SinOsc.kr(0.2,0,8,10),0, 400,800),0,0.1) }.scope(1, zoom: 4);
{ SyncSaw.ar(100, MouseX.kr(100, 1000), 0.1) }.scope(1, zoom: 4);
{ LFPulse.ar(LFPulse.kr(LFPulse.kr(0.2,0,0.5,8,10),0,0.5, 400,800),0,0.5,0.1) }.scope(1, zoom: 4);
{ Decay.ar(LFPulse.ar(440, 0, 0.02/10, 0.5), 0.02); }.scope;
{ LFPar.ar(800,0,0.1) }.scope(1, zoom: 4);

{ LFPulse.ar(100,0,MouseY.kr(0,1),0.1) }.scope(1, zoom: 4);

{ Blip.ar(XLine.kr(20000,200,6),100,0.2) }.scope(1);
{ Blip.ar(200,Line.kr(1,100,20),0.2) }.scope(1);







