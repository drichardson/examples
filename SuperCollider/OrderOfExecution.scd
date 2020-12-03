// Order of execution in this context doesn't mean the order in which statements are executed in the language (the client). It refers to the ordering of synth nodes on the server, which corresponds to the order in which their output is calculated each control cycle (blockSize). Whether or not you specify the order of execution, each synth and each group goes into a specific place in the chain of execution.

// If you don't have any synths that use In.ar, you don't have to worry about order of execution. It only matters when one synth is reading the output of another.

// The rule is simple: if you have a synth on the server (i.e. an "effect") that depends on the output from another synth (the "source"), the effect must appear later in the chain of nodes on the server than the source.


(
SynthDef(\redirect, { Out.ar(0, In.ar(9)) }).add;
SynthDef(\tone, { Out.ar(9, SinOsc.ar) }).add;
)

// Base case: due to ordering of Synth creation, you hear output.
(
var s1, s2;
s1 = Synth(\redirect);
s2 = Synth(\tone);
)

// If you change order of s1 and s2, you no longer hear anything but if you open the Stethoscope
// with 10 channels showing, you will see a signal on bus 10.
(
var s1, s2;
s2 = Synth(\tone);
s1 = Synth(\redirect);
)

// However, even with this order you can put the Synths into the correct order on the server using the addAction: \addAfter parameter.
(
var s1, s2;
s2 = Synth(\tone);
s1 = Synth(\redirect, addAction: \addAfter, target: s2);
)

// Same thing using convenience method Synth.after.
(
var s1, s2;
s2 = Synth(\tone);
s1 = Synth.after(s2, \redirect);
)

// You can also move the nodes.
(
var s1, s2;
s2 = Synth(\tone);
s1 = Synth(\redirect);
s1.moveAfter(s2);
)





//
// Using order of execution to your advantage
//
// This example demonstrates how to organizes your synths and effects like so:
// Group ( [lots of synths] ) ----> Group ( [effect] ) ----> transfer
//
s.boot;

(
l = Bus.control(s, 1);  // get a bus for the LFO--not relevant to order-of-exec
b = Bus.audio(s, 2);    // assuming stereo--this is to keep the src->fx chain separate from
                // other similar chains
~synthgroup = Group.tail(s);
~fxgroup = Group.tail(s);

// now you have synthgroup --> fxgroup within the default group of s

// make some synthdefs to play with
SynthDef("order-of-ex-dist", { arg bus, preGain, postGain;
    var sig;
    sig = In.ar(bus, 2);
    sig = (sig * preGain).distort;
    ReplaceOut.ar(bus, sig * postGain);
}).add;

SynthDef("order-of-ex-pulse", { arg freq, bus, ffreq, pan, lfobus;
    var sig, noteLen;
    noteLen = In.kr(lfobus, 1);
    sig = RLPF.ar(Pulse.ar(freq, 0.2, 0.5), ffreq, 0.3);
    Out.ar(bus, Pan2.ar(sig, pan)
        * EnvGen.kr(Env.perc(0.1, 1), timeScale: noteLen, doneAction: Done.freeSelf));
}).add;

SynthDef("LFNoise1", { arg freq, mul, add, bus;
    Out.kr(bus, LFNoise1.kr(freq, mul:mul, add:add));
}).add;
)

// Place LFO:

~lfo = Synth.head(s, "LFNoise1", [\freq, 0.3, \mul, 0.68, \add, 0.7, \bus, l]);

// Then place your effect:

~dist = Synth.tail(~fxgroup, "order-of-ex-dist", [\bus, b, \preGain, 8, \postGain, 0.6]);

// transfer the results to main out, with level scaling
// play at tail of s's default group (note that Function-play also takes addActions!

~xfer = { Out.ar(0, 0.25 * In.ar(b, 2)) }.play(s, addAction: \addToTail);

// And start your routine:

(
r = Routine({
    {
        Synth.tail(~synthgroup, "order-of-ex-pulse",
            [\freq, rrand(200, 800), \ffreq, rrand(1000, 15000), \pan, 1.0.rand2,
            \bus, b, \lfobus, l]);
        0.07.wait;
    }.loop;
}).play(SystemClock);
)

~dist.run(false);    // proves that the distortion effect is doing something
~dist.run(true);

// to clean up:
(
r.stop;
[~synthgroup, ~fxgroup, b, l, ~lfo, ~xfer].do({ arg x; x.free });
currentEnvironment.clear; // clear all environment variables
)