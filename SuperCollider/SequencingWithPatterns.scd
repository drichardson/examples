// Routine version

(
r = Routine({
    [60, 72, 71, 67, 69, 71, 72, 60, 69, 67].do({ |midi| midi.yield });
});
)

while { (m = r.next).notNil } { m.postln };


// Pseq version

p = Pseq([60, 72, 71, 67, 69, 71, 72, 60, 69, 67], 1);
r = p.asStream;
while { (m = r.next).notNil } { m.postln };


(
var midi, dur;
midi = Pseq([60, 72, 71, 67, 69, 71, 72, 60, 69, 67], 1).asStream;
dur = Pseq([2, 2, 1, 0.5, 0.5, 1, 1, 2, 2, 3], 1).asStream;

SynthDef(\smooth, { |out, freq = 440, sustain = 1, amp = 0.5|
    var sig;
    sig = SinOsc.ar(freq, 0, amp) * EnvGen.kr(Env.linen(0.05, sustain, 0.1), doneAction: Done.freeSelf);
    Out.ar(out, sig ! 2)
}).add;

r = Task({
    var delta;
    while {
        delta = dur.next;
        delta.notNil
    } {
        Synth(\smooth, [freq: midi.next.midicps, sustain: delta]);
        delta.yield;
    }
}).play(quant: TempoClock.default.beats + 1.0);
)



s = Pseries(1, 5, inf).asStream;
s = Pgeom(1, 3, 13).asStream;
s = Pwhite(10.0, 20.0).asStream; // floats
s = Pwhite(10, 20).asStream; // integers
s.next;


p = Pwhite(0.0, Pseries(0.01, 0.01, inf), 100).asStream;
    // .all pulls from the stream until it returns nil
    // obviously you don't want to do this for an 'inf' length stream!
p.all.plot;


// if you want to order a set of numbers randomly so that all numbers come out before a new order is chosen, use Pn to repeat a Pshuf.
p = Pn(Pshuf([1, 2, 3, 4, 5], 1), inf).asStream;
p.nextN(15);    // get 15 values from the pattern's stream


// Playing notes with a pattern: Pbind
// Not only can patterns produce data for notes, but they can also play the notes themselves. 'Over the Rainbow' again.

(
SynthDef(\smooth, { |out, freq = 440, sustain = 1, amp = 0.5|
    var sig;
    sig = SinOsc.ar(freq, 0, amp) * EnvGen.kr(Env.linen(0.05, sustain, 0.1), doneAction: Done.freeSelf);
    Out.ar(out, sig ! 2)
}).add;
)

(
p = Pbind(
        // the name of the SynthDef to use for each note
    \instrument, \smooth,
        // MIDI note numbers -- converted automatically to Hz
    \midinote, Pseq([60, 72, 71, 67, 69, 71, 72, 60, 69, 67], 1),
        // rhythmic values
    \dur, Pseq([2, 2, 1, 0.5, 0.5, 1, 1, 2, 2, 3], 1)
).play;
)