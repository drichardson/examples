// https://doc.sccode.org/Tutorials/Getting-Started/15-Sequencing-with-Routines-and-Tasks.html



// When you schedule a function (as in the Scheduling Events tutorial), the function always begins at the beginning and runs through to the end. For sequencing, it's more useful to have a control structure that can run part of the way through, return a value, and then pick up where it left off the next time it's needed. In SuperCollider, this is a Routine.

(
r = Routine({
    "abcde".yield;
    "fghij".yield;
    "klmno".yield;
    "pqrst".yield;
    "uvwxy".yield;
    "z{|}~".yield;
});
)

r.next;
6.do({ r.next.postln });


// Recall that, when you schedule a function on a clock, numbers returned by the function are treated as time values -- specifically, the amount of time until the function should execute again. The same thing happens with numbers yielded by a routine.

(
r = Routine({
    var delta;
    loop {
        delta = rrand(1, 3) * 0.5;
        "Will wait ".post; delta.postln;
        delta.yield;
    }
});
)

r.next;

TempoClock.default.sched(0, r);
r.stop;




(
SynthDef(\singrain, { |freq = 440, amp = 0.2, sustain = 1|
    var sig;
    sig = SinOsc.ar(freq, 0, amp) * EnvGen.kr(Env.perc(0.01, sustain), doneAction: Done.freeSelf);
    Out.ar(0, sig ! 2);    // sig ! 2 is the same as [sig, sig]
}).add;

r = Routine({
    var delta;
    loop {
        delta = rrand(1, 3) * 0.5;
        Synth(\singrain, [freq: exprand(200, 800), amp: rrand(0.1, 0.5), sustain: delta * 0.8]);
        delta.yield;
    }
});
)

r.play;

r.stop;


// Pause and resume: Task

(
t = Task({
    loop {
        [60, 62, 64, 65, 67, 69, 71, 72].do({ |midi|
            Synth(\singrain, [freq: midi.midicps, amp: 0.2, sustain: 0.1]);
            0.125.wait;
        });
    }
}).play;
)

// probably stops in the middle of the scale
t.stop;

t.play;    // should pick up with the next note

t.stop;




// When do you want to start?


(
f = {
    Task({
        loop {
            [60, 62, 64, 65, 67, 69, 71, 72].do({ |midi|
                Synth(\singrain, [freq: midi.midicps, amp: 0.2, sustain: 0.1]);
                0.25.wait;
            });
        }
    });
};
)

t = f.value.play(quant: 4);        // start on next 4-beat boundary

u = f.value.play(quant: [4, 0.5]);    // next 4-beat boundary + a half-beat

t.stop; u.stop;


// Using data routines in note sequencing

// The previous example controls the generation of one parameter (pitch) by looping over an array inside the Task. What if you want to control several parameters?

// Remember that routines can also generate data, in addition to their scheduling capabilities. You can refer to as many data routines as you want in your sequence.

(
var midi, dur, release, attack, tempoScale;

TempoClock.default.tempo = 1;
tempoScale = 1 / TempoClock.default.tempo; // values authored at 1 BPM
attack = 0.05 * tempoScale;
release = 0.1 * tempoScale;

midi = Routine({
	[60, 72, 71, 67, 69, 71, 72, 60, 69, 67].do({ |midi| midi.yield });
});
dur = Routine({
	[2, 2, 1, 0.5, 0.5, 1, 1, 2, 2, 4].do({ |dur| dur.yield });
});

SynthDef(\smooth, { |freq = 440, sustain = 1, amp = 0.2|
    var sig;
    sig = SinOsc.ar(freq, 0, amp) * EnvGen.kr(Env.linen(attack, sustain*tempoScale, release), doneAction: Done.freeSelf);
	Out.ar(0, sig ! 2)
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
}).play(quant: TempoClock.default.beats + 1); // delay by 1 beat so first note heard... documentation says this is necessary.
)


//
// A note on server messaging and timing

// Using Synth as in the preceding examples can result in small but sometimes noticeable timing inaccuracies. This is because it takes a short time to transmit OSC messages from your code to the server, and this time is not always constant. SuperCollider deals with this by giving you the option to send the message with a timestamp telling the server exactly when the message should take effect. A latency value is used to calculate the timestamp.

// Latency works by adding itself to the current time on the clock. If all the messages go out with the same latency value, their timing will be precise relative to each other and to the clock. The Scheduling and Server timing help file explains in more detail how this works, but you don't really need to know all of that in order to use it. The main point is to use a consistent, small latency value for perfect timing. (A Server object has a latency variable that you can use for consistency.)

// Here's an example illustrating the kinds of inaccuracy you might hear. The inaccuracy may be more or less noticeable on different systems. It uses the \singrain

// Notice that the time interval between notes is inconsistent.

(
t = Task({
    loop {
        Synth(\singrain, [freq: exprand(400, 1200), sustain: 0.08]);
        0.1.wait;
    }
}).play;
)

t.stop;

// The easiest way to add latency to your outgoing Synths is with the Server makeBundle method. Don't worry about how it works for now -- the important thing is that it uses the first value for latency, and runs the messages produced by the function according to that latency.

// Notice that now the time interval between notes is consistent.

(
t = Task({
    loop {
        s.makeBundle(s.latency, {
            Synth(\singrain, [freq: exprand(400, 1200), sustain: 0.08]);
        });
        0.1.wait;
    }
}).play;
)

t.stop;

