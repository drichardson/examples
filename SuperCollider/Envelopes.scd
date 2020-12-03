// From A Gentle Introduction to SuperCollider
// https://ccrma.stanford.edu/~ruviaro/texts/A_Gentle_Introduction_To_SuperCollider.pdf

// Use test to hear and plot to see
Env.perc.test.plot;
Env.adsr.test(3); // set the relase time to 3... only applies to sustaining envelopes like ADSR.

(
{
    EnvGen.kr(
        Env(
            levels: [0, 0.1, 0.2, 0.3],
            times: [0.1, 0.1, 0.1],
            curve: 8
        ),
        gate: Impulse.kr(3)
    );
}.plot(duration: 1);
)

// EnvGen

// as amplitude envelope
(
{
    var env = Env([0, 1, 0.5, 1, 0], [0.01, 0.5, 0.02, 0.5]);
    SinOsc.ar(470) * EnvGen.kr(env, doneAction: Done.freeSelf)
}.play
)

// as amplitude and modulation envelope
(
{
    var env = Env([0, 1, 0.5, 0.8, 0, 1.2, 0], [0.01, 0.5, 0.02, 0.5, 0.2, 0.5]);
    var gate = Impulse.kr(MouseX.kr(0.2, 3), 0.5);
    var gen = EnvGen.kr(env, gate);
    SinOsc.ar(270, SinOsc.ar(gen * 473)) * gen * 0.2
}.play
)

// EnvGen multichannel expands when passed a multichannel envelope
(
{
    SinOsc.ar(
        EnvGen.kr(
            Env.circle([0, 1, 0, (2..4), 0, LFNoise1.kr(0.1 ! 5) * 10, 0], [0.01, 0.6])
        )
        * 240 + 300
    ).sum * 0.2
}.play;
)

// retriggered envelope by Dust
(
{
    var env = Env([0.0, 0.5, 0.0, 1.0, 0.9, 0.0], [0.05, 0.1, 0.01, 1.0, 1.5], -4);
    var envgen = EnvGen.ar(env, Dust.ar(1));
    SinOsc.ar(
        envgen * 1000 + 440
    ) * envgen * 0.1
}.play
);


// an envelope in a SynthDef can be used to limit the synth's lifetime (doneAction: Done.freeSelf)

(
SynthDef(\env_help, { | out, gate = 0, freq = 440 |
    var z;
    z = EnvGen.kr(Env.perc, doneAction: Done.freeSelf) * SinOsc.ar(freq, 0, 0.1);
    Out.ar(out, z)
}).add;
)

(
fork {
    10.do {
        Synth(\env_help);
        0.2.rand.wait;
    }
}
)

// using a gated envelope to gate a sound:
(
SynthDef(\env_help, { | out, gate = 0, freq = 440, doneAction = 0 |
    var z = EnvGen.kr(Env.adsr, gate, doneAction: doneAction) * SinOsc.ar(freq, 0, 0.1);
    Out.ar(out, z)
}).add;
)

a = Synth(\env_help);


// turn on
a.set(\gate, 1);

// turn off
a.set(\gate, 0);

// it does not matter to what value the gate is set, as long as it is > 0
a.set(\gate, 2);

a.set(\doneAction, 2, \gate, 0); // set doneAction to two to let the synth free itself

a.free; // alternatively, free it directly.


// Specifying an envelope for each new synth
(
SynthDef(\help_Env_newClear, { |out = 0|
    var env, envctl;
    // make an empty 4 segment envelope
    env = Env.newClear(4);
    // create a control argument array
    envctl = \env.kr(env.asArray);
    Out.ar(out,
        SinOsc.ar(EnvGen.kr(envctl, \gate.tr), 0, 0.3) // the gate control is a trigger
    );
}).add;
)

Synth(\help_Env_newClear, [\gate, 1, \env, Env([700,900,900,800], [1,1,1], \exp)]); // 3 segments

// reset then play again:
Synth(\help_Env_newClear, [\gate, 1, \env, Env({ rrand(60, 70).midicps } ! 4, [1,1,1], \exp)]);

// the same written as an event:
(instrument: \help_Env_newClear, gate: 1, env: Env({ rrand(60, 70).midicps } ! 4, [1,1,1], \exp)).play;



// Forced release

(
SynthDef(\stealMe, { |out, gate = 1|
    Out.ar(out, {BrownNoise.ar}.dup * EnvGen.kr(Env.asr, gate, doneAction: Done.freeSelf))
}).add;
)
​
a = Synth(\stealMe);
a.release(3); //  // cutoff in 3 seconds
​
// this is how the OSC data looks like:
s.sendMsg(\s_new, \stealMe, 1001, 1, 0);
s.sendMsg(\n_set, 1001, \gate, -1.1); // cutoff in 0.1 seconds

// If the synthDef has an arg named "gate", the convenience method of Node can be used: node.release(releaseTime)
d = { arg gate=1; {BrownNoise.ar}.dup * EnvGen.kr(Env.asr, gate, doneAction: Done.freeSelf) }.play;
d.release(3);


// Fast triggering tests
(
{
    EnvGen.kr(
        Env.new([ 0.001, 1, 0.5, 0 ], [ 0.01, 0.3, 1 ], -4, 2, nil),
        Impulse.kr(10)
    ) * SinOsc.ar(440, 0, 0.1)
}.play;
)

(
{
    EnvGen.kr(
        Env.perc( 0.1, 0.0, 0.5, 1, \welch ),
        Impulse.kr(100),
        timeScale: 0.1
    ) * SinOsc.ar(440, 0, 0.3)
}.play;
)





// Changing an Env while playing
(
SynthDef(\env, { arg i_outbus=0;
    var env, envctl;

    // make a dummy 8 segment envelope
    env = Env.newClear(8);

    // create a control argument array
    envctl = \env.kr( env.asArray );

    ReplaceOut.kr(i_outbus, EnvGen.kr(envctl, doneAction: Done.freeSelf));
}).add;
)

(
SynthDef(\sine, { |out, freq = 440|
    Out.ar(out, SinOsc.ar(freq, 0, 0.2));
}).add;
)

f = Bus.control(s, 1);
f.set(800);

// use f's control bus value for frequency
// i.e. *map* the control to read from the bus
a = Synth(\sine, [freq: f.asMap]);

Synth(\env, [i_outbus: f, env: Env([700, 900, 900, 800], [1, 1, 1]*0.4, \exp)]);

Synth(\env, [i_outbus: f, env: Env([1000, 1000, 800, 1000, 900, 1000], [1, 1, 1, 1, 1]*0.3, \step)]);

a.free;
f.free;



// Env.perc

(
[
	Env.perc, // using all default args
	Env.perc(0.5), // attackTime: 0.5
	Env.perc(attackTime: 0.3, releaseTime: 2, level: 0.4),
	Env.perc(0.3, 2, 0.4, 0) // same as above, but curve:0 means straight lines
].plot;
)

Env.perc.test;

// now hook Env.perc up to a synth
{PinkNoise.ar(Env.perc.kr(doneAction: 2))}.play; // default Env.perc args
{PinkNoise.ar(Env.perc(0.5).kr(doneAction: 2))}.play;
{PinkNoise.ar(Env.perc(0.3, 2, 0.4).kr(2))}.play;
{PinkNoise.ar(Env.perc(0.3, 2, 0.4, 0).kr(2))}.play;



// Env.triangle
Env.triangle.plot;
{SinOsc.ar([440, 442], mul: Env.triangle.kr(2))}.play;
// By the way, an envelope can be a multiplier anywhere in your code
{SinOsc.ar([440, 442])*Env.triangle.kr(2)}.play;

// Env.linen
Env.linen.plot;
{SinOsc.ar([300, 350], mul: Env.linen(0.01, 2, 1, 0.2).kr(2))}.play;

// Env.pairs
(
{
	var env = Env.pairs([[0, 0], [0.4, 1], [1, 0.2], [1.1, 0.5], [2, 0]], \lin);
	env.plot;
	SinOsc.ar([440, 442], mul: env.kr(2));

}.play;

)

// Envelopes are not just for amplitude.

({
	var freqEnv = Env.pairs([[0, 100], [0.4, 1000], [0.9, 400], [1.1, 555], [2,440]], \lin);
	SinOsc.ar(freqEnv.kr, mul: 0.2);

}.play;
)


// ADSR Envelope

// ASR
// Play note ('press key')
// attackTime: 0.5 seconds, sustainLevel: 0.8, releaseTime: 3 seconds
x = {arg gate = 1, freq = 440; SinOsc.ar(freq: freq, mul: Env.asr(0.5, 0.8, 3).kr(doneAction: 2, gate: gate))}.play;
// Stop note ('finger off the key'−activate release stage)
x.set(\gate, 0); // alternatively, x.release

// ADSR (attack, decay, sustain, release)
// Play note:
(
d = {arg gate = 1;
	var snd, env;
	env = Env.adsr(0.01, 0.4, 0.7, 2);
	snd = Splay.ar(BPF.ar(Saw.ar((32.1, 32.2..33)), LFNoise2.kr(12).range(100,1000), 0.05, 10));
	Out.ar(0, snd*env.kr(doneAction: 2, gate: gate));

}.play;
)
// Stop note:
d.release; // this is equivalent to d.set(\gate, 0);







Env.new(levels: [0, 1, 0.9, 0], times: [0.1, 0.5, 1], curve: [-5, 0, -5]).plot;
{ SinOsc.ar(EnvGen.kr(Env.circle([0, 1, 0], [0.01, 0.5, 0.2])) * 440 + 200) * 0.2 }.play;




// an envelope in a synth
(
{
    var env = Env([0, 1, 0.5, 1, 0], [0.01, 0.5, 0.02, 0.5]);
    SinOsc.ar(470) * EnvGen.kr(env, doneAction: Done.freeSelf)
}.play
)
// an envelope to control a parameter in a pattern
(
Pbind(
    \note,  Env([0, 12, 6, 13, 0], [1, 5, 2, 10]),
    \dur, 0.1
).play
)

//
// Standard Shape Envelope Creation Methods
//

Env.linen(0.1, 0.2, 0.1, 0.6).test.plot;
Env.linen(1, 2, 3, 0.6).test.plot;
Env.linen(1, 2, 3, 0.6, \sine).test.plot;
Env.linen(1, 2, 3, 0.6, \welch).test.plot;
Env.linen(1, 2, 3, 0.6, -3).test.plot;
Env.linen(1, 2, 3, 0.6, -3).test.plot;
Env.linen(1, 2, 3, 0.6, [[\sine, \welch, \lin, \exp]]).plot;

Env.triangle(1, 1).test.plot;

Env.sine(1, 1).test.plot;

Env.perc(0.05, 1, 1, -4).test.plot;
Env.perc(0.001, 1, 1, -4).test.plot;    // sharper attack
Env.perc(0.001, 1, 1, -8).test.plot;    // change curvature
Env.perc(1, 0.01, 1, 4).test.plot;    // reverse envelope

Env.pairs([[0, 1], [2.1, 0.5],  [3, 1.4]], \exp).test.plot;
Env.pairs([[0, 1], [3, 1.4], [2.1, 0.5], [3, 4]], \exp).test.plot; // *if possible*, pairs are sorted according to time
Env.pairs({ { 1.0.rand } ! 2 } ! 16, \exp).test.plot;


Env.xyc([[0, 1, \sin], [2.1, 0.5, \lin],  [3, 1.4, \lin]]).test.plot;
Env.xyc([[2.1, 0.5, \lin], [0, 1, \sin], [3, 1.4, \lin]]).test.plot; // *if possible*, pairs are sorted according to time
Env.xyc({ [1.0.rand, 10.0.rand, -4.rand2] } ! 16, \exp).test.plot;

//
// Sustained Envelope Creation Methods
//

(
{
    var env = Env.step([0, 3, 5, 2, 7, 3, 0, 3, 4, 0], [0.5, 0.1, 0.2, 1.0, 1.5, 2, 0.2, 0.1, 0.2, 0.1]);
    var envgen = EnvGen.kr(env);
    var freq = (envgen + 60).midicps;
    SinOsc.ar(freq) * 0.1
}.play
);

Env.adsr(0.02, 0.2, 0.25, 1, 1, -4).test(2).plot;
Env.adsr(0.001, 0.2, 0.25, 1, 1, -4).test(2).plot;
Env.adsr(0.001, 0.2, 0.25, 1, 1, -4).test(0.45).plot;    // release after 0.45 sec

Env.asr(0.02, 0.5, 1, -4).test(2).plot;
Env.asr(0.001, 0.5, 1, -4).test(2).plot; // sharper attack
Env.asr(0.02, 0.5, 1, 'linear').test(2).plot; // linear segments

Env.cutoff(1, 1).test(2).plot;
Env.cutoff(1, 1, 4).test(2).plot;
Env.cutoff(1, 1, \sine).test(2).plot;

{ SinOsc.ar(EnvGen.kr(Env.circle([0, 1, 0], [0.01, 0.5, 0.2])) * 440 + 200) * 0.2 }.play;
{ SinOsc.ar(EnvGen.kr(Env.circle([0, 1, 0, 2, 0, 1, 0], [0.01, 0.3])) * 440 + 200) * 0.2 }.play;
{ SinOsc.ar(EnvGen.kr(Env.circle([0, 1, 0, (2..4), 0, (1..3), 0], [0.01, 0.3])) * 440 + 200).sum * 0.2 }.play; // multichannel expanded levels

//
// Multichannel expansion
//
(
{
    var env = Env([0.0, 0.5, 0.0, [1.0, 1.25, 1.5], 0.9, 0.0], [0.05, 0.1, 0.01, 1.0, 1.5], -4);
    var envgen = EnvGen.ar(env, doneAction: Done.freeSelf);
    SinOsc.ar(
        envgen * 1000 + 440
    ) * envgen * 0.1
}.play
);

(
{
    var env = Env([1, [1, 2, 3], 0.5, 0.5, [3, 2, 1], 2], [1, 1, 0.5, 1], [[\exp, \sin]]);
    env.plot;
    Splay.ar(SinOsc.ar(EnvGen.kr(env) * 400 + 600)) * 0.1
}.play;
);

(
{
    var levels = (1..30);
    var env = Env([1, levels, 0.5, levels / 2.5, 2], [1, 0.15, 1, 0.25, 0.1], \exp);
    Splay.ar(SinOsc.ar(EnvGen.kr(env) * 400 + 600)) * 0.1
}.play;
);


// multichannel levels

Env([0.1, 1, 0.1], [1, [1, 2, 3]], \exp).plot;
Env([0.1, 1, 0.1], [1, [1, 2, 3]], [\lin, [\lin, \exp, \sin]]).plot;

Env([1, 1, 0.5, 3, 2], [1, 0.5, 1, 0.25], \exp).plot;
Env([0, 1, 0, 2, 0] * [[1, 2, 3]], [1, 0.5, 1, 0.25], \lin).plot;

// multichannel curves

Env([0.01, 5, 1, 0.5] + 1, [1, 0.5, 1, 0.25], [[\lin, \sqr]]).plot;

Env([0.01, 5, 1, 0.5, 0.001] + 1, [1, 0.5, 1, 0.25, 1], [[\lin, \cub, \sin, \cubed, \welch, \step, \exp]]).plot(bounds: Rect(30, 100, 500, 700));

Env([0.01, 5, 1, 0.5, 0.001] + 1, [1, 0.5, 1, 0.25, 1], [(-4..4)]).plot(bounds: Rect(30, 100, 500, 700));
Env([0.01, 5, 1, 0.5] + 1, [1, 0.5, 1, 0.25], [(-4..4)]).plot(bounds: Rect(30, 100, 500, 700));


Env([[0, 0.01], 1, 0], [0.5, 0.5], [[\lin, \exp], \step]).plot;
Env([[0, 0.01], 1, [0, 0.01]], [0.5, 1], [[\lin, \exp]]).plot;

// multichannel times

Env([[2, 1], 0], [[1, 2]], \lin).plot;
Env([0, 1], [1/(1..5)], [(-4..4)]).plot(bounds: Rect(30, 100, 300, 700));
Env([0, 1], [1/(1..5)], \lin).plot(bounds: Rect(30, 100, 300, 700));


// mixed expansions

Env([1, [ 1, 2, 3, 4, 5 ], 0.5, [3, 2, 1], 2], [1, 0.5, 1, 0.25], [[\exp, \lin]]).plot;
Env([1, [ 1, 2, 3, 4, 5 ], 0.5, 4, 2], [1, 0.5, 1, 0.25], \exp).plot;


// expanding control point envelopes

Env.xyc([[2, 0.5, [\lin, \exp]], [0, 1, \lin], [3, 1.4, \lin]]).plot;
Env.xyc({ [1.0.rand, 1.0.rand, {[\lin, \exp, \step].choose} ! 3] } ! 8).plot

Env.xyc([[[2.0, 2.3], 0.5, \lin], [0, 1, \lin], [3, 1.4, \lin]]).plot; // multiple times

Env(levels: [0, 1, 1, 0], times: [0.1, 1, 3], curve: [4, 0, -4]).plot;
Env(levels: [0, 1, 1, 0], times: [0.1, 1, 3], curve: 0).plot;
