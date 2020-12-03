// Pbind

Pbind(\degree, Pseries(0, 1, 30), \dur, 0.05).play;

Pbind(\degree, 0).play; // Middle C
Pbind(\degree, [0,3,5]).play; // C major chord
Pbind(\degree, [0,3b,5]).play; // C minor chord

Pbind(\degree, 0, \dur, 0.5).play;


// Pseq
Pbind(\degree, Pseq([0,1,2,3,4,5,6,7], 1), \dur, 0.2).play; // play scale once
Pbind(\degree, Pseq([0,1,2,3,4,5,6,7], inf), \dur, 0.2).play; // loop forever


Pbind(\degree, Pseq([0, 1, 2, 3, 4, 5, 6, 7], 5), \dur, Pseq([0.2, 0.1, 0.1, 0.2,0.2, 0.35], inf)).play;

// same thing, but split up by lines to make more readable.
(
Pbind(
	\degree, Pseq([0, 1, 2, 3, 4, 5, 6, 7], 5),
	\dur, Pseq([0.2, 0.1, 0.1, 0.2,0.2, 0.35], inf)
).play;
)

// Four ways to specify pitch
Pbind(\degree, 5).play;
Pbind(\note, 9).play;
Pbind(\midinote, 69).play;
Pbind(\freq, 440).play;

// More keywords: amplitude and legato
(
Pbind(
	\degree, Pseq([0,−1, 2,−3, 4,−3, 7, 11, 4, 2, 0,−3], 5),
	\dur, Pseq([0.2, 0.1, 0.1], inf),
	\amp, Pseq([0.7, 0.5, 0.3, 0.2], inf),
	\legato, 0.47

).play;
)

// Prand
(
Pbind(
	\degree, Prand([2,3,4,5,6],inf),
	\dur, 0.15,
	\amp, 0.2,
	\legato, 0.1
).play;
)

// Pwhite
(
Pbind(
	\freq, Pwhite(100,500),
	\dur, Prand([0.15, 0.25, 0.3], inf),
	\amp, 0.2,
	\legato, 0.3
).trace.play;
)


// Fun with strum
(
Pbind(
	\note, Pseq([[−7, 3, 7, 10], [0, 3, 5, 8]], inf),
	\dur, 2,
	\legato, 0.8,
	\strum, 0.02 // try 0, 0.1, 0.2, etc
).play;
)

(
Pbind(\degree, Pseq([0, 0.1, 1, 2, 3, 4, 5, 6, 7]),
	\dur, 1;
).play(TempoClock(120/60)); // 120 beats over 60 seconds: 120 BPM
)

// Rests
(
Pbind(
	\degree, Pwhite(0, 10),
	\dur, Pseq([0.1, 0.1, 0.3, 0.6, Rest(0.3), 1], inf);
).play;
)

// Play two or more Pbinds together
(
Pbind(
	\freq, Pn(Pseries(110, 111, 10)),
	\dur, 1/2,
	\legato, Pwhite(0.1, 1)
).play;

Pbind(
	\freq, Pn(Pseries(220, 222, 10)),
	\dur, 1/4,
	\legato, Pwhite(0.1, 1)
).play;

Pbind(
	\freq, Pn(Pseries(330, 333, 10)),
	\dur, 1/6,
	\legato, 0.1
).play;
)


// fork: play Pbinds in a time ordered fashion

(
{
	"one thing".postln;
	2.wait;
	"another thing".postln;
	1.5.wait;
	"one last thing".postln;
}.fork;
)


(
t = TempoClock(76/60);
{
	Pbind(
		\note, Pseq([[4, 11], [6, 9]], 32),
		\dur, 1/6,
		\amp, Pseq([0.05, 0.03], inf)
	).play(t);

	2.wait;

	Pbind(
		\note, Pseq([[-25, -13, -1], [-20, -8, 4], \reset], 3),
		\dur, Pseq([1, 1, Rest(1)], inf),
		\amp, 0.1,
		\legato, Pseq([0.4, 0.7, \rest], inf)
	).play(t);

	2.75.wait;

	Pbind(
		\note, Pseq([23, 31, 25, 23, 31, 20, 18, 16, 20, 21, 23, 21], inf),
		\dur, Pseq([0.25, 0.75, 0.25, 1.75, 0.125, 0.80, 0.20, 0.125, 0.125, 1], 1),
		\tmp, 0.1,
		\legato, 0.5
	).play(t);
}.fork(t);
)

(
(
t = TempoClock(60/60);
{
	"beat".postln;
	1.wait;
	"beat".postln;
	1.wait;
	"beat".postln;
	1.wait;
}.fork(t);
)
)

// Pattern Guide Cookbook 01: Basic Sequencing

(
TempoClock.default.tempo = 84/60;

p = Pbind(
    \scale, #[0, 2, 3, 5, 7, 8, 10],
    \root, 2,
    \degree, Pseq(#[rest, 4, 3, 4, 2, 4, 1, 4, 0, 4, -0.9, 4, 0, 4, 1, 4, 2, 4,
        -3, 4, -1.9, 4, -0.9, 4, 0, 4, -0.9, 4, 0, 4, 1, 4, 2], 1),
    \dur, 0.25
).play;
)

(
p = Pbind(
    \scale, #[0, 2, 3, 5, 7, 8, 10],
    \root, 2,
    \degree, Place([#[rest, 3, 2, 1, 0, -0.9, 0, 1, 2, -3, -1.9, -0.9, 0, -0.9, 0, 1, 2],
        (4 ! 16) ++ \rest], 17),
    \dur, 0.25
).play;
)

(
p = Pbind(
    \scale, #[0, 2, 3, 5, 7, 8, 10],
    \root, 2,
    \degree, Ppatlace([Pseq(#[rest, 3, 2, 1, 0, -0.9, 0, 1, 2, -3, -1.9, -0.9, 0, -0.9, 0, 1, 2], 1),
        Pn(4, 16)], inf),
    \dur, 0.25
).play;
)


// Pattern Guide Cookbook 02: Manipulating Patterns

(
var    melodies = (
        lowMelody: Pseries(4, Prand(#[-2, -1, 1, 2], inf), inf).fold(-7, 11),
        highMelody: Pseries(14, Prand(#[-3, -2, 2, 3], inf), inf).fold(7, 18)
    );

p = Pbind(
    \degree, Pnsym1(Pwrand(#[lowMelody, highMelody], [0.7, 0.3], inf), melodies),
    \dur, Pwrand(#[0.25, 0.5], #[0.4, 0.6], inf)
).play;
)

p.stop;











// Pattern Guide Cookbook 05: Using Samples


(
b = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");

// one loop segment
SynthDef(\oneLoop, { |out, bufnum, start, time, amp, rate = 1|
    var sig = PlayBuf.ar(1, bufnum,
        rate: rate * BufRateScale.kr(bufnum),
        startPos: start, loop: 0
    ),
    env = EnvGen.kr(Env.linen(0.01, time, 0.05, level: amp),
        doneAction: Done.freeSelf);
    Out.ar(out, (sig * env).dup);
}).add;

SynthDef(\bell, { |out, accent = 0, amp = 0.1, decayScale = 1|
    var exc = PinkNoise.ar(amp)
    * Decay2.kr(Impulse.kr(0), 0.01, 0.05),
    sig = Klank.ar(`[
        { ExpRand(400, 1600) } ! 4,
        1 ! 4,
        { ExpRand(0.1, 0.4) } ! 4
    ], exc, freqscale: accent + 1, decayscale: decayScale);
    DetectSilence.ar(sig, doneAction: Done.freeSelf);
    Out.ar(out, sig.dup)
}).add;
)

(
var start = 0.404561, end = 3.185917,
beatsInLoop = 4,
originalTempo = beatsInLoop / (end - start);

TempoClock.tempo = originalTempo;

p = Ptpar([
    0, Pbind(
        \instrument, \oneLoop,
        \bufnum, b,
        \amp, 0.4,
        \start, start * b.sampleRate,
        \dur, beatsInLoop,
        \time, Pkey(\dur) / Pfunc { thisThread.clock.tempo },
        \rate, Pfunc { thisThread.clock.tempo / originalTempo }
    ),
    0.5, Pn(
        Pfindur(4,
            Pbind(
                \instrument, \bell,
                \accent, Pseq([2, Pn(0, inf)], 1),
                \amp, Pseq([0.3, Pn(0.1, inf)], 1),
                \decayScale, Pseq([6, Pn(1, inf)], 1),
                \dur, Pwrand(#[0.25, 0.5, 0.75, 1], #[2, 3, 1, 1].normalizeSum, inf)
            )
        ),
    inf),
    0.5, Pbind(
        \instrument, \bell,
        \accent, -0.6,
        \amp, 0.2,
        \decayScale, 0.1,
        \dur, 1
    )
], 1).play;
)

// for fun, change tempo
// resyncs on next bar
TempoClock.tempo = 104/60;

p.stop;






(
~swingify = Prout({ |ev|
    var now, nextTime = 0, thisShouldSwing, nextShouldSwing = false, adjust;
    while { ev.notNil } {
        // current time is what was "next" last time
        now = nextTime;
        nextTime = now + ev.delta;
        thisShouldSwing = nextShouldSwing;
        nextShouldSwing = ((nextTime absdif: nextTime.round(ev[\swingBase])) <= (ev[\swingThreshold] ? 0)) and: {
            (nextTime / ev[\swingBase]).round.asInteger.odd
        };
        adjust = ev[\swingBase] * ev[\swingAmount];
        // an odd number here means we're on an off-beat
        if(thisShouldSwing) {
            ev[\timingOffset] = (ev[\timingOffset] ? 0) + adjust;
            // if next note will not swing, this note needs to be shortened
            if(nextShouldSwing.not) {
                ev[\sustain] = ev.use { ~sustain.value } - adjust;
            };
        } {
            // if next note will swing, this note needs to be lengthened
            if(nextShouldSwing) {
                ev[\sustain] = ev.use { ~sustain.value } + adjust;
            };
        };
        ev = ev.yield;
    };
});
)


p = Pbind(\degree, Pseries(0, 1, 8), \dur, 0.25);

p.play;  // straight 16ths

// swingBase: 0.25: Every other 16th-note is delayed
// swingAmount: 1/3: Off-beat notes will be delayed by 1/3 of a 16th-note
Pchain(~swingify, p, (swingBase: 0.25, swingAmount: 1/3)).play;

// note duration = twice swingBase, no swing (correct)
Pchain(~swingify, Pstretch(2, p), (swingBase: 0.25, swingAmount: 1/3)).play;

// hear the result of different swing amounts
(
Ppar([
    // 60% of a 16th-note
    Pchain(~swingify, p, (swingBase: 0.25, swingAmount: 0.6, pan: -1)),
    // 20% of a 16th-note
    Pchain(~swingify, p, (swingBase: 0.25, swingAmount: 0.2, pan: 1, octave: 6))
]).play;
)

(
q = Ppar([
    // walking bass (by an idiot bass player who only chooses notes randomly)
    Pbind(
        \octave, 3,
        \degree, Pwhite(0, 7, inf),
        \dur, 0.5
    ),
    Pseq([
        Pchain(
            ~swingify,
            Pbind(
                \degree, Pseries(-7, 1, 15) +.x Pseq([0, 9], 1),
                \dur, Pwhite(1, 3, inf) * 0.25
            ),
            (swingBase: 0.25, swingAmount: 0.2)
        ),
        Pfuncn({ q.stop; Event.silent(1) }, 1)
    ])
]).play;
)

(
Pchain(
    ~swingify,
    Pbind(
        \degree, Pseries(-7, 1, 15),
        \dur, Pwrand([Pn(0.25, 2), Pn(1/6, 3)], [0.7, 0.3], inf)
    ),
    (swingBase: 0.25, swingAmount: 0.2, swingThreshold: 0.05)
).play;
)




