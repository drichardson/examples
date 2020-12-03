(
SynthDef("help-OffsetOut",
    { arg out=0, freq=440, dur=0.05;
        var env;
        env = EnvGen.kr(Env.perc(0.01, dur, 0.2), doneAction: Done.freeSelf);
        OffsetOut.ar(out, SinOsc.ar(freq, 0, env))
}).send(s);

SynthDef("help-Out",
    { arg out=0, freq=440, dur=0.05;
        var env;
        env = EnvGen.kr(Env.perc(0.01, dur, 0.2), doneAction: Done.freeSelf);
        //compare to Out:
        Out.ar(out, SinOsc.ar(freq, 0, env))
}).send(s);
)


Synth('help-OffsetOut');

// these are in sync
(
Routine({
    loop {
        s.sendBundle(0.2, ["/s_new", "help-OffsetOut", -1]);
        0.01.wait;
    }
}).play;
)

// these are less reliably in sync and are placed at multiples of blocksize.
(
Routine({
    loop {
        s.sendBundle(0.2, ["/s_new", "help-Out", -1]);
        0.01.wait;
    }
}).play;
)






(
plot({
    var z;
    z = Dust.ar(1000);
    [z, z - Delay1.ar(z)]     // [ original, subtract delayed from original ]
}))

(
plot({
    var z;
    z = Dust.ar(1000);
    [z, z - Delay2.ar(z)]     // [ original, subtract delayed from original ]
}))



// These examples compare the variants, so that you can hear the difference in interpolation

// Comb used as a resonator. The resonant fundamental is equal to
// reciprocal of the delay time.

{ CombN.ar(WhiteNoise.ar(0.01), 0.01, XLine.kr(0.0001, 0.01, 20), 0.2) }.play;

{ CombL.ar(WhiteNoise.ar(0.01), 0.01, XLine.kr(0.0001, 0.01, 20), 0.2) }.play;

{ CombC.ar(WhiteNoise.ar(0.01), 0.01, XLine.kr(0.0001, 0.01, 20), 0.2) }.play;

// with negative feedback:
{ CombN.ar(WhiteNoise.ar(0.01), 0.01, XLine.kr(0.0001, 0.01, 20), -0.2) }.play;

{ CombL.ar(WhiteNoise.ar(0.01), 0.01, XLine.kr(0.0001, 0.01, 20), -0.2) }.play;

{ CombC.ar(WhiteNoise.ar(0.01), 0.01, XLine.kr(0.0001, 0.01, 20), -0.2) }.play;

// used as an echo.
{ CombN.ar(Decay.ar(Dust.ar(1,0.5), 0.2, WhiteNoise.ar), 0.2, 0.2, 3) }.play;

// large delay lines
(
s.options.memSize= 8192*2;
s.reboot;
)
({
    var in, del1, del2, del3, del4;
    in = SoundIn.ar(0);
    del1 = CombC.ar(in, 8, LFNoise2.kr(0.01).range(1, 8), 1);
    del2 = CombC.ar(del1, 8, LFNoise2.kr(0.01).range(1, 8), 1);
    del3 = CombC.ar(del2, 8, LFNoise2.kr(0.01).range(1, 8), 1);
    del4 = CombC.ar(del3, 8, LFNoise2.kr(0.01).range(1, 8), 1);
    (del1 + del2 + del3 + del4)!2;
}.play;
);




// Since the allpass delay has no audible effect as a resonator on
// steady-state sound ...

{ AllpassC.ar(WhiteNoise.ar(0.1), 0.01, XLine.kr(0.0001, 0.01, 20), 0.2) }.play;

// ...these examples add the input to the effected sound and compare variants so that you can hear
// the effect of the phase comb:

(
{
    z = WhiteNoise.ar(0.2);
    z + AllpassN.ar(z, 0.01, XLine.kr(0.0001, 0.01, 20), 0.2)
}.play)

(
{
    z = WhiteNoise.ar(0.2);
    z + AllpassL.ar(z, 0.01, XLine.kr(0.0001, 0.01, 20), 0.2)
}.play)

(
{
    z = WhiteNoise.ar(0.2);
    z + AllpassC.ar(z, 0.01, XLine.kr(0.0001, 0.01, 20), 0.2)
}.play)


// used as an echo - doesn't really sound different than Comb,
// but it outputs the input signal immediately (inverted) and the echoes
// are lower in amplitude.
{ AllpassC.ar(Decay.ar(Dust.ar(1,0.5), 0.2, WhiteNoise.ar), 0.2, 0.2, 3) }.play;







(
s = Server.local;
s.waitForBoot({

b = Buffer.alloc(s, 44100 * 2, 2);

SynthDef("help-PingPong", { |out = 0, bufnum = 0, feedback = 0.5, delayTime = 0.2|
    var left, right;
    left = Decay2.ar(Impulse.ar(0.7, 0.25), 0.01, 0.25,
        SinOsc.ar(SinOsc.kr(3.7,0,200,500)));
    right = Decay2.ar(Impulse.ar(0.5, 0.25), 0.01, 0.25,
        Resonz.ar(PinkNoise.ar(4), SinOsc.kr(2.7,0,1000,2500), 0.2));

    Out.ar(out ,
        PingPong.ar(bufnum, [left,right], delayTime, feedback, 1)
    )
}).play(s, [\out, 0, \bufnum, b.bufnum, \feedback, 0.5, \delayTime,0.1]);

})
)

b.free;

(
s = Server.local;
s.waitForBoot({

b = Buffer.alloc(s, 44100 * 2, 2);

SynthDef("help-PingPong", { |out = 0, bufnum = 0|
    var left, right;
    left = Decay2.ar(Impulse.ar(0.7, 0.25), 0.01, 0.25,
        SinOsc.ar(SinOsc.kr(3.7,0,200,500)));
    right = Decay2.ar(Impulse.ar(0.5, 0.25), 0.01, 0.25,
        Resonz.ar(PinkNoise.ar(4), SinOsc.kr(2.7,0,1000,2500),
0.2));

    Out.ar(out,
        PingPong.ar(bufnum, [left, right] *  EnvGen.kr(Env([1, 1, 0], [2, 0.1])),
            0.1, 0.8, 1)
    )
}).play(s, [\out, 0, \bufnum, b.bufnum]);
});
)

b.free;








(
// Dust randomly triggers Decay to create an exponential
// decay envelope for the WhiteNoise input source
{
z = Decay.ar(Dust.ar(1,0.5), 0.3, WhiteNoise.ar);
DelayN.ar(z, 0.2, 0.2, 1, z); // input is mixed with delay via the add input
}.play
)

(
// recursive application of delay.
{
z = Decay2.ar(Dust.ar(1, 0.5), 0.01, 0.1, Saw.ar(100 + [0, 1]));
5.do { |i| z = DelayN.ar(RLPF.ar(z, Rand(100, 3000), 0.03), 1, 1 / (2**i), 1, z * 0.5) };
z
}.play
)