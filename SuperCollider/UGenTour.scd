// Most/all of these are from
// https://doc.sccode.org/Guides/Tour_of_UGens.html


//
// my favorites from the tour
//

// modulate bandwidth opposite direction
{ Resonz.ar(WhiteNoise.ar(0.5), 2000, XLine.kr(0.001, 1, 8)) }.scope(1);

{ Klank.ar(`[[200, 671, 1153, 1723], nil, [1, 1, 1, 1]], PinkNoise.ar(0.007)) }.play;

{ Klank.ar(`[ {exprand(200, 4000)}.dup(12), nil, nil ], PinkNoise.ar(0.007)) }.scope(1);


(
{
    // play with the room size
    var x;
    x = Klank.ar(`[[200, 671, 1153, 1723], nil, [1, 1, 1, 1]], Dust.ar(2, 0.1));
    x = Pan2.ar(x, -0.2);
    x = [x[0], DelayC.ar(x[1], 0.01, 0.01)]; // de-correlate
    FreeVerb.ar(x, 0.75, 0.9, 0.4);
}.scope;
)

(
{
    // play with the room size
    var x;
    x = Klank.ar(`[[200, 671, 1153, 1723], nil, [1, 1, 1, 1]], Dust.ar(2, 0.1));
    GVerb.ar(x, 105, 5, 0.7, 0.8, 60, 0.1, 0.5, 0.4) + x;
}.scope;
)


(
{
    var z;
    z = Decay.ar(Dust.ar(1,0.5), 0.1, WhiteNoise.ar);
    8.do { z = AllpassL.ar(z, 0.04, 0.04.rand, 2) };
    z
}.scope(1);
)


( // using default window
{
    var trigrate, winsize, trig;
    trigrate = MouseX.kr(2, 120);
    winsize = trigrate.reciprocal;
    trig = Impulse.ar(trigrate);
    GrainSin.ar(2, trig, winsize, TRand.ar(440.0, 880.0, trig), LFNoise1.kr(0.2),
        -1, 512, 0.2)
}.scope(2, zoom: 4);
)


{ SinOsc.ar(Lag.ar(LFPulse.ar(2,0,0.5,800,400), MouseX.kr(0,0.5)), 0, 0.2) }.scope(1, zoom:4);


(
{
    // mixing sine oscillators in parallel
    var n = 16; // number of structures to make
    // mix together  parallel structures
    Mix.fill(n,
            {
                var amp;
                amp = FSinOsc.kr(exprand(0.1,1),2pi.rand).max(0);
                Pan2.ar(
                    FSinOsc.ar(exprand(100,1000.0), 0, amp),
                    1.0.rand2)
            }
    ) / (2*n)            // scale amplitude
}.scope(2);
)


(
{
    var sig, chain;

    // The original sound source
    sig = sum({ SinOsc.ar(rrand(50,6000),0,2*Decay.ar(Dust2.ar(1),0.1)).tanh } ! 7);

    chain = sig;    // Start with the original signal
    8.do {|i|       // Loop 8 times. For each loop, connect the signal through something.

        // A simple reverb
        chain = LeakDC.ar(AllpassL.ar(LPF.ar(chain*0.9,3000), 0.2, {0.19.rand+0.01}!2, 3));
    };

    Limiter.ar(sig+chain);    // dry + wet
}.play;
)



//
// Random Stuff
//


// Constant values

{ DC.ar(0.5) }.plot;
{ DC.ar(-1) }.plot;

{ Silent.ar() }.scope;
{ Silent.ar() }.plot;


//
// Mostly Copy Paste from the UGen Tour
//


{ Pulse.ar(XLine.kr(20000,200,6),0.3,0.2) }.scope(1);

{ Klang.ar(`[ [800, 1000, 1200],[0.3, 0.3, 0.3],[pi,pi,pi]], 1, 0) * 0.4}.scope(1);
{ Klang.ar(`[ {exprand(400, 2000)}.dup(16), nil, nil ], 1, 0) * 0.04 }.scope(1);

(
b = Buffer.alloc(s, 2048, 1, bufnum: 80);
b.sine1(1.0/(1..6), true, true, true);
)

{ Osc.ar(80, 100, 0, 0.1) }.scope(1, zoom:4);

b.sine1(1.0/(1..12));
b.sine1(1.0/(1..24));
b.sine1(1.0/(1..32));
b.sine1([1.0/(1,3..12), 0].flop.flat.postln);
b.sine1([1.0/(1,3..32).squared, 0].flop.flat.postln);
b.sine1((1.dup(4) ++ 0.dup(8)).scramble.postln);
b.sine1((1.dup(4) ++ 0.dup(8)).scramble.postln);
b.sine1((1.dup(4) ++ 0.dup(8)).scramble.postln);
b.sine1((1.dup(4) ++ 0.dup(8)).scramble.postln);
b.sine1({1.0.rand2.cubed}.dup(8).round(1e-3).postln);
b.sine1({1.0.rand2.cubed}.dup(12).round(1e-3).postln);
b.sine1({1.0.rand2.cubed}.dup(16).round(1e-3).postln);
b.sine1({1.0.rand2.cubed}.dup(24).round(1e-3).postln);

b.sine1(1.0/(1..6), true, true, true);

{ COsc.ar(80, 100, 1, 0.1) }.scope(1, zoom:4);

(
// allocate tables 80 to 87
8.do {|i| s.sendMsg(\b_alloc, 80+i, 1024); };
)


(
// fill tables 80 to 87
8.do({|i|
    var n, a;
    // generate array of harmonic amplitudes
    n = (i+1)**2; // num harmonics for each table: [1,4,9,16,25,36,49,64]
    a = {|j| ((n-j)/n).squared }.dup(n);
    // fill table
    s.listSendMsg([\b_gen, 80+i, \sine1, 7] ++ a);
});
)

{ VOsc.ar(MouseX.kr(80,87), 120, 0, 0.3) }.scope(1, zoom:4);

(
// allocate and fill tables 80 to 87
8.do({|i|
    // generate array of harmonic amplitudes
    a = {1.0.rand2.cubed }.dup((i+1)*4);
    // fill table
    s.listSendMsg([\b_gen, 80+i, \sine1, 7] ++ a);
});
)

{ VOsc3.ar(MouseX.kr(80,87), 120, 121.04, 119.37, 0.2) }.scope(1, zoom:4);

{ VOsc3.ar(MouseX.kr(80,87), 120, 151.13, 179.42, 0.2) }.scope(1, zoom:4);

{ LFClipNoise.ar(MouseX.kr(200, 10000, 1), 0.125) }.scope(1);
{ LFNoise0.ar(MouseX.kr(200, 10000, 1), 0.25) }.scope(1);

{ ClipNoise.ar(0.2) }.scope(1);
{ WhiteNoise.ar(0.2) }.scope(1);
{ PinkNoise.ar(0.4) }.scope(1);
{ BrownNoise.ar(0.2) }.scope(1);
{ GrayNoise.ar(0.2) }.scope(1);

{ Dust.ar(MouseX.kr(1,10000,1), 0.4) }.scope(1, zoom:4);
{ Dust2.ar(MouseX.kr(1,10000,1), 0.4) }.scope(1, zoom:4);

{ Crackle.ar(MouseX.kr(1,2), 0.5) }.scope(1);

{ LPF.ar(WhiteNoise.ar, MouseX.kr(1e2,2e4,1), 0.2) }.scope(1);
{ HPF.ar(WhiteNoise.ar, MouseX.kr(1e2,2e4,1), 0.2) }.scope(1);
{ LPF.ar(Saw.ar(100), MouseX.kr(1e2,2e4,1), 0.2) }.scope(1);
{ HPF.ar(Saw.ar(100), MouseX.kr(1e2,2e4,1), 0.2) }.scope(1);

{ BPF.ar(WhiteNoise.ar, MouseX.kr(1e2,2e4,1), 0.4, 0.4) }.scope(1);
{ BRF.ar(WhiteNoise.ar, MouseX.kr(1e2,2e4,1), 0.4, 0.2) }.scope(1);
{ BPF.ar(Saw.ar(100), MouseX.kr(1e2,2e4,1), 0.4, 0.4) }.scope(1);
{ BRF.ar(Saw.ar(100), MouseX.kr(1e2,2e4,1), 0.4, 0.2) }.scope(1);

// modulating the bandwidth
{ BPF.ar(WhiteNoise.ar, 3000, MouseX.kr(0.01,0.7,1), 0.4) }.scope(1);

// modulate frequency
{ Resonz.ar(WhiteNoise.ar(0.5), XLine.kr(1000,8000,10), 0.05) }.scope(1);

// modulate bandwidth
{ Resonz.ar(WhiteNoise.ar(0.5), 2000, XLine.kr(1, 0.001, 8)) }.scope(1);

// modulate bandwidth opposite direction
{ Resonz.ar(WhiteNoise.ar(0.5), 2000, XLine.kr(0.001, 1, 8)) }.scope(1);












{ Ringz.ar(Dust.ar(3, 0.3), 2000, 2) }.scope(1, zoom:4);

{ Ringz.ar(WhiteNoise.ar(0.005), 2000, 0.5) }.scope(1);

// modulate frequency
{ Ringz.ar(WhiteNoise.ar(0.005), XLine.kr(100,3000,10), 0.5) }.scope(1, zoom:4);

{ Ringz.ar(Impulse.ar(6, 0, 0.3),  XLine.kr(100,3000,10), 0.5) }.scope(1, zoom:4);

// modulate ring time
{ Ringz.ar(Impulse.ar(6, 0, 0.3), 2000, XLine.kr(0.04, 4, 8)) }.scope(1, zoom:4);





{ OnePole.ar(WhiteNoise.ar(0.5), MouseX.kr(-0.99, 0.99)) }.scope(1);
{ OneZero.ar(WhiteNoise.ar(0.5), MouseX.kr(-0.49, 0.49)) }.scope(1);

{ Formlet.ar(Impulse.ar(MouseX.kr(2,300,1), 0, 0.4), 800, 0.01, 0.1) }.scope(1, zoom:4);


{ Klank.ar(`[[200, 671, 1153, 1723], nil, [1, 1, 1, 1]], Impulse.ar(2, 0, 0.1)) }.play;

{ Klank.ar(`[[200, 671, 1153, 1723], nil, [1, 1, 1, 1]], Dust.ar(8, 0.1)) }.play;

{ Klank.ar(`[[200, 671, 1153, 1723], nil, [1, 1, 1, 1]], PinkNoise.ar(0.007)) }.play;

{ Klank.ar(`[ {exprand(200, 4000)}.dup(12), nil, nil ], PinkNoise.ar(0.007)) }.scope(1);

{ Klank.ar(`[ (1..13)*200, 1/(1..13), nil ], PinkNoise.ar(0.01)) }.scope(1);

{ Klank.ar(`[ (1,3..13)*200, 1/(1,3..13), nil ], PinkNoise.ar(0.01)) }.scope(1);



{ SinOsc.ar(300, 0, 0.2) }.scope(1);
{ SinOsc.ar(300, 0, 0.2).abs }.scope(1);
{ SinOsc.ar(300, 0, 0.2).max(0) }.scope(1);
{ SinOsc.ar(300, 0).squared * 0.2 }.scope(1);
{ SinOsc.ar(300, 0).cubed * 0.2 }.scope(1);

{ SinOsc.ar(300, 0, MouseX.kr(0.1,80,1)).distort * 0.2 }.scope(1);
{ SinOsc.ar(300, 0, MouseX.kr(0.1,80,1)).softclip * 0.2 }.scope(1);
{ SinOsc.ar(300, 0, MouseX.kr(0.1,80,1)).clip2(1) * 0.2 }.scope(1);
{ SinOsc.ar(300, 0, MouseX.kr(0.1,80,1)).fold2(1) * 0.2 }.scope(1);
{ SinOsc.ar(300, 0, MouseX.kr(0.1,80,1)).wrap2(1) * 0.2 }.scope(1);
{ SinOsc.ar(300, 0, MouseX.kr(0.1,80,1)).wrap2(1) * 0.2 }.scope(1);

{ SinOsc.ar(200, 0, 0.2).scaleneg(MouseX.kr(-1,1)) }.scope(1);



(
s.quit;
s.options.numOutputBusChannels = 8;
s.options.numInputBusChannels = 8;
s.boot;
)

{ Pan2.ar(BrownNoise.ar, MouseX.kr(-1,1), 0.3) }.scope(2);
{ Pan2.ar(BrownNoise.ar, SinOsc.kr(0.2), 0.3) }.scope(2);

{ LinPan2.ar(BrownNoise.ar, MouseX.kr(-1,1), 0.3) }.scope(2);
{ LinPan2.ar(BrownNoise.ar, SinOsc.kr(0.2), 0.3) }.scope(2);

{ Balance2.ar(BrownNoise.ar, BrownNoise.ar, MouseX.kr(-1,1), 0.3) }.scope(2);

{ Pan4.ar(BrownNoise.ar, MouseX.kr(-1,1), MouseY.kr(1,-1), 0.3) }.scope(4);

{ PanAz.ar(5, BrownNoise.ar, MouseX.kr(-1,1), 0.3, 2) }.scope(5);

// change width to 3
{ PanAz.ar(5, BrownNoise.ar, MouseX.kr(-1,1), 0.3, 3) }.scope(5);


(
{
    var w, x, y, p, lf, rf, rr, lr;

    p = BrownNoise.ar; // source

    // B-format encode
    #w, x, y = PanB2.ar(p, MouseX.kr(-1,1), 0.3);

    // B-format decode to quad. outputs in clockwise order
    #lf, rf, rr, lr = DecodeB2.ar(4, w, x, y);

    [lf, rf, lr, rr] // reorder to my speaker arrangement: Lf Rf Lr Rr
}.scope(4);
)

(
{
    // rotation of stereo sound via mouse
    var x, y;
    x = Mix.fill(4, { LFSaw.ar(200 + 2.0.rand2, 0, 0.1) }); // left in
    y = WhiteNoise.ar * LFPulse.kr(3,0,0.7,0.2); // right in
    #x, y = Rotate2.ar(x, y, MouseX.kr(0,2));
    [x,y]
}.scope(2);
)

(
{
    // play with the room size
    var x;
    x = Klank.ar(`[[200, 671, 1153, 1723], nil, [1, 1, 1, 1]], Dust.ar(2, 0.1));
    x = Pan2.ar(x, -0.2);
    x = [x[0], DelayC.ar(x[1], 0.01, 0.01)]; // de-correlate
    FreeVerb.ar(x, 0.75, 0.9, 0.4);
}.scope;
)

(
{
    // play with the room size
    var x;
    x = Klank.ar(`[[200, 671, 1153, 1723], nil, [1, 1, 1, 1]], Dust.ar(2, 0.1));
    GVerb.ar(x, 105, 5, 0.7, 0.8, 60, 0.1, 0.5, 0.4) + x;
}.scope;
)


(
// Dust randomly triggers Decay to create an exponential
// decay envelope for the WhiteNoise input source
{
z = Decay.ar(Dust.ar(1,0.5), 0.3, WhiteNoise.ar);
DelayN.ar(z, 0.1, 0.1, 1, z); // input is mixed with delay via the add input
}.scope(1, zoom: 4)
)

(
{
z = Decay.ar(Impulse.ar(2,0,0.4), 0.3, WhiteNoise.ar);
DelayL.ar(z, 0.3, MouseX.kr(0,0.3), 1, z); // input is mixed with delay via the add input
}.scope(1, zoom: 4)
)

// used as an echo.
{ CombN.ar(Decay.ar(Dust.ar(1,0.5), 0.2, WhiteNoise.ar), 0.2, 0.2, 3) }.scope(1, zoom:4);

// Comb used as a resonator. The resonant fundamental is equal to
// reciprocal of the delay time.
{ CombN.ar(WhiteNoise.ar(0.02), 0.01, XLine.kr(0.0001, 0.01, 20), 0.2) }.scope(1);

{ CombL.ar(WhiteNoise.ar(0.02), 0.01, XLine.kr(0.0001, 0.01, 20), 0.2) }.scope(1);

{ CombC.ar(WhiteNoise.ar(0.02), 0.01, XLine.kr(0.0001, 0.01, 20), 0.2) }.scope(1);

// with negative feedback:
{ CombN.ar(WhiteNoise.ar(0.02), 0.01, XLine.kr(0.0001, 0.01, 20), -0.2) }.scope(1);

{ CombL.ar(WhiteNoise.ar(0.02), 0.01, XLine.kr(0.0001, 0.01, 20), -0.2) }.scope(1);

{ CombC.ar(WhiteNoise.ar(0.02), 0.01, XLine.kr(0.0001, 0.01, 20), -0.2) }.scope(1);

{ CombC.ar(Decay.ar(Dust.ar(1,0.1), 0.2, WhiteNoise.ar), 1/100, 1/100, 3) }.play;
{ CombC.ar(Decay.ar(Dust.ar(1,0.1), 0.2, WhiteNoise.ar), 1/200, 1/200, 3) }.play;
{ CombC.ar(Decay.ar(Dust.ar(1,0.1), 0.2, WhiteNoise.ar), 1/300, 1/300, 3) }.play;
{ CombC.ar(Decay.ar(Dust.ar(1,0.1), 0.2, WhiteNoise.ar), 1/400, 1/400, 3) }.scope(1, zoom:4);

(
{
    var z;
    z = Decay.ar(Dust.ar(1,0.5), 0.1, WhiteNoise.ar);
    8.do { z = AllpassL.ar(z, 0.04, 0.04.rand, 2) };
    z
}.scope(1);
)





// read sound
b = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");
{ SinOsc.ar(800 + (700 * PlayBuf.ar(1,b, BufRateScale.kr(b),  loop:1)),0,0.3) }.scope(1);

// loop is true
{ PlayBuf.ar(1,b, BufRateScale.kr(b), loop:1) }.scope(1);

// trigger one shot on each pulse
(
{
    var trig;
    trig = Impulse.kr(2.0);
    PlayBuf.ar(1,b,BufRateScale.kr(b),trig,0,0);
}.scope(1);
)

// mouse control of trigger rate and startpos
(
{
    var trig;
    trig = Impulse.kr(MouseY.kr(0.5,200,1));
    PlayBuf.ar(1,b,BufRateScale.kr(b),trig,MouseX.kr(0,BufFrames.kr(b)),1)
}.scope(1);
)

// accelerating pitch
(
{
    var rate;
    rate = XLine.kr(0.1,100,60);
    PlayBuf.ar(1, b, rate, 1.0,0.0, 1.0)
}.scope(1);
)


// sine wave control of playback rate. negative rate plays backwards
(
{
    var rate;
    rate = FSinOsc.kr(XLine.kr(0.2,8,30), 0, 3, 0.6);
    PlayBuf.ar(1,b,BufRateScale.kr(b)*rate,1,0,1)
}.scope(1);
)

// zig zag around sound
(
{
    var rate;
    rate = LFNoise2.kr(XLine.kr(1,20,60), 2);
    PlayBuf.ar(1,b,BufRateScale.kr(b) * rate,1,0,1)
}.scope(1);
)

// free sound
b.free;




// Granular Synthesis

// read sound
b = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");

(
{
    var trate, dur;
    trate = MouseY.kr(2,200,1);
    dur = 4 / trate;
    TGrains.ar(2, Impulse.ar(trate), b, 1, MouseX.kr(0,BufDur.kr(b)), dur, 0, 0.1, 2);
}.scope(2, zoom: 4);
)

(
{
    var trate, dur, clk, pos, pan;
    trate = MouseY.kr(8,120,1);
    dur = 12 / trate;
    clk = Impulse.kr(trate);
    pos = MouseX.kr(0,BufDur.kr(b)) + TRand.kr(0, 0.01, clk);
    pan = WhiteNoise.kr(0.6);
    TGrains.ar(2, clk, b, 1, pos, dur, pan, 0.1);
}.scope(2, zoom: 4);
)

// 4 channels
(
{
    var trate, dur, clk, pos, pan;
    trate = MouseY.kr(8,120,1);
    dur = 12 / trate;
    clk = Impulse.kr(trate);
    pos = MouseX.kr(0,BufDur.kr(b)) + TRand.kr(0, 0.01, clk);
    pan = WhiteNoise.kr(0.6);
    TGrains.ar(4, clk, b, 1, pos, dur, pan, 0.1);
}.scope(4, zoom: 4);
)

(
{
    var trate, dur, clk, pos, pan;
    trate = MouseY.kr(8,120,1);
    dur = 4 / trate;
    clk = Dust.kr(trate);
    pos = MouseX.kr(0,BufDur.kr(b)) + TRand.kr(0, 0.01, clk);
    pan = WhiteNoise.kr(0.6);
    TGrains.ar(2, clk, b, 1, pos, dur, pan, 0.1);
}.scope(2, zoom: 4);
)



(
{
    var trate, dur, clk, pos, pan;
    trate = LinExp.kr(LFTri.kr(MouseY.kr(0.1,2,1)),-1,1,8,120);
    dur = 12 / trate;
    clk = Impulse.ar(trate);
    pos = MouseX.kr(0,BufDur.kr(b));
    pan = WhiteNoise.kr(0.6);
    TGrains.ar(2, clk, b, 1, pos, dur, pan, 0.1);
}.scope(2, zoom: 4);
)


(
{
    var trate, dur, clk, pos, pan;
    trate = 12;
    dur = MouseY.kr(0.2,24,1) / trate;
    clk = Impulse.kr(trate);
    pos = MouseX.kr(0,BufDur.kr(b)) + TRand.kr(0, 0.01, clk);
    pan = WhiteNoise.kr(0.6);
    TGrains.ar(2, clk, b, 1, pos, dur, pan, 0.1);
}.scope(2, zoom: 4);
)

(
{
    var trate, dur, clk, pos, pan;
    trate = 100;
    dur = 8 / trate;
    clk = Impulse.kr(trate);
    pos = Integrator.kr(BrownNoise.kr(0.001));
    pan = WhiteNoise.kr(0.6);
    TGrains.ar(2, clk, b, 1, pos, dur, pan, 0.1);
}.scope(2, zoom: 4);
)

(
{
    var trate, dur, clk, pos, pan;
    trate = MouseY.kr(1,400,1);
    dur = 8 / trate;
    clk = Impulse.kr(trate);
    pos = MouseX.kr(0,BufDur.kr(b));
    pan = WhiteNoise.kr(0.8);
    TGrains.ar(2, clk, b, 2 ** WhiteNoise.kr(2), pos, dur, pan, 0.1);
}.scope(2, zoom: 4);
)

(
{
    var trate, dur;
    trate = MouseY.kr(2,120,1);
    dur = 1.2 / trate;
    TGrains.ar(2, Impulse.ar(trate), b, (1.2 ** WhiteNoise.kr(3).round(1)), MouseX.kr(0,BufDur.kr(b)), dur, WhiteNoise.kr(0.6), 0.1);
}.scope(2, zoom: 4);
)

// free sound
b.free;



( // using default window
{
    var trigrate, winsize, trig;
    trigrate = MouseX.kr(2, 120);
    winsize = trigrate.reciprocal;
    trig = Impulse.ar(trigrate);
    GrainSin.ar(2, trig, winsize, TRand.ar(440.0, 880.0, trig), LFNoise1.kr(0.2),
        -1, 512, 0.2)
}.scope(2, zoom: 4);
)

b = Buffer.sendCollection(s, Env([0, 1, 0], [0.5, 0.5], [8, -8]).discretize, 1);

( // using user supplied window
{
    var trigrate, winsize, trig;
    trigrate = MouseX.kr(2, 120);
    winsize = trigrate.reciprocal;
    trig = Impulse.ar(trigrate);
    GrainSin.ar(2, trig, winsize, TRand.ar(440.0, 880.0, trig), LFNoise1.kr(0.2),
        b, 512, 0.2)
}.scope(2, zoom: 4);
)



//
// CONTROL
//

// Filters for Controls

{ WhiteNoise.ar * Decay.ar(Impulse.ar(1), 0.9, 0.2) }.scope(1, zoom:4);
{ WhiteNoise.ar * Decay.ar(Dust.ar(3), 0.9, 0.2) }.scope(1, zoom:4);
{ SinOsc.ar(Decay.ar(Dust.ar(4), 0.5, 1000, 400), 0, 0.2) }.scope(1, zoom:4);

{ WhiteNoise.ar * Decay2.ar(Impulse.ar(1), 0.2, 0.9, 0.2) }.scope(1, zoom:4);
{ WhiteNoise.ar * Decay2.ar(Dust.ar(3), 0.2, 0.9, 0.2) }.scope(1, zoom:4);

{ SinOsc.ar(Lag.ar(LFPulse.ar(2,0,0.5,800,400), MouseX.kr(0,0.5)), 0, 0.2) }.scope(1, zoom:4);

{ SinOsc.ar(Integrator.ar(Dust2.ar(8), 0.99999, 200, 800), 0, 0.2) }.scope(1)

// amplitude determined by amplitude of trigger
{ Trig.ar(Dust.ar(2), 0.2) * FSinOsc.ar(800, 0, 0.4) }.scope(1, zoom:4);
// amplitude always the same.
{ Trig1.ar(Dust.ar(2), 0.2) * FSinOsc.ar(800, 0, 0.4) }.scope(1, zoom:4)



(
{
    var trig;
    trig = Dust.ar(2);
    [(Trig1.ar(trig, 0.05) * FSinOsc.ar(600, 0, 0.2)),
    (Trig1.ar(TDelay.ar(trig, 0.1), 0.05) * FSinOsc.ar(800, 0, 0.2))]
}.scope(2, zoom:4);
)


// Latch
{ Blip.ar(Latch.ar(WhiteNoise.ar, Impulse.ar(9)) * 400 + 500, 4, 0.2) }.play;
{ Blip.ar(Latch.ar(SinOsc.ar(0.3), Impulse.ar(9)) * 400 + 500, 4, 0.2) }.play;

{ Blip.ar(Gate.ar(LFNoise2.ar(40), LFPulse.ar(1)) * 400 + 500, 4, 0.2) }.scope(1, zoom:4);


{ Impulse.ar(9) }.plot(1);
{ Latch.ar(SinOsc.ar(), Impulse.ar(9)) }.plot(1);
{ Latch.ar(WhiteNoise.ar, Impulse.ar(9)) }.scope;//plot(1);

// Latch holds the input signal when triggered. A trigger occurs when the 2nd parameter is changes from non-positive to positive.
{ Latch.ar(DC.ar(1.5), DC.ar(-0.1)) }.plot(1, minval:-2, maxval:2);
{ Latch.ar(DC.ar(1.5), DC.ar(0)) }.plot(1, minval:-2, maxval:2);
{ Latch.ar(DC.ar(1.5), DC.ar(0.1)) }.plot(1, minval:-2, maxval:2);
{ Latch.ar(DC.ar(1.5), Line.ar(-0.5, 0.5)) }.plot(1, minval:-2, maxval:2);

{ SinOsc.ar(1, -pi/2) }.plot(1);
{ Latch.ar(DC.ar(1.5), SinOsc.ar(1, -pi/2)) }.plot(1, minval:-2, maxval:2);

{ Line.ar(-0.5, 0.5) }.plot(1);
{ Latch.ar(DC.ar(1), Line.ar(-0.5, 0.5)) }.plot(1, minval:-2, maxval:2);
{ Latch.ar(DC.ar(0.5), Line.ar(-0.5, 0.5)) }.plot(1, minval:-1, maxval:1, );
{ Latch.ar(Line.ar(), SinOsc.ar()) }.plot(1);

//{ Latch.ar(Line.ar() SinOsc.ar()) }.plot(1);


// Gate
{ SinOsc.ar(440 * 0.5*(1 + Gate.ar(SinOsc.ar(1), 0.5))) }.scope;

// PulseCount
(
{
SinOsc.ar(
            PulseCount.ar(Impulse.ar(10), Impulse.ar(0.4)) * 200,
            0, 0.05
        )
}.scope(2, zoom:4);
)

// PulseDivider
(
{
    var p, a, b;
    p = Impulse.ar(8);
    a = SinOsc.ar(1200, 0, Decay2.ar(p, 0.005, 0.1));
    b = SinOsc.ar(600,  0, Decay2.ar(PulseDivider.ar(p, MouseX.kr(1,8).round(1)), 0.005, 0.5));

    [a, b] * 0.4
}.scope(2, zoom:4);
)

// EnvGen
{ EnvGen.kr(Env.perc, doneAction: Done.freeSelf) * SinOsc.ar(880,0,0.2) }.play;
{ EnvGen.kr(Env.perc(1,0.005,1,4), doneAction: Done.freeSelf) * SinOsc.ar(880,0,0.2) }.play;

{ EnvGen.kr(Env.perc, Impulse.kr(2)) * SinOsc.ar(880,0,0.2) }.play;
{ EnvGen.kr(Env.perc, Dust.kr(3)) * SinOsc.ar(880,0,0.2) }.play;

// for sustain envelopes a gate is required
(
z = { arg gate=1;
	var env, snd;
	env = EnvGen.kr(Env.adsr, gate, doneAction: Done.freeSelf);
	snd = SinOsc.ar(880,0,0.2);
}.play;
)
z.release;

(
// randomly generated envelope
z = { arg gate=1;
    var env, n=32;
    env = Env(
                [0]++{1.0.rand.squared}.dup(n-1) ++ [0],
                {rrand(0.005,0.2)}.dup(n),
                \lin, n-8, 8 );
    EnvGen.kr(env, gate, doneAction: Done.freeSelf) * LFTri.ar(220,0,0.4)
}.scope(1, zoom:4);
)
z.release;


//
// Spectral
//

// alloc a buffer for the FFT
b = Buffer.alloc(s,2048,1);
// read a sound
c = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");


(
// do nothing
{
    var in, chain;
    in = PlayBuf.ar(1,c, BufRateScale.kr(c), loop:1);
    chain = FFT(b, in);
    0.5 * IFFT(chain);
}.scope(1);
)

(
// pass only magnitudes above a threshold
{
    var in, chain;
    in = PlayBuf.ar(1,c, BufRateScale.kr(c), loop:1);
    chain = FFT(b, in);
    chain = PV_MagAbove(chain, MouseX.kr(0.1,512,1));
    0.5 * IFFT(chain);
}.scope(1);
)

(
// pass only magnitudes below a threshold
{
    var in, chain;
    in = PlayBuf.ar(1,c, BufRateScale.kr(c), loop:1);
    chain = FFT(b, in);
    chain = PV_MagBelow(chain, MouseX.kr(0.1,512,1));
    0.5 * IFFT(chain);
}.scope(1);
)

(
// brick wall filter.
{
    var in, chain;
    in = PlayBuf.ar(1,c, BufRateScale.kr(c), loop:1);
    chain = FFT(b, in);
    chain = PV_BrickWall(chain, MouseX.kr(-1,1));
    0.5 * IFFT(chain);
}.scope(1);
)

(
// pass random frequencies. Mouse controls how many to pass.
// trigger changes the frequencies periodically
{
    var in, chain;
    in = PlayBuf.ar(1,c, BufRateScale.kr(c), loop:1);
    chain = FFT(b, in);
    chain = PV_RandComb(chain, MouseX.kr(0,1), Impulse.kr(0.4));
    0.5 * IFFT(chain);
}.scope(1);
)

(
// rectangular comb filter
{
    var in, chain;
    in = PlayBuf.ar(1,c, BufRateScale.kr(c), loop:1);
    chain = FFT(b, in);
    chain = PV_RectComb(chain, 8, MouseY.kr(0,1), MouseX.kr(0,1));
    0.5 * IFFT(chain);
}.scope(1);
)

(
// freeze magnitudes
{
    var in, chain;
    in = PlayBuf.ar(1,c, BufRateScale.kr(c), loop:1);
    chain = FFT(b, in);
    chain = PV_MagFreeze(chain, LFPulse.kr(1, 0.75));
    0.5 * IFFT(chain);
}.scope(1);
)


//
// Techniques
//

// Artificial Space
// Building a sense of space into a sound by setting up phase differences between the speakers.

{ var x; x = BrownNoise.ar(0.2); [x,x] }.scope(2); // correlated
{ {BrownNoise.ar(0.2)}.dup }.scope(2); // not correlated


// correlated
{ var x; x = LPF.ar(BrownNoise.ar(0.2), MouseX.kr(100,10000)); [x,x] }.scope(2);
// not correlated
{ LPF.ar({BrownNoise.ar(0.2)}.dup, MouseX.kr(100,10000)) }.scope(2);

// correlated
(
{ var x;
    x = Klank.ar(`[[200, 671, 1153, 1723], nil, [1, 1, 1, 1]], PinkNoise.ar(7e-3));
    [x,x]
}.scope(2))
// not correlated
{ Klank.ar(`[[200, 671, 1153, 1723], nil, [1, 1, 1, 1]], PinkNoise.ar([7e-3,7e-3])) }.scope(2);

// two waves mixed together coming out both speakers
{ var x; x = Mix.ar(VarSaw.ar([100,101], 0, 0.1, 0.2)); [x,x] }.scope(2);
// two waves coming out each speaker independently
{ VarSaw.ar([100,101], 0, 0.1, 0.2 * 1.414) }.scope(2); // * 1.414 to compensate for power

// delays as cues to direction
// mono
{ var x; x = LFTri.ar(1000,0,Decay2.ar(Impulse.ar(4,0,0.2),0.004,0.2)); [x,x]}.scope(2);

(
// inter-speaker delays
{ var x; x = LFTri.ar(1000,0,Decay2.ar(Impulse.ar(4,0,0.2),0.004,0.2));
    [DelayC.ar(x,0.01,0.01),DelayC.ar(x,0.02,MouseX.kr(0.02, 0))]
}.scope(2);
)

(
// mixing two delays together
// you hear a phasing sound but the sound is still flat.
{ var x; x = BrownNoise.ar(0.2);
    x = Mix.ar([DelayC.ar(x,0.01,0.01),DelayC.ar(x,0.02,MouseX.kr(0,0.02))]);
    [x,x]
}.scope(2);
)

(
// more spatial sounding. phasing causes you to perceive directionality
{ var x; x = BrownNoise.ar(0.2);
    [DelayC.ar(x,0.01,0.01),DelayC.ar(x,0.02,MouseX.kr(0.02, 0))]
}.scope(2);
)



//
// Parallel Structures
//

(
{
    // mixing sine oscillators in parallel
    var n = 16; // number of structures to make
    // mix together  parallel structures
    Mix.fill(n,
            // this function creates an oscillator at a random frequency
		{ FSinOsc.ar(200 + 1000.0.rand) }
    ) / (2*n)            // scale amplitude
}.scope;
)

(
{
    // mixing sine oscillators in parallel
    var n = 16; // number of structures to make
    // mix together  parallel structures
    Mix.fill(n,
            // this function creates an oscillator at a random frequency
            { FSinOsc.ar(200 + 1000.0.rand + [0, 0.5]) }
    ) / (2*n)            // scale amplitude
}.scope(2);
)

(
{
    // mixing sine oscillators in parallel
    var n = 16; // number of structures to make
    // mix together  parallel structures
    Mix.fill(n,
            {
                var amp;
                amp = FSinOsc.kr(exprand(0.1,1),2pi.rand).max(0);
                Pan2.ar(
                    FSinOsc.ar(exprand(100,1000.0), 0, amp),
                    1.0.rand2)
            }
    ) / (2*n)            // scale amplitude
}.scope(2);
)


(
{
    var n;
    n = 8; // number of 'voices'
    Mix.ar( // mix all stereo pairs down.
        Pan2.ar( // pan the voice to a stereo position
            CombL.ar( // a comb filter used as a string resonator
                Dust.ar( // random impulses as an excitation function
                    // an array to cause expansion of Dust to n channels
                    // 1 means one impulse per second on average
                    1.dup(n),
                    0.3 // amplitude
                ),
                0.01, // max delay time in seconds
                // array of different random lengths for each 'string'
                {0.004.rand+0.0003}.dup(n),
                4 // decay time in seconds
            ),
            {1.0.rand2}.dup(n) // give each voice a different pan position
        )
    )
}.scope(2, zoom:4);
)

//
// Serial Structures
//

(
{
    var sig, chain;

    // The original sound source
    sig = sum({ SinOsc.ar(rrand(50,6000),0,2*Decay.ar(Dust2.ar(1),0.1)).tanh } ! 7);

    chain = sig;    // Start with the original signal
    8.do {|i|       // Loop 8 times. For each loop, connect the signal through something.

        // A simple reverb
        chain = LeakDC.ar(AllpassL.ar(LPF.ar(chain*0.9,3000), 0.2, {0.19.rand+0.01}!2, 3));
    };

    Limiter.ar(sig+chain);    // dry + wet
}.play;
)
