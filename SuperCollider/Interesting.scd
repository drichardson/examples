(
{
	(({RHPF.ar(OnePole.ar(BrownNoise.ar, 0.99), LPF.ar(BrownNoise.ar, 14)
* 400 + 500, 0.03, 0.003)}!2) + ({RHPF.ar(OnePole.ar(BrownNoise.ar, 0.99), LPF.ar(BrownNoise.ar, 20)
		* 800 + 1000, 0.03, 0.005)}!2)) * 10;
}.play
)

Env.perc.test;
Env.circle.test;


//
// Paulstretch
//

(
SynthDef(\paulstretchMono, { |out = 0, bufnum, envBufnum, pan = 0, stretch = 50, window = 0.25, amp = 1|
	// Paulstretch for SuperCollider
	// Based on the Paul's Extreme Sound Stretch algorithm by Nasca Octavian PAUL
	// https://github.com/paulnasca/paulstretch_python/blob/master/paulstretch_steps.png
	//
	// By Jean-Philippe Drecourt
	// http://drecourt.com
	// April 2020
	//
	// Arguments:
	// out: output bus (stereo output)
	// bufnum: the sound buffer. Must be Mono. (Use 2 instances with Buffer.readChannel for stereo)
	// envBufnum: The grain envelope buffer created as follows:
	//// envBuf = Buffer.alloc(s, s.sampleRate, 1);
	//// envSignal = Signal.newClear(s.sampleRate).waveFill({|x| (1 - x.pow(2)).pow(1.25)}, -1.0, 1.0);
	//// envBuf.loadCollection(envSignal);
	// pan: Equal power panning, useful for stereo use.
	// stretch: stretch factor (modulatable)
	// window: the suggested grain size, will be resized to closest fft window size
	// amp: amplification
	var trigPeriod, sig, chain, trig, pos, fftSize;
	// Calculating fft buffer size according to suggested window size
	fftSize = 2**floor(log2(window*SampleRate.ir));
	// Grain parameters
	// The grain is the exact length of the FFT window
	trigPeriod = fftSize/SampleRate.ir;
	trig = Impulse.ar(1/trigPeriod);
	pos = Demand.ar(trig, 0, demandUGens: Dseries(0, trigPeriod/stretch));
	// Extraction of 2 consecutive grains
	// Both grains need to be treated together for superposition afterwards
	sig = [GrainBuf.ar(1, trig, trigPeriod, bufnum, 1, pos, envbufnum: envBufnum),
		GrainBuf.ar(1, trig, trigPeriod, bufnum, 1, pos + (trigPeriod/(2*stretch)), envbufnum: envBufnum)]*amp;
	// FFT magic
	sig = sig.collect({ |item, i|
		chain = FFT(LocalBuf(fftSize), item, hop: 1.0, wintype: -1);
		// PV_Diffuser is only active if its trigger is 1
		// And it needs to be reset for each grain to get the smooth envelope
		chain = PV_Diffuser(chain, 1 - trig);
		item = IFFT(chain, wintype: -1);
	});
	// Reapply the grain envelope because the FFT phase randomization removes it
	sig = sig*PlayBuf.ar(1, envBufnum, 1/(trigPeriod), loop:1);
	// Delay second grain by half a grain length for superposition
	sig[1] = DelayC.ar(sig[1], trigPeriod/2, trigPeriod/2);
	// Panned output
	Out.ar(out, Pan2.ar(Mix.new(sig), pan));
}).add;
)

// Example
({
	var envBuf, envSignal, buffer;
	buffer = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");
	// The grain envelope
	envBuf = Buffer.alloc(s, s.sampleRate, 1);
	envSignal = Signal.newClear(s.sampleRate).waveFill({|x| (1 - x.pow(2)).pow(1.25)}, -1.0, 1.0);
	envBuf.loadCollection(envSignal);
	s.sync();
	// Runs indefinitely
	Synth(\paulstretchMono, [\bufnum, buffer.bufnum, \envBufnum, envBuf.bufnum]);
}.fork;
)


(
{
	var humSource, noise, comb;

	humSource = Clip.ar(LFSaw.ar([99.8, 100.2], 1, 0.5, 0.5).sum - 1, -0.5, 0.5);

	noise = LPF.ar(LPF.ar(WhiteNoise.ar,2),2);
	noise = noise * noise * 500;

	humSource = humSource * noise;

	// fig 39.2: Comb unit
	comb = DelayC.ar(InFeedback.ar(10), delaytime: (noise+20)/1000);
	OffsetOut.ar(10, (humSource + OnePole.ar(comb * 0.2, exp(-2pi * (3000 * SampleDur.ir)))));

	comb ! 2

}.play;
)

(
{
var noise, phasor, chirpPulse, randGate, chirpAmp, clip, snap, trig, formant, comb;

phasor = LFSaw.ar([-99.8, 100.2], 1, 0.5, 0.5);

noise = WhiteNoise.ar!2;
noise[0] = OnePole.ar(noise[0], exp(-2pi * (0.1 * SampleDur.ir)));
noise[0] = OnePole.ar(noise[0], exp(-2pi * (0.1 * SampleDur.ir)));
noise[0] = noise[0].max(0) * 700 + 3;


// fig 39.4: Chirp pulse
chirpPulse = phasor[0].scope * noise[0];
chirpPulse = chirpPulse.min(1) - (chirpPulse.max(1) - 1 * 1e+09).min(1);
chirpPulse.scope;
chirpPulse = ((chirpPulse + 0.1)**2 * 12 * 2pi).cos * chirpPulse;
chirpPulse.scope;
chirpPulse = (chirpPulse - OnePole.ar(chirpPulse, exp(-2pi * (300 * SampleDur.ir))));


// fig 39.5: Random Gate
randGate = WhiteNoise.ar;
randGate = OnePole.ar(randGate, exp(-2pi * (3 * SampleDur.ir)));
randGate = OnePole.ar(randGate, exp(-2pi * (3 * SampleDur.ir)));
randGate = Clip.ar(noise.max(2) - 0.0008 * 1e+09, 0, 1);
randGate = OnePole.ar(randGate, exp(-2pi * (30 * SampleDur.ir)));
randGate = chirpPulse * randGate;


noise[1] = LPF.ar(LPF.ar(WhiteNoise.ar,2),2);
noise[1] = noise[1] * noise[1] * 500;


clip = Clip.ar((phasor.sum - 1) * noise[1], -0.9, 0.1);


chirpAmp = OnePole.ar(clip, exp(-2pi * (15 * SampleDur.ir)));
chirpAmp = OnePole.ar(chirpAmp, exp(-2pi * (15 * SampleDur.ir)));
chirpAmp = Clip.ar((chirpAmp * 500).max(0.1) - 0.1 * 1e+09, 0, 1);
chirpAmp = OnePole.ar(chirpAmp, exp(-2pi * (30 * SampleDur.ir)));


chirpPulse = chirpPulse * chirpAmp * 0.6;
trig = (Amplitude.kr(clip)>0.03);


// fig 39.7: Spark snap
snap = 0!2;
snap[0] = EnvGen.ar(Env.new([0,1,0], [0, 0.5]), trig);
snap[0] = snap[0] * snap[0] * snap[0] * WhiteNoise.ar * 0.5;
snap[1] = EnvGen.ar(Env.new([0,1,0], [0, 10/1000]), trig);
snap[1] = SinOsc.ar(snap[1] * 7000 + 20);
snap = snap.sum * 0.05;


// fig 39.6: Spark formant
formant = BPF.ar(snap, 1.8 * [4600, 7200, 480, 720], [5,5,7,8].reciprocal);
formant = formant[0..1].sum * 2 + formant[2] * 1.2 + formant[3] * 2.5;
formant = BPF.ar(formant, 2500, 0.5.reciprocal);


// fig 39.2 Comb unit
comb = DelayC.ar(InFeedback.ar(10), delaytime: (noise[1] + 20)/1000);
OffsetOut.ar(10, (chirpPulse + clip + snap + OnePole.ar(comb * 0.2, exp(-2pi * (3000 * SampleDur.ir)))));

comb!2;

}.play;
)


(
SynthDef(\electricity, {
	var noise, phasor, chirpPulse, randGate, chirpAmp, clip, snap, trig, formant, comb;

	phasor = LFSaw.ar([-99.8, 100.2], 1, 0.5, 0.5);

	noise = WhiteNoise.ar!2;
	noise[0] = OnePole.ar(noise[0], exp(-2pi * (0.1 * SampleDur.ir)));
	noise[0] = OnePole.ar(noise[0], exp(-2pi * (0.1 * SampleDur.ir)));
	noise[0] = noise[0].max(0) * 700 + 3;


	// fig 39.4: Chirp pulse
	chirpPulse = phasor[0] * noise[0];
	chirpPulse = chirpPulse.min(1) - (chirpPulse.max(1) - 1 * 1e+09).min(1);
	chirpPulse = ((chirpPulse + 0.1)**2 * 12 * 2pi).cos * chirpPulse;
	chirpPulse = (chirpPulse - OnePole.ar(chirpPulse, exp(-2pi * (300 * SampleDur.ir))));


	// fig 39.5: Random Gate
	randGate = WhiteNoise.ar;
	randGate = OnePole.ar(randGate, exp(-2pi * (3 * SampleDur.ir)));
	randGate = OnePole.ar(randGate, exp(-2pi * (3 * SampleDur.ir)));
	randGate = Clip.ar(noise.max(2) - 0.0008 * 1e+09, 0, 1);
	randGate = OnePole.ar(randGate, exp(-2pi * (30 * SampleDur.ir)));
	randGate = chirpPulse * randGate;


	noise[1] = LPF.ar(LPF.ar(WhiteNoise.ar,2),2);
	noise[1] = noise[1] * noise[1] * 500;


	clip = Clip.ar((phasor.sum - 1) * noise[1], -0.9, 0.1);


	chirpAmp = OnePole.ar(clip, exp(-2pi * (15 * SampleDur.ir)));
	chirpAmp = OnePole.ar(chirpAmp, exp(-2pi * (15 * SampleDur.ir)));
	chirpAmp = Clip.ar((chirpAmp * 500).max(0.1) - 0.1 * 1e+09, 0, 1);
	chirpAmp = OnePole.ar(chirpAmp, exp(-2pi * (30 * SampleDur.ir)));


	chirpPulse = chirpPulse * chirpAmp * 0.6;
	trig = (Amplitude.kr(clip)>0.03);


	// fig 39.7: Spark snap
	snap = 0!2;
	snap[0] = EnvGen.ar(Env.new([0,1,0], [0, 0.5]), trig);
	snap[0] = snap[0] * snap[0] * snap[0] * WhiteNoise.ar * 0.5;
	snap[1] = EnvGen.ar(Env.new([0,1,0], [0, 10/1000]), trig);
	snap[1] = SinOsc.ar(snap[1] * 7000 + 20);
	snap = snap.sum * 0.05;


	// fig 39.6: Spark formant
	formant = BPF.ar(snap, 1.8 * [4600, 7200, 480, 720], [5,5,7,8].reciprocal);
	formant = formant[0..1].sum * 2 + formant[2] * 1.2 + formant[3] * 2.5;
	formant = BPF.ar(formant, 2500, 0.5.reciprocal);


	// fig 39.2 Comb unit
	comb = DelayC.ar(InFeedback.ar(10), delaytime: (noise[1] + 20)/1000);
	OffsetOut.ar(10, (chirpPulse + clip + snap + OnePole.ar(comb * 0.2, exp(-2pi * (3000 * SampleDur.ir)))));

	Out.ar(0, comb!2);
}).add;
)

Synth(\electricity);





(

{
    var input, kernel;

    input = SoundIn.ar(0);
    kernel = Mix.ar(LFSaw.ar([300,500,800,1000] * MouseX.kr(1.0, 2.0), 0, 1.0));

    // must have power of two framesize
    Convolution.ar(input,kernel, 1024, 0.5)
}.play;

)

(
// must have power of two framesize- FFT size will be sorted by Convolution to be double this
// maximum is currently a=8192 for FFT of size 16384

a = 2048;
s = Server.local;

// kernel buffer
g = Buffer.alloc(s, a, 1);
)

(

// random impulse response

g.set(0, 1.0);

100.do({ arg i; g.set(a.rand, 1.0.rand) });


{
    var input, kernel;

    input = SoundIn.ar(0);
    kernel = PlayBuf.ar(1, g.bufnum,BufRateScale.kr(g.bufnum), 1, 0, 1);

    Convolution.ar(input, kernel, 2 * a, 0.5)
}.play;

)