// Rumush
// Facebook: https://www.facebook.com/rumushproduction
// SoundCloud: https://soundcloud.com/rumushproduction
// YouTube: https://www.youtube.com/channel/UCs_Cn1R4iFrYOyc8liFucSQ
// Blog: https://mycelialcordsblog.wordpress.com/
// GitHub: https://github.com/RumushMycelialCords
/*
BPF Experiment
*/
(
{// Noise Filter Experiment - Random
	var src=PinkNoise.ar();
	16.do{src=BRF.ar(src,rrand(100,15000)!2)};
	src
}.play
)
(
{// Noise Filter Experiment - Random with LFNoise1
 	var src=PinkNoise.ar();
	var lfo = {arg rt; LFNoise1.ar(rt!2)};
	16.do{src=BRF.ar(src,rrand(100,15000)*(0.75+(lfo.(8.0.rand)*0.25)),lfo.(8.0.rand).range(0.05,1))};
	src
}.play
)
(
{// Noise Filter Experiment - Random with LFPulse
 	var src=PinkNoise.ar();
	var lfo = {arg rt; LFPulse.ar(rt!2)};
	16.do{src=BRF.ar(src,rrand(100,15000)*(0.75+(lfo.(0.5.rand)*0.25)),lfo.(0.5.rand).range(0.05,1))};
	src
}.play
)
(
{// Noise BPF - Creative Use
	var src=PinkNoise.ar();
	var sawRt = 0.5;
	var lfo = {arg rt, rt2; LFPulse.ar(
		[rt+SinOsc.ar(rt2.rand,0,rrand(rt/2,rt*2)),rt+SinOsc.ar(rt2.rand,0,rrand(rt/2,rt*2))
	])};
	8.do{src=BRF.ar(
		src,rrand(250,15000)*(0.5+(lfo.(lfo.(1,2)*LFSaw.ar(sawRt).abs*5000,0.25)*0.5)),
		LFSaw.ar(sawRt*rrand(0.5,0.75)).abs*0.5+0.25)
	};
	CombC.ar(
		(src*LFSaw.ar(sawRt*rrand(0.25,0.5),0,8).abs).tanh,
		0.3,0.25+LFSaw.ar([sawRt*0.95,sawRt*0.93],0,0.05).abs,
		4,0.5
	).tanh
}.play
)
(
{// Noise BPF - Creative Use
	var freq = 100;
	var rt = 0.5;
	var src = PinkNoise.ar(1!2);
	4.do{src=BRF.ar(src,rrand(100,500).round(10)+(freq*LFNoise1.ar(rt.rand!2)),0.1)};
	4.do{src=BRF.ar(src,rrand(1000,2500).round(10)+(freq*LFNoise1.ar(rt.rand!2)),0.2)};
	4.do{src=BRF.ar(src,rrand(250,750).round(10)+(freq*LFNoise1.ar(rt.rand!2)),0.1)};
	4.do{src=BRF.ar(src,rrand(2500,5000).round(10)+(freq*LFNoise1.ar(rt.rand!2)),0.2)};
	src=FreqShift.ar(src,Duty.ar(8,0,Dwhite(-1*freq*10,freq*10).round(freq/2)),0,0.25)+src;
	src=CombC.ar(src,1,1,8);
	Limiter.ar(LPF.ar(src,2500+LFNoise1.ar(rt.rand!2,1250)))
}.play
)