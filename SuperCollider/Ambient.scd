// https://sccode.org/1-50j
// Rumush
// https://mycelialcordsblog.wordpress.com/
// https://fungorumush.bandcamp.com/releases
// https://soundcloud.com/fungorum

(
f = {
	var rep = [4, 8, 16, 32];
	var n = rep.choose;
	var x = [

	      54, 60, 66, 72, 81, 90, 96, 102,
	      108, 128, 132, 144, 162, 180, 192, 204,
	      216, 240, 264, 288, 324, 360, 384, 408,
	      432, 480, 528, 576, 648, 720, 768, 816,
	      864, 960, 1056, 1152, 1296, 1440, 1536, 1632,
	      1728, 1920, 2112, 2304, 2592, 2880, 3072, 3264,
	      3456, 3840, 4224, 4608, 5184, 5760, 6144, 6528,
	      6912, 7680, 8448, 9216, 10368, 11520, 12288, 528

       ];
	Mix.fill(n,
		{
			var detune = 5.rand;
			var sin = SinOsc.ar(x.choose, 4.rand, 0.25);
			var saw = RLPF.ar(Saw.ar(x.choose*0.01+detune, 0.75), x.choose, 2.rand).tanh;
			Pan2.ar((sin+saw) * EnvGen.kr(Env.sine(12, 1/n), 1, doneAction:2),
			1.rand2) }
	);
};
)

(
// DEFINE A ROUTINE
r = Routine({

    inf.do({
		f.play;
        10.wait;
    })
});
)

r.reset.play;

// https://sccode.org/1-j
n=LFNoise1;Ndef(\x,{a=SinOsc.ar(65,Ndef(\x).ar*n.ar(0.1,3),n.ar(3,6)).tanh;9.do{a=AllpassL.ar(a,0.3,{0.2.rand+0.1}!2,5)};a.tanh}).play

// https://sccode.org/1-9
play{a=HPF.ar(ar(PinkNoise,5e-3),10)*Line.kr(0,1,9);ar(GVerb,({|i|ar(Ringz,a*LFNoise1.kr(0.05+0.1.rand),55*i+60,0.2)}!99).sum,70,99).tanh}

