https://sccode.org/1-4TT

/*
By Schemawound - http://www.schemawound.com/
All code by Jonathan Siemasko

Created for The Haiku Project by Subterranean Tide - http://www.subterraneantide.com/hk07-schemawound.html
Description: Each artist has contributed a three track release in increments of five minutes, seven minutes, and five minutes accompanied by a Haiku poem.  Each song represents the line of the poem including title and reflection.
Source Haiku:

Autumn moonlight—

a worm digs silently

into the chestnut.

- Basho Matsuo
*/

s.options.memSize = 8192 * 32;
s.reboot;


//Autumn moonlight-
(
{
	var time = 60 * 5;
	var line = Line.ar(0, 1, time);
	var oscEnv = EnvGen.ar(Env.linen(sustainTime: time - 3, releaseTime: 2));
	var sin = SinOsc.ar(LFTri.kr(0.1).range(5000, 6000));
	var pulse = Pulse.ar(LFTri.kr([0.9, 0.92], 0, 100, 40), LFTri.kr(1, 0, 0.5, 0.5));
	var osc = pulse * sin * oscEnv;
	var compressLFO = SinOsc.kr(0.1).abs * SinOsc.kr(0.4).range(2, 20);
	var compress = Compander.ar(osc, osc, 0.7, compressLFO);
	var delay1 = compress + CombC.ar(compress, 10, LFTri.kr(0.3).range(2, 4), 10) * 0.2;
	var osc2 = delay1 * SinOsc.ar(LFTri.kr(0.5).range(300, 500));
	var delay2 = osc2 + CombC.ar(osc2, 10, LFTri.kr(0.2).range(2, 4), 10) * 0.2;
	var lpf = delay2 + (RLPF.ar(DelayC.ar(delay2, 20, 5, 0.5), 400, 0.5) * SinOsc.ar(LFTri.ar(0.0123).range(200, 300)));
	var verb1 = lpf + (GVerb.ar(lpf, LFTri.ar(0.01).range(1, 10)) * LFTri.ar(0.003));
	var verb2 = verb1 + (GVerb.ar(verb1) * 0.3);
	var compress2 = Compander.ar(verb2, verb2, 0.5, 1, 1/30);
	var hpf = HPF.ar(compress2, 30) * 0.1 * EnvGen.ar(Env.linen(0.01, time - 10, 3));
	var comb = (hpf * (1 - line)) + CombC.ar(hpf, 10, Line.ar(1, 0.1, time), 10, line);
	comb * 3;
}.play;
)

//A worm digs silently
(
{
	var time = 60*7;
	var hex = {|f| 1 - LFTri.ar(f)};
	var line = {|s,e| Line.kr(s,e,time,1,0)};
	var hexes = hex.(line.(17,1647)) * hex.(line.(217,17)) * hex.(67) * hex.([55,55.1]) * 0.05;
	var verb1 = hexes * SinOsc.ar(300) * SinOsc.ar(0.01);
	var verb2 = hexes * SinOsc.ar(SinOsc.ar(0.0334).range(100, 157.7)) * SinOsc.ar(0.008);
	var verb3 = hexes * SinOsc.ar(SinOsc.ar(0.0234).range(200, 257.7)) * SinOsc.ar(0.0043);
	var verb4 = (hexes * 20 * SinOsc.ar(3257.7)).clip(-1,1) * SinOsc.ar(1257.7) * SinOsc.ar(0.023) / 30;
	var combine = verb1 + verb2 + verb3 + CombC.ar(verb4 * SinOsc.ar(Line.ar(300, 10000, 60*7)), 10, 1, 10, 1, verb4);
	combine = combine * EnvGen.ar(Env.linen(0.01, time - 15, 13));
	combine = combine + DelayC.ar(GVerb.ar(combine, [1.7, 1.8], 10, drylevel: 0), 10, Line.kr(0.01, 1, time), 0.5) * 0.1;
	combine = combine + LPF.ar(CombC.ar(combine, 20, Line.kr(1, 0.01, time), 20), LFTri.ar([0.0312, 0.0222]).range(50, 10000)) * 0.1;
	combine = Compander.ar(combine, combine, 0.5, 1, 1/20) * 8;
	combine = Compander.ar(combine, combine, 0.5, 1, 1/20) / 2.5;
	HPF.ar(combine, 55);
}.play
)

//Into the chestnut.
(
{
	x = RLPF.ar(SinOsc.ar(SinOsc.ar([0.020, 0.019]).range(3, 10)) * SyncSaw.ar(SinOsc.ar(0.01).range(1, 100), SinOsc.ar(0.017).range(300, 1000)), Line.ar(60, 10000, 60*5));
	x = GVerb.ar(x + CombL.ar(x, 10, 0.2, 10));
	EnvGen.ar(Env.linen(0.01, 60*5, 20), doneAction:2) * x * 0.03!2
}.play
)