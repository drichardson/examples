(
f = { PinkNoise.ar(0.2) + SinOsc.ar(440, 0, 0.2) + Saw.ar(660, 0.2) };
f.plot; // default plot is 0.01 seconds
f.plot(1); // plot 1 second
f.scope; // oscilloscope like display and plays the sound as well
)

// Also works for multiple channels
{ [SinOsc.ar(440, 0, 0.2), SinOsc.ar(442, 0, 0.2)]}.scope;

// You can also scope the output of the server by calling 'scope' on it.

{ [SinOsc.ar(440, 0, 0.2)]}.play;
// { [SinOsc.ar(440, 0, 0.2)]}.play;
{ [SinOsc.ar(440, 0, 0.2), SinOsc.ar(442, 0, 0.2)]}.play;
s.scope;

// Poll method - sample a few values per second and print to post window
{SinOsc.kr(0.1).poll}.play;


