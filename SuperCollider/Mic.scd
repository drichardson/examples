// Warning: use headphones to avoid feedback
{SoundIn.ar(0)}.play; // same as In.ar(8): takes sound from the first input bus

// Stereo version
{SoundIn.ar([0, 1])}.play; // first and second inputs

// Some reverb just for fun?
{FreeVerb.ar(SoundIn.ar([0, 1]), mix: 0.5, room: 0.9)}.play;