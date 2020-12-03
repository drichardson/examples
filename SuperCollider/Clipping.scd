// Default parameters do not clip unless your computer's audio levels are too high.
{ SinOsc.ar() }.scope;

// Same thing with explicit parameters but using default values. Does not clip.
{ SinOsc.ar(440, 0, 1) }.scope;

// This will clip, regardless of computer audio levels.
{ SinOsc.ar(440, 0, 2) }.scope;

// Does not clip unless computer audio levels too high.
{ SinOsc.ar(440, 0, 0.6) }.scope;

// This probably won't clip, unless your computer's audio levels are really high.
{ SinOsc.ar(440, 0, 0.2) }.scope;