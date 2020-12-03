(
{ SinOsc.ar(880*Decay.ar(Impulse.ar(2), 1/2), 0, 1/2); }.scope;
{ SinOsc.ar(440*Decay.ar(Impulse.ar(4), 1/4), 0, 1/4) }.scope;
)

// Quarter Notes
(
var base = 1;
var mul = 0.5;
{ SinOsc.ar(440*Decay.ar(Impulse.ar(4*base), 1/(4*base)), 0, mul); }.scope;
)

// Half Notes
(
var base = 1;
var mul = 0.2;
{ SinOsc.ar(440*Decay.ar(Impulse.ar(4*base), 1/(4*base)), 0, mul); }.scope;
{ SinOsc.ar(440*Decay.ar(Impulse.ar(2*base), 1/(2*base)), 0, mul*4); }.scope;
)

// Whole Notes
(
var base = 1;
var mul = 0.2;
{ SinOsc.ar(440*Decay.ar(Impulse.ar(4*base), 1/(4*base)), 0, mul); }.scope;
{ SinOsc.ar(440*Decay.ar(Impulse.ar(2*base), 1/(2*base)), 0, mul*4); }.scope;
)


// Triplets
(
var base = 2;
var mul = 0.5;
{ [0, SinOsc.ar(880*Decay.ar(Impulse.ar(base), 1/base), 0, mul)]; }.scope;
//{ SinOsc.ar(440*Decay.ar(Impulse.ar(2*base), 1/(2*base)), 0, mul); }.scope;
{ SinOsc.ar(440*Decay.ar(Impulse.ar(3*base), 1/(3*base)), 0, mul); }.scope;
)
