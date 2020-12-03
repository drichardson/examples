{ Pan2.ar(WhiteNoise.ar(1) * 0.2, SinOsc.kr(2)) }.play;

{ Resonz.ar(WhiteNoise.ar(1), 1500, SinOsc.kr(5, mul: 0.5, add: 0.5) * 0.3 + 0.2) ! 2 }.play;