a = (1..7);
// recale to 0-127, linear to linear
a.linlin(1, 7, 0, 127).round(1);
// rescale to 0-127, linear to exponential
a.linexp(1, 7, 0.01, 127).round(1); // don't use zero for exponential range