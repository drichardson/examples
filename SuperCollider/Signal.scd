// Signal is a sampled audio buffer

(
[
	Signal.sineFill(1000, 1.0/(1..6)),
	Signal.chebyFill(1000, [1]),

	// shifted to avoid DC offset when waveshaping a zero signa
	Signal.chebyFill(1000, [0, 1], zeroOffset: true),

	// normalized sum of (unshifted) Chebyshev polynomials (the default
	Signal.chebyFill(1000, [0, 1, 0, 0, 0, 1], normalize: true, zeroOffset: false),

	Signal.chebyFill(1000, [0, 0, 1]),
	Signal.chebyFill(1000, [0.3, -0.8, 1.1]),

].plot("Signal: Fill Functions");
)

(
[
	Signal.hanningWindow(1024),
	Signal.hanningWindow(1024, 512),

	Signal.hammingWindow(1024),
	Signal.hammingWindow(1024, 512),

	Signal.welchWindow(1024),
	Signal.welchWindow(1024, 512),

	Signal.rectWindow(1024),
	Signal.rectWindow(1024, 512),

].plot("Signal: Window Functions");
)




b = Signal.sineFill(100, [1]).play(true, 0.2);
b.free;



(
a = Signal.newClear(256);
a.waveFill({ arg x, old, i; sin(x)}, 0, 3pi);
a.waveFill({ arg x, old, i; old * sin(11 * x + 0.3) }, 0, 3pi);
a.waveFill({ arg x, old, i; old * (x % 4) }, 0, 3pi);

a.plot;
)



Signal[1, 2, -4, 2.5].normalize;

a = Signal.fill(10, 1);
a.fade(0, 3);        // fade in
a.fade(6, 9, 1, 0);    // fade out


Signal[1, 2, 3, 4].integral;



Signal[1, 2, 3, 4].blend(Signal[5, 5, 5, 0], 0);
Signal[1, 2, 3, 4].blend(Signal[5, 5, 5, 0], 0.2);
Signal[1, 2, 3, 4].blend(Signal[5, 5, 5, 0], 0.4);
Signal[1, 2, 3, 4].blend(Signal[5, 5, 5, 0], 1);
Signal[1, 2, 3, 4].blend(Signal[5, 5, 5, 0], 2);

(
var size = 512, real, imag, cosTable, complex;

real = Signal.newClear(size);
        // some harmonics
real.sineFill2([[8], [13, 0.5], [21, 0.25], [55, 0.125, 0.5pi]]);
        // add a little noise
real.overDub(Signal.fill(size, { 0.2.bilinrand }));

imag = Signal.newClear(size);
cosTable = Signal.fftCosTable(size);

complex = fft(real, imag, cosTable);
[real, imag, (complex.magnitude) / 100 ].flop.flat
    .plot("fft", Rect(0, 0, 512 + 8, 500), numChannels: 3);
)

(
var size = 512, real, imag, cosTable, complex, ifft;

real = Signal.newClear(size);
        // some harmonics
real.sineFill2([[8], [13, 0.5], [21, 0.25], [55, 0.125, 0.5pi]]);
        // add a little noise
real.overDub(Signal.fill(size, { 0.2.bilinrand }));

imag = Signal.newClear(size);
cosTable = Signal.fftCosTable(size);

complex = fft(real, imag, cosTable).postln;
ifft = complex.real.ifft(complex.imag, cosTable);

[real, ifft.real].flop.flat
    .plot("fft and back", Rect(0, 0, 512 + 8, 500), numChannels: 2);
)



x = Signal.sineFill(512, [0, 0, 0, 1]);
[x, x.neg, x.abs, x.sign, x.squared, x.cubed, x.asin.normalize, x.exp.normalize, x.distort].flop.flat.plot(numChannels: 9);