(

// Put cursor just anywhere in block below and then press Control+Enter to run the entire thing. Press Control+. (period) to silence audio.

(
f = {[SinOsc.ar(440, 0, 0.2),SinOsc.ar(442,0,0.2)]};
f.postln;
f.play;
);

(
f = { "Function evaluated".postln; };
f.value;
f.value;
f.value;
);
(
"# value a number".postln;
f = 3; // set f to a number
f.value.postln; // post window says: 3
f.value.postln; // still says 3
);

"# value calling a function".postln;
(
f = { 3.0.rand; }; // here f is a function
f.value.postln;
f.value.postln;
);

(
"# Polymorphism".postln;
f = { arg a; a.value + 3 }; // call 'value' on the arg; polymorphism awaits!
f.value(3);
g = {3.0.rand; };
f.value(g).postln;
f.value(g).postln;
);

(
" # Function arguments".postln;
f = { arg a, b; a / b; }; // divide
f.value(10,2).postln; // regular style
f.value(b: 2, a: 10).postln; // keyword style
);

(
"# Function Variables".postln;
f = { arg a, b;
	var firstResult, finalResult; // the var statement has to occur before any other code in the function.
	firstResult = a + b;
	finalResult = firstResult * 2;
	finalResult;
};
f.value(2,3).postln; // this will return (2 + 3) * 2 = 10
);

)


// Function's
f = { |x| x * 2 }
g = { |x| x * 2 }
f.class
f.isKindOf(AbstractFunction);
f.isKindOf(Function);
f.isMemberOf(Function);
f == f;
f === f;
f == g;
f === g;
f.value(1);

// AbstractFunction implements a bunch of methods to support lazy evaluation of math functions.

// some lazy unary ops
f.sin.class;
f.sin.value(1);
f.cos.value(1);
f.squared.value(2);
f.cubed.value(2);
f.log2.value(10);
f.isPositive.value(0);
f.isStrictlyPositive.value(0);

// some lazy binary ops
g = { |x| x + 1 };
(f + g).class;
(f + g).value(10);
(g + f).value(10);
g.value(10) + f.value(10);
g.value(10);
(10 - 3).dist

// some lazy n-ary ops
g.blend


// Function composition
// compose a function that will return an array of random length
a = { |n| { 16.rand } ! n } <> { |x, y| rrand(4, 8) };
a.value;

// compose a function from a that selects only odd values
b = { |x| x.select(_.odd) } <> a;
b.value;


// flop - sort of like map in other languages. Produces a function that applies the original function to elements of argument lists.
f = { |a, b| if(a > 0) { a + b } { -inf } }.flop;
f.value([-1, 2, 1, -3.0], [10, 1000]);
f.value(2, 3);
f.value(-1, 3);
f.value([-1,0,1,2], 5);
f.value(-1, [1,2,3]);
f.value(1, [1,2,3]);
