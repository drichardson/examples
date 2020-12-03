// From Internal Snooping documentation

// Open the graphical class browser.
SequenceableCollection.browse;


//
// Snooping in Classes
//

Server.dump;
Server.instVarNames.postln;

// print all instance methods defined for this class
Collection.dumpInterface;
Mix.dumpInterface;

// print all class methods defined for this class
Collection.class.dumpInterface;
Mix.class.dumpInterface;


//
// Snooping in SynthDefs
//

// a synthdef to snoop in
(
f=SynthDef(\snoop, { arg out =0;
	Out.ar(out, PinkNoise.ar(0.1))
});
)

// get the ugens, listed in order of execution, with rate, index, and inputs.
f.dumpUGens

//
// Snooping on the Server
//

s.boot;

f = { PinkNoise.ar(0.1) * SinOsc.ar }; // a function
x = f.play;

// look at all the nodes on the server
s.queryAllNodes;

// parsed contents
s.dumpOSC(1);
x.free;
x = f.play;

// contents in hexadecimal
// status messages are not filtered
s.dumpOSC(2);
x.free;
x = f.play;

// turn off
s.dumpOSC(0);

//
// Snooping in the Interpreter
//

// display the values of all interpreter variables a-z
this.dump;

// set all variables a-z to nil
this.clearAll;

// compile some text into a Function
g = this.compile("(1 + 2).postln");
g.postln;   // g is a Function
g.value;    // evaluate g



//
// Plot Tree
//
s.plotTree;
w = { SinOsc.ar() }.play;
x = { SinOsc.ar(880) }.play;
w.free;
x.free;