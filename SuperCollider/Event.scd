// https://doc.sccode.org/Tutorials/Getting-Started/14-Scheduling-Events.html
// https://doc.sccode.org/Classes/Event.html

// An Event specifies an action to be taken in response to a -play message. Its key/value pairs specify the parameters of that action. Event inherits most of its methods from its superclasses, especially from Dictionary. For the usage and meaning of the parent and proto events, see IdentityDictionary.

a = (x: 6, y: 7, play: { (~x * ~y).postln });
a.play; // returns 6 * 7

(freq: 761).play // play a default synth sound with 761 Hz

// Scheduling
SystemClock.sched(5, { "5 seconds passed".postln });

(
var timeNow;
TempoClock.default.tempo = 2; // 2 beats/sec, or 120 BPM
timeNow = TempoClock.default.beats;
"Time is now: ".post; timeNow.postln;
"Scheduling for: ".post; (timeNow + 5).postln;
TempoClock.default.schedAbs(timeNow + 5, {
	"Time is later: ".post;
	thisThread.clock.beats.postln;
	nil;
});
)


// What time is it?
SystemClock.beats;
TempoClock.default.beats;
AppClock.beats;
thisThread.clock.beats; // use inside a scheduled function without worrying about which clock ran the function.


// What can you schedule?
TempoClock.default.sched(5, "hello"); // nothing happens, hello is just a value.
// rather, you need to schedule something that takes action, like a Function, Routine, or Task.

//
// CAUTION: If you schedule a *Function* (but not a Routine or Task) that returns a number, the clock will treat that number as the amount of time before running the function again.
//

// fires many times (but looks like it should fire just once). This will keep going forever, until you stop it with cmd-.
TempoClock.default.sched(1, { rrand(1, 3).postln; });

// If you want the function to run only once, make sure to end the function with 'nil':
TempoClock.default.sched(1, { rrand(1, 3).postln; nil }); // fires once