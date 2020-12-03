
// https://sccode.org/1-4RG
//Fig 44.5: Motor
//Contains the subpatches shown in fig 44.2, 44.3, 44.4 and 44.6.
// based on pure data code from the book "Designing Sound" by Andy Farnell. (Chapter 44, Practical 21, Motors)
(
{

  |     // arguments range: 0.0 - 1.0
        runtime         = 0.2,
        statorLevel     = 0.7,
        brushLevel      = 0.9,
        rotorLevel      = 0.6,
        maxSpeed        = 0.3,
        volume          = 0.5,
        tubeRes         = 0.2
  |

        var motorEnv, motorEnv1, motorEnv2, motor, rotor, drive, stator, resonance;

        // fig 44.2: Speed Control Envelope
        motorEnv = (Line.ar(0,1,runtime * 20, doneAction:2)) * 2;
        motorEnv1 = (1 - motorEnv.min(1)).pow(6); // fast growing curve
        motorEnv2 = motorEnv.max(1) - 1; // linear decay
        motorEnv = (motorEnv1 + motorEnv2) * (-1) + 1;

        drive = motorEnv * (maxSpeed * (-2000));
        drive = LFSaw.ar(drive, 1, 0.5, 0.5);

        // fig 44.3: Rotor
        rotor = WhiteNoise.ar;
        rotor = BPF.ar(rotor, 4000, 1.reciprocal) * brushLevel;
        rotor = (rotor + (rotorLevel * 0.2)) * drive.pow(4);

        // fig 44.4: Stator
        stator = (Wrap.ar(drive * 2) * 2pi).cos;
        // notice the difference in SC between (stator * stator) and stator.pow(2)
        // to achieve the desired sound result from the book, use (stator * stator)
        // stator = stator.pow(2) + 1; // equals stator ** 2 + 1;
        stator = (stator * stator) + 1;
        stator = (1 / stator - 0.5) * statorLevel;

        // fig 44.6: FM body resonance
        resonance = SinOsc.ar(178) * drive + motorEnv;
        resonance = (resonance * 2pi).cos;
        resonance = (resonance - OnePole.ar(resonance, exp(-2pi * (180 * SampleDur.ir))));
        resonance = (resonance - OnePole.ar(resonance, exp(-2pi * (180 * SampleDur.ir))));
        resonance = resonance * tubeRes;

        motor = motorEnv * (rotor + stator + resonance);
        motor = (motor * volume).dup;

}.play
)


// code also available here:
// http://en.wikibooks.org/wiki/Designing_Sound_in_SuperCollider/Motors