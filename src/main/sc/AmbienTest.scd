
(
SynthDef(\thetaPlus, {
	arg first, second;
	Out.ar(0, SinOsc.ar(first) + SinOsc.ar(second));

}).add;
)

(
SynthDef(\thetaStereo, {
	arg dur, first, secondStart, secondEnd, attack, amp;
	var secondEnv;
	secondEnv = Env(
		[secondStart, secondEnd, secondEnd],
		[attack, 1 - attack]);
	secondEnv.duration = dur;

	Out.ar(0, [SinOsc.ar(first) * amp, SinOsc.ar(EnvGen.ar(secondEnv, doneAction:2)) * amp]);

}).add;

SynthDef(\pink, {
	arg dur, amp, panStart, panEnd;
	var panEnv;
	panEnv = Line.kr(panStart, panEnd, dur, doneAction:2);
	Out.ar(0, Pan2.ar(PinkNoise.ar(amp), panEnv));
}).add;

SynthDef(\brown, {
	arg dur, amp, panStart, panEnd;
	var panEnv;
	panEnv = Line.kr(panStart, panEnd, dur, doneAction:2);
	Out.ar(0, Pan2.ar(BrownNoise.ar(amp), panEnv));
}).add;

SynthDef(\dust, {
	arg dur, freq, amp, panStart, panEnd;
	var panEnv;
	panEnv = Line.kr(panStart, panEnd, dur, doneAction:2);
	Out.ar(0, Pan2.ar(Dust.ar(freq) * amp, panEnv));
}).add;
)


Synth("thetaPlus", [first: 440, second: 446]);


// "http://en.wikipedia.org/wiki/Binaural_beats"
// "http://free-binaural-beats.com/product/golden-frequency/"
// "http://mynoise.net/NoiseMachines/binauralBrainwaveGenerator.php"
//"http://en.wikipedia.org/wiki/Monaural_beats"
// "http://brainwavewizard.com/entrainment/binaural-beats-monaural-beats-isochronic-tones/"
// "http://audioentrainment.com/osters-curve-carrier-frequency"
// "http://www.detoxorcist.com/binaural-beats-brainwave-entrainment-solfeggio-tones.html"
// "http://www.the-guided-meditation-site.com/the-oster-curve.html"

(
a = 60 * 5;
Synth("thetaStereo", [dur: a, first: 200, secondStart: 220, secondEnd: 207, attack: 0.3, amp: 0.1]);
Synth("pink", [dur: a, amp: 0.1, panStart: 1, panEnd: 0.5]);
Synth("brown", [dur: a, amp: 0.05, panStart: -1, panEnd: -0.5]);
Synth("dust", [dur: a, freq: 3, amp: 1.5, panStart: -1, panEnd: -0.5]);
Synth("dust", [dur: a, freq: 2, amp: 1.5, panStart: 1, panEnd: 0.5]);
)

Synth("thetaStereo", [dur: 10, first: 100, secondStart: 130, secondEnd: 107, attack: 0.3, amp: 0.1]);


{ PinkNoise.ar(SinOsc.kr(Line.kr(4/60, 7/60, 10)))}.play;

// "http://www.fastcolabs.com/3015970/why-ambient-noise-makes-you-more-productive-and-three-apps-that-do-it-right"

//"http://www.jstor.org/stable/10.1086/665048"

{ SinOsc.ar(40, SinOsc.kr(4))}.play;

{SinOsc.kr(4, mul:1, add:-0.5)}.plot(1);

{SinOsc.kr(4)}.plot(1);



{ WhiteNoise.ar(SinOsc.kr(Line.kr(4/60, 7/60, 10)))}.play;

{SinOsc.kr(4, mul:0.5)}.plot(1);






