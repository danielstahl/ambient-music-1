(

SynthDef(\lineControl, {
	arg dur = 1, startValue = 1, endValue = 1, out = 0;
	var lineEnv;
	lineEnv = Line.kr(startValue, endValue, dur, doneAction:2);
	Out.kr(out, lineEnv);
}).add;

SynthDef(\asrControl, {
	arg dur, out = 0,
	attackStart, sustainStart, decayStart, decayEnd,
	attackTime, sustainTime, decayTime;
	var asrEnv;
	asrEnv = Env(
		levels:[attackStart, sustainStart, decayStart, decayEnd],
		times: [attackTime, sustainTime, decayTime],
		curve: \sin);
	asrEnv.duration = dur;
	Out.kr(out, EnvGen.kr(asrEnv, doneAction:2));
}).add;

SynthDef(\arControl, {
	arg dur, out = 0,
	attackStart, releaseStart, releaseEnd,
	attackTime, attackType = \linear, releaseType = \linear;
	var arEnv;
	arEnv = Env(
		levels:[attackStart, releaseStart, releaseEnd],
		times: [attackTime, 1 - attackTime],
		curve: [attackType, releaseType]);
	arEnv.duration = dur;
	Out.kr(out, EnvGen.kr(arEnv, doneAction:2));
}).add;

SynthDef(\sinControl, {
	arg dur, out = 0,
	startFreq, endFreq,
	mulStart = 1, mulEnd = 1,
	addStart = 0, addEnd = 0;
	var freqEnv, mulEnv, addEnv;
	freqEnv = Line.kr(startFreq, endFreq, dur, doneAction:2);
	mulEnv = Line.kr(mulStart, mulEnd, dur);
	addEnv = Line.kr(addStart, addEnd, dur);
	Out.kr(out, SinOsc.kr(freqEnv, mul:mulEnv, add:addEnv));
}).add;

SynthDef(\pinkNoise, {
	arg dur = 1, ampBus = 0, out = 0;
	var noise, amp;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	noise = PinkNoise.ar(amp);
	Out.ar(out, noise);
}).add;

SynthDef(\whiteNoise, {
	arg dur = 1, ampBus = 0, out = 0;
	var noise, amp;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	noise = WhiteNoise.ar(amp);
	Out.ar(out, noise);
}).add;

SynthDef(\brownNoise, {
	arg dur = 1, ampBus = 0, out = 0;
	var noise, amp;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	noise = BrownNoise.ar(amp);
	Out.ar(out, noise);
}).add;

SynthDef(\dust, {
	arg dur = 1, freqBus = 0, ampBus = 0, out = 0;
	var noise, freq, amp;
	Line.kr(dur:dur, doneAction:2);
	freq = In.kr(freqBus);
	amp = In.kr(ampBus);
	noise = Dust.ar(freq);
	Out.ar(out, noise * amp);
}).add;

SynthDef(\binAural, {
	arg dur, leftFreqBus, rightFreqBus, ampBus, out = 0;
	var leftFreq, rightFreq, amp;
	Line.kr(dur:dur, doneAction:2);
	leftFreq = In.kr(leftFreqBus);
	rightFreq = In.kr(rightFreqBus);
	amp = In.kr(ampBus);

	Out.ar(out, [SinOsc.ar(leftFreq) * amp, SinOsc.ar(rightFreq) * amp]);
}).add;

SynthDef(\monAural, {
	arg dur, leftFreqBus, rightFreqBus, ampBus, out = 0;
	var leftFreq, rightFreq, amp;
	Line.kr(dur:dur, doneAction:2);
	leftFreq = In.kr(leftFreqBus);
	rightFreq = In.kr(rightFreqBus);
	amp = In.kr(ampBus);

	Out.ar(out, (SinOsc.ar(leftFreq) * amp) + (SinOsc.ar(rightFreq) * amp));
}).add;

SynthDef(\pan, {
	arg dur = 1, panBus = 0, in = 0, out = 0;
	var pan, input;
	Line.kr(dur:dur, doneAction:2);
	pan = In.kr(panBus);
	input = In.ar(in, 1);
	Out.ar(out, LinPan2.ar(input, pan));
}).add;

SynthDef(\stereoVolume, {
	arg dur = 1, ampBus = 0, in = 0, out = 0;
	var amp, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	input = In.ar(in, 2);
	Out.ar(out, input * amp);
}).add;

SynthDef(\monoVolume, {
	arg dur = 1, ampBus = 0, in = 0, out = 0;
	var amp, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	input = In.ar(in, 1);
	Out.ar(out, input * amp);
}).add;

SynthDef(\monoVolumeReplace, {
	arg dur = 1, ampBus = 0, in = 0;
	var amp, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	input = In.ar(in, 1);
	ReplaceOut.ar(in, input * amp);
}).add;

SynthDef(\gverb, {
	arg dur = 1, in = 0, out = 0,
	roomSize, revTime, damping, inputBw,
	spread = 15, earlyLevel, tailLevel;
	var input;
	Line.kr(dur:dur, doneAction:2);
	input = In.ar(in, 2);
	Out.ar(out,
		GVerb.ar(Mix.new(input),
		roomSize,
        revTime,
        damping,
        inputBw,
        spread,
        0,
        earlyLevel.dbamp,
        tailLevel.dbamp,
		roomSize, 0.3));
}).add;
)