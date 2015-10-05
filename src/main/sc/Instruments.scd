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

SynthDef(\sineControl, {
	arg dur, out = 0, phase = 0,
	startFreq, endFreq,
	mulStart = 1, mulEnd = 1,
	addStart = 0, addEnd = 0;
	var freqEnv, mulEnv, addEnv;
	freqEnv = Line.kr(startFreq, endFreq, dur, doneAction:2);
	mulEnv = Line.kr(mulStart, mulEnd, dur);
	addEnv = Line.kr(addStart, addEnd, dur);
	Out.kr(out, SinOsc.kr(freq:freqEnv, phase:phase, mul:mulEnv, add:addEnv));
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

SynthDef(\sine, {
	arg dur, freqBus, ampBus, out = 0;
	var freq, amp;
	Line.kr(dur:dur, doneAction:2);
	freq = In.kr(freqBus);
	amp = In.kr(ampBus);

	Out.ar(out, SinOsc.ar(freq) * amp);
}).add;

SynthDef(\pulse, {
        arg dur = 1, ampBus = 0, freqBus = 1, widthBus = 2, out = 0;
        var pulse, amp, freq, width;
        Line.kr(dur:dur, doneAction:2);
        amp = In.kr(ampBus);
        freq = In.kr(freqBus);
        width = In.kr(widthBus);
        pulse = Pulse.ar(freq, width);
        Out.ar(out, pulse * amp);
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

SynthDef(\filt, {
	arg dur = 1, ampBus = 0, freqBus = 1, bwBus = 2, in = 0, out = 0;
	var filt, amp, freq, bw, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	freq = In.kr(freqBus);
	bw = In.kr(bwBus);
	input = In.ar(in, 1);
	filt = BPF.ar(input, freq, bw * freq);
	filt = Balance.ar(filt, input);
	Out.ar(out, filt * amp);
}).add;

SynthDef(\filtReject, {
	arg dur = 1, ampBus = 0, freqBus = 1, bwBus = 2, in = 0, out = 0;
	var filt, amp, freq, bw, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	freq = In.kr(freqBus);
	bw = In.kr(bwBus);
	input = In.ar(in, 1);
	filt = BRF.ar(input, freq, bw * freq);
	filt = Balance.ar(filt, input);
	Out.ar(out, filt * amp);
}).add;

SynthDef(\filtReplace, {
	arg dur = 1, ampBus = 0, freqBus = 1, bwBus = 2, in = 0;
	var filt, amp, freq, bw, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	freq = In.kr(freqBus);
	bw = In.kr(bwBus);
	input = In.ar(in, 1);
	filt = BPF.ar(input, freq, bw * freq);
	filt = Balance.ar(filt, input);
	ReplaceOut.ar(in, filt * amp);
}).add;

SynthDef(\filtRejectReplace, {
	arg dur = 1, ampBus = 0, freqBus = 1, bwBus = 2, in = 0;
	var filt, amp, freq, bw, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	freq = In.kr(freqBus);
	bw = In.kr(bwBus);
	input = In.ar(in, 1);
	filt = BRF.ar(input, freq, bw * freq);
	filt = Balance.ar(filt, input);
	ReplaceOut.ar(in, filt * amp);
}).add;

SynthDef(\highPass, {
        arg dur = 1, freqBus = 1, in = 0, out = 0;
        var filt, freq, input;
        Line.kr(dur:dur, doneAction:2);
        freq = In.kr(freqBus);
        input = In.ar(in, 1);
        filt = HPF.ar(input, freq);
        Out.ar(out, filt);
}).add;

SynthDef(\highPassReplace, {
        arg dur = 1, freqBus = 1, in = 0;
        var filt, freq, input;
        Line.kr(dur:dur, doneAction:2);
        freq = In.kr(freqBus);
        input = In.ar(in, 1);
        filt = HPF.ar(input, freq);
        ReplaceOut.ar(in, filt);
}).add;

SynthDef(\lowPass, {
        arg dur = 1, freqBus = 1, in = 0, out = 0;
        var filt, freq, input;
        Line.kr(dur:dur, doneAction:2);
        freq = In.kr(freqBus);
        input = In.ar(in, 1);
        filt = LPF.ar(input, freq);
        Out.ar(out, filt);
}).add;

SynthDef(\lowPassReplace, {
        arg dur = 1, freqBus = 1, in = 0;
        var filt, freq, input;
        Line.kr(dur:dur, doneAction:2);
        freq = In.kr(freqBus);
        input = In.ar(in, 1);
        filt = LPF.ar(input, freq);
        ReplaceOut.ar(in, filt);
}).add;
)