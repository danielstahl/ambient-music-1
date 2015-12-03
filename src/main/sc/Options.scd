(
Server.local.options.blockSize = 128;
Server.internal.options.blockSize = 128;

Server.local.options.sampleRate = 96000;
Server.internal.options.sampleRate = 96000;

Server.local.options.numBuffers = 2056;
Server.internal.options.numBuffers = 2056;
)

s.boot;
s.shutdown;
s.plotTree;
s.queryAllNodes;
s.meter;

//Turn on dumpOSC
s.dumpOSC(1);

//Turn of dumpOSC
s.dumpOSC(0);


// Find the first private bus
s.options.firstPrivateBus;

(
// a synthdef browswer
SynthDescLib.global.read;
SynthDescLib.global.browse;
)

// Record
s.prepareForRecord;
s.record;
s.stopRecording;


s.prepareForRecord('/Users/danielstahl/Documents/Projects/ambient-i_mix/prologue-pulse_sc.aiff');
s.prepareForRecord('/Users/danielstahl/Documents/Projects/ambient-i_mix/ambient1-noise_sc.aiff');
s.prepareForRecord('/Users/danielstahl/Documents/Projects/ambient-i_mix/transition1-sine_sc.aiff');
s.prepareForRecord('/Users/danielstahl/Documents/Projects/ambient-i_mix/ambient2-pulse_sc.aiff');
s.prepareForRecord('/Users/danielstahl/Documents/Projects/ambient-i_mix/transition2-noise_sc.aiff');
s.prepareForRecord('/Users/danielstahl/Documents/Projects/ambient-i_mix/ambient3-sine_sc.aiff');
s.prepareForRecord('/Users/danielstahl/Documents/Projects/ambient-i_mix/epilogue-dust_sc.aiff');
s.record;
s.stopRecording;
