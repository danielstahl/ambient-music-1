package music

import net.soundmining._
import net.soundmining.Instrument._
import java.{lang => jl}

/**
 * Instruments
 */


object Instruments {

}

class LineControlInstrumentBuilder extends AbstractInstrumentBuilder with ControlInstrumentBuilder {
  type SelfType = LineControlInstrumentBuilder
  def self(): SelfType = this

  val instrumentName: String = "lineControl"

  var startValue: jl.Float = _
  var endValue: jl.Float = _

  def control(start: Float, end: Float): SelfType = {
    startValue = buildFloat(start)
    endValue = buildFloat(end)
    self()
  }

  def reverse: LineControlInstrumentBuilder =
    LineControlInstrumentBuilder.line(dur.floatValue(), endValue.floatValue(), startValue.floatValue())

  override def build(): Seq[Object] =
    super.build() ++
      buildOut() ++
      buildDur() ++
      Seq(
        "startValue", startValue,
        "endValue", endValue
      )
}

object LineControlInstrumentBuilder {
  def line(dur: Float, start: Float, end: Float, nodeId: Node = SOURCE): LineControlInstrumentBuilder = {
    new LineControlInstrumentBuilder()
      .control(start, end)
      .dur(dur)
      .nodeId(nodeId)
  }
}

class ASRControlInstrumentBuilder extends AbstractInstrumentBuilder with ControlInstrumentBuilder {
  type SelfType = ASRControlInstrumentBuilder
  def self(): SelfType = this

  val instrumentName: String = "asrControl"

  var attackStart: jl.Float = buildFloat(1.0f)
  var sustainStart: jl.Float = buildFloat(1.0f)
  var decayStart: jl.Float = buildFloat(1.0f)
  var decayEnd: jl.Float = buildFloat(1.0f)

  def values(attackStartValue: Float, sustainStartValue: Float, decayStartValue: Float, decayEndValue: Float): SelfType = {
    attackStart = buildFloat(attackStartValue)
    sustainStart = buildFloat(sustainStartValue)
    decayStart = buildFloat(decayStartValue)
    decayEnd = buildFloat(decayEndValue)
    self()
  }

  var attackTime: jl.Float = buildFloat(1.0f)
  var sustainTime: jl.Float = buildFloat(1.0f)
  var decayTime: jl.Float = buildFloat(1.0f)

  def times(attackTimeValue: Float, sustainTimeValue: Float, decayTimeValue: Float): SelfType = {
    attackTime = buildFloat(attackTimeValue)
    sustainTime = buildFloat(sustainTimeValue)
    decayTime = buildFloat(decayTimeValue)
    self()
  }

  override def build(): Seq[Object] =
    super.build() ++
      buildOut() ++
      buildDur() ++
      Seq(
        "attackStart", attackStart,
        "sustainStart", sustainStart,
        "decayStart", decayStart,
        "decayEnd", decayEnd,
        "attackTime", attackTime,
        "sustainTime", sustainTime,
        "decayTime", decayTime)
}

object ASRControlInstrumentBuilder {
  def asr(dur: Float, values: (Float, Float, Float, Float), times: (Float, Float, Float), nodeId: Node = SOURCE): ASRControlInstrumentBuilder = {
    new ASRControlInstrumentBuilder()
      .nodeId(nodeId)
      .values(values._1, values._2, values._3, values._4)
      .times(times._1, times._2, times._3)
      .dur(dur)
  }
}

object ARControlInstrumentBuilder {
  def ar(dur: Float, attackTime: Float, values: (Float, Float, Float), arType: (EnvCurve, EnvCurve) = (LINEAR, LINEAR), nodeId: Node = SOURCE): ARControlInstrumentBuilder = {
    new ARControlInstrumentBuilder()
      .nodeId(nodeId)
      .values(values._1, values._2, values._3)
      .types(arType._1, arType._2)
      .attackTime(attackTime)
      .dur(dur)
  }
}

class ARControlInstrumentBuilder extends AbstractInstrumentBuilder with ControlInstrumentBuilder {
  type SelfType = ARControlInstrumentBuilder
  def self(): SelfType = this

  val instrumentName: String = "arControl"

  var attackStart: jl.Float = buildFloat(1.0f)
  var releaseStart: jl.Float = buildFloat(1.0f)
  var releaseEnd: jl.Float = buildFloat(1.0f)

  def values(attackStartValue: Float, releaseStartValue: Float, releaseEndValue: Float): SelfType = {
    attackStart = buildFloat(attackStartValue)
    releaseStart = buildFloat(releaseStartValue)
    releaseEnd = buildFloat(releaseEndValue)
    self()
  }

  var attackTime: jl.Float = buildFloat(1.0f)

  def attackTime(attackTimeValue: Float): SelfType = {
    attackTime = buildFloat(attackTimeValue)
    self()
  }

  var attackType: EnvCurve = LINEAR
  var releaseType: EnvCurve = LINEAR

  def types(attackTypeValue: EnvCurve, releaseTypeValue: EnvCurve): SelfType = {
    attackType = attackTypeValue
    releaseType = releaseTypeValue
    self()
  }

  override def build(): Seq[Object] =
    super.build() ++
      buildOut() ++
      buildDur() ++
      Seq(
        "attackStart", attackStart,
        "releaseStart", releaseStart,
        "releaseEnd", releaseEnd,
        "attackTime", attackTime,
        "attackType", attackType.name,
        "releaseType", releaseType.name)
}

object SinControlInstrumentBuilder {
  def sin(dur: Float, startFreq: Float, endFreq: Float, mulStart: Float = 1.0f, mulEnd: Float = 1.0f, addStart: Float = 0, addEnd: Float = 0, nodeId: Node = SOURCE) =
    new SinControlInstrumentBuilder()
      .nodeId(nodeId)
      .freq(startFreq, endFreq)
      .mul(mulStart, mulEnd)
      .add(addStart, addEnd)
      .dur(dur)
}

class SinControlInstrumentBuilder extends AbstractInstrumentBuilder with ControlInstrumentBuilder {
  type SelfType = SinControlInstrumentBuilder
  def self(): SelfType = this
  val instrumentName: String = "sinControl"

  var startFreq: jl.Float = buildFloat(1.0f)
  var endFreq: jl.Float = buildFloat(1.0f)
  var mulStart: jl.Float = buildFloat(1.0f)
  var mulEnd: jl.Float = buildFloat(1.0f)
  var addStart: jl.Float = buildFloat(0.0f)
  var addEnd: jl.Float = buildFloat(0.0f)

  def freq(start: Float, end: Float): SelfType = {
    startFreq = buildFloat(start)
    endFreq = buildFloat(end)
    self()
  }

  def mul(start: Float, end: Float): SelfType = {
    mulStart = buildFloat(start)
    mulEnd = buildFloat(end)
    self()
  }

  def add(start: Float, end: Float): SelfType = {
    addStart = buildFloat(start)
    addEnd = buildFloat(end)
    self()
  }

  override def build(): Seq[Object] =
    super.build() ++
      buildOut() ++
      buildDur() ++
      Seq(
        "startFreq", startFreq,
        "endFreq", endFreq,
        "mulStart", mulStart,
        "mulEnd", mulEnd,
        "addStart", addStart,
        "addEnd", addEnd)
}

abstract class CommonNoiseInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with OutputBuilder {
  val ampBus = ControlArgumentBuilder[SelfType](self(), "ampBus")

  override def build(): Seq[Object] =
    super.build() ++
      buildOut() ++
      ampBus.buildBus() ++
      buildDur()
}

class PinkNoiseInstrumentBuilder extends CommonNoiseInstrumentBuilder {
  type SelfType = PinkNoiseInstrumentBuilder
  def self(): SelfType = this

  val instrumentName: String = "pinkNoise"
}

class WhiteNoiseInstrumentBuilder extends CommonNoiseInstrumentBuilder {
  type SelfType = WhiteNoiseInstrumentBuilder
  def self(): SelfType = this

  val instrumentName: String = "whiteNoise"
}

class BrownNoiseInstrumentBuilder extends CommonNoiseInstrumentBuilder {
  type SelfType = BrownNoiseInstrumentBuilder
  def self(): SelfType = this

  val instrumentName: String = "brownNoise"
}

class DustInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with OutputBuilder {
  type SelfType = DustInstrumentBuilder
  def self(): SelfType = this

  val instrumentName: String = "dust"

  val ampBus = ControlArgumentBuilder[SelfType](self(), "ampBus")

  val freqBus = ControlArgumentBuilder[SelfType](self(), "freqBus")

  override def build(): Seq[Object] =
    super.build() ++
      buildOut() ++
      ampBus.buildBus() ++
      freqBus.buildBus() ++
      buildDur()
}

abstract class CommonAuralInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with OutputBuilder {
  val ampBus = ControlArgumentBuilder[SelfType](self(), "ampBus")

  val leftFreqBus = ControlArgumentBuilder[SelfType](self(), "leftFreqBus")

  val rightFreqBus = ControlArgumentBuilder[SelfType](self(), "rightFreqBus")

  override def build(): Seq[Object] =
    super.build() ++
    buildOut() ++
    ampBus.buildBus() ++
    leftFreqBus.buildBus() ++
    rightFreqBus.buildBus() ++
    buildDur()
}

class BinAuralInstrumentBuilder extends CommonAuralInstrumentBuilder {
  type SelfType = BinAuralInstrumentBuilder
  def self(): SelfType = this

  val instrumentName: String = "binAural"
}

class MonAuralInstrumentBuilder extends CommonAuralInstrumentBuilder {
  type SelfType = MonAuralInstrumentBuilder
  def self(): SelfType = this

  val instrumentName: String = "monAural"
}

abstract class CommonVolumeBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder {

  val ampBus = ControlArgumentBuilder[SelfType](self(), "ampBus")

  override def build(): Seq[Object] =
    super.build() ++
      buildIn() ++
      buildDur() ++
      ampBus.buildBus()
}

class StereoVolumeBuilder extends CommonVolumeBuilder with OutputBuilder {
  type SelfType = StereoVolumeBuilder
  def self(): SelfType = this

  val instrumentName: String = "stereoVolume"

  override def build(): Seq[Object] =
    super.build() ++
      buildOut()
}

class MonoVolumeBuilder extends CommonVolumeBuilder with OutputBuilder {
  type SelfType = MonoVolumeBuilder
  def self(): SelfType = this

  val instrumentName: String = "monoVolume"

  override def build(): Seq[Object] =
    super.build() ++
      buildOut()
}

class MonoVolumeReplaceBuilder extends CommonVolumeBuilder {
  type SelfType = MonoVolumeReplaceBuilder
  def self(): SelfType = this

  val instrumentName: String = "monoVolumeReplace"
}

class PanInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder with OutputBuilder {
  type SelfType = PanInstrumentBuilder
  def self(): SelfType = this

  val instrumentName: String = "pan"

  val panBus = ControlArgumentBuilder[PanInstrumentBuilder](self(), "panBus")

  override def build(): Seq[Object] =
    super.build() ++
      buildIn() ++
      buildDur() ++
      buildOut() ++
      panBus.buildBus()
}

class GverbInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder with OutputBuilder {
  type SelfType = GverbInstrumentBuilder
  def self(): SelfType = this

  val instrumentName: String = "gverb"

  var roomSize: jl.Float = buildFloat(0f)

  def roomSize(value: Float): SelfType = {
    roomSize = buildFloat(value)
    self()
  }

  var revTime: jl.Float = buildFloat(0f)

  def revTime(value: Float): SelfType = {
    revTime = buildFloat(value)
    self()
  }

  var damping: jl.Float = buildFloat(0f)

  def damping(value: Float): SelfType = {
    damping = buildFloat(value)
    self()
  }

  var inputBw: jl.Float = buildFloat(0f)

  def inputBw(value: Float): SelfType = {
    inputBw = buildFloat(value)
    self()
  }

  var spread: jl.Float = buildFloat(0f)

  def spread(value: Float): SelfType = {
    spread = buildFloat(value)
    self()
  }

  var earlyLevel: jl.Float = buildFloat(0f)

  def earlyLevel(value: Float): SelfType = {
    earlyLevel = buildFloat(value)
    self()
  }

  var tailLevel: jl.Float = buildFloat(0f)

  def tailLevel(value: Float): SelfType = {
    tailLevel = buildFloat(value)
    self()
  }

  override def build(): Seq[Object] =
    super.build() ++
      buildIn() ++
      buildOut() ++
      buildDur() ++
      Seq(
        "roomSize", roomSize,
        "revTime", revTime,
        "damping", damping,
        "inputBw", inputBw,
        "spread", spread,
        "earlyLevel", earlyLevel,
        "tailLevel", tailLevel)
}