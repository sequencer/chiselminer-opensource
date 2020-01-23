package me.sequencer.chiselminer

import chisel3._
import chipsalliance.rocketchip.config._
import me.sequencer.chiselblocks._

case object BuildAdder extends Field[(String, Parameters) => Adder]

case object SlotsNum extends Field[Int]

case object MidStateNum extends Field[Int]

case object RegNum extends Field[Int]

case object ASICBoostEnable extends Field[Boolean]

case object SplitPipe extends Field[Boolean]

case object DFTEnable extends Field[Boolean]

case object UARTParam extends Field[UARTParams]

case object IdleCycles extends Field[BigInt]

case object CoreNum extends Field[Int]

class Adder(myName: String)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
    val out = Output(UInt(32.W))
  })

  override def desiredName = s"fullAdder${myName}"

  io.out := io.a + io.b
}

trait HasMinerConfig {
  implicit val p: Parameters

  def dft: Boolean = p(DFTEnable)

  def midStateNum: Int = p(MidStateNum)

  def regNum: Int = p(RegNum)

  def slots: Int = p(SlotsNum)

  def adder(myName: String): Adder = p(BuildAdder)(myName, p)

  def asicBoost: Boolean = p(ASICBoostEnable)

  def splitPipe: Boolean = p(SplitPipe)

  def uartParams: UARTParams = p(UARTParam)

  def idleCycles: BigInt = p(IdleCycles)

  def cores: Int = p(CoreNum)
}

class FPGAConfig extends Config(
  (site, here, up) => {
    case ASICBoostEnable => false
    case SplitPipe => false
    case SlotsNum => 1
    case MidStateNum => 4
    case CoreNum => 2
    case DFTEnable => true
    case RegNum => 32
    case IdleCycles => 32 * BigInt(here(UARTParam).frequency) / BigInt(here(UARTParam).baudrate)
    case UARTParam => UARTParams(frequency = 100000000, baudrate = 1000000, sampleNumber = 4)
    case BuildAdder => (myName: String, p: Parameters) => new Adder(myName)(p)
  }
)
