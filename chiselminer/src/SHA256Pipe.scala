package me.sequencer.chiselminer

import chisel3._
import chipsalliance.rocketchip.config.Parameters
import me.sequencer.chiselblocks._


class SHA256PipeIO(implicit val p: Parameters) extends Bundle
  with HasMinerConfig {
  val messageBlockIn = Input(Vec(64, UInt(32.W)))
  val messageBlockOutReg = Output(Vec(64, UInt(32.W)))
  val whkPreIn = Input(UInt(32.W))
  val whkPreOutReg = Output(UInt(32.W))
  val hashStateIn = Input(Vec(8, UInt(32.W)))
  val hashStateOutReg = Output(Vec(8, UInt(32.W)))
  val clockOut = Output(Clock())
  val extenderClockGating = if(asicBoost) Some(Input(Bool())) else None
  val extenderClockGatingOut = if(asicBoost) Some(Output(Bool())) else None
}

object SHA256PipeIO {
  def apply()(implicit p: Parameters): SHA256PipeIO = new SHA256PipeIO
}

class SHA256Pipe(val n: Int, val core: String, val phantom: Boolean)(implicit val p: Parameters) extends Module
  with HasMinerConfig {
  // TODO: as for fixedByte optimization:
  // reduce each extender.messageBlockReg number by modifying messageBlockMin and messageBlockMax
  // connect fixed MessageBlock in SHA256Core
  override def desiredName = s"SHA256Core${core}Pipe${n}"

  val io = IO(SHA256PipeIO())
  val extenderClock = Wire(Clock())
  if (asicBoost) {
    val gatingReg = RegNext(io.extenderClockGating.get)
    extenderClock := ClockGate(clock, gatingReg)
    io.extenderClockGatingOut.get := gatingReg
  } else {
    extenderClock := clock
  }
  io.messageBlockOutReg <> DontCare
  io.clockOut := clock
  if (phantom) {
    io.messageBlockOutReg := io.messageBlockIn
    io.whkPreOutReg := io.whkPreIn
    io.hashStateOutReg := io.hashStateIn
  } else {
    val compressor = Module(SHA256Compressor(n, core))
    val extender = withClock(extenderClock) {
      Module(SHA256Extender(n, core))
    }
    compressor.io.whkPreIn := io.whkPreIn
    extender.io.messageBlockIn := io.messageBlockIn
    if (n < 63) {
      compressor.io.nextMessageBlock := io.messageBlockIn(n + 1)
      io.whkPreOutReg := compressor.io.whkPreOutReg
      io.messageBlockOutReg := extender.io.messageBlockOut
    } else {
      compressor.io.nextMessageBlock := DontCare
      io.whkPreOutReg := DontCare
    }

    compressor.io.hashStateIn := io.hashStateIn
    io.hashStateOutReg := compressor.io.hashStateOutReg
  }
}

object SHA256Pipe {
  def apply(n: Int, core: String = "01", phantom: Boolean = false)(implicit p: Parameters): SHA256Pipe = new SHA256Pipe(n, core, phantom)
}