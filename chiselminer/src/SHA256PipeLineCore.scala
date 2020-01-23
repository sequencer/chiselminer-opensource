package me.sequencer.chiselminer

import chisel3._
import chipsalliance.rocketchip.config.Parameters

class SHA256PipeLineCoreIO()(implicit val p: Parameters) extends Bundle
  with HasMinerConfig {
  val coreWorkIn = Input(CoreWork())
  val messageBlockIn = Input(Vec(64, UInt(32.W)))
  val whkPreIn = Input(UInt(32.W))
  val gatingIn = if (asicBoost) Some(Input(Bool())) else None
  val hashStateOutReg = Output(Vec(8, UInt(32.W)))
  val whkPreOutReg = Output(UInt(32.W))
  val clockOut = Output(Clock())
  val verifyState = Output(UInt(32.W))
}

abstract class SHA256PipeLineCore(name: String)(implicit p: Parameters) extends Module {
  val io = IO(new SHA256PipeLineCoreIO)
  // core is "00"(software) "01"(ASICBoost) "1"(Normal)
  val core: String = name
  val sha256Pipes: Seq[SHA256Pipe]
}

class SHA256PipeLineCoreStandard(name: String, phantom: Boolean)(implicit val p: Parameters) extends SHA256PipeLineCore(name)
  with HasSHA256Implementation {
  val sha256Pipes: Seq[SHA256Pipe] = for (n <- 0 to 63) yield {
    val m = Module(SHA256Pipe(n, core, phantom = phantom && ((name == "01" && n < 3) || (name == "1" && n > 60))))
    m.suggestName(s"sha256Core${core}Pipe${n}")
    m
  }
  if (asicBoost) {
    for (n <- 0 to 63) {
      n match {
        case i if i == 0 =>
          sha256Pipes(i).io.extenderClockGating.get := io.gatingIn.get
        case default =>
          sha256Pipes(default).io.extenderClockGating.get := sha256Pipes(default - 1).io.extenderClockGating.get
      }
    }
  }
  sha256Pipes.head.io.messageBlockIn <> DontCare
  sha256Pipes.head.io.messageBlockIn := io.messageBlockIn
  sha256Pipes.head.io.hashStateIn := {
    if (phantom && name == "01") io.coreWorkIn.round67State else io.coreWorkIn.round64State
  }
  sha256Pipes.head.io.whkPreIn := io.whkPreIn
  for (i <- 0 to 62) {
    sha256Pipes(i + 1).io.hashStateIn := sha256Pipes(i).io.hashStateOutReg
    sha256Pipes(i + 1).io.messageBlockIn := sha256Pipes(i).io.messageBlockOutReg
    sha256Pipes(i + 1).io.whkPreIn := sha256Pipes(i).io.whkPreOutReg
  }
  sha256Pipes.last.io.messageBlockOutReg <> DontCare
  val stateOut = Wire(new StateOutBundle)
  withClock(sha256Pipes.last.io.clockOut) {
    stateOut := pipelineReg(addState(io.coreWorkIn.round64State, sha256Pipes.last.io.hashStateOutReg))
  }
  io.hashStateOutReg := stateOut.lastState
  io.whkPreOutReg := stateOut.whkPreOut
  io.clockOut := clock
  io.verifyState := {
    if (name == "1") pipelineReg(fullAdder(sha256Pipes.last.io.hashStateOutReg(4), constant.h(7), "Slow")) else DontCare
  }
}


object SHA256PipeLineCore {
  def apply()(implicit p: Parameters): SHA256PipeLineCore = new SHA256PipeLineCoreStandard(name = "01", phantom = false)

  def apply(name: String)(implicit p: Parameters): SHA256PipeLineCore = new SHA256PipeLineCoreStandard(name, phantom = false)

  def apply(name: String, phantom: Boolean)(implicit p: Parameters): SHA256PipeLineCore = new SHA256PipeLineCoreStandard(name, phantom)
}
