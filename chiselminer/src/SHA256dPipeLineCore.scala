package me.sequencer.chiselminer

import chisel3._
import chipsalliance.rocketchip.config.Parameters

class SHA256WorkBundle extends Bundle {
  // 80 bytes to be hashed
  // midState: 32 bytes generated by sha256Core00 with the input of 64 bytes of original work.
  // dataTail: 12 bytes remain in the original work.
  // midState will be send to sha256Shared
  val midState: Vec[UInt] = Vec(8, UInt(32.W))
  val dataTail: Vec[UInt] = Vec(3, UInt(32.W))
}

class SHA256dPipeLineCore()(implicit val p: Parameters) extends Module
  with HasSHA256Implementation
  with HasMinerConfig {
  // this is the example core with full SHA256 implementation without any optimization.
  val io = IO(new Bundle {
    val workIn = Input(new SHA256WorkBundle)
    val hashStateOut = Output(Vec(8, UInt(32.W)))
    val nonceOut = Output(UInt(32.W))
  })
  val sha256Core0: SHA256PipeLineCore = Module(SHA256PipeLineCore())
  val sha256Core1: SHA256PipeLineCore = withClock(sha256Core0.io.clockOut) {
    Module(SHA256PipeLineCore())
  }

  val nonceCounter: UInt = Reg(UInt(32.W))
  nonceCounter := nonceCounter + 1.U(32.W)
  sha256Core0.io.coreWorkIn.round64State := io.workIn.midState
  sha256Core0.io.whkPreIn := generateWHK(sha256Core0.io.coreWorkIn.round64State(7), sha256Core0.io.messageBlockIn(0), -1)
  for (i <- 0 to 15) {
    sha256Core0.io.messageBlockIn(i) := {
      i match {
        case j if j < 3 => io.workIn.dataTail(i)
        case j if j == 3 => nonceCounter
        case j if j == 4 => "h80000000".U(32.W)
        case j if j < 15 => 0.U(32.W)
        case j if j == 15 => 640.U(32.W)
      }
    }
  }
  sha256Core1.io.coreWorkIn.round64State := constant.h

  sha256Core1.io.whkPreIn := sha256Core0.io.whkPreOutReg

  for (i <- 0 to 15) {
    sha256Core1.io.messageBlockIn(i) := {
      i match {
        case j if j < 8 => sha256Core0.io.hashStateOutReg(i)
        case j if j == 8 => "h80000000".U(32.W)
        case j if j < 15 => 0.U(32.W)
        case j if j == 15 => 256.U(32.W)
      }
    }
  }
  io.hashStateOut := sha256Core1.io.hashStateOutReg
  io.nonceOut := nonceCounter
}

object SHA256dPipeLineCore {
  def apply()(implicit p:Parameters): SHA256dPipeLineCore = new SHA256dPipeLineCore
}
