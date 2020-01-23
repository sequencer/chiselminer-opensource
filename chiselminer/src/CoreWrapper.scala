package me.sequencer.chiselminer

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters

class CoreWrapper()(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val workIn = Flipped(Decoupled(new SHA256WorkBundle))
    val chipIdIn = Input(UInt(8.W))
    val nonceOut = Decoupled(UInt(32.W))
  })
  val workReg: SHA256WorkBundle = Reg(new SHA256WorkBundle)
  val sha256dCore = Module(new SHA256dPipeLineCore)
  when(io.workIn.fire()) {
    workReg := io.workIn.bits
  }
  io.workIn.ready := true.B
  sha256dCore.io.workIn := workReg
  io.nonceOut.bits := sha256dCore.io.nonceOut
  io.nonceOut.valid := sha256dCore.io.hashStateOut(7).orR() === 0.U
  io.nonceOut.bits := DontCare
}
