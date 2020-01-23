package me.sequencer.chiselminer

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters

class CoreWorkIO extends Bundle {
  val round64State = Decoupled(Vec(8, UInt(32.W)))
  val round67State = Decoupled(Vec(8, UInt(32.W)))
}

class CoreWork extends Bundle {
  val round64State = Vec(8, UInt(32.W))
  val round67State = Vec(8, UInt(32.W))
}

object CoreWorkIO {
  def apply(): CoreWorkIO = new CoreWorkIO()
}

object CoreWork {
  def apply(): CoreWork = new CoreWork()
}

class SHA256MicroCore()(implicit val p: Parameters) extends Module
  with HasSHA256Implementation {
  val io = IO(new Bundle {
    val messageBlockIn = Flipped(Decoupled(Vec(19, UInt(32.W))))
    val stateOut = CoreWorkIO()
  })
  val messageBlock = RegInit(VecInit(Seq.fill(19)(0.U(32.W))))
  val state = RegInit(VecInit(Seq.fill(8)(0.U(32.W))))
  val compressResult: CompressOutBundle = Wire(new CompressOutBundle)
  val counter = RegInit(69.U(7.W))
  val whkPreWire = Wire(UInt(32.W))
  val index = Wire(UInt(6.W))
  val enable = WireInit(true.B)
  enable := io.stateOut.round67State.ready
  index := Mux(counter > 64.U, counter - 65.U, counter)
  whkPreWire := state(7) + VecInit(constant.k)(index) + messageBlock(Mux(index > 15.U, 15.U, index))
  compressResult := noSplitCompress(state, whkPreWire)
  when(enable) {
    counter := counter + 1.U
    when(counter =/= 64.U) {
      state := VecInit(Seq(
        compressResult.a,
        state(0),
        state(1),
        state(2),
        compressResult.e,
        state(4),
        state(5),
        state(6)
      ))
    }
  }
  when(counter > 14.U && counter < 64.U) {
    messageBlock(15) := noSplitExtend(messageBlock(14), messageBlock(1), messageBlock(9), messageBlock(0))
    for (i <- 0 to 14) {
      messageBlock(i) := messageBlock(i + 1)
    }
  }
  when(counter === 64.U) {
    state := noSplitAddState(state, VecInit(constant.h)).lastState
    for (i <- 0 to 2) {
      messageBlock(i) := messageBlock(i + 16)
    }
  }
  when(io.messageBlockIn.fire()) {
    state := constant.h
    messageBlock := io.messageBlockIn.bits
    counter := 0.U
  }
  io.messageBlockIn.ready := counter > 69.U
  io.stateOut.round64State.bits := state
  io.stateOut.round67State.bits := state
  io.stateOut.round64State.valid := counter === 65.U
  io.stateOut.round67State.valid := counter === 68.U
}
