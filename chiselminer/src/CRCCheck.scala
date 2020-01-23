package me.sequencer.chiselminer

import chisel3._
import chisel3.util._

class CRCCheck(width: Int, poly: Int, init: Int, xorOut: Int, inputReverse: Boolean,reflect: Boolean = false) extends Module{
  require(width >= 8)
  val shift: Int = width - 8
  val io = IO(new Bundle {
    val input = Flipped(Decoupled(UInt(8.W)))
    val output = Decoupled(UInt(width.W))
  })
  val initReg = RegInit(init.U(width.W))
  val inputWire = WireInit(0.U(width.W))
  val clear = WireInit(false.B)
  clear := io.output.fire()
  inputWire := io.input.bits << shift
  val CalCount = RegInit(0.U(4.W))
  io.input.ready := CalCount === 0.U
  when(io.input.fire()) {
    initReg := initReg ^ inputWire
    CalCount := 8.U
  }
  when(CalCount =/= 0.U) {
    CalCount := CalCount - 1.U
    when(initReg(width - 1) === 0.U) {
      initReg := initReg << 1.U
    }.otherwise {
      initReg := (initReg << 1.U) ^ poly.U
    }
  }
  io.output.bits := initReg
  io.output.valid := CalCount === 0.U
  when(RegNext(clear)) {
    initReg := init.U
  }
}

object CRCCheck {
  def apply(width: Int, poly: Int, init: Int, xorOut: Int, inputReverse: Boolean, reflect: Boolean = false): CRCCheck = new CRCCheck(width, poly, init, xorOut, inputReverse, reflect)
}
