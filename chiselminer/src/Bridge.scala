package me.sequencer.chiselminer

import chisel3._
import chisel3.util._

class Bridge extends Module {
  val io = IO(new Bundle {
    // core wrapper interface
    val coreWrapperWorkIn = Decoupled(new SHA256WorkBundle)
    val coreWrapperChipIdIn = Output(UInt(8.W))
    val coreWrapperNonceOut = Flipped(Decoupled(UInt(32.W)))
    // controller interface
    val controllerRx = Decoupled(UInt(8.W))
    val controllerTx = Flipped(Decoupled(UInt(8.W)))
    val controllerWorkBundleOut = Flipped(Decoupled(new SHA256WorkBundle))
    val controllerNonceIn = Decoupled(UInt(32.W))
    // uartTx interface
    val txIn = Decoupled(Bits(8.W))
    // uart rx interface
    val rx_err = Input(Bits(1.W))
    val rx_out = Flipped(Decoupled(UInt(8.W)))
  })

  val errCounter = RegInit(0.U(4.W))
  val rxQueue: DecoupledIO[UInt] = Queue(io.rx_out, 3, flow = true, pipe = true)
  val txQueue: DecoupledIO[UInt] = Queue(io.controllerTx, 46, flow = true, pipe = true)
  val nonceQueue: DecoupledIO[UInt] = Queue(io.coreWrapperNonceOut, 3, flow = true, pipe = true)
  val workQueue: DecoupledIO[SHA256WorkBundle] = Queue(io.controllerWorkBundleOut, 2, flow = true, pipe = true)
  io.coreWrapperChipIdIn := DontCare

  io.controllerRx <> rxQueue
  io.txIn <> txQueue
  errCounter := errCounter + io.rx_err
  io.coreWrapperWorkIn <> workQueue
  io.controllerNonceIn <> nonceQueue
}

object Bridge {
  def apply(): Bridge = new Bridge()
}