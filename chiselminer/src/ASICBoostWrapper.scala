package me.sequencer.chiselminer

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters


class ASICBoostWrapper()(implicit val p: Parameters) extends Module
  with HasMinerConfig {
  val coreIdWidth: Int = log2Ceil(cores)
  val grayWidth: Int = log2Ceil(midStateNum)
  val nonceWidth: Int = 32 - coreIdWidth
  val io = IO(new Bundle {
    val upstreamWork = Input(new UpstreamWork)
    val debugValid = Output(Bool())
    val nonce = Decoupled(UInt(64.W))
  })

  val grayReg = RegInit(0.U(grayWidth.W))
  val garyCounter = Wire(UInt(grayWidth.W))
  val nonceCount = RegInit(0.U(nonceWidth.W))
  val workShifter = RegInit(VecInit(Seq.fill(midStateNum)(0.U.asTypeOf(CoreWork().cloneType))))
  val clearWire = Wire(Bool())
  val gatingWire = Wire(Bool())
  val sha256Core00 = Module(SHA256dCore00())
  val nonceIndicator = WireInit(VecInit(Seq.fill(cores)(false.B)))
  val kernels: Seq[SHA256dKernel] = for (n <- 0 until cores) yield {
    val k = Module(SHA256dKernel())
    k.io.coreId := n.U
    k.io.coreWork := workShifter(n)
    if (asicBoost) {
      k.io.gating.get := gatingWire
    }
    k.io.counter := nonceCount
    k.io.messageBlockIn := sha256Core00.io.messageBlockOut
    nonceIndicator(n) := k.io.nonceValid.get
    k.io.nonce <> DontCare
    k
  }

  nonceCount := nonceCount + (garyCounter === (1.U << (grayWidth - 1)).asUInt()).asUInt()
  gatingWire := garyCounter === 0.U
  clearWire := nonceCount === (0.U(nonceWidth.W) - (128 * 32 * 32).U(nonceWidth.W))
  sha256Core00.io.upstreamWork := io.upstreamWork
  sha256Core00.io.clear := clearWire
  sha256Core00.io.workOut.ready := true.B
  //OHToUInt(nonceIndicator),
  io.nonce.bits := Cat(OHToUInt(nonceIndicator), nonceCount, sha256Core00.io.versionCount)
  io.nonce.valid := Cat(nonceIndicator).orR()
  // sha256Core00.io.versionCount <> DontCare
  io.debugValid := sha256Core00.io.debugValid

  grayReg := grayReg + 1.U
  garyCounter := grayReg ^ (grayReg >> 1).asUInt()
  for (i <- 1 until workShifter.length) {
    workShifter(i) := workShifter(i - 1)
  }
  workShifter(0) := workShifter.last
  // TODO: if core =/= 4 there will be have a problem
  when(sha256Core00.io.workOut.fire()) {
    workShifter(0) := sha256Core00.io.workOut.bits
  }
}

object ASICBoostWrapper {
  def apply()(implicit p: Parameters): ASICBoostWrapper = new ASICBoostWrapper()
}