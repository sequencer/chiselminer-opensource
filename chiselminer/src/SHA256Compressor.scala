package me.sequencer.chiselminer

import chisel3._
import chipsalliance.rocketchip.config.Parameters

class SHA256Compressor(val n: Int, val core: String)(implicit val p: Parameters) extends Module
  with HasSHA256Implementation
  with HasMinerConfig {
  val io = IO(new Bundle {
    val nextMessageBlock = Input(UInt(32.W))
    val whkPreIn = Input(UInt(32.W))
    val whkPreOutReg = Output(UInt(32.W))
    val hashStateIn = Input(Vec(8, UInt(32.W)))
    val hashStateOutReg = Output(Vec(8, UInt(32.W)))
  })

  override def desiredName = s"SHA256Core${core}Compressor${n}"

  val compressResult: CompressOutBundle = Wire(new CompressOutBundle)
  if (n < 63) {
    io.whkPreOutReg := pipelineReg(generateWHK(io.hashStateIn(6), io.nextMessageBlock, n))
  } else {
    io.whkPreOutReg := DontCare
  }
  //
  compressResult := pipelineReg(compress(io.hashStateIn, io.whkPreIn))
  io.hashStateOutReg := VecInit(Seq(
    compressResult.a,
    pipelineReg(io.hashStateIn(0), defaultRegDelay),
    pipelineReg(io.hashStateIn(1), defaultRegDelay),
    pipelineReg(io.hashStateIn(2), defaultRegDelay),
    compressResult.e,
    pipelineReg(io.hashStateIn(4), defaultRegDelay),
    pipelineReg(io.hashStateIn(5), defaultRegDelay),
    pipelineReg(io.hashStateIn(6), defaultRegDelay)
  ))
}

object SHA256Compressor {
  def apply(n: Int, core: String)(implicit p: Parameters): SHA256Compressor = new SHA256Compressor(n, core)
}
