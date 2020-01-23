package me.sequencer.chiselminer

import chisel3._
import firrtl._
import firrtl.transforms.NoCircuitDedupAnnotation
import chipsalliance.rocketchip.config._
import logger.LogLevel

object SHA256dKernelEmitter extends App {
  implicit val config: Parameters = (new FPGAConfig).toInstance
  val optionsManager = new ExecutionOptionsManager("chisel3")
    with HasFirrtlOptions
    with HasChiselExecutionOptions {
    commonOptions = CommonOptions(
      targetDirName = "./SHA256dKernel",
      logToFile = true,
      globalLogLevel = LogLevel.Info
    )
    chiselOptions = ChiselExecutionOptions()
    firrtlOptions = FirrtlExecutionOptions(
      compilerName = "low",
      annotations = List{NoCircuitDedupAnnotation},
    )
  }

  chisel3.Driver.execute(optionsManager, () => SHA256dKernel())
}