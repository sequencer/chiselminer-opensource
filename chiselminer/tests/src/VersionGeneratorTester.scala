import me.sequencer.chiselminer._
import org.scalatest._
import chisel3.iotesters._
import chipsalliance.rocketchip.config.Parameters


class VersionGeneratorTester(dut: VersionGenerator) extends PeekPokeTester(dut) {
  poke(dut.io.versionIn.original,BigInt("20000000",16))
  poke(dut.io.versionIn.mask,BigInt("1fffe000",16))
  poke(dut.io.versionOut.ready, true)
  for(i <- 0 to 100) {
    step(1)
  }
  peek(dut.io.versionOut)
}

class VersionGeneratorTest extends org.scalatest.FlatSpec
  with Matchers with ParallelTestExecution {
  implicit val config: Parameters = (new FPGAConfig).toInstance
  "ASICBoostPeekPokeTester" should "pass" in {
    val testOptions: TesterOptionsManager = new TesterOptionsManager {
      testerOptions = testerOptions.copy(
        backendName = "vcs",
        isVerbose = true,
        testerSeed = 0,
        generateFsdbOutput = "on"
      )
    }
    chisel3.iotesters.Driver.execute(() => new VersionGenerator(), testOptions){
      c => new VersionGeneratorTester(c)
    } should be(true)
  }
}