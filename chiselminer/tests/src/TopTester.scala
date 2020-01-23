import me.sequencer.chiselminer._
import org.scalatest._
import chisel3.iotesters._
import chipsalliance.rocketchip.config.Parameters

//class TopTester(dut: FPGATop) extends PeekPokeTester(dut) {
//  poke(dut.io.tx.ready, true)
//  poke(dut.io.rx.valid, true)
//  poke(dut.io.rx.bits, 2)
//  step(1)
//  poke(dut.io.rx.bits, 0)
//  step(5)
//  poke(dut.io.rx.bits, 2)
//  step(1)
//  poke(dut.io.rx.bits, 0x8e)
//  step(1)
//  poke(dut.io.rx.bits, 0xfb)
//  step(1)
//  poke(dut.io.rx.valid, false)
//  step(200)
//  peek(dut.io.tx.bits)
//  poke(dut.io.rx.valid, true)
//  poke(dut.io.rx.bits, 1)
//  step(1)
//  poke(dut.io.rx.bits, 2)
//  step(1)
//  poke(dut.io.rx.bits, 0)
//  step(5)
//  poke(dut.io.rx.bits, 0xc2)
//  step(1)
//  poke(dut.io.rx.bits, 0xef)
//  step(1)
//  poke(dut.io.rx.valid, false)
//  step(500)
//}
class TopTester(dut: FPGATop) extends PeekPokeTester(dut) {
  step(2000)
}

class TopTest extends org.scalatest.FlatSpec
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
    chisel3.iotesters.Driver.execute(() => new FPGATop(), testOptions){
      c => new TopTester(c)
    } should be(true)
  }
}