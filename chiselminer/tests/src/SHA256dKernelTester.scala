import chisel3._
import chisel3.iotesters.Driver._
import me.sequencer.chiselminer._
import chisel3.testers._
import chisel3.util._
import org.scalatest._
import chisel3.iotesters._
import chipsalliance.rocketchip.config.Parameters


class SHA256dKernelPeekPokeTester(dut: SHA256dKernel) extends PeekPokeTester(dut) {
  var count: BigInt = 11356146
  poke(dut.io.coreId, 0)
  for(n <- 0 to 63) {
    n match {
      case i if i == 0 => poke(dut.io.messageBlockIn(i), BigInt("965e4132", 16))
      case i if i == 1 => poke(dut.io.messageBlockIn(i), 0)
      case i if i == 2 => poke(dut.io.messageBlockIn(i), 0)
      case i if i == 4 => poke(dut.io.messageBlockIn(i), BigInt("80000000", 16))
      case i if i < 15 => poke(dut.io.messageBlockIn(i), 0)
      case i if i == 15 => poke(dut.io.messageBlockIn(i), 640)
      case i if i == 16 => poke(dut.io.messageBlockIn(i), BigInt("965e4132", 16))
      case i if i == 17 => poke(dut.io.messageBlockIn(i), BigInt("01100000", 16))
      case _ => poke(dut.io.messageBlockIn(n), 0)
    }
  }
  val State64: IndexedSeq[BigInt] = IndexedSeq("38397a0d", "b554c527", "e7503724", "af624635", "a7c65dc0", "d1c0fee0", "27200a3a", "5b1b7f87").map(BigInt(_, 16))
  val State67: IndexedSeq[BigInt] = IndexedSeq("d51ce5c3", "ad4b48cd", "40697407", "38397a0d", "61eba401", "8e31c89f", "beeb976f", "a7c65dc0").map(BigInt(_, 16))
  for(i <- State64.indices) {
    poke(dut.io.coreWork.round64State(i), State64(i))
  }
  for(i <- State67.indices) {
    poke(dut.io.coreWork.round67State(i), State67(i))
  }
  poke(dut.io.counter, count)
  while(count < 11356546) {
    step(1)
    count += 1
    poke(dut.io.counter, count)
  }
}

class SHA256dKernelPeekPokeTest extends org.scalatest.FlatSpec
  with Matchers with ParallelTestExecution {
  implicit val config: Parameters = (new FPGAConfig).toInstance
  "SHA256dKernelPeekPokeTester" should "pass" in {
    val testOptions: TesterOptionsManager = new TesterOptionsManager {
      testerOptions = testerOptions.copy(
        backendName = "vcs",
        isVerbose = true,
        testerSeed = 0,
        generateFsdbOutput = "on"
      )
    }
    chisel3.iotesters.Driver.execute(() => new SHA256dKernel(), testOptions){
      c => new SHA256dKernelPeekPokeTester(c)
    } should be(true)
  }
}