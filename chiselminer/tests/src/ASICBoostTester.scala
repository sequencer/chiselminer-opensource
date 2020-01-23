import me.sequencer.chiselminer._
import org.scalatest._
import chisel3.iotesters._
import chipsalliance.rocketchip.config.Parameters

class ASICBoostPeekPokeTester(dut: ASICBoostWrapper) extends PeekPokeTester(dut) {
  var count: BigInt = 138358000
  poke(dut.io.upstreamWork.version.original,BigInt("00000020",16))
  poke(dut.io.upstreamWork.version.mask,BigInt("00e0ff1f",16))
  val prevHash: IndexedSeq[BigInt] = IndexedSeq("79de294d", "bce837c7", "328b5366", "5bcc84ee", "a878211d", "08d11d00", "00000000", "00000000").map(BigInt(_, 16))
  val merkleRoot: IndexedSeq[BigInt] = IndexedSeq("47845da4", "2411866c", "e42b55f7", "c9539591", "76f5433d", "c470262e", "dccbf6b8", "8476a7fb").map(BigInt(_, 16))

  for(i <- prevHash.indices) {
	poke(dut.io.upstreamWork.prevHash(i),prevHash(i))
  }
  for(i <- merkleRoot.indices) {
	poke(dut.io.upstreamWork.merkleRoot(i),merkleRoot(i))
  }
  poke(dut.io.upstreamWork.nTime,BigInt("2facbe5c",16))
  poke(dut.io.upstreamWork.nBits,BigInt("114e2c17",16))
  while(count < 138358900) {
    step(100)
    count += 100
  }
  // nonce 883f2dc5
}

class ASICBoostPeekPokeTest extends org.scalatest.FlatSpec
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
	chisel3.iotesters.Driver.execute(() => new ASICBoostWrapper(), testOptions){
	  c => new ASICBoostPeekPokeTester(c)
	} should be(true)
  }
}