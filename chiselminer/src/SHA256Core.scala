package me.sequencer.chiselminer

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.asyncqueue._

class Version extends Bundle {
  val original = UInt(32.W)
  val mask = UInt(32.W)
}

object Version {
  def apply(): Version = new Version()
}

class VersionGenerator extends Module {
  val io = IO(new Bundle {
    val versionIn = Input(Version())
    val versionOut = Decoupled(UInt(32.W))
    val versionCount = Output(UInt(32.W))
  })
  val counter = RegInit(0.U(32.W))
  val nextCounter = Wire(UInt(32.W))
  val index = RegInit(0.U(log2Ceil(32).W))
  val paddingCount = RegInit(0.U(log2Ceil(32).W))
  val counterCopy = RegInit(0.U(32.W))
  val maskedCounter = RegInit(VecInit(Seq.fill(32)(0.U(1.W))))
  val freeBit = WireInit(0.U(log2Ceil(32).W))

  io.versionCount := counter
  nextCounter := counter + 1.U
  io.versionOut.valid := paddingCount === freeBit
  io.versionOut.bits := ((~io.versionIn.mask).asUInt() & io.versionIn.original) | maskedCounter.asUInt()

  freeBit := PopCount(io.versionIn.mask)
  when(io.versionOut.fire()) {
    counter := nextCounter
    counterCopy := nextCounter
    paddingCount := 0.U
  }
  when(!io.versionOut.valid) {
    when(io.versionIn.mask(index) === 1.U) {
      maskedCounter(index) := counterCopy(0)
      paddingCount := paddingCount + 1.U
      counterCopy := counterCopy >> 1
    }
    index := index + 1.U
  }
}

class CoreWorkPool()(implicit val p: Parameters) extends Module with HasMinerConfig {
  val pipeSplit: Int = if (splitPipe) 2 else 1
  val io = IO(new Bundle {
    val coreWorkIn = Flipped(CoreWorkIO())
    val clear = Input(Bool())
    val workOut = Decoupled(CoreWork())
    val debugValid = Output(Bool())
  })
  val coreWorkPoolProcessingReg = RegInit(VecInit(Seq.fill(midStateNum)(0.U.asTypeOf(CoreWork().cloneType))))
  val coreWorkCounter = RegInit(0.U((log2Ceil(midStateNum) + 1).W))
  val coreWorkPending = RegInit(0.U((log2Ceil(midStateNum) + 1).W))
  val coreWorkPoolReg = RegInit(VecInit(Seq.fill(midStateNum)(0.U.asTypeOf(CoreWork().cloneType))))
  val debugValid = RegInit(false.B)
  debugValid := false.B
  io.debugValid := debugValid
  io.coreWorkIn.round64State.ready := coreWorkCounter < midStateNum.U
  io.coreWorkIn.round67State.ready := coreWorkCounter < midStateNum.U
  when(io.coreWorkIn.round64State.fire()) {
    for (i <- coreWorkPoolProcessingReg.indices) {
      coreWorkPoolProcessingReg(i).round64State := {
        i match {
          case k if k == 0 => io.coreWorkIn.round64State.bits
          case _ => coreWorkPoolProcessingReg(i - 1).round64State
        }
      }
    }
  }
  when(io.coreWorkIn.round67State.fire()) {
    coreWorkCounter := coreWorkCounter + 1.U
    for (i <- coreWorkPoolProcessingReg.indices) {
      coreWorkPoolProcessingReg(i).round67State := {
        i match {
          case k if k == 0 => io.coreWorkIn.round67State.bits
          case _ => coreWorkPoolProcessingReg(i - 1).round67State
        }
      }
    }
  }

  when(io.clear) {
    coreWorkCounter := 0.U
  }
  when(coreWorkCounter === midStateNum.U) {
    coreWorkCounter := coreWorkCounter + 1.U
    // Initial state requires offset
    for (k <- coreWorkPoolReg.indices) {
      var n = k - 3 * pipeSplit
      while (n < 0) {
        n = n + midStateNum
      }
      coreWorkPoolReg(k).round67State := coreWorkPoolProcessingReg(k).round67State
      coreWorkPoolReg(k).round64State := coreWorkPoolProcessingReg(n).round64State
    }
    coreWorkPending := midStateNum.U
    debugValid := true.B
  }
  when(coreWorkPending =/= 0.U) {
    coreWorkPending := coreWorkPending - 1.U
    for (i <- coreWorkPoolReg.indices) {
      coreWorkPoolReg(i) := {
        i match {
          case k if k == 0 => coreWorkPoolReg.last
          case _ => coreWorkPoolReg(i - 1)
        }
      }
    }
  }
  io.workOut.bits := coreWorkPoolReg.head
  io.workOut.valid := coreWorkPending =/= 0.U
}

class UpstreamWork extends Bundle {
  val version = Version()
  val prevHash = Vec(8, UInt(32.W))
  val merkleRoot = Vec(8, UInt(32.W))
  val nTime = UInt(32.W)
  val nBits = UInt(32.W)
}

class SHA256dCore00()(implicit val p: Parameters) extends Module
  with HasSHA256Implementation
  with HasMinerConfig {
  val io = IO(new Bundle {
    val upstreamWork = Input(new UpstreamWork)
    val clear = Input(Bool())
    val workOut = Decoupled(CoreWork())
    val messageBlockOut = Output(Vec(64, UInt(32.W)))
    val debugValid = Output(Bool())
    val versionCount = Output(UInt(32.W))
  })
  val versionGenerator = Module(new VersionGenerator)
  val microCore = Module(new SHA256MicroCore)
  val coreWorkPool = Module(new CoreWorkPool())
  io.workOut <> coreWorkPool.io.workOut

  coreWorkPool.io.clear := io.clear
  coreWorkPool.io.coreWorkIn <> microCore.io.stateOut
  io.debugValid := coreWorkPool.io.debugValid

  for (i <- 0 until 19) {
    microCore.io.messageBlockIn.bits(i) := {
      i match {
        case k if k == 0 => versionGenerator.io.versionOut.asUInt()
        case k if k < 9 => io.upstreamWork.prevHash(i - 1)
        case k if k < 17 => io.upstreamWork.merkleRoot(k - 9)
        case k if k == 17 => io.upstreamWork.nTime.asUInt()
        case k if k == 18 => io.upstreamWork.nBits.asUInt()
      }
    }
  }
  microCore.io.messageBlockIn.valid := versionGenerator.io.versionOut.valid

  io.versionCount := versionGenerator.io.versionCount
  versionGenerator.io.versionIn := io.upstreamWork.version
  versionGenerator.io.versionOut.ready := microCore.io.messageBlockIn.ready
  for (n <- 0 to 63) {
    io.messageBlockOut(n) := {
      n match {
        case i if i == 0 => io.upstreamWork.merkleRoot(7)
        case i if i == 1 => io.upstreamWork.nTime
        case i if i == 2 => io.upstreamWork.nBits
        case i if i == 4 => "h80000000".U(32.W)
        case i if i < 15 => 0.U(32.W)
        case i if i == 15 => 640.U(32.W)
        case i if Seq(16, 17).contains(i) => extend(io.messageBlockOut(i - 2), io.messageBlockOut(i - 15), io.messageBlockOut(i - 7), io.messageBlockOut(i - 16))
        case _ => DontCare
      }
    }
  }
}

class SHA256Core01()(implicit val p: Parameters) extends Module
  with HasMinerConfig {
  val io = IO(new Bundle {
    val coreWork = Input(CoreWork())
    val messageBlockIn = Input(Vec(64, UInt(32.W)))
    val whkPreIn = Input(UInt(32.W))
    val gating = if (asicBoost) Some(Input(Bool())) else None
    val hashStateOutReg = Output(Vec(8, UInt(32.W)))
    val whkPreOutReg = Output(UInt(32.W))
  })
  val sha256PipeLine = Module(SHA256PipeLineCore("01", phantom = true))
  sha256PipeLine.io.coreWorkIn := io.coreWork
  sha256PipeLine.io.messageBlockIn := io.messageBlockIn
  sha256PipeLine.io.whkPreIn := io.whkPreIn
  if (asicBoost) {
    sha256PipeLine.io.gatingIn.get := io.gating.get
  }
  io.hashStateOutReg := sha256PipeLine.io.hashStateOutReg
  io.whkPreOutReg := sha256PipeLine.io.whkPreOutReg
}

class SHA256Core1()(implicit val p: Parameters) extends Module
  with HasSHA256Constant {
  val io = IO(new Bundle {
    val stateIn = Input(Vec(8, UInt(32.W)))
    val whkPreIn = Input(UInt(32.W))
    val nonceValid = Output(Bool())
  })
  val sha256PipeLine = Module(SHA256PipeLineCore("1", phantom = true))
  io.nonceValid := false.B
  sha256PipeLine.io.coreWorkIn.round64State := constant.h
  sha256PipeLine.io.coreWorkIn.round67State := DontCare
  sha256PipeLine.io.messageBlockIn := DontCare
  for (i <- 0 to 15) {
    sha256PipeLine.io.messageBlockIn(i) := {
      i match {
        case j if j < 8 => io.stateIn(i)
        case j if j == 8 => "h80000000".U(32.W)
        case j if j < 15 => 0.U(32.W)
        case j if j == 15 => 256.U(32.W)
      }
    }
  }
  sha256PipeLine.io.whkPreIn := io.whkPreIn
  when(!sha256PipeLine.io.verifyState.orR()) {
    io.nonceValid := true.B
  }
}

class SHA256dKernel()(implicit val p: Parameters) extends Module
  with HasSHA256Implementation
  with HasMinerConfig {
  val coreIdWidth = log2Ceil(cores)
  val io = IO(new Bundle {
    val coreId = Input(UInt(coreIdWidth.W))
    val coreWork = Input(CoreWork())
    val gating = if (asicBoost) Some(Input(Bool())) else None
    val counter = Input(UInt((32 - coreIdWidth).W))
    val messageBlockIn = Input(Vec(64, UInt(32.W)))
    val nonceValid = if (dft) Some(Output(Bool())) else None
    val nonce = new CrossingIO(UInt(32.W))
  })
  val sha256Core01 = Module(new SHA256Core01)
  val sha256Core1 = Module(new SHA256Core1)
  val nonce = Wire(UInt(32.W))
  nonce := Cat(io.coreId, io.counter)
  if (dft) {
    io.nonceValid.get := sha256Core1.io.nonceValid
  }
  sha256Core01.io.coreWork := io.coreWork
  for(n <- 0 to 63) {
    sha256Core01.io.messageBlockIn(n) := {
      n match {
        case i if Seq(0, 1).contains(i) => DontCare
        case i if Seq(2, 16, 17).contains(i) => io.messageBlockIn(i)
        case i if i == 4 => "h80000000".U(32.W)
        case i if i < 15 => 0.U(32.W)
        case i if i == 15 => 640.U(32.W)
        case _ => DontCare
      }
    }
  }
  val nonceWire: Some[UInt] =if (splitPipe) Some(pipelineReg(nonce)) else Some(nonce)
  sha256Core01.io.messageBlockIn(3) := nonceWire.get
  val whkWire: Some[UInt] = if (splitPipe) Some(pipelineReg(noSplitGenerateWHK(io.coreWork.round67State(7), nonce, 2))) else Some(noSplitGenerateWHK(io.coreWork.round67State(7), nonce, 2))
  sha256Core01.io.whkPreIn := whkWire.get
  if (asicBoost) {
    sha256Core01.io.gating.get := io.gating.get
  }
  sha256Core1.io.stateIn := sha256Core01.io.hashStateOutReg
  sha256Core1.io.whkPreIn := sha256Core01.io.whkPreOutReg

  // async input from previous SHA256dKernel(need sync)
  val nonceQueue = Module(new AsyncQueue(UInt(32.W), AsyncQueueParams(2, 2, safe = false, narrow = false)))
  nonceQueue.io.enq_clock := io.nonce.enq_clock
  nonceQueue.io.enq_reset := io.nonce.enq_reset
  nonceQueue.io.enq <> io.nonce.enq
  nonceQueue.io.deq_reset := reset
  nonceQueue.io.deq_clock := clock

  // async input from SHA256Core1(no need to sync)
  val localNonce = Wire(Decoupled(UInt(32.W)))
  localNonce.bits := nonce
  localNonce.valid := sha256Core1.io.nonceValid
  val localNonceArbiter = Module(new Arbiter(UInt(32.W), 2))
  // nonceIn must be synced by AsyncFIFO
  localNonceArbiter.io.in(0) <> nonceQueue.io.deq
  // localNonce is ok with async
  localNonceArbiter.io.in(1) <> localNonce

  io.nonce.deq <> localNonceArbiter.io.out
}

object SHA256dCore00 {
  def apply()(implicit p: Parameters): SHA256dCore00 = new SHA256dCore00()
}

object SHA256dKernel {
  def apply()(implicit p: Parameters): SHA256dKernel = new SHA256dKernel()(p)
}
