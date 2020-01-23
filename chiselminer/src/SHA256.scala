package me.sequencer.chiselminer

import chisel3._
import chisel3.util.ShiftRegister
import chisel3.experimental.doNotDedup
import chipsalliance.rocketchip.config.Parameters

/**
  * TODO: implement these optimizations
  * Definition:
  * upstream:
  * program such as pyminer in linux.
  * core:
  * the atomic calculator of sha256d.
  * sha256Shared:
  * cores shard area for in ASIC chip.
  * space:
  * because of the power domain and limitation of nonce queue's fan-in, cores are located in each rectangle space.
  * sha256Core00:
  * the first part of sha256d's first core, located in upstream
  * sha256Core01:
  * the second part of sha256d's first core, which have 3 pipe which won't change during the nonce++
  * sha256Core1:
  * sha256d's second core, some final stage could be reduced because final hash is worthless.
  * These are designs of the SHA256 cores
  *   1. sha256Core01
  *     1.1. The first 3 stages could stay at upstream/sha256Shared.
  *       1.1.1. upstream:
  * There won't be more space allocate for these 3 pipeline in ASIC chip,
  * however, more register should be reserved in this case.
  *       1.1.2. stared:
  * Hardware could calculate these 3 stages and send to each space.
  *     1.2. Some var could be reduced to constant.
  *       1.2.1. w0, w1, w2 is 3 common round which will be generated in upstream or sha256Shared.
  *       1.2.2. w16, w17, w18, w19 could be pre-calculated and reduce some area in core.
  *   2. sha256Core2
  *     2.1. only need up to 60 round of sha256, and because some data is useless, register can be saved too.
  */

case class SHA256Constant(
                           h: Seq[UInt] = Seq(
                             // Initialize hash states:
                             // first 32 bits of the fractional parts of the square roots of the first 8 primes 2..19:
                             "h6a09e667", "hbb67ae85", "h3c6ef372", "ha54ff53a", "h510e527f", "h9b05688c", "h1f83d9ab", "h5be0cd19").map(_.U(32.W)),
                           k: Seq[UInt] = Seq(
                             // Initialize array of round constants:
                             // first 32 bits of the fractional parts of the cube roots of the first 64 primes 2..311:
                             "h428a2f98", "h71374491", "hb5c0fbcf", "he9b5dba5", "h3956c25b", "h59f111f1", "h923f82a4", "hab1c5ed5",
                             "hd807aa98", "h12835b01", "h243185be", "h550c7dc3", "h72be5d74", "h80deb1fe", "h9bdc06a7", "hc19bf174",
                             "he49b69c1", "hefbe4786", "h0fc19dc6", "h240ca1cc", "h2de92c6f", "h4a7484aa", "h5cb0a9dc", "h76f988da",
                             "h983e5152", "ha831c66d", "hb00327c8", "hbf597fc7", "hc6e00bf3", "hd5a79147", "h06ca6351", "h14292967",
                             "h27b70a85", "h2e1b2138", "h4d2c6dfc", "h53380d13", "h650a7354", "h766a0abb", "h81c2c92e", "h92722c85",
                             "ha2bfe8a1", "ha81a664b", "hc24b8b70", "hc76c51a3", "hd192e819", "hd6990624", "hf40e3585", "h106aa070",
                             "h19a4c116", "h1e376c08", "h2748774c", "h34b0bcb5", "h391c0cb3", "h4ed8aa4a", "h5b9cca4f", "h682e6ff3",
                             "h748f82ee", "h78a5636f", "h84c87814", "h8cc70208", "h90befffa", "ha4506ceb", "hbef9a3f7", "hc67178f2").map(_.U(32.W)))

trait HasSHA256Constant {
  def constant: SHA256Constant = new SHA256Constant
}

trait HasSHA256Implementation extends HasSHA256Constant
  with HasMinerConfig {
  implicit val p: Parameters

  class CompressOutBundle extends Bundle {
    val a = UInt(32.W)
    val e = UInt(32.W)
  }

  class StateOutBundle extends Bundle {
    val lastState = Vec(8, UInt(32.W))
    val whkPreOut = UInt(32.W)
  }

  def rotr(x: UInt, i: Int): UInt = {
    val m = Module(new Module {
      val io = IO(new Bundle {
        val x = Input(UInt(32.W))
        val out = Output(UInt(32.W))
      })

      override def desiredName = "rotr"

      io.out := (io.x >> i).asUInt() | (io.x << (32 - i)).asUInt()
    })
    m.io.x := x
    m.io.out
  }

  def choose(x: UInt, y: UInt, z: UInt): UInt = {
    val m = Module(new Module {
      val io = IO(new Bundle {
        val x = Input(UInt(32.W))
        val y = Input(UInt(32.W))
        val z = Input(UInt(32.W))
        val out = Output(UInt(32.W))
      })

      override def desiredName = "choose"

      io.out := (io.x & io.y) ^ ((~io.x).asUInt() & io.z)
    })
    m.io.x := x
    m.io.y := y
    m.io.z := z
    m.io.out
  }

  def majority(x: UInt, y: UInt, z: UInt): UInt = {
    val m = Module(new Module {
      val io = IO(new Bundle {
        val x = Input(UInt(32.W))
        val y = Input(UInt(32.W))
        val z = Input(UInt(32.W))
        val out = Output(UInt(32.W))
      })

      override def desiredName = "majority"

      io.out := (io.x & io.y) ^ (io.x & io.z) ^ (io.y & io.z)
    })
    m.io.x := x
    m.io.y := y
    m.io.z := z
    m.io.out
  }

  def bsig0(x: UInt): UInt = {
    val m = Module(new Module {
      val io = IO(new Bundle {
        val x = Input(UInt(32.W))
        val out = Output(UInt(32.W))
      })

      override def desiredName = "bsig0"

      io.out := rotr(io.x, 2) ^ rotr(io.x, 13) ^ rotr(io.x, 22)
    })
    m.io.x := x
    m.io.out
  }

  def bsig1(x: UInt): UInt = {
    val m = Module(new Module {
      val io = IO(new Bundle {
        val x = Input(UInt(32.W))
        val out = Output(UInt(32.W))
      })

      override def desiredName = "bsig1"

      io.out := rotr(io.x, 6) ^ rotr(io.x, 11) ^ rotr(io.x, 25)
    })
    m.io.x := x
    m.io.out
  }

  def ssig0(x: UInt): UInt = {
    val m = Module(new Module {
      val io = IO(new Bundle {
        val x = Input(UInt(32.W))
        val out = Output(UInt(32.W))
      })

      override def desiredName = "ssig0"

      io.out := rotr(io.x, 7) ^ rotr(io.x, 18) ^ (io.x >> 3).asUInt()
    })
    m.io.x := x
    m.io.out
  }

  def ssig1(x: UInt): UInt = {
    val m = Module(new Module {
      val io = IO(new Bundle {
        val x = Input(UInt(32.W))
        val out = Output(UInt(32.W))
      })

      override def desiredName = "ssig1"

      io.out := rotr(io.x, 17) ^ rotr(io.x, 19) ^ (io.x >> 10).asUInt()
    })
    m.io.x := x
    m.io.out
  }

  def pipelineReg[T <: Data](input: T, delay: Int = 1): T = {
    val m = Module(new Module {
      val io = IO(new Bundle {
        val d = Input(UInt())
        val q = Output(UInt())
      })

      override def desiredName = s"pipelineReg_delay${delay}}"

      io.q := ShiftRegister(io.d, delay)
    })
    m.io.d := input.asUInt()
    m.io.q.asTypeOf(input)
  }

  // special for split arch
  class CSAOutBundle extends Bundle {
    val s: UInt = UInt(32.W)
    val c: UInt = UInt(32.W)
  }

  def csa(a: UInt, b: UInt, c: UInt): CSAOutBundle = {
    val m = Module(new Module {
      val io = IO(new Bundle {
        val a = Input(UInt(32.W))
        val b = Input(UInt(32.W))
        val c = Input(UInt(32.W))
        val out = Output(new CSAOutBundle)
      })

      override def desiredName = "csa"

      io.out.s := io.a ^ io.b ^ io.c
      io.out.c := ((io.a & io.b) | (io.b & io.c) | (io.c & io.a)) << 1.U
    })
    //    doNotDedup(m)
    m.io.a := a
    m.io.b := b
    m.io.c := c
    m.io.out
  }

  def csa(csaPre: CSAOutBundle, c: UInt): CSAOutBundle = csa(csaPre.s, csaPre.c, c)

  def csaDual(a: UInt, b: UInt, c: UInt, d: UInt): Vec[CSAOutBundle] = {
    val m = Module(new Module {
      val io = IO(new Bundle {
        val a = Input(UInt(32.W))
        val b = Input(UInt(32.W))
        val c = Input(UInt(32.W))
        val d = Input(UInt(32.W))
        val out = Output(Vec(2, new CSAOutBundle))
      })
      val sShare: UInt = Wire(UInt(32.W))
      val cShare: UInt = Wire(UInt(32.W))
      sShare := io.a ^ io.b
      cShare := io.a & io.b
      io.out(0).c := (cShare | (io.b & io.c) | (io.a & io.c)) << 1.U
      io.out(0).s := sShare ^ io.c
      io.out(1).c := (cShare | (io.b & io.d) | (io.a & io.d)) << 1.U
      io.out(1).s := sShare ^ io.d

      override def desiredName = "csaDual"
    })
    m.io.a := a
    m.io.b := b
    m.io.c := c
    m.io.d := d
    m.io.out
  }

  def fullAdder(a: UInt, b: UInt, myName: String): UInt = {
    val m = Module(adder(myName))
    m.io.a := a
    m.io.b := b
    m.io.out
  }

  def fullAdder(csaPre: CSAOutBundle, name: String): UInt = fullAdder(csaPre.s, csaPre.c, name)

  def defaultRegDelay = if (splitPipe) 2 else 1

  def noSplitGenerateWHK(g: UInt, nextMessageBlock: UInt, n: Int): UInt = {
    val m = Module(new Module {
      val io = IO(new Bundle {
        val g = Input(UInt(32.W))
        val nextMessageBlock = Input(UInt(32.W))
        val out = Output(UInt(32.W))
      })

      override def desiredName = "WHKPreAdder"

      io.out := io.g + constant.k(n + 1) + io.nextMessageBlock
    })
    m.io.g := g
    m.io.nextMessageBlock := nextMessageBlock
    m.io.out
  }

  def noSplitCompress(hashState: Vec[UInt], whkPre: UInt): CompressOutBundle = {
    val m = Module(new Module {
      val io = IO(new Bundle {
        val hashStateIn = Input(Vec(8, UInt(32.W)))
        val whkPreIn = Input(UInt(32.W))
        val out = Output(new CompressOutBundle)
      })

      override def desiredName = "compress"

      val t1Wire: UInt = Wire(UInt(32.W))
      val t2Wire: UInt = Wire(UInt(32.W))
      t1Wire := bsig1(io.hashStateIn(4)) + choose(io.hashStateIn(4), io.hashStateIn(5), io.hashStateIn(6)) + io.whkPreIn
      t2Wire := bsig0(io.hashStateIn(0)) + majority(io.hashStateIn(0), io.hashStateIn(1), io.hashStateIn(2))
      io.out.a := t1Wire + t2Wire
      io.out.e := io.hashStateIn(3) + t1Wire
    })
    m.io.hashStateIn := hashState
    m.io.whkPreIn := whkPre
    m.io.out
  }

  def noSplitExtend(s1: UInt, s0: UInt, i0: UInt, i1: UInt): UInt = {
    val m = Module(new Module {
      val io = IO(new Bundle {
        val s1 = Input(UInt(32.W))
        val s0 = Input(UInt(32.W))
        val i0 = Input(UInt(32.W))
        val i1 = Input(UInt(32.W))
        val out = Output(UInt(32.W))
      })

      override def desiredName = "extend"

      io.out := ssig1(io.s1) + ssig0(io.s0) + io.i0 + io.i1
    })
    m.io.s1 := s1
    m.io.s0 := s0
    m.io.i0 := i0
    m.io.i1 := i1
    m.io.out
  }

  def noSplitAddState(firstState: Vec[UInt], lastState: Vec[UInt]): StateOutBundle = {
    val m = Module(new Module {
      val io = IO(new Bundle {
        val firstState = Input(Vec(8, UInt(32.W)))
        val lastState = Input(Vec(8, UInt(32.W)))
        val out = Output(new StateOutBundle)
      })

      override def desiredName = "stateAdder"

      io.out.lastState := VecInit((io.firstState zip io.lastState).map { case (x: UInt, y: UInt) => x + y })
      io.out.whkPreOut := generateWHK(constant.h(7), io.out.lastState(0), -1)
    })
    m.io.firstState := firstState
    m.io.lastState := lastState
    m.io.out
  }

  //
  def splitGenerateWHK(g: UInt, nextMessageBlock: UInt, n: Int): UInt = {
    val m = Module(new Module {
      val io = IO(new Bundle {
        val g = Input(UInt(32.W))
        val nextMessageBlock = Input(UInt(32.W))
        val out = Output(UInt(32.W))
      })

      override def desiredName = "generateWHK"

      io.out := fullAdder(pipelineReg(csa(io.g, constant.k(n + 1), io.nextMessageBlock)), "Slow")
    })
    m.io.g := g
    m.io.nextMessageBlock := nextMessageBlock
    m.io.out
  }

  def splitCompress(hashState: Vec[UInt], whkPre: UInt): CompressOutBundle = {
    val m = Module(new Module {
      val io = IO(new Bundle {
        val hashStateIn = Input(Vec(8, UInt(32.W)))
        val whkPreIn = Input(UInt(32.W))
        val out = Output(new CompressOutBundle)
      })

      override def desiredName = "compress"

      val t1Wire: CSAOutBundle = Wire(new CSAOutBundle)
      val csaShare: Vec[CSAOutBundle] = Wire(Vec(2, new CSAOutBundle))
      csaShare := pipelineReg(csaDual(t1Wire.c, t1Wire.s, bsig0(io.hashStateIn(0)), io.hashStateIn(3)))
      t1Wire := csa(bsig1(io.hashStateIn(4)), choose(io.hashStateIn(4), io.hashStateIn(5), io.hashStateIn(6)), io.whkPreIn)
      io.out.a := fullAdder(csa(csaShare(0), pipelineReg(majority(io.hashStateIn(0), io.hashStateIn(1), io.hashStateIn(2)))), "Fast")
      io.out.e := fullAdder(csaShare(1), "Slow")
    })
    m.io.hashStateIn := hashState
    m.io.whkPreIn := whkPre
    m.io.out
  }

  def splitExtend(s1: UInt, s0: UInt, i0: UInt, i1: UInt): UInt = {
    val m = Module(new Module {
      val io = IO(new Bundle {
        val s1 = Input(UInt(32.W))
        val s0 = Input(UInt(32.W))
        val i0 = Input(UInt(32.W))
        val i1 = Input(UInt(32.W))
        val out = Output(UInt(32.W))
      })

      override def desiredName = "extend"

      io.out := fullAdder(pipelineReg(csa(csa(ssig1(io.s1), ssig0(io.s0), io.i0), io.i1), 1), "Slow")
    })
    m.io.s1 := s1
    m.io.s0 := s0
    m.io.i0 := i0
    m.io.i1 := i1
    m.io.out
  }

  def splitAddState(firstState: Vec[UInt], lastState: Vec[UInt]): StateOutBundle = {
    val m = Module(new Module {
      val io = IO(new Bundle {
        val firstState = Input(Vec(8, UInt(32.W)))
        val lastState = Input(Vec(8, UInt(32.W)))
        val out = Output(new StateOutBundle)
      })

      override def desiredName = "stateAdder"

      val lastState = Wire(io.out.lastState.cloneType)
      lastState := VecInit((io.firstState zip io.lastState).map { case (x: UInt, y: UInt) => fullAdder(x, y, "Slow") })
      io.out.lastState := pipelineReg(lastState)
      io.out.whkPreOut := generateWHK(constant.h(7), lastState(0), -1)
    })
    m.io.firstState := firstState
    m.io.lastState := lastState
    m.io.out
  }

  // 32 bit 3 input with 1 constant
  def generateWHK(g: UInt, nextMessageSchedule: UInt, n: Int): UInt =
    if (splitPipe) splitGenerateWHK(g, nextMessageSchedule, n)
    else noSplitGenerateWHK(g, nextMessageSchedule, n)

  def compress(hashState: Vec[UInt], whkPre: UInt): CompressOutBundle =
    if (splitPipe) splitCompress(hashState, whkPre)
    else noSplitCompress(hashState, whkPre)

  // 32 bit 4 input
  def extend(s1: UInt, s0: UInt, i0: UInt, i1: UInt): UInt =
    if (splitPipe) splitExtend(s1, s0, i0, i1)
    else noSplitExtend(s1, s0, i0, i1)

  def addState(firstState: Vec[UInt], lastState: Vec[UInt]): StateOutBundle =
    if (splitPipe) splitAddState(firstState, lastState)
    else noSplitAddState(firstState, lastState)
}
