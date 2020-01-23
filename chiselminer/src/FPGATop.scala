package me.sequencer.chiselminer
import chisel3._
import chisel3.util._
import me.sequencer.chiselblocks._
import chipsalliance.rocketchip.config._

class FPGATop(implicit val p: Parameters) extends Module with HasMinerConfig {
  val io = IO(new Bundle{
    val rx = Input(Bool())
    val tx = Output(Bool())
  })
  val rxModule = Module(UARTRx(uartParams))
  rxModule.io.in := io.rx
  val txModule = Module(UARTTx(uartParams))
  io.tx := txModule.io.out
  val receive = Module(new RXReceive())
  receive.io.rx <> rxModule.io.out
  val regFile: RegFile = {
    val r = Module(new RegFile())
    r.io.raddr := receive.io.raddr
    receive.io.rdata := r.io.rdata
    r.io.wen := receive.io.wen
    r.io.waddr := receive.io.waddr
    r.io.wdata := receive.io.wdata
    receive.io.chipId := r.io.dataOut(0)
    r
  }
  val asicBoostWrapper: ASICBoostWrapper = {
    val s = Module(new ASICBoostWrapper())
    s.io.upstreamWork.version.original := regFile.io.dataOut(1)
    s.io.upstreamWork.version.mask := regFile.io.dataOut(2)
    s.io.upstreamWork.prevHash := regFile.io.dataOut.slice(3,11)
    s.io.upstreamWork.nTime := regFile.io.dataOut(11)
    s.io.upstreamWork.nBits := regFile.io.dataOut(12)
    s.io.upstreamWork.merkleRoot := regFile.io.dataOut.slice(21,29)
    s.io.debugValid <> DontCare
    s
  }
  val nonceHandle: NonceHandle = {
    val n = Module(new NonceHandle())
    n.io.nonce <> asicBoostWrapper.io.nonce
    n
  }
  val arbiter = {
    val a = Module(new TXArbiter(UInt(8.W), 2, idleCycles))
    a.io.input(0) <> receive.io.tx
    a.io.input(1) <> nonceHandle.io.out
    a.io.freed(0) := receive.io.freed
    a.io.freed(1) := nonceHandle.io.freed
    txModule.io.in <> a.io.out
    a
  }
}

class TXArbiter[T <: Data](gen: T, n: Int, idleCycles: BigInt = 0) extends Module {
  val io = IO(new Bundle {
    val input = Vec(n, Flipped(Decoupled(gen)))
    val out = Decoupled(gen)
    val freed = Input(Vec(n, Bool()))
  })
  val chosen = RegInit(0.U(log2Ceil(n).W))
  val waiting::transmission::idle::Nil = Enum(3)
  val state = RegInit(waiting)
  val idleReg = RegInit(0.U(log2Ceil(idleCycles).W))
  io.out.bits := DontCare
  io.out.valid := false.B
  for(i <- 0 until n) {
    io.input(i).ready := false.B
  }
  when(state === waiting) {
    for(i <- 0 until n) {
      when(io.input(i).valid) {
        state := transmission
        chosen := i.asUInt()
      }
    }
  }.elsewhen(state === transmission) {
    io.out <> io.input(chosen)
    when(io.freed.reduceLeft(_ || _)) {
      state := idle
      idleReg := idleCycles.U
    }
  }.otherwise {
    idleReg := idleReg - 1.U
    when(idleReg === 0.U) {
      state := waiting
    }
  }

}

class NonceHandle(implicit val p: Parameters) extends Module with HasMinerConfig {
  val io = IO(new Bundle {
    val nonce = Flipped(Decoupled(UInt(64.W)))
    val out = Decoupled(UInt(8.W))
    val freed = Output(Bool())
  })
  io.freed := false.B
  val nonceQueue = Queue(io.nonce, 2)
  val nonceShifter: Vec[UInt] = RegInit(VecInit(Seq.fill(9)(0.U(8.W))))
  val shiftCount = RegInit(0.U(4.W))
  io.out.valid := shiftCount =/= 0.U
  io.out.bits := nonceShifter(0)
  nonceQueue.ready := shiftCount === 0.U
  when(nonceQueue.fire()) {
    nonceShifter := Cat(4.U(8.W),nonceQueue.bits).asTypeOf(nonceShifter.cloneType).reverse
    shiftCount := 9.U
  }
  when(io.out.fire()) {
    when(shiftCount === 1.U) {
      io.freed := true.B
    }
    shiftCount := shiftCount - 1.U
    for(i <- nonceShifter.indices) {
      nonceShifter(i) := {
        i match {
          case k if k == nonceShifter.length - 1 => DontCare
          case _ => nonceShifter(i + 1)
        }
      }
    }
  }
}

class RegFileIO(implicit val p: Parameters)  extends Bundle with HasMinerConfig {
  val addressWidth = log2Ceil(regNum)
  val raddr = Input(UInt(addressWidth.W))
  val rdata = Output(UInt(32.W))
  val wen    = Input(Bool())
  val waddr  = Input(UInt(addressWidth.W))
  val wdata  = Input(UInt(32.W))
  val dataOut = Output(Vec(regNum, UInt(32.W)))
}

class RegFile(implicit val p: Parameters) extends Module with HasMinerConfig {
  val io = IO(new RegFileIO)
  val regs = RegInit(VecInit(Seq.fill(regNum)(0.U(32.W))))
  io.rdata := regs(io.raddr)
  io.dataOut := regs
  when(io.wen) {
    regs(io.waddr) := io.wdata
    when(io.waddr === 20.U) {
      for(i <- 0 to 7) {
        regs((21 + i).U) := {
          i match {
            case k if k == 7 => io.wdata
            case _ => regs((13 + i).U)
          }
        }
      }
    }
  }
}

class RXReceive()(implicit val p: Parameters) extends Module with HasMinerConfig {
  val addressWidth = log2Ceil(regNum)
  val io = IO(new Bundle {
    val raddr = Output(UInt(addressWidth.W))
    val rdata = Input(UInt(32.W))
    val wen    = Output(Bool())
    val waddr  = Output(UInt(addressWidth.W))
    val wdata  = Output(UInt(32.W))
    val rx = Flipped(Decoupled(UInt(8.W)))
    val tx = Decoupled(UInt(8.W))
    val chipId = Input(UInt(8.W))
    val freed = Output(Bool())
  })
  io.waddr := DontCare
  io.raddr := DontCare
  io.wdata := DontCare
  io.freed := false.B
  val wen = WireInit(false.B)
  io.wen := wen
  val initialization::receive::handle::Nil = Enum(3)
  val state = RegInit(initialization)
  val rxShifter = RegInit(VecInit(Seq.fill(9)(0.U(8.W))))
  val txShifter = RegInit(VecInit(Seq.fill(9)(0.U(8.W))))
  val crcSource = RegInit(VecInit(Seq.fill(7)(0.U(8.W))))
  val txCount = RegInit(0.U(4.W))
  io.tx.valid := txCount =/= 0.U
  io.tx.bits := txShifter(0)
  val typeWire: UInt = rxShifter(0)
  val targetChipId: UInt = rxShifter(1)
  val regAddress: UInt = rxShifter(2)
  val data: UInt = rxShifter.slice(3, 7).reduceLeft(Cat(_,_))
  val IdleReg = RegInit(0.U((log2Ceil(idleCycles) + 1).W))
  val needCrc = RegInit(false.B)
  val crcCount = RegInit(0.U(4.W))
  val crcModule: CRCCheck = {
    val crc = Module(CRCCheck(width = 16, poly = 4129, init = 65535, xorOut = 0, inputReverse = false))
    crc.io.output.ready := false.B
    crc.io.input.bits := DontCare
    crc.io.input.valid := false.B
    crc
  }
  io.rx.ready := state =/= handle
  when(io.tx.fire()) {
    when(txCount === 1.U) {
      io.freed := true.B
    }
    for(i <- txShifter.indices) {
      txShifter(i) := {
        i match {
          case k if k == txShifter.length - 1 => DontCare
          case _ => txShifter(i + 1)
        }
      }
    }
    txCount := txCount - 1.U
  }

  when(needCrc) {
    when(crcCount === 7.U) {
      when(txCount === 0.U) {
        crcModule.io.output.ready := true.B
        when(crcModule.io.output.fire()) {
          needCrc := false.B
          for(i <- txShifter.indices) {
            txShifter(i) := {
              i match {
                case k if k == 7 => crcModule.io.output.bits(15, 8)
                case k if k == 8 => crcModule.io.output.bits(7, 0)
                case _ => crcSource(i)
              }
            }
          }
          txCount := 9.U
        }
      }
    }.otherwise{
      when(crcModule.io.input.ready) {
        crcModule.io.input.enq(crcSource(0))
        for (i <- crcSource.indices) {
          crcSource(i) := {
            i match {
              case k if k == crcSource.length - 1 => crcSource.head
              case _ => crcSource(i + 1)
            }
          }
        }
        crcCount := crcCount + 1.U
      }
    }
  }

  when(state === initialization) {
    when(io.rx.fire()) {
      state := receive
      rxShifter.last := io.rx.bits
    }
  }.elsewhen(state === receive) {
    IdleReg := IdleReg + 1.U
    when(IdleReg === idleCycles.U) {
      state := handle
    }
    when(io.rx.fire()) {
      IdleReg := 0.U
      for(i <- rxShifter.indices) {
        rxShifter(i) := {
          i match {
            case k if k == 8 => io.rx.bits
            case _ => rxShifter(i + 1)
          }
        }
      }
    }
  }.elsewhen(state === handle) {
    state := initialization
    when(typeWire === 1.U) {
      // read regs
      when(targetChipId === io.chipId) {
        io.raddr := regAddress
        for(i <- crcSource.indices) {
          crcSource(i) := {
            i match {
              case k if k == 0 => 3.U
              case k if k == 1 => io.chipId
              case k if k == 2 => regAddress
              case k if k == 3 => io.rdata(31,24)
              case k if k == 4 => io.rdata(23,16)
              case k if k == 5 => io.rdata(15, 8)
              case k if k == 6 => io.rdata(7, 0)
            }
          }
        }
        needCrc := true.B
        crcCount := 0.U
      }.elsewhen(targetChipId === 255.U) {
        io.raddr := regAddress
        for(i <- crcSource.indices) {
          crcSource(i) := {
            i match {
              case k if k == 0 => 3.U
              case k if k == 1 => io.chipId
              case k if k == 2 => regAddress
              case k if k == 3 => io.rdata(31,24)
              case k if k == 4 => io.rdata(23,16)
              case k if k == 5 => io.rdata(15, 8)
              case k if k == 6 => io.rdata(7, 0)
            }
          }
        }
        needCrc := true.B
        crcCount := 0.U
        txShifter := rxShifter
        txCount := 9.U
      }.otherwise{
        txShifter := rxShifter
        txCount := 9.U
      }
    }.elsewhen(typeWire === 2.U) {
      // write regs
      when(targetChipId === io.chipId) {
        // TODO: maybe need crc
        io.waddr := regAddress
        io.wdata := data
        io.wen := true.B
      }.elsewhen(targetChipId === 255.U) {
        io.waddr := regAddress
        io.wdata := data
        io.wen := true.B
        txShifter := rxShifter
        txCount := 9.U
      }.otherwise{
        txShifter := rxShifter
        txCount := 9.U
      }
    }.otherwise {
      // nonce or read data
      txShifter := rxShifter
      txCount := 9.U
    }
  }
}


object FPGATopEmitter extends App {
  implicit val config: Parameters = (new FPGAConfig).toInstance
  chisel3.Driver.execute(args, () => new FPGATop())
}
