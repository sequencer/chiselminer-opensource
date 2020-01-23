package me.sequencer.chiselminer

import chisel3._
import chisel3.util._


class Controller extends Module {
  val io = IO(new Bundle {
    val rx = Flipped(Decoupled(UInt(8.W)))
    val tx = Decoupled(UInt(8.W))
    val workBundleOut = Decoupled(new SHA256WorkBundle)
    val nonceIn = Flipped(Decoupled(UInt(32.W)))
  })
  val txShifterLength: Int = 46
  val headerConst: UInt = "haa".U(8.W)
  // typeWork = header1Byte, type1Byte, work44Byte
  val typeWork: UInt = "b00000001".U(8.W)
  val typeWorkSize: Int = 46
  // typeWrite = header1Byte, type1Byte, chipId1Byte, address1Byte, data1Byte
  val typeWrite: UInt = "b00000010".U(8.W)
  val typeWriteSize: Int = 5
  // typeRead = header1Byte, type1Byte, chipId1Byte, address1Byte
  val typeRead: UInt = "b00000100".U(8.W)
  val typeReadSize: Int = 4
  // typeNonce = header1Byte, type1Byte, nonce4Byte
  val typeNonce: UInt = "b00001000".U(8.W)
  val typeNonceSize: Int = 6
  // typeFeedback8 = header1Byte, type1Byte, chipId1Byte, address1Byte, data1Byte
  val typeFeedback: UInt = "b00010000".U(8.W)
  val typeFeedBackSize: Int = 5
  val chipIdAddress :: pllAddress :: miscAddress = Enum(3)
  // TODO: add more register here.
  val globalRegNameMap: Map[String, UInt] = Map(
    "chipId" -> 0.U
  )
  val globalRegVec = RegInit(VecInit(Seq.fill(globalRegNameMap.size)(0.U(8.W))))

  val nonceValid: Bool = Wire(Bool())
  val nonceReg = RegInit(VecInit(Seq.fill(typeNonceSize)(0.U(8.W))))
  val nonceLengthCounter: UInt = RegInit(0.U(3.W))
  val rxShifter: Vec[UInt] = RegInit(VecInit(Seq.fill(64)(0.U(8.W))))
  val rxHeader: UInt = rxShifter(0)
  val rxTypeWire: UInt = rxShifter(1)
  val rxTypeWork: Bool = Wire(Bool())
  val rxTypeWrite: Bool = Wire(Bool())
  val rxTypeRead: Bool = Wire(Bool())
  val rxTypeNonce: Bool = Wire(Bool())
  val rxTypeFeedback: Bool = Wire(Bool())
  val rxChipId: UInt = rxShifter(2)
  val rxRegAddress: UInt = rxShifter(3)
  val rxRegData: UInt = rxShifter.slice(4, typeWriteSize).reduceLeft(Cat(_, _))
  val rxWork: UInt = rxShifter.slice(2, typeWorkSize).reduceLeft(Cat(_, _))
  val rxIsMyMessage: Bool = Wire(Bool())
  val rxValid: Bool = Wire(Bool())
  val txShifter: Vec[UInt] = RegInit(VecInit(Seq.fill(txShifterLength)(0.U(8.W))))
  val txShifterWire: Vec[UInt] = Wire(Vec(txShifterLength, UInt(8.W)))
  val txShifterCounter: UInt = RegInit(0.U(8.W))
  val txValid: Bool = txShifterCounter =/= 0.U
  val txProxyOn: Bool = Wire(Bool())
  val txUnlock: Bool = RegInit(true.B)
  val nonceQueue: DecoupledIO[UInt] = Queue(io.nonceIn, 2)
  val rxIsNew: Bool = RegInit(false.B)

  rxTypeWork := rxTypeWire === typeWork
  rxTypeWrite := rxTypeWire === typeWrite
  rxTypeRead := rxTypeWire === typeRead
  rxTypeNonce := rxTypeWire === typeNonce
  rxTypeFeedback := rxTypeWire === typeFeedback
  rxIsMyMessage := rxChipId === globalRegVec(globalRegNameMap("chipId"))
  rxValid := (rxHeader === headerConst) && (rxTypeWork || rxTypeWrite || rxTypeRead || rxTypeNonce || rxTypeFeedback) && rxIsNew

  io.rx.ready := !nonceValid
  io.workBundleOut.bits := rxWork.asTypeOf(new SHA256WorkBundle)
  io.workBundleOut.valid := false.B
  io.tx.valid := false.B
  io.tx.bits := DontCare
  nonceValid := nonceLengthCounter =/= 0.U
  txProxyOn := !rxIsMyMessage || rxTypeWork || rxTypeNonce || rxTypeFeedback
  txShifterWire := PriorityMux(Seq(
    (rxTypeWork, Cat(rxShifter.slice(0, typeWorkSize).reduceLeft(Cat(_, _))).asTypeOf(Vec(txShifterLength, UInt(8.W)))),
    (rxTypeNonce, Cat(Fill(txShifterLength - typeNonceSize, 0.U(8.W)), rxShifter.slice(0, typeNonceSize).reduceLeft(Cat(_, _))).asTypeOf(Vec(txShifterLength, UInt(8.W)))),
    (rxTypeFeedback, Cat(Fill(txShifterLength - typeFeedBackSize, 0.U(8.W)), rxShifter.slice(0, typeFeedBackSize).reduceLeft(Cat(_, _))).asTypeOf(Vec(txShifterLength, UInt(8.W)))),
    (rxTypeWrite, Cat(Fill(txShifterLength - typeWriteSize, 0.U(8.W)), rxShifter.slice(0, typeWriteSize).reduceLeft(Cat(_, _))).asTypeOf(Vec(txShifterLength, UInt(8.W)))),
    (rxTypeRead, Cat(Fill(txShifterLength - typeReadSize, 0.U(8.W)), rxShifter.slice(0, typeReadSize).reduceLeft(Cat(_, _))).asTypeOf(Vec(txShifterLength, UInt(8.W)))),
  ))
  nonceQueue.ready := true.B

  when(nonceQueue.fire()) {
    nonceLengthCounter := typeNonceSize.U
    nonceReg := Cat(headerConst, typeNonce, nonceQueue.bits).asTypeOf(Vec(typeNonceSize, UInt(8.W)))
  }
  when(io.rx.fire()) {
    rxIsNew := true.B
    for (i <- rxShifter.indices) {
      rxShifter(i) := {
        i match {
          case k if k == 0 => io.rx.bits
          case _ => rxShifter(i - 1)
        }
      }
    }
  }

  when(nonceValid) {
    nonceQueue.ready := false.B
    when(txUnlock) {
      io.tx.enq(nonceReg(nonceReg.length.U - nonceLengthCounter))
      nonceLengthCounter := nonceLengthCounter - 1.U
    }
  }

  when(rxValid) {
    txUnlock := false.B
    rxIsNew := false.B
    when(txProxyOn) {
      txShifter := txShifterWire
      io.workBundleOut.valid := rxTypeWork
      txShifterCounter := PriorityMux(Seq(
        (rxTypeWork, typeWorkSize.U),
        (rxTypeNonce, typeNonceSize.U),
        (rxTypeFeedback, typeFeedBackSize.U),
        (rxTypeWrite, typeWriteSize.U),
        (rxTypeRead, typeReadSize.U)))
    }.otherwise {
      when(rxTypeWrite) {
        globalRegVec(rxRegAddress) := rxRegData
      }.elsewhen(rxTypeRead) {
        txShifter := Cat(Fill(txShifterLength - typeFeedBackSize, 0.U(8.W)), headerConst, typeFeedback, globalRegVec(globalRegNameMap("chipId")), rxRegAddress, globalRegVec(rxRegAddress)).asTypeOf(Vec(txShifterLength, UInt(8.W)))
        txShifterCounter := typeFeedBackSize.U
      }
    }
  }

  when(txValid) {
    io.tx.enq(txShifter(0))
    for (i <- txShifter.indices) {
      txShifter(i) := {
        i match {
          case k if k == txShifterLength - 1 => 0.U
          case _ => txShifter(i + 1)
        }
      }
    }
    txShifterCounter := txShifterCounter - 1.U
  }.otherwise {
    txUnlock := true.B
  }
}


object Controller {
  def apply(): Controller = new Controller()
}