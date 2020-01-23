package me.sequencer.chiselminer

import chisel3._
import chipsalliance.rocketchip.config.Parameters

class SHA256Extender(val n: Int, val core: String, val propagateMessageList: Seq[Int], val generateMessageList: Seq[Int], val transparentMessageList: Seq[Int] = Seq())(implicit val p: Parameters) extends Module
  with HasSHA256Implementation {
  val io = IO(new Bundle {
    val messageBlockIn = Input(Vec(64, UInt(32.W)))
    val messageBlockOut = Output(Vec(64, UInt(32.W)))
  })

  override def desiredName = s"SHA256Core${core}Extender${n}"
  io.messageBlockOut := DontCare
  // constant message block used for fix data
  for (i <- transparentMessageList.indices) {
    io.messageBlockOut(transparentMessageList(i)) := io.messageBlockIn(transparentMessageList(i))
  }
  // propagating message block
  for (i <- propagateMessageList.indices) {
    io.messageBlockOut(propagateMessageList(i)) := pipelineReg(io.messageBlockIn(propagateMessageList(i)), defaultRegDelay)
  }
  // generated message block
  for (i <- generateMessageList.indices) {
    io.messageBlockOut(generateMessageList(i)) := pipelineReg(extend(io.messageBlockIn(generateMessageList(i) - 2), io.messageBlockIn(generateMessageList(i) - 15), io.messageBlockIn(generateMessageList(i) - 7), io.messageBlockIn(generateMessageList(i) - 16)))
  }
}

object SHA256Extender {
  // pipe n need nextMessageBlock = messageBlock(n + 1) from pipe(n - 1),
  // so pipe(n - 1) need to generate messageBlock(n + 1)
  // so first messageBlock(16) should be generate at pipe(14)
  // the first dualExtender should be located at x, the last dualExtender should be located at y:
  //     because messageBlock(63) will be used at pipe(61), which depended on messageBlock(61 - 16 = 47),
  //     so after we use messageBlock(47) at pipe(45), messageBlock(63) will be generated,
  //     because messageBlock(47) depend on messageBlock(47 - 16 = 31),
  //     TODO: this is the first propagateMessageBlock in dualExtender
  //     so the last singleExtender messageBlock(31) will be generated at pipe(29),
  //     so the first dualExtender is located at pipe(30)
  def apply(n: Int, core: String)(implicit p: Parameters): SHA256Extender = {
    core match {
      case i if i == "01" => n match {
        case i if i < 16 => new SHA256Extender(n, core, Seq(3), Seq(), (0 to 17).toList.filter(_ != 3))
        case i if i < 32 => new SHA256Extender(n, core, (n - 13 to n + 1).toList.filter((n: Int) => !(n < 3 || (3 < n && n < 18))), Seq(n + 2), (0 to 17).toList.filter(_ != 3))
        case i if i < 47 => new SHA256Extender(n, core, (2 * n - 44 to 2 * n - 31).toList, Seq(2 * n - 30, 2 * n - 29), (0 to 17).toList.filter(_ != 3))
        case i if i < 62 => new SHA256Extender(n, core, (n + 2 to 63).toList, Seq(), (0 to 17).toList.filter(_ != 3))
        case _ => new SHA256Extender(n, core, Seq(), Seq())
      }
      case i if i == "1" => n match {
        case i if i < 14 => new SHA256Extender(n, core, (0 to 7).toList, Seq(), (8 to 15).toList)
        case i if i < 29 => new SHA256Extender(n, core, (n - 13 to n + 1).toList.filter((n: Int) => !(7 < n && n < 16)), Seq(n + 2), (8 to 15).toList)
        case i if i < 44 => new SHA256Extender(n, core, (2 * n - 41 to 2 * n - 28).toList, Seq(2 * n - 27, 2 * n - 26), (8 to 15).toList)
        case i if i < 59 => new SHA256Extender(n, core, (n + 2 to 60).toList, Seq(), (8 to 15).toList)
        case _ => new SHA256Extender(n, core, Seq(), Seq())
      }
      case _ => n match {
        case i if i < 14 => new SHA256Extender(n, core, (0 to 15).toList, Seq())
        case i if i < 30 => new SHA256Extender(n, core, (n - 13 to n + 1).toList, Seq(n + 2))
        case i if i < 46 => new SHA256Extender(n, core, (2 * n - 46 to 2 * n - 29).toList, Seq(2 * n - 28, 2 * n - 27))
        case i if i < 62 => new SHA256Extender(n, core, (n + 1 to 63).toList, Seq())
        case _ => new SHA256Extender(n, core, Seq(), Seq())
      }
    }
  }
}
