package me.sequencer.chiselminer

case class RegisterLocation(start: Int, end: Int) {
  require(end >= start)
}
case class RegisterMap(constantSeq: Seq[Int], registerMap: Seq[RegisterLocation]) {
  // has 64 pipelines
  require(registerMap.size == 64)
  registerMap.indices foreach {
    case i if 16 to 63 contains i => {
      val genLocation: Int = registerMap(i).start - 1
      Seq(16, 15, 7, 2) foreach { dep =>
        require(
          (registerMap(i - dep).start <= genLocation &&
            genLocation <= registerMap(i - dep).end) ||
            constantSeq.contains(i - dep))
      }
    }
    case i => if(constantSeq.contains(i)) require(registerMap(i).start == 0 && registerMap(i).end == 0)
  }
}
