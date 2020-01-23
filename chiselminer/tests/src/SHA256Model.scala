class SHA256Util {
  // constant
  def h: IndexedSeq[BigInt] = IndexedSeq(
    // Initialize hash states:
    // first 32 bits of the fractional parts of the square roots of the first 8 primes 2..19:
    "6a09e667", "bb67ae85", "3c6ef372", "a54ff53a", "510e527f", "9b05688c", "1f83d9ab", "5be0cd19").map(BigInt(_, 16))

  def k: IndexedSeq[BigInt] = IndexedSeq(
    // Initialize array of round constants:
    // first 32 bits of the fractional parts of the cube roots of the first 64 primes 2..311:
    "428a2f98", "71374491", "b5c0fbcf", "e9b5dba5", "3956c25b", "59f111f1", "923f82a4", "ab1c5ed5",
    "d807aa98", "12835b01", "243185be", "550c7dc3", "72be5d74", "80deb1fe", "9bdc06a7", "c19bf174",
    "e49b69c1", "efbe4786", "0fc19dc6", "240ca1cc", "2de92c6f", "4a7484aa", "5cb0a9dc", "76f988da",
    "983e5152", "a831c66d", "b00327c8", "bf597fc7", "c6e00bf3", "d5a79147", "06ca6351", "14292967",
    "27b70a85", "2e1b2138", "4d2c6dfc", "53380d13", "650a7354", "766a0abb", "81c2c92e", "92722c85",
    "a2bfe8a1", "a81a664b", "c24b8b70", "c76c51a3", "d192e819", "d6990624", "f40e3585", "106aa070",
    "19a4c116", "1e376c08", "2748774c", "34b0bcb5", "391c0cb3", "4ed8aa4a", "5b9cca4f", "682e6ff3",
    "748f82ee", "78a5636f", "84c87814", "8cc70208", "90befffa", "a4506ceb", "bef9a3f7", "c67178f2").map(BigInt(_, 16))

  def u32Max: BigInt = BigInt("ffffffff", 16)

  def rightRotate(x: BigInt, n: Int): BigInt = ((x >> n) | (x << (32 - n))) & BigInt("ffffffff", 16)

  def rightShift(x: BigInt, n: Int): BigInt = x >> n

  /**
    *
    * for each chunk
    * create a 64-entry message schedule array w[0..63] of 32-bit words
    * (The initial values in w[0..63] don't matter, so many implementations zero them here)
    * copy chunk into first 16 words w[0..15] of the message schedule array
    *
    * Extend the first 16 words into the remaining 48 words w[16..63] of the message schedule array:
    * for i from 16 to 63
    * s0 := (w[i-15] rightRotate 7) xor (w[i-15] rightRotate 18) xor (w[i-15] rightshift 3)
    * s1 := (w[i-2] rightRotate 17) xor (w[i-2] rightRotate 19) xor (w[i-2] rightshift 10)
    * w[i] := w[i-16] + s0 + w[i-7] + s1
    *
    * use a 16 sized chunk extend whole 64 sized messageBlock
    *
    * @param chunk a 16 sized IndexedSeq[BigInt]
    * @return a full 64 sized IndexedSeq[BigInt] called messageBlock
    */
  def extend(chunk: IndexedSeq[BigInt]): IndexedSeq[BigInt] = {
    var messageBlock: IndexedSeq[BigInt] = chunk
    for (i <- 16 to 63) {
      val s0: BigInt = rightRotate(messageBlock(i - 15), 7) ^ rightRotate(messageBlock(i - 15), 18) ^ rightShift(messageBlock(i - 15), 3)
      val s1: BigInt = rightRotate(messageBlock(i - 2), 17) ^ rightRotate(messageBlock(i - 2), 19) ^ rightShift(messageBlock(i - 2), 10)
      val nextMessageBlock: BigInt = (messageBlock(i - 16) + s0 + messageBlock(i - 7) + s1) & u32Max
      messageBlock = messageBlock :+ nextMessageBlock
    }
    messageBlock
  }

  /**
    * the compress function for SHA256, which is:
    *
    * S1 := (e rightRotate 6) xor (e rightRotate 11) xor (e rightRotate 25)
    * ch := (e and f) xor ((not e) and g)
    * temp1 := h + S1 + ch + k[i] + w[i]
    * S0 := (a rightRotate 2) xor (a rightRotate 13) xor (a rightRotate 22)
    * maj := (a and b) xor (a and c) xor (b and c)
    * temp2 := S0 + maj
    *
    * h := g 6
    * g := f 5
    * f := e 4
    * e := d 3 + temp1
    * d := c 2
    * c := b 1
    * b := a 0
    * a := temp1 + temp2
    *
    * @param state a 8 sized IndexedSeq[BigInt]
    * @return a 8 sized IndexedSeq[BigInt]
    */
  def compress(state: IndexedSeq[BigInt], round: Int, messageBlock: IndexedSeq[BigInt]): IndexedSeq[BigInt] = {
    val s1 = rightRotate(state(4), 6) ^ rightRotate(state(4), 11) ^ rightRotate(state(4), 25)
    val ch = (state(4) & state(5)) ^ (~state(4) & state(6))
    val temp1 = state(7) + s1 + ch + k(round) + messageBlock(round)
    val s0 = rightRotate(state(0), 2) ^ rightRotate(state(0), 13) ^ rightRotate(state(0), 22)
    val maj = (state(0) & state(1)) ^ (state(0) & state(2)) ^ (state(1) & state(2))
    val temp2 = s0 + maj
    IndexedSeq(
      (temp1 + temp2) & u32Max,
      state(0),
      state(1),
      state(2),
      (state(3) + temp1) & u32Max,
      state(4),
      state(5),
      state(6))
  }
}

class SHA256CTX(iv: IndexedSeq[BigInt], messageBlock: IndexedSeq[BigInt], state: IndexedSeq[BigInt], round: Int) extends SHA256Util {
  // sanity check
  iv.foreach(x => require(x < u32Max))
  messageBlock.foreach(x => require(x < u32Max))
  state.foreach(x => require(x < u32Max))
  require(round <= 65)

  /**
    * generate next round of SHA256
    *
    * @return next round of SHA256
    */
  def nextRound: SHA256CTX = {
    if (round < 64)
      new SHA256CTX(iv, messageBlock, compress(state, round, messageBlock), round + 1)
    else
      new SHA256CTX(iv, messageBlock, for ((x, y) <- state zip iv) yield (x + y) & u32Max, round + 1)
  }

  def toRound(targetRound: Int): SHA256CTX = {
    require(targetRound > round)
    var ctx = this.nextRound
    while (ctx.getRound != targetRound) {
      ctx = ctx.nextRound
    }
    ctx
  }

  def toFinal: SHA256CTX = {
    this.toRound(targetRound = 65)
  }

  /**
    * for uvm check
    * get current round
    *
    * @return current round
    */
  def getRound: Int = {
    this.round
  }

  /**
    * for uvm check
    * get whole messageBlock
    *
    * @return whole messageBlock
    */
  def getMessageBlock: IndexedSeq[BigInt] = {
    this.messageBlock
  }

  /**
    * for uvm check
    * get current state
    *
    * @return current state
    */
  def getStateIn: IndexedSeq[BigInt] = {
    this.state
  }

  /**
    * for uvm check
    * get input whk
    *
    * @return input whk
    */
  def getWHKIn: BigInt = {
    (this.messageBlock(this.round) + this.state(7) + k(this.round)) & u32Max
  }

  /**
    * for uvm check
    * get output state
    *
    * @return output state
    */
  def getStateOut: IndexedSeq[BigInt] = {
    nextRound.getStateIn
  }

  /**
    * for uvm check
    * get output whk
    *
    * @return output whk
    */
  def getWHKOut: BigInt = {
    nextRound.getWHKIn
  }
}

/**
  * SHA256 UVM model.
  */
object SHA256Model extends SHA256Util {
  /**
    *
    * @param iv    a 8 sized IndexedSeq[BigInt]
    * @param chunk a 16 sized IndexedSeq[BigInt]
    * @param state a 8 sized IndexedSeq[BigInt]
    * @param round current round
    * @return
    */
  def apply(iv: IndexedSeq[BigInt], chunk: IndexedSeq[BigInt], state: IndexedSeq[BigInt], round: Int): SHA256CTX = {
    require(iv.size == 8)
    require(chunk.size == 16)
    require(state.size == 8)
    require((round <= 65) && (round >= 0))
    new SHA256CTX(iv, extend(chunk), state, round)
  }
}

object SHA256IVStateModel extends SHA256Util {
  /**
    * Generate a new SHA256CTX with h as iv
    *
    * @param chunk a 16 sized IndexedSeq[BigInt](maybe need padding).
    * @return a SHA256State Instance
    */
  def apply(chunk: IndexedSeq[BigInt]): SHA256CTX = {
    require(chunk.size == 16)
    new SHA256CTX(
      iv = h,
      messageBlock = extend(chunk),
      state = h,
      round = 0
    )
  }
}

object SHA256Models extends SHA256Util {
  /**
    * begin with the original message of length L bits
    * append a single '1' bit
    * append K '0' bits, where K is the minimum number >= 0 such that L + 1 + K + 64 is a multiple of 512
    * append L as a 64-bit big-endian integer, making the total post-processed length a multiple of 512 bits
    *
    * @param message : a arbitrate size of message blocks
    * @return a sequence of chunks, each of messageBlocks is 16 sized IndexedSeq[BigInt], the last one should be padded
    */
  def generateChunks(message: IndexedSeq[BigInt]): IndexedSeq[IndexedSeq[BigInt]] = {
    require(message != null)
    val tailSize = message.length % 16
    val zeroSize: Int = if (tailSize <= 13) {
      13 - tailSize
    } else {
      13 + 16 - tailSize
    }
    val messageSize: BigInt = BigInt(message.length) * 32
    require(messageSize < BigInt("ffffffffffffffff", 16))
    val paddedMessage: IndexedSeq[BigInt] = message ++:
        IndexedSeq.fill(1)(BigInt("80000000", 16)) ++: // 1 dw
        IndexedSeq.fill(zeroSize)(BigInt("00000000", 16)) ++: // n dw
        IndexedSeq(messageSize >> 32 & u32Max, messageSize & u32Max) // 2 dw
    for (i <- 0 until paddedMessage.length / 16) yield {
      paddedMessage.slice(i * 16, (i + 1) * 16)
    }
  }

  /**
    * Generate a list of SHA256CTX for message, the size of list is ceil(message.size / 16)
    *
    * @param message a list of message, represent the original data.
    * @return a SHA256State Instance
    */
  def apply(message: IndexedSeq[BigInt]): IndexedSeq[SHA256CTX] = {
    message.foreach(x => require(x < u32Max))
    var digest = h
    for (chunk <- generateChunks(message)) yield {
      val ctx = new SHA256CTX(
        iv = digest,
        messageBlock = extend(chunk),
        state = digest,
        round = 0
      )
      digest = ctx.toFinal.getStateIn
      ctx
    }
  }
}