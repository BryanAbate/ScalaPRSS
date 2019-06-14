package main.scala.babate

import java.math.BigInteger

import scala.math.BigInt.int2bigInt

import org.abstractj.kalium.crypto.Hash
import org.abstractj.kalium.crypto.Random

/**
 * This class is meant to be used as a tool to achieve secret sharing and threshold Schnorr signature.
 *
 * @param threshold the threshold
 *
 * @param nParticipants the number of participants we want to share the secret with
 */
class SchnorrTool(threshold: Int, nParticipants: Int) {
  require(threshold <= nParticipants, "Threshold cannot be bigger than the number of participants")
  require(nParticipants < 512) //Limit the participants to 1 byte
  require(threshold >= 2, "The threshold should be at least two")

  //Note: A participant number starts at 1

  val random = new Random()
  val k = threshold //Alias
  val n = nParticipants //Alias
  val nBits = 2048;
  //val ffTool = new FiniteFieldsTool(Utility.degreeToPolynomial(nBits))
  val hash = new Hash

  //Subset of participants
  val subsets: Set[Set[Int]] = {
    def inner(i: Int): Set[Set[Int]] = i match {
      case 1 => Set(Set(1), Set.empty)
      case _ => { val r = inner(i - 1); r.map { _.+(i) } ++ r }
    }
    inner(n).filter { _.size == n - (k - 1) }
  }

  /**
   * Setup a new PRSS and writes the files for each participants
   */
  def generatePRSS() = {

    val prss_s1 = subsets.map { s =>
      (s ->
        s.map { x =>
          x -> new FieldElement(random.randomBytes(nBits / 8).foldLeft(0: BigInt) { case (acc, b) => (acc << 8) | (b & 0xFF) })
        }.toMap)
    }

    val prss = prss_s1.map {
      case (s, m) => s -> m.foldLeft(new FieldElement(0)) { case (acc, b) => acc + b._2 }
    }.map { case (s, b) => s -> b.value.toByteArray }

    (1 to n).foreach { i =>
      val writer = new Utility.ByteWriter("PRSS_" + i)
      writer.write(i.toByte) //Frist: write the number of the participant
      writer.write(threshold.toByte) //Second: write the threshold
      writer.write(nParticipants.toByte) //Third: write the number of participants

      //Then write each set followed by its key
      prss.filter { _._1.contains(i) }
        .foreach {
          case (s1, a) =>
            writer.write(s1.toArray.map(_.toByte))
            val aToWrite = a.takeRight(nBits / 8)
            writer.writeWithSize(aToWrite)
        }
      writer.close()
    }
  }

  /**
   * Compute the public key Y
   *
   * @param participantShares Each participant mapped to their secret share
   */
  def computeY(participantShares: Map[Int, FieldElement]) = {

    require(participantShares.size >= threshold, "Not enough shares to compute Y")
    val shares = if (participantShares.size > threshold) participantShares.take(threshold) else participantShares

    val ci = shares.map {
      case (i, _) =>
        i -> shares.filter(_._1 != i).map { k => (new FieldElement(k._1) / (new FieldElement(k._1) - new FieldElement(i))) }.foldLeft(new FieldElement(1)) { case (acc, v) => acc * v }
    }
    shares.foldLeft(new GroupElement(1)) { case (acc, x) => acc + (ci(x._1) * (x._2 * GroupElement.g)) }
  }

  /**
   * Sign a message m
   *
   * @param message The message to be signed
   *
   * @param Y The public key
   *
   * @param originalSharesArg Each participant mapped to their secret share used to forge Y
   *
   * @param labelSharesArg Each participant mapped to their secret share for a specific label
   */
  def sign(message: String, Y: GroupElement, originalSharesArg: Map[Int, FieldElement], labelSharesArg: Map[Int, FieldElement]) = {
    require(labelSharesArg.size >= threshold, "Not enough shares to sign the message")
    require(originalSharesArg.size >= threshold, "Not enough shares to sign the message")

    val labelShares = if (labelSharesArg.size > threshold) labelSharesArg.take(threshold) else labelSharesArg
    val shares = if (originalSharesArg.size > threshold) originalSharesArg.filter(x => labelShares.contains(x._1)).take(threshold) else originalSharesArg.filter(x => labelShares.contains(x._1))
    require(shares.size == threshold, "Invalid combination of shares")

    val Ri = labelShares.map { x => x._1 -> x._2 * GroupElement.g }

    val ci = shares.map {
      case (i, _) =>
        i -> shares.filter(_._1 != i).map { k => (new FieldElement(k._1) / (new FieldElement(k._1) - new FieldElement(i))) }.foldLeft(new FieldElement(1)) { case (acc, v) => acc * v }
    }

    val R = Ri.foldLeft(new GroupElement(1)) { case (acc, iR) => acc + (ci(iR._1) * iR._2) }

    val c = new FieldElement(hash.sha256(Y.value.toByteArray ++ R.value.toByteArray ++ message.getBytes).foldLeft(0: BigInt) { case (acc, b) => (acc << 8) | (b & 0xFF) })

    val si = shares.map(x => x._1 -> (labelShares(x._1) + (c * x._2)))

    val s = si.foldLeft(new FieldElement(0)) { case (acc, s) => acc + (ci(s._1) * s._2) }

    (R, s)
  }

  /**
   * Verify a signature for a message
   *
   * @param message The message we want to verify the signature
   *
   * @param Y The public key
   *
   * @param R The key of the signature
   *
   * @param s The signature
   */
  def verify(message: String, Y: GroupElement, R: GroupElement, s: FieldElement) = {
    val sP = s * GroupElement.g

    val h = new FieldElement(hash.sha256(Y.value.toByteArray ++ R.value.toByteArray ++ message.getBytes).foldLeft(0: BigInt) { case (acc, b) => (acc << 8) | (b & 0xFF) })
    val R_HY = R + (h * Y)
    sP.value == R_HY.value
  }
}