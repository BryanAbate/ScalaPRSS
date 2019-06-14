package main.scala.babate

import java.math.BigInteger

import scala.annotation.tailrec
import scala.math.BigInt.int2bigInt

import org.abstractj.kalium.crypto.Hash

/**
 * This class defines a participant in the PRSS secret sharing
 *
 * @param number The number of the participant
 *
 * @param threshold The threshold for the encryption
 *
 * @param nParticipants the number of participants we want to share the secret with
 *
 * @param sets Sets in which the participant takes part in
 */
class SchnorrParticipant(fileName: String) {

  val nBits = 256;

  val reader = new Utility.ByteReader(fileName)

  val number = reader.readNextByte()
  val threshold = reader.readNextByte()
  val n = reader.readNextByte()
  val xs: Map[Set[Int], BigInt] = {
    def factorial(x: Int) = {
      @tailrec
      def inner(x: Int, r: Int): Int = if (x == 1) r else inner(x - 1, r * x)

      inner(x, 1)
    }

    val nSet = factorial(n - 1) / (factorial(threshold - 1) * factorial(n - 1 - (threshold - 1)))
    val bytesRead = for { i <- (0 until nSet) } yield (reader.readBytes(n - (threshold - 1)), reader.readWithSize())
    bytesRead.map { case (s, b) => (s.map(_.toInt).toSet -> b.foldLeft(0: BigInt) { case (acc, b) => (acc << 8) | (b & 0xFF) }) }.toMap
  }
  val sets = xs.map(_._1)

  reader.close()

  val hash = new Hash

  //This precomputes the value of the polynomial for each subset (See label formula)
  private val allP_s = sets.map { s =>
    s -> {
      val j = ((1 to n).filter(!s.contains(_)).map { x => x -> new FieldElement(0) } :+ (0 -> new FieldElement(1))).toMap
      val precomputations = j.map { case (i, v) => (i, FieldElement.precomputation(new FieldElement(i), j)) }
      FieldElement.LagrangeInterpolation(new FieldElement(number), precomputations)
    }
  }.toMap

  /**
   * Compute the share for a specified label
   *
   * @param l the desired label as a String
   *
   * @param encoding The encoding used for the string (default UTF-8)
   */
  def computeLabel(label: String, encoding: String = "UTF-8"): FieldElement = {
    computeLabel(label.getBytes(encoding))
  }

  /**
   * Compute the share for a specified label
   *
   * @param l the desired label as an array of bytes
   */
  def computeLabel(label: Array[Byte]): FieldElement = {
    allP_s.map { ps =>
      val h = hash.sha256((xs.get(ps._1) match {
        case Some(p) => p.toByteArray
        case _       => throw new Exception("Correspondence inside Ps not found")
      }) ++ label)
      ps._2 * new FieldElement(h.foldLeft(0: BigInt) { case (acc, b) => (acc << 8) | (b & 0xFF) })
    }.foldLeft(new FieldElement(0)) { case (acc, b) => acc + b }
  }

  /**
   * Compute the share used to construct Y
   */
  def computeOriginalShare() = computeLabel("key")
}