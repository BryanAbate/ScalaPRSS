package main.scala.babate

import java.math.BigInteger

import org.abstractj.kalium.crypto.Random

import scala.collection.mutable.ArrayBuilder

/**
 * This class is meant to be used as a tool for encryption and decryption using the Adi Shamir method
 *
 * @param blocSize Size of the blocs wanted for the encryption. One block is always 8*blocSize bits
 *
 * @param finiteFieldDegree Degree of the finite field used for the encryption
 *
 * @param t the threshold for the encryption
 *
 * @param n the number of key to be generated
 */
class FiniteFieldsCryptoTool(blocSize: Int, finiteFieldDegree: Int, t: Int, n: Int) {

  require(finiteFieldDegree >= blocSize * 8, "Encrypting with such parameters would result in a loss of information")
  require(t <= n, "Threshold cannot be bigger than the number of keys")

  val ffTool = new FiniteFieldsTool(Utility.degreeToPolynomial(finiteFieldDegree))

  val random = new Random()

  /**
   * Encrypts a secret s
   *
   * @param s the secret (indexed sequence of bytes) to be encrypted
   *
   * @return An array of mapping (key number -> key), each position of the array represents a bloc
   */
  def encrypt(s: IndexedSeq[Byte]): Array[Map[Int, BigInt]] = {
    val sBloc = s.grouped(blocSize).toArray
    sBloc.map { x => encryptBloc(x.foldLeft(0: BigInt) { case (acc, b) => (acc << 8) | (b & 0xFF) }, x.size) }
  }

  /**
   * Encrypts a secret s
   *
   * @param s the secret (a number) to be encrypted
   *
   * @return An array of mapping (key number -> key), each position of the array represents a bloc
   */
  def encrypt(s: BigInt): Array[Map[Int, BigInt]] = {
    encrypt(s.toByteArray)
  }

  /**
   * Encrypts a secret s
   *
   * @param s the secret (a string) to be encrypted
   *
   * @param encoding The encoding used for the string (default UTF-8)
   *
   * @return An array of mapping (key number -> key), each position of the array represents a bloc
   */
  def encrypt(s: String, encoding: String = "UTF-8"): Array[Map[Int, BigInt]] = {
    encrypt(s.getBytes(encoding))
  }

  /**
   * Encrypts a secret of bit size exactly blocSize*8, hence the private
   *
   * @param s the secret of size blocSize*8 bits
   *
   * @param size The size of the byte array of s
   *
   * @return A map (indice of the key) -> (key)
   */
  private def encryptBloc(s: BigInt, size: Int): Map[Int, BigInt] = {

    //Generate t-1 random values
    val randomValues = (1 until t).map { i => (i, random.randomBytes(finiteFieldDegree / 8).foldLeft(0: BigInt) { case (acc, b) => (acc << 8) | (b & 0xFF) }) }.toMap

    val values = randomValues + (0 -> s)
    //Compute the denominators (+ the multiplication by y_j) for each index for the Lagrange interpolation
    val precomputations = values.map(x => (x._1, ffTool.precomputation(x._1, values)))

    //Compute the n keys
    (1 to n).map { i => (i, ffTool.LagrangeInterpolation(i, precomputations)) }.toMap
  }

  /**
   * Decrypt a secret s using the keys provided in argument
   *
   * @param keys An array of mapping (key number -> key), each position of the array represents a bloc
   *
   * @return A byte array containing the decrypted bytes
   */
  def decryptAsByteArray(keys: Array[Map[Int, BigInt]]): Array[Byte] = {
    if (keys.isEmpty) throw new IllegalArgumentException("Empty keys map")
    val k1KeySet = keys(0).keySet
    keys.foreach(k => if (k.keySet != k1KeySet) throw new IllegalArgumentException("Two maps have different keys"))
    val x = keys.map { k => val b = decryptBloc(k); if (b.bitLength / 8 < blocSize) b.toByteArray.takeRight(math.ceil(b.bitLength / 8.0).toInt) else b.toByteArray.takeRight(blocSize) }.flatten
    val z: BigInt = 1
    x
  }
  /**
   * Decrypt a secret s using the keys provided in argument and return a string
   *
   * @param keys An array of mapping (key number -> key), each position of the array represents a bloc
   *
   * @param encoding The encoding used for the string (default UTF-8)
   *
   * @return The decrypted corresponding string
   */
  def decryptAsString(keys: Array[Map[Int, BigInt]], encoding: String = "UTF-8"): String = {
    new String(decryptAsByteArray(keys), encoding)
  }

  /**
   * Decrypts a secret of bit size exactly blocSize*8, hence the private
   *
   * @param keys A map mapping the number of a key to its key
   *
   */
  private def decryptBloc(keys: Map[Int, BigInt]): BigInt = {
    val precomputations = keys.map { case (i, v) => (i, ffTool.precomputation(i, keys)) }
    ffTool.LagrangeInterpolation(0, precomputations)
  }
}