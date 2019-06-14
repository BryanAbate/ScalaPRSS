package main.scala.babate

import scala.annotation.tailrec

/**
 * This class is meant to be used as a tool to work with finite fields.
 *
 * @param finiteFieldModulo the modulo to be used with the operations. This also fix the finite field.
 */
class FiniteFieldsTool(finiteFieldModulo: BigInt) {

  //This is the number of bits of the modulo - 1. The finite field is always of size 2^(number of bit of the modulo - 1)
  val nBit = finiteFieldModulo.bitLength - 1

  val leftMostBit = (1: BigInt) << (nBit - 1)

  val leftMostBitInverse = (0 until nBit - 1).foldLeft(0: BigInt)((acc, b) => (acc << 1) | 1)

  val modulo = finiteFieldModulo & ((leftMostBitInverse << 1 | 1)) //For all the calculus we need to remove the left most bit of the modulo

  /**
   * Multiplies a times b in the finite field
   *
   * @param a First term of the multiplication
   *
   * @param b Second term of the multiplication
   */
  def multiplication(a: BigInt, b: BigInt): BigInt = {
    @tailrec
    def inner(x: BigInt, y: BigInt, r: BigInt): BigInt = x.equals(0) match {
      case true => r
      case _ => inner((x >> 1) & leftMostBitInverse, //x
        if ((y & leftMostBit) != 0) (((y << 1) & ((leftMostBitInverse << 1) | 1)) ^ modulo) else ((y << 1) & ((leftMostBitInverse << 1) | 1)), //y
        if ((x & 0x01) != 0) r ^ y else r) //r
    }
    if (a > b) inner(a, b, 0) else inner(b, a, 0)
  }

  /**
   * Inverse the number a in the finite field
   *
   * @param a The number to inverse
   */
  def inverse(a: BigInt): BigInt = {
    if (a == 0) throw new ArithmeticException("Inverse of zero is not a number")

    var exponent = (2: BigInt).pow(nBit) - 2

    @tailrec
    def inner(s: BigInt, t: BigInt, counter: Int): BigInt = counter match {
      case 0 => s
      case _ => inner(if ((exponent & 1) != 0) { exponent = ((exponent >> 1) & leftMostBitInverse); multiplication(s, t) } else { exponent = ((exponent >> 1) & leftMostBitInverse); s },
        multiplication(t, t), counter - 1)
    }

    inner(1, multiplication(1, a), nBit)
  }

  /**
   * Divides a by b in the finite field
   *
   * @param a numerator
   *
   * @param b denominator
   */
  def division(a: BigInt, b: BigInt): BigInt = {
    multiplication(a, inverse(b))
  }

  /**
   * Perform the lagrange interpolation
   *
   * @param x The value we want to compute the function for
   *
   * @param precomputations The precomputations computed thanks to the function precomputation
   */
  def LagrangeInterpolation(x: BigInt, precomputations: Map[Int, BigInt]): BigInt = {
    precomputations.map { m =>
      precomputations.filter(_._1 != m._1).map {
        xk => x ^ xk._1
      }.foldLeft(precomputations(m._1))((acc, v) => multiplication(acc, v))
    }.foldLeft(0: BigInt)((acc, v) => acc ^ v)
  }

  /**
   * Precomputation of the value needed for the Lagrange interpolation
   *
   * @param j The index we want to precompute
   *
   * @param values A map (x, f(x))
   */
  def precomputation(j: Int, values: Map[Int, BigInt]): BigInt = {
    values.filter(_._1 != j).map { k =>
      division(1, j ^ k._1)
    }.foldLeft(values(j))((acc, d) => multiplication(acc, d))
  }
}