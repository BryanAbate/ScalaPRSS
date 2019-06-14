package test.scala.babate

import scala.math.BigInt.int2bigInt

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import main.scala.babate.FiniteFieldsTool
import java.math.BigInteger

class FiniteFieldsToolSuite extends FunSuite {

  val bit8Tool = new FiniteFieldsTool(0x11b)
  val bit40Tool = new FiniteFieldsTool(new BigInteger("10000000000000000101001011011000100101011", 2))

  test("Multiplication works on 8 bits") {
    val a: BigInt = Integer.parseInt("10001010", 2)
    val b: BigInt = Integer.parseInt("00101101", 2)
    assert(bit8Tool.multiplication(a, b) == 88)
    assert(bit8Tool.multiplication(b, a) == 88)
  }

  test("Inverse works on 8 bits") {
    val a: BigInt = Integer.parseInt("10001010", 2)
    assert(bit8Tool.multiplication(a, bit8Tool.inverse(a)) == 1)
    assert(bit8Tool.multiplication(bit8Tool.inverse(a), a) == 1)

    val b: BigInt = Integer.parseInt("00101101", 2)
    assert(bit8Tool.multiplication(b, bit8Tool.inverse(b)) == 1)
    assert(bit8Tool.multiplication(bit8Tool.inverse(b), b) == 1)

    val c: BigInt = Integer.parseInt("11111111", 2)
    assert(bit8Tool.multiplication(c, bit8Tool.inverse(c)) == 1)
    assert(bit8Tool.multiplication(bit8Tool.inverse(c), c) == 1)

    val d: BigInt = Integer.parseInt("10101010", 2)
    assert(bit8Tool.multiplication(d, bit8Tool.inverse(d)) == 1)
    assert(bit8Tool.multiplication(bit8Tool.inverse(d), d) == 1)

    val e: BigInt = Integer.parseInt("1011110", 2)
    assert(bit8Tool.multiplication(e, bit8Tool.inverse(e)) == 1)
    assert(bit8Tool.multiplication(bit8Tool.inverse(e), e) == 1)
  }

  test("Division works on 8 bits") {
    val a: BigInt = Integer.parseInt("10001010", 2)
    val b: BigInt = Integer.parseInt("00101101", 2)
    val c: BigInt = 88
    assert(bit8Tool.division(c, a) == b)
    assert(bit8Tool.division(c, b) == a)
  }

  test("Inverse works on 40 bits with a random number") {
    val r = scala.util.Random
    val a: BigInt = r.nextString(5).foldLeft(0: BigInt) { case (acc, b) => (acc << 8) | b }
    assert(bit40Tool.multiplication(a, bit40Tool.inverse(a)) == 1)
    assert(bit40Tool.multiplication(bit40Tool.inverse(a), a) == 1)
  }

  test("Multiplication works on 40 bits with random numbers") {
    val r = scala.util.Random
    val a: BigInt = r.nextString(4).foldLeft(0: BigInt) { case (acc, b) => (acc << 8) | b }
    val b: BigInt = r.nextString(4).foldLeft(0: BigInt) { case (acc, b) => (acc << 8) | b }

    if (a > b) {
      val c = bit40Tool.division(a, b)
      assert(bit40Tool.multiplication(b, c) == a)
    } else {
      val c = bit40Tool.division(b, a)
      assert(bit40Tool.multiplication(a, c) == b)
    }
  }
}