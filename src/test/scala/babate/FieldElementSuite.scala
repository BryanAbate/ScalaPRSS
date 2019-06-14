package test.scala.babate

import org.scalatest.FunSuite
import main.scala.babate.FieldElement

class FieldElementSuite extends FunSuite {

  test("Inverse of FieldElement working") {
    for (i <- 0 to 20) {
      val r = scala.util.Random.nextInt().abs.toInt
      val a = new FieldElement(r)
      val aInv = ~a
      assert((a*aInv).value == 1)
    }
  }

}