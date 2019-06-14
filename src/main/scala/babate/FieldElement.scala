package main.scala.babate

class FieldElement(initialValue: BigInt) {
  require(initialValue >= 0)

  val q = (GroupElement.p - 1) / 2

  val value = initialValue % q

  def +(b: FieldElement) = {
    new FieldElement((value + b.value + q) % q)
  }

  def -(b: FieldElement) = {
    new FieldElement((value - b.value + q) % q)
  }

  def *(b: FieldElement) = {
    new FieldElement(((value * b.value) + q) % q)
  }

  def unary_~ = {
    new FieldElement(this.value.modPow(q - 2, q))
  }

  def /(b: FieldElement) = {
    this * (~b)
  }

  def *(B: GroupElement) = {
    new GroupElement(B.value.modPow(value, GroupElement.p))
  }

}

object FieldElement {
  /**
   * Perform the lagrange interpolation
   */
  def LagrangeInterpolation(x: FieldElement, precomputations: Map[Int, FieldElement]): FieldElement = {
    precomputations.map { m =>
      precomputations.filter(_._1 != m._1).map {
        xk => x - new FieldElement(xk._1)
      }.foldLeft(precomputations(m._1))((acc, v) => acc * v)
    }.foldLeft(new FieldElement(0))((acc, v) => acc + v)
  }

  /**
   * Precomputation of the value needed for the Lagrange interpolation
   */
  def precomputation(j: FieldElement, values: Map[Int, FieldElement]): FieldElement = {
    values.filter(_._1 != j.value).map { k =>
      new FieldElement(1) / (j - new FieldElement(k._1))
    }.foldLeft(values(j.value.toInt))((acc, d) => acc * d)
  }
}