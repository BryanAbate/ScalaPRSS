package test.scala.babate

import scala.math.BigInt.int2bigInt

import org.scalatest.FunSuite

import main.scala.babate.FieldElement
import main.scala.babate.SchnorrParticipant
import main.scala.babate.SchnorrTool

class SchnorrToolSuite extends FunSuite {

  val t1 = new SchnorrTool(3, 5)

  test("Schnorr signing \"hello\"") {
    t1.generatePRSS()
    val (p1, p2, p3) = (new SchnorrParticipant("PRSS_1"), new SchnorrParticipant("PRSS_2"), new SchnorrParticipant("PRSS_3"))
    val sharesOrig = Map((1, p1.computeOriginalShare()), (2, p2.computeOriginalShare()), (3, p3.computeOriginalShare()))
    val Y = t1.computeY(sharesOrig)
    val sharesLabel = Map((1, p1.computeLabel("test")), (2, p2.computeLabel("test")), (3, p3.computeLabel("test")))
    val s = t1.sign("hello", Y, sharesOrig, sharesLabel)
    assert(t1.verify("hello", Y, s._1, s._2))
  }

  test("Schnorr signature with different signers on \"hello\"") {
    val prss = t1.generatePRSS()
    val (p3, p4, p5) = (new SchnorrParticipant("PRSS_3"), new SchnorrParticipant("PRSS_4"), new SchnorrParticipant("PRSS_5"))

    val sharesOrig = Map((3, p3.computeOriginalShare()), (4, p4.computeOriginalShare()), (5, p5.computeOriginalShare()))
    val Y = t1.computeY(sharesOrig)

    val sharesLabel = Map((3, p3.computeLabel("test")), (4, p4.computeLabel("test")), (5, p5.computeLabel("test")))
    val str = "hello"
    val s = t1.sign(str, Y, sharesOrig, sharesLabel)
    assert(t1.verify(str, Y, s._1, s._2))
  }

  test("Schnorr signature with random string of size 0 < s < 4096") {
    val prss = t1.generatePRSS()
    val (p2, p4, p5) = (new SchnorrParticipant("PRSS_2"), new SchnorrParticipant("PRSS_4"), new SchnorrParticipant("PRSS_5"))

    val sharesOrig = Map((2, p2.computeOriginalShare()), (4, p4.computeOriginalShare()), (5, p5.computeOriginalShare()))
    val Y = t1.computeY(sharesOrig)

    val sharesLabel = Map((2, p2.computeLabel("test")), (4, p4.computeLabel("test")), (5, p5.computeLabel("test")))
    val str = scala.util.Random.nextString(scala.util.Random.nextInt(4096))
    val s = t1.sign(str, Y, sharesOrig, sharesLabel)
    assert(t1.verify(str, Y, s._1, s._2))
  }

  test("Schnoor signature with random string of size 0 < s < 4096 and all participants") {
    val prss = t1.generatePRSS()
    val (p1, p2, p3, p4, p5) = (new SchnorrParticipant("PRSS_1"), new SchnorrParticipant("PRSS_2"), new SchnorrParticipant("PRSS_3"), new SchnorrParticipant("PRSS_4"), new SchnorrParticipant("PRSS_5"))

    val sharesOrig = Map((1, p1.computeOriginalShare()), (2, p2.computeOriginalShare()), (3, p3.computeOriginalShare()), (4, p4.computeOriginalShare()), (5, p5.computeOriginalShare()))
    val Y = t1.computeY(sharesOrig)

    val sharesLabel = Map((1, p1.computeLabel("test")), (2, p2.computeLabel("test")), (3, p3.computeLabel("test")), (4, p4.computeLabel("test")), (5, p5.computeLabel("test")))
    val str = scala.util.Random.nextString(scala.util.Random.nextInt(4096))
    val s = t1.sign(str, Y, sharesOrig, sharesLabel)
    assert(t1.verify(str, Y, s._1, s._2))
  }

  test("Schnoor signature with random string of size 0 < s < 4096 and different signer than Y generator") {
    val prss = t1.generatePRSS()
    val (p1, p2, p3, p4, p5) = (new SchnorrParticipant("PRSS_1"), new SchnorrParticipant("PRSS_2"), new SchnorrParticipant("PRSS_3"), new SchnorrParticipant("PRSS_4"), new SchnorrParticipant("PRSS_5"))

    val sharesOrig = Map((1, p1.computeOriginalShare()), (2, p2.computeOriginalShare()), (3, p3.computeOriginalShare()), (4, p4.computeOriginalShare()), (5, p5.computeOriginalShare()))
    val Y = t1.computeY(sharesOrig)

    val sharesLabel = Map((2, p2.computeLabel("test")), (4, p4.computeLabel("test")), (5, p5.computeLabel("test")))
    val str = scala.util.Random.nextString(scala.util.Random.nextInt(4096))
    val s = t1.sign(str, Y, sharesOrig, sharesLabel)
    assert(t1.verify(str, Y, s._1, s._2))
  }

  test("Two Y constructed by different participants should be the same") {
    val prss = t1.generatePRSS()
    val (p1, p2, p3, p4, p5) = (new SchnorrParticipant("PRSS_1"), new SchnorrParticipant("PRSS_2"), new SchnorrParticipant("PRSS_3"), new SchnorrParticipant("PRSS_4"), new SchnorrParticipant("PRSS_5"))

    val sharesOrig1 = Map((3, p3.computeOriginalShare()), (4, p4.computeOriginalShare()), (5, p5.computeOriginalShare()))
    val Y1 = t1.computeY(sharesOrig1)

    val sharesOrig2 = Map((1, p1.computeOriginalShare()), (3, p3.computeOriginalShare()), (5, p5.computeOriginalShare()))
    val Y2 = t1.computeY(sharesOrig2)

    val sharesOrig3 = Map((1, p1.computeOriginalShare()), (2, p2.computeOriginalShare()), (3, p3.computeOriginalShare()))
    val Y3 = t1.computeY(sharesOrig3)

    val sharesOrig4 = Map((2, p2.computeOriginalShare()), (3, p3.computeOriginalShare()), (4, p4.computeOriginalShare()))
    val Y4 = t1.computeY(sharesOrig4)

    val sharesOrig5 = Map((1, p1.computeOriginalShare()), (2, p2.computeOriginalShare()), (3, p3.computeOriginalShare()), (4, p4.computeOriginalShare()))
    val Y5 = t1.computeY(sharesOrig5)

    val sharesOrig6 = Map((1, p1.computeOriginalShare()), (2, p2.computeOriginalShare()), (3, p3.computeOriginalShare()), (4, p4.computeOriginalShare()), (5, p5.computeOriginalShare()))
    val Y6 = t1.computeY(sharesOrig6)

    assert(Y1.value == Y2.value)
    assert(Y1.value == Y3.value)
    assert(Y1.value == Y4.value)
    assert(Y1.value == Y5.value)
    assert(Y1.value == Y6.value)
  }

  test("Schorr singature with random string of size 0 < s < 4096 and paramaters (7, 15)") {
    val t = new SchnorrTool(7, 15)
    val prss = t.generatePRSS()
    val (p2, p4, p5, p7, p10, p11, p14) = (new SchnorrParticipant("PRSS_2"), new SchnorrParticipant("PRSS_4"), new SchnorrParticipant("PRSS_5"), new SchnorrParticipant("PRSS_7"), new SchnorrParticipant("PRSS_10"), new SchnorrParticipant("PRSS_11"), new SchnorrParticipant("PRSS_14"))

    val sharesOrig = Map((2, p2.computeOriginalShare()), (4, p4.computeOriginalShare()), (5, p5.computeOriginalShare()), (7, p7.computeOriginalShare()), (10, p10.computeOriginalShare()), (11, p11.computeOriginalShare()), (14, p14.computeOriginalShare()))
    val Y = t.computeY(sharesOrig)

    val sharesLabel = Map((2, p2.computeLabel("test")), (4, p4.computeLabel("test")), (5, p5.computeLabel("test")), (7, p7.computeLabel("test")), (10, p10.computeLabel("test")), (11, p11.computeLabel("test")), (14, p14.computeLabel("test")))
    val str = scala.util.Random.nextString(scala.util.Random.nextInt(4096))
    val s = t.sign(str, Y, sharesOrig, sharesLabel)
    assert(t.verify(str, Y, s._1, s._2))
  }
}