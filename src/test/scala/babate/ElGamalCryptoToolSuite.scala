package test.scala.babate

import org.scalatest.FunSuite
import main.scala.babate.ElGamalCryptoTool
import main.scala.babate.SchnorrParticipant
import main.scala.babate.SchnorrTool

class ElGamalCryptoToolSuite extends FunSuite {

  val schnorrTool = new SchnorrTool(3, 5)
  val tool = new ElGamalCryptoTool(3, 5)

  test("Encrypt and decrypt \"hello\"") {
    val prss = schnorrTool.generatePRSS()
    val (p1, p2, p3) = (new SchnorrParticipant("PRSS_1"), new SchnorrParticipant("PRSS_2"), new SchnorrParticipant("PRSS_3"))
    val shares = Map((1, p1.computeLabel("101")), (2, p2.computeLabel("101")), (3, p3.computeLabel("101")))
    val Y = schnorrTool.computeY(shares)

    val m = "hello"
    val ct = tool.encrypt(m, Y, shares)

    val dt = tool.decryptAsString(ct._1, ct._2, ct._3, ct._4, shares)

    assert(m == dt)
  }

  test("Encrypt and decrypt \"hello\" with different person for encryption and decryption") {
    val prss = schnorrTool.generatePRSS()
    val (p1, p2, p3, p4, p5) = (new SchnorrParticipant("PRSS_1"), new SchnorrParticipant("PRSS_2"), new SchnorrParticipant("PRSS_3"), new SchnorrParticipant("PRSS_4"), new SchnorrParticipant("PRSS_5"))
    val sharesEnc = Map((1, p1.computeLabel("hi42")), (2, p2.computeLabel("hi42")), (3, p3.computeLabel("hi42")))
    val Y = schnorrTool.computeY(sharesEnc)

    val m = "hello"
    val ct = tool.encrypt(m, Y, sharesEnc)

    val sharesDec = Map((2, p2.computeLabel("hi42")), (4, p4.computeLabel("hi42")), (5, p5.computeLabel("hi42")))
    val dt = tool.decryptAsString(ct._1, ct._2, ct._3, ct._4, sharesDec)

    assert(m == dt)
  }

  test("Encrypt and decrypt non-sense string with different person for encryption and decryption") {
    val prss = schnorrTool.generatePRSS()
    val (p1, p2, p3, p4, p5) = (new SchnorrParticipant("PRSS_1"), new SchnorrParticipant("PRSS_2"), new SchnorrParticipant("PRSS_3"), new SchnorrParticipant("PRSS_4"), new SchnorrParticipant("PRSS_5"))
    val sharesEnc = Map((2, p2.computeLabel("LI*(ZHDüä=?")), (3, p3.computeLabel("LI*(ZHDüä=?")), (4, p4.computeLabel("LI*(ZHDüä=?")))
    val Y = schnorrTool.computeY(sharesEnc)

    val m = "éädsjkfnJI*)(U=JFäP=*O£üç*O?*ç=IF?P!K*£?£%RIU=)UJR =JUWD=PR!JUH=ERHJW=!H!B=U!J ? Ï*=?WIRE=UDJ  U=  =W U=W U*E=üU 0w\n0932ouj90or3ujp09r2\tsiaopfdj3094@@@"
    val ct = tool.encrypt(m, Y, sharesEnc)

    val sharesDec = Map((3, p3.computeLabel("LI*(ZHDüä=?")), (4, p4.computeLabel("LI*(ZHDüä=?")), (5, p5.computeLabel("LI*(ZHDüä=?")))
    val dt = tool.decryptAsString(ct._1, ct._2, ct._3, ct._4, sharesDec)

    assert(m == dt)
  }
}