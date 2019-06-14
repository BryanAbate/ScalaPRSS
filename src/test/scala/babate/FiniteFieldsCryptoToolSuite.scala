package test.scala.babate

import org.scalatest.FunSuite
import main.scala.babate.FiniteFieldsCryptoTool

class FiniteFieldsCryptoToolSuite extends FunSuite {

  val tool0 = new FiniteFieldsCryptoTool(5, 40, 3, 5)
  val tool1 = new FiniteFieldsCryptoTool(3, 40, 8, 10)
  val tool37 = new FiniteFieldsCryptoTool(3, 37, 3, 5)
  val tool128 = new FiniteFieldsCryptoTool(13, 128, 3, 5)
  val tool256 = new FiniteFieldsCryptoTool(7, 256, 3, 5)

  test("Encryption followed by decryption works with \"Hello\"") {
    val m = "hello"
    val keys = tool0.encrypt(m)
    val decrypted = tool0.decryptAsString(keys)
    assert(decrypted == m)
  }

  test("Encryption followed by decryption works with \"pound\" with more than t-1 keys and less than n keys") {
    val m = "pound"
    val keys = tool0.encrypt(m)
    assert(m == tool0.decryptAsString(keys.map(x => x.drop(1))))
    assert(m == tool0.decryptAsString(keys.map(x => x.drop(2))))
    assert(m == tool0.decryptAsString(keys.map(x => x.take(3))))
    assert(m == tool0.decryptAsString(keys.map(x => x.take(4))))
  }

  test("Decryption does not work with less than t keys with \"hello\"") {
    val m = "hello"
    val keys = tool0.encrypt(m)
    assert(m != tool0.decryptAsString(keys.map(x => x.drop(3))))
    assert(m != tool0.decryptAsString(keys.map(x => x.drop(4))))
  }

  test("Tests in GF(2^37)") {
    val m = "hello"
    val keys = tool37.encrypt(m)
    val decrypted = tool37.decryptAsString(keys)
    assert(decrypted == m)
    assert(m == tool37.decryptAsString(keys.map(x => x.drop(1))))
    assert(m == tool37.decryptAsString(keys.map(x => x.drop(2))))
    assert(m == tool37.decryptAsString(keys.map(x => x.take(3))))
    assert(m == tool37.decryptAsString(keys.map(x => x.take(4))))
    assert(m != tool37.decryptAsString(keys.map(x => x.drop(3))))
    assert(m != tool37.decryptAsString(keys.map(x => x.drop(4))))
  }

  test("Encryption followed by decryption works with a non-sens string with parameters (5, 3)") {
    val rand = scala.util.Random
    val m = rand.nextString(31)
    val keys = tool37.encrypt(m)
    val decrypted = tool37.decryptAsString(keys)
    assert(decrypted == m)
  }
  //This tests work but takes some time to execute, uncomment if needed
  test("Tests in GF(2^128)") {
    val m = "asdfjaikfj3983u92482ud8j0qurjjfsoaur8943ru"
    val keys = tool128.encrypt(m)
    val decrypted = tool128.decryptAsString(keys)
    assert(decrypted == m)
    assert(m == tool128.decryptAsString(keys.map(x => x.drop(1))))
    assert(m == tool128.decryptAsString(keys.map(x => x.drop(2))))
    assert(m == tool128.decryptAsString(keys.map(x => x.take(3))))
    assert(m == tool128.decryptAsString(keys.map(x => x.take(4))))
    assert(m != tool128.decryptAsString(keys.map(x => x.drop(3))))
    assert(m != tool128.decryptAsString(keys.map(x => x.drop(4))))
  }

  test("Tests in GF(2^256)") {
    val m = "hello"
    val keys = tool256.encrypt(m)
    val decrypted = tool256.decryptAsString(keys)
    assert(decrypted == m)
    assert(m == tool256.decryptAsString(keys.map(x => x.drop(1))))
    assert(m == tool256.decryptAsString(keys.map(x => x.drop(2))))
    assert(m == tool256.decryptAsString(keys.map(x => x.take(3))))
    assert(m == tool256.decryptAsString(keys.map(x => x.take(4))))
    assert(m != tool256.decryptAsString(keys.map(x => x.drop(3))))
    assert(m != tool256.decryptAsString(keys.map(x => x.drop(4))))
  }

  test("Encryption followed by decryption works with a non-sens string with parameters (10, 8)") {
    val rand = scala.util.Random
    val m = "sdklfm8äö*LäPç)çä=*çP*çbbbbbbäPç⅜⅛Lç*öçL⅛£L⅜Ľä%öŁ⅛%*äöŁ⅝äö*&%Ł%L&ŁL*%ä9@@rð€@ð@3d2#←ŧàzfé½¬fp7àđ½[{þ¬f"
    val keys = tool1.encrypt(m)
    val decrypted = tool1.decryptAsString(keys)
    assert(decrypted == m)
  }
}