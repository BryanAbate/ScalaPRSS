package main.scala.babate

import scala.io.StdIn
import org.abstractj.kalium.crypto.Random
import java.math.BigInteger
import java.io.IOException

object Main {
  val incorrectArgumentsMessage = "Incorrect arguments, use -help if needed."
  val random = new Random()

  def main(args: Array[String]) {
    if (!args.isEmpty && args(0) == "-shamir") {
      polynomial()
    } else if (!args.isEmpty && args(0) == "-PRSS") {
      schnorr()
    } else if (!args.isEmpty && args(0) == "-help") {
      help()
    } else {
      println(incorrectArgumentsMessage)
    }

    def help() = {
      println("You have the choice between two secret sharing methods: the PRSS one or the Adi Shamir one")

      //Schnorr

      println("* PRSS secret sharing (with ElGamal encryption)")

      println("\tFor encryption use the following arguments:")
      println("\t\t-PRSS encrypt t n file")
      println("\t\t\tt: Threhsold value")
      println("\t\t\tn: Total number of participant")
      println("\t\t\tfile: The path to the file to be encrypted")

      println("\tFor decryption use the following arguments:")
      println("\t\t-PRSS decrypt file PRSS_1 ... PRSS_t")
      println("\t\t\tfile: The path to the file to be decrypted")
      println("\t\t\tPRSS_i: at least t PRSS files, where t is the threhsold\n")

      //Polynomial

      println("* Adi shamir secret sharing (128 bits modulus and 15 bytes blocs) (WARNING: This might be very slow and might fail with certain file extension!)")

      println("\tFor encryption use the following arguments:")
      println("\t\t-shamir encrypt t n file")
      println("\t\t\tt: Threhsold value")
      println("\t\t\tn: Total number of participant")
      println("\t\t\tfile: The path to the file to be encrypted")

      println("\tFor decryption use the following arguments:")
      println("\t\t-shamir decrypt P1_shares ... Pt_shares")
      println("\t\t\tPi_shares: at least t shares files, where t is the threhsold")
    }

    def polynomial() = {
      if (args.size >= 2) {
        if (args(1) == "encrypt") {
          polynomialEncrypt()
        } else if (args(1) == "decrypt") {
          polynomialDecrypt()
        } else {
          println(incorrectArgumentsMessage)
        }
      } else {
        println(incorrectArgumentsMessage)
      }
    }

    def polynomialEncrypt() = {
      if (args.size == 5) {
        try {
          //Parsing
          val t = Integer.parseInt(args(2))
          val n = Integer.parseInt(args(3))
          if (t > n) throw new IllegalArgumentException("[ERROR] Threshold should be less than total number of participants")
          val filePath = args(4)

          //Reader setup
          val reader = new Utility.ByteReader(filePath)

          val tool = new FiniteFieldsCryptoTool(15, 128, t, n)

          print("Encrypting...")
          val cipher = tool.encrypt(reader.readAll())
          reader.close()
          println("Done")

          //Write encryption output
          print("Writing shares files..")

          val writers = (1 to n).map { i => (i, new Utility.ByteWriter("P" + i + "_shares")) }
          val cipherSize: BigInt = cipher.size
          writers.foreach { case (i, w) => w.write(i.toByte); w.writeWithSize(filePath.split("/").last.getBytes("UTF-8")); w.write(t.toByte); w.write(n.toByte); w.writeWithSize(cipherSize.toByteArray) }
          writers.foreach { case (i, w) => cipher.foreach(m => m.filter(e => e._1 == i).foreach { e => w.writeWithSize(e._2.toByteArray) }) }
          writers.foreach(_._2.close())
          println("Done")

          println("[Success] Your file has been correctly encrypted and output as shares files named Pi_shares with i = 1 to " + n + ".")
        } catch {
          case e: IllegalArgumentException => println("[ERROR] " + incorrectArgumentsMessage)
          case e: Exception                => println("[ERROR] " + e.getMessage())
        }
      } else {
        println(incorrectArgumentsMessage)
      }
    }

    def polynomialDecrypt() = {
      if (args.size >= 4) {
        try {
          //Parsing + PRSS
          val cipherReader = new Utility.ByteReader(args(2))
          print("Reading shares files...")
          val participants = (1 to args.size - 2).map { i => new Utility.ByteReader(args(1 + i)) }.map { w => (w.readNextByte().toInt, w) }.toMap
          val fileName = new String(participants.head._2.readWithSize(), "UTF-8")
          val t = participants.head._2.readNextByte()
          val n = participants.head._2.readNextByte()
          val cipherSize = new BigInteger(participants.head._2.readWithSize()).intValue()
          participants.tail.foreach { x => x._2.readWithSize(); x._2.readBytes(2); x._2.readWithSize() }

          if (participants.size < t) throw new IllegalArgumentException("Not enough shares files")

          val cipher = (0 until cipherSize).map { x => participants.map { x => (x._1 -> new BigInt(new BigInteger(x._2.readWithSize()))) } }
          participants.foreach(_._2.close())
          println("Done")

          //Decryption
          print("Decrypting...")
          val cryptoTool = new FiniteFieldsCryptoTool(15, 128, t, n)
          val message = cryptoTool.decryptAsByteArray(cipher.toArray)
          println("Done")

          //Write of decrypted file
          print("Writing decrypted file...")
          val decryptedName = "decrypted_" + fileName
          val writer = new Utility.ByteWriter(decryptedName)

          writer.write(message)

          writer.close()
          println("Done")

          println("[Success] Successfully decrpyted file as " + decryptedName)

        } catch {
          case e: IllegalArgumentException => println("[ERROR] " + incorrectArgumentsMessage)
          case e: Exception                => println("[ERROR] " + e.getMessage())
        }
      } else {
        println(incorrectArgumentsMessage)
      }
    }

    def schnorr() = {
      if (args.size >= 2) {
        if (args(1) == "encrypt") {
          schnorrEncrypt()
        } else if (args(1) == "decrypt") {
          schnorrDecrypt()
        } else {
          println(incorrectArgumentsMessage)
        }
      } else {
        println(incorrectArgumentsMessage)
      }
    }

    def schnorrEncrypt() = {
      if (args.size == 5) {
        try {
          //Parsing
          val t = Integer.parseInt(args(2))
          val n = Integer.parseInt(args(3))
          if (t > n) throw new IllegalArgumentException("[ERROR] Threshold should be less than total number of participants")
          val filePath = args(4)

          //Reader setup
          val reader = new Utility.ByteReader(filePath)

          //Schnor setup
          print("Setting up Schnorr...")
          val schnorrTool = new SchnorrTool(t, n)
          val label = random.randomBytes()
          schnorrTool.generatePRSS()
          val participants = (1 to t).map { i => new SchnorrParticipant("PRSS_" + i) }
          val shares = participants.map { p => p.number -> p.computeLabel(label) }.toMap
          val Y = schnorrTool.computeY(shares)
          println("Done")

          //Encryption
          print("Encrypting...")
          val cryptoTool = new ElGamalCryptoTool(t, n)
          val message = reader.readAll()
          val (a, b, c, nonce) = cryptoTool.encrypt(message, Y, shares)

          reader.close()
          println("Done")
          //Write encryption output
          print("Writing encrypted file..")
          val encryptedFileName = "encrypted_" + filePath.split("/").last
          val encWriter = new Utility.ByteWriter(encryptedFileName)

          encWriter.write(nonce)
          encWriter.writeWithSize(label)
          encWriter.writeWithSize(a.value.toByteArray)
          encWriter.writeWithSize(b.value.toByteArray)
          encWriter.write(c)

          encWriter.close()
          println("Done")

          println("[Success] Your file has been correctly encrypted as " + encryptedFileName + " and the PRSS files are named PRSS_i with i = 1 to " + n + ".")
        } catch {
          case e: IllegalArgumentException => println("[ERROR] " + incorrectArgumentsMessage)
          case e: Exception                => println("[ERROR] " + e.getMessage())
        }
      } else {
        println(incorrectArgumentsMessage)
      }
    }

    def schnorrDecrypt() = {
      if (args.size >= 5) {
        try {
          //Parsing + PRSS
          val cipherReader = new Utility.ByteReader(args(2))
          print("Reading PRSS files...")
          val participants = (1 to args.size - 3).map { i => new SchnorrParticipant(args(2 + i)) }
          val t = participants.head.threshold
          val n = participants.head.n
          if (participants.size < t) throw new IllegalArgumentException("Not enough PRSS files")
          println("Done")

          val cryptoTool = new ElGamalCryptoTool(t, n)

          //Decryption
          print("Decrypting...")
          val nonce = cipherReader.readBytes(cryptoTool.nonceSize)
          val label = cipherReader.readWithSize()
          val a = new GroupElement(new BigInteger(cipherReader.readWithSize()))
          val b = new GroupElement(new BigInteger(cipherReader.readWithSize()))
          val c = cipherReader.readAll()
          cipherReader.close()

          val shares = participants.map { p => p.number -> p.computeLabel(label) }.toMap
          val message = cryptoTool.decryptAsByteArray(a, b, c, nonce, shares)
          println("Done")

          //Write of decrypted file
          print("Writing decrypted file...")
          val decryptedName = "decrypted_" + args(2).split("/").last
          val writer = new Utility.ByteWriter(decryptedName)

          writer.write(message)

          writer.close()
          println("Done")

          println("[Success] Successfully decrpyted file as " + decryptedName)

        } catch {
          case e: IllegalArgumentException => println("[ERROR] " + incorrectArgumentsMessage)
          case e: Exception                => println("[ERROR] " + e.getMessage())
        }
      } else {
        println(incorrectArgumentsMessage)
      }
    }
  }
}