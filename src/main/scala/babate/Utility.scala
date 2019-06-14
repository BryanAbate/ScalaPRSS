package main.scala.babate

import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.IOException
import java.math.BigInteger

import scala.collection.mutable.ArrayBuilder


object Utility {

  val degreeToPolynomial = Map(
    (256, new BigInteger("10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000100101", 2)),
    (128, new BigInteger("100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000111", 2)),
    (40, new BigInteger("10000000000000000101001011011000100101011", 2)),
    (37, new BigInteger("1000000000000000000000000000000111111", 2)),
    (8, new BigInteger("11b", 16)))

  /**
   * This class write bytes in a file. It is useful when writing bytes in more than one step. DO NOT forget to close it after using it
   *
   * @param fileName The name of the file to write in
   */
  class ByteWriter(fileName: String) {
    var out = None: Option[FileOutputStream]
    try {
      out = Some(new FileOutputStream(fileName))
    } catch {
      case e: IOException => throw new IOException("Cannot write in file: " + fileName)
    }

    def write(a: Array[Byte]) = {
      if (out.isDefined) out.get.write(a)
    }
    def write(a: Byte) = {
      if (out.isDefined) out.get.write(a)
    }

    def writeWithSize(a: Array[Byte]): Unit = {
      val size: BigInt = a.size
      val sizeArray = size.toByteArray
      write(sizeArray.size.toByte) //Write size of the size of the array
      write(sizeArray) //Write the size of the array
      write(a) //Write the array
    }

    def close() = {
      if (out.isDefined) out.get.close
    }
  }
  /**
   * This class read bytes in a file. It is useful when reading bytes in more than one step. DO NOT forget to close it after using it
   *
   * @param fileName The name of the file to read from
   */
  class ByteReader(fileName: String) {
    var in = None: Option[FileInputStream]

    try {
      in = Some(new FileInputStream(fileName))

    } catch {
      case e: IOException => throw new IOException("Cannot read file: " + fileName)
    }

    def readNextByte() = {
      val c = in.get.read
      if (c == -1) {
        throw new IOException("No more Data to read")
      } else {
        c
      }
    }
    def readBytes(n: Int) = {
      val array = new Array[Int](n)

      for (i <- 0 until n) {
        array(i) = in.get.read
      }

      array.map { x => x.toByte }
    }

    def readAll() = {
      val aBuilder = new ArrayBuilder.ofByte
      var c: Int = 0
      c = in.get.read()
      while (c != -1) {
        aBuilder += c.toByte
        c = in.get.read()
      }
      aBuilder.result()
    }

    def readWithSize() = {
      val sizeSize = readNextByte()
      val aSize = new BigInteger(readBytes(sizeSize))
      readBytes(aSize.intValue())
    }

    def close() = {
      if (in.isDefined) in.get.close
    }

  }
}