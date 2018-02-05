/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package symtab
package classfile

import java.nio.ByteBuffer
import java.nio.channels.{Channels, FileChannel}
import java.nio.channels.FileChannel.MapMode
import java.nio.file.Files

import scala.reflect.io.PlainNioFile
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.io.SourceReader.decode

object AbstractFileReader {
  private val bufferPool = new java.util.concurrent.LinkedBlockingDeque[ByteBuffer](8)
}
/**
 * This class reads files byte per byte. Only used by ClassFileParser
 *
 * @author Philippe Altherr
 * @version 1.0, 23/03/2004
 */
class AbstractFileReader(val file: AbstractFile) {
  val buf: ByteBuffer = file match {
    case f: PlainNioFile =>
      val channel = Files.newByteChannel(f.nioPath)
      try {
        channel match {
          case ch: FileChannel => ch.map(MapMode.READ_ONLY, 0, ch.size())
          case ch =>
            var buffer = AbstractFileReader.bufferPool.pollFirst()
            if (buffer == null) {
              buffer = ByteBuffer.allocateDirect(Files.size(f.nioPath).toInt)
            } else {
              if (buffer.capacity() >= ch.size()) {
                buffer = ByteBuffer.allocateDirect(Files.size(f.nioPath).toInt)
                buffer.clear()
              } else {
                buffer.clear()
                buffer.limit(Files.size(f.nioPath).toInt)
              }
            }
            var endOfInput = false
            while (!endOfInput) {
              endOfInput = ch.read(buffer) <= 0
            }
            buffer.position(0)
            buffer
        }
      } finally {
        channel.close()
      }
    case _ =>
      ByteBuffer.wrap(file.toByteArray)
  }
  def close(): Unit = {
    AbstractFileReader.bufferPool.offer(buf)
  }

  /** the current input pointer
   */
  def bp = buf.position()
  def bp_=(i: Int) = buf.position(i)

  /** read a byte
   */
  @throws(classOf[IndexOutOfBoundsException])
  def nextByte: Byte = {
    val b = buf.get(bp)
    bp += 1
    b
  }

  /** read some bytes
   */
  def nextBytes(len: Int): Array[Byte] = { // used in ide
    bp += len
    val dest = new Array[Byte](len)
    buf.get(dest)
    dest
  }

  /** read a character
   */
  def nextChar: Char =
    (((nextByte & 0xff) << 8) + (nextByte & 0xff)).toChar

  /** read an integer
   */
  def nextInt: Int =
    ((nextByte & 0xff) << 24) + ((nextByte & 0xff) << 16) +
    ((nextByte & 0xff) <<  8) +  (nextByte & 0xff)


  /** extract a character at position bp from buf
   */
  def getChar(mybp: Int): Char = {
    buf.getChar(mybp)
  }

  /** extract an integer at position bp from buf
   */
  def getInt(mybp: Int): Int =
    buf.getInt(mybp)

  /** extract a long integer at position bp from buf
   */
  def getLong(mybp: Int): Long =
    buf.getLong(mybp)

  /** extract a float at position bp from buf
   */
  def getFloat(mybp: Int): Float = buf.getFloat(mybp)

  /** extract a double at position bp from buf
   */
  def getDouble(mybp: Int): Double = buf.getDouble(mybp)

  /** skip next 'n' bytes
   */
  def skip(n: Int) { bp += n }

}
