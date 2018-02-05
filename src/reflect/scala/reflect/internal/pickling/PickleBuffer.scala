/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package reflect
package internal
package pickling

import java.nio.ByteBuffer

/** Variable length byte arrays, with methods for basic pickling and unpickling.
 */
abstract class PickleBase {
  def readIndex: Int
  def readIndex_=(i: Int): Unit

  /** Perform operation `op` until the condition
    *  `readIndex == end` is satisfied.
    *  Concatenate results into a list.
    */
  def until[T](end: Int, op: () => T): List[T] =
    if (readIndex == end) List() else op() :: until(end, op)

  /** Perform operation `op` the number of
    *  times specified.  Concatenate the results into a list.
    */
  def times[T](n: Int, op: ()=>T): List[T] =
    if (n == 0) List() else op() :: times(n-1, op)
}

class PickleReader(data: ByteBuffer) extends PickleBase {
  override def readIndex: Int = data.position()
  override def readIndex_=(i: Int): Unit = data.position(i)

  /** Pickle = majorVersion_Nat minorVersion_Nat nbEntries_Nat {Entry}
    *  Entry  = type_Nat length_Nat [actual entries]
    *
    *  Assumes that the ..Version_Nat are already consumed.
    *
    *  @return an array mapping entry numbers to locations in
    *  the byte array where the entries start.
    */
  def createIndex: Array[Int] = {
    val index = new Array[Int](readNat()) // nbEntries_Nat
    for (i <- 0 until index.length) {
      index(i) = readIndex
      readByte() // skip type_Nat
      readIndex = readNat() + readIndex // read length_Nat, jump to next entry
    }
    index
  }

  def readByte(): Int = {
    data.get.toInt
  }
  /** Read a natural number in big endian format, base 128.
    *  All but the last digits have bit 0x80 set.*/
  def readNat(): Int = readLongNat().toInt

  def readLongNat(): Long = {
    var b = 0L
    var x = 0L
    do {
      b = readByte().toLong
      x = (x << 7) + (b & 0x7f)
    } while ((b & 0x80) != 0L)
    x
  }

  /** Read a long number in signed big endian format, base 256. */
  def readLong(len: Int): Long = {
    var x = 0L
    var i = 0
    while (i < len) {
      x = (x << 8) + (readByte() & 0xff)
      i += 1
    }
    val leading = 64 - (len << 3)
    x << leading >> leading
  }

}

class PickleBuffer(data: Array[Byte], from: Int, to: Int) extends PickleBase {
  var readIndex = from
  var bytes = data
  var writeIndex = to

  /** Double bytes array */
  private def dble() {
    val bytes1 = new Array[Byte](bytes.length * 2)
    Array.copy(bytes, 0, bytes1, 0, writeIndex)
    bytes = bytes1
  }

  def ensureCapacity(capacity: Int) =
    while (bytes.length < writeIndex + capacity) dble()

  // -- Basic output routines --------------------------------------------

  /** Write a byte of data */
  def writeByte(b: Int) {
    if (writeIndex == bytes.length) dble()
    bytes(writeIndex) = b.toByte
    writeIndex += 1
  }

  /** Write a natural number in big endian format, base 128.
   *  All but the last digits have bit 0x80 set.
   */
  def writeNat(x: Int) =
    writeLongNat(x.toLong & 0x00000000FFFFFFFFL)

  /**
   * Like writeNat, but for longs. This is not the same as
   * writeLong, which writes in base 256. Note that the
   * binary representation of LongNat is identical to Nat
   * if the long value is in the range Int.MIN_VALUE to
   * Int.MAX_VALUE.
   */
  def writeLongNat(x: Long) {
    def writeNatPrefix(x: Long) {
      val y = x >>> 7
      if (y != 0L) writeNatPrefix(y)
      writeByte(((x & 0x7f) | 0x80).toInt)
    }
    val y = x >>> 7
    if (y != 0L) writeNatPrefix(y)
    writeByte((x & 0x7f).toInt)
  }

  /** Write a natural number `x` at position `pos`.
   *  If number is more than one byte, shift rest of array to make space.
   */
  def patchNat(pos: Int, x: Int) {
    def patchNatPrefix(x: Int) {
      writeByte(0)
      Array.copy(bytes, pos, bytes, pos+1, writeIndex - (pos+1))
      bytes(pos) = ((x & 0x7f) | 0x80).toByte
      val y = x >>> 7
      if (y != 0) patchNatPrefix(y)
    }
    bytes(pos) = (x & 0x7f).toByte
    val y = x >>> 7
    if (y != 0) patchNatPrefix(y)
  }

  /** Write a long number `x` in signed big endian format, base 256.
   *
   *  @param x The long number to be written.
   */
  def writeLong(x: Long) {
    val y = x >> 8
    val z = x & 0xff
    if (-y != (z >> 7)) writeLong(y)
    writeByte(z.toInt)
  }
}
