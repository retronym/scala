package scala.reflect.internal.util

import java.util
import java.util.Objects
import java.util.function.IntConsumer
import java.util.stream.{IntStream, StreamSupport}

import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.util.hashing.MurmurHash3

trait IntSet {
  def size: Int
  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty
  def head: Int
  def +=(i: Int): Unit
  def -=(i: Int): Unit
  def contains(i: Int): Boolean
  def foreach(f: IntConsumer)
  def iterator(): Iterator[Int]
  def intStream(): IntStream
  def copy(): IntSet
}


object IntSet {
  val HashSeed = "IntArraySet".hashCode()
  def apply(): IntSet = new BitSetWithOverflow()
}


final class IntArraySet private (var data: Array[Int], var _size: Int) extends IntSet {
  def this() {
    this(Array.emptyIntArray, 0)
  }
  private final val MinSize = 8
  def size: Int = this._size
  def isEmpty: Boolean = size == 0
  def head = if (_size == 0) throw new NoSuchElementException else data.apply(0)
  def +=(i: Int): Unit = {
    val index = binarySearch(i)
    if (data.length == 0) data = new Array[Int](MinSize)
    if (index < 0) {
      val insertionPoint = -(index + 1)
      val src = data
      if (data.length == _size) {
        this.data = util.Arrays.copyOf(data, data.length * 2)
      }
      System.arraycopy(src, insertionPoint, data, insertionPoint + 1, _size - insertionPoint)
      if (src ne data) {
        System.arraycopy(src, 0, data, 0, insertionPoint)
      }
      data(insertionPoint) = i
      _size += 1
    }
  }
  def -=(i: Int): Unit = {
    val index = binarySearch(i)
    if (index == size - 1) {
      data(index) = 0
      _size -= 1
    } else if (index >= 0) {
      System.arraycopy(data, index + 1, data, index, _size - index - 1)
      data(_size - 1) = 0 // needed to support use of `Arrays.equals` in `def equals` below
      _size -= 1
      // could shrink the array here if occupancy size/data.length falls below 50%
    }
  }
  def contains(i: Int): Boolean = data != null && binarySearch(i) >= 0
  def foreach(f: IntConsumer): Unit = {
    var i = 0
    while (i < _size) {
      f.accept(data(i))
      i += 1
    }
  }
  def iterator(): Iterator[Int] = {
    data.iterator().slice(0, size)
  }
  def intStream(): IntStream = {
    util.Arrays.stream(data, 0, size)
  }

  override def copy(): IntArraySet = {
    val newData = java.util.Arrays.copyOf(data, data.length)
    new IntArraySet(newData, _size)
  }

  override def equals(other: Any): Boolean = other match {
    case ias: IntArraySet => ias.size == size && (size == 0 || util.Arrays.equals(ias.data, data))
    case _ => false
  }
  override def hashCode: Int = {
    import MurmurHash3._
    var h = IntSet.HashSeed
    var i = 0
    while (i < size) {
      h = mix(h, data(i).##)
      i += 1
    }
    finalizeHash(h, size)
  }

  override def toString: String = {
    if (size == 0) "IntArraySet()"
    else {
      val result = new java.lang.StringBuilder(size * 2)
      result.append("IntArraySet(")
      var i = 0
      val n = _size - 1
      while (i < n) {
        result.append(data(i)).append(", ")
        i += 1
      }
      result.append(data(i)).append(")")
      result.toString
    }
  }

  private def binarySearch(i: Int) = java.util.Arrays.binarySearch(data, 0, _size, i)
}

class BitSetWithOverflow private(val pos: util.BitSet, val neg: util.BitSet, var _overflow: IntArraySet) extends IntSet {
  def this() = this(new util.BitSet(), new util.BitSet(), null)
  private final val Threshold = 1024
  private def overflow: IntArraySet = {
    if (_overflow eq null) _overflow = new IntArraySet()
    _overflow
  }
  override def size: Int = pos.cardinality() + neg.cardinality() + (if (_overflow eq null) 0 else _overflow.size)
  override def isEmpty: Boolean = pos.isEmpty && neg.isEmpty && (if (_overflow eq null) true else _overflow.isEmpty)
  override def nonEmpty: Boolean = !pos.isEmpty || !neg.isEmpty && (if (_overflow eq null) false else _overflow.nonEmpty)
  override def head: Int = neg.nextSetBit(0) match { case -1 => pos.nextSetBit(0) match { case -1 => throw new NoSuchElementException case x => x } case x => -x}
  override def +=(i: Int): Unit = if (Math.abs(i) >= Threshold) overflow += i else if (i >= 0) pos.set(i) else neg.set(-i)
  override def -=(i: Int): Unit = if (Math.abs(i) >= Threshold) overflow -= i else if (i >= 0) pos.set(i) else neg.set(-i)
  override def contains(i: Int): Boolean = if (Math.abs(i) >= Threshold) if (_overflow eq null) false else _overflow.contains(i) else if (i >= 0) pos.get(i) else neg.get(-i)
  override def foreach(f: IntConsumer): Unit = {neg.stream().forEach(x => f.accept(-x)); pos.stream().forEach(f); if (_overflow ne null) _overflow.foreach(f)}
  override def iterator(): Iterator[Int] = {
    val posNeg = (neg.stream().iterator().asScala ++ pos.stream().iterator().asScala).asInstanceOf[Iterator[Int]]
    if (_overflow eq null) posNeg else posNeg ++ _overflow.iterator()
  }
  override def intStream(): IntStream = {
    val posNeg = IntStream.concat(pos.stream(), neg.stream().map(x => -x))
    if (_overflow eq null) posNeg else IntStream.concat(posNeg, _overflow.intStream())
  }

  override def copy(): BitSetWithOverflow = {
    new BitSetWithOverflow(pos.clone().asInstanceOf[util.BitSet], neg.clone().asInstanceOf[util.BitSet], if (_overflow eq null) null else _overflow.copy())
  }

  override def equals(other: Any): Boolean = other match {
    case bs: BitSetWithOverflow => bs.pos.equals(pos) && bs.neg.equals(neg) && Objects.equals(bs._overflow, _overflow)
    case _ => false
  }
  override def hashCode: Int = {
    import MurmurHash3._
    var h = IntSet.HashSeed
    h = mix(h, pos.hashCode())
    h = mix(h, neg.hashCode())
    if (_overflow eq null) {
      h = mix(h, _overflow.hashCode)
      finalizeHash(h, 3)
    } else {
      finalizeHash(h, 2)
    }
  }

  override def toString: String = {
    if (size == 0) "BitSetWithOverflow()"
    else {
      val result = new java.lang.StringBuilder(size * 2)
      result.append("BitSetWithOverflow(")
      intStream.forEach(value => { result.append(value).append(", ")})
      result.delete(result.length() - 2, result.length())
      result.append(")")
      result.toString
    }
  }
}
