package scala.reflect.internal.util

import java.util
import java.util.function.IntConsumer

import scala.util.hashing.MurmurHash3

final class IntArraySet private (var data: Array[Int], var _size: Int) {
  def this() {
    this(Array.emptyIntArray, 0)
  }
  private final val MinSize = 8
  def size: Int = this._size
  def isEmpty: Boolean = size == 0
  def nonEmpty: Boolean = !isEmpty
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
  override def clone(): IntArraySet = {
    val newData = java.util.Arrays.copyOf(data, data.length)
    new IntArraySet(newData, _size)
  }

  override def equals(other: Any): Boolean = other match {
    case ias: IntArraySet => ias.size == size && (size == 0 || util.Arrays.equals(ias.data, data))
    case _ => false
  }
  override def hashCode: Int = {
    import MurmurHash3._
    var h = IntArraySet.HashSeed
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

object IntArraySet {
  private val HashSeed = "IntArraySet".hashCode()
}
