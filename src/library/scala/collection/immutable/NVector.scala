/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection
package immutable

import scala.annotation.switch
import scala.collection.mutable.{ArrayBuilder, Builder, ReusableBuilder}
import scala.collection.generic.DefaultSerializable
import scala.reflect.ClassTag
import java.util.{Arrays, Spliterator}
import java.util.Arrays.{copyOf, copyOfRange}
import java.lang.Math.{abs, max => mmax, min => mmin}

import NVectorStatics.{Arr1, Arr2, Arr3, Arr4, Arr5, Arr6}
import scala.collection.Stepper.EfficientSplit


object NVector extends StrictOptimizedSeqFactory[NVector] {
  import NVectorStatics._

  def empty[A]: NVector[A] = NVector0

  def from[E](it: collection.IterableOnce[E]): NVector[E] =
    it match {
      case v: NVector[E] => v
      case _ =>
        val knownSize = it.knownSize
        if (knownSize == 0) empty[E]
        else if (knownSize > 0 && knownSize <= WIDTH) {
          val a1 = new Array[Any](knownSize)
          it.iterator.copyToArray(a1)
          new NVector1[E](a1.asInstanceOf[Arr1])
        } else {
          (newBuilder ++= it).result()
        }
    }

  def newBuilder[A]: ReusableBuilder[A, NVector[A]] = new NVectorBuilder[A]

  /** Create a Vector with the same element at each index.
    *
    * Unlike `fill`, which takes a by-name argument for the value and can thereby
    * compute different values for each index, this method guarantees that all
    * elements are identical. This allows sparse allocation in O(log n) time and space.
    */
  def fillSparse[A](n: Int)(elem: A): NVector[A] = {
    if(n <= 0) NVector0
    else {
      val b = new NVectorBuilder[A]
      b.initSparse(n, elem)
      b.result()
    }
  }
}


/** Vector is a general-purpose, immutable data structure.  It provides random access and updates
  * in O(log n) time, as well as very fast append/prepend/tail/init (amortized O(1), worst case O(log n)).
  * Because vectors strike a good balance between fast random selections and fast random functional updates,
  * they are currently the default implementation of immutable indexed sequences.
  *
  * Vectors are implemented by radix-balanced finger trees of width 32. There is a separate subclass
  * for each level (0 to 6, with 0 being the empty vector and 6 a tree with a maximum width of 64 at the
  * top level).
  *
  * Tree balancing:
  * - Only the first dimension of an array may have a size < WIDTH
  * - In a `data` (central) array the first dimension may be up to WIDTH-2 long, in `prefix1` and `suffix1` up
  *   to WIDTH, and in other `prefix` and `suffix` arrays up to WIDTH-1
  * - `prefix1` and `suffix1` are never empty
  * - Balancing does not cross the main data array (i.e. prepending never touches the suffix and appending never touches
  *   the prefix). The level is increased/decreased when the affected side plus main data is already full/empty
  * - All arrays are left-aligned and truncated
  *
  * In addition to the data slices (`prefix1`, `prefix2`, ..., `dataN`, ..., `suffix2`, `suffix1`) we store a running
  * count of elements after each prefix for more efficient indexing without having to dereference all prefix arrays.
  */
sealed abstract class NVector[+A](protected[this] final val prefix1: Arr1, final val length: Int, depth: Int)
  extends AbstractSeq[A]
    with IndexedSeq[A]
    with IndexedSeqOps[A, NVector, NVector[A]]
    with StrictOptimizedSeqOps[A, NVector, NVector[A]]
    with IterableFactoryDefaults[A, NVector]
    with DefaultSerializable {
  import NVectorStatics._

  override def iterableFactory: SeqFactory[NVector] = NVector

  protected[this] def tinyAppendLimit: Int = 4 + vectorSliceCount

  override final def appendedAll[B >: A](suffix: collection.IterableOnce[B]): NVector[B] = {
    val k = suffix.knownSize
    if(k == 0) this
    else appendedAll0(suffix, k)
  }

  protected[this] def appendedAll0[B >: A](suffix: collection.IterableOnce[B], k: Int): NVector[B] = {
    if(k > 0 && k < tinyAppendLimit) {
      var v: NVector[B] = this
      suffix match {
        case it: Iterable[_] => it.foreach(x => v = v.appended(x))
        case _ => suffix.iterator.foreach(x => v = v.appended(x))
      }
      v
    } else new NVectorBuilder[B].initFrom(this).addAll(suffix).result()
  }

  override def className = "NVector"

  @inline override final def take(n: Int): NVector[A] = slice(0, n)
  @inline override final def drop(n: Int): NVector[A] = slice(n, length)
  @inline override final def takeRight(n: Int): NVector[A] = slice(length - mmax(n, 0), length)
  @inline override final def dropRight(n: Int): NVector[A] = slice(0, length - mmax(n, 0))
  override def tail: NVector[A] = slice(1, length)
  override def init: NVector[A] = slice(0, length-1)

  protected[this] def slice0(lo: Int, hi: Int): NVector[A]

  override final def slice(from: Int, until: Int): NVector[A] = {
    val lo = mmax(from, 0)
    val hi = mmin(until, length)
    val newlen = hi - lo
    if(newlen == length) this
    else if(newlen <= 0) NVector0
    else slice0(lo, hi)
  }

  /** Number of slices */
  protected[immutable] def vectorSliceCount: Int
  /** Slice at index */
  protected[immutable] def vectorSlice(idx: Int): Array[_ <: AnyRef]
  /** Dimension of the slice at index */
  protected[immutable] final def vectorSliceDim(idx: Int): Int = {
    val c = vectorSliceCount/2
    c+1-abs(idx-c)
  }
  /** Length of all slices up to and including index */
  protected[immutable] def vectorSlicePrefixLength(idx: Int): Int

  override def iterator: Iterator[A] = new NVectorIterator(this)

  override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Int = iterator.copyToArray(xs, start, len)

  //override def toVector: Vector[A] = this

  //override protected def applyPreferredMaxLength: Int = Vector.defaultApplyPreferredMaxLength

  override def stepper[S <: Stepper[_]](implicit shape: StepperShape[A, S]): S with EfficientSplit = {
    import convert.impl._
    val s = shape.shape match {
      case StepperShape.IntShape    => new IntNVectorStepper(new NVectorIterator(this).asInstanceOf[NVectorIterator[Int]])
      case StepperShape.LongShape   => new LongNVectorStepper(new NVectorIterator(this).asInstanceOf[NVectorIterator[Long]])
      case StepperShape.DoubleShape => new DoubleNVectorStepper(new NVectorIterator(this).asInstanceOf[NVectorIterator[Double]])
      case _                        => shape.parUnbox(new AnyNVectorStepper[A](new NVectorIterator(this)))
    }
    s.asInstanceOf[S with EfficientSplit]
  }

  protected[this] def ioob(index: Int): IndexOutOfBoundsException =
    new IndexOutOfBoundsException(s"$index is out of bounds (min 0, max ${length-1})")

  override final def head: A =
    try prefix1(0).asInstanceOf[A]
    catch { case _: NullPointerException => throw ioob(0) }

  override final def apply(index: Int): A = {
    (depth: @switch) match {
      case 0 =>
        NVector0.apply0(index)
      case 1 =>
        this.asInstanceOf[NVector1[A]].apply0(index)
      case 2 =>
        this.asInstanceOf[NVector2[A]].apply0(index)
      case 3 =>
        this.asInstanceOf[NVector3[A]].apply0(index)
      case 4 =>
        this.asInstanceOf[NVector4[A]].apply0(index)
      case 5 =>
        this.asInstanceOf[NVector5[A]].apply0(index)
      case 6 =>
        this.asInstanceOf[NVector6[A]].apply0(index)
    }
  }
}


/** Empty vector */
private final object NVector0 extends NVector[Nothing](null, 0, 0) {
  import NVectorStatics._

  def apply0(index: Int) = throw ioob(index)

  override def updated[B >: Nothing](index: Int, elem: B): NVector[B] = throw ioob(index)

  override def appended[B >: Nothing](elem: B): NVector[B] = new NVector1(wrap1(elem))

  override def prepended[B >: Nothing](elem: B): NVector[B] = new NVector1(wrap1(elem))

  override def iterator: Iterator[Nothing] = Iterator.empty

  override def foreach[U](f: Nothing => U): Unit = ()

  override def map[B](f: Nothing => B): NVector[B] = this

  override def tail: NVector[Nothing] = throw new UnsupportedOperationException("empty.tail")

  override def init: NVector[Nothing] = throw new UnsupportedOperationException("empty.init")

  protected[this] def slice0(lo: Int, hi: Int): NVector[Nothing] = this

  protected[immutable] def vectorSliceCount: Int = 0
  protected[immutable] def vectorSlice(idx: Int): Array[_ <: AnyRef] = null
  protected[immutable] def vectorSlicePrefixLength(idx: Int): Int = 0

  override def equals(o: Any): Boolean = {
    if(this eq o.asInstanceOf[AnyRef]) true
    else o match {
      case that: NVector[_] => false
      case o => super.equals(o)
    }
  }

  override def stepper[S <: Stepper[_]](implicit shape: StepperShape[Nothing, S]): S with EfficientSplit =
    empty1.stepper(shape.asInstanceOf[StepperShape[AnyRef, S]])

  override protected[this]def appendedAll0[B >: Nothing](suffix: collection.IterableOnce[B], k: Int): NVector[B] =
    NVector.from(suffix)

  override protected[this] def ioob(index: Int) =
    new IndexOutOfBoundsException(s"$index is out of bounds (empty vector)")
}


/** Flat ArraySeq-like structure */
private final class NVector1[+A](_data1: Arr1) extends NVector[A](_data1, _data1.length, 1) {
  import NVectorStatics._

  @inline def apply0(index: Int): A =
    try prefix1(index).asInstanceOf[A]
    catch { case _: ArrayIndexOutOfBoundsException => throw ioob(index) }

  override def updated[B >: A](index: Int, elem: B): NVector[B] = {
    if(index < 0 || index >= length) throw ioob(index)
    new NVector1(copyUpdate(prefix1, index, elem))
  }

  override def appended[B >: A](elem: B): NVector[B] = {
    if(length < WIDTH) {
      val a = copyOf(prefix1, length+1)
      a(length) = elem.asInstanceOf[AnyRef]
      new NVector1(a)
    } else new NVector2(prefix1, WIDTH, empty2, wrap1(elem), WIDTH+1)
  }

  override def prepended[B >: A](elem: B): NVector[B] = {
    if(length < WIDTH) new NVector1(copyPrepend1(elem, prefix1))
    else new NVector2(wrap1(elem), 1, empty2, prefix1, length+1)
  }

  override def iterator: Iterator[A] = new ArrayOps.ArrayIterator(prefix1).asInstanceOf[Iterator[A]]

  override def foreach[U](f: A => U): Unit = foreachElem(prefix1, f)

  override def map[B](f: A => B): NVector[B] = new NVector1(mapElems1(prefix1, f))

  override def last: A = prefix1(prefix1.length-1).asInstanceOf[A]

  protected[this] def slice0(lo: Int, hi: Int): NVector[A] =
    new NVector1(copyOfRange(prefix1, lo, hi))

  override def tail: NVector[A] =
    if(prefix1.length == 1) NVector0
    else new NVector1(copyTail(prefix1))

  override def init: NVector[A] =
    if(prefix1.length == 1) NVector0
    else new NVector1(copyInit(prefix1))

  protected[immutable] def vectorSliceCount: Int = 1
  protected[immutable] def vectorSlice(idx: Int): Array[_ <: AnyRef] = prefix1
  protected[immutable] def vectorSlicePrefixLength(idx: Int): Int = prefix1.length

  override def stepper[S <: Stepper[_]](implicit shape: StepperShape[A, S]): S with EfficientSplit =
    prefix1.stepper(shape.asInstanceOf[StepperShape[AnyRef, S]])

  override protected[this] def appendedAll0[B >: A](suffix: collection.IterableOnce[B], k: Int): NVector[B] = {
    val data1b = append1IfSpace(prefix1, suffix)
    if(data1b ne null) new NVector1(data1b)
    else super.appendedAll0(suffix, k)
  }
}


/** 2-dimensional radix-balanced finger tree */
private final class NVector2[+A](_prefix1: Arr1, len1: Int,
                                 data2: Arr2,
                                 suffix1: Arr1,
                                 length: Int) extends NVector[A](_prefix1, length, 2) {
  import NVectorStatics._

  @inline private[this] def copy(prefix1: Arr1 = prefix1, len1: Int = len1,
                                 data2: Arr2 = data2,
                                 suffix1: Arr1 = suffix1,
                                 length: Int = length) =
    new NVector2(prefix1, len1, data2, suffix1, length)

  @inline def apply0(index: Int): A = {
    if(index < 0 || index >= length) throw ioob(index)
    if(index >= len1) {
      val io = index - len1
      val i2 = io >>> BITS
      val i1 = io & MASK
      if(i2 < data2.length) data2(i2)(i1)
      else suffix1(i1)
    } else prefix1(index)
  }.asInstanceOf[A]

  override def updated[B >: A](index: Int, elem: B): NVector[B] = {
    if(index < 0 || index >= length) throw ioob(index)
    if(index >= len1) {
      val io = index - len1
      val i2 = io >>> BITS
      val i1 = io & MASK
      if(i2 < data2.length) copy(data2 = copyUpdate(data2, i2, i1, elem))
      else copy(suffix1 = copyUpdate(suffix1, i1, elem))
    } else {
      copy(prefix1 = copyUpdate(prefix1, index, elem))
    }
  }

  override def appended[B >: A](elem: B): NVector[B] = {
    if     (suffix1.length < WIDTH  ) copy(suffix1 = copyAppend(suffix1, elem.asInstanceOf[AnyRef]), length = length+1)
    else if(data2.length   < WIDTH-2) copy(data2 = copyAppend(data2, suffix1), suffix1 = wrap1(elem), length = length+1)
    else new NVector3(prefix1, len1, data2, WIDTH*(WIDTH-2) + len1, empty3, wrap2(suffix1), wrap1(elem), length+1)
  }

  override def prepended[B >: A](elem: B): NVector[B] = {
    if     (len1         < WIDTH  ) copy(copyPrepend1(elem, prefix1), len1+1, length = length+1)
    else if(data2.length < WIDTH-2) copy(wrap1(elem), 1, copyPrepend(prefix1, data2), length = length+1)
    else new NVector3(wrap1(elem), 1, wrap2(prefix1), len1+1, empty3, data2, suffix1, length+1)
  }

  override def foreach[U](f: A => U): Unit = {
    foreachElem(prefix1, f)
    foreachElem(data2, f)
    foreachElem(suffix1, f)
  }

  override def map[B](f: A => B): NVector[B] =
    copy(prefix1 = mapElems1(prefix1, f), data2 = mapElems(2, data2, f), suffix1 = mapElems1(suffix1, f))

  override def last: A = suffix1(suffix1.length-1).asInstanceOf[A]

  protected[this] def slice0(lo: Int, hi: Int): NVector[A] = {
    val b = new NVectorSliceBuilder(lo, hi)
    b.consider(1, prefix1)
    b.consider(2, data2)
    b.consider(1, suffix1)
    b.result()
  }

  override def tail: NVector[A] =
    if(len1 > 1) copy(copyTail(prefix1), len1-1, length = length-1)
    else slice0(1, length)

  override def init: NVector[A] =
    if(suffix1.length > 1) copy(suffix1 = copyInit(suffix1), length = length-1)
    else slice0(0, length-1)

  protected[immutable] def vectorSliceCount: Int = 3
  protected[immutable] def vectorSlice(idx: Int): Array[_ <: AnyRef] = (idx: @switch) match {
    case 0 => prefix1
    case 1 => data2
    case 2 => suffix1
  }
  protected[immutable] def vectorSlicePrefixLength(idx: Int): Int = (idx: @switch) match {
    case 0 => len1
    case 1 => length - suffix1.length
    case 2 => length
  }

  override protected[this] def appendedAll0[B >: A](suffix: collection.IterableOnce[B], k: Int): NVector[B] = {
    val suffix1b = append1IfSpace(suffix1, suffix)
    if(suffix1b ne null) copy(suffix1 = suffix1b, length = length-suffix1.length+suffix1b.length)
    else super.appendedAll0(suffix, k)
  }
}


/** 3-dimensional radix-balanced finger tree */
private final class NVector3[+A](_prefix1: Arr1, len1: Int,
                                 prefix2: Arr2, len12: Int,
                                 data3: Arr3,
                                 suffix2: Arr2, suffix1: Arr1,
                                 length: Int) extends NVector[A](_prefix1, length, 3) {
  import NVectorStatics._

  @inline private[this] def copy(prefix1: Arr1 = prefix1, len1: Int = len1,
                                 prefix2: Arr2 = prefix2, len12: Int = len12,
                                 data3: Arr3 = data3,
                                 suffix2: Arr2 = suffix2, suffix1: Arr1 = suffix1,
                                 length: Int = length) =
    new NVector3(prefix1, len1, prefix2, len12, data3, suffix2, suffix1, length)

  @inline def apply0(index: Int): A = {
    if(index < 0 || index >= length) throw ioob(index)
    if(index >= len12) {
      val io = index - len12
      val i3 = io >>> BITS2
      val i2 = (io >>> BITS) & MASK
      val i1 = io & MASK
      if(i3 < data3.length) data3(i3)(i2)(i1)
      else if(i2 < suffix2.length) suffix2(i2)(i1)
      else suffix1(i1)
    } else if(index >= len1) {
      val io = index - len1
      prefix2(io >>> BITS)(io & MASK)
    } else prefix1(index)
  }.asInstanceOf[A]

  override def updated[B >: A](index: Int, elem: B): NVector[B] = {
    if(index < 0 || index >= length) throw ioob(index)
    if(index >= len12) {
      val io = index - len12
      val i3 = io >>> BITS2
      val i2 = (io >>> BITS) & MASK
      val i1 = io & MASK
      if     (i3 < data3.length  ) copy(data3   = copyUpdate(data3, i3, i2, i1, elem))
      else if(i2 < suffix2.length) copy(suffix2 = copyUpdate(suffix2, i2, i1, elem))
      else                         copy(suffix1 = copyUpdate(suffix1, i1, elem))
    } else if(index >= len1) {
      val io = index - len1
      copy(prefix2 = copyUpdate(prefix2, io >>> BITS, io & MASK, elem))
    } else {
      copy(prefix1 = copyUpdate(prefix1, index, elem))
    }
  }

  override def appended[B >: A](elem: B): NVector[B] = {
    if     (suffix1.length < WIDTH  ) copy(suffix1 = copyAppend(suffix1, elem.asInstanceOf[AnyRef]), length = length+1)
    else if(suffix2.length < WIDTH-1) copy(suffix2 = copyAppend(suffix2, suffix1), suffix1 = wrap1(elem), length = length+1)
    else if(data3.length   < WIDTH-2) copy(data3 = copyAppend(data3, copyAppend(suffix2, suffix1)), suffix2 = empty2, suffix1 = wrap1(elem), length = length+1)
    else new NVector4(prefix1, len1, prefix2, len12, data3, (WIDTH-2)*WIDTH2 + len12, empty4, wrap3(copyAppend(suffix2, suffix1)), empty2, wrap1(elem), length+1)
  }

  override def prepended[B >: A](elem: B): NVector[B] = {
    if     (len1         < WIDTH  ) copy(prefix1 = copyPrepend1(elem, prefix1), len1 = len1+1, len12 = len12+1, length = length+1)
    else if(len12        < WIDTH2 ) copy(prefix1 = wrap1(elem), len1 = 1, prefix2 = copyPrepend(prefix1, prefix2), len12 = len12+1, length = length+1)
    else if(data3.length < WIDTH-2) copy(prefix1 = wrap1(elem), len1 = 1, prefix2 = empty2, len12 = 1, data3 = copyPrepend(copyPrepend(prefix1, prefix2), data3), length = length+1)
    else new NVector4(wrap1(elem), 1, empty2, 1, wrap3(copyPrepend(prefix1, prefix2)), len12+1, empty4, data3, suffix2, suffix1, length+1)
  }

  override def foreach[U](f: A => U): Unit = {
    foreachElem(prefix1, f)
    foreachElem(prefix2, f)
    foreachElem(data3, f)
    foreachElem(suffix2, f)
    foreachElem(suffix1, f)
  }

  override def map[B](f: A => B): NVector[B] =
    copy(prefix1 = mapElems1(prefix1, f), prefix2 = mapElems(2, prefix2, f),
      data3 = mapElems(3, data3, f),
      suffix2 = mapElems(2, suffix2, f), suffix1 = mapElems1(suffix1, f))

  override def last: A = suffix1(suffix1.length-1).asInstanceOf[A]

  protected[this] def slice0(lo: Int, hi: Int): NVector[A] = {
    val b = new NVectorSliceBuilder(lo, hi)
    b.consider(1, prefix1)
    b.consider(2, prefix2)
    b.consider(3, data3)
    b.consider(2, suffix2)
    b.consider(1, suffix1)
    b.result()
  }

  override def tail: NVector[A] =
    if(len1 > 1) copy(prefix1 = copyTail(prefix1), len1 = len1-1, len12 = len12-1, length = length-1)
    else slice0(1, length)

  override def init: NVector[A] =
    if(suffix1.length > 1) copy(suffix1 = copyInit(suffix1), length = length-1)
    else slice0(0, length-1)

  protected[immutable] def vectorSliceCount: Int = 5
  protected[immutable] def vectorSlice(idx: Int): Array[_ <: AnyRef] = (idx: @switch) match {
    case 0 => prefix1
    case 1 => prefix2
    case 2 => data3
    case 3 => suffix2
    case 4 => suffix1
  }
  protected[immutable] def vectorSlicePrefixLength(idx: Int): Int = (idx: @switch) match {
    case 0 => len1
    case 1 => len12
    case 2 => len12 + data3.length*WIDTH2
    case 3 => length - suffix1.length
    case 4 => length
  }

  override protected[this] def appendedAll0[B >: A](suffix: collection.IterableOnce[B], k: Int): NVector[B] = {
    val suffix1b = append1IfSpace(suffix1, suffix)
    if(suffix1b ne null) copy(suffix1 = suffix1b, length = length-suffix1.length+suffix1b.length)
    else super.appendedAll0(suffix, k)
  }
}


/** 4-dimensional radix-balanced finger tree */
private final class NVector4[+A](_prefix1: Arr1, len1: Int,
                                 prefix2: Arr2, len12: Int,
                                 prefix3: Arr3, len123: Int,
                                 data4: Arr4,
                                 suffix3: Arr3, suffix2: Arr2, suffix1: Arr1,
                                 length: Int) extends NVector[A](_prefix1, length, 4) {
  import NVectorStatics._

  @inline private[this] def copy(prefix1: Arr1 = prefix1, len1: Int = len1,
                                 prefix2: Arr2 = prefix2, len12: Int = len12,
                                 prefix3: Arr3 = prefix3, len123: Int = len123,
                                 data4: Arr4 = data4,
                                 suffix3: Arr3 = suffix3, suffix2: Arr2 = suffix2, suffix1: Arr1 = suffix1,
                                 length: Int = length) =
    new NVector4(prefix1, len1, prefix2, len12, prefix3, len123, data4, suffix3, suffix2, suffix1, length)

  @inline def apply0(index: Int): A = {
    if(index < 0 || index >= length) throw ioob(index)
    if(index >= len123) {
      val io = index - len123
      val i4 = io >>> BITS3
      val i3 = (io >>> BITS2) & MASK
      val i2 = (io >>> BITS) & MASK
      val i1 = io & MASK
      if(i4 < data4.length) data4(i4)(i3)(i2)(i1)
      else if(i3 < suffix3.length) suffix3(i3)(i2)(i1)
      else if(i2 < suffix2.length) suffix2(i2)(i1)
      else suffix1(i1)
    } else if(index >= len12) {
      val io = index - len12
      prefix3(io >>> BITS2)((io >>> BITS) & MASK)(io & MASK)
    } else if(index >= len1) {
      val io = index - len1
      prefix2(io >>> BITS)(io & MASK)
    } else prefix1(index)
  }.asInstanceOf[A]

  override def updated[B >: A](index: Int, elem: B): NVector[B] = {
    if(index < 0 || index >= length) throw ioob(index)
    if(index >= len123) {
      val io = index - len123
      val i4 = io >>> BITS3
      val i3 = (io >>> BITS2) & MASK
      val i2 = (io >>> BITS) & MASK
      val i1 = io & MASK
      if     (i4 < data4.length  ) copy(data4   = copyUpdate(data4, i4, i3, i2, i1, elem))
      else if(i3 < suffix3.length) copy(suffix3 = copyUpdate(suffix3, i3, i2, i1, elem))
      else if(i2 < suffix2.length) copy(suffix2 = copyUpdate(suffix2, i2, i1, elem))
      else                         copy(suffix1 = copyUpdate(suffix1, i1, elem))
    } else if(index >= len12) {
      val io = index - len12
      copy(prefix3 = copyUpdate(prefix3, io >>> BITS2, (io >>> BITS) & MASK, io & MASK, elem))
    } else if(index >= len1) {
      val io = index - len1
      copy(prefix2 = copyUpdate(prefix2, io >>> BITS, io & MASK, elem))
    } else {
      copy(prefix1 = copyUpdate(prefix1, index, elem))
    }
  }

  override def appended[B >: A](elem: B): NVector[B] = {
    if     (suffix1.length < WIDTH  ) copy(suffix1 = copyAppend(suffix1, elem.asInstanceOf[AnyRef]), length = length+1)
    else if(suffix2.length < WIDTH-1) copy(suffix2 = copyAppend(suffix2, suffix1), suffix1 = wrap1(elem), length = length+1)
    else if(suffix3.length < WIDTH-1) copy(suffix3 = copyAppend(suffix3, copyAppend(suffix2, suffix1)), suffix2 = empty2, suffix1 = wrap1(elem), length = length+1)
    else if(data4.length   < WIDTH-2) copy(data4   = copyAppend(data4, copyAppend(suffix3, copyAppend(suffix2, suffix1))), suffix3 = empty3, suffix2 = empty2, suffix1 = wrap1(elem), length = length+1)
    else new NVector5(prefix1, len1, prefix2, len12, prefix3, len123, data4, (WIDTH-2)*WIDTH3 + len123, empty5, wrap4(copyAppend(suffix3, copyAppend(suffix2, suffix1))), empty3, empty2, wrap1(elem), length+1)
  }

  override def prepended[B >: A](elem: B): NVector[B] = {
    if     (len1         < WIDTH  ) copy(copyPrepend1(elem, prefix1), len1+1, len12 = len12+1, len123 = len123+1, length = length+1)
    else if(len12        < WIDTH2 ) copy(wrap1(elem), 1, copyPrepend(prefix1, prefix2), len12+1, len123 = len123+1, length = length+1)
    else if(len123       < WIDTH3 ) copy(wrap1(elem), 1, empty2, 1, copyPrepend(copyPrepend(prefix1, prefix2), prefix3), len123+1, length = length+1)
    else if(data4.length < WIDTH-2) copy(wrap1(elem), 1, empty2, 1, empty3, 1, copyPrepend(copyPrepend(copyPrepend(prefix1, prefix2), prefix3), data4), length = length+1)
    else new NVector5(wrap1(elem), 1, empty2, 1, empty3, 1, wrap4(copyPrepend(copyPrepend(prefix1, prefix2), prefix3)), len123+1, empty5, data4, suffix3, suffix2, suffix1, length+1)
  }

  override def foreach[U](f: A => U): Unit = {
    foreachElem(prefix1, f)
    foreachElem(prefix2, f)
    foreachElem(prefix3, f)
    foreachElem(data4, f)
    foreachElem(suffix3, f)
    foreachElem(suffix2, f)
    foreachElem(suffix1, f)
  }

  override def map[B](f: A => B): NVector[B] =
    copy(prefix1 = mapElems1(prefix1, f), prefix2 = mapElems(2, prefix2, f), prefix3 = mapElems(3, prefix3, f),
      data4 = mapElems(4, data4, f),
      suffix3 = mapElems(3, suffix3, f), suffix2 = mapElems(2, suffix2, f), suffix1 = mapElems1(suffix1, f))

  override def last: A = suffix1(suffix1.length-1).asInstanceOf[A]

  protected[this] def slice0(lo: Int, hi: Int): NVector[A] = {
    val b = new NVectorSliceBuilder(lo, hi)
    b.consider(1, prefix1)
    b.consider(2, prefix2)
    b.consider(3, prefix3)
    b.consider(4, data4)
    b.consider(3, suffix3)
    b.consider(2, suffix2)
    b.consider(1, suffix1)
    b.result()
  }

  override def tail: NVector[A] =
    if(len1 > 1) copy(copyTail(prefix1), len1-1, len12 = len12-1, len123 = len123-1, length = length-1)
    else slice0(1, length)

  override def init: NVector[A] =
    if(suffix1.length > 1) copy(suffix1 = copyInit(suffix1), length = length-1)
    else slice0(0, length-1)

  protected[immutable] def vectorSliceCount: Int = 7
  protected[immutable] def vectorSlice(idx: Int): Array[_ <: AnyRef] = (idx: @switch) match {
    case 0 => prefix1
    case 1 => prefix2
    case 2 => prefix3
    case 3 => data4
    case 4 => suffix3
    case 5 => suffix2
    case 6 => suffix1
  }
  protected[immutable] def vectorSlicePrefixLength(idx: Int): Int = (idx: @switch) match {
    case 0 => len1
    case 1 => len12
    case 2 => len123
    case 3 => len123 + data4.length*WIDTH3
    case 4 => len123 + data4.length*WIDTH3 + suffix3.length*WIDTH2
    case 5 => length - suffix1.length
    case 6 => length
  }

  override protected[this] def appendedAll0[B >: A](suffix: collection.IterableOnce[B], k: Int): NVector[B] = {
    val suffix1b = append1IfSpace(suffix1, suffix)
    if(suffix1b ne null) copy(suffix1 = suffix1b, length = length-suffix1.length+suffix1b.length)
    else super.appendedAll0(suffix, k)
  }
}


/** 5-dimensional radix-balanced finger tree */
private final class NVector5[+A](_prefix1: Arr1, len1: Int,
                                 prefix2: Arr2, len12: Int,
                                 prefix3: Arr3, len123: Int,
                                 prefix4: Arr4, len1234: Int,
                                 data5: Arr5,
                                 suffix4: Arr4, suffix3: Arr3, suffix2: Arr2, suffix1: Arr1,
                                 length: Int) extends NVector[A](_prefix1, length, 5) {
  import NVectorStatics._

  @inline private[this] def copy(prefix1: Arr1 = prefix1, len1: Int = len1,
                                 prefix2: Arr2 = prefix2, len12: Int = len12,
                                 prefix3: Arr3 = prefix3, len123: Int = len123,
                                 prefix4: Arr4 = prefix4, len1234: Int = len1234,
                                 data5: Arr5 = data5,
                                 suffix4: Arr4 = suffix4, suffix3: Arr3 = suffix3, suffix2: Arr2 = suffix2, suffix1: Arr1 = suffix1,
                                 length: Int = length) =
    new NVector5(prefix1, len1, prefix2, len12, prefix3, len123, prefix4, len1234, data5, suffix4, suffix3, suffix2, suffix1, length)

  @inline def apply0(index: Int): A = {
    if(index < 0 || index >= length) throw ioob(index)
    if(index >= len1234) {
      val io = index - len1234
      val i5 = io >>> BITS4
      val i4 = (io >>> BITS3) & MASK
      val i3 = (io >>> BITS2) & MASK
      val i2 = (io >>> BITS) & MASK
      val i1 = io & MASK
      if(i5 < data5.length) data5(i5)(i4)(i3)(i2)(i1)
      else if(i4 < suffix4.length) suffix4(i4)(i3)(i2)(i1)
      else if(i3 < suffix3.length) suffix3(i3)(i2)(i1)
      else if(i2 < suffix2.length) suffix2(i2)(i1)
      else suffix1(i1)
    } else if(index >= len123) {
      val io = index - len123
      prefix4(io >>> BITS3)((io >>> BITS2) & MASK)((io >>> BITS) & MASK)(io & MASK)
    } else if(index >= len12) {
      val io = index - len12
      prefix3(io >>> BITS2)((io >>> BITS) & MASK)(io & MASK)
    } else if(index >= len1) {
      val io = index - len1
      prefix2(io >>> BITS)(io & MASK)
    } else prefix1(index)
  }.asInstanceOf[A]

  override def updated[B >: A](index: Int, elem: B): NVector[B] = {
    if(index < 0 || index >= length) throw ioob(index)
    if(index >= len1234) {
      val io = index - len1234
      val i5 = io >>> BITS4
      val i4 = (io >>> BITS3) & MASK
      val i3 = (io >>> BITS2) & MASK
      val i2 = (io >>> BITS) & MASK
      val i1 = io & MASK
      if     (i5 < data5.length  ) copy(data5   = copyUpdate(data5,   i5, i4, i3, i2, i1, elem))
      else if(i4 < suffix4.length) copy(suffix4 = copyUpdate(suffix4, i4, i3, i2, i1, elem))
      else if(i3 < suffix3.length) copy(suffix3 = copyUpdate(suffix3, i3, i2, i1, elem))
      else if(i2 < suffix2.length) copy(suffix2 = copyUpdate(suffix2, i2, i1, elem))
      else                         copy(suffix1 = copyUpdate(suffix1, i1, elem))
    } else if(index >= len123) {
      val io = index - len123
      copy(prefix4 = copyUpdate(prefix4, io >>> BITS3, (io >>> BITS2) & MASK, (io >>> BITS) & MASK, io & MASK, elem))
    } else if(index >= len12) {
      val io = index - len12
      copy(prefix3 = copyUpdate(prefix3, io >>> BITS2, (io >>> BITS) & MASK, io & MASK, elem))
    } else if(index >= len1) {
      val io = index - len1
      copy(prefix2 = copyUpdate(prefix2, io >>> BITS, io & MASK, elem))
    } else {
      copy(prefix1 = copyUpdate(prefix1, index, elem))
    }
  }

  override def appended[B >: A](elem: B): NVector[B] = {
    if     (suffix1.length < WIDTH  ) copy(suffix1 = copyAppend(suffix1, elem.asInstanceOf[AnyRef]), length = length+1)
    else if(suffix2.length < WIDTH-1) copy(suffix2 = copyAppend(suffix2, suffix1), suffix1 = wrap1(elem), length = length+1)
    else if(suffix3.length < WIDTH-1) copy(suffix3 = copyAppend(suffix3, copyAppend(suffix2, suffix1)), suffix2 = empty2, suffix1 = wrap1(elem), length = length+1)
    else if(suffix4.length < WIDTH-1) copy(suffix4 = copyAppend(suffix4, copyAppend(suffix3, copyAppend(suffix2, suffix1))), suffix3 = empty3, suffix2 = empty2, suffix1 = wrap1(elem), length = length+1)
    else if(data5.length   < WIDTH-2) copy(data5   = copyAppend(data5, copyAppend(suffix4, copyAppend(suffix3, copyAppend(suffix2, suffix1)))), suffix4 = empty4, suffix3 = empty3, suffix2 = empty2, suffix1 = wrap1(elem), length = length+1)
    else new NVector6(prefix1, len1, prefix2, len12, prefix3, len123, prefix4, len1234, data5, (WIDTH-2)*WIDTH4 + len1234, empty6, wrap5(copyAppend(suffix4, copyAppend(suffix3, copyAppend(suffix2, suffix1)))), empty4, empty3, empty2, wrap1(elem), length+1)
  }

  override def prepended[B >: A](elem: B): NVector[B] = {
    if     (len1         < WIDTH  ) copy(copyPrepend1(elem, prefix1), len1+1, len12 = len12+1, len123 = len123+1, len1234 = len1234+1, length = length+1)
    else if(len12        < WIDTH2 ) copy(wrap1(elem), 1, copyPrepend(prefix1, prefix2), len12+1, len123 = len123+1, len1234 = len1234+1, length = length+1)
    else if(len123       < WIDTH3 ) copy(wrap1(elem), 1, empty2, 1, copyPrepend(copyPrepend(prefix1, prefix2), prefix3), len123+1, len1234 = len1234+1, length = length+1)
    else if(len1234      < WIDTH4 ) copy(wrap1(elem), 1, empty2, 1, empty3, 1, copyPrepend(copyPrepend(copyPrepend(prefix1, prefix2), prefix3), prefix4), len1234+1, length = length+1)
    else if(data5.length < WIDTH-2) copy(wrap1(elem), 1, empty2, 1, empty3, 1, empty4, 1, copyPrepend(copyPrepend(copyPrepend(copyPrepend(prefix1, prefix2), prefix3), prefix4), data5), length = length+1)
    else new NVector6(wrap1(elem), 1, empty2, 1, empty3, 1, empty4, 1, wrap5(copyPrepend(copyPrepend(copyPrepend(prefix1, prefix2), prefix3), prefix4)), len1234+1, empty6, data5, suffix4, suffix3, suffix2, suffix1, length+1)
  }

  override def foreach[U](f: A => U): Unit = {
    foreachElem(prefix1, f)
    foreachElem(prefix2, f)
    foreachElem(prefix3, f)
    foreachElem(prefix4, f)
    foreachElem(data5, f)
    foreachElem(suffix4, f)
    foreachElem(suffix3, f)
    foreachElem(suffix2, f)
    foreachElem(suffix1, f)
  }

  override def map[B](f: A => B): NVector[B] =
    copy(prefix1 = mapElems1(prefix1, f), prefix2 = mapElems(2, prefix2, f), prefix3 = mapElems(3, prefix3, f), prefix4 = mapElems(4, prefix4, f),
      data5 = mapElems(5, data5, f),
      suffix4 = mapElems(4, suffix4, f), suffix3 = mapElems(3, suffix3, f), suffix2 = mapElems(2, suffix2, f), suffix1 = mapElems1(suffix1, f))

  override def last: A = suffix1(suffix1.length-1).asInstanceOf[A]

  protected[this] def slice0(lo: Int, hi: Int): NVector[A] = {
    val b = new NVectorSliceBuilder(lo, hi)
    b.consider(1, prefix1)
    b.consider(2, prefix2)
    b.consider(3, prefix3)
    b.consider(4, prefix4)
    b.consider(5, data5)
    b.consider(4, suffix4)
    b.consider(3, suffix3)
    b.consider(2, suffix2)
    b.consider(1, suffix1)
    b.result()
  }

  override def tail: NVector[A] =
    if(len1 > 1) copy(copyTail(prefix1), len1-1, len12 = len12-1, len123 = len123-1, len1234 = len1234-1, length = length-1)
    else slice0(1, length)

  override def init: NVector[A] =
    if(suffix1.length > 1) copy(suffix1 = copyInit(suffix1), length = length-1)
    else slice0(0, length-1)

  protected[immutable] def vectorSliceCount: Int = 9
  protected[immutable] def vectorSlice(idx: Int): Array[_ <: AnyRef] = (idx: @switch) match {
    case 0 => prefix1
    case 1 => prefix2
    case 2 => prefix3
    case 3 => prefix4
    case 4 => data5
    case 5 => suffix4
    case 6 => suffix3
    case 7 => suffix2
    case 8 => suffix1
  }
  protected[immutable] def vectorSlicePrefixLength(idx: Int): Int = (idx: @switch) match {
    case 0 => len1
    case 1 => len12
    case 2 => len123
    case 3 => len1234
    case 4 => len1234 + data5.length*WIDTH4
    case 5 => len1234 + data5.length*WIDTH4 + suffix4.length*WIDTH3
    case 6 => len1234 + data5.length*WIDTH4 + suffix4.length*WIDTH3 + suffix3.length*WIDTH2
    case 7 => length - suffix1.length
    case 8 => length
  }

  override protected[this] def appendedAll0[B >: A](suffix: collection.IterableOnce[B], k: Int): NVector[B] = {
    val suffix1b = append1IfSpace(suffix1, suffix)
    if(suffix1b ne null) copy(suffix1 = suffix1b, length = length-suffix1.length+suffix1b.length)
    else super.appendedAll0(suffix, k)
  }
}


/** 6-dimensional radix-balanced finger tree */
private final class NVector6[+A](_prefix1: Arr1, len1: Int,
                                 prefix2: Arr2, len12: Int,
                                 prefix3: Arr3, len123: Int,
                                 prefix4: Arr4, len1234: Int,
                                 prefix5: Arr5, len12345: Int,
                                 data6: Arr6,
                                 suffix5: Arr5, suffix4: Arr4, suffix3: Arr3, suffix2: Arr2, suffix1: Arr1,
                                 length: Int) extends NVector[A](_prefix1, length, 6) {
  import NVectorStatics._

  @inline private[this] def copy(prefix1: Arr1 = prefix1, len1: Int = len1,
                                 prefix2: Arr2 = prefix2, len12: Int = len12,
                                 prefix3: Arr3 = prefix3, len123: Int = len123,
                                 prefix4: Arr4 = prefix4, len1234: Int = len1234,
                                 prefix5: Arr5 = prefix5, len12345: Int = len12345,
                                 data6: Arr6 = data6,
                                 suffix5: Arr5 = suffix5, suffix4: Arr4 = suffix4, suffix3: Arr3 = suffix3, suffix2: Arr2 = suffix2, suffix1: Arr1 = suffix1,
                                 length: Int = length) =
    new NVector6(prefix1, len1, prefix2, len12, prefix3, len123, prefix4, len1234, prefix5, len12345, data6, suffix5, suffix4, suffix3, suffix2, suffix1, length)

  @inline def apply0(index: Int): A = {
    if(index < 0 || index >= length) throw ioob(index)
    if(index >= len12345) {
      val io = index - len12345
      val i6 = io >>> BITS5
      val i5 = (io >>> BITS4) & MASK
      val i4 = (io >>> BITS3) & MASK
      val i3 = (io >>> BITS2) & MASK
      val i2 = (io >>> BITS) & MASK
      val i1 = io & MASK
      if(i6 < data6.length) data6(i6)(i5)(i4)(i3)(i2)(i1)
      else if(i5 < suffix5.length) suffix5(i5)(i4)(i3)(i2)(i1)
      else if(i4 < suffix4.length) suffix4(i4)(i3)(i2)(i1)
      else if(i3 < suffix3.length) suffix3(i3)(i2)(i1)
      else if(i2 < suffix2.length) suffix2(i2)(i1)
      else suffix1(i1)
    } else if(index >= len1234) {
      val io = index - len1234
      prefix5(io >>> BITS4)((io >>> BITS3) & MASK)((io >>> BITS2) & MASK)((io >>> BITS) & MASK)(io & MASK)
    } else if(index >= len123) {
      val io = index - len123
      prefix4(io >>> BITS3)((io >>> BITS2) & MASK)((io >>> BITS) & MASK)(io & MASK)
    } else if(index >= len12) {
      val io = index - len12
      prefix3(io >>> BITS2)((io >>> BITS) & MASK)(io & MASK)
    } else if(index >= len1) {
      val io = index - len1
      prefix2(io >>> BITS)(io & MASK)
    } else prefix1(index)
  }.asInstanceOf[A]

  override def updated[B >: A](index: Int, elem: B): NVector[B] = {
    if(index < 0 || index >= length) throw ioob(index)
    if(index >= len12345) {
      val io = index - len12345
      val i6 = io >>> BITS5
      val i5 = (io >>> BITS4) & MASK
      val i4 = (io >>> BITS3) & MASK
      val i3 = (io >>> BITS2) & MASK
      val i2 = (io >>> BITS) & MASK
      val i1 = io & MASK
      if     (i6 < data6.length  ) copy(data6   = copyUpdate(data6,   i6, i5, i4, i3, i2, i1, elem))
      else if(i5 < suffix5.length) copy(suffix5 = copyUpdate(suffix5, i5, i4, i3, i2, i1, elem))
      else if(i4 < suffix4.length) copy(suffix4 = copyUpdate(suffix4, i4, i3, i2, i1, elem))
      else if(i3 < suffix3.length) copy(suffix3 = copyUpdate(suffix3, i3, i2, i1, elem))
      else if(i2 < suffix2.length) copy(suffix2 = copyUpdate(suffix2, i2, i1, elem))
      else                         copy(suffix1 = copyUpdate(suffix1, i1, elem))
    } else if(index >= len1234) {
      val io = index - len1234
      copy(prefix5 = copyUpdate(prefix5, io >>> BITS4, (io >>> BITS3) & MASK, (io >>> BITS2) & MASK, (io >>> BITS) & MASK, io & MASK, elem))
    } else if(index >= len123) {
      val io = index - len123
      copy(prefix4 = copyUpdate(prefix4, io >>> BITS3, (io >>> BITS2) & MASK, (io >>> BITS) & MASK, io & MASK, elem))
    } else if(index >= len12) {
      val io = index - len12
      copy(prefix3 = copyUpdate(prefix3, io >>> BITS2, (io >>> BITS) & MASK, io & MASK, elem))
    } else if(index >= len1) {
      val io = index - len1
      copy(prefix2 = copyUpdate(prefix2, io >>> BITS, io & MASK, elem))
    } else {
      copy(prefix1 = copyUpdate(prefix1, index, elem))
    }
  }

  override def appended[B >: A](elem: B): NVector[B] = {
    if     (suffix1.length < WIDTH      ) copy(suffix1 = copyAppend(suffix1, elem.asInstanceOf[AnyRef]), length = length+1)
    else if(suffix2.length < WIDTH-1    ) copy(suffix2 = copyAppend(suffix2, suffix1), suffix1 = wrap1(elem), length = length+1)
    else if(suffix3.length < WIDTH-1    ) copy(suffix3 = copyAppend(suffix3, copyAppend(suffix2, suffix1)), suffix2 = empty2, suffix1 = wrap1(elem), length = length+1)
    else if(suffix4.length < WIDTH-1    ) copy(suffix4 = copyAppend(suffix4, copyAppend(suffix3, copyAppend(suffix2, suffix1))), suffix3 = empty3, suffix2 = empty2, suffix1 = wrap1(elem), length = length+1)
    else if(suffix5.length < WIDTH-1    ) copy(suffix5 = copyAppend(suffix5, copyAppend(suffix4, copyAppend(suffix3, copyAppend(suffix2, suffix1)))), suffix4 = empty4, suffix3 = empty3, suffix2 = empty2, suffix1 = wrap1(elem), length = length+1)
    else if(data6.length   < LASTWIDTH-2) copy(data6   = copyAppend(data6, copyAppend(suffix5, copyAppend(suffix4, copyAppend(suffix3, copyAppend(suffix2, suffix1))))), suffix5 = empty5, suffix4 = empty4, suffix3 = empty3, suffix2 = empty2, suffix1 = wrap1(elem), length = length+1)
    else throw new IllegalArgumentException
  }

  override def prepended[B >: A](elem: B): NVector[B] = {
    if     (len1         < WIDTH      ) copy(copyPrepend1(elem, prefix1), len1+1, len12 = len12+1, len123 = len123+1, len1234 = len1234+1, len12345 = len12345+1, length = length+1)
    else if(len12        < WIDTH2     ) copy(wrap1(elem), 1, copyPrepend(prefix1, prefix2), len12+1, len123 = len123+1, len1234 = len1234+1, len12345 = len12345+1, length = length+1)
    else if(len123       < WIDTH3     ) copy(wrap1(elem), 1, empty2, 1, copyPrepend(copyPrepend(prefix1, prefix2), prefix3), len123+1, len1234 = len1234+1, len12345 = len12345+1, length = length+1)
    else if(len1234      < WIDTH4     ) copy(wrap1(elem), 1, empty2, 1, empty3, 1, copyPrepend(copyPrepend(copyPrepend(prefix1, prefix2), prefix3), prefix4), len1234+1, len12345 = len12345+1, length = length+1)
    else if(len12345     < WIDTH5     ) copy(wrap1(elem), 1, empty2, 1, empty3, 1, empty4, 1, copyPrepend(copyPrepend(copyPrepend(copyPrepend(prefix1, prefix2), prefix3), prefix4), prefix5), len12345+1, length = length+1)
    else if(data6.length < LASTWIDTH-2) copy(wrap1(elem), 1, empty2, 1, empty3, 1, empty4, 1, empty5, 1, copyPrepend(copyPrepend(copyPrepend(copyPrepend(copyPrepend(prefix1, prefix2), prefix3), prefix4), prefix5), data6), length = length+1)
    else throw new IllegalArgumentException
  }

  override def foreach[U](f: A => U): Unit = {
    foreachElem(prefix1, f)
    foreachElem(prefix2, f)
    foreachElem(prefix3, f)
    foreachElem(prefix4, f)
    foreachElem(prefix5, f)
    foreachElem(data6, f)
    foreachElem(suffix5, f)
    foreachElem(suffix4, f)
    foreachElem(suffix3, f)
    foreachElem(suffix2, f)
    foreachElem(suffix1, f)
  }

  override def map[B](f: A => B): NVector[B] =
    copy(prefix1 = mapElems1(prefix1, f), prefix2 = mapElems(2, prefix2, f), prefix3 = mapElems(3, prefix3, f), prefix4 = mapElems(4, prefix4, f), prefix5 = mapElems(5, prefix5, f),
      data6 = mapElems(6, data6, f),
      suffix5 = mapElems(5, suffix5, f), suffix4 = mapElems(4, suffix4, f), suffix3 = mapElems(3, suffix3, f), suffix2 = mapElems(2, suffix2, f), suffix1 = mapElems1(suffix1, f))

  override def last: A = suffix1(suffix1.length-1).asInstanceOf[A]

  protected[this] def slice0(lo: Int, hi: Int): NVector[A] = {
    val b = new NVectorSliceBuilder(lo, hi)
    b.consider(1, prefix1)
    b.consider(2, prefix2)
    b.consider(3, prefix3)
    b.consider(4, prefix4)
    b.consider(5, prefix5)
    b.consider(6, data6)
    b.consider(5, suffix5)
    b.consider(4, suffix4)
    b.consider(3, suffix3)
    b.consider(2, suffix2)
    b.consider(1, suffix1)
    b.result()
  }

  override def tail: NVector[A] =
    if(len1 > 1) copy(copyTail(prefix1), len1-1, len12 = len12-1, len123 = len123-1, len1234 = len1234-1, len12345 = len12345-1, length = length-1)
    else slice0(1, length)

  override def init: NVector[A] =
    if(suffix1.length > 1) copy(suffix1 = copyInit(suffix1), length = length-1)
    else slice0(0, length-1)

  protected[immutable] def vectorSliceCount: Int = 11
  protected[immutable] def vectorSlice(idx: Int): Array[_ <: AnyRef] = (idx: @switch) match {
    case 0 => prefix1
    case 1 => prefix2
    case 2 => prefix3
    case 3 => prefix4
    case 4 => prefix5
    case 5 => data6
    case 6 => suffix5
    case 7 => suffix4
    case 8 => suffix3
    case 9 => suffix2
    case 10 => suffix1
  }
  protected[immutable] def vectorSlicePrefixLength(idx: Int): Int = (idx: @switch) match {
    case 0 => len1
    case 1 => len12
    case 2 => len123
    case 3 => len1234
    case 4 => len12345
    case 5 => len12345 + data6.length*WIDTH5
    case 6 => len12345 + data6.length*WIDTH5 + suffix5.length*WIDTH4
    case 7 => len12345 + data6.length*WIDTH5 + suffix5.length*WIDTH4 + suffix4.length*WIDTH3
    case 8 => len12345 + data6.length*WIDTH5 + suffix5.length*WIDTH4 + suffix4.length*WIDTH3 + suffix3.length*WIDTH2
    case 9 => length - suffix1.length
    case 10 => length
  }

  override protected[this] def appendedAll0[B >: A](suffix: collection.IterableOnce[B], k: Int): NVector[B] = {
    val suffix1b = append1IfSpace(suffix1, suffix)
    if(suffix1b ne null) copy(suffix1 = suffix1b, length = length-suffix1.length+suffix1b.length)
    else super.appendedAll0(suffix, k)
  }
}


/** Helper class for vector slicing. It is initialized with the validated start and end index,
  * then the vector slices are added in succession with `consider`. No matter what the dimension
  * of the originating vector is or where the cut is performed, this always results in a
  * structure with the highest-dimensional data in the middle and fingers of decreasing dimension
  * at both ends, which can be turned into a new vector with very little rebalancing.
  */
private[immutable] final class NVectorSliceBuilder(lo: Int, hi: Int) {
  //println(s"***** NVectorSliceBuilder($lo, $hi)")
  import NVectorStatics._

  private[this] val slices = new Array[Array[AnyRef]](11)
  private[this] var len, pos, maxDim = 0

  @inline private[this] def prefixIdx(n: Int) = n-1
  @inline private[this] def suffixIdx(n: Int) = 11-n

  def consider[T <: AnyRef](n: Int, a: Array[T]): Unit = {
    //println(s"*****   consider($n, /${a.length})")
    val count = a.length * (1 << (BITS*(n-1)))
    val lo0 = mmax(lo-pos, 0)
    val hi0 = mmin(hi-pos, count)
    if(hi0 > lo0) addSlice(n, a, lo0, hi0)
    pos += count
  }

  private[this] def addSlice[T <: AnyRef](n: Int, a: Array[T], lo: Int, hi: Int): Unit = {
    //println(s"*****     addSlice($n, /${a.length}, $lo, $hi)")
    if(n == 1) {
      add(1, copyOrUse(a, lo, hi))
    } else {
      val bitsN = BITS * (n-1)
      var loN = lo >>> bitsN
      var hiN = hi >>> bitsN
      if(n != 6) {
        loN &= MASK
        hiN &= MASK
      }
      val loRest = lo & ((1 << bitsN) - 1)
      val hiRest = hi & ((1 << bitsN) - 1)
      //println(s"*****       bitsN=$bitsN, loN=$loN, hiN=$hiN, loRest=$loRest, hiRest=$hiRest")
      if(loRest == 0) {
        if(hiRest == 0) {
          add(n, copyOrUse(a, loN, hiN))
        } else {
          if(hiN > loN) add(n, copyOrUse(a, loN, hiN))
          addSlice(n-1, a(hiN).asInstanceOf[Array[AnyRef]], 0, hiRest)
        }
      } else {
        addSlice(n-1, a(loN).asInstanceOf[Array[AnyRef]], loRest, 1 << bitsN)
        if(hiRest == 0) {
          if(hiN > loN+1) add(n, copyOrUse(a, loN+1, hiN))
        } else {
          if(hiN-1 > loN+1) add(n, copyOrUse(a, loN+1, hiN-1))
          addSlice(n-1, a(hiN-1).asInstanceOf[Array[AnyRef]], 0, hiRest)
        }
      }
    }
  }

  private[this] def add[T <: AnyRef](n: Int, a: Array[T]): Unit = {
    val idx =
      if(n <= maxDim) suffixIdx(n)
      else { maxDim = n; prefixIdx(n) }
    slices(idx) = a.asInstanceOf[Array[AnyRef]]
    len += (a.length * (1 << (BITS*(n-1))))
  }

  def result[A](): NVector[A] = {
    if(len <= 32) {
      if(len == 0) NVector0
      else {
        val prefix1 = slices(prefixIdx(1))
        val suffix1 = slices(suffixIdx(1))
        val a: Arr1 =
          if(prefix1 ne null) {
            if(suffix1 ne null) concatArrays(prefix1, suffix1)
            else prefix1
          } else if(suffix1 ne null) suffix1
          else {
            val prefix2 = slices(prefixIdx(2)).asInstanceOf[Arr2]
            if(prefix2 ne null) prefix2(0)
            else {
              val suffix2 = slices(suffixIdx(2)).asInstanceOf[Arr2]
              suffix2(0)
            }
          }
        new NVector1(a)
      }
    } else {
      balancePrefix(1)
      balanceSuffix(1)
      var resultDim = maxDim
      if(resultDim < 6) {
        val pre = slices(prefixIdx(maxDim))
        val suf = slices(suffixIdx(maxDim))
        if((pre ne null) && (suf ne null)) {
          // The highest-dimensional data consists of two slices: concatenate if they fit into the main data array,
          // otherwise increase the dimension
          if(pre.length + suf.length <= WIDTH-2) {
            slices(prefixIdx(maxDim)) = concatArrays(pre, suf)
            slices(suffixIdx(maxDim)) = null
          } else resultDim += 1
        } else {
          // A single highest-dimensional slice could have length WIDTH-1 if it came from a prefix or suffix but we
          // only allow WIDTH-2 for the main data, so increase the dimension in this case
          val one = if(pre ne null) pre else suf
          if(one.length > WIDTH-2) resultDim += 1
        }
      }
      val prefix1 = slices(prefixIdx(1))
      val suffix1 = slices(suffixIdx(1))
      val len1 = prefix1.length
      val res = (resultDim: @switch) match {
        case 2 =>
          val data2 = dataOr(2, empty2)
          new NVector2[A](prefix1, len1, data2, suffix1, len)
        case 3 =>
          val prefix2 = prefixOr(2, empty2)
          val data3 = dataOr(3, empty3)
          val suffix2 = suffixOr(2, empty2)
          val len12 = len1 + (prefix2.length * WIDTH)
          new NVector3[A](prefix1, len1, prefix2, len12, data3, suffix2, suffix1, len)
        case 4 =>
          val prefix2 = prefixOr(2, empty2)
          val prefix3 = prefixOr(3, empty3)
          val data4 = dataOr(4, empty4)
          val suffix3 = suffixOr(3, empty3)
          val suffix2 = suffixOr(2, empty2)
          val len12 = len1 + (prefix2.length * WIDTH)
          val len123 = len12 + (prefix3.length * WIDTH2)
          new NVector4[A](prefix1, len1, prefix2, len12, prefix3, len123, data4, suffix3, suffix2, suffix1, len)
        case 5 =>
          val prefix2 = prefixOr(2, empty2)
          val prefix3 = prefixOr(3, empty3)
          val prefix4 = prefixOr(4, empty4)
          val data5 = dataOr(5, empty5)
          val suffix4 = suffixOr(4, empty4)
          val suffix3 = suffixOr(3, empty3)
          val suffix2 = suffixOr(2, empty2)
          val len12 = len1 + (prefix2.length * WIDTH)
          val len123 = len12 + (prefix3.length * WIDTH2)
          val len1234 = len123 + (prefix4.length * WIDTH3)
          new NVector5[A](prefix1, len1, prefix2, len12, prefix3, len123, prefix4, len1234, data5, suffix4, suffix3, suffix2, suffix1, len)
        case 6 =>
          val prefix2 = prefixOr(2, empty2)
          val prefix3 = prefixOr(3, empty3)
          val prefix4 = prefixOr(4, empty4)
          val prefix5 = prefixOr(5, empty5)
          val data6 = dataOr(6, empty6)
          val suffix5 = suffixOr(5, empty5)
          val suffix4 = suffixOr(4, empty4)
          val suffix3 = suffixOr(3, empty3)
          val suffix2 = suffixOr(2, empty2)
          val len12 = len1 + (prefix2.length * WIDTH)
          val len123 = len12 + (prefix3.length * WIDTH2)
          val len1234 = len123 + (prefix4.length * WIDTH3)
          val len12345 = len1234 + (prefix5.length * WIDTH4)
          new NVector6[A](prefix1, len1, prefix2, len12, prefix3, len123, prefix4, len1234, prefix5, len12345, data6, suffix5, suffix4, suffix3, suffix2, suffix1, len)
      }
      res
    }
  }

  @inline private[this] def prefixOr[T <: AnyRef](n: Int, a: Array[T]): Array[T] = {
    val p = slices(prefixIdx(n))
    if(p ne null) p.asInstanceOf[Array[T]] else a
  }

  @inline private[this] def suffixOr[T <: AnyRef](n: Int, a: Array[T]): Array[T] = {
    val s = slices(suffixIdx(n))
    if(s ne null) s.asInstanceOf[Array[T]] else a
  }

  @inline private[this] def dataOr[T <: AnyRef](n: Int, a: Array[T]): Array[T] = {
    val p = slices(prefixIdx(n))
    if(p ne null) p.asInstanceOf[Array[T]]
    else {
      val s = slices(suffixIdx(n))
      if(s ne null) s.asInstanceOf[Array[T]] else a
    }
  }

  /** Ensure prefix is not empty */
  private[this] def balancePrefix(n: Int): Unit = {
    if(slices(prefixIdx(n)) eq null) {
      if(n == maxDim) {
        slices(prefixIdx(n)) = slices(suffixIdx(n))
        slices(suffixIdx(n)) = null
      } else {
        balancePrefix(n+1)
        val preN1 = slices(prefixIdx(n+1)).asInstanceOf[Array[Array[AnyRef]]]
        //assert(preN1 ne null)
        slices(prefixIdx(n)) = preN1(0)
        if(preN1.length == 1) {
          slices(prefixIdx(n+1)) = null
          if((maxDim == n+1) && (slices(suffixIdx(n+1)) eq null)) maxDim = n
        } else {
          slices(prefixIdx(n+1)) = copyOfRange(preN1, 1, preN1.length).asInstanceOf[Array[AnyRef]]
        }
      }
    }
  }

  /** Ensure suffix is not empty */
  private[this] def balanceSuffix(n: Int): Unit = {
    if(slices(suffixIdx(n)) eq null) {
      if(n == maxDim) {
        slices(suffixIdx(n)) = slices(prefixIdx(n))
        slices(prefixIdx(n)) = null
      } else {
        balanceSuffix(n+1)
        val sufN1 = slices(suffixIdx(n+1)).asInstanceOf[Array[Array[AnyRef]]]
        //assert(sufN1 ne null, s"n=$n, maxDim=$maxDim, slices=${slices.mkString(",")}")
        slices(suffixIdx(n)) = sufN1(sufN1.length-1)
        if(sufN1.length == 1) {
          slices(suffixIdx(n+1)) = null
          if((maxDim == n+1) && (slices(prefixIdx(n+1)) eq null)) maxDim = n
        } else {
          slices(suffixIdx(n+1)) = copyOfRange(sufN1, 0, sufN1.length-1).asInstanceOf[Array[AnyRef]]
        }
      }
    }
  }

  override def toString: String =
    s"NVectorSliceBuilder(lo=$lo, hi=$hi, len=$len, pos=$pos, maxDim=$maxDim)"

  private[immutable] def getSlices: Array[Array[AnyRef]] = slices
}


private final class NVectorBuilder[A] extends ReusableBuilder[A, NVector[A]] {
  import NVectorStatics._

  private[this] var a6: Arr6 = _
  private[this] var a5: Arr5 = _
  private[this] var a4: Arr4 = _
  private[this] var a3: Arr3 = _
  private[this] var a2: Arr2 = _
  private[this] var a1: Arr1 = new Arr1(WIDTH)
  private[this] var len1, lenRest, offset = 0
  private[this] var depth = 1

  @inline private[this] final def setLen(i: Int): Unit = {
    len1 = i & MASK
    lenRest = i - len1
  }

  override def knownSize: Int = len1 + lenRest - offset

  def clear(): Unit = {
    a6 = null
    a5 = null
    a4 = null
    a3 = null
    a2 = null
    a1 = new Arr1(WIDTH)
    len1 = 0
    lenRest = 0
    offset = 0
    depth = 1
  }

  def initSparse(size: Int, elem: A): Unit = {
    setLen(size)
    Arrays.fill(a1, elem)
    if(size > WIDTH) {
      a2 = new Array(WIDTH)
      Arrays.fill(a2.asInstanceOf[Array[AnyRef]], a1)
      if(size > WIDTH2) {
        a3 = new Array(WIDTH)
        Arrays.fill(a3.asInstanceOf[Array[AnyRef]], a2)
        if(size > WIDTH3) {
          a4 = new Array(WIDTH)
          Arrays.fill(a4.asInstanceOf[Array[AnyRef]], a3)
          if(size > WIDTH4) {
            a5 = new Array(WIDTH)
            Arrays.fill(a5.asInstanceOf[Array[AnyRef]], a4)
            if(size > WIDTH5) {
              a6 = new Array(LASTWIDTH)
              Arrays.fill(a6.asInstanceOf[Array[AnyRef]], a5)
              depth = 6
            } else depth = 5
          } else depth = 4
        } else depth = 3
      } else depth = 2
    } else depth = 1
  }

  def initFrom(v: NVector[_]): this.type = {
    (v.vectorSliceCount: @switch) match {
      case 0 =>
      case 1 =>
        depth = 1
        setLen(v.length)
        a1 = copyOrUse(v.vectorSlice(0).asInstanceOf[Arr1], 0, WIDTH)
      case 3 =>
        val p1 = v.vectorSlice(0).asInstanceOf[Arr1]
        val d2 = v.vectorSlice(1).asInstanceOf[Arr2]
        a1 = copyOrUse(v.vectorSlice(2).asInstanceOf[Arr1], 0, WIDTH)
        depth = 2
        offset = WIDTH - p1.length
        setLen(v.length + offset)
        a2 = new Arr2(WIDTH)
        a2(0) = p1
        System.arraycopy(d2, 0, a2, 1, d2.length)
        a2(d2.length+1) = a1
      case 5 =>
        val p1 = v.vectorSlice(0).asInstanceOf[Arr1]
        val p2 = v.vectorSlice(1).asInstanceOf[Arr2]
        val d3 = v.vectorSlice(2).asInstanceOf[Arr3]
        val s2 = v.vectorSlice(3).asInstanceOf[Arr2]
        a1 = copyOrUse(v.vectorSlice(4).asInstanceOf[Arr1], 0, WIDTH)
        depth = 3
        offset = WIDTH2 - p1.length - WIDTH*p2.length
        setLen(v.length + offset)
        a3 = new Arr3(WIDTH)
        a3(0) = copyPrepend(p1, p2)
        System.arraycopy(d3, 0, a3, 1, d3.length)
        a2 = copyOf(s2, WIDTH)
        a3(d3.length+1) = a2
        a2(s2.length) = a1
      case 7 =>
        val p1 = v.vectorSlice(0).asInstanceOf[Arr1]
        val p2 = v.vectorSlice(1).asInstanceOf[Arr2]
        val p3 = v.vectorSlice(2).asInstanceOf[Arr3]
        val d4 = v.vectorSlice(3).asInstanceOf[Arr4]
        val s3 = v.vectorSlice(4).asInstanceOf[Arr3]
        val s2 = v.vectorSlice(5).asInstanceOf[Arr2]
        a1 = copyOrUse(v.vectorSlice(6).asInstanceOf[Arr1], 0, WIDTH)
        depth = 4
        offset = WIDTH3 - p1.length - WIDTH*p2.length - WIDTH2*p3.length
        setLen(v.length + offset)
        a4 = new Arr4(WIDTH)
        a4(0) = copyPrepend(copyPrepend(p1, p2), p3)
        System.arraycopy(d4, 0, a4, 1, d4.length)
        a3 = copyOf(s3, WIDTH)
        a2 = copyOf(s2, WIDTH)
        a4(d4.length+1) = a3
        a3(s3.length) = a2
        a2(s2.length) = a1
      case 9 =>
        val p1 = v.vectorSlice(0).asInstanceOf[Arr1]
        val p2 = v.vectorSlice(1).asInstanceOf[Arr2]
        val p3 = v.vectorSlice(2).asInstanceOf[Arr3]
        val p4 = v.vectorSlice(3).asInstanceOf[Arr4]
        val d5 = v.vectorSlice(4).asInstanceOf[Arr5]
        val s4 = v.vectorSlice(5).asInstanceOf[Arr4]
        val s3 = v.vectorSlice(6).asInstanceOf[Arr3]
        val s2 = v.vectorSlice(7).asInstanceOf[Arr2]
        a1 = copyOrUse(v.vectorSlice(8).asInstanceOf[Arr1], 0, WIDTH)
        depth = 5
        offset = WIDTH4 - p1.length - WIDTH*p2.length - WIDTH2*p3.length - WIDTH3*p4.length
        setLen(v.length + offset)
        a5 = new Arr5(WIDTH)
        a5(0) = copyPrepend(copyPrepend(copyPrepend(p1, p2), p3), p4)
        System.arraycopy(d5, 0, a5, 1, d5.length)
        a4 = copyOf(s4, WIDTH)
        a3 = copyOf(s3, WIDTH)
        a2 = copyOf(s2, WIDTH)
        a5(d5.length+1) = a4
        a4(s4.length) = a3
        a3(s3.length) = a2
        a2(s2.length) = a1
      case 11 =>
        val p1 = v.vectorSlice(0).asInstanceOf[Arr1]
        val p2 = v.vectorSlice(1).asInstanceOf[Arr2]
        val p3 = v.vectorSlice(2).asInstanceOf[Arr3]
        val p4 = v.vectorSlice(3).asInstanceOf[Arr4]
        val p5 = v.vectorSlice(4).asInstanceOf[Arr5]
        val d6 = v.vectorSlice(5).asInstanceOf[Arr6]
        val s5 = v.vectorSlice(6).asInstanceOf[Arr5]
        val s4 = v.vectorSlice(7).asInstanceOf[Arr4]
        val s3 = v.vectorSlice(8).asInstanceOf[Arr3]
        val s2 = v.vectorSlice(9).asInstanceOf[Arr2]
        a1 = copyOrUse(v.vectorSlice(10).asInstanceOf[Arr1], 0, WIDTH)
        depth = 6
        offset = WIDTH5 - p1.length - WIDTH*p2.length - WIDTH2*p3.length - WIDTH3*p4.length - WIDTH4*p5.length
        setLen(v.length + offset)
        a6 = new Arr6(WIDTH)
        a6(0) = copyPrepend(copyPrepend(copyPrepend(copyPrepend(p1, p2), p3), p4), p5)
        System.arraycopy(d6, 0, a6, 1, d6.length)
        a5 = copyOf(s5, WIDTH)
        a4 = copyOf(s4, WIDTH)
        a3 = copyOf(s3, WIDTH)
        a2 = copyOf(s2, WIDTH)
        a6(d6.length+1) = a5
        a5(s5.length) = a4
        a4(s4.length) = a3
        a3(s3.length) = a2
        a2(s2.length) = a1
    }
    if(len1 == 0 && lenRest > 0) {
      // force advance() on next addition:
      len1 = WIDTH
      lenRest -= WIDTH
    }
    this
  }

  def addOne(elem: A): this.type = {
    if(len1 == WIDTH) advance()
    a1(len1) = elem.asInstanceOf[AnyRef]
    len1 += 1
    this
  }

  private[this] def addArr1(data: Arr1): Unit = {
    val dl = data.length
    if(dl > 0) {
      if(len1 == WIDTH) advance()
      val copy1 = mmin(WIDTH-len1, dl)
      val copy2 = dl - copy1
      System.arraycopy(data, 0, a1, len1, copy1)
      len1 += copy1
      if(copy2 > 0) {
        advance()
        System.arraycopy(data, copy1, a1, 0, copy2)
        len1 += copy2
      }
    }
  }

  private[this] def addVector(xs: NVector[A]): this.type = {
    val sliceCount = xs.vectorSliceCount
    var sliceIdx = 0
    while(sliceIdx < sliceCount) {
      val slice = xs.vectorSlice(sliceIdx)
      xs.vectorSliceDim(sliceIdx) match {
        case 1 => addArr1(slice.asInstanceOf[Arr1])
        case n => foreachRec(n-2, slice, addArr1)
      }
      sliceIdx += 1
    }
    this
  }

  override def addAll(xs: IterableOnce[A]): this.type = xs match {
    case v: NVector[_] =>
      if(len1 == 0 && lenRest == 0) initFrom(v)
      else addVector(v)
    case _ =>
      super.addAll(xs)
  }

  private[this] def advance(): Unit = {
    val idx = lenRest + WIDTH
    val xor = idx ^ lenRest
    lenRest = idx
    len1 = 0
    advance1(idx, xor)
  }

  private[this] def advance1(idx: Int, xor: Int): Unit = {
    if (xor < WIDTH2) { // level = 1
      if (depth == 1) { a2 = new Array(WIDTH); a2(0) = a1; depth += 1 }
      a1 = new Array(WIDTH)
      a2((idx >>> BITS) & MASK) = a1
    } else if (xor < WIDTH3) { // level = 2
      if (depth == 2) { a3 = new Array(WIDTH); a3(0) = a2; depth += 1 }
      a1 = new Array(WIDTH)
      a2 = new Array(WIDTH)
      a2((idx >>> BITS) & MASK) = a1
      a3((idx >>> BITS2) & MASK) = a2
    } else if (xor < WIDTH4) { // level = 3
      if (depth == 3) { a4 = new Array(WIDTH); a4(0) = a3; depth += 1 }
      a1 = new Array(WIDTH)
      a2 = new Array(WIDTH)
      a3 = new Array(WIDTH)
      a2((idx >>> BITS) & MASK) = a1
      a3((idx >>> BITS2) & MASK) = a2
      a4((idx >>> BITS3) & MASK) = a3
    } else if (xor < WIDTH5) { // level = 4
      if (depth == 4) { a5 = new Array(WIDTH); a5(0) = a4; depth += 1 }
      a1 = new Array(WIDTH)
      a2 = new Array(WIDTH)
      a3 = new Array(WIDTH)
      a4 = new Array(WIDTH)
      a2((idx >>> BITS) & MASK) = a1
      a3((idx >>> BITS2) & MASK) = a2
      a4((idx >>> BITS3) & MASK) = a3
      a5((idx >>> BITS4) & MASK) = a4
    } else if (xor < WIDTH6) { // level = 5
      if (depth == 5) { a6 = new Array(LASTWIDTH); a6(0) = a5; depth += 1 }
      a1 = new Array(WIDTH)
      a2 = new Array(WIDTH)
      a3 = new Array(WIDTH)
      a4 = new Array(WIDTH)
      a5 = new Array(WIDTH)
      a2((idx >>> BITS) & MASK) = a1
      a3((idx >>> BITS2) & MASK) = a2
      a4((idx >>> BITS3) & MASK) = a3
      a5((idx >>> BITS4) & MASK) = a4
      a6((idx >>> BITS5) & MASK) = a5
    } else {                      // level = 6
      throw new IllegalArgumentException(s"advance1($idx, $xor): a1=$a1, a2=$a2, a3=$a3, a4=$a4, a5=$a5, a6=$a6, depth=$depth")
    }
  }

  def result(): NVector[A] = {
    val len = len1 + lenRest
    val realLen = len - offset
    if(realLen == 0) NVector.empty
    else if(len <= WIDTH) {
      if(realLen == WIDTH) new NVector1(a1)
      else new NVector1(copyOf(a1, realLen))
    } else if(len <= WIDTH2) {
      val i1 = (len-1) & MASK
      val i2 = (len-1) >>> BITS
      val data = copyOfRange(a2, 1, i2)
      val prefix1 = a2(0)
      val suffix1 = copyIfDifferentSize(a2(i2), i1+1)
      new NVector2(prefix1, WIDTH-offset, data, suffix1, realLen)
    } else if(len <= WIDTH3) {
      val i1 = (len-1) & MASK
      val i2 = ((len-1) >>> BITS) & MASK
      val i3 = ((len-1) >>> BITS2)
      val data = copyOfRange(a3, 1, i3)
      val prefix2 = copyTail(a3(0))
      val prefix1 = a3(0)(0)
      val suffix2 = copyOf(a3(i3), i2)
      val suffix1 = copyIfDifferentSize(a3(i3)(i2), i1+1)
      val len1 = prefix1.length
      val len12 = len1 + prefix2.length*WIDTH
      new NVector3(prefix1, len1, prefix2, len12, data, suffix2, suffix1, realLen)
    } else if(len <= WIDTH4) {
      val i1 = (len-1) & MASK
      val i2 = ((len-1) >>> BITS) & MASK
      val i3 = ((len-1) >>> BITS2) & MASK
      val i4 = ((len-1) >>> BITS3)
      val data = copyOfRange(a4, 1, i4)
      val prefix3 = copyTail(a4(0))
      val prefix2 = copyTail(a4(0)(0))
      val prefix1 = a4(0)(0)(0)
      val suffix3 = copyOf(a4(i4), i3)
      val suffix2 = copyOf(a4(i4)(i3), i2)
      val suffix1 = copyIfDifferentSize(a4(i4)(i3)(i2), i1+1)
      val len1 = prefix1.length
      val len12 = len1 + prefix2.length*WIDTH
      val len123 = len12 + prefix3.length*WIDTH2
      new NVector4(prefix1, len1, prefix2, len12, prefix3, len123, data, suffix3, suffix2, suffix1, realLen)
    } else if(len <= WIDTH5) {
      val i1 = (len-1) & MASK
      val i2 = ((len-1) >>> BITS) & MASK
      val i3 = ((len-1) >>> BITS2) & MASK
      val i4 = ((len-1) >>> BITS3) & MASK
      val i5 = ((len-1) >>> BITS4)
      val data = copyOfRange(a5, 1, i5)
      val prefix4 = copyTail(a5(0))
      val prefix3 = copyTail(a5(0)(0))
      val prefix2 = copyTail(a5(0)(0)(0))
      val prefix1 = a5(0)(0)(0)(0)
      val suffix4 = copyOf(a5(i5), i4)
      val suffix3 = copyOf(a5(i5)(i4), i3)
      val suffix2 = copyOf(a5(i5)(i4)(i3), i2)
      val suffix1 = copyIfDifferentSize(a5(i5)(i4)(i3)(i2), i1+1)
      val len1 = prefix1.length
      val len12 = len1 + prefix2.length*WIDTH
      val len123 = len12 + prefix3.length*WIDTH2
      val len1234 = len123 + prefix4.length*WIDTH3
      new NVector5(prefix1, len1, prefix2, len12, prefix3, len123, prefix4, len1234, data, suffix4, suffix3, suffix2, suffix1, realLen)
    } else {
      val i1 = (len-1) & MASK
      val i2 = ((len-1) >>> BITS) & MASK
      val i3 = ((len-1) >>> BITS2) & MASK
      val i4 = ((len-1) >>> BITS3) & MASK
      val i5 = ((len-1) >>> BITS4) & MASK
      val i6 = ((len-1) >>> BITS5)
      val data = copyOfRange(a6, 1, i6)
      val prefix5 = copyTail(a6(0))
      val prefix4 = copyTail(a6(0)(0))
      val prefix3 = copyTail(a6(0)(0)(0))
      val prefix2 = copyTail(a6(0)(0)(0)(0))
      val prefix1 = a6(0)(0)(0)(0)(0)
      val suffix5 = copyOf(a6(i6), i5)
      val suffix4 = copyOf(a6(i6)(i5), i4)
      val suffix3 = copyOf(a6(i6)(i5)(i4), i3)
      val suffix2 = copyOf(a6(i6)(i5)(i4)(i3), i2)
      val suffix1 = copyIfDifferentSize(a6(i6)(i5)(i4)(i3)(i2), i1+1)
      val len1 = prefix1.length
      val len12 = len1 + prefix2.length*WIDTH
      val len123 = len12 + prefix3.length*WIDTH2
      val len1234 = len123 + prefix4.length*WIDTH3
      val len12345 = len1234 + prefix5.length*WIDTH4
      new NVector6(prefix1, len1, prefix2, len12, prefix3, len123, prefix4, len1234, prefix5, len12345, data, suffix5, suffix4, suffix3, suffix2, suffix1, realLen)
    }
  }

  override def toString: String =
    s"NVectorBuilder(len1=$len1, lenRest=$lenRest, offset=$offset, depth=$depth)"

  private[immutable] def getData: Array[Array[_]] = Array(a1, a2, a3, a4, a5, a6)
}


private[immutable] object NVectorStatics {
  // compile-time numeric constants
  final val BITS = 5
  final val WIDTH = 1 << BITS
  final val MASK = WIDTH - 1
  final val BITS2 = BITS * 2
  final val WIDTH2 = 1 << BITS2
  final val BITS3 = BITS * 3
  final val WIDTH3 = 1 << BITS3
  final val BITS4 = BITS * 4
  final val WIDTH4 = 1 << BITS4
  final val BITS5 = BITS * 5
  final val WIDTH5 = 1 << BITS5
  final val BITS6 = BITS * 6
  final val WIDTH6 = 1 << BITS6
  final val LASTWIDTH = WIDTH << 1 // 1 extra bit in the last level to go up to Int.MaxValue (2^31-1) instead of 2^30:

  type Arr1 = Array[AnyRef]
  type Arr2 = Array[Array[AnyRef]]
  type Arr3 = Array[Array[Array[AnyRef]]]
  type Arr4 = Array[Array[Array[Array[AnyRef]]]]
  type Arr5 = Array[Array[Array[Array[Array[AnyRef]]]]]
  type Arr6 = Array[Array[Array[Array[Array[Array[AnyRef]]]]]]

  @inline def copyOrUse[T <: AnyRef](a: Array[T], start: Int, end: Int): Array[T] =
    if(start == 0 && end == a.length) a else copyOfRange[T](a, start, end)

  final def copyAppend[T <: AnyRef](a: Array[T], elem: T): Array[T] = {
    val ac = copyOf(a, a.length+1)
    ac(ac.length-1) = elem
    ac
  }

  final def copyPrepend1(elem: Any, a: Arr1): Arr1 = {
    val ac = new Arr1(a.length+1)
    System.arraycopy(a, 0, ac, 1, a.length)
    ac(0) = elem.asInstanceOf[AnyRef]
    ac
  }

  final def copyPrepend[T <: AnyRef](elem: T, a: Array[T]): Array[T] = {
    val ac = java.lang.reflect.Array.newInstance(a.getClass.getComponentType, a.length+1).asInstanceOf[Array[T]]
    System.arraycopy(a, 0, ac, 1, a.length)
    ac(0) = elem
    ac
  }

  final def append1IfSpace(suffix1: Arr1, xs: IterableOnce[_]): Arr1 = xs match {
    case it: Iterable[_] =>
      if(it.sizeCompare(WIDTH-suffix1.length) <= 0) {
        it.size match {
          case 0 => null
          case 1 => copyAppend(suffix1, it.head.asInstanceOf[AnyRef])
          case s =>
            val suffix1b = copyOf(suffix1, suffix1.length + s)
            it.copyToArray(suffix1b.asInstanceOf[Array[Any]], suffix1.length)
            suffix1b
        }
      } else null
    case it =>
      val s = it.knownSize
      if(s > 0 && s <= WIDTH-suffix1.length) {
        val suffix1b = copyOf(suffix1, suffix1.length + s)
        it.iterator.copyToArray(suffix1b.asInstanceOf[Array[Any]], suffix1.length)
        suffix1b
      } else null
  }

  @inline final def copyTail[T <: AnyRef](a: Array[T]): Array[T] = copyOfRange[T](a, 1, a.length)

  @inline final def copyInit[T <: AnyRef](a: Array[T]): Array[T] = copyOfRange[T](a, 0, a.length-1)

  @inline final def copyIfDifferentSize[T <: AnyRef](a: Array[T], len: Int): Array[T] =
    if(a.length == len) a else copyOf[T](a, len)

  final val empty1: Arr1 = new Array(0)
  final val empty2: Arr2 = new Array(0)
  final val empty3: Arr3 = new Array(0)
  final val empty4: Arr4 = new Array(0)
  final val empty5: Arr5 = new Array(0)
  final val empty6: Arr6 = new Array(0)

  @inline final def wrap1(x: Any ): Arr1 = { val a = new Arr1(1); a(0) = x.asInstanceOf[AnyRef]; a }
  @inline final def wrap2(x: Arr1): Arr2 = { val a = new Arr2(1); a(0) = x; a }
  @inline final def wrap3(x: Arr2): Arr3 = { val a = new Arr3(1); a(0) = x; a }
  @inline final def wrap4(x: Arr3): Arr4 = { val a = new Arr4(1); a(0) = x; a }
  @inline final def wrap5(x: Arr4): Arr5 = { val a = new Arr5(1); a(0) = x; a }

  @inline final def copyUpdate(a1: Arr1, idx1: Int, elem: Any): Arr1 = {
    val a1c = a1.clone()
    a1c(idx1) = elem.asInstanceOf[AnyRef]
    a1c
  }

  @inline final def copyUpdate(a2: Arr2, idx2: Int, idx1: Int, elem: Any): Arr2 = {
    val a2c = a2.clone()
    a2c(idx2) = copyUpdate(a2c(idx2), idx1, elem)
    a2c
  }

  @inline final def copyUpdate(a3: Arr3, idx3: Int, idx2: Int, idx1: Int, elem: Any): Arr3 = {
    val a3c = a3.clone()
    a3c(idx3) = copyUpdate(a3c(idx3), idx2, idx1, elem)
    a3c
  }

  @inline final def copyUpdate(a4: Arr4, idx4: Int, idx3: Int, idx2: Int, idx1: Int, elem: Any): Arr4 = {
    val a4c = a4.clone()
    a4c(idx4) = copyUpdate(a4c(idx4), idx3, idx2, idx1, elem)
    a4c
  }

  @inline final def copyUpdate(a5: Arr5, idx5: Int, idx4: Int, idx3: Int, idx2: Int, idx1: Int, elem: Any): Arr5 = {
    val a5c = a5.clone()
    a5c(idx5) = copyUpdate(a5c(idx5), idx4, idx3, idx2, idx1, elem)
    a5c
  }

  @inline final def copyUpdate(a6: Arr6, idx6: Int, idx5: Int, idx4: Int, idx3: Int, idx2: Int, idx1: Int, elem: Any): Arr6 = {
    val a6c = a6.clone()
    a6c(idx6) = copyUpdate(a6c(idx6), idx5, idx4, idx3, idx2, idx1, elem)
    a6c
  }

  @inline final def concatArrays[T <: AnyRef](a: Array[T], b: Array[T]): Array[T] = {
    val dest = copyOf[T](a, a.length+b.length)
    System.arraycopy(b, 0, dest, a.length, b.length)
    dest
  }

  final def foreachRec[T <: AnyRef, A, U](level: Int, a: Array[T], f: A => U): Unit = {
    var i = 0
    var len = a.length
    if(level == 0) {
      while(i < len) {
        f(a(i).asInstanceOf[A])
        i += 1
      }
    } else {
      val l = level-1
      while(i < len) {
        foreachRec(l, a(i).asInstanceOf[Array[AnyRef]], f)
        i += 1
      }
    }
  }

  @inline final def foreachElem[A, U](a1: Arr1, f: A => U): Unit = {
    var i1 = 0
    while(i1 < a1.length) {
      f(a1(i1).asInstanceOf[A])
      i1 += 1
    }
  }

  final def foreachElem[A, U](a2: Arr2, f: A => U): Unit = {
    var i2 = 0
    while(i2 < a2.length) {
      foreachElem(a2(i2), f)
      i2 += 1
    }
  }

  final def foreachElem[A, U](a3: Arr3, f: A => U): Unit = {
    var i3 = 0
    while(i3 < a3.length) {
      foreachElem(a3(i3), f)
      i3 += 1
    }
  }

  final def foreachElem[A, U](a4: Arr4, f: A => U): Unit = {
    var i4 = 0
    while(i4 < a4.length) {
      foreachElem(a4(i4), f)
      i4 += 1
    }
  }

  final def foreachElem[A, U](a5: Arr5, f: A => U): Unit = {
    var i5 = 0
    while(i5 < a5.length) {
      foreachElem(a5(i5), f)
      i5 += 1
    }
  }

  final def foreachElem[A, U](a6: Arr6, f: A => U): Unit = {
    var i6 = 0
    while(i6 < a6.length) {
      foreachElem(a6(i6), f)
      i6 += 1
    }
  }

  final def mapElems1[A, B](a: Arr1, f: A => B): Arr1 = {
    var i = 0
    while(i < a.length) {
      val v1 = a(i).asInstanceOf[AnyRef]
      val v2 = f(v1.asInstanceOf[A]).asInstanceOf[AnyRef]
      if(v1 ne v2)
        return mapElems1Rest(a, f, i, v2)
      i += 1
    }
    a
  }

  private[this] final def mapElems1Rest[A, B](a: Arr1, f: A => B, at: Int, v2: AnyRef): Arr1 = {
    var ac = new Arr1(a.length)
    if(at > 0) System.arraycopy(a, 0, ac, 0, at)
    ac(at) = v2
    var i = at+1
    while(i < a.length) {
      ac(i) = f(a(i).asInstanceOf[A]).asInstanceOf[AnyRef]
      i += 1
    }
    ac
  }

  final def mapElems[A, B, T <: AnyRef](n: Int, a: Array[T], f: A => B): Array[T] = {
    if(n == 1)
      mapElems1[A, B](a.asInstanceOf[Arr1], f).asInstanceOf[Array[T]]
    else {
      var i = 0
      while(i < a.length) {
        val v1 = a(i)
        val v2 = mapElems(n-1, v1.asInstanceOf[Array[AnyRef]], f)
        if(v1 ne v2)
          return mapElemsRest(n, a, f, i, v2)
        i += 1
      }
      a
    }
  }

  private[this] final def mapElemsRest[A, B, T <: AnyRef](n: Int, a: Array[T], f: A => B, at: Int, v2: AnyRef): Array[T] = {
    var ac = java.lang.reflect.Array.newInstance(a.getClass.getComponentType, a.length).asInstanceOf[Array[AnyRef]]
    if(at > 0) System.arraycopy(a, 0, ac, 0, at)
    ac(at) = v2
    var i = at+1
    while(i < a.length) {
      ac(i) = mapElems(n-1, a(i).asInstanceOf[Array[AnyRef]], f)
      i += 1
    }
    ac.asInstanceOf[Array[T]]
  }
}


private[immutable] final class NVectorIterator[A](v: NVector[A]) extends Iterator[A] with java.lang.Cloneable {
  import NVectorStatics._

  private[this] var totalLength = v.length

  private[this] var slice: Array[_ <: AnyRef] = _
  private[this] var sliceIdx, sliceDim = -1
  private[this] var sliceStart, sliceEnd = 0 // absolute positions

  private[this] var a1: Arr1 = _
  private[this] var a1len = 0
  private[this] var i1 = 0 // current index in a1
  private[this] var len1 = totalLength // remaining length relative to a1

  advanceA1()

  @inline override def knownSize = len1 - i1

  @inline def hasNext: Boolean = len1 > i1

  private[this] def advanceSlice(): Unit = {
    if(!hasNext) Iterator.empty.next()
    slice = null
    while((slice eq null) || slice.length == 0) {
      sliceIdx += 1
      slice = v.vectorSlice(sliceIdx)
    }
    sliceStart = sliceEnd
    sliceDim = v.vectorSliceDim(sliceIdx)
    sliceEnd = sliceStart + slice.length * (1 << (BITS*(sliceDim-1)))
  }

  private[this] def advanceA1(): Unit = {
    val pos = i1-len1+totalLength
    if(pos == sliceEnd) advanceSlice()
    setA1(pos - sliceStart)
    len1 -= i1
    i1 = 0
  }

  private[this] def setA1(io: Int): Unit = {
    a1 = (sliceDim: @switch) match {
      case 1 => slice.asInstanceOf[Arr1]
      case 2 => slice.asInstanceOf[Arr2](io >>> BITS)
      case 3 => slice.asInstanceOf[Arr3](io >>> BITS2)((io >>> BITS) & MASK)
      case 4 => slice.asInstanceOf[Arr4](io >>> BITS3)((io >>> BITS2) & MASK)((io >>> BITS) & MASK)
      case 5 => slice.asInstanceOf[Arr5](io >>> BITS4)((io >>> BITS3) & MASK)((io >>> BITS2) & MASK)((io >>> BITS) & MASK)
      case 6 => slice.asInstanceOf[Arr6](io >>> BITS5)((io >>> BITS4) & MASK)((io >>> BITS3) & MASK)((io >>> BITS2) & MASK)((io >>> BITS) & MASK)
    }
    a1len = a1.length
  }

  def next(): A = {
    if(i1 == a1len) advanceA1()
    val r = a1(i1)
    i1 += 1
    r.asInstanceOf[A]
  }

  override def drop(n: Int): Iterator[A] = {
    if(n > 0) {
      val oldpos = i1-len1+totalLength
      val newpos = mmin(oldpos + n, totalLength)
      if(newpos == totalLength) {
        i1 = 0
        len1 = 0
      } else {
        while(newpos >= sliceEnd) advanceSlice()
        val io = newpos - sliceStart
        setA1(io)
        i1 = io & MASK
        len1 = i1 + (totalLength-newpos)
      }
    }
    this
  }

  override def take(n: Int): Iterator[A] = {
    if(n < knownSize) {
      val trunc = knownSize - mmax(0, n)
      totalLength -= trunc
      len1 -= trunc
      if(len1 < a1len) a1len = len1
      if(totalLength < sliceEnd) sliceEnd = totalLength
    }
    this
  }

  override def slice(from: Int, until: Int): Iterator[A] = {
    val _until =
      if(from > 0) {
        drop(from)
        until - from
      } else until
    take(_until)
  }

  override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Int = {
    val xsLen = xs.length
    val total = IterableOnce.elemsToCopyToArray(knownSize, xsLen, start, len)
    var copied = 0
    val isBoxed = xs.isInstanceOf[Array[AnyRef]]
    while(copied < total) {
      if(i1 == a1len) advanceA1()
      val count = mmin(total-copied, a1.length-i1)
      if(isBoxed) System.arraycopy(a1, i1, xs, start+copied, count)
      else Array.copy(a1, i1, xs, start+copied, count)
      i1 += count
      copied += count
    }
    total
  }

  protected[immutable] def split(at: Int): NVectorIterator[A] = {
    val it2 = clone().asInstanceOf[NVectorIterator[A]]
    it2.take(at)
    drop(at)
    it2
  }
}


private[immutable] abstract class NVectorStepperBase[A, Sub >: Null <: Stepper[A], Semi <: Sub](it: NVectorIterator[A])
  extends Stepper[A] with EfficientSplit {

  protected[this] def build(it: NVectorIterator[A]): Semi

  final def hasStep: Boolean = it.hasNext

  final def characteristics: Int = Spliterator.ORDERED + Spliterator.SIZED + Spliterator.SUBSIZED

  final def estimateSize: Long = it.knownSize

  def trySplit(): Sub = {
    val len = it.knownSize
    if(len > 1) build(it.split(len >>> 1))
    else null
  }

  override final def iterator: Iterator[A] = it
}

private[immutable] class AnyNVectorStepper[A](it: NVectorIterator[A])
  extends NVectorStepperBase[A, AnyStepper[A], AnyNVectorStepper[A]](it) with AnyStepper[A] {
  protected[this] def build(it: NVectorIterator[A]) = new AnyNVectorStepper(it)
  def nextStep(): A = it.next()
}

private[immutable] class DoubleNVectorStepper(it: NVectorIterator[Double])
  extends NVectorStepperBase[Double, DoubleStepper, DoubleNVectorStepper](it) with DoubleStepper {
  protected[this] def build(it: NVectorIterator[Double]) = new DoubleNVectorStepper(it)
  def nextStep(): Double = it.next()
}

private[immutable] class IntNVectorStepper(it: NVectorIterator[Int])
  extends NVectorStepperBase[Int, IntStepper, IntNVectorStepper](it) with IntStepper {
  protected[this] def build(it: NVectorIterator[Int]) = new IntNVectorStepper(it)
  def nextStep(): Int = it.next()
}

private[immutable] class LongNVectorStepper(it: NVectorIterator[Long])
  extends NVectorStepperBase[Long, LongStepper, LongNVectorStepper](it) with LongStepper {
  protected[this] def build(it: NVectorIterator[Long]) = new LongNVectorStepper(it)
  def nextStep(): Long = it.next()
}
