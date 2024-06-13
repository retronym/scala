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

package scala
package collection
package mutable

import scala.reflect.ClassTag
import scala.collection.generic._
import scala.collection.parallel.mutable.ParArray
import scala.util.hashing.MurmurHash3

import java.util.Arrays

/**
 *  A class representing `Array[T]`.
 *
 *  @tparam T    type of the elements in this wrapped array.
 *
 *  @author  Martin Odersky, Stephane Micheloud
 *  @since 2.8
 *  @define Coll `WrappedArray`
 *  @define coll wrapped array
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
abstract class WrappedArray[T]
extends AbstractSeq[T]
    with IndexedSeq[T]
    with ArrayLike[T, WrappedArray[T]]
    with CustomParallelizable[T, ParArray[T]]
{

  override protected[this] def thisCollection: WrappedArray[T] = this
  override protected[this] def toCollection(repr: WrappedArray[T]): WrappedArray[T] = repr

  /** The tag of the element type */
  def elemTag: ClassTag[T]

  @deprecated("use elemTag instead", "2.10.0")
  def elemManifest: ClassManifest[T] = ClassManifest.fromClass[T](elemTag.runtimeClass.asInstanceOf[Class[T]])

  /** The length of the array */
  def length: Int

  /** The element at given index */
  def apply(index: Int): T

  /** Update element at given index */
  def update(index: Int, elem: T): Unit

  /** The underlying array */
  def array: Array[T]

  override def par = ParArray.handoff(array)

  private def elementClass: Class[_] =
    array.getClass.getComponentType

  override def toArray[U >: T : ClassTag]: Array[U] = {
    val thatElementClass = implicitly[ClassTag[U]].runtimeClass
    if (elementClass eq thatElementClass)
      array.asInstanceOf[Array[U]]
    else
      super.toArray[U]
  }

  override def stringPrefix = "WrappedArray"

  /** Clones this object, including the underlying Array. */
  override def clone(): WrappedArray[T] = WrappedArray make array.clone()

  /** Creates new builder for this collection ==> move to subclasses
   */
  override protected[this] def newBuilder: Builder[T, WrappedArray[T]] =
    new WrappedArrayBuilder[T](elemTag)

}

/** A companion object used to create instances of `WrappedArray`.
 */
object WrappedArray {
  // This is reused for all calls to empty.
  private val EmptyWrappedArray  = new ofRef[AnyRef](new Array[AnyRef](0))
  def empty[T <: AnyRef]: WrappedArray[T] = EmptyWrappedArray.asInstanceOf[WrappedArray[T]]

  // If make is called explicitly we use whatever we're given, even if it's
  // empty.  This may be unnecessary (if WrappedArray is to honor the collections
  // contract all empty ones must be equal, so discriminating based on the reference
  // equality of an empty array should not come up) but we may as well be
  // conservative since wrapRefArray contributes most of the unnecessary allocations.
  def make[T](x: AnyRef): WrappedArray[T] = (x match {
    case null              => null
    case x: Array[AnyRef]  => new ofRef[AnyRef](x)
    case x: Array[Int]     => new ofInt(x)
    case x: Array[Double]  => new ofDouble(x)
    case x: Array[Long]    => new ofLong(x)
    case x: Array[Float]   => new ofFloat(x)
    case x: Array[Char]    => new ofChar(x)
    case x: Array[Byte]    => new ofByte(x)
    case x: Array[Short]   => new ofShort(x)
    case x: Array[Boolean] => new ofBoolean(x)
    case x: Array[Unit]    => new ofUnit(x)
  }).asInstanceOf[WrappedArray[T]]

  implicit def canBuildFrom[T](implicit m: ClassTag[T]): CanBuildFrom[WrappedArray[_], T, WrappedArray[T]] = {
    val tag = implicitly[ClassTag[T]]
    val cls = tag.runtimeClass
    (if (cls.isPrimitive) {
      tag.runtimeClass match {
        case java.lang.Integer.TYPE => cbfIntArray
        case java.lang.Double.TYPE => cbfDoubleArray
        case java.lang.Long.TYPE => cbfLongArray
        case java.lang.Float.TYPE => cbfFloatArray
        case java.lang.Character.TYPE => cbfCharArray
        case java.lang.Byte.TYPE => cbfByteArray
        case java.lang.Short.TYPE => cbfShortArray
        case java.lang.Boolean.TYPE => cbfBooleanArray
        case java.lang.Void.TYPE => cbfUnitArray
      }
    } else if (cls == ObjectClass) {
      cbfObjectArray
    } else {
      refCBF[T with AnyRef](tag.asInstanceOf[ClassTag[T with AnyRef]])
    }).asInstanceOf[CanBuildFrom[WrappedArray[_], T, WrappedArray[T]]]
  }

  private[this] val ObjectClass = classOf[Object]

  private[this] val cbfBooleanArray = new CanBuildFrom[WrappedArray[_], Boolean, WrappedArray[Boolean]] {
    def apply(from: WrappedArray[_]) = new ArrayBuilder.ofBoolean mapResult WrappedArray.make[Boolean]
    def apply = new ArrayBuilder.ofBoolean mapResult WrappedArray.make[Boolean]
  }
  private[this] val cbfByteArray    = new CanBuildFrom[WrappedArray[_], Byte, WrappedArray[Byte]] {
    def apply(from: WrappedArray[_]) = new ArrayBuilder.ofByte mapResult WrappedArray.make[Byte]
    def apply = new ArrayBuilder.ofByte mapResult WrappedArray.make[Byte]
  }
  private[this] val cbfCharArray    = new CanBuildFrom[WrappedArray[_], Char, WrappedArray[Char]] {
    def apply(from: WrappedArray[_]) = new ArrayBuilder.ofChar mapResult WrappedArray.make[Char]
    def apply = new ArrayBuilder.ofChar mapResult WrappedArray.make[Char]
  }
  private[this] val cbfDoubleArray  = new CanBuildFrom[WrappedArray[_], Double, WrappedArray[Double]] {
    def apply(from: WrappedArray[_]) = new ArrayBuilder.ofDouble mapResult WrappedArray.make[Double]
    def apply = new ArrayBuilder.ofDouble mapResult WrappedArray.make[Double]
  }
  private[this] val cbfFloatArray   = new CanBuildFrom[WrappedArray[_], Float, WrappedArray[Float]] {
    def apply(from: WrappedArray[_]) = new ArrayBuilder.ofFloat mapResult WrappedArray.make[Float]
    def apply = new ArrayBuilder.ofFloat mapResult WrappedArray.make[Float]
  }
  private[this] val cbfIntArray     = new CanBuildFrom[WrappedArray[_], Int, WrappedArray[Int]] {
    def apply(from: WrappedArray[_]) = new ArrayBuilder.ofInt mapResult WrappedArray.make[Int]
    def apply = new ArrayBuilder.ofInt mapResult WrappedArray.make[Int]
  }
  private[this] val cbfLongArray    = new CanBuildFrom[WrappedArray[_], Long, WrappedArray[Long]] {
    def apply(from: WrappedArray[_]) = new ArrayBuilder.ofLong mapResult WrappedArray.make[Long]
    def apply = new ArrayBuilder.ofLong mapResult WrappedArray.make[Long]
  }
  private[this] val cbfShortArray   = new CanBuildFrom[WrappedArray[_], Short, WrappedArray[Short]] {
    def apply(from: WrappedArray[_]) = new ArrayBuilder.ofShort mapResult WrappedArray.make[Short]
    def apply = new ArrayBuilder.ofShort mapResult WrappedArray.make[Short]
  }
  private[this] val cbfUnitArray    = new CanBuildFrom[WrappedArray[_], Unit, WrappedArray[Unit]] {
    def apply(from: WrappedArray[_]) = new ArrayBuilder.ofUnit mapResult WrappedArray.make[Unit]
    def apply = new ArrayBuilder.ofUnit mapResult WrappedArray.make[Unit]
  }
  private[this] val cbfObjectArray  = refCBF[Object]
  private[this] def refCBF[T <: AnyRef](implicit m: ClassTag[T]): CanBuildFrom[WrappedArray[_], T, WrappedArray[T]] =
    new CanBuildFrom[WrappedArray[_], T, WrappedArray[T]] {
      def apply(from: WrappedArray[_]): Builder[T, WrappedArray[T]] =
        ArrayBuilder.make[T]()(m) mapResult WrappedArray.make[T]

      def apply: Builder[T, WrappedArray[T]] =
        new ArrayBuilder.ofRef[T]()(m) mapResult WrappedArray.make[T]
    }

  def newBuilder[A]: Builder[A, IndexedSeq[A]] = new ArrayBuffer

  @SerialVersionUID(3456489343829468865L)
  final class ofRef[T <: AnyRef](val array: Array[T]) extends WrappedArray[T] with Serializable {
    def elemTag = ClassTag[T](array.getClass.getComponentType)
    def length: Int = array.length
    def apply(index: Int): T = array(index).asInstanceOf[T]
    def update(index: Int, elem: T) { array(index) = elem }
    override def hashCode = MurmurHash3.wrappedArrayHash(array)
  }

  @SerialVersionUID(-4502363748086738L)
  final class ofByte(val array: Array[Byte]) extends WrappedArray[Byte] with Serializable {
    def elemTag = ClassTag.Byte
    def length: Int = array.length
    def apply(index: Int): Byte = array(index)
    def update(index: Int, elem: Byte) { array(index) = elem }
    override def hashCode = MurmurHash3.wrappedBytesHash(array)
    override def equals(that: Any) = that match {
      case that: ofByte => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
  }

  @SerialVersionUID(3569089221887297170L)
  final class ofShort(val array: Array[Short]) extends WrappedArray[Short] with Serializable {
    def elemTag = ClassTag.Short
    def length: Int = array.length
    def apply(index: Int): Short = array(index)
    def update(index: Int, elem: Short) { array(index) = elem }
    override def hashCode = MurmurHash3.wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofShort => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
  }

  @SerialVersionUID(4353470320490138993L)
  final class ofChar(val array: Array[Char]) extends WrappedArray[Char] with Serializable {
    def elemTag = ClassTag.Char
    def length: Int = array.length
    def apply(index: Int): Char = array(index)
    def update(index: Int, elem: Char) { array(index) = elem }
    override def hashCode = MurmurHash3.wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofChar => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
  }

  @SerialVersionUID(-3796494337148298008L)
  final class ofInt(val array: Array[Int]) extends WrappedArray[Int] with Serializable {
    def elemTag = ClassTag.Int
    def length: Int = array.length
    def apply(index: Int): Int = array(index)
    def update(index: Int, elem: Int) { array(index) = elem }
    override def hashCode = MurmurHash3.wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofInt => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
    override def sum[B >: Int](implicit num: Numeric[B]): B = if (num eq scala.math.Numeric.IntIsIntegral) {
      var z = 0
      var i = 0
      val as = array
      while (i < as.length) {
        z += as(i)
        i += 1
      }
      z
    } else {
      super.sum[B]
    }
    override def product[B >: Int](implicit num: Numeric[B]): B = if (num eq scala.math.Numeric.IntIsIntegral) {
      var z = 0
      var i = 0
      val as = array
      while (i < as.length) {
        z *= as(i)
        i += 1
      }
      z
    } else {
      super.sum[B]
    }
  }

  @SerialVersionUID(7604729449860217276L)
  final class ofLong(val array: Array[Long]) extends WrappedArray[Long] with Serializable {
    def elemTag = ClassTag.Long
    def length: Int = array.length
    def apply(index: Int): Long = array(index)
    def update(index: Int, elem: Long) { array(index) = elem }
    override def hashCode = MurmurHash3.wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofLong => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
    override def sum[B >: Long](implicit num: Numeric[B]): B = if (num eq scala.math.Numeric.LongIsIntegral) {
      var z = 0L
      var i = 0
      val as = array
      while (i < as.length) {
        z += as(i)
        i += 1
      }
      z
    } else {
      super.sum[B]
    }
    override def product[B >: Long](implicit num: Numeric[B]): B = if (num eq scala.math.Numeric.LongIsIntegral) {
      var z = 0L
      var i = 0
      val as = array
      while (i < as.length) {
        z *= as(i)
        i += 1
      }
      z
    } else {
      super.sum[B]
    }
  }

  @SerialVersionUID(-5070075925231686368L)
  final class ofFloat(val array: Array[Float]) extends WrappedArray[Float] with Serializable {
    def elemTag = ClassTag.Float
    def length: Int = array.length
    def apply(index: Int): Float = array(index)
    def update(index: Int, elem: Float) { array(index) = elem }
    override def hashCode = MurmurHash3.wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofFloat => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
    override def sum[B >: Float](implicit num: Numeric[B]): B = if (num eq scala.math.Numeric.FloatIsFractional) {
      var z = 0f
      var i = 0
      val as = array
      while (i < as.length) {
        z += as(i)
        i += 1
      }
      z
    } else {
      super.sum[B]
    }
    override def product[B >: Float](implicit num: Numeric[B]): B = if (num eq scala.math.Numeric.FloatIsFractional) {
      var z = 0f
      var i = 0
      val as = array
      while (i < as.length) {
        z *= as(i)
        i += 1
      }
      z
    } else {
      super.sum[B]
    }
  }

  @SerialVersionUID(6556610635003622495L)
  final class ofDouble(val array: Array[Double]) extends WrappedArray[Double] with Serializable {
    def elemTag = ClassTag.Double
    def length: Int = array.length
    def apply(index: Int): Double = array(index)
    def update(index: Int, elem: Double) { array(index) = elem }
    override def hashCode = MurmurHash3.wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofDouble => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
    override def sum[B >: Double](implicit num: Numeric[B]): B = if (num eq scala.math.Numeric.DoubleIsFractional) {
      var z = 0d
      var i = 0
      val as = array
      while (i < as.length) {
        z += as(i)
        i += 1
      }
      z
    } else {
      super.sum[B]
    }
    override def product[B >: Double](implicit num: Numeric[B]): B = if (num eq scala.math.Numeric.DoubleIsFractional) {
      var z = 0d
      var i = 0
      val as = array
      while (i < as.length) {
        z *= as(i)
        i += 1
      }
      z
    } else {
      super.sum[B]
    }
  }

  @SerialVersionUID(-4835600351252182105L)
  final class ofBoolean(val array: Array[Boolean]) extends WrappedArray[Boolean] with Serializable {
    def elemTag = ClassTag.Boolean
    def length: Int = array.length
    def apply(index: Int): Boolean = array(index)
    def update(index: Int, elem: Boolean) { array(index) = elem }
    override def hashCode = MurmurHash3.wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofBoolean => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
  }

  @SerialVersionUID(3443664051778905707L)
  final class ofUnit(val array: Array[Unit]) extends WrappedArray[Unit] with Serializable {
    def elemTag = ClassTag.Unit
    def length: Int = array.length
    def apply(index: Int): Unit = array(index)
    def update(index: Int, elem: Unit) { array(index) = elem }
    override def hashCode = MurmurHash3.wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofUnit => array.length == that.array.length
      case _ => super.equals(that)
    }
  }
}
