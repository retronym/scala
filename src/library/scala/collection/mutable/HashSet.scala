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
package mutable

import scala.annotation.tailrec
import scala.collection.Stepper.EfficientSplit
import scala.collection.generic.DefaultSerializationProxy
import scala.util.hashing.MurmurHash3

/** This class implements mutable sets using a hashtable.
  *
  * @see [[https://docs.scala-lang.org/overviews/collections-2.13/concrete-mutable-collection-classes.html#hash-tables "Scala's Collection Library overview"]]
  * section on `Hash Tables` for more information.
  *
  * @define Coll `mutable.HashSet`
  * @define coll mutable hash set
  * @define mayNotTerminateInf
  * @define willNotTerminateInf
  */
final class HashSet[A](contents: FlatHashTable.Contents[A])
  extends AbstractSet[A]
    with SetOps[A, HashSet, HashSet[A]]
    with StrictOptimizedIterableOps[A, HashSet, HashSet[A]]
    with IterableFactoryDefaults[A, HashSet]
    with FlatHashTable[A]
    with Serializable {

  initWithContents(contents)

  def this() = this(null)

  override def size: Int = tableSize

  override def contains(elem: A): Boolean = containsElem(elem)

  override def sizeHint(size: Int): Unit = {
    super.sizeHint(size)
  }

  override def add(elem: A) : Boolean = {
    addElem(elem)
  }

  override def addAll(xs: IterableOnce[A]): this.type = {
    super.addAll(xs)
  }

  override def subtractAll(xs: IterableOnce[A]): this.type = {
    super.subtractAll(xs)
  }

  override def remove(elem: A) : Boolean = removeElem(elem)

  override def iterator: Iterator[A] = super[FlatHashTable].iterator

  override def filterInPlace(p: A => Boolean): this.type = {
    super.filterInPlace(p)
  }

  def clear(): Unit = {
    clearTable()
  }

  override def iterableFactory: IterableFactory[HashSet] = HashSet

  @`inline` def addOne(elem: A): this.type = { add(elem); this }

  @`inline` def subtractOne(elem: A): this.type = { remove(elem); this }

  override def knownSize: Int = size

  override def isEmpty: Boolean = size == 0

  override def foreach[U](f: A => U): Unit = {
    var i = 0
    val len = table.length
    while (i < len) {
      val curEntry = table(i)
      if (curEntry ne null) f(entryToElem(curEntry))
      i += 1
    }
  }

  protected[this] def writeReplace(): AnyRef = new DefaultSerializationProxy(new HashSet.DeserializationFactory[A](table.length, _loadFactor), this)

  override protected[this] def className = "HashSet"
}

/**
  * $factoryInfo
  * @define Coll `mutable.HashSet`
  * @define coll mutable hash set
  */
@SerialVersionUID(3L)
object HashSet extends IterableFactory[HashSet] {

  def from[B](it: scala.collection.IterableOnce[B]): HashSet[B] = {
    new HashSet[B]() ++= it
  }

  def empty[A]: HashSet[A] = new HashSet[A]

  def newBuilder[A]: Builder[A, HashSet[A]] = newBuilder(defaultInitialCapacity, defaultLoadFactor)

  def newBuilder[A](initialCapacity: Int, loadFactor: Double): Builder[A, HashSet[A]] =
    new GrowableBuilder[A, HashSet[A]](new HashSet[A]()) {
      override def sizeHint(size: Int) = elems.sizeHint(size)
    }

  /** The default load factor for the hash table */
  final def defaultLoadFactor: Double = 0.75

  /** The default initial capacity for the hash table */
  final def defaultInitialCapacity: Int = 32

  @SerialVersionUID(3L)
  private final class DeserializationFactory[A](val tableLength: Int, val loadFactor: Double) extends Factory[A, HashSet[A]] with Serializable {
    def fromSpecific(it: IterableOnce[A]): HashSet[A] = new HashSet[A]() ++= it
    def newBuilder: Builder[A, HashSet[A]] = HashSet.newBuilder(tableLength, loadFactor)
  }
}


object HashSetTest {
  def main(args: Array[String]): Unit = {
//    var hs = HashSet[Int]()
//    hs += 1
//    hs += 324
//    hs += 647
//    hs += 970
//    val l = hs.toList
//    println(l)
//
//    val hs2 = HashSet.from(List(1, 324, 647, 970))
//    println(hs2.toString())
//    println((1 to 1000000 by 123).groupBy(x => ~(x % 32)).mapValues(_.sum).toList)
//    println(HashSet("the", "quick", "brown", "fox", "jumped", "over", "the", "lazy", "dog"))
    val hs1 = HashSet[String]()
    hs1 += "the"
    hs1 += "quick"
    hs1 += "brown"
    hs1 += "fox"
    hs1 += "jumped"
    hs1 += "over"
    hs1 += "the"
    hs1 += "lazy"
    hs1 += "dog"
    println(hs1)
  }
}