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

import scala.annotation.{nowarn, tailrec}
import scala.collection.Stepper.EfficientSplit
import scala.collection.generic.DefaultSerializationProxy
import scala.util.hashing.MurmurHash3

/** This class implements mutable maps using a hashtable.
  *
  *  @see [[https://docs.scala-lang.org/overviews/collections-2.13/concrete-mutable-collection-classes.html#hash-tables "Scala's Collection Library overview"]]
  *  section on `Hash Tables` for more information.
  *
  *  @tparam K    the type of the keys contained in this hash map.
  *  @tparam V    the type of the values assigned to keys in this hash map.
  *
  *  @define Coll `mutable.HashMap`
  *  @define coll mutable hash map
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  */
@deprecatedInheritance("HashMap will be made final; use .withDefault for the common use case of computing a default value", "2.13.0")
class HashMap[K, V](contents: HashTable.Contents[K, DefaultEntry[K, V]])
  extends AbstractMap[K, V]
    with MapOps[K, V, HashMap, HashMap[K, V]]
    with StrictOptimizedIterableOps[(K, V), Iterable, HashMap[K, V]]
    with StrictOptimizedMapOps[K, V, HashMap, HashMap[K, V]]
    with MapFactoryDefaults[K, V, HashMap, Iterable]
    with HashTable[K, V, DefaultEntry[K, V]]
    with Serializable {
  initWithContents(contents)
  type Entry = DefaultEntry[K, V]

  /* The HashMap class holds the following invariant:
   * - For each i between  0 and table.length, the bucket at table(i) only contains keys whose hash-index is i.
   * - Every bucket is sorted in ascendent hash order
   * - The sum of the lengths of all buckets is equal to contentSize.
   */
  def this() = this(null)

  override def size: Int = tableSize
  override def contains(key: K): Boolean = findEntry(key) != null

  override def sizeHint(size: Int): Unit = {
    super.sizeHint(size)
  }

  override def addAll(xs: IterableOnce[(K, V)]): this.type = {
    super.addAll(xs)
  }

  override def updateWith(key: K)(remappingFunction: Option[V] => Option[V]): Option[V] = {
    super.updateWith(key)(remappingFunction)
  }

  override def subtractAll(xs: IterableOnce[K]): this.type = {
    super.subtractAll(xs)
  }

  override def iterator: Iterator[(K, V)] = entriesIterator map (e => ((e.key, e.value)))

  override def keysIterator: Iterator[K] = new AbstractIterator[K] {
    val iter    = entriesIterator
    def hasNext = iter.hasNext
    def next()  = iter.next().key
  }
  override def valuesIterator: Iterator[V] = new AbstractIterator[V] {
    val iter    = entriesIterator
    def hasNext = iter.hasNext
    def next()  = iter.next().value
  }

  override def clear(): Unit = {
    clearTable()
  }

  def get(key: K): Option[V] = {
    val e = findEntry(key)
    if (e eq null) None
    else Some(e.value)
  }

  @throws[NoSuchElementException]
  override def apply(key: K): V =  {
    val result = findEntry(key)
    if (result eq null) default(key)
    else result.value
  }

  override def getOrElseUpdate(key: K, defaultValue: => V): V = {
    val hash = elemHashCode(key)
    val i = index(hash)
    val firstEntry = findEntry0(key, i)
    if (firstEntry != null) firstEntry.value
    else {
      val table0 = table
      val default = defaultValue
      // Avoid recomputing index if the `defaultValue()` hasn't triggered
      // a table resize.
      val newEntryIndex = if (table0 eq table) i else index(hash)
      val e = createNewEntry(key, default)
      // Repeat search
      // because evaluation of `default` can bring entry with `key`
      val secondEntry = findEntry0(key, newEntryIndex)
      if (secondEntry == null) addEntry0(e, newEntryIndex)
      else secondEntry.value = default
      default
    }
  }

  override def put(key: K, value: V): Option[V] = {
    val e = findOrAddEntry(key, value)
    if (e eq null) None
    else { val v = e.value; e.value = value; Some(v) }
  }

  override def remove(key: K): Option[V] = {
    val e = removeEntry(key)
    if (e ne null) Some(e.value)
    else None
  }


  override def update(key: K, value: V): Unit = {
    val e = findOrAddEntry(key, value)
    if (e ne null) e.value = value
  }

  def addOne(elem: (K, V)): this.type = {
    val e = findOrAddEntry(elem._1, elem._2)
    if (e ne null) e.value = elem._2
    this
  }

  def subtractOne(elem: K): this.type = { removeEntry(elem); this }

  override def knownSize: Int = size

  override def isEmpty: Boolean = size == 0

  override def foreach[U](f: ((K, V)) => U): Unit = foreachEntry0(e => f((e.key, e.value)))

  override def foreachEntry[U](f: (K, V) => U): Unit = {
    super.foreachEntry0(entry => f(entry.key, entry.value))
  }

  protected[this] def writeReplace(): AnyRef = new DefaultSerializationProxy(new mutable.HashMap.DeserializationFactory[K, V](table.length, _loadFactor), this)

  override def filterInPlace(p: (K, V) => Boolean): this.type = {
    super.filterInPlace(p)
  }

  override def mapFactory: MapFactory[HashMap] = HashMap

  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected[this] def stringPrefix = "HashMap"

  def createNewEntry(key: K, value: V): Entry = {
    new Entry(key, value)
  }

}

/**
  * $factoryInfo
  *  @define Coll `mutable.HashMap`
  *  @define coll mutable hash map
  */
@SerialVersionUID(3L)
object HashMap extends MapFactory[HashMap] {

  def empty[K, V]: HashMap[K, V] = new HashMap[K, V]

  def from[K, V](it: collection.IterableOnce[(K, V)]): HashMap[K, V] = {
    new HashMap[K, V]().addAll(it)
  }

  def newBuilder[K, V]: Builder[(K, V), HashMap[K, V]] = newBuilder(defaultInitialCapacity, defaultLoadFactor)

  def newBuilder[K, V](initialCapacity: Int, loadFactor: Double): Builder[(K, V), HashMap[K, V]] =
    new GrowableBuilder[(K, V), HashMap[K, V]](new HashMap[K, V]()) {
      override def sizeHint(size: Int) = elems.sizeHint(size)
    }

  /** The default load factor for the hash table */
  final def defaultLoadFactor: Double = 0.75

  /** The default initial capacity for the hash table */
  final def defaultInitialCapacity: Int = 32

  @SerialVersionUID(3L)
  private final class DeserializationFactory[K, V](val tableLength: Int, val loadFactor: Double) extends Factory[(K, V), HashMap[K, V]] with Serializable {
    def fromSpecific(it: IterableOnce[(K, V)]): HashMap[K, V] = new HashMap[K, V]().addAll(it)
    def newBuilder: Builder[(K, V), HashMap[K, V]] = HashMap.newBuilder(tableLength, loadFactor)
  }

  private[collection] final class Node[K, V](_key: K, _hash: Int, private[this] var _value: V, private[this] var _next: Node[K, V]) {
    def key: K = _key
    def hash: Int = _hash
    def value: V = _value
    def value_= (v: V): Unit = _value = v
    def next: Node[K, V] = _next
    def next_= (n: Node[K, V]): Unit = _next = n

    @tailrec
    def findNode(k: K, h: Int): Node[K, V] =
      if(h == _hash && k == _key) this
      else if((_next eq null) || (_hash > h)) null
      else _next.findNode(k, h)

    @tailrec
    def foreach[U](f: ((K, V)) => U): Unit = {
      f((_key, _value))
      if(_next ne null) _next.foreach(f)
    }

    @tailrec
    def foreachEntry[U](f: (K, V) => U): Unit = {
      f(_key, _value)
      if(_next ne null) _next.foreachEntry(f)
    }

    override def toString = s"Node($key, $value, $hash) -> $next"
  }
}
