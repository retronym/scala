/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect.internal.util

import java.lang.ref.{ReferenceQueue, WeakReference}

/** A hash set for hash-consing, laid out so that G1 does not pay for it on every young collection.
 *
 *  It is open-addressed (linear probing) over parallel arrays of hashes and elements, split into chunks of
 *  `ChunkSize` slots (256 KB each). One probe sequence runs across the chunks, so a lookup costs what it would in one
 *  flat array, but no array reaches G1's humongous threshold (half a region, 2 MB at an 8 GB heap). A humongous array
 *  is allocated straight into the old generation, so every insert into it stores a young element into an old
 *  object, which dirties a card that the next young collection scans. A chunk is allocated young, and a growth
 *  rebuilds the table into fresh, young chunks.
 *
 *  - `weak = true`: each element is held by a `Ref`, a `WeakReference` that also carries the element's hash. When an
 *    element is collected its slot becomes a tombstone, and the next rebuild drops it. There is no per-element chain
 *    link.
 *  - `weak = false`: elements are held strongly until `clear`, with no object per element.
 *
 *  Elements are compared with `equals`, probe first, as in `WeakHashSet.findEntryOrUpdate`. Lookups do not
 *  allocate. Not thread-safe.
 */
final class ChunkedInterner[A <: AnyRef](initialCapacity: Int, val weak: Boolean) {
  import ChunkedInterner._

  private[this] val InitialCapacity = Integer.highestOneBit(math.max(initialCapacity, 16) * 2 - 1)

  private[this] var mask: Int = InitialCapacity - 1
  private[this] var hashes: Array[Array[Int]] = newHashes(InitialCapacity)
  private[this] var elems: Array[Array[AnyRef]] = newElems(InitialCapacity)
  /** live elements plus tombstones */
  private[this] var used = 0
  private[this] var live = 0
  private[this] val queue: ReferenceQueue[AnyRef] = if (weak) new ReferenceQueue[AnyRef] else null

  /** The element held at the non-null slot content `e`, or null if `e` is a tombstone or its element was collected. */
  @inline private[this] def deref(e: AnyRef): A =
    if (!weak) e.asInstanceOf[A]
    else if (e eq Tombstone) null.asInstanceOf[A]
    else e.asInstanceOf[Ref].get().asInstanceOf[A]

  /** The element with hash `h` that `probe` equals, or null. */
  def find(h: Int, probe: AnyRef): A = {
    removeStaleEntries()
    val mask = this.mask
    val elems = this.elems
    val hashes = this.hashes
    var i = spread(h) & mask
    while (true) {
      val e = elems(i >>> ChunkBits)(i & ChunkMask)
      if (e == null) return null.asInstanceOf[A]
      if (hashes(i >>> ChunkBits)(i & ChunkMask) == h) {
        val x = deref(e)
        if (x != null && probe.equals(x)) return x
      }
      i = (i + 1) & mask
    }
    null.asInstanceOf[A]
  }

  /** Add `elem`, with hash `h`, which a `find` just failed to find. */
  def add[T <: A](elem: T, h: Int): T = {
    if ((used + 1) * 2 > mask + 1) {
      // grow if live elements fill a quarter of the table, otherwise rebuild at the same size to drop tombstones
      rebuild(if (live * 4 >= mask + 1) (mask + 1) * 2 else mask + 1)
    }
    insert(if (weak) new Ref(elem, h, queue) else elem, h)
    used += 1
    live += 1
    elem
  }

  /** The element equal to `elem`, adding `elem` if there is none. */
  def findOrAdd[T <: A](elem: T): T = {
    if (elem == null) throw new NullPointerException("ChunkedInterner cannot hold nulls")
    val h = elem.hashCode
    val e = find(h, elem)
    if (e != null) e.asInstanceOf[T] else add(elem, h)
  }

  /** Store slot content `e` (an element or its `Ref`) with hash `h` in the first free slot of its probe sequence. */
  private[this] def insert(e: AnyRef, h: Int): Unit = {
    var i = spread(h) & mask
    var x = elems(i >>> ChunkBits)(i & ChunkMask)
    while (x != null && (x ne Tombstone)) {
      i = (i + 1) & mask
      x = elems(i >>> ChunkBits)(i & ChunkMask)
    }
    elems(i >>> ChunkBits)(i & ChunkMask) = e
    hashes(i >>> ChunkBits)(i & ChunkMask) = h
  }

  /** Reinsert the live elements into fresh chunks of `capacity` slots. */
  private[this] def rebuild(capacity: Int): Unit = {
    val oldElems = elems
    val oldHashes = hashes
    mask = capacity - 1
    elems = newElems(capacity)
    hashes = newHashes(capacity)
    used = 0
    live = 0
    var c = 0
    while (c < oldElems.length) {
      val es = oldElems(c)
      val hs = oldHashes(c)
      var j = 0
      while (j < es.length) {
        val e = es(j)
        if (e != null && deref(e) != null) {
          insert(e, hs(j))
          used += 1
          live += 1
        }
        j += 1
      }
      c += 1
    }
  }

  /** Turn the slots of collected elements into tombstones. */
  private[this] def removeStaleEntries(): Unit = if (weak) {
    var stale = queue.poll()
    while (stale != null) {
      val ref = stale.asInstanceOf[Ref]
      var i = spread(ref.hash) & mask
      var done = false
      while (!done) {
        val e = elems(i >>> ChunkBits)(i & ChunkMask)
        if (e == null) done = true // a rebuild already dropped it
        else if (e eq ref) {
          elems(i >>> ChunkBits)(i & ChunkMask) = Tombstone
          live -= 1
          done = true
        }
        else i = (i + 1) & mask
      }
      stale = queue.poll()
    }
  }

  def clear(): Unit = {
    mask = InitialCapacity - 1
    elems = newElems(InitialCapacity)
    hashes = newHashes(InitialCapacity)
    used = 0
    live = 0
    if (weak) while (queue.poll() != null) ()
  }

  /** The number of elements, counting ones collected since the last lookup. */
  def size: Int = live

  /** The number of chunks per array, for testing. */
  private[util] def chunkCount: Int = elems.length
}

object ChunkedInterner {
  final val ChunkBits = 16
  final val ChunkSize = 1 << ChunkBits
  final val ChunkMask = ChunkSize - 1

  final class Ref(elem: AnyRef, val hash: Int, queue: ReferenceQueue[AnyRef]) extends WeakReference[AnyRef](elem, queue)

  private val Tombstone: AnyRef = new Object

  /** Spread the bits of `hash`, as `WeakHashSet` and `java.util.HashMap` do, so that linear probing over the low bits
   *  does not cluster on hash codes that differ only in their high bits. */
  @inline private def spread(hash: Int): Int = {
    var h = hash
    h ^= h >>> 20 ^ h >>> 12
    h ^ h >>> 7 ^ h >>> 4
  }

  private def chunks(capacity: Int): Int = (capacity + ChunkSize - 1) >>> ChunkBits
  private def chunkLength(capacity: Int): Int = math.min(capacity, ChunkSize)

  private def newHashes(capacity: Int): Array[Array[Int]] =
    Array.fill(chunks(capacity))(new Array[Int](chunkLength(capacity)))

  private def newElems(capacity: Int): Array[Array[AnyRef]] =
    Array.fill(chunks(capacity))(new Array[AnyRef](chunkLength(capacity)))
}
