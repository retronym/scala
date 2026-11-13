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
  // The slot where a failed `find` would insert (the first tombstone it passed, or the empty slot that ended it), so
  // that `add` can store there without probing again. It is valid only while `stamp` is unchanged and for the same
  // hash: the caller may intern into this table between the `find` and the `add`, for example while constructing
  // the element.
  private[this] var stamp = 0
  private[this] var free = -1
  private[this] var freeHash = 0
  private[this] var freeStamp = -1

  /** DO NOT MERGE: the uniques census (`-Dscala.uniques.census=true`), told of each `Ref` added and cleared. */
  var listener: Listener = null

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
    var tomb = -1
    while (true) {
      val e = elems(i >>> ChunkBits)(i & ChunkMask)
      if (e == null) {
        free = if (tomb >= 0) tomb else i
        freeHash = h
        freeStamp = stamp
        return null.asInstanceOf[A]
      }
      if (tomb < 0 && (e eq Tombstone)) tomb = i
      if (hashes(i >>> ChunkBits)(i & ChunkMask) == h) {
        val x = deref(e)
        if (x != null && probe.equals(x)) {
          if (listener != null && weak) listener.hit(e.asInstanceOf[Ref])
          return x
        }
      }
      i = (i + 1) & mask
    }
    null.asInstanceOf[A]
  }

  /** Add `elem`, with hash `h`, which a `find` just failed to find. */
  def add[T <: A](elem: T, h: Int): T = {
    val e = if (weak) new Ref(elem, h, queue) else elem
    if (listener != null && weak) listener.added(e.asInstanceOf[Ref])
    if ((used + 1) * 2 > mask + 1) {
      // grow if live elements fill a quarter of the table, otherwise rebuild at the same size to drop tombstones
      rebuild(if (live * 4 >= mask + 1) (mask + 1) * 2 else mask + 1)
      insert(e, h)
    }
    else if (freeStamp == stamp && freeHash == h) store(free, e, h)
    else insert(e, h)
    live += 1
    stamp += 1
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
    store(i, e, h)
  }

  /** Store slot content `e` with hash `h` at the empty or tombstone slot `i`. */
  private[this] def store(i: Int, e: AnyRef, h: Int): Unit = {
    if (elems(i >>> ChunkBits)(i & ChunkMask) == null) used += 1 // a tombstone is already counted
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
    stamp += 1
    var c = 0
    while (c < oldElems.length) {
      val es = oldElems(c)
      val hs = oldHashes(c)
      var j = 0
      while (j < es.length) {
        val e = es(j)
        if (e != null && deref(e) != null) {
          insert(e, hs(j))
          live += 1
        } else if (listener != null && e != null && (e ne Tombstone)) listener.cleared(e.asInstanceOf[Ref])
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
      if (listener != null) listener.cleared(ref)
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
    stamp += 1
    mask = InitialCapacity - 1
    elems = newElems(InitialCapacity)
    hashes = newHashes(InitialCapacity)
    used = 0
    live = 0
    if (weak) while (queue.poll() != null) ()
  }

  /** The number of elements, counting ones collected since the last lookup. */
  def size: Int = live

  /** DO NOT MERGE (census): the `Ref` holding `elem`, with hash `h`, or null. */
  def refOf(elem: AnyRef, h: Int): Ref = if (!weak) null else {
    removeStaleEntries()
    var i = spread(h) & mask
    while (true) {
      val e = elems(i >>> ChunkBits)(i & ChunkMask)
      if (e == null) return null
      if ((e ne Tombstone) && hashes(i >>> ChunkBits)(i & ChunkMask) == h && (e.asInstanceOf[Ref].get() eq elem)) return e.asInstanceOf[Ref]
      i = (i + 1) & mask
    }
    null
  }

  /** DO NOT MERGE (census): drain the reference queue, then visit every `Ref` still in a slot. */
  def foreachRef(f: Ref => Unit): Unit = if (weak) {
    removeStaleEntries()
    var c = 0
    while (c < elems.length) {
      val es = elems(c)
      var j = 0
      while (j < es.length) {
        val e = es(j)
        if (e != null && (e ne Tombstone)) f(e.asInstanceOf[Ref])
        j += 1
      }
      c += 1
    }
  }

  /** The number of chunks per array, for testing. */
  private[util] def chunkCount: Int = elems.length
}

object ChunkedInterner {
  final val ChunkBits = 16
  final val ChunkSize = 1 << ChunkBits
  final val ChunkMask = ChunkSize - 1

  final class Ref(elem: AnyRef, val hash: Int, queue: ReferenceQueue[AnyRef]) extends WeakReference[AnyRef](elem, queue) {
    /** DO NOT MERGE (census): the census's tag bits, and the serial of the completion frame the element was added in. */
    var census: Int = 0
    var serial: Int = 0
    var site: Int = 0
  }

  /** DO NOT MERGE (census). */
  trait Listener {
    def added(ref: Ref): Unit
    /** A lookup found the element held by `ref`. */
    def hit(ref: Ref): Unit
    /** A `Ref` whose element was collected; may be called more than once for one `Ref`. */
    def cleared(ref: Ref): Unit
  }

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
