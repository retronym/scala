package scala.collection.mutable

import org.junit.Test
import org.junit.Assert._

import scala.annotation.nowarn
import scala.collection.SeqFactory
import scala.collection.immutable.List
import scala.tools.nsc.`strip margin`
import scala.tools.testkit.AllocationTest
import scala.tools.testkit.AssertUtil.assertThrows

class ArrayDequeTest extends AllocationTest {

  @Test
  def apply: Unit = {
    val buffer = ArrayDeque.empty[Int]
    val standard = ArrayBuffer.empty[Int]

    def apply[U](f: Buffer[Int] => U) = {
      //println(s"Before: [buffer1=${buffer}; buffer2=${buffer2}]")
      assertEquals(f(standard), f(buffer))
      assertEquals(standard, buffer)
      assertEquals(standard.reverse, buffer.reverse)
    }

    apply(_.+=(1, 2, 3, 4, 5)): @nowarn("cat=deprecation")
    apply(_.prepend(6).prepend(7).prepend(8))
    apply(_.dropInPlace(2))
    apply(_.dropRightInPlace(3))
    apply(_.insert(2, -3))
    apply(_.insertAll(0, collection.Seq(9, 10, 11)))
    apply(_.insertAll(1, collection.Seq(12, 13)))
    apply(_.insertAll(0, collection.Seq(23, 24)))
    apply(_ ++= Seq(25, 26))
    apply(_.insertAll(3, collection.IndexedSeq(18, 33)))
    apply(_.remove(2))
    apply(_.prependAll(collection.Seq(14, 15, 16, 17)))
    apply(_.remove(1, 5))
    apply(_.prependAll(Seq.tabulate(100)(identity)))
    apply(b => b.insertAll(b.length - 5, collection.Seq.tabulate(10)(identity)))
    buffer.trimToSize()
    apply(_.addAll(collection.Seq.tabulate(100)(identity)))
    apply(_.addAll(collection.Iterator.tabulate(100)(identity)))
    apply(_.addAll(collection.immutable.Vector.tabulate(10)(identity)))

    (-100 to 100) foreach {i =>
      assertEquals(standard.splitAt(i), buffer.splitAt(i))
    }

    for {
      i <- -100 to 100
      j <- -100 to 100
    } {
      assertEquals(standard.slice(i, j), buffer.slice(i, j))
      if (i > 0 && j > 0) assertEquals(List.from(standard.sliding(i, j)), List.from(buffer.sliding(i, j)))
    }
  }

  @Test
  def queueBounds(): Unit = {
    import scala.collection.mutable.Queue

    val xs = Queue.empty[Int]
    assertEquals("Queue()", xs.toString)

    val a = xs.toArray
    assertEquals(0, a.length)

    xs.insert(0, 0)
    assertEquals(Queue(0), xs)
  }

  @Test
  def copyToArrayOutOfBounds(): Unit = {
    val target = Array[Int]()
    assertEquals(0, collection.mutable.ArrayDeque(1, 2).copyToArray(target, 1, 0))
  }

  @Test
  def insertsWhenResizeIsNeeded(): Unit = {
    val arrayDeque = ArrayDeque.from(Array.range(0, 15))
    arrayDeque.insert(1, -1)
    assertEquals(ArrayDeque(0, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14), arrayDeque)
  }

  @Test
  def insertAll(): Unit = {
    var a = ArrayDeque(0, 1)
    a.insertAll(1, Seq(2))
    assertEquals(ArrayDeque(0, 2, 1), a)
    a = ArrayDeque(0, 1)
    a.insertAll(2, Seq(2))
    assertEquals(ArrayDeque(0, 1, 2), a)
    var q = Queue(0, 1)
    q.patchInPlace(1, Seq(2), 0)
    assertEquals(Queue(0, 2, 1), q)
    q = Queue(0, 1)
    q.patchInPlace(2, Seq(2), 0)
    assertEquals(Queue(0, 1, 2), q)
  }

  @Test
  def sliding(): Unit = ArrayDequeTest.genericSlidingTest(ArrayDeque, "ArrayDeque")


  class PeekingArrayDeque[C] extends ArrayDeque[C] {
    def capacity = array.length
  }

  @Test
  def trimToSize(): Unit = {
    val a = new PeekingArrayDeque().addAll(0 to 255)

    a.trimToSize()  // Can't shrink
    assertEquals(a.capacity, 512)
    a.remove(0)     // No reallocation because array isn't empty enough
    assertEquals(a.capacity, 512)
    a.trimToSize()  // Shrink to 256
    assertEquals(a.capacity, 256)
  }

  @Test def `head of empty throws NoSuchElement`: Unit =
    assertThrows[NoSuchElementException](ArrayDeque.empty[Int].head, _.endsWith("head of empty ArrayDeque"))

  @Test def `last of empty throws NoSuchElement`: Unit =
    assertThrows[NoSuchElementException](ArrayDeque.empty[Int].last, _.endsWith("last of empty ArrayDeque"))

  @Test def `sliding throws on shrink`: Unit = {
    val sut = ArrayDeque.from(1 to 10)
    val it = sut.sliding(size = 2, step = 1)
    assertTrue(it.hasNext)
    assertEquals(2, it.next().size)
    sut.clear()
    assertThrows[java.util.ConcurrentModificationException](it.hasNext)
  }

  // scala/bug#12121: a source read lazily from the deque gives what the same source of an equal deque gives
  @nowarn("cat=deprecation") // Stream
  @Test def `bulk operations with a source that reads the deque`: Unit = {
    val sources: List[(String, ArrayDeque[Int] => IterableOnce[Int])] = List(
      "self"          -> (d => d),
      "view"          -> (d => d.view),
      "view.reverse"  -> (d => d.view.reverse),
      "view.map"      -> (d => d.view.map(_ + 1)),
      "view.filter"   -> (d => d.view.filter(_ > 0)),
      "view.slice"    -> (d => d.view.slice(1, 4)),
      "iterator"      -> (d => d.iterator),
      "iterator.map"  -> (d => d.iterator.map(_ * 10)),
      "LazyList"      -> (d => LazyList.from(d.view.reverse)),
      "Stream"        -> (d => d.view.reverse.to(Stream)),
      "concat"        -> (d => Iterator(5, 6) ++ d.view.reverse.take(10)),
    )
    // with and without spare capacity, so that both the in-place and the reallocating paths are taken
    for (init <- List(Vector(6, -2, 3, 9, 4), Vector.empty[Int]); capacity <- List(init.length, 64); (name, source) <- sources) {
      def sut() = new ArrayDeque[Int](capacity) ++= init
      def check(op: String)(f: (ArrayDeque[Int], IterableOnce[Int]) => Unit): Unit = {
        val expected = sut()
        f(expected, source(sut()).iterator.toVector)
        val d = sut()
        f(d, source(d))
        assertEquals(s"$op($name) of $init with capacity $capacity", expected, d)
      }
      check("addAll")(_ addAll _)
      check("prependAll")(_ prependAll _)
      check("subtractAll")(_ subtractAll _)
      for (from <- 0 to init.length; replaced <- 0 to 2)
        check(s"patchInPlace($from, _, $replaced)")(_.patchInPlace(from, _, replaced))
    }
    val q = Queue(1, 2, 3)
    q.patchInPlace(1, q.view.map(_ * 10), 1)
    assertEquals(Queue(1, 10, 20, 30, 3), q)
  }

  // scala/bug#12121: a source that cannot read the receiver is not copied; each bound is what the call allocated before the copy existed
  @Test def `bulk operations with a strict source allocate no copy`: Unit = {
    val d = new ArrayDeque[Int](64)
    val other = ArrayBuffer(1, 2, 3)
    val list = List(1, 2, 3)
    val some: Option[Int] = Some(1)
    def append(source: IterableOnce[Int]): Int = { d += 0; d ++= source; val n = d.length; d.clear(); n }
    onlyAllocates(sizeFor(40, 32), "deque ++= Some")(append(some))
    onlyAllocates(sizeFor(32, 32), "deque ++= List")(append(list))
    onlyAllocates(sizeFor(88, 80), "deque ++= ArrayBuffer")(append(other))
    val b = new ArrayBuffer[Int](64)
    val set = Set(1, 2, 3)
    onlyAllocates(sizeFor(296, 264), "buffer --= immutable.Set")({ b ++= other; b --= set; b.length })
  }

  @Test def `sliding throws on grow`: Unit = {
    val sut = ArrayDeque.from(1 to 10)
    val it = sut.sliding(size = 2, step = 1)
    assertTrue(it.hasNext)
    assertEquals(2, it.next().size)
    sut.addOne(100)
    assertThrows[java.util.ConcurrentModificationException](it.hasNext)
  }
}

object ArrayDequeTest {

  // tests scala/bug#11047
  def genericSlidingTest(factory: SeqFactory[ArrayDeque], collectionName: String): Unit = {
    for (i <- 0 to 40; j <- 1 to 40; k <- 1 to 40) {
      val range = 0 until i
      val other = factory.from(range)
      val iterableSliding = range.sliding(j, k).to(Seq)
      val otherSliding = other.sliding(j, k).to(Seq)
      assert(iterableSliding == otherSliding,
        sm"""Iterable.from($range)).sliding($j,$k) differs from $collectionName.from($range)).sliding($j,$k)
            |Iterable yielded: $iterableSliding
            |$collectionName yielded: $otherSliding
            |"""
      )
    }

    // scala/bug#11440
    assertEquals(0, factory.empty[Int].sliding(1).size)

    // no general mutation check
    locally {
      val sut = factory.from(1 to 2)
      val it = sut.sliding(size = 2, step = 1)
      sut(1) = 100
      assertTrue(it.hasNext)
      assertEquals(1, it.size)
    }
  }
}
