package scala.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

import scala.annotation.nowarn
import scala.tools.testkit.AllocationTest

@RunWith(classOf[JUnit4])
class UnrolledBufferTestTest extends AllocationTest {
  @Test
  def test_SI9254_original(): Unit = {
    val b = new UnrolledBuffer[Int]()
    (1 to 16).foreach(i => b append i)
    b.insert(0,-1)
    b append 17
    assert(b sameElements (Seq(-1) ++ (1 to 16) ++ Seq(17)))
  }

  @Test
  def test_SI9254_additional(): Unit = {
    val b = new UnrolledBuffer[Int]()
    (1 to 100).foreach(i => b append i)
    b.insert(40, -1)
    assert(b sameElements((1 to 40) ++ Seq(-1) ++ (41 to 100)))
  }

  @Test
  def test_removal(): Unit = {
    val b = UnrolledBuffer(1, 2, 3)
    b -= 2
    assert(b == (1 :: 3 :: Nil))
    assert(b.size == 2)
  }

  @Test
  def insertAllSelf(): Unit = {
    import org.junit.Assert.assertEquals
    // sizes within one node: insertion at a node boundary is a separate bug
    for (n <- List(0, 1, 2, 3, 15); idx <- 0 to n) {
      val expected = ArrayBuffer.range(0, n)
      expected.insertAll(idx, expected)
      val b = UnrolledBuffer.range(0, n)
      b.insertAll(idx, b)
      assertEquals(s"insertAll($idx, self) on $n elements", expected, b)
    }
    val b = UnrolledBuffer(1, 2, 3)
    b.prependAll(b)
    assertEquals(List(1, 2, 3, 1, 2, 3), b.toList)
    val c = UnrolledBuffer(1, 2, 3)
    c.patchInPlace(1, c, 1)
    assertEquals(List(1, 1, 2, 3, 3), c.toList)
  }

  // scala/bug#12121: a source read lazily from the buffer gives what the same source of an equal buffer gives
  @nowarn("cat=deprecation") // Stream
  @Test
  def bulkOperationsWithSourceReadingSelf(): Unit = {
    import org.junit.Assert.assertEquals
    // `take` bounds the sources that would otherwise follow the elements being added and never end
    val sources: List[(String, UnrolledBuffer[Int] => IterableOnce[Int])] = List(
      "view.take"     -> (b => b.view.take(100)),
      "view.map"      -> (b => b.view.take(100).map(_ + 1)),
      "view.filter"   -> (b => b.view.take(100).filter(_ > 0)),
      "view.reverse"  -> (b => b.view.reverse),
      "iterator.take" -> (b => b.iterator.take(100)),
      "LazyList"      -> (b => LazyList.from(b.iterator.take(100))),
      "Stream"        -> (b => b.view.take(100).filter(_ > 0).to(Stream)),
      "concat"        -> (b => Iterator(5, 6) ++ b.iterator.take(10)),
    )
    for (init <- List(List(6, -2, 3, 9, 4), Nil); (name, source) <- sources) {
      def check(op: String)(f: (UnrolledBuffer[Int], IterableOnce[Int]) => Unit): Unit = {
        val expected = UnrolledBuffer.from(init)
        f(expected, source(UnrolledBuffer.from(init)).iterator.toVector)
        val b = UnrolledBuffer.from(init)
        f(b, source(b))
        assertEquals(s"$op($name) of $init", expected, b)
      }
      check("addAll")(_ addAll _)
      check("prependAll")(_ prependAll _)
      for (idx <- 0 to init.length)
        check(s"insertAll($idx, _)")(_.insertAll(idx, _))
      // within bounds: `patchInPlace` past the end is a separate bug
      for (from <- 0 to init.length; replaced <- 0 to math.min(2, init.length - from))
        check(s"patchInPlace($from, _, $replaced)")(_.patchInPlace(from, _, replaced))
    }
  }

  // scala/bug#12121: a source that cannot read the buffer is not copied; each bound is what the call allocated before the copy existed
  @Test
  def addAllOfStrictSourceAllocatesNoCopy(): Unit = {
    val u = new UnrolledBuffer[Int]
    val list = List(1, 2, 3)
    val some: Option[Int] = Some(1)
    def append(source: IterableOnce[Int]): Int = { u += 0; u ++= source; val n = u.length; u.clear(); n }
    onlyAllocates(sizeFor(200, 192), "UnrolledBuffer ++= Some")(append(some))
    onlyAllocates(sizeFor(192, 192), "UnrolledBuffer ++= List")(append(list))
  }
}
