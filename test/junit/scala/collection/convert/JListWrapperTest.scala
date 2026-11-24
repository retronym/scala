package scala.collection.convert

import java.{util => ju}

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test
import scala.collection.JavaConverters._
import scala.collection.immutable.List
import scala.collection.mutable.{ArrayBuffer, Buffer}

@deprecated("Tests deprecated API", since="2.13")
class JListWrapperTest {

  @Test
  def testIteratorDoesNotCauseStackOverflow(): Unit = {
    val jList: ju.List[Int] = ju.Arrays.asList(1, 2, 3)
    val sList: Buffer[Int] = jList.asScala

    assertTrue(sList.isInstanceOf[JavaCollectionWrappers.JListWrapper[_]])
    assertTrue(sList.iterator.sameElements(List(1, 2, 3)))
  }

  @Test
  def patchInPlaceClampsLikeArrayBuffer(): Unit = {
    // Buffer.patchInPlace: a negative `from` is treated as 0, a `from` past the end appends,
    // and a `replaced` count beyond the available elements is ignored
    for (from <- -2 to 6; replaced <- -1 to 6) {
      val expected = ArrayBuffer(1, 2, 3, 4).patchInPlace(from, List(9), replaced)
      val jList = new ju.ArrayList[Int](ju.Arrays.asList(1, 2, 3, 4))
      jList.asScala.patchInPlace(from, List(9), replaced)
      assertEquals(s"patchInPlace($from, List(9), $replaced)", expected.asJava, jList)
    }
  }
}
