package scala

import org.junit.Assert.assertFalse
import org.junit.Test

import scala.tools.testkit.AssertUtil.assertThrows

class ProductTest {
  @Test def productIteratorNextAtEnd(): Unit = {
    case class C(a: Int, b: String)
    for (p <- List[Product](C(1, "a"), (1, 2, 3), None, Some(1))) {
      val it = p.productIterator
      it.foreach(_ => ())
      assertFalse(it.hasNext)
      assertThrows[NoSuchElementException](it.next())
    }
  }
}
