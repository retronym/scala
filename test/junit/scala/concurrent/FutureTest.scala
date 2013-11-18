package scala.concurrent

import org.junit.Assert._
import org.junit.{ After, Before, Test }
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._

@RunWith(classOf[JUnit4])
class FutureTest {
  class C

  @Test
  def testMapToFail {
    val f = Future.successful("<string>").mapTo[C]
    try {
      Await.result(f, Duration.Inf)
    } catch {
      case cce: ClassCastException =>
        assertEquals("Unable to cast <string> (of class java.lang.String) to class scala.concurrent.FutureTest$C", cce.getMessage)
    }
  }
}
