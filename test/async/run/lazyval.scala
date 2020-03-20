object Test extends scala.tools.partest.JUnitTest(classOf[scala.async.run.lazyval.LazyValSpec])

package scala.async.run.lazyval {

  import org.junit.Test
  import org.junit.Assert._
  import scala.concurrent._
  import scala.concurrent.duration._
  import ExecutionContext.Implicits.global
  import scala.tools.partest.async.Async.{async, await}
  object TestUtil {
    import language.implicitConversions
    implicit def lift[T](t: T): Future[T] = Future.successful(t)
    def block[T](f: Future[T]): T = Await.result(f, Duration.Inf)
  }
  import TestUtil._

  class LazyValSpec {
    @Test
    def lazyValAllowed(): Unit = {
      val result = block(async {
        var x = 0
        lazy val y = { x += 1; 42 }
        assert(x == 0, x)
        val z = await(1)
        val result = y + x
        assert(x == 1, x)
        identity(y)
        assert(x == 1, x)
        result
      })

      assertEquals(43, result)
    }
  }

}
