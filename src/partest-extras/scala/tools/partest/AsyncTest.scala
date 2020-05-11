package scala.tools.partest

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.reflect.{classTag, ClassTag}

abstract class AsyncTest[C: ClassTag] {
  def block[T](f: Future[T]): T = Await.result(f, Duration.Inf)
  val className: String = reflect.classTag[C].runtimeClass.getName

  private val tests = mutable.ListBuffer[(String, () => Unit)]()
  def test(name: String)(f: => Unit) = {
    tests += (name, () => f)
  }
  def main(args: Array[String]): Unit = {
    var failed = false
    for (test <- tests) {
      try {
        test._2()
      } catch {
        case _: AssertionError =>
          println(s"[failed] $className.${test._1}")
        case _: Throwable =>
          println(s"[error] $className.${test._1}")
          failed = true
      }
    }
    System.exit(if (failed) -1 else 0)
  }

  implicit class FutureOps[T](f: Future[T]) {
    def block: T = AsyncTest.this.block(f)
  }
  // scala.tools.partest.TestUtil.intercept is not good enough for us
  def intercept[T <: Throwable : ClassTag](body: => Any): T = {
    try {
      body
      throw new Exception(s"Exception of type ${classTag[T]} was not thrown")
    } catch {
      case t: Throwable =>
        if (!classTag[T].runtimeClass.isAssignableFrom(t.getClass)) throw t
        else t.asInstanceOf[T]
    }
  }
  implicit class objectops(obj: Any) {
    def mustBe(other: Any) = assert(obj == other, obj + " is not " + other)

    def mustEqual(other: Any) = mustBe(other)
  }
}
