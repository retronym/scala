//> using options -Xlint:nonlocal-return -Werror

import scala.annotation.nowarn
import scala.util.control.ControlThrowable

class Meters(val value: Int) extends AnyVal

class Spec[@specialized(Int) T] {
  def get(a: Int, t: T, u: T): T =
    Test.id(try { if (a > 0) return t; u } catch { case _: Throwable => u })
}

object Test {
  def log(s: String): Unit = println("  " + s)
  def id[A](a: A): A = a

  // The try is an operand, so uncurry moves it to a local method.
  def operand(a: Int): Int = {
    val x = 1 + (try { if (a > 0) return 5; 2 } catch { case _: Throwable => 7 })
    x
  }
  // Same try in a position that is not lifted.
  def statement(a: Int): Int = {
    val t = try { if (a > 0) return 5; 2 } catch { case _: Throwable => 7 }
    1 + t
  }
  def controlThrowable(a: Int): String =
    id(try { if (a > 0) return "returned"; "body" } catch { case _: ControlThrowable => "handler" })
  def withFinally(a: Int): String =
    id(try { if (a > 0) return "returned"; "body" } catch { case _: Throwable => "handler" } finally log("finally"))
  def inHandler(a: Int): String =
    id(try { if (a > 0) throw new Exception; "body" } catch { case _: Throwable => return "returned" })
  def nested(a: Int): String =
    id(try {
      val s = id(try { if (a > 0) return "returned"; "inner" } catch { case _: Throwable => "inner handler" })
      s + " body"
    } catch { case _: Throwable => "handler" })
  def innerTry(a: Int): String =
    id(try {
      try { if (a > 0) return "returned"; "inner" } catch { case _: Throwable => "inner handler" }
    } catch { case _: Throwable => "handler" })
  def allReturn(a: Int): String = {
    val s: String = id(try { if (a > 0) return "returned" else throw new Exception } catch { case _: Exception => return "handler" })
    s
  }
  // The type of the try is Nothing.
  def nothingTry(a: Int): String =
    id[Nothing](try { if (a > 0) return "returned" else throw new Exception } catch { case _: Exception => return "handler" })
  def unit(a: Int): Unit = {
    id(try { if (a > 0) { log("returning"); return }; 1 } catch { case _: Throwable => 2 })
    log("not returned")
  }
  def generic[T](a: Int, t: T, u: T): T =
    id(try { if (a > 0) return t; u } catch { case _: Throwable => u })
  def primitive(a: Int): Long =
    1L + (try { if (a > 0) return 5; 2L } catch { case _: Throwable => 7L })
  def nothing(a: Int): Nothing =
    id(try { if (a > 0) return (throw new IllegalStateException("thrown")); 1 } catch { case _: IllegalArgumentException => 2 }) match {
      case _ => throw new IllegalStateException("not returned")
    }
  def byNameFunction(a: Int): (=> Int) => Int =
    id[(=> Int) => Int](try { if (a > 0) return (x => x); (x => x + 1) } catch { case _: Throwable => (x => 0) })
  def valueClass(a: Int): Meters =
    id(try { if (a > 0) return new Meters(5); new Meters(2) } catch { case _: Throwable => new Meters(7) })
  def inMatch(a: Int): String =
    id(try { a match { case 0 => "body"; case _ => return "returned" } } catch { case _: Throwable => "handler" } finally log("finally"))

  // The methods below still use an exception for the return, and -Xlint:nonlocal-return says so.

  // The call of the lifted method is in a closure, so its return is non-local.
  @nowarn("cat=lint-nonlocal-return")
  def inClosure(a: Int): String = {
    List(1).foreach(_ => id(try { if (a > 0) return "returned"; "body" } catch { case _: Exception => "handler" }))
    "end"
  }
  // Likewise in a local lazy val.
  @nowarn("cat=lint-nonlocal-return")
  def inLazyVal(a: Int): String = {
    lazy val s = { val t = id(try { if (a > 0) return "returned"; "body" } catch { case _: Exception => "handler" }); t }
    s
  }
  // The call of the lifted method is under a finally, or in a synchronized block, and not on an empty stack.
  @nowarn("cat=lint-nonlocal-return")
  def enclosingFinally(c: Boolean, d: Boolean): Int =
    try { if (d) return 9; 1 + (try { if (c) return 5; 2 } catch { case _: Exception => 0 }) } finally log("finally")
  private[this] val lock = new Object
  @nowarn("cat=lint-nonlocal-return")
  def enclosingSynchronized(c: Boolean, d: Boolean): Int =
    lock.synchronized { if (d) return 9; 1 + (try { if (c) return 5; 2 } catch { case _: Exception => 0 }) }
  // The second return is under the finally of the lifted try and not on an empty stack.
  @nowarn("cat=lint-nonlocal-return")
  def twoDepths(c: Boolean, d: Boolean): Int =
    id(try { if (d) return 9; 1 + (if (c) return 5 else 2) } catch { case _: IllegalStateException => 0 } finally log("finally"))
  @nowarn
  def inFinalizer(a: Int): String =
    id(try "body" catch { case _: Exception => "handler" } finally { if (a > 0) return "returned" })

  def main(args: Array[String]): Unit = {
    for (a <- 0 to 1) {
      println(s"operand($a) = ${operand(a)}")
      println(s"statement($a) = ${statement(a)}")
      println(s"controlThrowable($a) = ${controlThrowable(a)}")
      println(s"withFinally($a) = ${withFinally(a)}")
      println(s"inHandler($a) = ${inHandler(a)}")
      println(s"nested($a) = ${nested(a)}")
      println(s"innerTry($a) = ${innerTry(a)}")
      println(s"allReturn($a) = ${allReturn(a)}")
      println(s"nothingTry($a) = ${nothingTry(a)}")
      println(s"unit($a)"); unit(a)
      println(s"generic($a) = ${generic(a, "returned", "body")}")
      println(s"primitive($a) = ${primitive(a)}")
      println(s"nothing($a) = ${try nothing(a) catch { case e: IllegalStateException => e.getMessage }}")
      println(s"byNameFunction($a) = ${byNameFunction(a)(1)}")
      println(s"valueClass($a) = ${valueClass(a).value}")
      println(s"specialized($a) = ${new Spec[Int].get(a, 5, 2)} ${new Spec[String].get(a, "returned", "body")}")
      println(s"inMatch($a) = ${inMatch(a)}")
      println(s"inClosure($a) = ${inClosure(a)}")
      println(s"inLazyVal($a) = ${inLazyVal(a)}")
      println(s"inFinalizer($a) = ${inFinalizer(a)}")
    }
    for ((c, d) <- List((false, false), (true, false), (false, true))) {
      println(s"enclosingFinally($c, $d) = ${enclosingFinally(c, d)}")
      println(s"enclosingSynchronized($c, $d) = ${enclosingSynchronized(c, d)}")
      println(s"twoDepths($c, $d) = ${twoDepths(c, d)}")
    }
  }
}
