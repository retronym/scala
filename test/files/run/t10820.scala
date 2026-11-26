// A `try` with handlers where the operand stack may not be empty is moved to a local method, so a
// `return` in it throws NonLocalReturnControl. Neither the handlers of that `try` nor those of the
// `try`s around it may catch it: written in place, the `return` does not pass through them.
// Most methods return "returned" when `ret` is true.

import scala.util.control.{ControlThrowable, NonFatal}

class Meters(val value: Int) extends AnyVal

class Spec[@specialized(Int) T] {
  def get(ret: Boolean, t: T, u: T, v: T): T =
    Test.id(try { if (ret) return t; u } catch { case _: Throwable => v })
}

class Wrap(val s: String) extends AnyVal {
  def get(ret: Boolean): String = Test.id(try { if (ret) return s; "body" } catch { case _: Throwable => "handler" })
}

class Base(val s: String)

object Test {
  def id[A](a: A): A = a
  def p(s: String): Unit = println("  " + s)

  // the position of the `try`
  def operand(ret: Boolean): Int = 1 + (try { if (ret) return 5; 2 } catch { case _: Throwable => 7 })
  def notLifted(ret: Boolean): Int = { val t = try { if (ret) return 5; 2 } catch { case _: Throwable => 7 }; 1 + t }
  def argument(ret: Boolean): String = id(try { if (ret) return "returned"; "body" } catch { case _: Throwable => "handler" })
  def byNameArg(ret: Boolean): String = Option.empty[Int].fold(try { if (ret) return "returned"; "body" } catch { case _: Throwable => "handler" })(_ => "some")
  def strictByName(ret: Boolean): String = { def f(s: => String) = s; f(try { if (ret) return "returned"; "body" } catch { case _: Throwable => "handler" }) }
  var field = ""
  def assignField(ret: Boolean): String = { field = try { if (ret) return "returned"; "body" } catch { case _: Throwable => "handler" }; field }
  def inWhile(ret: Boolean): String = {
    var i = 0; var s = ""
    while (i < 3) { s += (try { if (ret && i == 1) return "returned" + s; i.toString } catch { case _: Throwable => "handler" }); i += 1 }
    s
  }
  def inDoWhile(ret: Boolean): String = {
    var i = 0; var s = ""
    do { s = s + id(try { if (ret && i == 1) return "returned" + s; i.toString } catch { case _: Throwable => "handler" }); i += 1 } while (i < 3)
    s
  }
  def inGuard(ret: Boolean): String = (ret: Any) match {
    case b: Boolean if id(try { if (b) return "returned"; false } catch { case _: Throwable => true }) => "guard"
    case _ => "body"
  }
  def interpolated(ret: Boolean): String = s"<${try { if (ret) return "returned"; "body" } catch { case _: Throwable => "handler" }}>"
  def inSynchronized(ret: Boolean): String = this.synchronized { id(try { if (ret) return "returned"; "body" } catch { case _: Throwable => "handler" }) }
  def constructorArg(ret: Boolean): String = new Base(try { if (ret) return "returned"; "body" } catch { case _: Throwable => "handler" }).s
  def inSynchronizedOperand(ret: Boolean): String = "<" + this.synchronized { try { if (ret) return "returned"; "body" } catch { case _: Throwable => "handler" } }

  // where the `return` is
  def inHandler(ret: Boolean): String = id(try throw new Exception catch { case _: Exception => if (ret) return "returned"; "handler" })
  def inFinally(ret: Boolean): String = id(try "body" catch { case _: Throwable => "handler" } finally { if (ret) return "returned" })
  def inFinallyCaughtOutside(ret: Boolean): String =
    id(try id(try "body" catch { case _: Exception => "inner handler" } finally { if (ret) return "returned" }) catch { case _: Throwable => "handler" })
  def withFinally(ret: Boolean): String = id(try { if (ret) return "returned"; "body" } catch { case _: Throwable => "handler" } finally p("finally"))
  def inClosure(ret: Boolean): String = id(try { List(1).foreach(_ => if (ret) return "returned"); "body" } catch { case _: Throwable => "handler" })
  def inClosureAndDirect(ret: Boolean): String =
    id(try { if (!ret) return "direct"; List(1).foreach(_ => return "returned"); "body" } catch { case _: Throwable => "handler" })
  def inNestedTry(ret: Boolean): String =
    id(try { try { if (ret) return "returned"; "body" } catch { case _: Throwable => "inner handler" } } catch { case _: Throwable => "handler" })

  // the handler
  @annotation.nowarn("msg=catches all Throwables")
  def caseWildcard(ret: Boolean): String = id(try { if (ret) return "returned"; "body" } catch { case _ => "handler" })
  def caseThrowable(ret: Boolean): String = id(try { if (ret) return "returned"; "body" } catch { case _: Throwable => "handler" })
  def caseBoundThrowable(ret: Boolean): String = id(try { if (ret) return "returned"; "body" } catch { case t: Throwable => "handler " + t.getClass.getSimpleName })
  def caseNonFatal(ret: Boolean): String = id(try { if (ret) return "returned"; "body" } catch { case NonFatal(_) => "handler" })
  val pf: PartialFunction[Throwable, String] = { case _ => "handler" }
  def partialFunction(ret: Boolean): String = id(try { if (ret) return "returned"; "body" } catch pf)
  def noMatch(ret: Boolean): String = id(try { if (ret) return "returned"; "body" } catch { case _: IllegalStateException => "handler" })
  def rethrows(ret: Boolean): String = id(try { if (ret) return "returned"; "body" } catch { case t: Throwable => p("rethrow"); throw t })
  // The `return` is not an exception in the source, so a handler for ControlThrowable does not see it either.
  def controlThrowable(ret: Boolean): String = id(try { if (ret) return "returned"; "body" } catch { case _: ControlThrowable => "handler" })
  def otherThrowable(ret: Boolean): String = id(try { if (ret) throw new Error; "body" } catch { case _: Throwable => "handler" })

  // nested lifted `try`s
  def nested(r1: Boolean, r2: Boolean, r3: Boolean): String =
    id(try {
      if (r1) return "returned 1"
      id(try {
        if (r2) return "returned 2"
        id(try { if (r3) return "returned 3"; "body 3" } catch { case _: Throwable => "handler 3" })
      } catch { case _: Throwable => "handler 2" })
    } catch { case _: Throwable => "handler 1" })
  // The `try` around the lifted one is not lifted.
  def enclosing(ret: Boolean): Int = try 1 + (try { if (ret) return 5; 2 } catch { case _: Exception => 0 }) catch { case _: Throwable => 7 }
  def enclosingFinally(ret: Boolean): Int =
    try 1 + (try { if (ret) return 5; 2 } catch { case _: Exception => 0 }) catch { case _: Throwable => 7 } finally p("finally")
  // A closure between the `try` and the method: the `return` is non-local in the source, and a handler in
  // the closure may catch it as before.
  def tryInClosure(ret: Boolean): String = {
    List(1).foreach(_ => p(id(try { if (ret) return "returned"; "body" } catch { case _: Throwable => "handler" })))
    "end"
  }
  def lazyVal(ret: Boolean): String = { lazy val s = id(try { if (ret) return "returned"; "body" } catch { case _: Throwable => "handler" }); s }
  def localMethod(ret: Boolean): String = { def m: String = id(try { if (ret) return "returned"; "body" } catch { case _: Throwable => "handler" }); "<" + m + ">" }

  // returns at different stack depths under a `finally` or in `synchronized`
  def twoDepthsFinally(r1: Boolean, r2: Boolean): Int =
    try { if (r1) return 9; 1 + (try { if (r2) return 5; 2 } catch { case _: Throwable => 0 }) } finally p("finally")
  def twoDepthsSynchronized(r1: Boolean, r2: Boolean): Int =
    this.synchronized { if (r1) return 9; 1 + (try { if (r2) return 5; 2 } catch { case _: Throwable => 0 }) }
  def twoDepthsLifted(r1: Boolean, r2: Boolean): Int =
    id(try { if (r1) return 9; 1 + (if (r2) return 5 else 2) } catch { case _: Throwable => 0 } finally p("finally"))
  def inMatch(ret: Boolean): String =
    id(try { ret match { case false => "body"; case true => return "returned" } } catch { case _: Throwable => "handler" } finally p("finally"))

  // result types
  def unit(ret: Boolean): Unit = { id(try { if (ret) return; 1 } catch { case _: Throwable => 2 }); p("not returned") }
  def nothing(ret: Boolean): Nothing = { id(try { if (ret) return throw new IllegalStateException("thrown"); 1 } catch { case _: IllegalArgumentException => 2 }); throw new IllegalStateException("not thrown") }
  def nothingTry(ret: Boolean): String = id[Nothing](try { if (ret) return "returned" else throw new Exception } catch { case _: Exception => return "handler" })
  def primitive(ret: Boolean): Long = 1L + (try { if (ret) return 5L; 2L } catch { case _: Throwable => 7L })
  def valueClass(ret: Boolean): Meters = id(try { if (ret) return new Meters(5); new Meters(2) } catch { case _: Throwable => new Meters(7) })
  def byNameFunction(ret: Boolean): (=> String) => String =
    id[(=> String) => String](try { if (ret) return (_ => "returned"); (_ => "body") } catch { case _: Throwable => (_ => "handler") })
  def generic[T](ret: Boolean, t: T, u: T, v: T): T = id(try { if (ret) return t; u } catch { case _: Throwable => v })

  def show(name: String)(f: Boolean => Any): Unit =
    println(s"$name: ${List(false, true).map(r => try f(r).toString catch { case e: Exception => e.getMessage }).mkString(", ")}")

  def main(args: Array[String]): Unit = {
    show("operand")(operand)
    show("notLifted")(notLifted)
    show("argument")(argument)
    show("byNameArg")(byNameArg)
    show("strictByName")(strictByName)
    show("assignField")(assignField)
    show("inWhile")(inWhile)
    show("inDoWhile")(inDoWhile)
    show("inGuard")(inGuard)
    show("interpolated")(interpolated)
    show("inSynchronized")(inSynchronized)
    show("inSynchronizedOperand")(inSynchronizedOperand)
    show("constructorArg")(constructorArg)
    show("inHandler")(inHandler)
    show("inFinally")(inFinally)
    show("inFinallyCaughtOutside")(inFinallyCaughtOutside)
    show("withFinally")(withFinally)
    show("inClosure")(inClosure)
    show("inClosureAndDirect")(inClosureAndDirect)
    show("inNestedTry")(inNestedTry)
    show("caseWildcard")(caseWildcard)
    show("caseThrowable")(caseThrowable)
    show("caseBoundThrowable")(caseBoundThrowable)
    show("caseNonFatal")(caseNonFatal)
    show("partialFunction")(partialFunction)
    show("noMatch")(noMatch)
    show("rethrows")(rethrows)
    show("controlThrowable")(controlThrowable)
    show("otherThrowable")(otherThrowable)
    for (r1 <- List(false, true); r2 <- List(false, true); r3 <- List(false, true)) println(s"nested($r1, $r2, $r3): ${nested(r1, r2, r3)}")
    show("enclosing")(enclosing)
    show("enclosingFinally")(enclosingFinally)
    show("tryInClosure")(tryInClosure)
    show("lazyVal")(lazyVal)
    show("localMethod")(localMethod)
    for (r1 <- List(false, true); r2 <- List(false, true)) {
      println(s"twoDepthsFinally($r1, $r2): ${twoDepthsFinally(r1, r2)}")
      println(s"twoDepthsSynchronized($r1, $r2): ${twoDepthsSynchronized(r1, r2)}")
      println(s"twoDepthsLifted($r1, $r2): ${twoDepthsLifted(r1, r2)}")
    }
    show("inMatch")(inMatch)
    show("unit")(unit)
    show("nothing")(nothing)
    show("nothingTry")(nothingTry)
    show("primitive")(primitive)
    show("valueClass")(r => valueClass(r).value)
    show("byNameFunction")(r => byNameFunction(r)(""))
    show("generic")(r => generic(r, "returned", "body", "handler"))
    show("specialized")(r => s"${new Spec[Int].get(r, 5, 2, 7)} ${new Spec[String].get(r, "returned", "body", "handler")}")
    show("valueClassMethod")(r => new Wrap("returned").get(r))
  }
}
