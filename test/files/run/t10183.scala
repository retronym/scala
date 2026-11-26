// A `return` evaluated while operands are on the stack, inside a `try` with a finalizer or a `synchronized`
object Test {
  val log = new StringBuilder
  def fin(s: String): Unit = log.append(s"[$s]")
  def f(d: Int): Int = d * 10
  class Box(val x: Int)

  def unit(c: Boolean): Unit = {
    try {
      f(if (c) return else 1)
      return
    } finally fin("unit")
  }

  def arg(b: Boolean, c: Boolean): Int = {
    try {
      if (b) return 2
      f(if (c) return 4 else 1)
    } finally fin("arg")
  }

  def receiver(s: String): Int =
    try {
      log.append(if (s == null) return -1 else s)
      if (s.isEmpty) return 0
      s.length
    } finally fin("receiver")

  def wide(c: Boolean, d: Double): Long =
    try {
      val i = 3
      i + (d + (if (c) return 5L else 2L)).toLong
    } finally fin("wide")

  def ctor(c: Boolean): Int =
    try new Box(if (c) return 7 else 1).x
    finally fin("ctor")

  def catchFinally(c: Boolean, t: Boolean): Int =
    try {
      if (t) throw new RuntimeException("t")
      f(if (c) return 8 else 2)
    } catch {
      case _: RuntimeException => f(if (c) return 9 else 3)
    } finally fin("catchFinally")

  def nested(a: Int): Int =
    try {
      try {
        if (a == 0) return 0
        f(if (a == 1) return 1 else a)
      } finally fin("inner")
    } finally fin("outer")

  def sync(c: Boolean): Int = this.synchronized {
    f(if (c) return 11 else 1)
  }

  def syncInFinally(c: Boolean): Int =
    try this.synchronized(f(if (c) return 12 else 2))
    finally fin("syncInFinally")

  def inFinalizer(a: Int): Long =
    try {
      1L + (try 2L finally { if (a > 0) return 6L })
    } finally fin("inFinalizer")

  def operandInFinalizer(a: Int, b: Boolean): Int =
    try {
      try {
        if (a == 0) return 0
        a
      } finally log.append(if (b) return -2 else "x")
    } finally fin("operandInFinalizer")

  def run[T](name: String)(body: => T): Unit = {
    log.clear()
    val r = body
    println(s"$name: $r|$log")
  }

  def main(args: Array[String]): Unit = {
    run("unit true")(unit(true))
    run("unit false")(unit(false))
    run("arg 1")(arg(true, false))
    run("arg 2")(arg(false, true))
    run("arg 3")(arg(false, false))
    run("receiver null")(receiver(null))
    run("receiver empty")(receiver(""))
    run("receiver abc")(receiver("abc"))
    run("wide true")(wide(true, 1.0))
    run("wide false")(wide(false, 1.0))
    run("ctor true")(ctor(true))
    run("ctor false")(ctor(false))
    run("catchFinally 1")(catchFinally(true, false))
    run("catchFinally 2")(catchFinally(false, false))
    run("catchFinally 3")(catchFinally(true, true))
    run("catchFinally 4")(catchFinally(false, true))
    run("nested 0")(nested(0))
    run("nested 1")(nested(1))
    run("nested 2")(nested(2))
    run("sync true")(sync(true))
    run("sync false")(sync(false))
    run("syncInFinally true")(syncInFinally(true))
    run("syncInFinally false")(syncInFinally(false))
    run("inFinalizer 0")(inFinalizer(0))
    run("inFinalizer 1")(inFinalizer(1))
    run("operandInFinalizer 0 true")(operandInFinalizer(0, true))
    run("operandInFinalizer 0 false")(operandInFinalizer(0, false))
    run("operandInFinalizer 1 true")(operandInFinalizer(1, true))
    run("operandInFinalizer 1 false")(operandInFinalizer(1, false))
  }
}
