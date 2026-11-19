class Top {
  class E extends Exception

  def multi(t: => Unit): String =
    try { t; "none" } catch {
      case _: E         => "mine"
      case _: Exception => "other"
    }

  def bound(t: => Unit): String =
    try { t; "none" } catch {
      case e: E         => "mine"
      case e: Exception => "other"
    }

  def single(t: => Unit): String =
    try { t; "none" } catch {
      case _: E => "mine"
    }

  def viaMatch(t: Throwable): String = t match {
    case _: E         => "mine"
    case _: Exception => "other"
  }
}

object Holder {
  class Outer {
    class E extends Exception

    def multi(t: => Unit): String =
      try { t; "none" } catch {
        case _: E         => "mine"
        case _: Exception => "other"
      }
  }

  // no outer instance to test: the prefix is a static object
  class S extends Exception
  def static(t: => Unit): String =
    try { t; "none" } catch {
      case _: S         => "mine"
      case _: Exception => "other"
    }
}

object Test {
  def rethrown(body: => String): String =
    try body catch { case _: Exception => "rethrown" }

  def main(args: Array[String]): Unit = {
    val t1 = new Top
    val t2 = new Top
    println("multi:    " + t1.multi(throw new t1.E) + " " + t1.multi(throw new t2.E))
    println("bound:    " + t1.bound(throw new t1.E) + " " + t1.bound(throw new t2.E))
    println("single:   " + t1.single(throw new t1.E) + " " + rethrown(t1.single(throw new t2.E)))
    println("match:    " + t1.viaMatch(new t1.E) + " " + t1.viaMatch(new t2.E))

    val o1 = new Holder.Outer
    val o2 = new Holder.Outer
    println("nested:   " + o1.multi(throw new o1.E) + " " + o1.multi(throw new o2.E))
    println("static:   " + Holder.static(throw new Holder.S) + " " + Holder.static(throw new RuntimeException))

    // a path through a val is not a static prefix either
    def path(t: => Unit): String =
      try { t; "none" } catch {
        case _: t1.E      => "mine"
        case _: Exception => "other"
      }
    println("path:     " + path(throw new t1.E) + " " + path(throw new t2.E))
  }
}
