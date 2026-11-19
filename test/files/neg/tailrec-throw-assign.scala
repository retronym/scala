import annotation.tailrec

// The operand of `throw` and the right-hand side of an assignment are not in tail position
class C {
  var x: Any = null
  @tailrec final def thr(n: Int): Exception = if (n == 0) new Exception else throw thr(n - 1)
  @tailrec final def thrMatch(n: Int): Exception = if (n == 0) new Exception else throw (n match { case _ => thrMatch(n - 1) })
  @tailrec final def asg(n: Int): Unit = if (n > 0) x = asg(n - 1)
  def local(): Any = {
    var y: Any = null
    @tailrec def asg(n: Int): Unit = if (n > 0) y = asg(n - 1)
    asg(1)
    y
  }
}
