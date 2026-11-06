package exov

// several units, so that threads use the same method `P.apply` with different prefixes at once
class Use08(d: D) {
  def go(c: P[_ >: Throwable]): D = d.on[Throwable](c.apply)
  def other(c: P[_ >: Exception]): Exception => Unit = c.apply
  def both(a: P[_ >: Throwable], b: P[_ >: Throwable]): D = d.on[Throwable](a.apply).on[Throwable](b.apply)
}
