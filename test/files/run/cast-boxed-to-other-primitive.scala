object Test {
  def show(label: String)(body: => Any): Unit =
    println(label + " = " + (try { val b = body; b.getClass.getName + ":" + b } catch { case t: Throwable => t.getClass.getName }))

  def l: java.lang.Long = 1L
  def c: java.lang.Character = 'a'
  def b: java.lang.Boolean = true

  def main(args: Array[String]): Unit = {
    // the cast result is used at type Int: ClassCastException
    show("val x: Int = l.asInstanceOf[Int]")({ val x: Int = l.asInstanceOf[Int]; x })
    // the same receiver typed Any, result boxed: ClassCastException
    show("val a: Any = (l: Any).asInstanceOf[Int]")({ val a: Any = (l: Any).asInstanceOf[Int]; a })
    // the cast result is boxed straight away: the cast disappears
    show("val a: Any = l.asInstanceOf[Int]")({ val a: Any = l.asInstanceOf[Int]; a })
    show("List[Any](c.asInstanceOf[Int]).head")(List[Any](c.asInstanceOf[Int]).head)
    show("val a: Any = b.asInstanceOf[Double]")({ val a: Any = b.asInstanceOf[Double]; a })
    show("\"\" + l.asInstanceOf[Boolean]")("" + l.asInstanceOf[Boolean])
    show("l.asInstanceOf[Int].isInstanceOf[Int] (through Any)")({ val a: Any = l.asInstanceOf[Int]; a.isInstanceOf[Int] })
    show("[T <: Int] l.asInstanceOf[T].toString")({ def g[T <: Int] = l.asInstanceOf[T].toString; g[Int] })
  }
}
