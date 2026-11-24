//> using options -nowarn
object Test {
  val log = new collection.mutable.ArrayBuffer[String]
  def t[A](s: String, a: A): A = { log += s; a }
  def show(name: String)(body: => Any): Unit = {
    log.clear()
    val r = try body.toString catch { case e: Throwable => e.getClass.getSimpleName }
    println(s"$name: ${log.mkString(" ")} -> $r")
  }
  class C { val arr = new Array[Int](2) }
  var size = 0
  lazy val lz: C = t("lz", { size += 1; new C })
  lazy val lz2: C = t("lz2", new C)
  object O { t("O", ()); val arr = new Array[Int](2) }
  object O2 { t("O2", ()); val arr = new Array[Int](2) }
  def idx(): Int = t("i", size)

  def main(args: Array[String]): Unit = {
    // receiver first, then the index, then the right-hand side
    show("apply    lz2.arr(i)       ")(lz2.arr(t("i", 0)))
    show("update   O2.arr(i) = v    ")(O2.arr(t("i", 0)) = t("v", 1))
    show("op=      lz.arr(i) += v   ")(lz.arr(idx()) += t("v", 1))
    show("op=      O.arr(i) += v    ")(O.arr(t("i", 0)) += t("v", 1))
    // the index was computed before the lazy val's initializer changed `size`
    println(lz.arr.mkString(","))
  }
}
