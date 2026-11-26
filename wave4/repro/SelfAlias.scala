import scala.collection.mutable._
object SelfAlias {
  def t(name: String, expected: String)(f: => Any): Unit = {
    val th = new Thread(() => {
      val got = try f.toString catch { case e: Throwable => e.getClass.getSimpleName }
      println(f"${if (got == expected) "ok  " else "FAIL"} $name%-58s expected $expected%-26s got $got")
    }); th.setDaemon(true); th.start(); th.join(5000)
    if (th.isAlive) println(f"FAIL $name%-58s expected $expected%-26s got <no result after 5s>")
  }
  def main(a: Array[String]): Unit = {
    t("ArrayDeque(6,-2).addAll(self.view.reverse)", "ArrayDeque(6, -2, -2, 6)") { val d = ArrayDeque(6, -2); d.addAll(d.view.reverse) }
    t("ArrayDeque(0,1,-1,-1).patchInPlace(3, self.view.map(_+1), 65)", "ArrayDeque(0, 1, -1, 1, 2, 0, 0)") { val d = ArrayDeque(0, 1, -1, -1); d.patchInPlace(3, d.view.map(_ + 1), 65) }
    t("ArrayDeque(1,4).subtractAll(self.view.slice(0,2))", "ArrayDeque()") { val d = ArrayDeque(1, 4); d.subtractAll(d.view.slice(0, 2)) }
    t("UnrolledBuffer(1).prependAll(self.view.map(_+1))", "UnrolledBuffer(2, 1)") { val d = UnrolledBuffer(1); d.prependAll(d.view.map(_ + 1)) }
    t("self.view.map(_+1) ++=: UnrolledBuffer(1)", "UnrolledBuffer(2, 1)") { val d = UnrolledBuffer(1); d.view.map(_ + 1) ++=: d }
    t("UnrolledBuffer(3,4).addAll(self.iterator)", "UnrolledBuffer(3, 4, 3, 4)") { val d = UnrolledBuffer(3, 4); d.addAll(d.iterator); d.take(8) }
    t("UnrolledBuffer(5,-1).addAll(self.view.filter(_ > 0))", "UnrolledBuffer(5, -1, 5)") { val d = UnrolledBuffer(5, -1); d.addAll(d.view.filter(_ > 0)); d.take(8) }
    t("UnrolledBuffer(4,6).patchInPlace(2, self.view, 0)", "UnrolledBuffer(4, 6, 4, 6)") { val d = UnrolledBuffer(4, 6); d.patchInPlace(2, d.view, 0); d.take(8) }
    t("UnrolledBuffer(1,4).subtractAll(self.view.slice(0,2))", "UnrolledBuffer()") { val d = UnrolledBuffer(1, 4); d.subtractAll(d.view.slice(0, 2)) }
    t("ArrayBuffer(-2).addAll(self.view.reverse)", "ArrayBuffer(-2, -2)") { val d = ArrayBuffer(-2); d.addAll(d.view.reverse) }
    t("ArrayBuffer(-2).addAll(self.view)   [control]", "ArrayBuffer(-2, -2)") { val d = ArrayBuffer(-2); d.addAll(d.view) }
    t("ListBuffer(1,4).subtractAll(self.view.slice(0,2))", "ListBuffer()") { val d = ListBuffer(1, 4); d.subtractAll(d.view.slice(0, 2)) }
    t("ListBuffer(6,-2).addAll(self.view.reverse)  [control]", "ListBuffer(6, -2, -2, 6)") { val d = ListBuffer(6, -2); d.addAll(d.view.reverse) }
    System.exit(0)
  }
}
