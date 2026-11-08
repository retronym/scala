package lnr
class Base2 { def m(x: Int): Int = x; final def f = 1; val v = 1; def d: String = "d" }
class Bad2 extends Base2 { def m(x: Int): String = "s"; override def f = 2; override def nothing = 3 }
trait T2 { def t: Int }
class Abs2 extends T2
object Dep2 { @deprecated("old", "1") def old = 1; def use = old + Dep3.old }
class Ov2 extends Base2 { override val d = "x"; override def v = 2 }
