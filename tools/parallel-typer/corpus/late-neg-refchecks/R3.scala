package lnr
class Base3 { def m(x: Int): Int = x; final def f = 1; val v = 1; def d: String = "d" }
class Bad3 extends Base3 { def m(x: Int): String = "s"; override def f = 2; override def nothing = 3 }
trait T3 { def t: Int }
class Abs3 extends T3
object Dep3 { @deprecated("old", "1") def old = 1; def use = old + Dep4.old }
class Ov3 extends Base3 { override val d = "x"; override def v = 2 }
