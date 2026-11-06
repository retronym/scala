package lnr
class Base4 { def m(x: Int): Int = x; final def f = 1; val v = 1; def d: String = "d" }
class Bad4 extends Base4 { def m(x: Int): String = "s"; override def f = 2; override def nothing = 3 }
trait T4 { def t: Int }
class Abs4 extends T4
object Dep4 { @deprecated("old", "1") def old = 1; def use = old + Dep5.old }
class Ov4 extends Base4 { override val d = "x"; override def v = 2 }
