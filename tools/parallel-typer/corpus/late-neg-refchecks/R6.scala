package lnr
class Base6 { def m(x: Int): Int = x; final def f = 1; val v = 1; def d: String = "d" }
class Bad6 extends Base6 { def m(x: Int): String = "s"; override def f = 2; override def nothing = 3 }
trait T6 { def t: Int }
class Abs6 extends T6
object Dep6 { @deprecated("old", "1") def old = 1; def use = old + Dep7.old }
class Ov6 extends Base6 { override val d = "x"; override def v = 2 }
