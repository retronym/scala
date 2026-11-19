package lnr
class Base7 { def m(x: Int): Int = x; final def f = 1; val v = 1; def d: String = "d" }
class Bad7 extends Base7 { def m(x: Int): String = "s"; override def f = 2; override def nothing = 3 }
trait T7 { def t: Int }
class Abs7 extends T7
object Dep7 { @deprecated("old", "1") def old = 1; def use = old + Dep8.old }
class Ov7 extends Base7 { override val d = "x"; override def v = 2 }
