package lnr
class Base5 { def m(x: Int): Int = x; final def f = 1; val v = 1; def d: String = "d" }
class Bad5 extends Base5 { def m(x: Int): String = "s"; override def f = 2; override def nothing = 3 }
trait T5 { def t: Int }
class Abs5 extends T5
object Dep5 { @deprecated("old", "1") def old = 1; def use = old + Dep6.old }
class Ov5 extends Base5 { override val d = "x"; override def v = 2 }
