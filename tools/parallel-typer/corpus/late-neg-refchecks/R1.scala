package lnr
class Base1 { def m(x: Int): Int = x; final def f = 1; val v = 1; def d: String = "d" }
class Bad1 extends Base1 { def m(x: Int): String = "s"; override def f = 2; override def nothing = 3 }
trait T1 { def t: Int }
class Abs1 extends T1
object Dep1 { @deprecated("old", "1") def old = 1; def use = old + Dep2.old }
class Ov1 extends Base1 { override val d = "x"; override def v = 2 }
