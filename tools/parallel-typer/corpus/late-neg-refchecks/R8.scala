package lnr
class Base8 { def m(x: Int): Int = x; final def f = 1; val v = 1; def d: String = "d" }
class Bad8 extends Base8 { def m(x: Int): String = "s"; override def f = 2; override def nothing = 3 }
trait T8 { def t: Int }
class Abs8 extends T8
object Dep8 { @deprecated("old", "1") def old = 1; def use = old + Dep1.old }
class Ov8 extends Base8 { override val d = "x"; override def v = 2 }
