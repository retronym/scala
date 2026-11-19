trait Base { val len: Int = nm.length; def nm: String }
class C(p: Int) extends { lazy val le = p } with Base { def nm = "" }
class D(p: Int) extends { val e: Int = p; lazy val le: Int = e + 1 } with Base { def nm = "x" * le }
object O extends { private lazy val x = 1; val y = 2 } with Base { def nm = "" }
