package lp

class C1 extends Base0 with WithVal with Stack1 with Stack2 with AbsOverride
class C2 extends C1 with LazyMany with SelfT with InitOrder with WithPrivate
abstract class C3 extends WithVal { override lazy val slow = "over"; override val tag = "t3" }
object O1 extends C3 with Stack2 { def call = log("x") + secret + InTrait.x + count }
class C4 extends { val early = 1 } with InitOrder
object Companion1 { val shared = new C2; def apply() = new Companion1 }
class Companion1 { private val p = 5; def sum = Companion1.shared.pq + p }
trait Forward { def f(x: Int): Int; def g = f(1) }
class Impl extends Forward { def f(x: Int) = x }
trait Generic[T] { def in(t: T): T; def out: List[T] = Nil }
class GI extends Generic[String] { def in(t: String) = t; override def out = List("a") }
class GJ extends GI with Generic[String]
