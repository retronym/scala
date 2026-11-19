// javac cannot resolve a type variable of an enclosing class from inside a class nested
// in a module class, so references to it are erased in the generic signatures of such classes.
class O1[T](x: T) { object Ob { def t: T = x }; def ob = Ob }
class O3[T](x: T) { object Ob { class E { def t: T = x; def l: List[T] = List(x) } }; def e: Ob.E = new Ob.E }
class O4[T](x: T) { class C { object Ob { def t: T = x } }; def ob = (new C).Ob }
class O5[T](x: T) { object Ob { object Oc { def t: T = x } }; def oc = Ob.Oc }
class O6[T](x: T) { object Ob { class F[U](v: U) extends java.util.ArrayList[T] { def t: T = x; def u: U = v } }; def f = new Ob.F[String]("u") }
