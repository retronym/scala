// The bridge for `f(a: String)` has the signature of the user's `f(a: Object)`.
// In a named class this is an error; in an anonymous class it used to produce a class file
// with a duplicate method (ClassFormatError at run time).

trait T[A] { def f(a: A): String }

class C {
  val t = new T[String] { def f(a: String) = "s"; def f(a: Object) = "o" }
}
