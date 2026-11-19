// `J.bar` and `J.baz` have package access, so they are not overridden from package `b`, as in Java:
// `override` is not required, and the result type may differ.
package b

class S extends a.J {
  def bar(): Unit = ()
  def baz(x: Int): String = ""
}

object Use {
  def len = (new S).baz(1).length
}
