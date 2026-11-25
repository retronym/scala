// A by-name parameter does not make its alternative more specific (SLS 6.26.3, 3.5.4): `=> Any` is not
// compatible with `AnyRef`. Neither alternative of `f` is applicable to the parameter types of the other,
// and the relative weights of `P.g` and `Q.g` are both 1, so both calls are ambiguous, as they are when
// the parameter is by-value.

object ByName  { def f(a: => Any, b: String) = "f(=> Any, String)"; def f(a: AnyRef, b: Any) = "f(AnyRef, Any)" }
object ByValue { def f(a: Any, b: String) = "f(Any, String)"; def f(a: AnyRef, b: Any) = "f(AnyRef, Any)" }

class P { def g(a: AnyRef) = "P.g(AnyRef)" }
object Q extends P { def g(a: => Any) = "Q.g(=> Any)" }

class PV { def g(a: AnyRef) = "PV.g(AnyRef)" }
object QV extends PV { def g(a: Any) = "QV.g(Any)" }

object Test {
  ByName.f("s", "t")
  ByValue.f("s", "t")
  Q.g("s")
  QV.g("s")
}
