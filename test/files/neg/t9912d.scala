// Like t9912, but the parameter of the overridden method is not a bare type parameter.
// It still erases to Object, so the bridge would override `Base.f` and `(c: Base).f(Integer.valueOf(1))`
// would throw ClassCastException.

class Base { def f(a: Object): String = "Base.f" }

trait TArr[A] { def f(a: Array[A]): String }
class CArr extends Base with TArr[String] { def f(a: Array[String]) = "CArr.f" }

trait TMem { type M; def f(m: M): String }
class CMem extends Base with TMem { type M = String; def f(a: String) = "CMem.f" }

trait TAnd[A] { def f(a: A with Serializable): String }
class CAnd extends Base with TAnd[String] { def f(a: String with Serializable) = "CAnd.f" }
