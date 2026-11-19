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

// The bridge `f(List): Object` has the parameter types of the member `f(List): String`, but the JVM also
// matches the result type, so the bridge overrides `BaseL.f`, which the member does not.
class BaseL { def f(a: List[Int]): Object = "BaseL.f" }

trait TBound[A <: List[_]] { def f(a: A): Object }
class CBound extends BaseL with TBound[List[String]] { def f(a: List[String]): String = "CBound.f" }

trait TList[A] { def f(a: List[A]): Object }
class CList extends BaseL with TList[String] { def f(a: List[String]): String = "CList.f" }

trait TMemL { type M <: List[_]; def f(a: M): Object }
class CMemL extends BaseL with TMemL { type M = List[String]; def f(a: List[String]): String = "CMemL.f" }
