// Erasure's double-definition check, for the pairs RefChecks records and for those it doesn't.
// Every error here comes from Erasure: an error in RefChecks would stop the run before it.

// Don't match at refchecks, same erasure: errors.
trait A1 { def f(x: List[String]): Int }
trait A2 { def f(x: List[Int]): Int }
abstract class InheritedClash extends A1 with A2

trait B1 { def f(x: List[String]): Int }
abstract class DefinedClash extends B1 { def f(x: List[Int]): Int }

trait C1 { def f(x: => Int): Int }
trait C2 { def f(x: () => Int): Int }
abstract class ByNameClash extends C1 with C2

trait D1 { def f(x: Int*): Int }
trait D2 { def f(x: collection.immutable.Seq[Int]): Int }
abstract class VarargsClash extends D1 with D2

// Same name and erasure after posterasure only.
class V(val s: String) extends AnyVal
trait E1 { def f(x: V): Int }
trait E2 { def f(x: String): Int }
abstract class ValueClassClash extends E1 with E2

// Matched at refchecks, recorded, and silent, though the erased types are the same.
trait F1[T] { def f(x: List[T]): Int }
abstract class MatchedInSubclass extends F1[String] { def f(x: List[String]): Int }

// Matched in the class, recorded for it, and clashing in no other.
trait G1[T] { def f(x: T): Int }
trait G2 extends G1[String] { def f(x: String): Int }
abstract class MatchedTwice extends G1[String] with G2

// Patmat substitutes the binder in these infos with `updateInfo`, at the phase whose infos the check reads.
class P { type T }
object LocalClassesInCase {
  def m(a: Any): Int = a match {
    case p: P =>
      trait H { def f(x: List[p.T]): Int }
      abstract class L extends H { def f(x: List[p.T]): Int }
      0
  }
}
