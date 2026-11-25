//> using options -Wconf:cat=unused-imports:s
// Three implicits: `x` improves on `y` (its type is more specific), `y` improves on `z` (it is defined in a
// subclass of the class that defines `z`), and `x` and `z` are unrelated. `x` is not more specific than every
// other alternative, so by SLS 6.26.3 each of the four searches below is ambiguous.
//
// Ranking tries the candidates in order and, when one typechecks, drops the remaining candidates that it
// improves on without looking at them again. The relation is not transitive, so the outcome depends on the
// order of the candidates: if `y` is tried before `z`, `z` is dropped, `x` then beats `y` and wins; if `x` is
// tried first, `y` is dropped and `z` is left to be ambiguous with `x`. The order follows the linearisation of
// the enclosing class and the nesting of the imports. scala/scala3#9066 describes the same for Scala 3 before
// its ordering was changed.
//
// This test records the current outcomes: the first search of each pair is accepted and the second is not.
package p

class TC
trait M1; trait M2; trait M3

trait Low { implicit val z: TC with M2 = new TC with M2 }
trait Y extends Low { implicit val y: TC with M1 = new TC with M1 }
trait X { implicit val x: TC with M1 with M3 = new TC with M1 with M3 }

class XY extends X with Y { def site = implicitly[TC] } // accepted, picks x
class YX extends Y with X { def site = implicitly[TC] } // ambiguous

trait LowOY { implicit val z: TC with M2 = new TC with M2 }
object OY extends LowOY { implicit val y: TC with M1 = new TC with M1 }
object OX { implicit val x: TC with M1 with M3 = new TC with M1 with M3 }

class Imports {
  def xy = {
    import OX._
    import OY._
    implicitly[TC] // accepted, picks x
  }
  def yx = {
    import OY._
    import OX._
    implicitly[TC] // ambiguous
  }
}
