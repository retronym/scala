//> using options -Werror

final class X
final class Y
final class G[T]
object O
object P

class Test {
  def eqXY(x: X, y: Y) = x == y           // w: equal if both are null
  def neXY(x: X, y: Y) = x != y           // w: as above
  def refXY(x: X, y: Y) = x eq y          // w: as above
  def eqGY(g: G[Int], y: Y) = g == y      // w: as above

  def eqXO(x: X) = x == O                 // w: always false, O is not null
  def eqOX(x: X) = O == x                 // w: as above
  def eqOP = O == P                       // w: always false

  def check(): Unit = assert(eqXY(null, null) && !neXY(null, null) && refXY(null, null))
}
