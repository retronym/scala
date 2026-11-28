class A(val id: String)
class B(val id: String)

object O {
  implicit val x: A = new A("O.x")
  implicit val y: B = new B("O.y")
}

// The selectors of each import clause bind the same names in any order, so implicit search finds
// the same implicits in any order.
class Chain1 {
  import O.{x => y, y => z}
  def site = { val a: A = y; val b: B = z; s"${a.id}, ${b.id}; ${implicitly[A].id}, ${implicitly[B].id}" }
}
class Chain2 {
  import O.{y => z, x => y}
  def site = { val a: A = y; val b: B = z; s"${a.id}, ${b.id}; ${implicitly[A].id}, ${implicitly[B].id}" }
}
class Swap1 {
  import O.{x => y, y => x}
  def site = { val a: A = y; val b: B = x; s"${a.id}, ${b.id}; ${implicitly[A].id}, ${implicitly[B].id}" }
}
class Swap2 {
  import O.{y => x, x => y}
  def site = { val a: A = y; val b: B = x; s"${a.id}, ${b.id}; ${implicitly[A].id}, ${implicitly[B].id}" }
}

// A renamed or masked name is still not imported by the wildcard.
object Fallback {
  implicit val a: A = new A("Fallback.a")
}
class Masked {
  import Fallback._
  def site = {
    import O.{x => _, _}
    s"${implicitly[A].id}, ${implicitly[B].id}"
  }
}
class RenamedWithWildcard {
  def site = {
    import O.{x => w, _}
    val a: A = w
    s"${a.id}; ${implicitly[A].id}, ${implicitly[B].id}"
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    println(new Chain1().site)
    println(new Chain2().site)
    println(new Swap1().site)
    println(new Swap2().site)
    println(new Masked().site)
    println(new RenamedWithWildcard().site)
  }
}
