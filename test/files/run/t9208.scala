//> using options -Wconf:cat=unused-imports:s
// In each class the simple name of the implicit denotes the definition named in the output, so that
// definition is the implicit that is in scope under that name.
class TC(val id: String)
object TC { implicit val fallback: TC = new TC("fallback in companion") }

object Imp { implicit def y: TC = new TC("Imp.y") }

class OwnMember {
  import Imp._
  implicit val y: TC = new TC("OwnMember.y")
  def site: String = s"y is ${y.id}; implicitly finds ${implicitly[TC].id}"
}

trait Par { implicit val y: TC = new TC("Par.y") }
class Inherited extends Par {
  import Imp._
  class Inner {
    def site: String = s"y is ${y.id}; implicitly finds ${implicitly[TC].id}"
  }
}

object Wild { implicit def x: TC = new TC("Wild.x") }
object Expl { implicit val x: TC = new TC("Expl.x") }
class WildcardThenExplicit {
  def site = {
    import Wild._
    import Expl.x
    s"x is ${x.id}; implicitly finds ${implicitly[TC].id}"
  }
}
class ExplicitThenWildcard {
  def site = {
    import Expl.x
    import Wild._
    s"x is ${x.id}; implicitly finds ${implicitly[TC].id}"
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    println(new OwnMember().site)
    val o = new Inherited
    println(new o.Inner().site)
    println(new WildcardThenExplicit().site)
    println(new ExplicitThenWildcard().site)
  }
}
