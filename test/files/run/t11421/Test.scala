package p {
  class TC(val id: String)
  object TC { implicit val fallback: TC = new TC("fallback in companion of TC") }
}
package q {
  object Imp { implicit def z: p.TC = new p.TC("q.Imp.z") }
  import Imp._
  class C {
    // `z` is Imp.z: the import has precedence over the package object's member, which is in another file.
    def site: String = { val check: p.TC = z; s"z is ${check.id}; implicitly finds ${implicitly[p.TC].id}" }
  }
}
package r {
  object Imp { implicit def z: p.TC = new p.TC("r.Imp.z") }
  import Imp._
  class C {
    def site: String = { val check: p.TC = z; s"z is ${check.id}; implicitly finds ${implicitly[p.TC].id}" }
  }
}
object Test {
  def main(args: Array[String]): Unit = {
    println(new q.C().site)
    println(new r.C().site)
  }
}
