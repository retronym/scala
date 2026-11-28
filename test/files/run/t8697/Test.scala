//> using options -Wconf:cat=other-implicit-type:s
// One program in five arrangements. The class has an implicit member `x` without a result type, defined after
// the search, and the companion of TC has an instance of lower precedence.

package plain {
  class TC(val id: String)
  object TC { implicit val fallback: TC = new TC("companion") }
  class C {
    def site = implicitly[TC].id
    implicit val x = new TC("member")
  }
}

// An unrelated method above the search mentions `x`.
package referenceBefore {
  class TC(val id: String)
  object TC { implicit val fallback: TC = new TC("companion") }
  class C {
    def unrelated = x.toString
    def site = implicitly[TC].id
    implicit val x = new TC("member")
  }
}

// The same method below the search.
package referenceAfter {
  class TC(val id: String)
  object TC { implicit val fallback: TC = new TC("companion") }
  class C {
    def site = implicitly[TC].id
    def unrelated = x.toString
    implicit val x = new TC("member")
  }
}

// `x` is inherited from a trait later in this file.
package parentBelow {
  class TC(val id: String)
  object TC { implicit val fallback: TC = new TC("companion") }
  class C extends Instances {
    def site = implicitly[TC].id
  }
  trait Instances { implicit val x = new TC("member") }
}

// `x` is inherited from a trait in Instances.scala.
package otherfile {
  class TC(val id: String)
  object TC { implicit val fallback: TC = new TC("companion") }
  class C extends Instances {
    def site = implicitly[TC].id
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    println("no other reference: " + new plain.C().site)
    println("unrelated reference before the search: " + new referenceBefore.C().site)
    println("unrelated reference after the search: " + new referenceAfter.C().site)
    println("parent trait later in the same file: " + new parentBelow.C().site)
    println("parent trait in another file: " + new otherfile.C().site)
  }
}
