// Within the one search for T, Z is looked for twice, with the same expected type and in the same context.
// The first time, as the argument of `c1`, it fails: `z` needs a Missing. The second time, below `c2`, `zz` and
// the by-name argument of `y`, it succeeds as a recursive reference to the open search for ZZ. So that a type
// was not found earlier in a search does not mean that it cannot be found later in the same search.
trait Missing
trait T { def id: String }
trait Z
object Z { implicit def z(implicit m: Missing): Z = new Z {} }
trait Y
object Y { implicit def y(implicit z: Z): Y = new Y {} }
trait ZZ extends Z
object ZZ { implicit def zz(implicit y: => Y): ZZ = new ZZ {} }
object T {
  implicit def c2(implicit zz: ZZ): T = new T { def id = "c2" }
  implicit def c1(implicit z: Z): T = new T { def id = "c1" }
}
object Test {
  def main(args: Array[String]): Unit = println(implicitly[T].id)
}
