// The member `m` erases to the same signature as the bridges for `Foo.m` and `Bar.m`, so it is renamed
// (it is in an anonymous class, scala/bug#6260). One bridge serves both overridden methods; a second
// one used to be added because the lookup for an existing bridge used the member's new name.

class Bippy[A](val value: A) extends AnyVal

trait Foo[A] { def m(x: A): Int }
trait Bar[A] extends Foo[A] { def m(x: A): Int }

object Test {
  def create[A] = new Bar[Bippy[A]] { def m(x: Bippy[A]) = 42 }

  def main(args: Array[String]): Unit = {
    val b = create[Int]
    assert(b.m(new Bippy(1)) == 42)
    assert((b: Foo[Bippy[Int]]).m(new Bippy(1)) == 42)
  }
}
