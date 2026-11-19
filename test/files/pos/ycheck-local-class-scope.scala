//> using options -Ycheck:_ -Werror
//
// A local class is in scope for its siblings (a by-name implicit expands into one, `LazyDefns$1`),
// and the existential in `Class[_]` of `ModuleSerializationProxy`'s constructor is bound in its type.

trait Foo { def next: Foo }
object Foo { implicit def foo(implicit rec: => Foo): Foo = new Foo { def next = rec } }

case object Obj

object Test {
  def main(args: Array[String]): Unit = {
    final class C { def x = 1 }
    val c = new C
    println(c.x)
    println(implicitly[Foo].next ne null)
  }
}
