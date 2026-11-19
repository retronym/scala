//> using options -Ycheck:_
//> using filter "^\[No[tw] check"
//
// -Ycheck re-typechecks a copy of the trees, so the program compiles and runs as it does without it.

trait Show[A] { def show(a: A): String }

trait Foo { def next: Foo }
object Foo { implicit def foo(implicit rec: => Foo): Foo = new Foo { def next = rec } }

object Test {
  val s: Show[Int] = i => s"show $i"

  def main(args: Array[String]): Unit = {
    var count = 0
    val y = 10
    val s1: Show[Int] = i => { count += 1; (i * y).toString }
    val r: Runnable = () => count += 1
    val f: java.util.function.Function[String, Integer] = _.length + y
    r.run()
    println(s.show(1))
    println(s1.show(2))
    println(f.apply("abc"))
    println(count)
    println(implicitly[Foo].next ne null)
  }
}
