//> using options -Ycheck:_ -Werror
//
// The lambdas that erasure keeps for LambdaMetaFactory have the erased SAM type, not FunctionN.

trait Show[A] { def show(a: A): String }

object Test {
  val s: Show[Int] = i => i.toString

  def mk(x: Int): Show[Int] = i => (i + x).toString

  def main(args: Array[String]): Unit = {
    var count = 0
    val y = 10
    val s1: Show[Int] = i => { count += 1; (i * y).toString }
    val r: Runnable = () => count += 1
    val f: java.util.function.Function[String, Integer] = _.length + y
    val p: java.util.function.IntPredicate = _ > y
    r.run()
    println(s.show(1) + s1.show(2) + mk(3).show(4) + f.apply("abc") + p.test(count))
  }
}
