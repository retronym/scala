package lp

object Lam {
  def compose[A, B, C](f: A => B, g: B => C): A => C = a => g(f(a))
  def byName(x: => Int, y: => String): () => String = () => y + x
  def counter: () => Int = { var c = 0; () => { c += 1; c } }
  def nonLocal(xs: List[Int]): Int = { xs.foreach(x => if (x > 1) return x); 0 }
  def localDefs(a: Int): Int = {
    var v = a
    def inc(): Unit = v += 1
    def twice(i: Int): Int = { inc(); def inner = i + v; inner * 2 }
    class Loc { def z = twice(a) + v }
    object LocO { def y = new Loc().z }
    LocO.y
  }
  trait Sam { def run(i: Int): Int }
  val sam: Sam = i => i + 1
  val runnable: Runnable = () => println("r")
  val cmp: java.util.Comparator[String] = (a, b) => a.length - b.length
  def tryLift(x: Int): Int = x + (try x / 2 catch { case _: ArithmeticException => 0 } finally println("f"))
  @annotation.tailrec def loop(i: Int, acc: Int): Int = if (i == 0) acc else loop(i - 1, acc + i)
  final def tail2(xs: List[Int]): Int = xs match { case Nil => 0; case _ :: t => tail2(t) }
  def varargs(xs: Int*): Int = xs.sum
  @annotation.varargs def jv(s: String, xs: String*): Int = xs.length
  def curried(a: Int)(b: Int)(implicit c: Ordering[Int]): Int = c.compare(a, b)
  def defaults(a: Int = 1, b: String = "x"): String = b + a
  val pf: PartialFunction[Any, Int] = { case i: Int => i; case s: String if s.nonEmpty => s.length }
  def structural(x: { def size: Int; def foo(i: Int): String }): Int = x.size + x.foo(1).length
  def sym = Symbol("late")
  def strSwitch(s: String) = (s: @annotation.switch) match { case "a" => 1; case "b" => 2; case _ => 3 }
  def intSwitch(i: Int) = (i: @annotation.switch) match { case 1 => "a"; case 2 => "b"; case _ => "c" }
  val tupled = ((a: Int, b: Int) => a + b).tupled
}
abstract class Delayed extends DelayedInit { var run = 0; def delayedInit(b: => Unit): Unit = { run += 1; b } }
object Main1 extends App { val args2 = args.length; println(Lam.counter()) }
class DI extends Delayed { val k = 1 }
