import scala.jdk.{IntAccumulator, AnyAccumulator}
import scala.collection.mutable.ArrayBuffer
import scala.util.Try
object Test {
  def show(what: String, r: => Any): Unit = println(what + ": " + Try(r).fold(e => e.getClass.getSimpleName, x => String.valueOf(x)))
  def main(args: Array[String]): Unit = {
    val b = ArrayBuffer(7)
    show("ArrayBuffer(7)(1)", b(1))
    show("ArrayBuffer(7)(1) = 5", b(1) = 5)
    val a = IntAccumulator(7)
    show("IntAccumulator(7)(1)", a(1))
    show("IntAccumulator(7)(1) = 5", a(1) = 5)
    show("  then length, toList", (a.length, a.toList))
    val s = AnyAccumulator("x")
    show("AnyAccumulator(x)(1)", s(1))
    show("AnyAccumulator(x)(1) = y", s(1) = "y")
    a += 8
    show("IntAccumulator after += 8", a.toList)
    val big = IntAccumulator.fromSpecific(0 until 5000)
    show("IntAccumulator(0 until 5000)(5000)", big(5000))
    show("IntAccumulator(0 until 5000)(-1)", big(-1))
  }
}
