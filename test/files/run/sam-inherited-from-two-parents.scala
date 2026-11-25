// SAM types that inherit their abstract method from two unrelated parents with different erasures.
// LambdaMetafactory can't implement both signatures, so the lambda must be an anonymous class.
trait G[A] { def ap(a: A): A }
trait I1 { def ap(a: Int): Int }
trait B1 extends G[Int] with I1
trait B2 extends I1 with G[Int]

case class VS(s: String) extends AnyVal
trait S1 { def ap(a: VS): VS }
trait B3 extends G[VS] with S1

trait G2[A, B] { def ap(a: A): B }
trait H2[A] { def ap(a: A): Int }
trait B4[A] extends G2[A, Int] with H2[A]

trait IntFun { def apply(x: Int): Int }
trait B5 extends java.util.function.Function[Int, Int] with IntFun
trait B6 extends IntFun with java.util.function.Function[Int, Int]

// both parents have the same erasure: LambdaMetafactory will do
trait H[A] { def ap(a: A): A }
trait B7 extends G[String] with H[String]

object Test {
  def t(name: String)(body: => Any): Unit =
    try println(name + ": " + body)
    catch { case e: Throwable => println(name + ": " + e.getClass.getName + (if (e.getCause != null) " caused by " + e.getCause.getClass.getName else "")) }

  def main(args: Array[String]): Unit = {
    t("B1 through G")  { val z: B1 = _ + 1; (z: G[Int]).ap(1) }
    t("B1 through I1") { val z: B1 = _ + 1; (z: I1).ap(1) }
    t("B2 through G")  { val z: B2 = _ + 1; (z: G[Int]).ap(1) }
    t("B2 through I1") { val z: B2 = _ + 1; (z: I1).ap(1) }
    t("B3 through G")  { val z: B3 = v => VS(v.s + "!"); (z: G[VS]).ap(VS("a")) }
    t("B3 through S1") { val z: B3 = v => VS(v.s + "!"); (z: S1).ap(VS("a")) }
    t("B4 through G2") { val z: B4[String] = _.length; (z: G2[String, Int]).ap("a") }
    t("B4 through H2") { val z: B4[String] = _.length; (z: H2[String]).ap("a") }
    t("B5 through Function") { val z: B5 = _ + 1; (z: java.util.function.Function[Int, Int]).apply(1) }
    t("B5 through IntFun")   { val z: B5 = _ + 1; (z: IntFun).apply(1) }
    t("B6 through Function") { val z: B6 = _ + 1; (z: java.util.function.Function[Int, Int]).apply(1) }
    t("B6 through IntFun")   { val z: B6 = _ + 1; (z: IntFun).apply(1) }
    t("B7 through G")  { val z: B7 = _ + "!"; (z: G[String]).ap("a") }
    t("B7 through H")  { val z: B7 = _ + "!"; (z: H[String]).ap("a") }
    val b7: B7 = _ + "!"
    println("B7 is a lambda: " + b7.getClass.getName.contains("$$Lambda"))
  }
}
