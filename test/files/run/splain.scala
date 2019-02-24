import scala.tools.partest._

object Test
extends DirectTest
{
  override def extraSettings: String = "-usejavacp -Ysplain:enable -Ysplain:no-color"

  def code = """
object ImplicitChain
{
  trait I1
  trait I2
  trait I3
  trait I4
  trait II
  implicit def i1(implicit impPar7: I3): I1 = ???
  implicit def i2a(implicit impPar8: I3): I2 = ???
  implicit def i2b(implicit impPar8: I3): I2 = ???
  implicit def i4(implicit impPar9: I2): I4 = ???
  implicit def g(implicit impPar3: I1, impPar1: I4): II = ???
  implicitly[II]
}
  """.trim

  def foundReq = """
object FoundReq
{
  class L
  type R
  def f(r: R): Int = ???
  f(new L)
}
  """.trim

  def bounds = """
object Bounds
{
  trait Base
  trait Arg
  trait F[A]
  implicit def g[A <: Base, B]: F[A] = ???
  implicitly[F[Arg]]
}
  """.trim


  def show() {
    val global = newCompiler()
    compileString(global)(code)
    compileString(global)(foundReq)
    compileString(global)(bounds)
  }
}
