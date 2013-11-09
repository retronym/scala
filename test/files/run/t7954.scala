import scala.annotation._
import scala.collection.JavaConverters._

object Test {
  def main(args:Array[String]) {
    assert((new B).f(5) == 6)
    assert((new D).f(5) == 25)

    def check(c: Class[_]) {
      import java.lang.reflect._
      def isAbstract(m: Method) = Modifier.isAbstract(m.getModifiers)
      def isStrict(m: Method)   = Modifier.isStrict(m.getModifiers)
      val (abstrakt, concrete) = c.getDeclaredMethods.toList.partition(isAbstract)
      abstrakt.foreach(m => assert(!isStrict(m), m.toString)) // actually a ClassFormatError, this just shows intent.
      concrete.foreach(m => assert(isStrict(m), m.toString))
    }

    check(classOf[A])
    check(classOf[B])
    check(Class.forName("C$class"))
    check(O.getClass)
    check(P.getClass)
    check(classOf[O.Inner])
    check(O.lambda.getClass)
  }
}

@strictfp
abstract class A {
  def f(x:Int) = x+1
  def g(x:Int):Int
}
@strictfp class B extends A {
  def g(x:Int) = x*2
}

@strictfp
trait C {
  def f(x:Int) = x*x
}
@strictfp class D extends C {}

@strictfp object O {
  def f(x:Int) = x*x 

  class Inner {
    def inner(x: Int) = x * x
  }
  val lambda = (x: Int) => -x
}
object P {
   @strictfp def f(x:Int) = x*x 
}
