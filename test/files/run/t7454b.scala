import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.universe.{TypeTag, typeTag}

class C

object Test extends App {
  implicit val cctag: ClassTag[C] = ClassTag[C](classOf[C])
  def foo[T: ClassTag](xs: T*) = println(classTag[T])
  foo(1, 2)
  foo()
  foo[Nothing]()
  def g1 = foo(???)

  implicit val cttag: TypeTag[C] = { val cttag = "shadow cttag"; typeTag[C] }
  def bar[T: TypeTag](xs: T*) = println(typeTag[T])
  bar(1, 2)
  bar()
  bar[Nothing]()
  def g2 = bar(???)
}
