import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def code = """
import reflect.ClassTag
object InstanceOf { def unapply[T](t: T)(implicit T: ClassTag[T]) = if (T.runtimeClass.isInstance(t)) Some(t) else None }
("hey!": Any) match { case InstanceOf[String](s) => s; case _ =>  "!" }
(false: Any)  match { case InstanceOf[String](s) => s; case _ =>  "!" }
case class Foo[M[_]](a: M[Int])
type IntAnd[x] = (Int, x)
(new Foo[({type l[a]=(Int, a)})#l](0, 0))  match { case Foo[IntAnd](x) => x }
"""
}