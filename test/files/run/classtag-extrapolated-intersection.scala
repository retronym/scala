// An inferred `(T with U) with C`, from `_$1 with C` where `_$1 <: T with U`, used to reach
// `intersectionDominator` nested when materializing a ClassTag. A refinement class counts as
// a class, so it beat `C`: the tag said `T`, while `uncurry` flattens the type before erasure
// and the array erases to `C[]`.
import scala.reflect.ClassTag

trait T
trait U
trait V extends T
class C
class D extends C with T with U

class Box[A] {
  def withClass(c: C): A with C = c.asInstanceOf[A with C]
  def withSub(v: V): A with V = v.asInstanceOf[A with V]
}

object Test {
  def tag[X: ClassTag](x: X): String = implicitly[ClassTag[X]].runtimeClass.getName

  def classParent(b: Box[_ <: T with U], c: C) = b.withClass(c)
  def shadowed(b: Box[_ <: T with U], v: V) = b.withSub(v)

  def main(args: Array[String]): Unit = {
    val b: Box[_ <: T with U] = new Box[T with U]
    val d = new D
    val v = new V with U

    println(s"erased ${getClass.getMethod("classParent", classOf[Box[_]], classOf[C]).getReturnType.getName}")
    println(s"tag ${tag(classParent(b, d))}")
    println(s"erased ${getClass.getMethod("shadowed", classOf[Box[_]], classOf[V]).getReturnType.getName}")
    println(s"tag ${tag(shadowed(b, v))}")
    println(s"written ${tag[T with U with C](d)}")

    // Was a ClassCastException: [LT; cannot be cast to [LC;
    val array = List(classParent(b, d)).toArray
    println(s"toArray ${array.getClass.getName}")
  }
}
