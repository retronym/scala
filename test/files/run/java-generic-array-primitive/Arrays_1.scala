import scala.reflect.ClassTag
import scala.util.Try

// `Array[Int with AnyRef]` erases to `Object[]`, so everything that creates such an array from its element
// type must create an `Object[]`: `Array(...)` and `new Array`, `ClassTag` and `Manifest`, varargs.
class ArrayUser {
  def f(a: Array[Int with AnyRef]): Int = a.length
  def g[T](a: Array[T with AnyRef]): Array[T with AnyRef] = a
  def v(xs: (Int with AnyRef)*): Int = xs.length
}

object Arrays {
  def one: Int with AnyRef = Y.arr(1)(0)
  def tag[T: ClassTag] = implicitly[ClassTag[T]]
  def man[T: Manifest] = implicitly[Manifest[T]]
  def mkArray[T: ClassTag](t: T): Array[T] = Array(t, t)
  def show(x: Any): String = x match {
    case a: Array[_] => a.getClass.getSimpleName + a.map(show).mkString("(", ",", ")")
    case x           => String.valueOf(x)
  }
  def run(t: Try[Any]): String = t.map(show).recover { case e => e.getClass.getSimpleName }.get

  def results: List[String] = {
    val u = new ArrayUser
    val xs = Y.arr(1, 2)
    List(
      run(Try(u.f(Array[Int with AnyRef]()))),
      run(Try(u.f(new Array[Int with AnyRef](3)))),
      run(Try(u.f(Array(one, one)))),
      run(Try(u.g[Int](Array[Int with AnyRef]()))),
      run(Try(u.v(one, one))),
      run(Try(u.v(xs.toIndexedSeq: _*))),
      run(Try(xs ++ Y.arr(3))),
      run(Try(xs :+ one)),
      run(Try(xs.clone())),
      run(Try(xs.reverse)),
      run(Try(xs.filter(_ > 1))),
      run(Try(xs.map(_ + 1))),
      run(Try(Array.fill(2)(xs(0)))),
      run(Try(Array.ofDim[Int with AnyRef](2))),
      run(Try(Array.ofDim[Int with AnyRef](1, 1))),
      run(Try(mkArray(one))),
      run(Try(mkArray(xs))),
      run(Try(Y.arr(xs: _*))),
      run(Try(Y.arr(Y.arr(1), Y.arr(2)))),
      run(Try(tag[Int with AnyRef].newArray(1))),
      run(Try(tag[Array[Int with AnyRef]].newArray(1))),
      run(Try(man[Int with AnyRef].newArray(1))),
      run(Try((xs: AnyRef) match { case a: Array[Int with AnyRef] => a.length case _ => -1 })),
      run(Try((Array(1): AnyRef) match { case a: Array[Int with AnyRef] => a.length case _ => -1 })),
      classOf[ArrayUser].getMethods.filter(_.getName == "f").map(_.toGenericString).mkString,
    )
  }
}
