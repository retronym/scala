import scala.reflect.{ArrayTag, arrayTag}

object Test extends App {
  println(implicitly[ArrayTag[Byte]].newArray(10).getClass)
  println(implicitly[ArrayTag[Byte]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Array[Byte]]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Short]].newArray(10).getClass)
  println(implicitly[ArrayTag[Short]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Array[Short]]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Char]].newArray(10).getClass)
  println(implicitly[ArrayTag[Char]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Array[Char]]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Int]].newArray(10).getClass)
  println(implicitly[ArrayTag[Int]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Array[Int]]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Long]].newArray(10).getClass)
  println(implicitly[ArrayTag[Long]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Array[Long]]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Float]].newArray(10).getClass)
  println(implicitly[ArrayTag[Float]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Array[Float]]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Double]].newArray(10).getClass)
  println(implicitly[ArrayTag[Double]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Array[Double]]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Boolean]].newArray(10).getClass)
  println(implicitly[ArrayTag[Boolean]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Array[Boolean]]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Unit]].newArray(10).getClass)
  println(implicitly[ArrayTag[Unit]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Array[Unit]]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Any]].newArray(10).getClass)
  println(implicitly[ArrayTag[Any]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Array[Any]]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Object]].newArray(10).getClass)
  println(implicitly[ArrayTag[Object]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Array[Object]]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[AnyVal]].newArray(10).getClass)
  println(implicitly[ArrayTag[AnyVal]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Array[AnyVal]]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[AnyRef]].newArray(10).getClass)
  println(implicitly[ArrayTag[AnyRef]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Array[AnyRef]]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Null]].newArray(10).getClass)
  println(implicitly[ArrayTag[Null]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Array[Null]]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Nothing]].newArray(10).getClass)
  println(implicitly[ArrayTag[Nothing]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Array[Nothing]]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[String]].newArray(10).getClass)
  println(implicitly[ArrayTag[String]].wrap.newArray(10).getClass)
  println(implicitly[ArrayTag[Array[String]]].wrap.newArray(10).getClass)
}