object Test extends App {
  class C
  implicit val ctag = scala.reflect.ClassTag[C](classOf[C])
  val x = Array()
  println(x.getClass)
}
