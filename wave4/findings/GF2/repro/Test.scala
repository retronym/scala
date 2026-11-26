@Ann(z = { println("side effect"); false }, i = { val u = 1 / 0; 3 }) class A
object Test {
  def main(args: Array[String]): Unit = {
    val a = classOf[A].getAnnotation(classOf[Ann])
    println(s"${a.z} ${a.i}")
  }
}
