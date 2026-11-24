trait Top { private object c { override def toString = "c" }; def usec: Any = c }
class TopC extends Top

object Test {
  def main(args: Array[String]): Unit = {
    trait T { private object c { override def toString = "c" }; def usec: Any = c }
    class C extends T
    println(new C().usec)

    trait U { private[this] object c { override def toString = "c" }; def usec: Any = c }
    println(new U {}.usec)

    val top = new TopC
    println(top.usec)
    // the names of the accessor and of the field are unchanged for a trait that is not local
    println(classOf[TopC].getDeclaredMethods.map(_.getName).sorted.mkString(" "))
    println(classOf[TopC].getDeclaredFields.map(_.getName).sorted.mkString(" "))
  }
}
