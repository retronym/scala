object Test {
  // anonymous class directly in a generic method: fine
  def direct[B](b: B): AnyRef = new java.util.ArrayList[B]() { override def toString = s"$b" }
  // same anonymous class, but inside a local def: B is not resolvable
  def viaLocalDef[B](b: B): AnyRef = {
    def inner(n: Int): java.util.ArrayList[B] = new java.util.ArrayList[B]() { override def toString = s"$n $b" }
    inner(1)
  }
  // the type parameter belongs to the local def itself
  def viaGenericLocalDef(): AnyRef = {
    def loc[T](t: T): java.util.ArrayList[T] = new java.util.ArrayList[T]() { override def toString = s"$t" }
    loc(1)
  }
  // a local def between the method and a class, and a method of the class around the anonymous class
  def viaLocalClass[B](b: B): AnyRef = {
    class L { def m(n: Int): AnyRef = new java.util.ArrayList[B]() { override def toString = s"$n $b" } }
    def inner(n: Int): AnyRef = new L().m(n)
    inner(1)
  }
  def check(o: AnyRef): Unit = {
    val c = o.getClass
    println(c.getEnclosingMethod)
    println(try c.getGenericSuperclass catch { case e: Throwable => e })
    // the members can refer to the type parameter too
    try {
      c.getDeclaredFields.foreach(_.getGenericType)
      c.getDeclaredConstructors.foreach(_.getGenericParameterTypes)
      c.getDeclaredMethods.foreach { m => m.getGenericReturnType; m.getGenericParameterTypes }
    } catch { case e: Throwable => println(e) }
  }
  def main(args: Array[String]): Unit = { check(direct("x")); check(viaLocalDef("x")); check(viaGenericLocalDef()); check(viaLocalClass("x")) }
}
