trait Thing { type A; var p: A = _ }
class A[T](final val x: Thing { type A = T }) {
  type Q = T
 
  def x1: T   = x.p
  def x2: Q   = x.p
  def x3: x.A = x.p
}
class B extends A[Int](null) {
  // def y1 = x1
  // def y2 = x2
  val y3 = x3
}


// object Test extends App {
//   val methods = classOf[B].getDeclaredMethods.sortBy(_.getName)
//   println(methods.map(_.toGenericString).mkString("\n"))
// }
