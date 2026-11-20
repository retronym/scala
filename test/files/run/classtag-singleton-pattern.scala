// `case a: A` with a ClassTag[A] in scope is checked by `ClassTag.unapply`, which tests only the
// runtime class. For a literal or singleton type the ClassTag is that of the underlying class, so
// the match succeeds for any value of that class and binds `a: A` to a value that is not of type A.
// SIP-23 says a literal type pattern compares with `==`. Pinned as on 2.13.x (Scala 3 behaves
// the same, scala/scala3#7554).
//
// TODO scala/scala review X3: if the pattern is made sound (or rejected) for singleton type
// arguments, the first three lines print `None`; with an unchecked warning instead, the warning
// appears in the .check and the output stays.
import scala.reflect.ClassTag

object Test {
  def viaTag[A: ClassTag](x: Any): Option[A] = x match { case a: A => Some(a); case _ => None }

  object O
  def main(args: Array[String]): Unit = {
    println(viaTag["lit"]("other"))
    println(viaTag[1](2))
    val s = "s"
    println(viaTag[s.type]("t"))
    println(viaTag[O.type](Test))
  }
}
