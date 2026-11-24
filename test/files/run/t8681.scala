// `SimpleClass.myMethod(String)` and `SimpleTrait.myMethod(T)` are overloads in `SimpleClass[T]`
// and an overriding pair in `BadClass`, which needs the bridge `myMethod(Object)`.

trait SimpleTrait[T] {
  def myMethod(t: T): Int
  def doIt(t: T): Int = myMethod(t)
}
abstract class SimpleClass[T] extends SimpleTrait[T] {
  def myMethod(t: String): Int = 5
}
class BadClass extends SimpleClass[String]

// the bridge is inherited from `BadClass`
class Sub extends BadClass
class SubOverride extends BadClass { override def myMethod(t: String): Int = 6 }

// one more level of instantiation
abstract class Mid[U] extends SimpleClass[U]
class Leaf extends Mid[String]

// the two members match through a type member
trait Member { type A; def m(a: A): Int; def call(a: A): Int = m(a) }
abstract class MemberClass extends Member { def m(s: String): Int = 7 }
class MemberLeaf extends MemberClass { type A = String }

object Test {
  def bridges(c: Class[_]) = c.getDeclaredMethods.filter(_.isBridge).map(_.toString).sorted.mkString(", ")

  def main(args: Array[String]): Unit = {
    val b = new BadClass
    println(b.myMethod("x"))
    println((b: SimpleTrait[String]).doIt("x"))
    println((new Sub: SimpleTrait[String]).doIt("x"))
    println((new SubOverride: SimpleTrait[String]).doIt("x"))
    println((new Leaf: SimpleTrait[String]).doIt("x"))
    println(new MemberLeaf().call("x"))
    List(classOf[BadClass], classOf[Sub], classOf[SubOverride], classOf[Leaf], classOf[MemberLeaf]).foreach(c => println(s"${c.getName}: ${bridges(c)}"))
  }
}
