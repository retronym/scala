// A local trait and a class that mixes it in both capture the same local.
// LambdaLift used to give the class's proxy field the same name as the field
// Mixin adds for the trait's captured value, so the class had a duplicate field.
//
// Each line of the check file shows who read what: `T(k=3)` is a read of the captured
// `k` in the body of `T`. A proxy that was never initialized shows up as `0` or `null`.

import scala.language.reflectiveCalls

object Test {
  def capturedVal(k: Int) = {
    trait T { def m(x: Int): String; def t = s"T(k=$k)" }
    val a: T = new T { def m(x: Int) = s"anon(x=$x, k=$k)" }
    s"${a.m(1)} ${a.t}"
  }

  def capturedVar(k0: Int) = {
    var k = k0
    var s = "s"
    trait T { def m(x: Int): String; def bump() = { k += 1; s += "!" }; def t = s"T(k=$k, s=$s)" }
    val a: T = new T { def m(x: Int) = { k += x; s += "?"; s"anon(k=$k, s=$s)" } }
    a.bump()
    s"${a.m(10)} ${a.t} method(k=$k, s=$s)"
  }

  def capturedLazyByNameAndDef(k: Int, n: => Int) = {
    lazy val l = k * 2
    def d = s"d(k=$k, l=$l)"
    trait T { def m(x: Int): String; def t = s"T(l=$l, $d, n=$n)" }
    val a: T = new T { def m(x: Int) = s"anon(x=$x, l=$l, $d, n=$n)" }
    s"${a.m(1)} ${a.t}"
  }

  def twoCaptured(k: Int, j: String) = {
    trait T { def m(x: Int): String; def t = s"T(k=$k, j=$j)" }
    val a: T = new T { def m(x: Int) = s"anon(x=$x, k=$k, j=$j)" }
    s"${a.m(1)} ${a.t}"
  }

  def nestedTraits(k: Int) = {
    trait T1 { def t1 = s"T1(k=$k)" }
    trait T2 extends T1 { def t2 = s"T2(k=$k)" }
    trait T3 { def t3 = s"T3(k=$k)" }
    val a = new T2 with T3 { def m = s"anon(k=$k, $t1, $t2, $t3)" }
    a.m
  }

  def namedClass(k: Int) = {
    trait T { def t = s"T(k=$k)" }
    class C(x: Int) extends T { def m = s"C(x=$x, k=$k, $t)" }
    class D extends C(1) with T { def n = s"D(k=$k, $m)" }
    s"${new C(1).m} ${new D().n}"
  }

  def abstractClassWithTrait(k: Int) = {
    trait T { def t = s"T(k=$k)" }
    abstract class A extends T { def ak = s"A(k=$k)" }
    val a = new A { def m = s"anon(k=$k, $ak, $t)" }
    a.m
  }

  def samLiteral(k: Int) = {
    trait T { def m(x: Int): String; def t = s"T(k=$k)" }
    val a: T = (x: Int) => s"sam(x=$x, k=$k)"
    s"${a.m(1)} ${a.t}"
  }

  def extendedTwice(k: Int) = {
    trait T { def t = s"T(k=$k)" }
    val a = new T { def m = s"anon1(k=$k, $t)" }
    val b = new T { def m = s"anon2(k=$k, $t)" }
    s"${a.m} ${b.m}"
  }

  def generic[A](k: A, f: A => String) = {
    trait T { def kk: A = k; def t = s"T(k=$k)" }
    val a = new T { def m = s"anon(f(k)=${f(k)}, f(kk)=${f(kk)}, $t)" }
    a.m
  }

  // scala/bug#10036
  def localObject(k: Int) = {
    trait T { def t = s"T(k=$k)" }
    object O extends T { def m = s"O(k=$k, $t)" }
    O.m
  }

  // scala/bug#5252
  def caseClass(k0: Int) = {
    var k = k0
    trait T { def bump() = { k += 1; s"T(k=$k)" } }
    case class C(parent: T) extends T { def m = s"C(k=$k, ${bump()}, k=$k)" }
    C(null).copy().m
  }

  def onlyInTrait(k: Int) = {
    trait T { def t = s"T(k=$k)" }
    val a = new T { def m = s"anon($t)" }
    a.m
  }

  def onlyInClass(k: Int) = {
    trait T { def m: String }
    val a = new T { def m = s"anon(k=$k)" }
    a.m
  }

  def serialized(k: Int) = {
    trait T extends Serializable { def t = s"T(k=$k)" }
    val a = new T { def m = s"anon(k=$k, $t)" }
    val bos = new java.io.ByteArrayOutputStream
    val oos = new java.io.ObjectOutputStream(bos)
    oos.writeObject(a)
    oos.close()
    val ois = new java.io.ObjectInputStream(new java.io.ByteArrayInputStream(bos.toByteArray))
    val b = ois.readObject().asInstanceOf[T { def m: String }]
    s"${a.m} -> ${b.m}"
  }

  def main(args: Array[String]): Unit = {
    def show(name: String, result: String) = println(s"$name: $result")
    show("capturedVal", capturedVal(3))
    show("capturedVar", capturedVar(3))
    show("capturedLazyByNameAndDef", capturedLazyByNameAndDef(3, 5))
    show("twoCaptured", twoCaptured(3, "j"))
    show("nestedTraits", nestedTraits(3))
    show("namedClass", namedClass(3))
    show("abstractClassWithTrait", abstractClassWithTrait(3))
    show("samLiteral", samLiteral(3))
    show("extendedTwice", extendedTwice(3))
    show("generic", generic[String]("k", _.toUpperCase))
    show("localObject", localObject(3))
    show("caseClass", caseClass(3))
    show("onlyInTrait", onlyInTrait(3))
    show("onlyInClass", onlyInClass(3))
    show("serialized", serialized(3))
  }
}
