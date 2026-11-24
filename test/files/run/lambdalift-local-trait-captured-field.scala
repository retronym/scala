// A local trait and a class that mixes it in both capture the same local.
// LambdaLift used to give the class's proxy field the same name as the field
// Mixin adds for the trait's captured value, so the class had a duplicate field.

import scala.language.reflectiveCalls

object Test {
  def capturedVal(k: Int) = {
    trait T { def m(x: Int): Int; def kk = k }
    val a: T = new T { def m(x: Int) = x + k }
    a.m(1) + a.kk
  }

  def capturedVar(k0: Int) = {
    var k = k0
    var s = "s"
    trait T { def m(x: Int): Int; def bump() = { k += 1; s += "!" } ; def kk = k }
    val a: T = new T { def m(x: Int) = { k += x; s += "?"; k } }
    a.bump()
    val r = a.m(10)
    s"$r ${a.kk} $k $s"
  }

  def capturedLazyByNameAndDef(k: Int, n: => Int) = {
    lazy val l = k * 2
    def d = k + l
    trait T { def m(x: Int): Int; def t = l + d + n }
    val a: T = new T { def m(x: Int) = x + l + d + n }
    s"${a.m(1)} ${a.t}"
  }

  def twoCaptured(k: Int, j: String) = {
    trait T { def m(x: Int): String; def t = s"$k$j" }
    val a: T = new T { def m(x: Int) = s"${x + k}$j" }
    s"${a.m(1)} ${a.t}"
  }

  def nestedTraits(k: Int) = {
    trait T1 { def t1 = k }
    trait T2 extends T1 { def t2 = k + 1 }
    trait T3 { def t3 = k + 2 }
    val a = new T2 with T3 { def m = k + t1 + t2 + t3 }
    a.m
  }

  def namedClass(k: Int) = {
    trait T { def kk = k }
    class C(x: Int) extends T { def m = x + k + kk }
    class D extends C(1) with T { def n = k }
    s"${new C(1).m} ${new D().m} ${new D().n}"
  }

  def abstractClassWithTrait(k: Int) = {
    trait T { def kk = k }
    abstract class A extends T { def ak = k }
    val a = new A { def m = k + ak + kk }
    a.m
  }

  def samLiteral(k: Int) = {
    trait T { def m(x: Int): Int; def kk = k }
    val a: T = (x: Int) => x + k
    a.m(1) + a.kk
  }

  def extendedTwice(k: Int) = {
    trait T { def kk = k }
    val a = new T { def m = k + kk }
    val b = new T { def m = k * kk }
    a.m + b.m
  }

  def generic[A](k: A, f: A => String) = {
    trait T { def kk: A = k }
    val a = new T { def m = f(k) + f(kk) }
    a.m
  }

  def localObject(k: Int) = {
    trait T { def kk = k }
    object O extends T { def m = k + kk }
    O.m
  }

  // scala/bug#5252
  def caseClass(k0: Int) = {
    var k = k0
    trait T { def bump() = k += 1 }
    case class C(parent: T) extends T { def m = { bump(); k } }
    C(null).copy().m
  }

  def onlyInTrait(k: Int) = {
    trait T { def kk = k }
    val a = new T { def m = 1 }
    a.m + a.kk
  }

  def onlyInClass(k: Int) = {
    trait T { def m: Int }
    val a = new T { def m = k }
    a.m
  }

  def serialized(k: Int) = {
    trait T extends Serializable { def kk = k }
    val a = new T { def m = k + kk }
    val bos = new java.io.ByteArrayOutputStream
    val oos = new java.io.ObjectOutputStream(bos)
    oos.writeObject(a)
    oos.close()
    val ois = new java.io.ObjectInputStream(new java.io.ByteArrayInputStream(bos.toByteArray))
    val b = ois.readObject().asInstanceOf[T { def m: Int }]
    b.m
  }

  def main(args: Array[String]): Unit = {
    println(capturedVal(3))
    println(capturedVar(3))
    println(capturedLazyByNameAndDef(3, 5))
    println(twoCaptured(3, "j"))
    println(nestedTraits(3))
    println(namedClass(3))
    println(abstractClassWithTrait(3))
    println(samLiteral(3))
    println(extendedTwice(3))
    println(generic[String]("k", _.toUpperCase))
    println(localObject(3))
    println(caseClass(3))
    println(onlyInTrait(3))
    println(onlyInClass(3))
    println(serialized(3))
  }
}
