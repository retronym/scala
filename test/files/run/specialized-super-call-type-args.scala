// A super call with type arguments from a class that fixes a specialized type parameter of a trait
// must call the trait's implementation, not dispatch back to the overriding method.

trait T0[@specialized(Int) B] {
  def m[X](x: X, b: B): Int = 1
}
class C0 extends T0[Int] {
  override def m[X](x: X, b: Int): Int = super.m[X](x, b) + 1
}
class C0i extends T0[Int] {
  override def m[X](x: X, b: Int): Int = super.m(x, b) + 1
}

trait T1[@specialized(Int) B] {
  def m[@specialized(Int) X](x: X, b: B): Int = 1
}
class C1 extends T1[Int] {
  override def m[@specialized(Int) X](x: X, b: Int): Int = super.m[X](x, b) + 1
}

trait Fold[@specialized(Int) A] {
  def foldAll[Z](z: Z)(f: (Z, A) => Z): Z = z
}
class IntFold extends Fold[Int] {
  override def foldAll[Z](z: Z)(f: (Z, Int) => Z): Z = f(super.foldAll(z)(f), 1)
}

class B0[@specialized(Int) B] {
  def m[X](x: X, b: B): Int = 1
}
class D0 extends B0[Int] {
  override def m[X](x: X, b: Int): Int = super.m[X](x, b) + 1
}

// Overloaded variants: the call must bind to the alternative it was typed against.
trait O2[@specialized(Int) B] {
  def m[X](x: X, b: B): Int = 1
  def m[X](x: X, b: B, c: Int): Int = 2
}
class O2n extends O2[Int] {
  def n[X](x: X): Int = super.m[X](x, 3) * 10 + super.m(x, 3, 4)
}
class O2o extends O2[Int] {
  override def m[X](x: X, b: Int): Int = super.m[X](x, b) + 10
  override def m[X](x: X, b: Int, c: Int): Int = super.m(x, b, c) + 10
}

trait O3[@specialized(Int) B] {
  def m[X](x: X, b: B, c: Int): Int = 1
  def m[X](x: X, b: B, c: String): Int = 2
  def m(b: B): Int = 3
}
class O3n extends O3[Int] {
  def n[X](x: X): Int = super.m[X](x, 3, 4) * 100 + super.m(x, 3, "c") * 10 + super.m(3)
}
class O3o extends O3[Int] {
  override def m[X](x: X, b: Int, c: Int): Int = super.m[X](x, b, c) + 10
  override def m[X](x: X, b: Int, c: String): Int = super.m(x, b, c) + 10
}

class OB2[@specialized(Int) B] {
  def m[X](x: X, b: B): Int = 1
  def m[X](x: X, b: B, c: Int): Int = 2
}
class OB2n extends OB2[Int] {
  def n[X](x: X): Int = super.m[X](x, 3) * 10 + super.m(x, 3, 4)
}
class OB2o extends OB2[Int] {
  override def m[X](x: X, b: Int): Int = super.m[X](x, b) + 10
  override def m[X](x: X, b: Int, c: Int): Int = super.m(x, b, c) + 10
}

class OB3[@specialized(Int) B] {
  def m[X](x: X, b: B, c: Int): Int = 1
  def m[X](x: X, b: B, c: String): Int = 2
  def m(b: B): Int = 3
}
class OB3n extends OB3[Int] {
  def n[X](x: X): Int = super.m[X](x, 3, 4) * 100 + super.m(x, 3, "c") * 10 + super.m(3)
}
class OB3o extends OB3[Int] {
  override def m[X](x: X, b: Int, c: Int): Int = super.m[X](x, b, c) + 10
  override def m[X](x: X, b: Int, c: String): Int = super.m(x, b, c) + 10
}

class K[@specialized(Int) B] {
  def m[X](x: X, b: B, c: Int): Int = 1
  def m[X](x: X, b: B, c: String): Int = 2
}

// A super call from another method reaches the parent's method, not the override.
class S0 extends T0[Int] {
  override def m[X](x: X, b: Int): Int = 10
  def n[X](x: X): Int = super.m[X](x, 3)
}

object Test {
  def t(label: String)(f: => Any): Unit =
    try println(s"$label: $f")
    catch { case _: StackOverflowError => println(s"$label: StackOverflowError") }

  def main(args: Array[String]): Unit = {
    t("type argument")(new C0().m(1, 2))
    t("inferred type argument")(new C0i().m(1, 2))
    t("specialized type parameter, Int")(new C1().m(1, 2))
    t("specialized type parameter, String")(new C1().m("s", 2))
    t("fold")(new IntFold().foldAll(1)(_ + _))
    t("class parent")(new D0().m(1, 2))
    t("overloads by arity, trait parent, from another method")(new O2n().n(1))
    t("overloads by arity, trait parent, overridden")(s"${new O2o().m(1, 2)} ${new O2o().m(1, 2, 3)}")
    t("overloads by type and arity, trait parent, from another method")(new O3n().n(1))
    t("overloads by type and arity, trait parent, overridden")(s"${new O3o().m(1, 2, 3)} ${new O3o().m(1, 2, "c")}")
    t("overloads by arity, class parent, from another method")(new OB2n().n(1))
    t("overloads by arity, class parent, overridden")(s"${new OB2o().m(1, 2)} ${new OB2o().m(1, 2, 3)}")
    t("overloads by type and arity, class parent, from another method")(new OB3n().n(1))
    t("overloads by type and arity, class parent, overridden")(s"${new OB3o().m(1, 2, 3)} ${new OB3o().m(1, 2, "c")}")
    t("super call from another method")(new S0().n(1))
    t("overloads by type, not a super call")({ val k = new K[Int]; k.m("s", 3, 4) * 10 + k.m("s", 3, "c") })
  }
}
