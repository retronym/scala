//> using options -Wconf:cat=deprecation:s

// Each method defines a local trait with fields and is called at a type argument that selects the generic
// method and at one that selects a specialized variant. The two lines printed for a method must agree, other
// than in the argument itself.
object Test {
  def members[@specialized(Int) T](t: T): String = {
    var count = 0
    val captured = "c"
    trait TT {
      count += 1
      val v: Int = 8
      var r: Int = 9
      lazy val l: Int = { count += 10; 10 }
      private val p: String = "p"
      private[this] val q: String = "q"
      val param: T = t
      val cap: String = captured
      val u: Unit = count += 100
      final val k = 11
      def show = s"v=$v r=$r l=$l,$l p=$p q=$q param=$param cap=$cap k=$k count=$count"
    }
    (new TT {}).show
  }

  def localClass[@specialized(Int) T](t: T): String = {
    trait TT { val v: Int = 8; val param: T = t; private val p: String = "p"; def show = s"v=$v param=$param p=$p" }
    class C extends TT { val c: T = param }
    val c = new C
    s"${c.show} c=${c.c}"
  }

  def overridden[@specialized(Int) T](t: T): String = {
    trait A { val a: Int = 1; val o: String = "A" }
    trait B extends A { val b: Int = a + 1; override val o: String = "B" }
    val x = new B { override val a: Int = 5 }
    s"a=${x.a} b=${x.b} o=${x.o}"
  }

  def selfType[@specialized(Int) T](t: T): String = {
    trait S { def s: T = t }
    trait TT { self: S => val v: T = s; val w: String = "w" + s }
    val x = new S with TT
    s"v=${x.v} w=${x.w}"
  }

  def sameMember[@specialized(Int) T](t: T): String = {
    trait A { val v: Int = 1; private val p: String = "a"; def pa = p }
    trait B { val w: Int = 2; private val p: String = "b"; def pb = p }
    trait D { val v: Int = 3 }
    class C extends A with B
    val c = new C
    s"A.v=${c.v} B.w=${c.w} A.p=${c.pa} B.p=${c.pb} D.v=${(new D {}).v}"
  }

  def sameName[@specialized(Int) T](t: T): String = {
    val first = { trait TT { val v: String = "first " + t }; (new TT {}).v }
    val second = { trait TT { val v: String = "second " + t }; (new TT {}).v }
    s"$first, $second"
  }

  def nested[@specialized(Int) T](t: T): String = {
    trait Outer {
      val o: T = t
      trait Member { val m: String = "m" + o }
      def inner = {
        trait Inner { val i: String = "i" + o }
        (new Inner {}).i
      }
    }
    val outer = new Outer {}
    s"o=${outer.o} m=${(new outer.Member {}).m} i=${outer.inner}"
  }

  def early[@specialized(Int) T](t: T): String = {
    trait TT { val e: T; val v: String = "v" + e }
    class C extends { val e: T = t } with TT
    val c = new C
    s"e=${c.e} v=${c.v}"
  }

  def byName[@specialized(Int) T](t: T, n: => T): String = {
    trait TT { val v: T = n; lazy val l: T = n; val w: T = t }
    val x = new TT {}
    s"v=${x.v} l=${x.l} w=${x.w}"
  }

  def inFunction[@specialized(Int) T](t: T): String = {
    val f = () => { trait TT { val v: T = t }; (new TT {}).v }
    s"v=${f()}"
  }

  def two[@specialized(Int, Long) A, @specialized(Int, Double) B](a: A, b: B): String = {
    trait TT { val x: A = a; val y: B = b; val v: Int = 8 }
    val tt = new TT {}
    s"x=${tt.x} y=${tt.y} v=${tt.v}"
  }

  class AnyRefClass[@specialized(Int, AnyRef) A, @specialized(Int, AnyRef) B](val a: A, val b: B) {
    def method(x: A): String = {
      trait TT { val v: Int = 8; val p: A = x; val q: B = b }
      val tt = new TT {}
      s"v=${tt.v} p=${tt.p} q=${tt.q}"
    }
  }

  class InClass[@specialized(Int) T](val t: T) {
    def method(x: T): String = {
      trait TT { val v: T = x; val w: T = t; private val p: String = "p"; def show = s"v=$v w=$w p=$p" }
      (new TT {}).show
    }
    def specializedMethod[@specialized(Int) U](u: U): String = {
      trait TT { val v: T = t; val w: U = u }
      val tt = new TT {}
      s"v=${tt.v} w=${tt.w}"
    }
    val field: String = {
      trait TT { val v: T = t }
      "v=" + (new TT {}).v
    }
  }

  def main(args: Array[String]): Unit = {
    def show(label: String, generic: String, specialized: String): Unit = {
      println(s"$label")
      println(s"  generic:     $generic")
      println(s"  specialized: $specialized")
    }
    show("members", members("s"), members(1))
    show("local class", localClass("s"), localClass(1))
    show("overridden", overridden("s"), overridden(1))
    show("self type", selfType("s"), selfType(1))
    show("same member", sameMember("s"), sameMember(1))
    show("same name", sameName("s"), sameName(1))
    show("nested", nested("s"), nested(1))
    show("early", early("s"), early(1))
    show("by-name", byName("s", "n"), byName(1, 2))
    show("in function", inFunction("s"), inFunction(1))
    show("two parameters", two("s", "t"), two(1, 2.0))
    show("two parameters, long and int", two("s", "t"), two(1L, 2))
    show("class, AnyRef", new AnyRefClass(1.0, "t").method(2.0), new AnyRefClass(1, "t").method(2))
    show("class, method", new InClass("s").method("x"), new InClass(1).method(2))
    show("class, specialized method", new InClass("s").specializedMethod("u"), new InClass(1).specializedMethod(2))
    show("class, field", new InClass("s").field, new InClass(1).field)
  }
}
