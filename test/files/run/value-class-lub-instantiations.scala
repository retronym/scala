// Conditionals, matches, trys and blocks whose results are values of one value class with
// different erased underlying types, used as the receiver of a member that erasure keeps as a
// member selection: different type arguments, an injection wider or narrower than its argument,
// and arguments of type Null or Nothing, also outside a conditional.

case class G[T](x: T) extends AnyVal {
  def show: String = s"G($x: ${if (x == null) "null" else x.getClass.getSimpleName})"
}
class Wrap[+A](val a: A) extends AnyVal { def get: A = a }
class R(val x: AnyRef) extends AnyVal { def len: Int = String.valueOf(x).length }
class St(val x: String) extends AnyVal
class Cs(val x: CharSequence) extends AnyVal
class In(val x: Int) extends AnyVal
class Do(val x: Double) extends AnyVal
class Bo(val x: Boolean) extends AnyVal
class Ch(val x: Char) extends AnyVal
class Lo(val x: Long) extends AnyVal
class Meters(val value: Double) extends AnyVal

object Test {
  def boom: Nothing = throw new IllegalStateException("boom")
  def attempt[A](a: => A): Any = try a catch { case e: IllegalStateException => e.getMessage }
  def cls(a: Any): String = if (a == null) "null" else a.getClass.getSimpleName

  // the injection is wider than its argument
  def widerX(c: Boolean, s: String): CharSequence = (if (c) new G(s) else new G[CharSequence](s)).x
  def widerEq(c: Boolean, s: String): Boolean = (if (c) new G(s) else new G[CharSequence](s)) == G("a")
  def covariant(c: Boolean, s: String): AnyRef = (if (c) new Wrap(s) else new Wrap[AnyRef](s)).a
  def covariantGet(c: Boolean, s: String): AnyRef = (if (c) new Wrap(s) else new Wrap[AnyRef](s)).get

  // values of different instantiations
  def vals(c: Boolean, gs: G[String], gc: G[CharSequence]): CharSequence = (if (c) gs else gc).x
  def valsNe(c: Boolean, gs: G[String], gc: G[CharSequence]): Boolean = (if (c) gs else gc) != G("a")
  def valsShow(c: Boolean, gs: G[String], gc: G[CharSequence]): String = (if (c) gs else gc).show
  def intOrString(c: Boolean): Any = (if (c) G(1) else G("s")).x
  def intOrStringCls(c: Boolean): String = cls((if (c) G(1) else G("s")).x)
  def intOrStringEq(c: Boolean): Boolean = (if (c) G(1) else G("s")) == G(1)
  def intOrStringShow(c: Boolean): String = (if (c) G(1) else G("s")).show
  def intOrDouble(c: Boolean): String = cls((if (c) G(1) else G(2.0)).x)
  def intOrVC(c: Boolean): String = cls((if (c) G(1) else G(new Meters(2))).x)
  def intOrArray(c: Boolean): String = cls((if (c) G(1) else G(Array(1))).x)
  def nested(c: Boolean): String = cls((if (c) G(G(1)) else G(G("s"))).x.x)
  def matchInst(i: Int): String = cls((i match { case 0 => G(1) case 1 => G("s") case _ => G('c') }).x)
  def tryInst(c: Boolean): String = cls((try { if (c) G(1) else boom } catch { case _: IllegalStateException => G("s") }).x)
  def blockInst(i: Int): String = cls({ val j = i + 1; if (j > 1) G(1L) else G(true) }.x)

  // null and Nothing arguments
  def nullArg(c: Boolean, s: String): String = (if (c) new G(s) else new G(null)).x
  def nullArgEq(c: Boolean, s: String): Boolean = (if (c) new G(s) else new G(null)) == G[String](null)
  def throwArg(c: Boolean, s: String): String = (if (c) new G(s) else new G(throw new IllegalStateException("t"))).x
  def nothingArgSt(c: Boolean, s: String): Int = (if (c) new St(s) else new St(boom)).x.length
  def nothingArgIn(c: Boolean, i: Int): Int = (if (c) new In(i) else new In(boom)).x
  def stNull(c: Boolean, s: St): String = (if (c) s else new St(null)).x
  def rNull(c: Boolean, s: String): AnyRef = (if (c) new R(s) else new R(null)).x
  def rMixed(c: Boolean, s: String, i: Integer): Int = (if (c) new R(s) else new R(i)).len
  def rMixedX(c: Boolean, s: String, i: Integer): String = cls((if (c) new R(s) else new R(i)).x)
  def csMixed(c: Boolean, s: String, sb: java.lang.StringBuilder): Int = (if (c) new Cs(s) else new Cs(sb)).x.length

  // Nothing and Null arguments outside a conditional
  def stNothing: Int = new St(boom).x.length
  def stNullLength: Int = new St(null).x.length
  def inNothing: Int = new In(boom).x + 1
  def gNothing: Int = new G[String](boom).x.length
  def stReturn(c: Boolean): Int = { if (c) return 3; new St(return 4).x.length }

  // non-generic classes over primitives with literal arguments
  def in(i: Int): Int = (i match { case 0 => new In(1) case 1 => new In(i) case _ => new In(-1) }).x
  def doTry(c: Boolean, d: Double): Double = (try { if (c) new Do(d) else boom } catch { case _: IllegalStateException => new Do(0) }).x
  def bo(c: Boolean, b: Boolean): Boolean = { val n = !c; if (n) new Bo(b) else new Bo(true) }.x
  def ch(c: Boolean): Boolean = (if (c) new Ch('a') else new Ch('b')) == new Ch('a')
  def lo(c: Boolean, l: Long): Long = (if (c) new Lo(l) else new Lo(0L)).x

  def main(args: Array[String]): Unit = {
    println(List(widerX(true, "a"), widerX(false, "a"), widerEq(true, "a"), widerEq(false, "b"), covariant(true, "a"), covariantGet(false, "a")))
    println(List(vals(true, G("a"), G(new java.lang.StringBuilder("b"))), vals(false, G("a"), G(new java.lang.StringBuilder("b"))), valsNe(true, G("a"), G("a")), valsNe(false, G("a"), G("b")), valsShow(false, G("a"), G(new java.lang.StringBuilder("b")))))
    println(List(intOrString(true), intOrString(false), intOrStringCls(true), intOrStringCls(false), intOrStringEq(true), intOrStringEq(false), intOrStringShow(true), intOrStringShow(false)))
    println(List(intOrDouble(true), intOrDouble(false), intOrVC(true), intOrVC(false), intOrArray(true), intOrArray(false), nested(true), nested(false)))
    println(List(matchInst(0), matchInst(1), matchInst(2), tryInst(true), tryInst(false), blockInst(1), blockInst(0)))
    println(List(nullArg(true, "a"), nullArg(false, "a"), nullArgEq(true, "a"), nullArgEq(false, "a"), throwArg(true, "a"), attempt(throwArg(false, "a"))))
    println(List(nothingArgSt(true, "abc"), attempt(nothingArgSt(false, "abc")), nothingArgIn(true, 3), attempt(nothingArgIn(false, 3))))
    println(List(stNull(true, new St("a")), stNull(false, new St("a")), rNull(true, "a"), rNull(false, "a"), rMixed(true, "abc", 12), rMixed(false, "abc", 12), rMixedX(true, "abc", 12), rMixedX(false, "abc", 12), csMixed(true, "ab", new java.lang.StringBuilder("abc")), csMixed(false, "ab", new java.lang.StringBuilder("abc"))))
    println(List(attempt(stNothing), attempt(try stNullLength catch { case _: NullPointerException => "npe" }), attempt(inNothing), attempt(gNothing), stReturn(true), stReturn(false)))
    println(List(in(0), in(1), in(2), doTry(true, 1.5), doTry(false, 1.5), bo(true, false), bo(false, false), ch(true), ch(false), lo(true, 3L), lo(false, 3L)))
  }
}
