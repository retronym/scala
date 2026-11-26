// A conditional whose branches construct the same value class, one from a constant,
// used as the receiver of a member that erasure keeps as a member selection.

trait Shows extends Any { def show: String = s"<$this>" }

case class V0(u: Int) extends AnyVal
class Meters(val value: Double) extends AnyVal with Shows {
  def plus(o: Meters): Meters = new Meters(value + o.value)
  override def toString = s"Meters($value)"
}
class Bo(val x: Boolean) extends AnyVal
class By(val x: Byte) extends AnyVal
class Sh(val x: Short) extends AnyVal
class Ch(val x: Char) extends AnyVal
class Lo(val x: Long) extends AnyVal
class Fl(val x: Float) extends AnyVal
class St(val x: String) extends AnyVal with Shows { override def toString = s"St($x)" }
class G[T](val t: T) extends AnyVal
class R(val x: AnyRef) extends AnyVal
class Lit(val x: 7) extends AnyVal

object Test {
  final val Seven = 7
  final val Name = "seven"

  def ifValue(ok: Boolean, d: Double): Double = (if (ok) new Meters(d) else new Meters(0)).value
  def ifEq(ok: Boolean, d: Double, o: Meters): Boolean = (if (ok) new Meters(d) else new Meters(0)) == o
  def ifNe(ok: Boolean, d: Double, o: Meters): Boolean = (if (ok) new Meters(d) else new Meters(0)) != o
  def ifMethod(ok: Boolean, d: Double): Meters = (if (ok) new Meters(d) else new Meters(1)).plus(new Meters(10))
  def ifUniversal(ok: Boolean, d: Double): String = (if (ok) new Meters(d) else new Meters(2)).show

  def caseEq(a: Int, b: Int): Boolean = (if (a > 0) V0(b) else V0(7)) == V0(5)
  def caseField(a: Int, b: Int): Int = (if (a > 0) V0(b) else V0(7)).u
  def caseFinalVal(a: Int, b: Int): Int = (if (a > 0) V0(b) else V0(Seven)).u
  def bothConst(a: Int): Int = (if (a > 0) V0(3) else V0(7)).u
  def bothConstEq(a: Int): Boolean = (if (a > 0) V0(3) else V0(7)) == V0(3)
  def constFirst(a: Int, b: Int): Int = (if (a > 0) V0(7) else V0(b)).u

  def matchField(a: Int, b: Int): Int = (a match { case 0 => V0(7) case 1 => V0(b) case _ => V0(Seven) }).u
  def matchNe(a: Int, b: Int): Boolean = (a match { case 0 => V0(b) case _ => V0(7) }) != V0(7)
  def blockIf(a: Int, b: Int): Int = { val c = a + 1; if (c > 1) V0(b) else V0(7) }.u
  def nested(a: Int, b: Int): Int = (if (a > 0) (if (a > 1) V0(b) else V0(1)) else (if (a < -1) V0(2) else V0(b + 1))).u
  def nestedMatch(a: Int, b: Int): Boolean = (if (a > 0) a match { case 1 => V0(1) case _ => V0(b) } else V0(9)) == V0(9)

  def bo(ok: Boolean, x: Boolean): Boolean = (if (ok) new Bo(x) else new Bo(true)).x
  def by(ok: Boolean, x: Byte): Byte = (if (ok) new By(x) else new By(3)).x
  def sh(ok: Boolean, x: Short): Short = (if (ok) new Sh(x) else new Sh(4)).x
  def ch(ok: Boolean, x: Char): Char = (if (ok) new Ch(x) else new Ch('z')).x
  def lo(ok: Boolean, x: Long): Long = (if (ok) new Lo(x) else new Lo(5L)).x
  def fl(ok: Boolean, x: Float): Float = (if (ok) new Fl(x) else new Fl(6f)).x
  def st(ok: Boolean, x: String): String = (if (ok) new St(x) else new St("")).x
  def stEq(ok: Boolean, x: String): Boolean = (if (ok) new St(x) else new St(Name)) == new St("seven")
  def stNull(ok: Boolean, x: String): String = (if (ok) new St(x) else new St(null)).x
  def stUniversal(ok: Boolean, x: String): String = (if (ok) new St(x) else new St("lit")).show
  def gen(ok: Boolean, s: String): String = (if (ok) new G(s) else new G("lit")).t
  def genInt(ok: Boolean, i: Int): Int = (if (ok) new G(i) else new G(9)).t
  def genNull(ok: Boolean, s: String): String = (if (ok) new G(s) else new G[String](null)).t

  // not constants, but the argument's type is narrower than the class's underlying type
  def anyRef(ok: Boolean, s: String, o: AnyRef): AnyRef = (if (ok) new R(s) else new R(o)).x
  def genAnyRef(ok: Boolean, g: G[AnyRef]): AnyRef = (if (ok) g else new G[AnyRef]("s")).t
  def stNullOnly(ok: Boolean, s: St): String = (if (ok) s else new St(null)).x
  def lit(ok: Boolean, l: Lit): Int = (if (ok) l else new Lit(7)).x

  def main(args: Array[String]): Unit = {
    println(List(ifValue(true, 1.5), ifValue(false, 1.5)))
    println(List(ifEq(true, 1.5, new Meters(1.5)), ifEq(false, 1.5, new Meters(0)), ifEq(false, 1.5, new Meters(1.5))))
    println(List(ifNe(true, 1.5, new Meters(1.5)), ifNe(false, 1.5, new Meters(1.5))))
    println(List(ifMethod(true, 1.5), ifMethod(false, 1.5)))
    println(List(ifUniversal(true, 1.5), ifUniversal(false, 1.5)))
    println(List(caseEq(1, 5), caseEq(0, 5), caseField(1, 4), caseField(0, 4), caseFinalVal(0, 4)))
    println(List(bothConst(1), bothConst(0), bothConstEq(1), bothConstEq(0), constFirst(1, 4), constFirst(0, 4)))
    println(List(matchField(0, 4), matchField(1, 4), matchField(2, 4), matchNe(0, 4), matchNe(1, 4)))
    println(List(blockIf(1, 4), blockIf(0, 4)))
    println(List(nested(2, 4), nested(1, 4), nested(-2, 4), nested(0, 4), nestedMatch(1, 4), nestedMatch(2, 9), nestedMatch(0, 4)))
    println(List[Any](bo(true, false), bo(false, false), by(true, 1), by(false, 1), sh(true, 1), sh(false, 1)))
    println(List[Any](ch(true, 'a'), ch(false, 'a'), lo(true, 1L), lo(false, 1L), fl(true, 1f), fl(false, 1f)))
    println(List(st(true, "x"), st(false, "x"), stEq(true, "seven"), stEq(false, "x"), stNull(true, "x"), stNull(false, "x"), stUniversal(true, "x"), stUniversal(false, "x")))
    println(List(gen(true, "x"), gen(false, "x"), genInt(true, 1), genInt(false, 1), genNull(true, "x"), genNull(false, "x")))
    println(List(anyRef(true, "a", "b"), anyRef(false, "a", "b"), genAnyRef(true, new G("g")), genAnyRef(false, new G("g")), stNullOnly(true, new St("a")), stNullOnly(false, new St("a")), lit(true, new Lit(7)), lit(false, new Lit(7))))
  }
}
