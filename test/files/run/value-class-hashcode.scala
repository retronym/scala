// The synthetic `hashCode` of a value class must agree with its synthetic `equals`, which compares
// the underlying values with `==`: numeric equality for primitives, cooperative equality for `Any`.

class VD(val d: Double) extends AnyVal
class VF(val f: Float) extends AnyVal
class VL(val l: Long) extends AnyVal
class VI(val i: Int) extends AnyVal
class VC(val c: Char) extends AnyVal
class VZ(val z: Boolean) extends AnyVal
class VU(val u: Unit) extends AnyVal
class VA(val a: Any) extends AnyVal
class VG[T](val t: T) extends AnyVal
class VS(val s: String) extends AnyVal
class VJ(val d: java.lang.Double) extends AnyVal
case class CD(d: Double) extends AnyVal

object Test {
  private var failed = false

  def consistent(a: Any, b: Any): Unit = {
    def fail(what: String) = { failed = true; println(s"$a == $b, but $what differs: ${a.hashCode} != ${b.hashCode}") }
    if (a != b) { failed = true; println(s"$a != $b") }
    else if (a.hashCode != b.hashCode) fail("hashCode")
    else if (a.## != b.##) fail("##")
    else if (!Set(a)(b) || !collection.mutable.HashSet(a)(b)) fail("set membership")
  }

  def same(what: String, actual: Int, expected: Int): Unit =
    if (actual != expected) { failed = true; println(s"$what: $actual != $expected") }

  def main(args: Array[String]): Unit = {
    consistent(new VD(0.0), new VD(-0.0))
    consistent(new VF(0.0f), new VF(-0.0f))
    consistent(CD(0.0), CD(-0.0))
    consistent(new VA(1), new VA(1L))
    consistent(new VA(1), new VA(1.0))
    consistent(new VA(1), new VA(1.0f))
    consistent(new VA('a'), new VA(97))
    consistent(new VA(0.0), new VA(-0.0))
    consistent(new VA(1L << 40), new VA((1L << 40).toDouble))
    consistent(new VG[Any](1), new VG[Any](1.0))
    consistent(new VG(1L), new VG(1L))
    consistent(new VL(-1L), new VL(-1L))
    consistent(new VA(new VD(0.0)), new VA(new VD(-0.0)))

    // the hash is that of the underlying value, as computed by `##`, which is consistent with `==`.
    same("VD", new VD(1.0).hashCode, 1.0.##)
    same("VD.##", new VD(1.0).##, 1.0.##)
    same("VD", new VD(1.5).hashCode, 1.5.##)
    same("VD", new VD(Double.NaN).hashCode, Double.NaN.##)
    same("VF", new VF(1.0f).hashCode, 1.0f.##)
    same("VL", new VL(-1L).hashCode, -1L.##)
    same("VL", new VL(1L << 40).hashCode, (1L << 40).##)
    same("VJ", new VJ(1.0).hashCode, 1.0.##)
    same("VJ", new VJ(null).hashCode, 0)
    same("VA", new VA(1.0).hashCode, 1)
    same("VA", new VA(null).hashCode, 0)
    same("CD", CD(1.0).hashCode, 1)

    // unchanged
    same("VI", new VI(-42).hashCode, -42)
    same("VC", new VC('a').hashCode, 97)
    same("VZ", new VZ(true).hashCode, true.hashCode)
    same("VZ", new VZ(false).hashCode, false.hashCode)
    same("VU", new VU(()).hashCode, ().hashCode)
    same("VS", new VS("abc").hashCode, "abc".hashCode)
    same("VS", new VS(null).hashCode, 0)
    same("VA", new VA("abc").hashCode, "abc".hashCode)

    assert(!failed)
  }
}
