import scala.reflect.ClassTag
trait U0[A] extends Any { def uu: A; def uc(x: Int): (A, Int) = (uu, x) }
//#unit
import scala.reflect.ClassTag
trait Sam0[A, @specialized B] { def ap(a: A): B; def twice(a: A): (B, B) = (ap(a), ap(a)) }
//#unit
import scala.reflect.ClassTag
object Tr { val sb = new java.lang.StringBuilder; def t[T](k: Int, v: T): T = { sb.append(k).append(' '); v }; def take(): String = { val s = sb.toString; sb.setLength(0); s } }
sealed trait Sh
case class K0(a: Int, b: String) extends Sh
case class K1(x: Sh, n: Long) extends Sh
case object K2 extends Sh
object Ev { def unapply(i: Int): Option[Int] = if (i % 2 == 0) Some(i / 2) else None }
object Imp { implicit class RichI(private val self: Int) extends AnyVal { def dbl: Int = self * 2 }; implicit class Pairer[T](private val self: T) { def pairUp: (T, T) = (self, self) }; implicit val defaultK: K0 = K0(7, "imp"); def withImp(x: Int)(implicit k: K0): Int = x + k.a }
//#unit
import scala.reflect.ClassTag; import Imp._
trait T0[AT0, BT0] {
  def zAT0: AT0
  def zBT0: BT0
  var v1: Int = (((4L + 0L): Any) match { case x2: Int => x2; case _ => (6 + 2) })
  lazy val lz3: BT0 = (try { if (false) throw new IllegalStateException("x"); ((List[Int](v1, 2).max, "a") match { case (0, x4) => (((x7: String) => zBT0): Sam0[String, BT0]).ap(x4); case (x5, "a" | "bc") => (v1 match { case Ev(Ev(x8)) => zBT0; case Ev(x8) => zBT0; case x9 => zBT0 }); case (x5, x6) if x5 > x6.length => (if (true) zBT0 else zBT0); case _ => zBT0 }) } catch { case _: IllegalStateException => ({ def loc10(): BT0 = { try { if ((v1 match { case Ev(Ev(x11)) => false; case Ev(x11) => true; case x12 => true })) return zBT0; (zBT0, 7)._1 } finally { Tr.t(13, 0) } }; loc10() }) } finally { Tr.t(14, 0) })
  def m16(p15: Option[Sh]): (Double, Option[Int]) = ({ lazy val x17: Sh = (if ((((x18: Int) => true): Sam0[Int, Boolean]).ap(2)) (K2: Sh) else p15.getOrElse((K2: Sh))); val x19: AT0 = ({ def loc20(p21: Long, p22: BT0 = zBT0): Int = (v1 + 1); val x23: Option[Double] = ({ case x24 if x24 > 1 => (None: Option[Double]) }: PartialFunction[Int, Option[Double]]).applyOrElse(4, ((x24: Int) => (None: Option[Double]))); (None: Option[AT0]).getOrElse(zAT0) }); (try { if (true) throw new IllegalStateException("x"); (0.5, (None: Option[Int])) } catch { case _: IllegalStateException => (2.5, (None: Option[Int])) } finally { Tr.t(25, 0) }) })
  lazy val lz26: List[AT0] = ({ def loc27(): List[AT0] = { try { if (((None: Option[Int]) match { case Some(x28) if ({ lazy val x29: (Sh, AT0) = ((K2: Sh), zAT0); false }) => (Option[Int](5) match { case Some(x30) if true => true; case Some(_) => false; case None => true }); case Some(_) => (List[AT0]() == List[AT0](zAT0)); case None => Array[Boolean](true, false).apply(0) })) return (for (x31 <- List[String](); if (Option[Double](3.0) match { case Some(x32) if true => true; case Some(_) => true; case None => true })) yield zAT0); ({ @scala.annotation.tailrec def loop33(i34: Int, acc35: List[AT0]): List[AT0] = if (i34 <= 0) acc35 else loop33(i34 - 1, (try { if (true) throw new IllegalStateException("x"); acc35 } catch { case _: IllegalStateException => acc35 } finally { Tr.t(36, 0) })); loop33(2, List[AT0](zAT0)) }) } finally { Tr.t(37, 0) } }; loc27() })
}
//#unit
import scala.reflect.ClassTag; import Imp._
class C0(val zAT0: Double, val zBT0: Int) extends T0[Double, Int] {
  var v38: Option[Double] = ((zBT0 match { case Ev(Ev(x44)) => (K2: Sh); case Ev(x44) => (K2: Sh); case x45 => (K2: Sh) }) match { case K0(x39, x40) if x39 > 2 => (zAT0, Option[Double](zAT0))._2; case K1(K0(x41, _), x42) => (for (x46 <- Option[String]("bc"); x47 <- Option[String]("a")) yield zAT0); case K1(_, x43) => Option[Double](3.0).map(((x48: Double) => zAT0)); case K2 | K0(_, _) => ({ class L49(p50: (String, Long)) { def g(p51: Int): Int = p51 }; Option[Double](zAT0) }) })
  def ov52(p53: String, p54: Double): Option[(Double, Int)] = ({ def loc55(): Option[(Double, Int)] = { try { if (Tr.t(56, ((x57: Long) => false)).apply(((K2: Sh) match { case K0(x58, x59) if x58 > 2 => 0L; case K1(K0(x60, _), x61) => x61; case K1(_, x62) => x62; case K2 | K0(_, _) => 8L }))) return (4 match { case Ev(Ev(x63)) => Array[Option[(Double, Int)]]((None: Option[(Double, Int)]), (None: Option[(Double, Int)])); case Ev(x63) => Array[Option[(Double, Int)]]((None: Option[(Double, Int)]), (None: Option[(Double, Int)])); case x64 => Array[Option[(Double, Int)]]((None: Option[(Double, Int)]), (None: Option[(Double, Int)])) }).apply(0); ({ @scala.annotation.tailrec def loop65(i66: Int, acc67: Option[(Double, Int)]): Option[(Double, Int)] = if (i66 <= 0) acc67 else loop65(i66 - 1, Option[(Double, Int)]((p54, i66))); loop65(2, Tr.t(68, (None: Option[(Double, Int)]))) }) } finally { Tr.t(69, 0) } }; loc55() })
  def ov52(p70: Int, p71: String): Option[(Double, Int)] = ((lz3, (((x74: Int) => p70): Sam0[Int, Int]))._2.ap(p71.length) match { case Ev(Ev(x72)) => ((Tr.t(78, x72), (p71 + p71)) match { case (0, x75) => Option[Option[(Double, Int)]](Option[(Double, Int)]((zAT0, p70))).getOrElse(Option[(Double, Int)]((3.0, p70))); case (x76, "a" | "bc") => (if (true) (None: Option[(Double, Int)]) else (None: Option[(Double, Int)])); case (x76, x77) if x76 > x77.length => (None: Option[(Double, Int)]); case _ => (try { if (true) throw new IllegalStateException("x"); (None: Option[(Double, Int)]) } catch { case _: IllegalStateException => (None: Option[(Double, Int)]) } finally { Tr.t(79, 0) }) }); case Ev(x72) => Tr.t(80, ({ @scala.annotation.tailrec def loop81(i82: Int, acc83: Option[(Double, Int)]): Option[(Double, Int)] = if (i82 <= 0) acc83 else loop81(i82 - 1, acc83); loop81(2, Option[(Double, Int)]((zAT0, x72))) })); case x73 => ov52(p71, p70.toDouble) })
  def ov84(p85: Int, p86: Long): Int = Tr.t(87, p85)
  def ov84(p88: String): Int = lz3
}
//#unit
import scala.reflect.ClassTag; import Imp._
class C1[@specialized(Int, Long, Double) AC1: ClassTag](val zAT0: String, val zBT0: AC1, val zAC1: AC1) extends T0[String, AC1] {
  val v89: Option[Sh] = (if (((None: Option[Int]) match { case Some(x90) if false => false; case Some(_) => false; case None => false })) ({ case x91 if x91 > 1 => (K2: Sh) }: PartialFunction[Int, Sh]).lift(0) else (try { if (true) throw new IllegalStateException("x"); Option[Sh]((K2: Sh)) } catch { case _: IllegalStateException => Option[Sh]((K0(1, "bc"): Sh)) } finally { Tr.t(92, 0) }))
  lazy val lz93: Array[Int] = Tr.t(94, Tr.t(95, Array[Int](v1)))
}
//#unit
import scala.reflect.ClassTag; import Imp._
object Main {
  def run(): String = {
    val sb = new java.lang.StringBuilder
    val c1 = new C0(3.0, (({ lazy val x96: String = ""; 0 }) + ({ @scala.annotation.tailrec def loop97(i98: Int, acc99: Int): Int = if (i98 <= 0) acc99 else loop97(i98 - 1, 3); loop97(2, 2) }))); sb.append("c1 [" + Tr.take() + "]\n")
    try { val r: Double = c1.zAT0; sb.append("c1.zAT0=" + r) } catch { case e: Throwable => sb.append("c1.zAT0!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: Int = c1.zBT0; sb.append("c1.zBT0=" + r) } catch { case e: Throwable => sb.append("c1.zBT0!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { c1.v1 = 3; val r: Int = c1.v1; sb.append("c1.v1=" + r) } catch { case e: Throwable => sb.append("c1.v1!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: Int = c1.lz3; sb.append("c1.lz3=" + r) } catch { case e: Throwable => sb.append("c1.lz3!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: (Double, Option[Int]) = c1.m16((for (x100 <- (None: Option[Double]); x101 <- Option[Int](7)) yield (K2: Sh))); sb.append("c1.m16=" + r) } catch { case e: Throwable => sb.append("c1.m16!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: List[Double] = c1.lz26; sb.append("c1.lz26=" + r) } catch { case e: Throwable => sb.append("c1.lz26!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { c1.v38 = ({ def loc103(): Option[Double] = { try { if (false) return Option[Double](0.5); Option[Double](2.5) } finally { Tr.t(104, 0) } }; loc103() }).map(((x102: Double) => (Option[Double](x102) match { case Some(x105) if true => 2.5; case Some(_) => x102; case None => x102 }))); val r: Option[Double] = c1.v38; sb.append("c1.v38=" + r) } catch { case e: Throwable => sb.append("c1.v38!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: Option[(Double, Int)] = c1.ov52((Option[Long](4L) match { case Some(x106) if false => "xyz"; case Some(_) => ""; case None => "a" }), Tr.t(107, 2.5)); sb.append("c1.ov52=" + r) } catch { case e: Throwable => sb.append("c1.ov52!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: Option[(Double, Int)] = c1.ov52(p71 = ({ @scala.annotation.tailrec def loop109(i110: Int, acc111: String): String = if (i110 <= 0) acc111 else loop109(i110 - 1, acc111); loop109(2, "bc") }), p70 = ((None: Option[Double]) match { case Some(x108) if false => 5; case Some(_) => 5; case None => 6 })); sb.append("c1.ov52=" + r) } catch { case e: Throwable => sb.append("c1.ov52!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: Int = c1.ov84(8, (8L + 4L)); sb.append("c1.ov84=" + r) } catch { case e: Throwable => sb.append("c1.ov84!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: Int = c1.ov84("a"); sb.append("c1.ov84=" + r) } catch { case e: Throwable => sb.append("c1.ov84!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    val c1T0: T0[Double, Int] = c1
    try { val r: Double = c1T0.zAT0; sb.append("c1T0.zAT0=" + r) } catch { case e: Throwable => sb.append("c1T0.zAT0!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: Int = c1T0.zBT0; sb.append("c1T0.zBT0=" + r) } catch { case e: Throwable => sb.append("c1T0.zBT0!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { c1T0.v1 = 6; val r: Int = c1T0.v1; sb.append("c1T0.v1=" + r) } catch { case e: Throwable => sb.append("c1T0.v1!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: Int = c1T0.lz3; sb.append("c1T0.lz3=" + r) } catch { case e: Throwable => sb.append("c1T0.lz3!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: (Double, Option[Int]) = c1T0.m16((((x112: Int) => (None: Option[Sh])): Sam0[Int, Option[Sh]]).ap(7)); sb.append("c1T0.m16=" + r) } catch { case e: Throwable => sb.append("c1T0.m16!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: List[Double] = c1T0.lz26; sb.append("c1T0.lz26=" + r) } catch { case e: Throwable => sb.append("c1T0.lz26!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    val c2 = new C1[Long]((((None: Option[Sh]) match { case Some(x118) if false => (K0(0, "a"): Sh); case Some(_) => (K0(7, "xyz"): Sh); case None => (K0(0, "bc"): Sh) }) match { case K0(x113, x114) if x113 > 2 => ({ @scala.annotation.tailrec def loop119(i120: Int, acc121: String): String = if (i120 <= 0) acc121 else loop119(i120 - 1, x114); loop119(2, x114) }); case K1(K0(x115, _), x116) => ""; case K1(_, x117) => (0 match { case Ev(Ev(x122)) => "a"; case Ev(x122) => "bc"; case x123 => "a" }); case K2 | K0(_, _) => (if (true) "a" else "xyz") }), (Option[Sh]((K0(1, "xyz"): Sh)).getOrElse((K2: Sh)) match { case K0(x124, x125) if x124 > 2 => x124.toLong; case K1(K0(x126, _), x127) => x127; case K1(_, x128) => 2L; case K2 | K0(_, _) => ((K2: Sh) match { case K0(x129, x130) if x129 > 2 => 3L; case K1(K0(x131, _), x132) => x132; case K1(_, x133) => x133; case K2 | K0(_, _) => 5L }) }), ({ var i134 = 0; var acc135: Long = 5L; while (i134 < 3) { acc135 = acc135 + acc135; i134 += 1 }; acc135 })); sb.append("c2 [" + Tr.take() + "]\n")
    try { val r: String = c2.zAT0; sb.append("c2.zAT0=" + r) } catch { case e: Throwable => sb.append("c2.zAT0!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: Long = c2.zBT0; sb.append("c2.zBT0=" + r) } catch { case e: Throwable => sb.append("c2.zBT0!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { c2.v1 = ((3, "xyz") match { case (0, x136) => ((x139: String) => 4); case (x137, "a" | "bc") => ((x140: String) => x137); case (x137, x138) if x137 > x138.length => ((x141: String) => x137); case _ => ((x142: String) => 6) }).apply(({ val x143: Function1[Int, Int] = ((x144: Int) => x144); "xyz" })); val r: Int = c2.v1; sb.append("c2.v1=" + r) } catch { case e: Throwable => sb.append("c2.v1!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: Long = c2.lz3; sb.append("c2.lz3=" + r) } catch { case e: Throwable => sb.append("c2.lz3!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: (Double, Option[Int]) = c2.m16((try { if (true) throw new IllegalStateException("x"); (None: Option[Sh]) } catch { case _: IllegalStateException => (None: Option[Sh]) } finally { Tr.t(145, 0) })); sb.append("c2.m16=" + r) } catch { case e: Throwable => sb.append("c2.m16!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: List[String] = c2.lz26; sb.append("c2.lz26=" + r) } catch { case e: Throwable => sb.append("c2.lz26!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: Long = c2.zAC1; sb.append("c2.zAC1=" + r) } catch { case e: Throwable => sb.append("c2.zAC1!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: Option[Sh] = c2.v89; sb.append("c2.v89=" + r) } catch { case e: Throwable => sb.append("c2.v89!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: Array[Int] = c2.lz93; sb.append("c2.lz93=" + r.toList) } catch { case e: Throwable => sb.append("c2.lz93!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    val c2T0: T0[String, Long] = c2
    try { val r: String = c2T0.zAT0; sb.append("c2T0.zAT0=" + r) } catch { case e: Throwable => sb.append("c2T0.zAT0!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: Long = c2T0.zBT0; sb.append("c2T0.zBT0=" + r) } catch { case e: Throwable => sb.append("c2T0.zBT0!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { c2T0.v1 = ({ def loc146(): Int = { try { if (false) return 0; ({ var i147 = 0; var acc148: Int = 8; while (i147 < 3) { acc148 = acc148 + acc148; i147 += 1 }; acc148 }) } finally { Tr.t(149, 0) } }; loc146() }); val r: Int = c2T0.v1; sb.append("c2T0.v1=" + r) } catch { case e: Throwable => sb.append("c2T0.v1!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: Long = c2T0.lz3; sb.append("c2T0.lz3=" + r) } catch { case e: Throwable => sb.append("c2T0.lz3!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: (Double, Option[Int]) = c2T0.m16(({ def loc150(p151: => Sh, p152: Double = 2.5): Sh = (K2: Sh); var x153: Int = 3; (None: Option[Sh]) })); sb.append("c2T0.m16=" + r) } catch { case e: Throwable => sb.append("c2T0.m16!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: List[String] = c2T0.lz26; sb.append("c2T0.lz26=" + r) } catch { case e: Throwable => sb.append("c2T0.lz26!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    val c3 = new C1[Double]((None: Option[Sam0[Long, String]]).getOrElse((((x154: Long) => "a"): Sam0[Long, String])).ap(2L), ({ def loc155(): Double = { try { if ((try { if (true) throw new IllegalStateException("x"); false } catch { case _: IllegalStateException => false } finally { Tr.t(156, 0) })) return 1.0; Option[Double](1.0).getOrElse(1.0) } finally { Tr.t(157, 0) } }; loc155() }), 2.5); sb.append("c3 [" + Tr.take() + "]\n")
    try { val r: String = c3.zAT0; sb.append("c3.zAT0=" + r) } catch { case e: Throwable => sb.append("c3.zAT0!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: Double = c3.zBT0; sb.append("c3.zBT0=" + r) } catch { case e: Throwable => sb.append("c3.zBT0!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { c3.v1 = List[Int]((0 match { case Ev(Ev(x158)) => 7; case Ev(x158) => x158; case x159 => x159 }), ({ def loc160(): Int = { try { if (true) return 6; 7 } finally { Tr.t(161, 0) } }; loc160() })).max; val r: Int = c3.v1; sb.append("c3.v1=" + r) } catch { case e: Throwable => sb.append("c3.v1!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: Double = c3.lz3; sb.append("c3.lz3=" + r) } catch { case e: Throwable => sb.append("c3.lz3!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: (Double, Option[Int]) = c3.m16(({ def loc162(): Option[Sh] = { try { if (false) return Option[Sh]((K0(4, "a"): Sh)); Option[Sh]((K2: Sh)) } finally { Tr.t(163, 0) } }; loc162() })); sb.append("c3.m16=" + r) } catch { case e: Throwable => sb.append("c3.m16!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: List[String] = c3.lz26; sb.append("c3.lz26=" + r) } catch { case e: Throwable => sb.append("c3.lz26!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: Double = c3.zAC1; sb.append("c3.zAC1=" + r) } catch { case e: Throwable => sb.append("c3.zAC1!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: Option[Sh] = c3.v89; sb.append("c3.v89=" + r) } catch { case e: Throwable => sb.append("c3.v89!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: Array[Int] = c3.lz93; sb.append("c3.lz93=" + r.toList) } catch { case e: Throwable => sb.append("c3.lz93!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    val c3T0: T0[String, Double] = c3
    try { val r: String = c3T0.zAT0; sb.append("c3T0.zAT0=" + r) } catch { case e: Throwable => sb.append("c3T0.zAT0!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: Double = c3T0.zBT0; sb.append("c3T0.zBT0=" + r) } catch { case e: Throwable => sb.append("c3T0.zBT0!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { c3T0.v1 = 6; val r: Int = c3T0.v1; sb.append("c3T0.v1=" + r) } catch { case e: Throwable => sb.append("c3T0.v1!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: Double = c3T0.lz3; sb.append("c3T0.lz3=" + r) } catch { case e: Throwable => sb.append("c3T0.lz3!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: (Double, Option[Int]) = c3T0.m16((((x164: Int) => Option[Sh]((K2: Sh))): Sam0[Int, Option[Sh]]).ap(8)); sb.append("c3T0.m16=" + r) } catch { case e: Throwable => sb.append("c3T0.m16!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    try { val r: List[String] = c3T0.lz26; sb.append("c3T0.lz26=" + r) } catch { case e: Throwable => sb.append("c3T0.lz26!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\n")
    sb.toString
  }
  def main(args: Array[String]): Unit = print(run())
}
// CRASH scala.reflect.internal.FatalError:  |   EmptyScope.enter |      while compiling: p.scala