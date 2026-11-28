// inline|ok/crash|CRASH scala.tools.asm.LimitExceededException: Too many allocated bytes
import scala.reflect.ClassTag
trait U0[A] extends Any { def uu: A; def uc(x: Int): (A, Int) = (uu, x) }
//#unit
import scala.reflect.ClassTag
trait Sam0[A, @specialized(Int) B] { def ap(a: A): B; def twice(a: A): (B, B) = (ap(a), ap(a)) }
//#unit
import scala.reflect.ClassTag
case class V0[T0](u: T0) extends AnyVal with U0[T0] {
  def uu: T0 = u
  def m4(p1: V0[Int], p2: T0, p3: Int*): Option[T0] = ({ def loc5(p6: Sam0[Int, T0], p7: T0 = p2): (T0, Int) = ((x8: V0[Int]) => (u, 8)).apply(V0[Int](6)); (None: Option[(Int, Option[T0])]).getOrElse((4, Option[T0](u))) })._2
  def m9(): List[Int] = ({ def loc10(p11: => Array[V0[Long]], p12: V0[Double] = V0[Double](1.0)): Option[T0] = (Option[T0](u), V0[String]("a"))._1; ({ val x13: Array[Int] = Array[Int](3); ((x14: String) => List[Int]()) }) }).apply("xyz")
  def m15(): List[T0] = List[T0](u)
}
//#unit
import scala.reflect.ClassTag
case class V1(u: Int) extends AnyVal with U0[Int] {
  def uu: Int = u
  def m16(): Int = 2
  def m17(): Long = ({ def loc19(p20: Int, p21: String = "xyz"): List[V0[Long]] = List[V0[Long]](V0[Long](7L)); Option[Long](8L) }).map(((x18: Long) => (V1(u), x18)._2)).getOrElse(8L)
  def m22(): Long = 1L
}
//#unit
import scala.reflect.ClassTag
trait T0[AT0, @specialized(Int, Long, Double) BT0] {
  def zAT0: AT0
  def zBT0: BT0
  var v23: Double = ({ var x24: (Double, V1) = Option[(Double, V1)]((1.0, V1(6))).getOrElse((0.5, V1(4))); val x25: (V0[Int], V1) = ({ val x26: Array[V1] = Array[V1](V1(2), V1(7)); val x27: Function1[V1, V0[String]] = ((x28: V1) => V0[String]("a")); (V0[Int](3), V1(4)) }); (((x29: V1) => 2.5): Sam0[V1, Double]).ap(V1(2)) })
  var v30: Int = ("".length + 4)
  def ov31(p32: String): BT0 = ({ class L33(p34: Sam0[BT0, AT0]) { def g(p35: AT0): AT0 = p35 }; def loc36(p37: Int): List[Long] = Array[Option[List[Long]]]((None: Option[List[Long]]), (None: Option[List[Long]])).apply(0).getOrElse((((x38: V0[Long]) => List[Long](0L)): Sam0[V0[Long], List[Long]]).ap(V0[Long](0L))); ({ def loc39(p40: => AT0, p41: Int = 5): V0[Long] = V0[Long](1L); val x42: V1 = ({ var x43: Int = v30; lazy val x44: Function1[BT0, Long] = ((x45: BT0) => 5L); V1(x43) }); zBT0 }) })
  def ov31(p46: V1): BT0 = ({ var x47: (Double, V0[Double]) = ((x48: String) => (1.0, V0[Double](3.0))).apply(""); lazy val x49: AT0 = ({ var x50: V0[Int] = V0[Int](3); zAT0 }); ((x51: V0[Int]) => (zBT0, v23)).apply(V0[Int](v30)) })._1
  lazy val lz52: Option[V1] = (if ((zAT0 == zAT0)) (None: Option[Option[V1]]).getOrElse(Option[V1](V1(8))) else ({ lazy val x54: V0[Long] = V0[Long](1L); def loc55(p56: => Option[AT0], p57: Double = v23): String = ""; Option[V1](V1(v30)) })).map(((x53: V1) => Array[(Double, V1)]((v23, x53), (0.5, V1(6))).apply(1)._2))
}
//#unit
import scala.reflect.ClassTag
abstract class AC[AAC] extends T0[Int, AAC] {
  def zAAC: AAC
  def ov58(p59: Double, p60: V0[Int]): Array[V0[Int]] = ({ var x61: Int = ({ val x62: V1 = (((x63: V1) => x63): Sam0[V1, V1]).ap(V1(zAT0)); v30 }); def loc64(p65: List[AAC]): String = ({ def loc66(p67: => Sam0[V1, AAC], p68: V0[String] = V0[String]("bc")): AAC = ({ x61 = v30; zBT0 }); "a" }); (if ((List[String]() == List[String]())) Option[Array[V0[Int]]](Array[V0[Int]](p60, p60)).getOrElse(Array[V0[Int]](p60, V0[Int](4))) else (Array[V0[Int]](p60), V1(zAT0))._1) })
  def ov58(p69: String): Array[V0[Int]] = Array[V0[Int]](({ lazy val x70: AAC = zAAC; ((x71: Long) => V0[Int](zAT0)) }).apply(((V1(3): Any) match { case x72: Long => x72; case _ => 4L })))
  def m74(p73: Double): V0[String] = ({ lazy val x75: AAC = ov31(V1(v30)); val x76: Sam0[AAC, String] = (((x77: Long) => ((x78: V0[Int]) => (((x79: AAC) => ""): Sam0[AAC, String]))): Sam0[Long, Function1[V0[Int], Sam0[AAC, String]]]).ap(8L).apply(V0[Int](8)); V0[String]("a") })
  def m80(): List[Option[AAC]]
  lazy val lz81: V0[Int] = (if (true) Array[V0[Int]](V0[Int](zAT0), V0[Int](zAT0)).apply(0) else (None: Option[Function1[Int, V0[Int]]]).getOrElse(((x82: Int) => V0[Int](x82))).apply((V0[String](""), zAT0)._2))
}
//#unit
import scala.reflect.ClassTag
class C0[@specialized(Int) AC0: ClassTag](val zAT0: Int, val zBT0: Int, val zAAC: Int, val zAC0: AC0) extends AC[Int] {
  override def m80(): List[Option[Int]] = ({ lazy val x83: V1 = ((x84: String) => (((x85: V0[Int]) => V1(zBT0)): Sam0[V0[Int], V1])).apply("a").ap(V0[Int](v30)); def loc86(p87: Option[Int], p88: AC0 = zAC0): Array[V1] = (if (false) Array[V1](x83, V1(v30)) else (if (false) Array[V1](x83) else Array[V1](x83))); (Array[List[Option[Int]]](List[Option[Int]]((None: Option[Int]))), 2.5)._1.apply(0) })
  val v89: Long = ({ class L90(p91: Option[AC0]) { def g(p92: Sam0[V1, AC0]): V0[Double] = ((x93: Int) => V0[Double](1.0)).apply(zAT0) }; Array[Long](0L, 5L).apply(0) })
  def m95(p94: V1 = (if (false) V1(2) else V1(5))): Array[AC0] = ({ class L96(p97: AC0) { def g(p98: V0[Long]): AC0 = p97 }; lazy val x99: (V0[String], AC0) = (V0[String]("xyz"), zAC0); (((x100: Long) => ("", Array[AC0](zAC0, zAC0))): Sam0[Long, (String, Array[AC0])]) }).ap(((x101: Int) => 7L).apply(zBT0))._2
  def ov102(p103: V0[Int]): (String, (V1, Int)) = Array[(Long, (String, (V1, Int)))]((v89, ("xyz", (V1(zBT0), 2))), (v89, ("xyz", (V1(zAAC), zAAC)))).apply(1)._2
  def ov102(p104: Int): (String, (V1, Int)) = ({ def loc105(p106: Option[AC0]): AC0 = ({ val x107: List[V1] = List[V1](V1(p104)); class L108(p109: String) { def g(p110: AC0): AC0 = p110 }; zAC0 }); class L111(p112: List[AC0]) { def g(p113: String): AC0 = zAC0 }; Array[(String, (V1, Int))](("bc", (V1(zAAC), p104)), ("", (V1(4), zBT0))) }).apply(0)
}
//#unit
import scala.reflect.ClassTag
class C1[AC1](val zAT0: Int, val zBT0: String, val zAAC: String, val zAC1: AC1) extends AC[String] {
  override def m80(): List[Option[String]] = ({ lazy val x114: V1 = lz52.getOrElse(V1(6)); var x115: List[Int] = (None: Option[List[Int]]).getOrElse(List[Int]()); (((x116: Int) => List[Option[String]](Option[String](zBT0), (None: Option[String]))): Sam0[Int, List[Option[String]]]) }).ap((({ class L117(p118: AC1) { def g(p119: Sam0[AC1, Double]): AC1 = zAC1 }; v30 }) + ({ val x120: AC1 = zAC1; val x121: AC1 = zAC1; v30 })))
  def ov122(p123: Int, p124: Int): Long = 0L
  def ov122(p125: V0[Long]): Long = ((Option[Function1[Int, V1]](((x127: Int) => V1(zAT0))).getOrElse(((x128: Int) => V1(x128))).apply((None: Option[Int]).getOrElse(7)): Any) match { case x126: Long => x126; case _ => (3L + Array[Long](1L).apply(0)) })
  def m131(p129: Int, p130: AC1): AC1 = ({ class L132(p133: Int) { def g(p134: (AC1, Long)): List[Int] = List[Int]() }; (((x135: Int) => (v23, zAC1)): Sam0[Int, (Double, AC1)]) }).ap((0, V0[String](""))._1)._2
  lazy val lz136: AC1 = (if (false) (3.0, (((x137: V0[String]) => zAC1): Sam0[V0[String], AC1])) else (v23, (((x138: V0[String]) => zAC1): Sam0[V0[String], AC1])))._2.ap(m74(v30.toDouble))
}
//#unit
import scala.reflect.ClassTag
object Main {
  def run(): String = {
    val sb = new java.lang.StringBuilder
    val c1 = new C0[String](0, ({ def loc139(p140: => Int, p141: Int = 2): Sam0[V1, String] = ((x142: V1) => (((x143: V1) => "bc"): Sam0[V1, String])).apply(V1(4)); var x144: V1 = (if (false) V1(7) else V1(8)); 8 }), (if ((((x145: String) => true): Sam0[String, Boolean]).ap("")) 7 else "bc".length), ("a" + ("" + V0[String]("bc"))))
    try { val r: Int = c1.zAT0; sb.append("c1.zAT0=" + r) } catch { case e: Throwable => sb.append("c1.zAT0!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Int = c1.zBT0; sb.append("c1.zBT0=" + r) } catch { case e: Throwable => sb.append("c1.zBT0!" + e.getClass.getName) }; sb.append('\n')
    try { c1.v23 = (None: Option[Int]).getOrElse(5).toDouble; val r: Double = c1.v23; sb.append("c1.v23=" + r) } catch { case e: Throwable => sb.append("c1.v23!" + e.getClass.getName) }; sb.append('\n')
    try { c1.v30 = (6 + "a".length); val r: Int = c1.v30; sb.append("c1.v30=" + r) } catch { case e: Throwable => sb.append("c1.v30!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Int = c1.ov31("a"); sb.append("c1.ov31=" + r) } catch { case e: Throwable => sb.append("c1.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Int = c1.ov31(("xyz", V1(4))._2); sb.append("c1.ov31=" + r) } catch { case e: Throwable => sb.append("c1.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Option[V1] = c1.lz52; sb.append("c1.lz52=" + r) } catch { case e: Throwable => sb.append("c1.lz52!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Int = c1.zAAC; sb.append("c1.zAAC=" + r) } catch { case e: Throwable => sb.append("c1.zAAC!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Array[V0[Int]] = c1.ov58(3.0, Array[V0[Int]](V0[Int](5)).apply(0)); sb.append("c1.ov58=" + r.toList) } catch { case e: Throwable => sb.append("c1.ov58!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Array[V0[Int]] = c1.ov58(("" + "")); sb.append("c1.ov58=" + r.toList) } catch { case e: Throwable => sb.append("c1.ov58!" + e.getClass.getName) }; sb.append('\n')
    try { val r: V0[String] = c1.m74(2.5); sb.append("c1.m74=" + r) } catch { case e: Throwable => sb.append("c1.m74!" + e.getClass.getName) }; sb.append('\n')
    try { val r: List[Option[Int]] = c1.m80(); sb.append("c1.m80=" + r) } catch { case e: Throwable => sb.append("c1.m80!" + e.getClass.getName) }; sb.append('\n')
    try { val r: V0[Int] = c1.lz81; sb.append("c1.lz81=" + r) } catch { case e: Throwable => sb.append("c1.lz81!" + e.getClass.getName) }; sb.append('\n')
    try { val r: String = c1.zAC0; sb.append("c1.zAC0=" + r) } catch { case e: Throwable => sb.append("c1.zAC0!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Long = c1.v89; sb.append("c1.v89=" + r) } catch { case e: Throwable => sb.append("c1.v89!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Array[String] = c1.m95(); sb.append("c1.m95=" + r.toList) } catch { case e: Throwable => sb.append("c1.m95!" + e.getClass.getName) }; sb.append('\n')
    try { val r: (String, (V1, Int)) = c1.ov102(({ var x146: V1 = V1(3); def loc147(p148: => Long, p149: Int = 0): Double = 3.0; V0[Int](8) })); sb.append("c1.ov102=" + r) } catch { case e: Throwable => sb.append("c1.ov102!" + e.getClass.getName) }; sb.append('\n')
    try { val r: (String, (V1, Int)) = c1.ov102(((x150: String) => 8).apply("xyz")); sb.append("c1.ov102=" + r) } catch { case e: Throwable => sb.append("c1.ov102!" + e.getClass.getName) }; sb.append('\n')
    val c1AC: AC[Int] = c1
    try { val r: Int = c1AC.zAT0; sb.append("c1AC.zAT0=" + r) } catch { case e: Throwable => sb.append("c1AC.zAT0!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Int = c1AC.zBT0; sb.append("c1AC.zBT0=" + r) } catch { case e: Throwable => sb.append("c1AC.zBT0!" + e.getClass.getName) }; sb.append('\n')
    try { c1AC.v23 = ((if (false) 2.5 else 3.0) + (if (true) 2.5 else 0.5)); val r: Double = c1AC.v23; sb.append("c1AC.v23=" + r) } catch { case e: Throwable => sb.append("c1AC.v23!" + e.getClass.getName) }; sb.append('\n')
    try { c1AC.v30 = ({ def loc151(p152: Double, p153: V0[String] = V0[String]("bc")): V1 = V1(3); var x154: (Double, Int) = ({ var x155: V1 = V1(6); (2.5, 3) }); (((x156: String) => 6): Sam0[String, Int]).ap("xyz") }); val r: Int = c1AC.v30; sb.append("c1AC.v30=" + r) } catch { case e: Throwable => sb.append("c1AC.v30!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Int = c1AC.ov31(({ def loc157(p158: V0[Int], p159: Long = 1L): Long = 0L; "a" })); sb.append("c1AC.ov31=" + r) } catch { case e: Throwable => sb.append("c1AC.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Int = c1AC.ov31(V1(0)); sb.append("c1AC.ov31=" + r) } catch { case e: Throwable => sb.append("c1AC.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Option[V1] = c1AC.lz52; sb.append("c1AC.lz52=" + r) } catch { case e: Throwable => sb.append("c1AC.lz52!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Int = c1AC.zAAC; sb.append("c1AC.zAAC=" + r) } catch { case e: Throwable => sb.append("c1AC.zAAC!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Array[V0[Int]] = c1AC.ov58(Array[Double](2.5).apply(0), (3, V0[Int](1))._2); sb.append("c1AC.ov58=" + r.toList) } catch { case e: Throwable => sb.append("c1AC.ov58!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Array[V0[Int]] = c1AC.ov58(("" + Option[V0[Long]](V0[Long](5L)))); sb.append("c1AC.ov58=" + r.toList) } catch { case e: Throwable => sb.append("c1AC.ov58!" + e.getClass.getName) }; sb.append('\n')
    try { val r: V0[String] = c1AC.m74((3L, 0.5)._2); sb.append("c1AC.m74=" + r) } catch { case e: Throwable => sb.append("c1AC.m74!" + e.getClass.getName) }; sb.append('\n')
    try { val r: List[Option[Int]] = c1AC.m80(); sb.append("c1AC.m80=" + r) } catch { case e: Throwable => sb.append("c1AC.m80!" + e.getClass.getName) }; sb.append('\n')
    try { val r: V0[Int] = c1AC.lz81; sb.append("c1AC.lz81=" + r) } catch { case e: Throwable => sb.append("c1AC.lz81!" + e.getClass.getName) }; sb.append('\n')
    val c1T0: T0[Int, Int] = c1
    try { val r: Int = c1T0.zAT0; sb.append("c1T0.zAT0=" + r) } catch { case e: Throwable => sb.append("c1T0.zAT0!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Int = c1T0.zBT0; sb.append("c1T0.zBT0=" + r) } catch { case e: Throwable => sb.append("c1T0.zBT0!" + e.getClass.getName) }; sb.append('\n')
    try { c1T0.v23 = ((((x160: Int) => 3.0): Sam0[Int, Double]).ap(7) + 6.toDouble); val r: Double = c1T0.v23; sb.append("c1T0.v23=" + r) } catch { case e: Throwable => sb.append("c1T0.v23!" + e.getClass.getName) }; sb.append('\n')
    try { c1T0.v30 = 7; val r: Int = c1T0.v30; sb.append("c1T0.v30=" + r) } catch { case e: Throwable => sb.append("c1T0.v30!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Int = c1T0.ov31(("a" + "bc")); sb.append("c1T0.ov31=" + r) } catch { case e: Throwable => sb.append("c1T0.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Int = c1T0.ov31(Option[V1](V1(4)).getOrElse(V1(8))); sb.append("c1T0.ov31=" + r) } catch { case e: Throwable => sb.append("c1T0.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Option[V1] = c1T0.lz52; sb.append("c1T0.lz52=" + r) } catch { case e: Throwable => sb.append("c1T0.lz52!" + e.getClass.getName) }; sb.append('\n')
    val c2 = new C0[Int](Array[Int](6).apply(0), ("a", (3, V1(3)))._2._1, (if ((5 > 6)) ((x161: Int) => x161).apply(0) else 1), 2)
    try { val r: Int = c2.zAT0; sb.append("c2.zAT0=" + r) } catch { case e: Throwable => sb.append("c2.zAT0!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Int = c2.zBT0; sb.append("c2.zBT0=" + r) } catch { case e: Throwable => sb.append("c2.zBT0!" + e.getClass.getName) }; sb.append('\n')
    try { c2.v23 = ((((x163: V1) => 0).apply(V1(8)): Any) match { case x162: Double => x162; case _ => (((x164: Int) => 2.5): Sam0[Int, Double]).ap(4) }); val r: Double = c2.v23; sb.append("c2.v23=" + r) } catch { case e: Throwable => sb.append("c2.v23!" + e.getClass.getName) }; sb.append('\n')
    try { c2.v30 = (((x165: V0[String]) => (((x166: Int) => x166): Sam0[Int, Int])): Sam0[V0[String], Sam0[Int, Int]]).ap(V0[String]("")).ap((6, 3)._1); val r: Int = c2.v30; sb.append("c2.v30=" + r) } catch { case e: Throwable => sb.append("c2.v30!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Int = c2.ov31("xyz"); sb.append("c2.ov31=" + r) } catch { case e: Throwable => sb.append("c2.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Int = c2.ov31(((x167: Double) => V1(7)).apply(0.5)); sb.append("c2.ov31=" + r) } catch { case e: Throwable => sb.append("c2.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Option[V1] = c2.lz52; sb.append("c2.lz52=" + r) } catch { case e: Throwable => sb.append("c2.lz52!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Int = c2.zAAC; sb.append("c2.zAAC=" + r) } catch { case e: Throwable => sb.append("c2.zAAC!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Array[V0[Int]] = c2.ov58(Option[Double](0.5).getOrElse(2.5), ((x168: Int) => V0[Int](x168)).apply(4)); sb.append("c2.ov58=" + r.toList) } catch { case e: Throwable => sb.append("c2.ov58!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Array[V0[Int]] = c2.ov58(({ class L169(p170: Function1[V1, V1]) { def g(p171: String): Int = 3 }; var x172: (V0[Int], V1) = (V0[Int](5), V1(3)); "a" })); sb.append("c2.ov58=" + r.toList) } catch { case e: Throwable => sb.append("c2.ov58!" + e.getClass.getName) }; sb.append('\n')
    try { val r: V0[String] = c2.m74((1.0 + 3.0)); sb.append("c2.m74=" + r) } catch { case e: Throwable => sb.append("c2.m74!" + e.getClass.getName) }; sb.append('\n')
    try { val r: List[Option[Int]] = c2.m80(); sb.append("c2.m80=" + r) } catch { case e: Throwable => sb.append("c2.m80!" + e.getClass.getName) }; sb.append('\n')
    try { val r: V0[Int] = c2.lz81; sb.append("c2.lz81=" + r) } catch { case e: Throwable => sb.append("c2.lz81!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Int = c2.zAC0; sb.append("c2.zAC0=" + r) } catch { case e: Throwable => sb.append("c2.zAC0!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Long = c2.v89; sb.append("c2.v89=" + r) } catch { case e: Throwable => sb.append("c2.v89!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Array[Int] = c2.m95(((V1(8): Any) match { case x173: V1 => x173; case _ => V1(4) })); sb.append("c2.m95=" + r.toList) } catch { case e: Throwable => sb.append("c2.m95!" + e.getClass.getName) }; sb.append('\n')
    try { val r: (String, (V1, Int)) = c2.ov102((((x174: Double) => V0[Int](1)): Sam0[Double, V0[Int]]).ap(3.0)); sb.append("c2.ov102=" + r) } catch { case e: Throwable => sb.append("c2.ov102!" + e.getClass.getName) }; sb.append('\n')
    try { val r: (String, (V1, Int)) = c2.ov102((6 + 0)); sb.append("c2.ov102=" + r) } catch { case e: Throwable => sb.append("c2.ov102!" + e.getClass.getName) }; sb.append('\n')
    val c2AC: AC[Int] = c2
    try { val r: Int = c2AC.zAT0; sb.append("c2AC.zAT0=" + r) } catch { case e: Throwable => sb.append("c2AC.zAT0!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Int = c2AC.zBT0; sb.append("c2AC.zBT0=" + r) } catch { case e: Throwable => sb.append("c2AC.zBT0!" + e.getClass.getName) }; sb.append('\n')
    try { c2AC.v23 = ((x175: Double) => x175).apply(3.toDouble); val r: Double = c2AC.v23; sb.append("c2AC.v23=" + r) } catch { case e: Throwable => sb.append("c2AC.v23!" + e.getClass.getName) }; sb.append('\n')
    try { c2AC.v30 = ({ def loc176(p177: => Function1[Long, V1]): String = "xyz"; ((V1(2): Any) match { case x178: Int => x178; case _ => 2 }) }); val r: Int = c2AC.v30; sb.append("c2AC.v30=" + r) } catch { case e: Throwable => sb.append("c2AC.v30!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Int = c2AC.ov31(((x179: String) => "bc").apply("")); sb.append("c2AC.ov31=" + r) } catch { case e: Throwable => sb.append("c2AC.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Int = c2AC.ov31((None: Option[V1]).getOrElse(V1(0))); sb.append("c2AC.ov31=" + r) } catch { case e: Throwable => sb.append("c2AC.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Option[V1] = c2AC.lz52; sb.append("c2AC.lz52=" + r) } catch { case e: Throwable => sb.append("c2AC.lz52!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Int = c2AC.zAAC; sb.append("c2AC.zAAC=" + r) } catch { case e: Throwable => sb.append("c2AC.zAAC!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Array[V0[Int]] = c2AC.ov58((0.5 + 3.0), (None: Option[V0[Int]]).getOrElse(V0[Int](8))); sb.append("c2AC.ov58=" + r.toList) } catch { case e: Throwable => sb.append("c2AC.ov58!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Array[V0[Int]] = c2AC.ov58(((2L: Any) match { case x180: String => x180; case _ => "xyz" })); sb.append("c2AC.ov58=" + r.toList) } catch { case e: Throwable => sb.append("c2AC.ov58!" + e.getClass.getName) }; sb.append('\n')
    try { val r: V0[String] = c2AC.m74(1.0); sb.append("c2AC.m74=" + r) } catch { case e: Throwable => sb.append("c2AC.m74!" + e.getClass.getName) }; sb.append('\n')
    try { val r: List[Option[Int]] = c2AC.m80(); sb.append("c2AC.m80=" + r) } catch { case e: Throwable => sb.append("c2AC.m80!" + e.getClass.getName) }; sb.append('\n')
    try { val r: V0[Int] = c2AC.lz81; sb.append("c2AC.lz81=" + r) } catch { case e: Throwable => sb.append("c2AC.lz81!" + e.getClass.getName) }; sb.append('\n')
    val c2T0: T0[Int, Int] = c2
    try { val r: Int = c2T0.zAT0; sb.append("c2T0.zAT0=" + r) } catch { case e: Throwable => sb.append("c2T0.zAT0!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Int = c2T0.zBT0; sb.append("c2T0.zBT0=" + r) } catch { case e: Throwable => sb.append("c2T0.zBT0!" + e.getClass.getName) }; sb.append('\n')
    try { c2T0.v23 = ((V1(1): Any) match { case x181: Double => x181; case _ => ((0L: Any) match { case x182: Double => x182; case _ => 2.5 }) }); val r: Double = c2T0.v23; sb.append("c2T0.v23=" + r) } catch { case e: Throwable => sb.append("c2T0.v23!" + e.getClass.getName) }; sb.append('\n')
    try { c2T0.v30 = (((V1(8), V0[String](""))._2: Any) match { case x183: Int => x183; case _ => Array[Int](3).apply(0) }); val r: Int = c2T0.v30; sb.append("c2T0.v30=" + r) } catch { case e: Throwable => sb.append("c2T0.v30!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Int = c2T0.ov31("a"); sb.append("c2T0.ov31=" + r) } catch { case e: Throwable => sb.append("c2T0.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Int = c2T0.ov31(V1(1)); sb.append("c2T0.ov31=" + r) } catch { case e: Throwable => sb.append("c2T0.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Option[V1] = c2T0.lz52; sb.append("c2T0.lz52=" + r) } catch { case e: Throwable => sb.append("c2T0.lz52!" + e.getClass.getName) }; sb.append('\n')
    val c3 = new C1[Double](0, (((7, "a")._1: Any) match { case x184: String => x184; case _ => ((0: Any) match { case x185: String => x185; case _ => "" }) }), ((if (false) "xyz" else "xyz") + ("" + 4L)), 0.5)
    try { val r: Int = c3.zAT0; sb.append("c3.zAT0=" + r) } catch { case e: Throwable => sb.append("c3.zAT0!" + e.getClass.getName) }; sb.append('\n')
    try { val r: String = c3.zBT0; sb.append("c3.zBT0=" + r) } catch { case e: Throwable => sb.append("c3.zBT0!" + e.getClass.getName) }; sb.append('\n')
    try { c3.v23 = (({ def loc186(p187: => V0[Double], p188: V0[String] = V0[String]("bc")): (V1, Long) = (V1(1), 0L); lazy val x189: Option[Int] = (None: Option[Int]); 0.5 }) + ((x190: Long) => 0.5).apply(2L)); val r: Double = c3.v23; sb.append("c3.v23=" + r) } catch { case e: Throwable => sb.append("c3.v23!" + e.getClass.getName) }; sb.append('\n')
    try { c3.v30 = ({ def loc191(p192: V1): List[Double] = List[Double](0.5, 2.5); (None: Option[Int]).getOrElse(7) }); val r: Int = c3.v30; sb.append("c3.v30=" + r) } catch { case e: Throwable => sb.append("c3.v30!" + e.getClass.getName) }; sb.append('\n')
    try { val r: String = c3.ov31(((x193: String) => x193).apply("xyz")); sb.append("c3.ov31=" + r) } catch { case e: Throwable => sb.append("c3.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: String = c3.ov31(Array[V1](V1(0), V1(0)).apply(1)); sb.append("c3.ov31=" + r) } catch { case e: Throwable => sb.append("c3.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Option[V1] = c3.lz52; sb.append("c3.lz52=" + r) } catch { case e: Throwable => sb.append("c3.lz52!" + e.getClass.getName) }; sb.append('\n')
    try { val r: String = c3.zAAC; sb.append("c3.zAAC=" + r) } catch { case e: Throwable => sb.append("c3.zAAC!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Array[V0[Int]] = c3.ov58((((x194: Int) => 3.0): Sam0[Int, Double]).ap(0), (if (false) V0[Int](0) else V0[Int](7))); sb.append("c3.ov58=" + r.toList) } catch { case e: Throwable => sb.append("c3.ov58!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Array[V0[Int]] = c3.ov58(Array[String]("", "a").apply(0)); sb.append("c3.ov58=" + r.toList) } catch { case e: Throwable => sb.append("c3.ov58!" + e.getClass.getName) }; sb.append('\n')
    try { val r: V0[String] = c3.m74(Array[Double](0.5).apply(0)); sb.append("c3.m74=" + r) } catch { case e: Throwable => sb.append("c3.m74!" + e.getClass.getName) }; sb.append('\n')
    try { val r: List[Option[String]] = c3.m80(); sb.append("c3.m80=" + r) } catch { case e: Throwable => sb.append("c3.m80!" + e.getClass.getName) }; sb.append('\n')
    try { val r: V0[Int] = c3.lz81; sb.append("c3.lz81=" + r) } catch { case e: Throwable => sb.append("c3.lz81!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Double = c3.zAC1; sb.append("c3.zAC1=" + r) } catch { case e: Throwable => sb.append("c3.zAC1!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Long = c3.ov122(({ def loc195(p196: V1): (Double, V1) = (0.5, V1(0)); 8 }), (V0[Double](0.5), 4)._2); sb.append("c3.ov122=" + r) } catch { case e: Throwable => sb.append("c3.ov122!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Long = c3.ov122(V0[Long](6L)); sb.append("c3.ov122=" + r) } catch { case e: Throwable => sb.append("c3.ov122!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Double = c3.m131(8, 1.0); sb.append("c3.m131=" + r) } catch { case e: Throwable => sb.append("c3.m131!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Double = c3.lz136; sb.append("c3.lz136=" + r) } catch { case e: Throwable => sb.append("c3.lz136!" + e.getClass.getName) }; sb.append('\n')
    val c3AC: AC[String] = c3
    try { val r: Int = c3AC.zAT0; sb.append("c3AC.zAT0=" + r) } catch { case e: Throwable => sb.append("c3AC.zAT0!" + e.getClass.getName) }; sb.append('\n')
    try { val r: String = c3AC.zBT0; sb.append("c3AC.zBT0=" + r) } catch { case e: Throwable => sb.append("c3AC.zBT0!" + e.getClass.getName) }; sb.append('\n')
    try { c3AC.v23 = ({ def loc197(p198: Array[V1], p199: V1 = V1(3)): Long = (((x200: V1) => 5L): Sam0[V1, Long]).ap(p199); ((V0[Int](1): Any) match { case x201: Double => x201; case _ => 2.5 }) }); val r: Double = c3AC.v23; sb.append("c3AC.v23=" + r) } catch { case e: Throwable => sb.append("c3AC.v23!" + e.getClass.getName) }; sb.append('\n')
    try { c3AC.v30 = (((x202: Long) => 7): Sam0[Long, Int]).ap((((x203: String) => 1L): Sam0[String, Long]).ap("bc")); val r: Int = c3AC.v30; sb.append("c3AC.v30=" + r) } catch { case e: Throwable => sb.append("c3AC.v30!" + e.getClass.getName) }; sb.append('\n')
    try { val r: String = c3AC.ov31((if (true) "a" else "a")); sb.append("c3AC.ov31=" + r) } catch { case e: Throwable => sb.append("c3AC.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: String = c3AC.ov31(("bc", V1(6))._2); sb.append("c3AC.ov31=" + r) } catch { case e: Throwable => sb.append("c3AC.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Option[V1] = c3AC.lz52; sb.append("c3AC.lz52=" + r) } catch { case e: Throwable => sb.append("c3AC.lz52!" + e.getClass.getName) }; sb.append('\n')
    try { val r: String = c3AC.zAAC; sb.append("c3AC.zAAC=" + r) } catch { case e: Throwable => sb.append("c3AC.zAAC!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Array[V0[Int]] = c3AC.ov58((((x204: Int) => 1.0): Sam0[Int, Double]).ap(2), (((x205: Long) => V0[Int](4)): Sam0[Long, V0[Int]]).ap(7L)); sb.append("c3AC.ov58=" + r.toList) } catch { case e: Throwable => sb.append("c3AC.ov58!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Array[V0[Int]] = c3AC.ov58(({ def loc206(p207: (V0[Int], Long), p208: V0[Long] = V0[Long](3L)): List[Long] = List[Long](6L, 0L); class L209(p210: List[V1]) { def g(p211: (V1, Int)): String = "" }; "a" })); sb.append("c3AC.ov58=" + r.toList) } catch { case e: Throwable => sb.append("c3AC.ov58!" + e.getClass.getName) }; sb.append('\n')
    try { val r: V0[String] = c3AC.m74(Array[Double](3.0, 0.5).apply(0)); sb.append("c3AC.m74=" + r) } catch { case e: Throwable => sb.append("c3AC.m74!" + e.getClass.getName) }; sb.append('\n')
    try { val r: List[Option[String]] = c3AC.m80(); sb.append("c3AC.m80=" + r) } catch { case e: Throwable => sb.append("c3AC.m80!" + e.getClass.getName) }; sb.append('\n')
    try { val r: V0[Int] = c3AC.lz81; sb.append("c3AC.lz81=" + r) } catch { case e: Throwable => sb.append("c3AC.lz81!" + e.getClass.getName) }; sb.append('\n')
    val c3T0: T0[Int, String] = c3
    try { val r: Int = c3T0.zAT0; sb.append("c3T0.zAT0=" + r) } catch { case e: Throwable => sb.append("c3T0.zAT0!" + e.getClass.getName) }; sb.append('\n')
    try { val r: String = c3T0.zBT0; sb.append("c3T0.zBT0=" + r) } catch { case e: Throwable => sb.append("c3T0.zBT0!" + e.getClass.getName) }; sb.append('\n')
    try { c3T0.v23 = 3.0; val r: Double = c3T0.v23; sb.append("c3T0.v23=" + r) } catch { case e: Throwable => sb.append("c3T0.v23!" + e.getClass.getName) }; sb.append('\n')
    try { c3T0.v30 = ({ val x212: Function1[V0[String], V0[Int]] = ((x213: V0[String]) => V0[Int](2)); def loc214(p215: => Option[Double]): List[Int] = List[Int](1, 8); Option[Int](3) }).getOrElse((((x216: String) => 5): Sam0[String, Int]).ap("a")); val r: Int = c3T0.v30; sb.append("c3T0.v30=" + r) } catch { case e: Throwable => sb.append("c3T0.v30!" + e.getClass.getName) }; sb.append('\n')
    try { val r: String = c3T0.ov31(("" + (5, V1(7)))); sb.append("c3T0.ov31=" + r) } catch { case e: Throwable => sb.append("c3T0.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: String = c3T0.ov31((if (false) V1(4) else V1(7))); sb.append("c3T0.ov31=" + r) } catch { case e: Throwable => sb.append("c3T0.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Option[V1] = c3T0.lz52; sb.append("c3T0.lz52=" + r) } catch { case e: Throwable => sb.append("c3T0.lz52!" + e.getClass.getName) }; sb.append('\n')
    val c4 = new C1[Double](({ class L217(p218: Int) { def g(p219: V0[Long]): Long = 6L }; var x220: Double = 3.0; Array[Int](8) }).apply(0), (if ((V1(3) == V1(1))) "bc" else ({ lazy val x221: Double = 1.0; lazy val x222: Sam0[Int, Int] = (((x223: Int) => 0): Sam0[Int, Int]); "bc" })), Array[String]("", "xyz").apply(1), (((x224: V1) => (((x225: V1) => 1.0): Sam0[V1, Double])): Sam0[V1, Sam0[V1, Double]]).ap(V1(5)).ap(Option[V1](V1(1)).getOrElse(V1(4))))
    try { val r: Int = c4.zAT0; sb.append("c4.zAT0=" + r) } catch { case e: Throwable => sb.append("c4.zAT0!" + e.getClass.getName) }; sb.append('\n')
    try { val r: String = c4.zBT0; sb.append("c4.zBT0=" + r) } catch { case e: Throwable => sb.append("c4.zBT0!" + e.getClass.getName) }; sb.append('\n')
    try { c4.v23 = (((x226: V0[String]) => Option[Double](1.0)): Sam0[V0[String], Option[Double]]).ap(V0[String]("a")).getOrElse(((x227: Int) => 1.0).apply(8)); val r: Double = c4.v23; sb.append("c4.v23=" + r) } catch { case e: Throwable => sb.append("c4.v23!" + e.getClass.getName) }; sb.append('\n')
    try { c4.v30 = ((x228: Int) => (((x229: Double) => x228): Sam0[Double, Int])).apply(2).ap(Array[Double](3.0).apply(1)); val r: Int = c4.v30; sb.append("c4.v30=" + r) } catch { case e: Throwable => sb.append("c4.v30!" + e.getClass.getName) }; sb.append('\n')
    try { val r: String = c4.ov31(((6: Any) match { case x230: String => x230; case _ => "a" })); sb.append("c4.ov31=" + r) } catch { case e: Throwable => sb.append("c4.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: String = c4.ov31(({ val x231: V1 = V1(1); def loc232(p233: Option[V1]): Double = 3.0; x231 })); sb.append("c4.ov31=" + r) } catch { case e: Throwable => sb.append("c4.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Option[V1] = c4.lz52; sb.append("c4.lz52=" + r) } catch { case e: Throwable => sb.append("c4.lz52!" + e.getClass.getName) }; sb.append('\n')
    try { val r: String = c4.zAAC; sb.append("c4.zAAC=" + r) } catch { case e: Throwable => sb.append("c4.zAAC!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Array[V0[Int]] = c4.ov58(0.5, (((x234: Int) => V0[Int](6)): Sam0[Int, V0[Int]]).ap(0)); sb.append("c4.ov58=" + r.toList) } catch { case e: Throwable => sb.append("c4.ov58!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Array[V0[Int]] = c4.ov58((((x235: Long) => "a"): Sam0[Long, String]).ap(0L)); sb.append("c4.ov58=" + r.toList) } catch { case e: Throwable => sb.append("c4.ov58!" + e.getClass.getName) }; sb.append('\n')
    try { val r: V0[String] = c4.m74(Array[Double](3.0, 3.0).apply(0)); sb.append("c4.m74=" + r) } catch { case e: Throwable => sb.append("c4.m74!" + e.getClass.getName) }; sb.append('\n')
    try { val r: List[Option[String]] = c4.m80(); sb.append("c4.m80=" + r) } catch { case e: Throwable => sb.append("c4.m80!" + e.getClass.getName) }; sb.append('\n')
    try { val r: V0[Int] = c4.lz81; sb.append("c4.lz81=" + r) } catch { case e: Throwable => sb.append("c4.lz81!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Double = c4.zAC1; sb.append("c4.zAC1=" + r) } catch { case e: Throwable => sb.append("c4.zAC1!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Long = c4.ov122((if (false) 4 else 8), 2); sb.append("c4.ov122=" + r) } catch { case e: Throwable => sb.append("c4.ov122!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Long = c4.ov122((((x236: Int) => V0[Long](0L)): Sam0[Int, V0[Long]]).ap(3)); sb.append("c4.ov122=" + r) } catch { case e: Throwable => sb.append("c4.ov122!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Double = c4.m131(8, 1.0); sb.append("c4.m131=" + r) } catch { case e: Throwable => sb.append("c4.m131!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Double = c4.lz136; sb.append("c4.lz136=" + r) } catch { case e: Throwable => sb.append("c4.lz136!" + e.getClass.getName) }; sb.append('\n')
    val c4AC: AC[String] = c4
    try { val r: Int = c4AC.zAT0; sb.append("c4AC.zAT0=" + r) } catch { case e: Throwable => sb.append("c4AC.zAT0!" + e.getClass.getName) }; sb.append('\n')
    try { val r: String = c4AC.zBT0; sb.append("c4AC.zBT0=" + r) } catch { case e: Throwable => sb.append("c4AC.zBT0!" + e.getClass.getName) }; sb.append('\n')
    try { c4AC.v23 = ({ lazy val x237: V1 = V1(0); def loc238(p239: => String, p240: V0[Int] = V0[Int](3)): Array[Int] = Array[Int](3, 2); (None: Option[Double]) }).getOrElse((((x241: Int) => 2.5): Sam0[Int, Double]).ap(3)); val r: Double = c4AC.v23; sb.append("c4AC.v23=" + r) } catch { case e: Throwable => sb.append("c4AC.v23!" + e.getClass.getName) }; sb.append('\n')
    try { c4AC.v30 = ({ val x242: Array[V0[Long]] = Array[V0[Long]](V0[Long](1L), V0[Long](2L)); (None: Option[Int]) }).getOrElse("a".length); val r: Int = c4AC.v30; sb.append("c4AC.v30=" + r) } catch { case e: Throwable => sb.append("c4AC.v30!" + e.getClass.getName) }; sb.append('\n')
    try { val r: String = c4AC.ov31(((x243: V1) => "bc").apply(V1(5))); sb.append("c4AC.ov31=" + r) } catch { case e: Throwable => sb.append("c4AC.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: String = c4AC.ov31(V1(8)); sb.append("c4AC.ov31=" + r) } catch { case e: Throwable => sb.append("c4AC.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Option[V1] = c4AC.lz52; sb.append("c4AC.lz52=" + r) } catch { case e: Throwable => sb.append("c4AC.lz52!" + e.getClass.getName) }; sb.append('\n')
    try { val r: String = c4AC.zAAC; sb.append("c4AC.zAAC=" + r) } catch { case e: Throwable => sb.append("c4AC.zAAC!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Array[V0[Int]] = c4AC.ov58((0.5, V1(2))._1, V0[Int](2)); sb.append("c4AC.ov58=" + r.toList) } catch { case e: Throwable => sb.append("c4AC.ov58!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Array[V0[Int]] = c4AC.ov58(Array[String]("xyz", "bc").apply(0)); sb.append("c4AC.ov58=" + r.toList) } catch { case e: Throwable => sb.append("c4AC.ov58!" + e.getClass.getName) }; sb.append('\n')
    try { val r: V0[String] = c4AC.m74((V0[Double](3.0), 2.5)._2); sb.append("c4AC.m74=" + r) } catch { case e: Throwable => sb.append("c4AC.m74!" + e.getClass.getName) }; sb.append('\n')
    try { val r: List[Option[String]] = c4AC.m80(); sb.append("c4AC.m80=" + r) } catch { case e: Throwable => sb.append("c4AC.m80!" + e.getClass.getName) }; sb.append('\n')
    try { val r: V0[Int] = c4AC.lz81; sb.append("c4AC.lz81=" + r) } catch { case e: Throwable => sb.append("c4AC.lz81!" + e.getClass.getName) }; sb.append('\n')
    val c4T0: T0[Int, String] = c4
    try { val r: Int = c4T0.zAT0; sb.append("c4T0.zAT0=" + r) } catch { case e: Throwable => sb.append("c4T0.zAT0!" + e.getClass.getName) }; sb.append('\n')
    try { val r: String = c4T0.zBT0; sb.append("c4T0.zBT0=" + r) } catch { case e: Throwable => sb.append("c4T0.zBT0!" + e.getClass.getName) }; sb.append('\n')
    try { c4T0.v23 = (None: Option[Sam0[Long, Double]]).getOrElse((((x244: Long) => 1.0): Sam0[Long, Double])).ap(((x245: V1) => 0L).apply(V1(7))); val r: Double = c4T0.v23; sb.append("c4T0.v23=" + r) } catch { case e: Throwable => sb.append("c4T0.v23!" + e.getClass.getName) }; sb.append('\n')
    try { c4T0.v30 = ({ def loc246(p247: => List[Long], p248: V0[String] = V0[String]("xyz")): V1 = V1(1); lazy val x249: List[Int] = List[Int](3); "bc" }).length; val r: Int = c4T0.v30; sb.append("c4T0.v30=" + r) } catch { case e: Throwable => sb.append("c4T0.v30!" + e.getClass.getName) }; sb.append('\n')
    try { val r: String = c4T0.ov31((if (true) "bc" else "a")); sb.append("c4T0.ov31=" + r) } catch { case e: Throwable => sb.append("c4T0.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: String = c4T0.ov31((((x250: String) => V1(1)): Sam0[String, V1]).ap("")); sb.append("c4T0.ov31=" + r) } catch { case e: Throwable => sb.append("c4T0.ov31!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Option[V1] = c4T0.lz52; sb.append("c4T0.lz52=" + r) } catch { case e: Throwable => sb.append("c4T0.lz52!" + e.getClass.getName) }; sb.append('\n')
    val w5: V0[Long] = ({ var x251: V1 = V1(5); V0[Long](3L) })
    try { val r: Option[Long] = w5.m4((if (false) V0[Int](6) else V0[Int](0)), 3L); sb.append("w5.m4=" + r) } catch { case e: Throwable => sb.append("w5.m4!" + e.getClass.getName) }; sb.append('\n')
    try { val r: List[Int] = w5.m9(); sb.append("w5.m9=" + r) } catch { case e: Throwable => sb.append("w5.m9!" + e.getClass.getName) }; sb.append('\n')
    try { val r: List[Long] = w5.m15(); sb.append("w5.m15=" + r) } catch { case e: Throwable => sb.append("w5.m15!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Any = w5; sb.append("w5.any=" + r + (r == w5)) } catch { case e: Throwable => sb.append("w5.any!" + e.getClass.getName) }; sb.append('\n')
    val w6: V1 = (if (false) V1(4) else V1(1))
    try { val r: Int = w6.m16(); sb.append("w6.m16=" + r) } catch { case e: Throwable => sb.append("w6.m16!" + e.getClass.getName) }; sb.append('\n')
    try { val r: Long = w6.m17(); sb.append("w6.m17=" + r) } catch { case e: Throwable => sb.append("w6.m17!" + e.getClass.getName) }; sb.append('\n')
    sb.toString
  }
}
