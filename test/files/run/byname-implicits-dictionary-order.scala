// The entries of the dictionary of by-name implicits are vals of one class. An entry that another
// entry takes by value has to be initialized first.
class Y(y0: => Y) { def y = y0; override def toString = "Y" }
object Y { implicit def mkY(implicit y: => Y): Y = new Y(y) }

class X(x0: => X, val y: Y) { def x = x0 }
object X { implicit def mkX(implicit x: => X, y: Y): X = new X(x, y) }

class R(val y: Y, val x: X)
object R { implicit def mkR(implicit y: Y, x: X): R = new R(y, x) }

// the same with the by-value reference inside the argument of an entry that is not in the dictionary
class W(val x: X)
object W { implicit def mkW(implicit x: X): W = new W(x) }
class Z(z0: => Z, val w: W) { def z = z0 }
object Z { implicit def mkZ(implicit z: => Z, w: W): Z = new Z(z, w) }
class S(val x: X, val z: Z)
object S { implicit def mkS(implicit x: X, z: Z): S = new S(x, z) }

object Test {
  def main(args: Array[String]): Unit = {
    val r = implicitly[R]
    println(r.y)
    println(r.x.y)
    println(r.x.x.y)
    println(r.x.y.y)
    val s = implicitly[S]
    println(s.z.w.x.y)
    println(s.z.z.w.x.x.y)
    println(s.z.w.x eq s.x)
  }
}
