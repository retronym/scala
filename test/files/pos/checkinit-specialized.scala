//> using options -Xcheckinit

class Sp[@specialized(Int) A](val a: A) { val y = a.toString }

class Sp2[@specialized(Int, Long) A](a: A) {
  val x: A = a
  val y = "s"
  var z = 1
  @transient val t = "t"
  lazy val l = y
}

class Sp3[@specialized(Int) A, @specialized(Int) B](a: A, b: B) { val ab = (a, b); val s = ab.toString }
