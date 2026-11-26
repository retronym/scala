//> using options -Werror -deprecation
object C {
  final val T = true
  final val F = false
}

class Test {
  @deprecated("use y", "1.0") def old: Int = 1
  def y: Int = 1

  def a = if (C.T) old else 0
  def b = if (C.F) 0 else old
  def c = if (C.T) (if (C.F) 0 else old) else 1
  def d(s: String) = if (C.T) (if (C.F) s else null) match { case null => old; case _ => 0 } else 1
}
