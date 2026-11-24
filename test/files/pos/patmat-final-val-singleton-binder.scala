// The binder of a singleton type pattern on a path has the path's type, also when the path is a
// final val of constant type, whose singleton type is a literal type.
// This test fails to compile on 2.13.18: `y` is typed as the scrutinee type, `Any`.
object Test {
  val v: Int = 1
  final val one = 1

  def path(x: Any): String = x match { case y: v.type => "v" + (y + 1); case _ => "other" }
  def finalPath(x: Any): String = x match { case y: one.type => "one" + (y + 1); case _ => "other" }
}
