// `J` (Java) overrides the accessors of a `var`, a `val` and a `lazy val` of `S`, which Scala code
// may not do. javac does not know those rules, and `K` must not be rejected for them.
class S { var x: Int = 1; val v: Int = 1; lazy val lz: Int = 1 }

class K extends J

object Use {
  def k = { val k = new K; (k.x, k.v, k.lz) }
}
