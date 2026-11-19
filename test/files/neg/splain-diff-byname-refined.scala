//> using options -Vtype-diffs
object Test {
  trait A

  def byName: (=> Int) => String = (null: (=> String) => String)

  def renamed: A { def y: Int } = (null: A { def x: Int })

  def missing: A { def y: String; def x: Int } = (null: A { def x: Int; def z: Int })
}
