object Test {
  val x = ""
  def single[T <: Singleton](x : T) = x
  single(x)
  single("const")
}