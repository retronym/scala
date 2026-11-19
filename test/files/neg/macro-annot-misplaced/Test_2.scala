//> using options -Ymacro-annotations
object Test {
  @ident def ok: Int = 1

  def onExpr = (1: @ident)

  def onType: Int @ident = 1
}
