package pneg

// parser warnings only: they are suspended until the typer, which this corpus never reaches
class A01Warn {
  def proc() { println("procedure syntax") }
  val sym = 'aSymbol
  val long = 1l
  def arrow(x: Int) = List(x).map(y ⇒ y + 1)
  def then = 1
}
