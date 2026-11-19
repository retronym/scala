//> using options -Ycheck:typer,patmat
//> using filter "^\[No[tw] check"
//
// Re-typing in place under -Ycheck:typer or -Ycheck:patmat turned the non-local `return`
// in a partial function literal into a return from its `applyOrElse`, so every method returned 0.
object Test {
  var cnt = 0
  // local def g; return in PF nested in lambda
  def a(): Int = { def g(): Int = { List(1, 2).foreach { x => val z: Int = ({ case u: Int => { if (x == 2) return 13; 1 } }: PartialFunction[Int, Int]).apply(1); cnt += z }; 0 }; g() }
  // same but method-level (no local def)
  def b(): Int = { List(1, 2).foreach { x => val z: Int = ({ case u: Int => { if (x == 2) return 13; 1 } }: PartialFunction[Int, Int]).apply(1); cnt += z }; 0 }
  // local def, no lambda nesting
  def c(): Int = { def g(): Int = { val z: Int = ({ case u: Int => { if (cnt >= 0) return 13; 1 } }: PartialFunction[Int, Int]).apply(1); cnt += z; 0 }; g() }
  // local def, Function1 instead of PF
  def d(): Int = { def g(): Int = { List(1, 2).foreach { x => val z: Int = ((u: Int) => { if (x == 2) return 13; 1 }).apply(1); cnt += z }; 0 }; g() }
  def main(args: Array[String]): Unit = { println(s"a=${a()} b=${b()} c=${c()} d=${d()}") }
}
