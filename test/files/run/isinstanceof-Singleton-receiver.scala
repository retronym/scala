//> using options -nowarn
object Test {
  var n = 0
  def nextInt(): Int = { n += 1; println("nextInt"); n }
  def nextAny(): Any = { n += 1; println("nextAny"); "x" }
  def nextStr(): String = { n += 1; println("nextStr"); null }
  def main(args: Array[String]): Unit = {
    println(nextInt().isInstanceOf[Singleton])   // receiver not evaluated
    println(nextAny().isInstanceOf[Singleton])   // receiver not evaluated
    println(n)                                   // 2 expected
    println((null: Any).isInstanceOf[Singleton]) // true, although null is not an instance of anything
    // controls
    println(nextStr().isInstanceOf[Singleton])   // AnyRef receiver: evaluated, false for null
    println(nextAny().isInstanceOf[Any])         // evaluated
    println((null: Any).isInstanceOf[Any])       // false
  }
}
