// A type pattern with a bounded wildcard matches like the corresponding `isInstanceOf`.
object Test {
  def list(x: Any) = x match { case _: List[_ <: Int] => "list"; case _ => "other" }
  def array(x: Any) = x match { case _: Array[_ <: Int] => "array"; case _ => "other" }
  def head(x: Any) = x match { case y: List[_ <: Int] => y.headOption; case _ => None }

  def main(args: Array[String]): Unit = {
    val values: List[Any] = List(List(1, 2), List("a"), Array(1), Array("a"), "s")
    for (v <- values) {
      assert((list(v) == "list") == v.isInstanceOf[List[_ <: Int]])
      assert((array(v) == "array") == v.isInstanceOf[Array[_ <: Int]])
    }
    println(values.map(list))
    println(head(List(1, 2)))
    println(head("s"))
  }
}
