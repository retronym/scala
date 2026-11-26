object Test {
  def f: String = (if (false) -1 else ???) match { case 3 => "b"; case _ => "c" }
  def main(args: Array[String]): Unit =
    println(try f catch { case e: NotImplementedError => "NotImplementedError" })
}
