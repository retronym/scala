//> using options -nowarn
object Test {
  var n = 0
  def inc(): Unit = n += 1
  def last(): Null = { n += 10; null }
  def main(args: Array[String]): Unit = {
    val h1 = inc().##        // Unit receiver
    println(s"after inc().##       : n = $n, hash = $h1")
    val h2 = inc().hashCode  // same receiver, hashCode instead of ##
    println(s"after inc().hashCode : n = $n")
    val h3 = last().##       // Null receiver
    println(s"after last().##      : n = $n, hash = $h3")
    val h4 = (last(): String).##
    println(s"after (last(): String).##: n = $n, hash = $h4")
  }
}
