//> using options -Werror
// A constant `if` reduced inside a block that is a match scrutinee. The block keeps its type, so the match
// is translated against it; -Ycheck re-types the block from its narrower last expression and fails here.
object Config {
  final val Enabled = false
  final val Quiet = true
}

object Test {
  def inBlock(s: String): Int = { val y = s.length; if (Config.Enabled) y else "s" } match {
    case "s" => 0
    case 1   => 1
    case _   => 2
  }
  def nullInBlock(s: String): Int = { val y = s; if (Config.Quiet) null else y } match {
    case "a"  => 1
    case null => 0
    case _    => 2
  }

  def main(args: Array[String]): Unit = println(List(inBlock("b"), nullInBlock("b")))
}
