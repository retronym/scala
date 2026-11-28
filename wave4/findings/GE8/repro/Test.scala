// Shrunk by the reviewer from the generated program (seed 60211).
// Two specialized type parameters; B occurs in m only under a by-name parameter.
trait AC[@specialized(Int) A, @specialized(Int) B] {
  def m(q: => B): A
}
object Test { def main(args: Array[String]): Unit = () }
