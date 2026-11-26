// Shrunk by the reviewer from the generated program (seed 60054).
// A pattern match with a guard in the constructor of a class with a specialized field.
class C[@specialized(Int) A](val a: A) {
  val b: Int = Option(a) match { case Some(x) if x == a => 1; case Some(_) => 2; case None => 3 }
}
object Test { def main(args: Array[String]): Unit = println(new C(1).b) }
