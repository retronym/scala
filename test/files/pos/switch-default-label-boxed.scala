//> using options -Ycheck:all

// The default label of a guarded switch whose arms are boxed after erasure
object Test {
  def f(s: Int): Any = s match { case 1 => 0.5; case 2 if s > 0 => 1.0; case _ => 2.0 }
  def g(s: String): Any = s match { case "a" => 0.5; case "abc" if s.isEmpty => 1.0; case _ => 2.0 }
}
