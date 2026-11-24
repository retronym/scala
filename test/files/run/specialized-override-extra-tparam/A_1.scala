// Compiled separately from the classes that mix these traits in.
trait TT { def ap[A, @specialized(Int, Long) B](a: A, b: B): String = "TT" }
trait TH extends TT {
  override def ap[@specialized(Long) A, @specialized(Int, Long) B](a: A, b: B): String = "TH " + a + " " + b
}
trait TI extends TH
trait TJ extends TH {
  override def ap[@specialized(Long) A, @specialized(Int, Long) B](a: A, b: B): String = "TJ " + a + " " + b
}
trait TG[X] { def ap[A, @specialized(Int) B](a: A, b: B, x: X): String = "TG" }
trait TK[X] extends TG[X] {
  override def ap[@specialized(Int) A, @specialized(Int) B](a: A, b: B, x: X): String = "TK " + a + " " + b + " " + x
}
trait TK2[X] extends TG[X] {
  override def ap[@specialized(Int) A, @specialized(Int) B](a: A, b: B, x: X): String = "TK2 " + a + " " + b + " " + x
}
trait TS[X] extends TG[X] {
  override def ap[A, @specialized(Int) B](a: A, b: B, x: X): String = "TS " + a + " " + b + " " + x
}
