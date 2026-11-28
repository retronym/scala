object Test {
  def sm(s: S): Int = s match { case _: S.A => 0; case _: S.B => 1 } // D is missing
  def main(args: Array[String]): Unit = println(s"${sm(new S.B)} ${scala.util.Try(sm(new S.D)).isSuccess}")
}
