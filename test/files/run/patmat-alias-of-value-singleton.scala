// A type alias of a singleton type is tested like the singleton type it stands for.
object Test {
  val v: Int = 1
  type A = v.type
  val s: String = new String("s")
  type S = s.type
  object O
  type OT = O.type

  def int(a: Any) = a match { case x: A => "v " + (x + 41); case _ => "no" }
  def str(a: Any) = a match { case x: S => "s " + x; case _ => "no" }
  def obj(a: Any) = a match { case _: OT => "O"; case _ => "no" }

  def main(args: Array[String]): Unit = {
    println(List(int(1), int(2), int("1")).mkString(", "))
    println(List(str(s), str(new String("s")), str(1)).mkString(", "))
    println(List(obj(O), obj(1)).mkString(", "))
  }
}
