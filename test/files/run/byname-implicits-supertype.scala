// A by-name implicit argument of a supertype of the type of an open implicit search is not a
// recursive reference to that search.
trait Encoder[A] { def enc(a: A): String }
trait Decoder[A] { def dec(s: String): A }
object Encoder { implicit val int: Encoder[Int] = new Encoder[Int] { def enc(a: Int) = "i" + a } }
object Decoder { implicit val int: Decoder[Int] = new Decoder[Int] { def dec(s: String) = s.drop(1).toInt } }
trait Codec[A] extends Encoder[A] with Decoder[A]
object Codec {
  implicit def fromParts[A](implicit e: => Encoder[A], d: => Decoder[A]): Codec[A] = new Codec[A] {
    def enc(a: A) = e.enc(a)
    def dec(s: String) = d.dec(s)
  }
}

trait Super { def name: String }
object Super { implicit val sup: Super = new Super { def name = "sup" } }
trait Sub extends Super
object Sub { implicit def sub(implicit s: => Super): Sub = new Sub { def name = "sub(" + s.name + ")" } }

object Test {
  def main(args: Array[String]): Unit = {
    val c = implicitly[Codec[Int]]
    println(c.enc(1))
    println(c.dec("i2"))
    println(implicitly[Sub].name)
  }
}
