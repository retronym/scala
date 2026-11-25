//> using options -nowarn
import java.io._
case object CO { def readResolve(): AnyRef = { println("user readResolve CO"); "replaced" } }
object PO extends Serializable { def readResolve(): AnyRef = { println("user readResolve PO"); "replaced" } }
trait RR { def readResolve(): AnyRef = { println("trait readResolve"); "replaced" } }
case object CT extends RR
object Test {
  def roundTrip(o: AnyRef): AnyRef = {
    val b = new ByteArrayOutputStream
    new ObjectOutputStream(b).writeObject(o)
    new ObjectInputStream(new ByteArrayInputStream(b.toByteArray)).readObject()
  }
  def main(args: Array[String]): Unit = {
    println(roundTrip(CO) == "replaced")
    println(roundTrip(PO) == "replaced")
    println(roundTrip(CT) == "replaced")
  }
}
