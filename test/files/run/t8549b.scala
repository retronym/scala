
@SerialVersionUID(42)
class C

object Test extends App {
  val id = classOf[C].getDeclaredField("serialVersionUID").get(null)
  assert(id == 42, id)
}
