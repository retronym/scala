package ann

final class tag(value: Int) extends scala.annotation.StaticAnnotation
final class tagged(value: String) extends scala.annotation.ConstantAnnotation

// annotation arguments that refer to the annotated class's own companion
@tag(Holder.N) @tagged(Holder.Name)
class Holder { @tag(Holder.N + 1) def m: Int = Holder.N }
object Holder {
  final val N = 42
  final val Name = "holder"
}
