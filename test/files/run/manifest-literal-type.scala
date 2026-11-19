// A Manifest cannot represent a literal type; the manifest of the widened type is used,
// consistent with `classTag`. Previously these overflowed the stack in the compiler.
import scala.reflect.classTag

object Test {
  def f[T: Manifest] = manifest[T]
  def g[T](implicit m: Manifest[T]) = m
  type F[X] = X
  final val c = 1

  def main(args: Array[String]): Unit = {
    println(manifest[1])
    println(manifest["s"])
    println(manifest[true])
    println(f[1])
    println(optManifest[1])
    println(manifest[List[1]])
    println(manifest[Array[1]])
    println(g[Int => 1])
    println(manifest[F[1]])
    println(manifest[Option[_ >: 1 <: Int]])
    println(manifest[c.type])
    println(manifest[1] == manifest[Int])
    println(manifest[1].runtimeClass == classTag[1].runtimeClass)
  }
}
