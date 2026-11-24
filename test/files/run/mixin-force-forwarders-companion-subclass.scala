//> using options -Xmixin-force-forwarders:false
trait T { def m(): String = "T.m" }
class C
object C extends C with T
object Test {
  def main(args: Array[String]): Unit = println(C.m())
}
