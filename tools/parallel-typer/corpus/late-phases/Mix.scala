package lp

trait Base0 { def name: String = "base"; def log(s: String): String = s }
trait WithVal extends Base0 {
  val tag: String = "tag"; var count: Int = 0; lazy val slow: String = name + tag
  private val hidden = 42
  protected val prot = hidden + 1
  object InTrait { val x = hidden }
  override def name: String = "val:" + super.name
  def secret = hidden + prot
  final val constant = 3
}
trait Stack1 extends Base0 { override def log(s: String): String = "1" + super.log(s) }
trait Stack2 extends Base0 { override def log(s: String): String = "2" + super.log(s) }
trait AbsOverride extends Base0 { abstract override def name: String = "ao:" + super.name }
trait LazyMany { lazy val a = 1; lazy val b = a + 1; lazy val c = b + 1; lazy val d = c + 1; lazy val e = d + 1; lazy val f = e + 1; lazy val g = f + 1; lazy val h = g + 1; lazy val i = h + 1 }
trait SelfT { self: Base0 => def viaSelf = self.name }
trait InitOrder { val first = 1; val second = first + 1; def third = second + 1 }
trait WithPrivate { private def p = 1; private[this] var q = 2; def pq = { q += 1; p + q } ; private[lp] def pkg = pq }
