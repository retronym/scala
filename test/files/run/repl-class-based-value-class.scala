import scala.tools.nsc.Settings
import scala.tools.partest.{ Hashless, ReplTest }

object Test extends ReplTest with Hashless {
  override def transformSettings(s: Settings) = {
    s.Yreplclassbased.value  = true
    s.YreplMagicImport.value = true
    s
  }

  def code = """
    |class Meter(val value: Int) // extends AnyVal // show
    |val x = new Meter(1) // show
    |""".stripMargin
}
