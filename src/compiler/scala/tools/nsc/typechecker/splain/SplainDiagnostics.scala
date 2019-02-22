package scala.tools.nsc
package typechecker
package splain

import scala.util.control.NonFatal

trait SplainDiagnostics
extends SplainFormatting
{ self: Analyzer with SplainData =>
  import global._

  def splainSettingNoFoundReq: Boolean =
    settings.YsplainNoFoundReq.value

  def showStats[A](desc: String, run: => A): A = {
    val ret = run
    if (sys.env.contains("SPLAIN_CACHE_STATS"))
      reporter.echo(s"$desc entries/hits: $cacheStats")
    ret
  }

  def foundReqMsgShort(found: Type, req: Type): Option[TypeRepr] =
    try {
      Some(showStats("foundreq", showFormattedL(formatDiff(found, req, true), true)))
    } catch {
      case NonFatal(e) =>
        None
    }

  def splainFoundReqMsg(found: Type, req: Type): Option[String] =
    if (!splainSettingEnable || splainSettingNoFoundReq) None
    else foundReqMsgShort(found, req).map(a => ";\n" + a.indent.joinLines)
}
