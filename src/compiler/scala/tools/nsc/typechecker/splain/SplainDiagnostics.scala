/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package typechecker
package splain

import scala.util.control.NonFatal

trait SplainDiagnostics
extends SplainFormatting
{ self: Analyzer with SplainData =>
  import global._

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
    if (!settings.splainSettingEnable || settings.splainSettingNoFoundReq) None
    else foundReqMsgShort(found, req).map(a => ";\n" + a.indent.joinLines)
}
