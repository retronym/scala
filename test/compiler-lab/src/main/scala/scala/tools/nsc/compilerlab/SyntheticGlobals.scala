package scala.tools.nsc.compilerlab

import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.typechecker.Analyzer
import scala.tools.nsc.util.ClassPath

/** Distinct subclasses of `Global`, so that the compiler's call sites see several receiver classes, as they do when one
 *  JVM also runs Scaladoc, the presentation compiler or toolboxes (`c.eval` in macros), each with its own `Global`
 *  subclass. With `distinctAnalyzers`, each also has its own `Analyzer` class, as `interactive.Global` and
 *  `ScaladocGlobal` do; otherwise they share `Global`'s.
 *
 *  Each class must be written out: a `new ... with Analyzer` in shared code is one class however many globals run it.
 */
object SyntheticGlobals {
  final val Count = 8

  abstract class LabGlobal(settings: Settings, reporter: Reporter, cp: ClassPath) extends Global(settings, reporter) {
    override lazy val platform: ThisPlatform = new GlobalPlatform {
      currentClassPath = Some(cp)
    }
  }

  def apply(i: Int, distinctAnalyzers: Boolean, s: Settings, r: Reporter, cp: ClassPath): Global =
    if (distinctAnalyzers) i match {
      case 0 => new LabGlobal(s, r, cp) { g => override lazy val analyzer = new { val global: g.type = g } with Analyzer }
      case 1 => new LabGlobal(s, r, cp) { g => override lazy val analyzer = new { val global: g.type = g } with Analyzer }
      case 2 => new LabGlobal(s, r, cp) { g => override lazy val analyzer = new { val global: g.type = g } with Analyzer }
      case 3 => new LabGlobal(s, r, cp) { g => override lazy val analyzer = new { val global: g.type = g } with Analyzer }
      case 4 => new LabGlobal(s, r, cp) { g => override lazy val analyzer = new { val global: g.type = g } with Analyzer }
      case 5 => new LabGlobal(s, r, cp) { g => override lazy val analyzer = new { val global: g.type = g } with Analyzer }
      case 6 => new LabGlobal(s, r, cp) { g => override lazy val analyzer = new { val global: g.type = g } with Analyzer }
      case 7 => new LabGlobal(s, r, cp) { g => override lazy val analyzer = new { val global: g.type = g } with Analyzer }
    } else i match {
      case 0 => new LabGlobal(s, r, cp) {}
      case 1 => new LabGlobal(s, r, cp) {}
      case 2 => new LabGlobal(s, r, cp) {}
      case 3 => new LabGlobal(s, r, cp) {}
      case 4 => new LabGlobal(s, r, cp) {}
      case 5 => new LabGlobal(s, r, cp) {}
      case 6 => new LabGlobal(s, r, cp) {}
      case 7 => new LabGlobal(s, r, cp) {}
    }
}
