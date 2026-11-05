/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package ast.parser

import javac._

/** The reporter of a phase that runs its units at the same time and issues their diagnostics in the order of the
 *  units (`-Yparallel-typer`, `ParallelTyper.parallelPhase`).
 */
trait UnitOrderReporter {
  /** Has the phase reported an error in a unit before the current one, or in the current one so far? Thrown as
   *  `RetryInUnitOrder` while the units before it may still be running: the unit is then processed again, from the
   *  start, after them.
   */
  def hasErrorsInUnitOrder: Boolean
  /** Is the current unit being processed again? */
  def isRetryInUnitOrder: Boolean
}
object RetryInUnitOrder extends scala.util.control.ControlThrowable

/** The compiler sub-component that defines the parser phase, which converts source code into Trees.
 */
abstract class SyntaxAnalyzer extends SubComponent with Parsers with MarkupParsers with Scanners with JavaParsers with JavaScanners {
  import global._

  val phaseName = "parser"
  def newPhase(prev: Phase): StdPhase = new ParserPhase(prev)

  abstract class MemberDefTraverser extends Traverser {
    def onMember(defn: MemberDef): Unit

    private var depth: Int = 0
    private def lower[T](body: => T): T = {
      depth += 1
      try body finally depth -= 1
    }
    def currentDepth = depth

    /** Prune this tree and all trees beneath it. Can be overridden. */
    def prune(md: MemberDef): Boolean = (
         md.mods.isSynthetic
      || md.mods.isParamAccessor
      || nme.isConstructorName(md.name)
      || md.name.containsName(nme.ANON_CLASS_NAME)
    )

    override def traverse(t: Tree): Unit = t match {
      case md: MemberDef if prune(md) =>
      case PackageDef(_, stats)       => traverseTrees(stats)
      case md: ImplDef                => onMember(md) ; lower(traverseTrees(md.impl.body))
      case md: ValOrDefDef            => onMember(md) ; lower(traverse(md.rhs))
      case _                          => super.traverse(t)
    }
  }

  class MemberPosReporter(unit: CompilationUnit) extends MemberDefTraverser {
    private var outputFn: MemberDef => String = outputForScreen
    val path = unit.source.file.path

    // If a single line, outputs the line; if it spans multiple lines
    // outputs NN,NN with start and end lines, e.g. 15,25.
    def outputPos(md: MemberDef): String = {
      val pos   = md.pos
      val start = pos.focusStart.line
      val end   = pos.focusEnd.line

      if (start == end) "" + start else s"$start,$end"
    }
    def outputForSed(md: MemberDef): String = {
      val pos_s = "%-12s" format outputPos(md) + "p"
      s"$pos_s $path    # ${md.keyword} ${md.name}"
    }
    def outputForScreen(md: MemberDef): String = {
      val pos_s = "%-20s" format " " * currentDepth + outputPos(md)
      s"$pos_s ${md.keyword} ${md.name}"
    }

    def onMember(md: MemberDef) = println(outputFn(md))
    // It recognizes "sed" and "anything else".
    def show(style: String): Unit = {
      if (style == "sed") {
        outputFn = outputForSed
        traverse(unit.body)
      }
      else {
        outputFn = outputForScreen
        println(path)
        traverse(unit.body)
      }
      println("")
    }
  }

  private def initialUnitBody(unit: CompilationUnit): Tree = {
    if (unit.isJava) newJavaUnitParser(unit).parse()
    else if (currentRun.parsing.incompleteHandled) newUnitParser(unit).parse()
    else newUnitParser(unit).smartParse()
  }

  class ParserPhase(prev: Phase) extends StdPhase(prev) {
    override val checkable = false
    override val keepsTypeParams = false

    // -Yparallel-typer with -Dscalac.parallelTyper.parallelParser: a unit per task on the parallel typer's pool.
    // What a parser writes is its own or its unit's: the scanner, the tree builder, the unit's fresh names, its
    // suspended warnings. Shared: the name table (its own lock), the positions (assigned per thread), the reporter
    // (buffered, issued in the order of the units, which is the serial order), the symbols of `definitions` that the
    // parsers ask for (below).
    private def inParallel: Boolean =
      settings.YparallelTyper.value && System.getProperty("scalac.parallelTyper.parallelParser") != null &&
        global.analyzer.parallelPhasesSupported &&
        !settings.Ymemberpos.isSetByUser &&      // prints as it goes
        !currentRun.parsing.incompleteHandled    // the REPL's handler

    override def run(): Unit = {
      val units = currentRun.units.toList
      if (units.lengthCompare(1) > 0 && inParallel) {
        global.echoPhaseSummary(this)
        forceParserDefinitions()
        runReporting.suspendInOrderOf(units.map(_.source)) {
          global.analyzer.parallelPhase(this, units, "parser", unitOrder = true)(applyPhase(_))
        }
      }
      else super.run()
    }

    /** The lazy vals of `definitions` that every parser reads, forced before the workers start: a lazy val is
     *  initialised under the monitor of `definitions`, and one whose initialiser completes a symbol would take the
     *  completion lock inside it, the reverse of the order of a completion that reads a lazy val. These are symbols
     *  of the root classes, which `definitions.init` has completed, so that forcing them here changes nothing a
     *  serial run would see. The few that can complete a class of the classpath or of the source path (`Predef`,
     *  `Nil`, `String`, the Java annotations) are read under the completion lock where the parsers use them.
     */
    private def forceParserDefinitions(): Unit = {
      import definitions._
      def force(sym: => Any): Unit = try sym catch { case _: FatalError => () } // missing: reported where it is used
      force(AnyTpe); force(ObjectClass); force(TupleClass)
      force(UnitTpe); force(BooleanTpe); force(ByteTpe); force(ShortTpe); force(CharTpe); force(IntTpe); force(LongTpe)
      force(FloatTpe); force(DoubleTpe)
    }

    def apply(unit: CompilationUnit): Unit = {
      informProgress("parsing " + unit)
      // if the body is already filled in, don't overwrite it
      // otherwise compileLate is going to overwrite bodies of synthetic source files
      if (unit.body == EmptyTree) {
        reporter match {
          case r: UnitOrderReporter if r.isRetryInUnitOrder =>
            // parsed again after the units before it (`RetryInUnitOrder`): nothing of the first attempt remains
            unit.fresh.reset()
            runReporting.clearSuspendedMessages(unit.source)
          case _ =>
        }
        unit.body = initialUnitBody(unit)
      }

      if (settings.Ymemberpos.isSetByUser)
        new MemberPosReporter(unit).show(style = settings.Ymemberpos.value)
    }
  }
}
