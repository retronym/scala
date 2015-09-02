/* NSC -- new Scala compiler
 * Copyright 2005-2015 LAMP/EPFL
 * @author Martin Odersky
 */
package scala.tools.nsc.interpreter

import scala.reflect.internal.util.RangePosition
import scala.tools.nsc.backend.JavaPlatform
import scala.tools.nsc.{interactive, Settings}
import scala.tools.nsc.io._
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.util.ClassPath.DefaultJavaContext
import scala.tools.nsc.util.{DirectoryClassPath, MergedClassPath}

trait PresentationCompilation {
  self: IMain =>

  private[scala] def presentationCompile(line: String): Either[IR.Result, PresentationCompileResult] = {
    if (global == null) Left(IR.Error)
    else {
      val line1 = if (Completion.looksLikeInvocation(line)) {
        self.mostRecentVar + line
      } else line
      val compiler = newPresentationCompiler()
      val trees = compiler.newUnitParser(line1).parseStats()
      val importer = global.mkImporter(compiler)
      val request = new Request(line1, trees map (t => importer.importTree(t)), generousImports = true)
      val wrappedCode: String = request.ObjectSourceCode(request.handlers)
      val unit = compiler.newCompilationUnit(wrappedCode)
      import compiler._
      val richUnit = new RichCompilationUnit(unit.source)
      unitOfFile(richUnit.source.file) = richUnit
      enteringTyper(typeCheck(richUnit))
      val result = PresentationCompileResult(compiler)(richUnit, request.ObjectSourceCode.preambleLength + line1.length - line.length)
      Right(result)
    }
  }

  def newPresentationCompiler(): interactive.Global = {
    val replOutClasspath: DirectoryClassPath = new DirectoryClassPath(replOutput.dir, DefaultJavaContext)
    val mergedClasspath = new MergedClassPath[AbstractFile](replOutClasspath :: global.platform.classPath :: Nil, DefaultJavaContext)
    def copySettings: Settings = {
      val s = new Settings(_ => () /* ignores "bad option -nc" errors, etc */)
      s.processArguments(global.settings.recreateArgs, processAll = false)
      s.YpresentationAnyThread.value = true
      s
    }
    val storeReporter: StoreReporter = new StoreReporter
    val interactiveGlobal = new interactive.Global(copySettings, storeReporter) { self =>
      override lazy val platform: ThisPlatform = new JavaPlatform {
        val global: self.type = self

        override def classPath: PlatformClassPath = mergedClasspath
      }
    }
    new interactiveGlobal.TyperRun()
    interactiveGlobal
  }

  abstract class PresentationCompileResult {
    val compiler: scala.tools.nsc.interactive.Global
    def unit: compiler.RichCompilationUnit
    def preambleLength: Int
    def cleanup(): Unit = {
      compiler.askShutdown()
    }
    import compiler.CompletionResult

    def completionsAt(cursor: Int): CompletionResult = {
      val pos = unit.source.position(preambleLength + cursor)
      compiler.completionsAt(pos)
    }
    def typedTreeAt(code: String, selectionStart: Int, selectionEnd: Int): compiler.Tree = {
      val start = selectionStart + preambleLength
      val end   = selectionEnd + preambleLength
      val pos   = new RangePosition(unit.source, start, start, end)
      compiler.typedTreeAt(pos)
    }
  }

  object PresentationCompileResult {
    def apply(compiler0: interactive.Global)(unit0: compiler0.RichCompilationUnit, preambleLength0: Int) = new PresentationCompileResult {

      override val compiler = compiler0

      override def unit = unit0.asInstanceOf[compiler.RichCompilationUnit]

      override def preambleLength = preambleLength0
    }
  }
}
