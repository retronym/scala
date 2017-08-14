/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package jvm

import scala.reflect.internal.util.Statistics
import scala.tools.asm.Opcodes

abstract class GenBCode extends SubComponent {
  self =>
  import global._

  val postProcessorFrontendAccess: PostProcessorFrontendAccess = new PostProcessorFrontendAccess.PostProcessorFrontendAccessImpl(global)

  val bTypes: BTypesFromSymbols[global.type] = new { val frontendAccess = postProcessorFrontendAccess } with BTypesFromSymbols[global.type](global)

  val codeGen: CodeGen[global.type] = new { val bTypes: self.bTypes.type = self.bTypes } with CodeGen[global.type](global)

  val postProcessor: PostProcessor = new { val bTypes: self.bTypes.type = self.bTypes } with PostProcessor

  val phaseName = "jvm"

  override def newPhase(prev: Phase) = new BCodePhase(prev)

  class BCodePhase(prev: Phase) extends StdPhase(prev) {
    override def description = "Generate bytecode from ASTs using the ASM library"

    override val erasedTypes = true

    private val writeClassesEarly = postProcessorFrontendAccess.compilerSettings.optNone

    def apply(unit: CompilationUnit): Unit = {
      val genStart = Statistics.startTimer(BackendStats.bcodeGenStat)
      codeGen.genUnit(unit)
      Statistics.stopTimer(BackendStats.bcodeGenStat, genStart)
      if (writeClassesEarly) {
        postProcessor.postProcessAndSendToDisk()
        postProcessor.generatedClasses.clear()
      }

    }

    override def run(): Unit = {
      val bcodeStart = Statistics.startTimer(BackendStats.bcodeTimer)
      try {
        initialize()

        super.run() // invokes `apply` for each compilation unit

        if (!writeClassesEarly) {
          postProcessor.runGlobalOptimizations()
          postProcessor.postProcessAndSendToDisk()
        }
      } finally {
        postProcessor.classfileWriter.get.close()
        Statistics.stopTimer(BackendStats.bcodeTimer, bcodeStart)
      }
    }

    /**
     * Several backend components have state that needs to be initialized in each run, because
     * it depends on frontend data that may change between runs: Symbols, Types, Settings.
     */
    private def initialize(): Unit = {
      val initStart = Statistics.startTimer(BackendStats.bcodeInitTimer)
      scalaPrimitives.init()
      bTypes.initialize()
      codeGen.initialize()
      postProcessorFrontendAccess.initialize()
      postProcessor.initialize()
      Statistics.stopTimer(BackendStats.bcodeInitTimer, initStart)
    }
  }
}

object GenBCode {
  def mkFlags(args: Int*) = args.foldLeft(0)(_ | _)

  final val PublicStatic = Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC
  final val PublicStaticFinal = Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC | Opcodes.ACC_FINAL

  val CLASS_CONSTRUCTOR_NAME = "<clinit>"
  val INSTANCE_CONSTRUCTOR_NAME = "<init>"
}
