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
package backend.jvm

import scala.collection.mutable.ListBuffer
import scala.tools.asm
import scala.tools.asm.tree.ClassNode
import scala.util.control.NonFatal

abstract class CodeGen[G <: Global](val global: G) extends PerRunInit {
  val bTypes: BTypesFromSymbols[global.type]

  import global._
  import bTypes._
  import genBCode.{generatedClassHandler, postProcessor}

  // TODO: do we really need a new instance per run? Is there state that depends on the compiler frontend (symbols, types, settings)?
  private[this] lazy val mirrorCodeGen: LazyVar[CodeGenImpl.JMirrorBuilder] = perRunLazy(this)(new CodeGenImpl.JMirrorBuilder())

  /**
    * Generate ASM ClassNodes for classes found in a compilation unit. The resulting classes are
    * passed to the `genBCode.generatedClassHandler`.
    */
  def genUnit(unit: CompilationUnit): Unit = {
    val generatedClasses = ListBuffer.empty[GeneratedClass]

    def genClassDef(cd: ClassDef): Unit = try {
      val sym = cd.symbol
      val position = sym.pos
      val fullSymbolName = sym.javaClassName
      addSbtIClassShim(cd)
      generatedClasses += generated(fullSymbolName, position, isArtifact = false)(new CodeGenImpl.SyncAndTryBuilder(unit)) { (b, visitor) =>
        b.genPlainClass(cd, visitor)
      }
      if (bTypes.isTopLevelModuleClass(sym)) {
        if (sym.companionClass == NoSymbol) {
          generatedClasses += generated(fullSymbolName, position, isArtifact = true)(mirrorCodeGen.get) { (b, visitor) =>
            b.genMirrorClass(sym, unit, visitor)
          }
        }
        else
          log(s"No mirror class for module with linked class: ${sym.fullName}")
      }
    } catch {
      case ex: InterruptedException => throw ex
      case ex: Throwable =>
        if (settings.isDebug) ex.printStackTrace()
        globalError(s"Error while emitting ${unit.source}\n${ex.getMessage}")
    }

    def genClassDefs(tree: Tree): Unit = tree match {
      case EmptyTree => ()
      case PackageDef(_, stats) => stats foreach genClassDefs
      case cd: ClassDef => frontendAccess.frontendSynch(genClassDef(cd))
      case x            => throw new MatchError(x)
    }

    statistics.timed(statistics.bcodeGenStat) {
      genClassDefs(unit.body)
    }

    generatedClassHandler.process(GeneratedCompilationUnit(unit.source.file, generatedClasses.toList))
  }

  /** Whether nothing needs the `ClassNode` of a class, so that codegen can write it directly. With backend threads we
   *  stay with `ClassNode`s, as the work they do, such as computing stack map frames, would otherwise happen under the
   *  frontend lock. The output differs in the order of the constant pool, and in that dead code stays as ASM's
   *  `nop; athrow`.
   */
  private def emitsDirectly: Boolean =
    !settings.YbackendTree.value && !settings.opt.isSetByUser && !settings.optAddToBytecodeRepository && settings.YaddBackendThreads.value == 1

  /** A class as `gen` emits it, directly if we can. If that throws, it is generated again as a `ClassNode`, whose path
   *  has the checks and messages for a class that can't be written. A string constant that is too long, for example,
   *  only throws this early when going straight to a `ClassWriter`.
   */
  private def generated[B <: CodeGenImpl.BCPickles](sourceClassName: String, position: Position, isArtifact: Boolean)(builder: => B)(gen: (B, asm.ClassVisitor) => Unit): GeneratedClass = {
    def tree(b: B): GeneratedClass = {
      val node = new ClassNode1()
      gen(b, node)
      GeneratedClass(node.name, node, null, sourceClassName, position, isArtifact)
    }
    // TODO: do we need a new builder for each class? could we use one per run? or one per Global compiler instance?
    val b = builder
    if (!emitsDirectly) tree(b)
    else {
      b.forgetTakenPickles()
      val direct = new DirectClassVisitor(postProcessor.newClassWriter())
      try {
        gen(b, direct)
        GeneratedClass(direct.name, null, postProcessor.finishDirect(direct, isArtifact), sourceClassName, position, isArtifact)
      } catch {
        case NonFatal(ex) =>
          if (settings.isDebug) ex.printStackTrace()
          b.restorePickles()
          tree(builder)
      }
    }
  }

  def genClass(cd: ClassDef, unit: CompilationUnit): ClassNode = {
    addSbtIClassShim(cd)

    // TODO: do we need a new builder for each class? could we use one per run? or one per Global compiler instance?
    val b = new CodeGenImpl.SyncAndTryBuilder(unit)
    val node = new ClassNode1()
    b.genPlainClass(cd, node)
    node
  }

  def genMirrorClass(classSym: Symbol, unit: CompilationUnit): ClassNode = {
    val node = new ClassNode1()
    mirrorCodeGen.get.genMirrorClass(classSym, unit, node)
    node
  }


  private def addSbtIClassShim(cd: ClassDef): Unit = {
    // shim for SBT, see https://github.com/sbt/sbt/issues/2076
    // TODO put this closer to classfile writing once we have closure elimination
    // TODO create a nicer public API to find out the correspondence between sourcefile and ultimate classfiles
    currentUnit.icode += new icodes.IClass(cd.symbol)
  }

  object CodeGenImpl extends {
    val global: CodeGen.this.global.type = CodeGen.this.global
    val bTypes: CodeGen.this.bTypes.type = CodeGen.this.bTypes
  } with BCodeSyncAndTry
}
