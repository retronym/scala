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

import scala.tools.asm.{Handle, Label, MethodVisitor, Opcodes}
import scala.tools.asm.tree.{MethodInsnNode, MethodNode}

/** A label made by codegen, which knows whether a jump to it has been emitted so far. */
final class CodegenLabel extends Label {
  var isJumpTarget: Boolean = false
}

/** What codegen goes through to emit a method, and what it asks about the code emitted so far. Everything
 *  it needs to know is kept here, so that nothing has to be read back from what is underneath.
 *
 *  @param tree the method node underneath, or `null` if it is not a tree. Only for what the optimizer needs.
 */
final class MethodEmitter(underlying: MethodVisitor, val tree: MethodNode) extends MethodVisitor(Opcodes.ASM9, underlying) {
  import Opcodes._

  private[this] var pendingLine = -1
  private[this] var pendingLineStart: Label = null

  private[this] var _lastOpcode = -1
  private[this] var _lastLabel: Label = null

  private[this] var unreachable = false
  private[this] var unreachableBeforeLastLabel = false

  /** Emitted immediately before each `RETURN`. */
  var beforeReturn: () => Unit = null

  /** The opcode of the last thing emitted, `-1` if that was a label or line number. */
  def lastOpcode: Int = _lastOpcode

  /** The last thing emitted if that was a label, otherwise `null`. */
  def lastLabel: Label = _lastLabel

  /** True if control cannot reach the current position, so anything emitted here would be dead.
   *  A `false` result is not a proof of reachability.
   */
  def isAtUnreachablePoint: Boolean = unreachable

  /** The last label is only ever jumped to from earlier in the method, so nothing but those jumps reach it. */
  def lastLabelIsJoinPoint(): Unit =
    unreachable = unreachableBeforeLastLabel && !(_lastLabel match {
      case l: CodegenLabel => l.isJumpTarget
      case _ => false
    })

  /** The last label may be reached in a way this class doesn't see, for example as a handler entry, or by a later jump. */
  def lastLabelIsReachable(): Unit = unreachable = false

  /** Replaces the line number of the last thing emitted, if that was a line number. */
  def overwriteLineNumber(line: Int): Boolean =
    pendingLine != -1 && { pendingLine = line; true }

  /** The call just emitted, when there is a tree. */
  def lastCall: MethodInsnNode =
    if (tree == null || pendingLine != -1) null
    else tree.instructions.getLast match {
      case m: MethodInsnNode => m
      case _ => null
    }

  private def flushLine(): Unit = if (pendingLine != -1) {
    super.visitLineNumber(pendingLine, pendingLineStart)
    pendingLine = -1
    pendingLineStart = null
  }

  private def before(opcode: Int): Unit = {
    flushLine()
    _lastOpcode = opcode
    _lastLabel = null
    unreachable = opcode == GOTO || opcode == ATHROW || opcode == TABLESWITCH || opcode == LOOKUPSWITCH || (opcode >= IRETURN && opcode <= RETURN)
  }

  private def jumpsTo(label: Label): Unit = label.asInstanceOf[CodegenLabel].isJumpTarget = true

  /** To end a method that has code. */
  def endCode(): Unit = {
    flushLine()
    super.visitMaxs(0, 0)
  }

  override def visitLabel(label: Label): Unit = {
    flushLine()
    unreachableBeforeLastLabel = unreachable
    unreachable = false
    _lastOpcode = -1
    _lastLabel = label
    super.visitLabel(label)
  }

  override def visitLineNumber(line: Int, start: Label): Unit = {
    flushLine()
    pendingLine = line
    pendingLineStart = start
    _lastOpcode = -1
    _lastLabel = null
  }

  override def visitInsn(opcode: Int): Unit = {
    if (opcode == RETURN && beforeReturn != null) beforeReturn()
    before(opcode)
    super.visitInsn(opcode)
  }

  override def visitIntInsn(opcode: Int, operand: Int): Unit = {
    before(opcode)
    super.visitIntInsn(opcode, operand)
  }

  override def visitVarInsn(opcode: Int, varIndex: Int): Unit = {
    before(opcode)
    super.visitVarInsn(opcode, varIndex)
  }

  override def visitTypeInsn(opcode: Int, tpe: String): Unit = {
    before(opcode)
    super.visitTypeInsn(opcode, tpe)
  }

  override def visitFieldInsn(opcode: Int, owner: String, name: String, descriptor: String): Unit = {
    before(opcode)
    super.visitFieldInsn(opcode, owner, name, descriptor)
  }

  override def visitMethodInsn(opcode: Int, owner: String, name: String, descriptor: String, isInterface: Boolean): Unit = {
    before(opcode)
    super.visitMethodInsn(opcode, owner, name, descriptor, isInterface)
  }

  override def visitInvokeDynamicInsn(name: String, descriptor: String, bootstrapMethodHandle: Handle, bootstrapMethodArguments: Object*): Unit = {
    before(INVOKEDYNAMIC)
    super.visitInvokeDynamicInsn(name, descriptor, bootstrapMethodHandle, bootstrapMethodArguments: _*)
  }

  override def visitJumpInsn(opcode: Int, label: Label): Unit = {
    before(opcode)
    jumpsTo(label)
    super.visitJumpInsn(opcode, label)
  }

  override def visitLdcInsn(value: Object): Unit = {
    before(LDC)
    super.visitLdcInsn(value)
  }

  override def visitIincInsn(varIndex: Int, increment: Int): Unit = {
    before(IINC)
    super.visitIincInsn(varIndex, increment)
  }

  override def visitTableSwitchInsn(min: Int, max: Int, dflt: Label, labels: Label*): Unit = {
    before(TABLESWITCH)
    jumpsTo(dflt)
    labels.foreach(jumpsTo)
    super.visitTableSwitchInsn(min, max, dflt, labels: _*)
  }

  override def visitLookupSwitchInsn(dflt: Label, keys: Array[Int], labels: Array[Label]): Unit = {
    before(LOOKUPSWITCH)
    jumpsTo(dflt)
    labels.foreach(jumpsTo)
    super.visitLookupSwitchInsn(dflt, keys, labels)
  }

  override def visitMultiANewArrayInsn(descriptor: String, numDimensions: Int): Unit = {
    before(MULTIANEWARRAY)
    super.visitMultiANewArrayInsn(descriptor, numDimensions)
  }
}
