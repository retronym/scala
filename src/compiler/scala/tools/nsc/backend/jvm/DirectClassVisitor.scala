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

import java.util.{ArrayList, HashSet}

import scala.collection.mutable
import scala.tools.asm.{AnnotationVisitor, ClassVisitor, ClassWriter, FieldVisitor, Handle, MethodVisitor, Opcodes, Type}
import scala.tools.nsc.backend.jvm.analysis.BackendUtils

/** Passes a class straight to `writer` when no `ClassNode` is needed, recording on the way what would otherwise
 *  be found by walking the node: the serializable lambdas, for `$deserializeLambda$`, and what
 *  `NestedClassesCollector` looks at, for the `InnerClasses` attribute.
 *
 *  The references are resolved once the class is finished, because a name that appears in a signature only needs
 *  to have a `ClassBType` by the time the class is finished. Only a name containing a `$` can be a nested class,
 *  which most aren't, and most of the others recur.
 */
final class DirectClassVisitor(val writer: ClassWriter) extends ClassVisitor(Opcodes.ASM9, writer) {
  private[this] var _name: String = null
  private[this] var _classSignature: String = null
  val internalNames = new HashSet[String]
  val descriptors = new HashSet[String]
  val fieldSignatures = new ArrayList[String]
  val methodSignatures = new ArrayList[String]
  val serializableLambdas: mutable.TreeSet[Handle] = mutable.TreeSet.empty[Handle](BackendUtils.handleOrdering)

  def name: String = _name
  def classSignature: String = _classSignature

  /** An internal name, or the array descriptor that can take the place of one in an instruction. */
  def internalName(name: String): Unit =
    if (name != null && name.indexOf('$') >= 0) internalNames.add(name)

  def descriptor(desc: String): Unit =
    if (desc.indexOf('$') >= 0) descriptors.add(desc)

  def constant(value: Object): Unit = value match {
    case t: Type => descriptor(t.getDescriptor)
    case _ =>
  }

  def handle(h: Handle): Unit = {
    internalName(h.getOwner)
    descriptor(h.getDesc)
  }

  /** As `NestedClassesCollector`, the type of an annotation and the classes among its direct arguments. */
  def annotation(desc: String, visitor: AnnotationVisitor): AnnotationVisitor = {
    descriptor(desc)
    new AnnotationVisitor(Opcodes.ASM9, visitor) {
      override def visit(name: String, value: Object): Unit = {
        constant(value)
        super.visit(name, value)
      }
    }
  }

  override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit = {
    _name = name
    _classSignature = signature
    internalName(superName)
    var i = 0
    while (i < interfaces.length) { internalName(interfaces(i)); i += 1 }
    super.visit(version, access, name, signature, superName, interfaces)
  }

  override def visitOuterClass(owner: String, name: String, descriptor: String): Unit = {
    internalName(owner)
    super.visitOuterClass(owner, name, descriptor)
  }

  override def visitAnnotation(descriptor: String, visible: Boolean): AnnotationVisitor =
    annotation(descriptor, super.visitAnnotation(descriptor, visible))

  override def visitField(access: Int, name: String, desc: String, signature: String, value: Object): FieldVisitor = {
    descriptor(desc)
    if (signature != null) fieldSignatures.add(signature)
    new FieldVisitor(Opcodes.ASM9, super.visitField(access, name, desc, signature, value)) {
      override def visitAnnotation(descriptor: String, visible: Boolean): AnnotationVisitor =
        annotation(descriptor, super.visitAnnotation(descriptor, visible))
    }
  }

  override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
    descriptor(desc)
    if (signature != null) methodSignatures.add(signature)
    if (exceptions != null) {
      var i = 0
      while (i < exceptions.length) { internalName(exceptions(i)); i += 1 }
    }
    new MethodEmitter(super.visitMethod(access, name, desc, signature, exceptions), null, this)
  }
}
