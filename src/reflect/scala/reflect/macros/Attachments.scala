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

package scala
package reflect
package macros

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  Attachments provide a way to associate custom metadata with symbols and trees.
 *
 *  Along with `symbol` and `tpe`, which represent core metadata of trees, each tree
 *  carries the `attachments` field that can store other metadata: compiler-defined (e.g. positions) or user-defined.
 *  Same story is true for symbols, which also have extensible metadata by the virtue
 *  of the same `attachments` field.
 *
 *  Typically attachments just store a [[scala.reflect.api.Position]], but they can be extended to
 *  encompass arbitrary payloads. Payloads are stored in type-indexed slots, which can be read with `get[T]` and written
 *  with `update[T]` and `remove[T]`.
 *
 *  This API doesn't have much use in the runtime reflection API (the [[scala.reflect.api]] package), but it might be of help
 *  for macro writers, providing a way to coordinate multiple macros operating on the same code. Therefore the `attachments`
 *  field is only declared in trees and symbols belonging to [[scala.reflect.macros.Universe]].
 */
abstract class Attachments { self =>

  /** The position type of this attachment */
  type Pos >: Null

  /** The type of a similar attachments object. */
  type This = Attachments { type Pos = self.Pos }

  /** The underlying position */
  def pos: Pos

  /** Creates a copy of this attachment with the position replaced by `newPos` */
  def withPos(newPos: Pos): This

  /** The underlying payload with the guarantee that no two elements have the same type. */
  def all: Set[Any]

  /** An underlying payload of the given class type `T`. */
  def get[T: ClassTag]: Option[T]

  /** Check underlying payload contains an instance of type `T`. */
  def contains[T: ClassTag]: Boolean

  /** Creates a copy of this attachment with the payload slot of T added/updated with the provided value.
   *  Replaces an existing payload of the same type, if exists.
   */
  def update[T: ClassTag](attachment: T): This

  /** Creates a copy of this attachment with the payload of the given class type `T` removed. */
  def remove[T: ClassTag]: This

  def isEmpty: Boolean

  def copy(): This
}

// OPT: specialize for the common case of one attachment
// when compiling library/reflect/compiler, about 1.1% of trees/symbols receive
// any attachments; fewer than 0.1% receive more than that.

private[reflect] abstract class EmptyAttachments extends Attachments {
  final def all: Set[Any] = Set.empty
  final def get[T: ClassTag]: Option[T] = None
  final def contains[T: ClassTag]: Boolean = false
  final def update[T: ClassTag](attachment: T): This =
    new OneAttachment[Pos](pos, attachment.asInstanceOf[AnyRef])
  final def remove[T: ClassTag]: This = this
  final def isEmpty: Boolean = true
  final def copy(): This = this
}

private final class OneAttachment[P >: Null](val pos: P, private[this] var att: AnyRef) extends Attachments {
  type Pos = P
  def withPos(newPos: Pos): This =
    if (att eq null) newPos.asInstanceOf[This]
    else new OneAttachment[P](newPos, att)
  def all: Set[Any] = Set(att)
  def get[T](implicit T: ClassTag[T]): Option[T] =
    if (contains[T]) Some(att.asInstanceOf[T]) else None
  def contains[T](implicit T: ClassTag[T]): Boolean =
    T.runtimeClass isInstance att
  def update[T](att1: T)(implicit T: ClassTag[T]): This = {
    if (T.runtimeClass isInstance att) { att = att1.asInstanceOf[AnyRef]; this }
    else new ManyAttachments[P](pos, att1 :: att :: Nil)
  }
  def remove[T](implicit T: ClassTag[T]): This = {
    if (T.runtimeClass isInstance att) att = null
    this
  }
  def isEmpty: Boolean = att eq null
  def copy(): This = withPos(pos)
}

private final class ManyAttachments[P >: Null](val pos: P, private[this] var atts: List[Any]) extends Attachments {
  type Pos = P
  def withPos(newPos: Pos): This =
    if (atts.isEmpty) newPos.asInstanceOf[This]
    else new ManyAttachments[P](newPos, atts)
  def all: Set[Any] = atts.toSet
  def get[T](implicit T: ClassTag[T]): Option[T] =
    atts.find(T.runtimeClass.isInstance).asInstanceOf[Option[T]]
  def contains[T](implicit T: ClassTag[T]): Boolean =
    atts.exists(T.runtimeClass.isInstance)
  def update[T](att1: T)(implicit T: ClassTag[T]): This = {
    atts = att1 :: atts.filterNot(T.runtimeClass.isInstance)
    this
  }
  def remove[T](implicit T: ClassTag[T]): This = {
    atts = atts.filterNot(T.runtimeClass.isInstance)
    this
  }
  def isEmpty: Boolean = atts.isEmpty
  def copy(): This = withPos(pos)
}
