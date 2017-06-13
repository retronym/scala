/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc.interpreter

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => ru}
import scala.tools.nsc.typechecker.TypeStrings

trait NamedParamCreator {
}

object NamedParam extends NamedParamCreator {
  def fromMonomorphicClass[T](name: String, value: T)(implicit tag: ClassTag[T]) = {
    fromCompoundMonomorphicClass(name, tag.runtimeClass :: Nil, value)
  }
  def fromCompoundMonomorphicClass(name: String, classes: Seq[Class[_]], value: Any) = {
    assert(classes.forall(_.getTypeParameters.isEmpty), classes)
    assert(classes.nonEmpty)
    new NamedParamClass(name, classes.map(c => "_root_." + c.getName).mkString(" with "), value)
  }
}

trait NamedParam {
  def name: String
  def tpe: String
  def value: Any
  override def toString = name + ": " + tpe
}

case class NamedParamClass(name: String, tpe: String, value: Any) extends NamedParam

