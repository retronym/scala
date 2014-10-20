/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation

/** A base class for classfile annotations visible at runtime.
 *
 *  Inheriting from this trait is equivalent to adding the meta-annotation
 *  `@java.lang.annotation.Retention(java.lang.annotation.RetentionPolicy.RUNTIME)`
 *  to an annotation in Java.
 *
 *  These are stored as
 *  [[http://docs.oracle.com/javase/7/docs/technotes/guides/language/annotations.html#_top Java annotations]]]
 *  in classfiles.
 *
 *  @since 2.12
 */
trait RuntimeAnnotation extends StaticAnnotation with ConstantAnnotation
