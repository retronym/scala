/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2012, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
*/

package scala

/**
 * Annotation for specifying the `static SerialVersionUID` field
 * of a serializable class.
 */
class SerialVersionUID(value: Long) extends scala.annotation.ClassfileAnnotation
