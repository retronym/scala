/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.annotation

/**
 * An annotation to be applied to a match expression.  If present,
 * the compiler will generated a separate method for the body of each case.
 *
 * Intended to break large pattern matches into smaller, JIT friendly bytecode.
 *
 * @author   Jason Zaugg
 * @since    2.10
 */
final class methodPerCase extends annotation.StaticAnnotation
