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

package scala.tools.nsc.transform.patmat

/** SELF-CHECK (throwaway, for the community build): the faster patmat analysis computes each result the old way
 *  as well, and crashes the compiler if they differ. The index thresholds are 0, so the new code runs everywhere.
 */
object PatmatSelfCheck {
  final class Failure(msg: String) extends AssertionError(s"patmat self-check failed: $msg")

  def apply(cond: Boolean, msg: => String): Unit = if (!cond) throw new Failure(msg)
}
