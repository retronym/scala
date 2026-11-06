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

package scala.reflect.internal.util

import scala.reflect.internal.{Phase, SymbolTable}

/** A thread of a concurrent universe's worker pool (`-Yparallel-typer`) that holds that universe's per-thread state in
 *  its own fields: a read of `phase` or `currentPeriod` by a worker is then a type test, a comparison with the owner and
 *  a load, on one object. Other threads use a `ThreadLocal`.
 *
 *  Final for the sake of those reads, whose type test is then one comparison of the class word. They are spelled out in
 *  `SymbolTable` so that they stay within HotSpot's limit for inlining where a call is not hot (35 bytes of bytecode).
 *
 *  Plain fields: only this thread reads and writes them.
 */
final class ScratchThread(group: ThreadGroup, body: Runnable, name: String, stackSize: Long) extends Thread(group, body, name, stackSize) {
  /** The universe whose state these fields hold, or null. */
  var scratchOwner: AnyRef = null
  var scratch: SymbolTable#Scratch = null
  /** This thread's phase and period, as in `scratch`. `SymbolTable.phase_=` writes both copies. */
  var ph: Phase = null
  var per: Int = 0
}
