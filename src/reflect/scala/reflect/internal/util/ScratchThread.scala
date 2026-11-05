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

/** A thread of a concurrent universe's worker pool (`-Yparallel-typer`) that holds that universe's per-thread scratch
 *  state in a field: the lookup of `SymbolTable.scratch`, made for every `phase` read, is then a type test and two
 *  loads, where a `ThreadLocal` hashes into the thread's map and checks a weak key. Other threads use the `ThreadLocal`.
 */
class ScratchThread(group: ThreadGroup, body: Runnable, name: String, stackSize: Long) extends Thread(group, body, name, stackSize) {
  /** The universe whose scratch state `scratch` is, or null. */
  @volatile var scratchOwner: AnyRef = null
  var scratch: AnyRef = null
}
