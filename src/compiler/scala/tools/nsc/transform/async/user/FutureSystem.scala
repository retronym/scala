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

package scala.tools.nsc.transform.async.user

import scala.tools.nsc.Global

/**
 * Used by the macro implementations in [[scala.tools.nsc.transform.async.AsyncPhase]] to
 * customize the code generation.
 */
abstract class FutureSystem {
  def awaitSymbol(global: Global): global.Symbol

  /** A hook for custom macros to transform the tree post-ANF transform */
  def postAnfTransform(global: Global)(tree: global.Block): global.Block = tree

  /** A hook for custom macros to selectively generate and process a Graphviz visualization of the transformed state machine */
  def dot(global: Global)(enclosingOwner: global.Symbol, macroApplication: global.Tree): Option[(String => Unit)] = None

  def continueCompletedFutureOnSameThread: Boolean = false
}

object ScalaConcurrentFutureSystem extends FutureSystem {
  def awaitSymbol(global: Global): global.Symbol = global.async.asyncSymbols.Async_await.asInstanceOf[global.Symbol]
  override def continueCompletedFutureOnSameThread: Boolean = true
}
