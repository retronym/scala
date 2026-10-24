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

package scala
package reflect
package runtime

import scala.ref.{WeakReference => sWeakRef}

/** This trait overrides methods in reflect.internal, bracketing
 *  them in synchronized { ... } to make them thread-safe
 */
private[reflect] trait SynchronizedTypes extends internal.Types { self: SymbolTable =>

  private lazy val _intersectionWitness = mkThreadLocalStorage(perRunCaches.newWeakMap[List[Type], sWeakRef[Type]]())
  override def intersectionWitness = _intersectionWitness.get

  /* The idea of caches is as follows.
   * When in reflexive mode, a cache is either null, or one sentinel
   * value representing undefined or the final defined
   * value. Hence, we can ask in non-synchronized mode whether the cache field
   * is non null and different from the sentinel (if a sentinel exists).
   * If that's true, the cache value is current.
   * Otherwise we arrive in one of the defined... methods listed below
   * which go through all steps in synchronized mode.
   */

  override protected def defineUnderlyingOfSingleType(tpe: SingleType) =
    gilSynchronized { super.defineUnderlyingOfSingleType(tpe) }

  override protected def defineBaseTypeSeqOfCompoundType(tpe: CompoundType) =
    gilSynchronized { super.defineBaseTypeSeqOfCompoundType(tpe) }

  override protected def defineBaseClassesOfCompoundType(tpe: CompoundType) =
    gilSynchronized { super.defineBaseClassesOfCompoundType(tpe) }

  override protected def defineParentsOfTypeRef(tpe: TypeRef) =
    gilSynchronized { super.defineParentsOfTypeRef(tpe) }

  override protected def defineBaseTypeSeqOfTypeRef(tpe: TypeRef) =
    gilSynchronized { super.defineBaseTypeSeqOfTypeRef(tpe) }

  override protected def defineNormalized(tr: TypeRef): Unit = {
    gilSynchronized { super.defineNormalized(tr) }
  }
}
