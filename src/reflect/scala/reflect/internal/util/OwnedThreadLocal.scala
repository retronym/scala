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

import java.lang.ref.WeakReference
import java.util.{Collections, WeakHashMap}

/** A value per thread for an owner that holds this, such as a universe, which keeps a value alive
 *  only while both its thread and the owner are.
 *
 *  A plain `ThreadLocal` field would leak the owner whenever its values reach it: a thread holds
 *  its `ThreadLocal` values strongly, a value reaches the owner and so the `ThreadLocal`, which is
 *  their key, and the entry never becomes stale. A long-lived thread would keep every owner it had
 *  used. So the owner holds the values, in a map weakly keyed by thread, and the `ThreadLocal`
 *  caches only a weak reference to the current thread's for the fast path. This is the remedy that
 *  `java.util.WeakHashMap`'s documentation gives for values that refer to their keys.
 *
 *  `make` runs once per thread, on its first `get`.
 */
final class OwnedThreadLocal[T <: AnyRef](make: () => T) {
  private[this] val values = Collections.synchronizedMap(new WeakHashMap[Thread, T])
  private[this] val cache = new ThreadLocal[WeakReference[T]]

  def get: T = {
    val ref = cache.get
    val value = if (ref eq null) null.asInstanceOf[T] else ref.get
    if (value ne null) value else first()
  }

  private def first(): T = {
    val value = make()
    values.put(Thread.currentThread, value)
    cache.set(new WeakReference(value))
    value
  }
}
