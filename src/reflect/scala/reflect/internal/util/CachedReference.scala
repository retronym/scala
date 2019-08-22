package scala.reflect.internal.util

import java.lang.ref.WeakReference

final class CachedReference[T](private var t: T) {
  val weakRef: WeakReference[T] = new WeakReference[T](t)
  def singleShotGet: T = {
    try t
    finally t = null.asInstanceOf[T]
  }
}
