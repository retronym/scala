package scala.reflect.internal.util

import java.lang.ref.SoftReference

private[scala] abstract class WeakCache[K, S, V] {
  private val cache = collection.mutable.WeakHashMap.empty[K, SoftReference[(S, V)]]
  protected def create(k: K): V
  protected def stamp(k: K): S
  protected def share(v: V): Unit = ()
  final def apply(k: K): V = get(k)._2
  final def get(k: K): (Boolean, V) = synchronized {
    val currentStamp = stamp(k)
    def createEntry: (Boolean, V) = {
      val instance = create(k)
      cache.put(k, new SoftReference((currentStamp, instance)))
      (true, instance)
    }
    cache.get(k) match {
      case Some(ref) =>
        val entry = ref.get()
        if (entry == null) {
          createEntry
        } else {
          val (cachedStamp, cachedValue) = entry
          if (cachedStamp != currentStamp) {
            createEntry
          } else {
            share(cachedValue)
            (false, cachedValue)
          }
        }
      case None =>
        createEntry
    }
  }
}
