package scala.reflect
package runtime

import java.lang.Thread._

private[reflect] trait Tls {
  self: SymbolTable =>

  // see a discussion at scala-internals for more information:
  // http://groups.google.com/group/scala-internals/browse_thread/thread/337ce68aa5e51f79
  trait Tls[T] { def get: T; def set(newValue: T): Unit }
  private class MyTls[T](initialValue: => T) extends Tls[T] {
    // TODO: how do we use org.cliffc.high_scale_lib.NonBlockingHashMap here?
    val values = new java.util.concurrent.ConcurrentHashMap[Thread, T]()
    def get: T = {
      if (values containsKey currentThread) values.get(currentThread)
      else {
        val value = initialValue
        values.putIfAbsent(currentThread, value)
        value
      }
    }
    def set(newValue: T): Unit = {
      values.put(currentThread, newValue)
    }
  }
  @inline final def mkTls[T](x: => T): Tls[T] = new MyTls(x)
}
