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

package scala.collection.immutable

import java.util.concurrent.atomic.AtomicReferenceFieldUpdater
import java.util.concurrent.locks.AbstractQueuedSynchronizer

/**
 * Base class for [[LazyList]] to split out code that uses concurrency utilities that are not available
 * on Scala.js. This way, Scala.js does not need to override all of LazyList.
 *
 * This class cannot be a trait because `AtomicReferenceFieldUpdater.newUpdater` checks if the caller
 * class has access to the corresponding field. So it needs to be called in the class where the field is
 * declared (fields are always private in Scala).
 */
abstract class LazyListBase[+A] private[immutable] (initialTail: AnyRef) extends AbstractSeq[A] with Serializable {
  /** See [[LazyList._head]] for the possible states of this field.
    *
    * The field is `private[this]` so that `initState` writes it with a `putfield`, not through an accessor method.
    */
  @volatile private[this] var _tail: AnyRef /* () => LazyList[A] | Thread | InRace | LazyList[A] */ = initialTail

  private[immutable] def rawTail: AnyRef = _tail

  private[immutable] def setRawTail(value: AnyRef): Unit = _tail = value

  @noinline private[immutable] def makeTailUpdater: LazyListBase.TailUpdater =
    new LazyListBase.TailUpdater(AtomicReferenceFieldUpdater.newUpdater(classOf[LazyListBase[_]], classOf[AnyRef], "_tail"))

  /** The evaluated lazy list that holds the state (head and tail) for this one, given the result of the state function. */
  private[immutable] def evaluatedState(result: AnyRef): LazyListBase[_]

  /** Sets `_head` of this lazy list to the head of `state`. */
  private[immutable] def initHead(state: LazyListBase[_]): Unit

  /** One attempt at evaluating the state of this lazy list, or at waiting for the thread that is evaluating it.
    * The caller repeats it until `_head` is initialized.
    *
    * The thread that evaluates the state function marks `_tail` and removes the marker in this frame, whatever is
    * thrown in between. A `StackOverflowError` from the state function is caught here with the stack nearly full,
    * and the calls that restore `_tail` and wake up waiting threads may overflow it again. So the last resort is a
    * plain write of the field, which needs no stack. A marker that stayed behind would make every other thread that
    * evaluates this lazy list wait forever, and would tell this thread that the lazy list refers to itself.
    */
  private[immutable] final def initState(updater: LazyListBase.TailUpdater): Unit = _tail match {
    case t: Thread =>
      if (LazyListBase.isCurrentThread(t)) selfRef()
      val ir = LazyListBase.InRace(t)
      if (updater.compareAndSet(this, t, ir))
        ir.await(this)
      // retry on lost CAS

    case ir: LazyListBase.InRace =>
      if (LazyListBase.isCurrentThread(ir.owner)) selfRef()
      ir.await(this)

    case fun: Function0[_] =>
      var marked = false
      var sentinel: AnyRef = null
      try {
        // use the current thread as marker that `fun` is being evaluated.
        // this way, there is no allocation in the common case where there's no race.
        // if multiple threads attempt to initialize a LazyList, an `InRace` instance is created to coordinate.
        if (updater.compareAndSet(this, fun, Thread.currentThread)) {
          marked = true
          // `fun` returns a LazyList that represents the state (head/tail) of `this`. `evaluatedState` ensures the
          // result is initialized, to prevent races when reading its tail and head below.
          // Often, lazy lists are created with `newLL(eagerCons(...))` so the result is already initialized, but
          // `newLL` also accepts non-evaluated lazy lists.
          val state = evaluatedState(fun().asInstanceOf[AnyRef])
          val newTail = state.rawTail
          // update `_tail` before `_head`, because `_head` is used to test `isEvaluated`
          sentinel = updater.getAndSet(this, newTail)
          initHead(state)
          marked = false
          release(sentinel)
        }
        // retry on lost CAS
      } catch {
        case t: Throwable =>
          if (marked) {
            // Also reached when `initHead` fails after `_tail` was updated. Threads that saw the new `_tail` without
            // a `_head` are retrying, and find `fun` again.
            val previous =
              try updater.getAndSet(this, fun)
              catch { case _: Throwable => val p = _tail; _tail = fun; p } // no call up to the end of the write
            // if these fail, or if the plain write has overwritten a waiter's `InRace`, `InRace.await` notices
            release(sentinel)
            release(previous)
          }
          throw t
      }

    case _ =>
      // retry when _tail is a LazyList but _head is still `Uninitialized`
      // could call `Thread.onSpinWait()` on JDK 9+
  }

  private[this] def selfRef(): Nothing =
    // if it's already mid-evaluation, we're stuck in an infinite self-referential loop (also it's empty)
    throw new RuntimeException(
      "LazyList evaluation depends on its own result (self-reference); see docs for more info")

  private[this] def release(sentinel: AnyRef): Unit = sentinel match {
    case ir: LazyListBase.InRace => ir.countDown()
    case _ =>
  }
}

private[immutable] object LazyListBase {
  final class TailUpdater(u: AtomicReferenceFieldUpdater[LazyListBase[_], AnyRef]) {
    def compareAndSet(ll: LazyListBase[_], expected: AnyRef, value: AnyRef): Boolean = u.compareAndSet(ll, expected, value)
    def getAndSet(ll: LazyListBase[_], value: AnyRef): AnyRef = u.getAndSet(ll, value)
  }

  // this utility is constant `true` on Scala.js -> enables DCE in LazyList
  def isCurrentThread(t: Thread) = t eq Thread.currentThread
  // also for Scala.js
  def InRace(t: Thread) = new InRace(t)

  final class InRace private[LazyListBase] (val owner: Thread) {
    // Implements a one-time latch
    final private class Sync extends AbstractQueuedSynchronizer {
      override def tryAcquireShared(unused: Int): Int = if (getState == 0) -1 else 1
      override def tryReleaseShared(unused: Int): Boolean = getState == 0 && compareAndSetState(0, 1)
    }

    private val sync = new Sync()

    /** Waits until the owner is done with `ll`. The owner normally calls `countDown`. It may fail to (see
      * `LazyListBase.initState`), so this also returns when `ll` does not hold this instance any longer, which is
      * checked every 100 ms. Like `acquireShared`, it does not respond to interruption.
      */
    def await(ll: LazyListBase[_]): Unit = {
      var interrupted = false
      var done = false
      while (!done)
        try done = sync.tryAcquireSharedNanos(0, 100000000L) || (ll.rawTail ne this)
        catch { case _: InterruptedException => interrupted = true }
      if (interrupted) Thread.currentThread.interrupt()
    }
    def countDown(): Unit = sync.releaseShared(0)
  }
}
