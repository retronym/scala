package scala.reflect.runtime

import java.util.concurrent.{CountDownLatch, TimeUnit}

import org.junit.Assert._
import org.junit.Test

/** In runtime reflection, a completion runs under the GIL, through `SynchronizedSymbol`'s
 *  overrides. It must still run in the next frame of its own thread's `Scratch`, and leave the
 *  frames of other threads alone.
 */
class CompletionFrameTest {
  @Test def completionThroughInfoRunsInTheNextFrame(): Unit = {
    val u = new JavaUniverse
    import u._
    val outer = scratch.frame
    var inner: CompletionFrame = null
    var innerDepth = -1
    var innerTablesFree = false
    val sym = rootMirror.RootClass.newMethod(TermName("completed"), NoPosition, 0L)
    sym.setInfo(new LazyType {
      override def complete(s: Symbol): Unit = {
        inner = scratch.frame
        innerDepth = inner.subTypes.depth
        innerTablesFree = inner.claimLubGlbTables()
        if (innerTablesFree) inner.releaseLubGlbTables()
        s.setInfo(NullaryMethodType(definitions.IntTpe))
      }
    })
    // As in the middle of an outermost lub that runs a subtype check.
    assertTrue(outer.claimLubGlbTables())
    outer.subTypes.enter()
    try sym.info
    finally { outer.subTypes.exit(); outer.releaseLubGlbTables() }
    assertSame(outer.next, inner)
    assertEquals("the completion does not see the check in progress", 0, innerDepth)
    assertTrue("the completion does not see the lub in progress", innerTablesFree)
    assertSame("the frame is restored", outer, scratch.frame)
    assertTrue(outer.subTypes.isIdle)
  }

  @Test def loadThroughTypeParamsRunsInTheNextFrame(): Unit = {
    val u = new JavaUniverse
    import u._
    val outer = scratch.frame
    var inner: CompletionFrame = null
    val sym = rootMirror.RootClass.newAbstractType(TypeName("Loaded"), NoPosition, 0L)
    sym.setInfo(new LazyType {
      override def load(s: Symbol): Unit = {
        inner = scratch.frame
        s.setInfo(TypeBounds.empty)
      }
      override def complete(s: Symbol): Unit = load(s)
    })
    assertEquals(Nil, sym.typeParams)
    assertSame(outer.next, inner)
    assertSame(outer, scratch.frame)
  }

  @Test def anotherThreadKeepsItsOwnFrames(): Unit = {
    val u = new JavaUniverse
    import u._
    val inCompletion, done = new CountDownLatch(1)
    @volatile var failure: Throwable = null
    val sym = rootMirror.RootClass.newMethod(TermName("slow"), NoPosition, 0L)
    sym.setInfo(new LazyType {
      override def complete(s: Symbol): Unit = {
        inCompletion.countDown()
        assertTrue(done.await(10, TimeUnit.SECONDS))
        s.setInfo(NullaryMethodType(definitions.IntTpe))
      }
    })
    val completer = new Thread(() => {
      val frame = scratch.frame
      frame.subTypes.enter()
      try sym.info catch { case t: Throwable => failure = t }
      finally frame.subTypes.exit()
    })
    completer.start()
    assertTrue(inCompletion.await(10, TimeUnit.SECONDS))
    try {
      // The other thread is in a completion, under the GIL, with a check in progress.
      // This thread's frame is its own: outermost, with nothing in progress.
      val frame = scratch.frame
      assertTrue(frame.subTypes.isIdle)
      assertTrue(frame.claimLubGlbTables())
      frame.releaseLubGlbTables()
    } finally done.countDown()
    completer.join()
    if (failure ne null) throw failure
  }
}
