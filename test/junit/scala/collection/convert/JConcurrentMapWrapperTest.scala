package scala.collection.convert

import org.junit.Assert.{assertEquals, assertFalse}
import org.junit.Test

import java.util.concurrent.{ConcurrentHashMap, ConcurrentSkipListMap, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicReference

import scala.collection.concurrent
import scala.util.Try

import scala.collection.concurrent.ConcurrentMapTestHelper
import scala.jdk.CollectionConverters._

class JConcurrentMapWrapperTest {
  @Test
  def CHM_filterInPlace(): Unit = {
    ConcurrentMapTestHelper.genericTest_filterInPlace(new ConcurrentHashMap[String, Int].asScala)
  }

  @Test
  def CHM_mapValuesInPlace(): Unit = {
    ConcurrentMapTestHelper.genericTest_mapValuesInPlace(new ConcurrentHashMap[String, Int].asScala)
  }

  @Test
  def CSLM_filterInPlace(): Unit = {
    ConcurrentMapTestHelper.genericTest_filterInPlace(new ConcurrentSkipListMap[String, Int].asScala)
  }

  @Test
  def CSLM_mapValuesInPlace(): Unit = {
    ConcurrentMapTestHelper.genericTest_mapValuesInPlace(new ConcurrentSkipListMap[String, Int].asScala)
  }

  // scala/bug#12978. The default value of `getOrElseUpdate` reads and updates the same map. Both tests run the
  // operation on daemon threads and wait for them with a timeout, since a failure can be a blocked or spinning thread.

  // A memoised recursive function.
  @Test
  def CHM_getOrElseUpdateDefaultValueUpdatesTheMap(): Unit = {
    def fib(m: concurrent.Map[Int, BigInt])(n: Int): BigInt =
      if (n < 2) n else m.getOrElseUpdate(n, fib(m)(n - 1) + fib(m)(n - 2))
    val result = new AtomicReference[Try[BigInt]]
    val t = new Thread(() => result.set(Try(fib(new ConcurrentHashMap[Int, BigInt].asScala)(60))))
    t.setDaemon(true)
    t.start()
    t.join(10000)
    assertFalse("still running after 10 s", t.isAlive)
    assertEquals(Try(BigInt(1548008755920L)), result.get)
  }

  // Two threads, each computing the value of its key from the value of the other thread's key.
  @Test
  def CHM_getOrElseUpdateDefaultValuesReadEachOthersKey(): Unit = {
    val m = new ConcurrentHashMap[String, Int].asScala
    val bothInside = new CountDownLatch(2)
    def task(mine: String, other: String) = new Thread(() => {
      m.getOrElseUpdate(mine, {
        bothInside.countDown()
        bothInside.await(2, TimeUnit.SECONDS)
        m.getOrElseUpdate(other, 1) + 1
      })
      ()
    })
    val threads = List(task("a", "b"), task("b", "a"))
    threads.foreach { t => t.setDaemon(true); t.start() }
    val deadline = System.nanoTime + TimeUnit.SECONDS.toNanos(5)
    threads.foreach(t => t.join(1L max TimeUnit.NANOSECONDS.toMillis(deadline - System.nanoTime)))
    assertFalse("threads still blocked after 5 s", threads.exists(_.isAlive))
    assertEquals(Set("a", "b"), m.keySet)
  }
}
