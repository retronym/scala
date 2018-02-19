package scala.collection

import org.junit.{Assert, Test}

import scala.collection.immutable.{HashMap, HashSet}

class MapNSetNKeyIdentityTest {
  private val a = Value("a")("a")
  private val b = Value("b")("b")
  private val c = Value("c")("c")
  private val d = Value("d")("d")
  private val a_ = Value("a")("a_")
  private val b_ = Value("b")("b_")
  private val c_ = Value("c")("c_")
  private val d_ = Value("d")("d_")
  private val aCollision = Value("aC")("A", "a".##)
  private val bCollision = Value("bC")("B", "b".##)
  private val cCollision = Value("cC")("C", "c".##)
  private val dCollision = Value("dC")("D", "d".##)

  @Test def mapsShouldStoreLatestKey() {
    test("Map1   ", Map[AnyRef, AnyRef](a -> b), MapTestOps)
    test("HashMap", HashMap[AnyRef, AnyRef](a -> b), MapTestOps)
    test("MapColl", HashMap[AnyRef, AnyRef](a -> b, aCollision -> bCollision), MapTestOps)
  }

  @Test def setShouldStoreLatestKey() {
    test("Set1   ", Set[AnyRef](a), SetTestOps)
    test("HashSet", HashSet[AnyRef](a), SetTestOps)
    test("MapColl", HashSet[AnyRef](a, aCollision), SetTestOps)
  }

  def test[T <: AnyRef](name:String, initial: T, ops: TestOps[T]): Unit = {
    val updated = ops.updated(initial, a, b)
    println(s"$name eq after update - ${initial eq updated}")
    assert (initial == updated)
    def checkUpdated(m: T)(k: AnyRef, v: AnyRef) = {
      val res = ops.updated(initial, k, v)
      if (!ops.keySet(res).exists(k1 => k1 eq k)) Assert.fail(("new key not stored", initial, initial.getClass, res, k).toString())
      res
    }
    for {
      keys <- List(a, b, c, d).inits
      extra <- List(a, b, c, d, a_, b_, c_, d_)
    } {
      var m = initial
      keys foreach (k => m = checkUpdated(m)(k, new Object))
      checkUpdated(m)(extra, new Object)
    }

  }

  // TODO test with Map.++, Map.+ etc that route through updated. Ditto for Set

  // TODO what about union, etc? Should Set(k) union Set(_k) endeavour to keep the first encountered key?
  // See immutable.HashSet1.union0

  private trait TestOps[T <: AnyRef] {
    def keySet(coll: T): Set[AnyRef]
    def updated(coll: T, k: AnyRef, v: AnyRef): T
  }
  private object SetTestOps extends TestOps[Set[AnyRef]] {
    override def keySet(coll: Set[AnyRef]): Set[AnyRef] = coll
    override def updated(coll: Set[AnyRef], k: AnyRef, v: AnyRef): Set[AnyRef] = coll + k
  }
  private object MapTestOps extends TestOps[Map[AnyRef, AnyRef]] {
    override def keySet(coll: Map[AnyRef, AnyRef]): Set[AnyRef] = coll.keySet
    override def updated(coll: Map[AnyRef, AnyRef], k: AnyRef, v: AnyRef): Map[AnyRef, AnyRef] = {
      val res = coll.updated(k, v)
      if (res(k) ne v) Assert.fail("new value not stored")
      res
    }
  }

  private case class Value(data:String)(info:String, hash:Int = data.##) {
    override def hashCode(): Int = hash

    override def toString: String = info
  }
}
