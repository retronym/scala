package scala.collection.immutable

import java.util

import org.scalacheck._
import org.scalacheck.Prop._
import Gen._

import scala.collection.mutable.ArrayBuffer

object BitSetProperties extends Properties("immutable.BitSet") {

  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(500)
      .withInitialSeed(42L)

  // the top of the range shouldn't be too high, else we may not get enough overlap
  implicit val arbitraryBitSet: Arbitrary[BitSet] = Arbitrary(
    oneOf(
      const(BitSet()),
      oneOf(0 to 100).map(i => BitSet(i)),
      listOfN(200, oneOf(0 to 10000)).map(_.to(BitSet))
    )
  )

  property("min") = forAll { (bs: BitSet) =>
    bs.nonEmpty ==> (bs.min ?= bs.toList.min)
  }
  property("min reverse") = forAll { (bs: BitSet) =>
    bs.nonEmpty ==> (bs.min(Ordering.Int.reverse) ?= bs.toList.min(Ordering.Int.reverse))
  }

  property("max") = forAll { (bs: BitSet) =>
    bs.nonEmpty ==> (bs.max ?= bs.toList.max)
  }

  property("max reverse") = forAll { (bs: BitSet) =>
    bs.nonEmpty ==> (bs.max(Ordering.Int.reverse) ?= bs.toList.max(Ordering.Int.reverse))
  }

  property("diff bitSet") = forAll { (left: BitSet, right: BitSet) =>
    (left.diff(right): Set[Int]) ?= left.to(HashSet).diff(right.to(HashSet))
  }
  property("diff hashSet") = forAll { (left: BitSet, right: BitSet) =>
    (left.diff(right.to(HashSet)) : Set[Int]) ?= left.to(HashSet).diff(right.to(HashSet))
  }

  property("filter") = forAll { (bs: BitSet) =>
    bs.filter(_ % 2 == 0) ?= bs.toList.filter(_ % 2 == 0).to(BitSet)
  }
  property("filterNot") = forAll { (bs: BitSet) =>
    bs.filterNot(_ % 2 == 0) ?= bs.toList.filterNot(_ % 2 == 0).to(BitSet)
  }

  property("partition") = forAll { (bs: BitSet) =>
    val p = (i: Int) => i % 2 == 0
    val (left, right) = bs.partition(p)
    (left ?= bs.filter(p)) && (right ?= bs.filterNot(p))
  }

  property("stepper vs j.u.BitSet") = forAll { (bs: BitSet) =>
    val jbs = java.util.BitSet.valueOf(bs.toBitMask)
    import scala.jdk.StreamConverters.Ops._
    jbs.stream().toScala(Iterator).sameElements(bs.stepper.iterator)
  }

  property("iterator vs j.u.BitSet") = forAll { (bs: BitSet) =>
    val jbs = java.util.BitSet.valueOf(bs.toBitMask)
    import scala.jdk.StreamConverters.Ops._
    jbs.stream().toScala(Iterator).sameElements(bs.iterator)
  }

  property("foreach vs j.u.BitSet") = forAll { (bs: BitSet) =>
    val buf = new ArrayBuffer[Int]()
    bs.foreach(buf += _)
    val jbs = java.util.BitSet.valueOf(bs.toBitMask)
    import scala.jdk.StreamConverters.Ops._
    jbs.stream().toScala(Iterator).sameElements(buf.iterator)
  }

  property("next/prev set/clear bit vs j.u.BitSet") = forAll { (bs: BitSet) =>
    def check: Prop = {
      val jbs = java.util.BitSet.valueOf(bs.toBitMask)
      var i = -1
      val max = bs.maxOption.getOrElse(0) + 65
      while (i <= max) {
        if (i != -1) {
          if (jbs.nextClearBit(i) != bs.nextClearBit(i)) return Prop.falsified.label("nextClearBit(" + i + ")")
          if (jbs.nextSetBit(i) != bs.nextSetBit(i)) return Prop.falsified.label("nextSetBit(" + i + ")")
        }
        if (jbs.previousClearBit(i) != bs.previousClearBit(i)) return Prop.falsified.label("previousClearBit(" + i + ")")
        if (jbs.previousSetBit(i) != bs.previousSetBit(i)) return Prop.falsified.label("previousSetBit(" + i + ")")
        i += 1
      }
      true
    }
    check
  }
}
