package scala.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class EnumerationTest {

  @Test def t10906(): Unit = {
    object A extends Enumeration
    val s1 = A.values.map(x => 42)
    assertEquals(Set.empty, (s1: scala.collection.immutable.SortedSet[Int]))
    val s2 = A.values.map(x => x)
    assertEquals(Set.empty, (s2: A.ValueSet))
    val s3 = A.values.flatMap(x => Set(42))
    assertEquals(Set.empty, (s3: scala.collection.immutable.SortedSet[Int]))
    val s4 = A.values.flatMap(x => Set(x))
    assertEquals(Set.empty, (s4: A.ValueSet))
  }

  @Test def implicitOrderingIsSameAsEnumerationOrdering(): Unit = {
    object MyEnum extends Enumeration {
      val a, b, c = Value
    }
    assertSame(Ordering[MyEnum.Value], MyEnum.ValueOrdering)
  }

  private def roundTrip[A](a: A): A = {
    import java.io._
    val bo = new ByteArrayOutputStream
    val oo = new ObjectOutputStream(bo)
    oo.writeObject(a)
    oo.close()
    new ObjectInputStream(new ByteArrayInputStream(bo.toByteArray)).readObject().asInstanceOf[A]
  }

  @Test def deserializeTopLevelEnumeration(): Unit = {
    assertSame(EnumerationTest.TopLevel, roundTrip(EnumerationTest.TopLevel))
    assertSame(EnumerationTest.TopLevel.B, roundTrip(EnumerationTest.TopLevel.B))
  }

  @Test def deserializeNonStaticEnumeration(): Unit = {
    import EnumerationTest._
    val sizes = new Sizes
    assertEquals("M", roundTrip(sizes.M).toString)
    assertEquals(List("S", "M", "L"), roundTrip(sizes.values).toList.map(_.toString))
    assertEquals(List("S", "M", "L"), roundTrip(sizes).values.toList.map(_.toString))
    val (sizes1, m1) = roundTrip((sizes, sizes.M))
    assertSame(sizes1.withName("M"), m1)

    val holder = new Holder
    assertEquals("Green", roundTrip(holder.Colours.Green).toString)
    assertEquals("Green", roundTrip(holder).Colours.Green.toString)

    assertEquals("Q", roundTrip(anon(1)).toString)
  }
}

object EnumerationTest {
  object TopLevel extends Enumeration { val A, B = Value }
  class Sizes extends Enumeration { val S, M, L = Value }
  class Holder extends Serializable { object Colours extends Enumeration { val Red, Green = Value } }
  @annotation.nowarn("msg=inferred structural type") val anon = new Enumeration { val P, Q = Value }
}
