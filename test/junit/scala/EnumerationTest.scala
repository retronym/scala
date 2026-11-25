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

  // The accessors of the values defined further down return null while the enumeration is being initialized
  @Test def nameOfValueDuringInitialization(): Unit = {
    object Colour extends Enumeration {
      val Red = Value
      val default: String = Red.toString
      val Green = Value
      val both: String = s"$Red $Green"
      val Blue = Value
    }
    assertEquals("Red", Colour.default)
    assertEquals("Red Green", Colour.both)
    assertEquals("Blue", Colour.Blue.toString)
    assertEquals(Colour.Green, Colour.withName("Green"))
  }
}
