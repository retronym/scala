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

  @Test def valueSetKeepsElementsWhenLowerIdIsDefinedLater(): Unit = {
    object Level extends Enumeration {
      val Low = Value(2, "Low")
      val High = Value(5, "High")
      val Alerting = ValueSet(High)
      val Debug = Value(-3, "Debug")
    }
    assertEquals(List(Level.High), Level.Alerting.toList)
    assertTrue(Level.Alerting.contains(Level.High))
    assertFalse(Level.Alerting.contains(Level.Low))
    assertEquals(Level.ValueSet(Level.High), Level.Alerting)
    assertEquals(Level.ValueSet(Level.Low, Level.High), Level.Alerting + Level.Low)
    assertEquals(Level.Alerting, Level.ValueSet.fromBitMask(Level.Alerting.toBitMask))
  }

  @Test def valueSetSerializedAfterLowerIdIsDefined(): Unit = {
    import java.io._
    val bos = new ByteArrayOutputStream
    val oos = new ObjectOutputStream(bos)
    oos.writeObject(LevelWithSet.Alerting)
    oos.close()
    val read = new ObjectInputStream(new ByteArrayInputStream(bos.toByteArray)).readObject()
    assertEquals(LevelWithSet.ValueSet(LevelWithSet.High), read)
  }
}

object LevelWithSet extends Enumeration {
  val Low = Value(2, "Low")
  val High = Value(5, "High")
  val Alerting = ValueSet(High)
  val Debug = Value(-3, "Debug")
}
