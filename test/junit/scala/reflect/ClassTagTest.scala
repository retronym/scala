package scala.reflect

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

class Misc

@RunWith(classOf[JUnit4])
class ClassTagTest {
  def checkNotString[A: ClassTag](a: Any)  = a match { case x: String  => false case x: A => true case _ => false }
  def checkNotInt[A: ClassTag](a: Any)  = a match { case x: Int  => false case x: A => true case _ => false }
  def checkNotLong[A: ClassTag](a: Any) = a match { case x: Long => false case x: A => true case _ => false }

  @Test def checkMisc(): Unit    = assertTrue(checkNotString[Misc](new Misc))
  @Test def checkString(): Unit  = assertTrue(checkNotInt[String] ("woele"))
  @Test def checkByte(): Unit    = assertTrue(checkNotInt[Byte]   (0.toByte))
  @Test def checkShort(): Unit   = assertTrue(checkNotInt[Short]  (0.toShort))
  @Test def checkChar(): Unit    = assertTrue(checkNotInt[Char]   (0.toChar))
  @Test def checkInt(): Unit     = assertTrue(checkNotLong[Int]   (0.toInt))
  @Test def checkLong(): Unit    = assertTrue(checkNotInt[Long]   (0.toLong))
  @Test def checkFloat(): Unit   = assertTrue(checkNotInt[Float]  (0.toFloat))
  @Test def checkDouble(): Unit  = assertTrue(checkNotInt[Double] (0.toDouble))
  @Test def checkBoolean(): Unit = assertTrue(checkNotInt[Boolean](false))
  @Test def checkUnit(): Unit    = assertTrue(checkNotInt[Unit]   ({}))

  @deprecated("Tests deprecated API", since="2.13")
  @Test def t9534(): Unit = {
    val ct = implicitly[scala.reflect.ClassTag[Unit]]
    val a1 = ct.newArray(1)
    a1(0) = ()
    val a2 = ct.wrap.newArray(1)
    a2(0) = a1
    val a3 = ct.newArray2(1)
    a3(0) = a1
  }

  @Test def classTagEqualsManifestSymmetric(): Unit = {
    val ct: ClassTag[_] = ClassTag(classOf[String])
    val mf: ClassTag[_] = manifest[String]
    assertEquals(mf == ct, ct == mf)
    assertFalse(ct == mf)
    assertEquals(ct, ClassTag(classOf[String]))
    assertEquals(mf, manifest[String])
    val cl: ClassTag[_] = classTag[List[_]]
    assertFalse(cl == manifest[List[Int]])
    assertFalse(cl == manifest[List[String]])
    assertEquals(ClassTag.Int, ClassTag(classOf[Int]))
    assertEquals(ClassTag(classOf[Int]), manifest[Int])
  }
}
