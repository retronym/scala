package scala.reflect.runtime

import org.junit.Assert._
import org.junit.Test

trait CommonOwnerX
trait CommonOwnerY
class CommonOwnerA extends CommonOwnerX with CommonOwnerY
class CommonOwnerB extends CommonOwnerX with CommonOwnerY

class CommonOwnerTest {
  private val u = universe.asInstanceOf[JavaUniverse]
  import u._

  private def thisPackage = symbolOf[CommonOwnerA].owner

  // The refinement classes that type operations create are owned by the innermost owner common
  // to the types involved. Runtime reflection computed that owner as NoSymbol.
  @Test def intersectionTypeOwner(): Unit = {
    val tp = internal.intersectionType(List(typeOf[CommonOwnerX], typeOf[CommonOwnerY]))
    assertTrue(tp.toString, tp.typeSymbol.isRefinementClass)
    assertEquals(thisPackage, tp.typeSymbol.owner)
  }

  @Test def lubOwner(): Unit = {
    val tp = lub(List(typeOf[CommonOwnerA], typeOf[CommonOwnerB]))
    assertTrue(tp.toString, tp.typeSymbol.isRefinementClass)
    assertEquals(thisPackage, tp.typeSymbol.owner)
  }

  @Test def narrowOwner(): Unit = {
    val tp = typeOf[CommonOwnerX].narrow
    assertTrue(tp.toString, tp.typeSymbol.isRefinementClass)
    assertEquals(thisPackage, tp.typeSymbol.owner)
  }

  @Test def ownerCommonToTwoPackages(): Unit = {
    val tp = internal.intersectionType(List(typeOf[java.io.Serializable], typeOf[java.lang.CharSequence]))
    assertEquals(symbolOf[java.io.Serializable].owner.owner, tp.typeSymbol.owner)
  }
}
