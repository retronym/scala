package scala.reflect.runtime

import java.lang.ref.WeakReference
import java.util.function.Supplier

import org.junit.Assert._
import org.junit.Test

import scala.reflect.io.Streamable

/** Creates a universe from a class that only a child class loader can see. */
class UniverseLoaderRetentionTestCreator extends Supplier[AnyRef] {
  def get(): AnyRef = new JavaUniverse
}

class UniverseLoaderRetentionTest {
  import UniverseLoaderRetentionTest._

  /** A universe outlives the code that created it, as `scala.reflect.runtime.universe` outlives the first code to
   *  use it. The universe must not keep that code's class loader reachable, as it did through the stack traces of
   *  the `Throwable`s `CyclicInheritance` and `GlbFailure`.
   */
  @Test def universeDoesNotRetainCreatorsLoader(): Unit = {
    val (universe, loader) = createInChildLoader()
    assertTrue("the class loader of the code that created the universe is reachable", collected(loader))
    assertTrue(universe.isInstanceOf[JavaUniverse])
  }
}

object UniverseLoaderRetentionTest {
  private val creator = classOf[UniverseLoaderRetentionTestCreator].getName

  /** Defines `creator` itself and delegates every other class to the parent. */
  private class ChildLoader(parent: ClassLoader) extends ClassLoader(parent) {
    override def loadClass(name: String, resolve: Boolean): Class[_] =
      if (name != creator) super.loadClass(name, resolve)
      else getClassLoadingLock(name).synchronized {
        val loaded = findLoadedClass(name)
        if (loaded != null) loaded
        else {
          val bytes = Streamable.bytes(parent.getResourceAsStream(name.replace('.', '/') + ".class"))
          defineClass(name, bytes, 0, bytes.length)
        }
      }
  }

  def createInChildLoader(): (AnyRef, WeakReference[ClassLoader]) = {
    val loader = new ChildLoader(getClass.getClassLoader)
    val cls = loader.loadClass(creator)
    assertSame(loader, cls.getClassLoader)
    val universe = cls.getDeclaredConstructor().newInstance().asInstanceOf[Supplier[AnyRef]].get()
    (universe, new WeakReference(loader))
  }

  def collected(ref: WeakReference[_]): Boolean = {
    var i = 0
    while (i < 50 && ref.get != null) { System.gc(); Thread.sleep(10); i += 1 }
    ref.get == null
  }
}
