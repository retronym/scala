package scala
package collection

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.reflect.ClassTag
import org.junit.Assert._

/* Tests various maps by making sure they all agree on the same answers. */
@RunWith(classOf[JUnit4])
class NewBuilderTest {

  @Test
  def mapPreservesCollectionType() {
    def test[T: ClassTag](mapped: Any): Unit = {
      val expected = reflect.classTag[T].runtimeClass
      val isInstance = reflect.classTag[T].runtimeClass.isInstance(mapped)
      assertTrue(s"$mapped (of class ${mapped.getClass} is not a in instance of ${expected}", isInstance)
    }
    test[collection.Iterable[_]]((collection.Iterable(1): GenTraversable[Int]).map(x => x))
    test[collection.Seq[_]]((collection.Seq(1): GenTraversable[Int]).map(x => x))
    test[collection.LinearSeq[_]]((collection.LinearSeq(1): GenTraversable[Int]).map(x => x))
    test[collection.IndexedSeq[_]]((collection.IndexedSeq(1): GenTraversable[Int]).map(x => x))
    test[collection.Set[_]]((collection.Set(1): GenTraversable[Int]).map(x => x))

    test[immutable.Seq[_]]((immutable.Seq(1): GenTraversable[Int]).map(x => x))
    test[immutable.LinearSeq[_]]((immutable.LinearSeq(1): GenTraversable[Int]).map(x => x))
    test[immutable.IndexedSeq[_]]((immutable.IndexedSeq(1): GenTraversable[Int]).map(x => x))
    test[immutable.Vector[_]]((immutable.Vector(1): GenTraversable[Int]).map(x => x))
    test[immutable.List[_]]((immutable.List(1): GenTraversable[Int]).map(x => x))
    test[immutable.HashSet[_]]((immutable.HashSet(1): GenTraversable[Int]).map(x => x))
    test[immutable.Stack[_]]((immutable.Stack(1): GenTraversable[Int]).map(x => x))
    test[immutable.Stream[_]]((immutable.Stream(1): GenTraversable[Int]).map(x => x))
    test[immutable.Queue[_]]((immutable.Queue(1): GenTraversable[Int]).map(x => x))

    test[mutable.HashSet[_]]((mutable.HashSet(1): GenTraversable[Int]).map(x => x))
    test[mutable.Buffer[_]]((mutable.Buffer(1): GenTraversable[Int]).map(x => x))
    test[mutable.ArrayBuffer[_]]((mutable.ArrayBuffer(1): GenTraversable[Int]).map(x => x))
    test[mutable.ListBuffer[_]]((mutable.ListBuffer(1): GenTraversable[Int]).map(x => x))
    test[mutable.Queue[_]]((mutable.Queue(1): GenTraversable[Int]).map(x => x))

    test[mutable.LinkedHashSet[_]]((mutable.LinkedHashSet(1): GenTraversable[Int]).map(x => x))

    // test[mutable.ListMap[_, _]]((mutable.ListMap(1 -> 1): Map[Int, Int]).map(x => x))
    // test[immutable.ListMap[_, _]]((immutable.ListMap(1 -> 1): Map[Int, Int]).map(x => x))
    // test[immutable.HashMap[_, _]]((immutable.HashMap(1 -> 1): Map[Int, Int]).map(x => x))
    // test[mutable.HashMap[_, _]]((mutable.HashMap(1 -> 1): collection.Map[Int, Int]).map(x => x))
    // test[mutable.LinkedHashMap[_, _]]((mutable.LinkedHashMap(1 -> 1): GenTraversable[(Int, Int)]).map(x => x))
  }
}
