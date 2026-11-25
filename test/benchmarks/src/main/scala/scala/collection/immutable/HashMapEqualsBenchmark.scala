package scala.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

import scala.util.Random

/** `==` on two independently built `HashMap`s or `HashSet`s that are equal, differ in one key or
 *  element, differ in one value, or are unrelated. The node field `cachedJavaKeySetHashCode` lets
 *  `equals` reject the one-key case at the root; without it that case walks the trie.
 */
@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class HashMapEqualsBenchmark {
  @Param(Array("10", "100", "1000", "10000", "100000"))
  var size: Int = _

  @Param(Array("equal", "oneKey", "oneValue", "unrelated"))
  var kind: String = _

  var map: HashMap[Int, Int] = _
  var otherMap: HashMap[Int, Int] = _
  var set: HashSet[Int] = _
  var otherSet: HashSet[Int] = _

  @Setup(Level.Trial) def init(): Unit = {
    val r = new Random(0x1234567890abcdefL)
    def fresh(avoid: collection.Set[Int]): Int = { var k = r.nextInt(); while (avoid(k)) k = r.nextInt(); k }
    val keys = collection.mutable.LinkedHashSet.empty[Int]
    while (keys.size < size) keys += r.nextInt()
    val ks = keys.toVector
    val mid = size / 2
    val otherKeys = kind match {
      case "oneKey"    => ks.updated(mid, fresh(keys)).reverse
      case "unrelated" =>
        val s = collection.mutable.LinkedHashSet.empty[Int]
        while (s.size < size) s += fresh(keys)
        s.toVector
      case _           => ks.reverse
    }
    map = HashMap.from(ks.map(k => k -> k))
    otherMap = HashMap.from(otherKeys.map(k => k -> k))
    if (kind == "oneValue") otherMap = otherMap.updated(ks(mid), 0)
    set = HashSet.from(ks)
    otherSet = HashSet.from(otherKeys)
  }

  @Benchmark def mapEquals: Boolean = map == otherMap
  @Benchmark def setEquals: Boolean = set == otherSet
}
