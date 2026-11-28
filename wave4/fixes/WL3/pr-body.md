This pull request holds a failing test and no fix: the fix is a small design question, set out below.

The `asJava` views of collections whose elements or keys are `Int` or `Long` underneath (`BitSet`, `immutable.IntMap`, `mutable.LongMap`, and `TreeSet`/`TreeMap` ordered by `Ordering.Int` or `Ordering.Long`) treat a null argument from Java as 0:

```scala
mutable.BitSet(0, 5).asJava.contains(null)                // true
mutable.LongMap(0L -> "a").asJava.get(null)               // "a"
immutable.TreeMap[Long, String](0L -> "a").asJava.get(null) // "a"
val bs = mutable.BitSet(0, 5); bs.asJava.remove(null)     // true, bs is now BitSet(5)
```

`add(null)` on a `BitSet` view adds 0, and `put(null, v)` on a `LongMap` view overwrites key 0. A Java caller probing a `Set<Integer>` or `Map<Long, V>` from Scala with a possibly-null boxed key (`map.get(request.getId())`) gets the entry for 0, and a null remove or put changes it. `java.util.Set.contains` and `Map.get`/`containsKey`/`remove` allow a null argument to answer false or null, or to throw `NullPointerException` where the collection does not permit nulls; answering for another key is outside that contract. Views of collections that store boxed elements (`HashSet[Int]`, `HashMap[Int, V]`, `Vector[Int]`) already answer false or null, so the result depends on which Scala collection is behind the view.

Mechanism: `SetWrapper`, `MutableSetWrapper`, `MapWrapper` and `MutableMapWrapper` in `JavaCollectionWrappers` cast the argument with `asInstanceOf[A]`, which is a no-op on an erased type parameter, and call the Scala collection. The unboxing happens in the Scala collection's erasure bridge: `BitSet.contains(Object)`, `LongMap.get(Object)` and `Ordering.Int`/`Ordering.Long.compare(Object, Object)` call `BoxesRunTime.unboxToInt`/`unboxToLong`, which map null to 0. Long-standing: 2.12's `JavaConverters` behave the same.

The test accepts either valid answer (false or null, or `NullPointerException`) for each probe, checks that null removes and inserts leave key 0 alone, and checks that `Set[String]("a", null).asJava.contains(null)` stays true.

Possible fix. The wrapper does not know whether the element type is primitive, so answering false for every null argument is wrong: a `Set[String]` holding null must still answer true. Options:

- For a null argument only, answer from iteration (`underlying.exists(_ == null)`, `keysIterator` for maps). Correct for every collection; a null probe becomes O(n), other probes are unchanged. Removal of a null key would need the same route, and `add`/`put` of null would still need a rule.
- Type-test the underlying for the known primitive-keyed classes (`BitSet`, `IntMap`, `LongMap`, sorted collections whose ordering is `Ordering.Int` or `Ordering.Long`) and answer false/null, or throw `NullPointerException` for `add` and `put`. Constant time, but the list is open: any user collection whose `contains` unboxes, or a sorted collection with a custom `Int` ordering, keeps the defect.
- Fix it in the Scala collections' bridges, which is not possible without changing what `contains(null)` means on the Scala side.

The first option is the only one that is right for every collection; the cost falls only on null probes. Either way `add(null)` and `put(null, v)` on a view that cannot store null should throw `NullPointerException`, which the contract allows.

Related: retronym/scala#540 adds `Buffer.asJava.remove(Object)` through `indexOf`, which already answers null correctly.

Found in the fourth wave of the coordinated code review of 2.13.x (finding WL3). Prepared with Claude.
