# Library campaign (track `library`, prefix WL)

Harness: `wave4/harness/library/` (README there). Build under test: `w3-int` for the first development runs, `w4-int` from round 1 on (it appeared before the first measured round). Each finding was re-run on `w4-int`, `w3-base` and 2.13.18.

## What was built

Programs of 1 to 30 calls on the library files the earlier spikes did not reach, each call compared with the same call on a model that is obviously right, with every version, clone and snapshot kept live and re-checked after every call. A failing program is shrunk by deleting calls while its signature (family, operation, kind of expected and actual result) is unchanged; signatures are bucketed with a count and the first shrunk example. Known defects and unspecified behaviour are excluded narrowly and counted.

| Round | Change | Cases | Seconds | New signatures | Scope lines covered |
|---|---|---|---|---|---|
| 1 | Specialised maps (mutable LongMap, AnyRefMap, OpenHashMap, CollisionProofHashMap, TrieMap with snapshots; immutable IntMap, LongMap, TreeSeqMap both orders, VectorMap, ListMap), PriorityQueue, Stack, Queue, immutable Queue | 523,839 | 150 | 1 (WL1) | 2,907 / 7,713 |
| 2 | `scala.jdk` and `convert`: wrappers both ways against java.util models, steppers over 48 shapes with recursive `trySplit`, Java streams, accumulators | 744,573 | 160 | 2 (WL2, WL3) | 4,657 / 7,713 |
| 3 | Iterator chains, LazyList evaluation counts, Sorting, BigInt/BigDecimal, Random, `format`, StringView | 8,475,744 | 110 | 0 | 5,660 / 7,713 |

Coverage is JaCoCo line coverage of 31 files plus `convert/impl`, cumulative; per-file figures are in `wave4/rounds/library.json` and `reports/library-treemap.html`. Selected files after round 3: OpenHashMap 124/125, PriorityQueue 130/138, IntMap 208/228, immutable LongMap 202/225, mutable LongMap 300/339, TrieMap 464/612, TreeSeqMap 292/381, JavaCollectionWrappers 215/325, IntAccumulator 202/278, Iterator 462/520, LazyList 307/451, BigInt 183/200, BigDecimal 95/163, Sorting 111/139.

False alarms, all the harness's own: 18 across the three rounds, each fixed before the measured run of its round. Causes: optional exceptions in the java.util contracts (read-only views that answer a no-change call instead of throwing, TreeMap and ConcurrentSkipListMap models that reject foreign or null keys); a value removed from `values()` when two keys share it; Float NaN compared with `==`; terminals that consumed one iterator twice; a java.math model that rounded its operands where `BigDecimal(String)` is exact with a widened MathContext; `BigDecimal == Double` using the decimal expansion; and LazyList oracles that assumed an element can stay unevaluated while its cell is forced, which the Scaladoc rules out ("elements are computed in order and are never skipped").

Exclusions on the final runs: LM7 (`Buffer.asJava` iterator cannot remove) 257,196; WL3 53,864; WL2 1,965; WL1 3,557; optional java.util exceptions 324,108; LazyList `updated`/`tail` out of range 48,172.

## Findings

- WL1: `snapshot()` and `readOnlySnapshot()` on a TrieMap obtained from `readOnlySnapshot()` throw NullPointerException (the null root updater reaches `CAS_ROOT`). Same on base and 2.13.18.
- WL2: `IntAccumulator`, `LongAccumulator`, `DoubleAccumulator` and `AnyAccumulator` `apply`/`update` do not check the index against the size: `acc(acc.length)` returns 0 or null, and `acc(acc.length) = x` is accepted and lost on the next `addOne`. Same on base and 2.13.18.
- WL3: `asJava` views of `BitSet`, `TreeSet[Int]`, `mutable.LongMap`, `IntMap` and `TreeMap[Int, V]` unbox a null argument to 0: `contains(null)` is true, `get(null)` returns key 0's value, `remove(null)` removes key 0. Collections that store boxed elements answer false or null. Same on base and 2.13.18.

## Differences that are unspecified behaviour, not findings

- `IntMap.firstKey` / `LongMap.firstKey` on an empty map throw IllegalStateException, not NoSuchElementException.
- `TreeSeqMap`'s companion builder (what `mapFactory.newBuilder` gives) orders by insertion whatever the receiver's ordering; `empty` keeps the ordering.
- `ArrayList.asScala.insert` / `insertAll` out of range throw IllegalArgumentException where ArrayBuffer throws IndexOutOfBoundsException; `remove(i, 0)` with `i` out of range throws where ArrayBuffer returns.
- `LazyList.updated` with an index past the end throws only when forced, and on an empty LazyList the exception is UnsupportedOperationException rather than IndexOutOfBoundsException.
- `PriorityQueue.map` returns an `Iterable`, not a `PriorityQueue`.

## Clean results

At the volumes above, no value difference in: the specialised mutable and immutable maps including their own methods and TrieMap snapshot isolation (writes after a snapshot are never seen by it or by an iterator over it); insertion and modification order of TreeSeqMap, VectorMap and ListMap; PriorityQueue `dequeueAll`, `clone`, `reverse`, `reverseIterator`; Stack/Queue own methods; immutable Queue persistence; the java.util contracts of `equals`, `hashCode`, `subList`, list iterators, `entrySet` with `setValue` and `remove`, `putIfAbsent`/`compute*`/`merge`; stepper splitting and characteristics over 48 shapes; parallel streams; accumulator `drain`; Iterator `span`/`partition`/`duplicate` consumed in either order; LazyList memoisation and in-order evaluation; stable sorting and agreement with `java.util.Arrays.sort` on every primitive type including NaN and -0.0; BigInt and BigDecimal arithmetic and conversions; Random bounds and `shuffle`.

## Not yet produced

`java.util.Dictionary` and `Properties` wrappers, `concurrent.Map.asJava` of a non-TrieMap, LazyList builders and serialization, TrieMap under concurrency, BigDecimal with a non-default MathContext on the left operand, `StringOps` beyond `format` (measured by collections3), `CollisionProofHashMap` tree bins against a deliberately bad Ordering.

## Longer campaign?

Not on these files at this shape. The three findings came from constructs reached for the first time (a snapshot of a snapshot, an index one past the end, a null probe from Java); round 3 added 1,000 lines of coverage and found nothing. The stop rule was not reached: each round raised coverage. The remaining unreached lines are mostly the wrappers not generated yet and concurrent paths, which need a different harness.
