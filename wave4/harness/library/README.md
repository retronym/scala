# library: ordinary usage of the library files earlier spikes left unreached

Programs of calls checked step by step against obviously-right models; on the first mismatch a program is shrunk by deleting calls while the signature (family, operation, kind of expected and actual result) stays the same. No dependencies beyond scala-library (JaCoCo only for measuring).

- `Maps.scala`: mutable `LongMap`, `AnyRefMap`, `OpenHashMap`, `CollisionProofHashMap` (keys with colliding hashes and an Ordering), `TrieMap` with snapshots and read-only snapshots kept live, against an immutable `TreeMap` per live map; immutable `IntMap`/`LongMap` (including `unionWith`, `intersectionWith`, `modifyOrRemove`, `updateWith`, `firstKey`/`lastKey` in unsigned order), `TreeSeqMap` in both orders, `VectorMap`, `ListMap` against a vector of entries in the documented order, every version kept to check persistence.
- `Queues.scala`: `PriorityQueue` against a sorted list (clones and `reverse` kept live), mutable `Stack`/`Queue` and immutable `Queue` against a `List`.
- `Convert.scala`: Scala collections as `java.util` List/Set/Map against a java.util model with the same contract (ArrayList, `Arrays.asList`, unmodifiable views, HashSet, TreeMap, ConcurrentHashMap), accepting the contracts' optional exceptions; Java collections as Scala Buffer/Set/Map/concurrent.Map against ArrayBuffer/Set/TreeMap models.
- `Streams.scala`: steppers over 48 collection shapes drained after a random tree of `trySplit` (every element once, ORDERED order, SIZED/SUBSIZED exact, DISTINCT, SORTED), the Spliterator and Java iterator views; sequential and parallel streams; accumulator programs against ArrayBuffer.
- `Misc.scala`: Iterator combinator chains against List; LazyList evaluation counts against the Scaladoc tags and "computed in order, never skipped"; `Sorting` and `sorted`/`sortInPlace`/`sortBy` against `java.util.Arrays.sort` (stability with a key-only ordering); `BigInt`/`BigDecimal` against `java.math`; `Random` bounds and `shuffle`; `StringOps.format`/`formatLocal`; `StringView`.
- `LibDiff.scala` (driver, shrinker, watchdog), `Families.scala`, `Extra.scala` (registry), `cov.py` (lines covered per scope file, largest unreached methods).

Run: `run.sh <build/pack> <family[,family]|all> <seconds> [seed]`; `OUT=<dir>` keeps the compiled harness, `JACOCO=1 TAG=<name>` writes `<dir>/<name>.exec`. Exclusions for known defects (WL1, WL2, WL3, LM7) and for unspecified behaviour are counted and printed at the end.

Measured cases per second, one JVM under JaCoCo: maps and queues 1,600 to 6,000; wrappers and steppers 1,000 to 15,000; Iterator 68,000; LazyList 160,000; BigInt/BigDecimal 118,000; Sorting 2,400.
