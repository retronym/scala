This pull request holds failing tests and no fix: which behaviour is right is the open question of scala/bug#12121.

`addAll` and `subtractAll` of a mutable set or map read a mapped view of the receiver while they change it, and add or remove the wrong elements:

```scala
val s = HashSet(1, 2, 3); s.subtractAll(s.view.map(_ + 1))        // HashSet(1, 3), expected HashSet(1)
val t = TreeSet(1 to 40: _*); t.subtractAll(t.view.map(_ + 1))    // TreeSet(1, 4, 7, 11, 21, 31), expected TreeSet(1)
val m = HashMap(1 -> 2, 2 -> 3, 3 -> 4)
m.addAll(m.view.map { case (k, v) => (k + 1, v + 10) })           // 8 entries, expected (1 -> 2, 2 -> 12, 3 -> 13, 4 -> 14)
val n = HashMap(1 -> 2, 2 -> 3, 3 -> 4); n.subtractAll(n.keySet.view.map(_ + 1)) // HashMap(1 -> 2, 3 -> 4), expected HashMap(1 -> 2)
```

The tests cover `subtractAll` on `HashSet`, `TreeSet`, `LinkedHashSet`, `HashMap`, `TreeMap`, `LinkedHashMap` and `LongMap`, and `addAll` on `HashMap` and `LongMap`; retronym/scala#535 already holds `addAll` tests for `HashSet`, `LinkedHashSet`, `TreeSet`, `LinkedHashMap` and `TreeMap`. `CollisionProofHashMap`, `AnyRefMap` and the `java.util` map wrappers also go wrong in `addAll`. The expected results are what the call gives with the view taken from an equal, separate collection, which is also what the immutable collections and `mutable.BitSet` give. `addAll` keeps consuming the entries it has just added; `subtractAll` iterates over the structure it is unlinking, skipping or over-removing. For sets, `subtractAll` goes wrong only with a mapped source: a plain or filtered view or iterator of the receiver (`s --= s.iterator.filter(p)`) gives the right result on every Scala set. Long-standing since 2.13.0, not a regression.

The buffers are fixed in separate pull requests by copying a source that may read the receiver before the first write, with the test retronym/scala#534 and retronym/scala#548 use: anything other than an immutable collection (except `LazyList`), a wrapped array or another array-backed buffer is copied. For sets and maps that is not plainly compatible. `s ++= iterator`, `m ++= view` and `s --= iterator` are the common uses of these methods, they are linear and cheap today, and the copy would roughly double their cost for every lazy source to fix a call shape that is rare; for buffers the copy sits next to work that is already larger or is limited to sources that cannot be told apart. The options:

- Snapshot: copy a source that is not known to be safe in `addAll` and `subtractAll`, skipping the copy for an empty receiver so that building a collection costs what it did. Correct for every source, at the cost above for non-empty receivers.
- Fail fast: count modifications in `HashSet`, `HashMap`, `LinkedHashMap` and the tree collections and check the count in their iterators, as `ArrayBuffer` and `ListBuffer` do. These calls then throw `ConcurrentModificationException`, and so does iteration while modifying, which works today for some of these classes; every iterator pays a check.
- Document a view or iterator of the receiver as unsupported for these methods, and drop these tests and those of retronym/scala#535.

Part of scala/bug#12121.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GC7). Prepared with Claude.
