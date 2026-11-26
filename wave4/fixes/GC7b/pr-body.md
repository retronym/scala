Stacked on retronym/scala#538, which copies an `UnrolledBuffer` before inserting it into itself; this widens that copy from the buffer itself to any source that may read the buffer, and adds `addAll`.

Every view or iterator of an `UnrolledBuffer` still goes wrong when passed to the buffer's own bulk methods:

```scala
val a = UnrolledBuffer(1); a.prependAll(a.view.map(_ + 1))           // UnrolledBuffer(1), expected (2, 1)
val b = UnrolledBuffer(1, 2, 3); b.insertAll(1, b.iterator)         // (1, 1, 2, 3), expected (1, 1, 2, 3, 2, 3)
val c = UnrolledBuffer(1, 2, 3); c.patchInPlace(1, c.view.map(_ * 10), 1) // (1, 10, 3), expected (1, 10, 20, 30, 3)
val d = UnrolledBuffer(5, -1); d.addAll(d.view.filter(_ > 0))       // (5, -1, 5, 5), expected (5, -1, 5)
val e = UnrolledBuffer(3, 4); e.addAll(e.iterator)                  // never returns, OutOfMemoryError
```

`insertAll` splits the node at the insertion point and detaches the tail while it reads the source, so a source walking the buffer sees a truncated chain; `prependAll` and `patchInPlace` reach the same code. `addAll` is `Growable`'s loop, and the buffer's iterator follows the nodes appended after it was created, so an iterator or unbounded view of the buffer never runs out. The expected results are what the same call gives with the source taken from an equal, separate buffer, as `ArrayBuffer` and `ListBuffer` give.

`unaliased` now copies the source into an `ArrayBuffer` unless it cannot read the buffer: an immutable collection other than a `LazyList`, a `mutable.ArraySeq`, an `ArrayBuffer`, an `ArrayDeque`, or another `UnrolledBuffer`. That is the test retronym/scala#534 and retronym/scala#548 apply in `insertAll`, with `LazyList` added because one built from an iterator of the buffer reads it lazily. `addAll` applies the same rule in `Growable.addAll`, guarded by the receiver being an `UnrolledBuffer` with elements, because an override in `UnrolledBuffer` returns `UnrolledBuffer` and MiMa reports it as forward incompatible. The buffer's iterator is left as it is: making it stop at the length it started with would fix `addAll` without a copy, but would change code that iterates over the buffer while appending to it.

Cost: such a source is copied once into an `ArrayBuffer` before an insert, a patch, or an append to a non-empty buffer. Appending to an empty buffer, which is how `UnrolledBuffer.from` and its builder fill one, skips the copy, as every view or iterator of it is then empty. Sources in the safe list cost what they did, and no result changes for a source that is not derived from the buffer. Every other `Growable` pays one type test in `addAll`.

`subtractAll` of a view of the buffer is fixed for all buffers in the separate pull request for `ArrayDeque`, `patchInPlace` and `subtractAll`, which also holds the `UnrolledBuffer` test for it. Long-standing since 2.13.0, not a regression. Insertion at a node boundary is a separate bug (retronym/scala#321), so the test stays within one node.

Part of scala/bug#12121.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GC7). Prepared with Claude.
