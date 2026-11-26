Stacked on retronym/scala#538, which copies an `UnrolledBuffer` before inserting it into itself; this widens that copy from the buffer itself to any source that may read the buffer, and adds `addAll`.

Every view or iterator of an `UnrolledBuffer` still goes wrong when passed to the buffer's own bulk methods:

```scala
val a = UnrolledBuffer(1); a.prependAll(a.view.map(_ + 1))                     // UnrolledBuffer(1), expected (2, 1)
val b = UnrolledBuffer(1, 2, 3); b.insertAll(1, b.iterator)                   // (1, 1, 2, 3), expected (1, 1, 2, 3, 2, 3)
val c = UnrolledBuffer(1, 2, 3); c.patchInPlace(1, c.view.map(_ * 10), 1)     // (1, 10, 3), expected (1, 10, 20, 30, 3)
val d = UnrolledBuffer(5, -1); d.addAll(d.view.filter(_ > 0))                 // (5, -1, 5, 5), expected (5, -1, 5)
val e = UnrolledBuffer(3, 4); e.addAll(e.iterator)                            // never returns, OutOfMemoryError
val f = UnrolledBuffer(5, -1); f.addAll(f.view.filter(_ > 0).to(Stream))      // (5, -1, 5, 5), expected (5, -1, 5)
val g = UnrolledBuffer[Int](); g.addAll(Iterator(5, 6) ++ g.iterator.take(10)) // twelve elements, expected (5, 6)
```

`insertAll` splits the node at the insertion point and detaches the tail while it reads the source, so a source walking the buffer sees a truncated chain; `prependAll` and `patchInPlace` reach the same code. `addAll` is `Growable`'s loop, and the buffer's iterator follows the nodes appended after it was created, so an iterator or unbounded view of the buffer never runs out. The expected results are what the same call gives with the source taken from an equal, separate buffer, as `ArrayBuffer` and `ListBuffer` give.

`unaliased`, which `insertAll` and `patchInPlace` use, and `addAll` now read a source that may see the buffer in full before the first change. A source is read as it is when it is strict and cannot see the receiver: an `Option`, an immutable collection other than a `LazyList` or `Stream`, a `mutable.ArraySeq` (which is what an `Array` argument arrives as), or an `ArrayBuffer`, `ArrayDeque`, `ListBuffer`, `UnrolledBuffer`, `mutable.HashSet` or `mutable.LinkedHashSet` other than the receiver. Anything else is copied into an `ArrayBuffer` first: every view and iterator, a `LazyList` or `Stream` (either may be built over the receiver and read lazily), a Java collection wrapper, and any other mutable collection. The rule holds for an empty receiver too, since a source can reach the receiver after the elements it produced first have been added, as the concatenation above does. `UnrolledBuffer.from` fills a new buffer directly, since a source cannot read a buffer that did not exist when it was made.

`addAll` applies the rule in `Growable.addAll`, guarded by `this.isInstanceOf[UnrolledBuffer[_]]`, because an override in `UnrolledBuffer` returns `UnrolledBuffer` and MiMa reports it as forward incompatible. `UnrolledBuffer` is sealed, so no outside subclass is affected; every other `Growable` pays one type test per `addAll`.

Cost, in bytes allocated per call on JDK 21, appending three elements to a one-element buffer, before and after this change:

| source | before | after |
|---|---|---|
| `List(1, 2, 3).iterator` | 192 | 296 |
| `arrayBuffer.view` | 248 | 352 |
| `UnrolledBuffer.from(List(1, 2, 3).iterator)` | 208 | 208 |

`buffer ++= Some(x)` and `buffer ++= List(...)` allocate what they did before. An `UnrolledBuffer` is its own builder, so `addAll` of a view or an iterator through the builder, as `flatMap` does, now copies it once. No result changes for a source that is not derived from the buffer.

Two decisions for the owner. First, copying the source first gives the result of the same call with an equal, separate source. The alternative is to fail fast, as `ArrayBuffer`'s views already do: they throw `ConcurrentModificationException` when the buffer changes under them. Failing fast costs nothing for any source, but needs mutation tracking in the views and iterators of `ArrayDeque` and `UnrolledBuffer`, and turns the calls shown above into exceptions. Second, the type test in `Growable.addAll`: a trait naming one of its subclasses is unusual. The alternatives are the `addAll` override, once forward compatibility allows it, or an `UnrolledBuffer` iterator that stops at the length it started with, which fixes `addAll` without a copy but changes code that iterates over the buffer while appending to it.

`subtractAll` of a view of the buffer is fixed for all buffers in retronym/scala#758, which also holds the `UnrolledBuffer` test for it; retronym/scala#760 applies the same rule to `flatMapInPlace`. Long-standing since 2.13.0, not a regression. Insertion at a node boundary is a separate bug (retronym/scala#321), so the test stays within one node.

Part of scala/bug#12121.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GC7). Prepared with Claude.
