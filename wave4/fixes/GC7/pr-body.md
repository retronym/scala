`ArrayDeque.addAll`, `patchInPlace` and `subtractAll` read their argument while they change the deque, so an argument that reads the deque lazily, such as a view or an iterator of it, gives the wrong contents or throws:

```scala
val d = ArrayDeque(6, -2); d.addAll(d.view.reverse)                         // ArrayDeque(6, -2, -2, -2), expected (6, -2, -2, 6)
val e = ArrayDeque(1, 2, 3); e.patchInPlace(1, e, 1)                         // ArrayDeque(1, 1, 1, 3, 3), expected (1, 1, 2, 3, 3)
val f = ArrayDeque(1, 4); f.subtractAll(f.view.slice(0, 2))                 // IndexOutOfBoundsException, expected ArrayDeque()
val g = ArrayDeque(6, -2, 3); g.addAll(g.view.reverse.to(Stream))            // ArrayDeque(6, -2, 3, 3, 3, 3), expected (6, -2, 3, 3, -2, 6)
val h = ArrayDeque[Int](); h.addAll(Iterator(5, 6) ++ h.view.reverse.take(10)) // ArrayDeque(5, 6, 6, 6), expected (5, 6)
```

`Queue` and `Stack` inherit all three. `ArrayBuffer` throws `ConcurrentModificationException` for the same `patchInPlace` and `subtractAll` shapes, and `UnrolledBuffer.subtractAll` of a slice of itself leaves an element behind. `ListBuffer.patchInPlace` already gives the expected result, as it reads the patch into a fresh list before changing anything. The expected results are what the same call gives with the argument taken from an equal, separate buffer, which is how `b ++= b` already behaves for every buffer.

`ArrayDeque.addAll` appends while it reads, and a reversed view indexes from the end that is growing. `IndexedBuffer.patchInPlace` overwrites slots with `update` before it has read the rest of the patch, then hands the remaining iterator to `insertAll`. `Shrinkable.subtractAll` removes one element at a time, and a buffer shifts the elements its argument is still reading. `ArrayDeque.prependAll` is unaffected, as it writes only slots before the start until it is done.

The three methods now read a source that may see the receiver in full before the first change. A source is read as it is when it is strict and cannot see the receiver: an `Option`, an immutable collection other than a `LazyList` or `Stream`, a `mutable.ArraySeq` (which is what an `Array` argument arrives as), or an `ArrayBuffer`, `ArrayDeque`, `ListBuffer`, `UnrolledBuffer`, `mutable.HashSet` or `mutable.LinkedHashSet` other than the receiver. Anything else is copied into an `ArrayBuffer` first: every view and iterator, a `LazyList` or `Stream` (either may be built over the receiver and read lazily), a Java collection wrapper, and any other mutable collection. The rule holds for an empty receiver too, since a source can reach the receiver after the elements it produced first have been added, as the concatenation above does.

In `Shrinkable` the rule applies only when the receiver is a `Buffer`, so it covers `ListBuffer` and `UnrolledBuffer` as well; sets and maps are the open part of scala/bug#12121 and get a separate pull request with failing tests. `subtractAll` cannot be overridden in `ArrayDeque` instead, as the override's `ArrayDeque` result type is a new method that MiMa reports as forward incompatible. `ArrayDeque.from` fills a new deque directly, since a source cannot read a deque that did not exist when it was made.

Cost, in bytes allocated per call on JDK 21, appending three elements to a one-element deque with spare capacity, before and after this change:

| source | before | after |
|---|---|---|
| `List(1, 2, 3).iterator` | 32 | 208 |
| `arrayBuffer.view` | 88 | 248 |
| `List(1, 2, 3).iterator`, empty deque | 0 | 176 |
| `ArrayDeque.from(List(1, 2, 3).iterator)` | 184 | 168 |

`deque ++= Some(x)`, `deque ++= List(...)`, `deque ++= otherArrayBuffer` and `buffer --= immutableSet` allocate what they did before. `Queue.from` and `Stack.from` of a view or an iterator, and `addAll` of one through a deque's builder, now copy it once. `patchInPlace` copies a lazy patch once; `ArrayBuffer.insertAll` already copied an iterator for the part after the replaced slots. `subtractAll` on a buffer copies a lazy argument once, next to removals that are each linear in the buffer's length. No result changes for a source that is not derived from the receiver.

A decision for the owner: copying the source first gives the result of the same call with an equal, separate source. The alternative is to fail fast, as `ArrayBuffer`'s views already do: they throw `ConcurrentModificationException` when the buffer changes under them. Failing fast costs nothing for any source, but needs mutation tracking in the views and iterators of `ArrayDeque` and `UnrolledBuffer`, and turns the calls shown above into exceptions.

Long-standing since 2.13.0, not a regression. This merges cleanly with retronym/scala#534 and retronym/scala#548, which cover `insertAll` and `prependAll` of `ArrayBuffer` and `ArrayDeque` and `patchInPlace` at index 0; their `insertAll` checks predate this rule and still trust a `LazyList` or `Stream`. The `UnrolledBuffer` cells are retronym/scala#759 and `IndexedBuffer.flatMapInPlace` is retronym/scala#760, with the same rule. Possible follow-up: once these land, one shared predicate in place of the match each site repeats; a helper needs a new class or member, which MiMa reports.

Part of scala/bug#12121.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GC7). Prepared with Claude.
