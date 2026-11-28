`IntAccumulator`, `LongAccumulator`, `DoubleAccumulator` and `AnyAccumulator` are `mutable.Seq`s whose `apply(ix: Long)` and `update(idx: Long, elem)` did not check the index against the size. When the index fell in the current chunk's range they indexed straight into the chunk's array, which has spare capacity past the size, so `acc(acc.length)` returned 0, 0L, 0.0 or null, and `acc(acc.length) = x` returned normally and was overwritten by the next append. Indices further out threw `ArrayIndexOutOfBoundsException` only once they left the array.

`Seq.apply` and `mutable.Seq.update` declare `@throws IndexOutOfBoundsException`, the `apply` Scaladoc says "Retrieves the `ix`th element", and every other `mutable.Seq` in the library checks the index. Correct code is unaffected; a caller with an off-by-one got a default value or a lost write instead of an error.

Fix: `apply` and `update` in the four classes throw `IndexOutOfBoundsException` (with the message `CommonErrors.indexOutOfBounds` uses) for an index outside `0 until size`. The `Int` overloads delegate to the `Long` ones and get the check too.

On cost: this is one comparison against `totalSize` on a path that already does arithmetic and a branch, the same check `ArrayBuffer.apply` makes. The accumulators exist to make appending and unboxed traversal (through `stepper` and `foreach`) cheap, and neither of those goes through `apply`. A behaviour change only for indices that were already wrong, so I judged it clear cut; the alternative is to document `apply` and `update` as unchecked, which leaves the lost write.

In the library since 2.13.0, when the accumulators came into `scala.jdk` from scala-java8-compat.

Found in the fourth wave of the coordinated code review of 2.13.x (finding WL2). Prepared with Claude.
