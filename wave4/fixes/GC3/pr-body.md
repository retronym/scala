`drop` on the iterator of certain empty Ranges, or on an exhausted iterator of a Range ending at `Int.MaxValue`, makes the iterator non-empty. `(Int.MinValue until Int.MinValue).iterator.drop(1)` yields -2147483647, -2147483646 and so on for 2^32 elements; `(10 until Int.MaxValue by -1).iterator.drop(3).take(2)` yields 7, 6; an iterator of `Int.MaxValue - 2 to Int.MaxValue` that has been run to the end has `hasNext` true after `drop(1)`. `indexWhere(p, from)` and `segmentLength(p, from)` on a Range go through `iterator.drop(from)`, so `(Int.MinValue to 10).take(0).indexWhere(_ => true, 1)` is 1 and `segmentLength(_ => true, 1)` on such a range walks 2^32 elements (about 3 s) before returning. An iterator of an empty range should stay empty.

`RangeIterator.drop` (`Range.scala`) sets `_hasNext` from a comparison of the new position with `lastElement` and never looks at whether the iterator was already exhausted. For an empty range `lastElement` is not an element and can wrap (`Int.MinValue until Int.MinValue` has `lastElement == Int.MaxValue`); after the last element of a range ending at `Int.MaxValue`, `next()` leaves the position wrapped to `Int.MinValue`. Empty ranges away from the Int bounds are unaffected. Long-standing: the `drop` override dates from 2.13.0.

`drop` now returns the iterator unchanged when it is exhausted.

retronym/scala#558 fixes an overflow of the position in the same method for non-empty ranges; this change is independent of it and merges cleanly with it.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GC3). Prepared with Claude.
