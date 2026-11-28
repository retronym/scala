Stacked on retronym/scala#555, which moves `Range.posOf` to Long arithmetic; this pull request adds one commit on top of it and should be reviewed and merged after it.

In a Range of more than `Int.MaxValue` elements, `lastIndexOf(elem, end)` and `indexOf(elem, from)` return a negative number other than -1 for an element whose position exceeds `Int.MaxValue`: `(Int.MinValue to 51).lastIndexOf(3, 1073741823)` and `(Int.MinValue to 51).indexOf(3, Int.MinValue)` are both -2147483645. Both methods should return a valid index or -1; callers that compare the result with -1 take this for a hit. Such ranges cannot be indexed (`length` throws), so this needs a search of an oversized range with an explicit `end`, or a negative `from`; the defaults are safe (`lastIndexOf` throws from `length`, `indexOf` returns -1).

`Range.posOf` (`Range.scala`) computes the position and narrows it to Int. With retronym/scala#555 the subtraction is done in Long, so only the narrowing is left to wrap; before it, the same number came from Int wrap-around. Long-standing: `posOf` dates from 2.13.0.

`posOf` now returns -1 when the position does not fit in an Int, which is what `indexOf` with the default `from` already returned for such an element. Throwing as `length` does would be the other choice; -1 keeps the methods total.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GC4). Prepared with Claude.
