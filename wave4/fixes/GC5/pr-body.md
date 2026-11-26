`Range.BigDecimal.inclusive(0.34, -21474836.11, -0.01)` has 2147483646 elements, but its `length` throws `IllegalArgumentException: More than Int.MaxValue elements.`. The same holds for any BigDecimal range from a fractional start between -1 and 1 across zero whose element count is within twice `|start / step|` of `Int.MaxValue` (68 here), and therefore for `take(Int.MaxValue)` of a longer range of that shape, whose `length`, `last`, `apply` and `min` all throw. Only BigDecimal reaches this, since integral types have no start strictly between 0 and ±1, and only next to the `Int.MaxValue` limit.

For a range whose start and end have different signs, `NumericRange.count` (`NumericRange.scala`) counts in three pieces: from the start to a waypoint near zero, one step across zero, and from there to the end, checking each piece against `Int.MaxValue`. When the start is already closer to zero than the waypoint limit (1 or -1), the first piece took `quot(start, step)` steps. That number is negative (0.34 / -0.01 is -34), so the waypoint moved away from zero to 0.68, and the last piece was 68 steps too long. The total came out right, but the last piece alone could exceed `Int.MaxValue`. Not a regression: the first-piece logic was changed for scala/bug#12073 in 2.13.4, and 2.13.3 throws for the same ranges.

The first piece now takes `quot(-start, step)` steps, which moves the start towards zero and is never negative.

Related open pull requests on other defects in `NumericRange.count`, which merge cleanly with this one: retronym/scala#533, retronym/scala#655.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GC5). Prepared with Claude.
