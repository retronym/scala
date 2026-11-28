A `return` evaluated while the enclosing expression has operands on the stack, inside a `try` with a finalizer or inside `synchronized`, produced a method whose stack heights disagree at the finalizer:

```scala
def m(log: StringBuilder, s: String): Int =
  try {
    log.append(if (s == null) return -1 else s)
    if (s.isEmpty) return 0
    s.length
  } finally println("fin")
```

Depending on the shape and the ASM version this is a compiler crash ("Error while emitting ... Index 0 out of bounds for length 0" from ASM's frame computation) or a class that fails verification with "Inconsistent stackmap frames". The ASM 9.11.1 upgrade in the unreleased 2.13.19 moved some programs from one symptom to the other. The method should behave as Java's equivalent: run the finalizer and return the value. This is scala/bug#10183, open since 2.12.0; 2.11 inlined the finalizer at each `return` and so had no shared jump target.

`genReturn` in `BCodeBodyBuilder` stores the returned value and jumps to the cleanup copy of the innermost finalizer (or monitor exit). Every jump to that copy must arrive with the same stack, and a `return` in operand position (an argument after the receiver, an operand of `+`, a constructor argument after `NEW`/`DUP`) arrives with the earlier operands still there, while another `return`, or the same one reached through the exception-handler copy of a finalizer, arrives with none. As lrytz noted on the ticket, the fix is to drop those operands before the jump, which the back end could not do because it does not track the stack.

After a method is emitted, if any `return` jumped to a cleanup, the back end now runs a `BasicAnalyzer` over it to find the stack at each such jump and inserts `POP`/`POP2` for the values found. During the analysis each of these jumps is preceded by a temporary `throw`, so that the cleanup blocks, whose incoming stacks are not yet consistent, are not reached through them; a `return` inside a cleanup block (a `return` in the finalizer of a `try` that itself had an early `return`) is reached once the jumps into that block are fixed, so the analysis repeats until nothing changes. Methods without a `return` under a finalizer are not analysed, and a `return` whose stack is empty gets the same code as before. If the analysis fails the method is left as it was.

This also covers the return-in-finalizer shape of retronym/scala#618 (`try 2L finally { return 6L }` inside another `try`/`finally`), where the try's value is on the stack when the normal-path copy of the finalizer returns. The two changes touch different code and compose: with both, the spill in retronym/scala#618 takes the try's own value off the stack and this change drops any operands that remain.

Possible alternatives, not taken: tracking the operand stack throughout the code generator, as Scala 3's back end does for its labelled jumps, would avoid the analysis but touches every `genLoad` path; lifting a `return` out of operand position in UnCurry would need the preceding operands bound to locals, as the try-lifting does for `try` in argument position.

Fixes scala/bug#10183

Found in the fourth wave of the coordinated code review of 2.13.x (finding WP5). Prepared with Claude.
