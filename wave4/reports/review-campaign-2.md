eager-jackson-1fc898 · claude/eager-jackson-1fc898 · local only (wave4/reviews/{WX1,WP1,WP2,WP3}.json, this report) · PR none · unverified: the 2.13.11 commit behind WP1, and the proposed fixes are untested

# Review campaign-2: WX1, WP1, WP2, WP3

All four reproduce on w4-int, w3-base and 2.13.18. Verdicts: 4 real. Severity: 1 critical (WP3), 1 medium (WX1), 2 low (WP1, WP2). No duplicates.

WP3 (critical): VerifyError at load time, a regression since 2.13.9 / 2.12.17 (2.13.8 and 2.12.16 are correct). It does not need a partial function. Erasure boxes a value class from outside, `new VC(...)`, around an expression that holds a try which uncurry left in place, because uncurry judged the stack to be empty there. Two routes reach it:
- A match typed at the value class whose result goes into an Object-typed matchEnd label. scala/scala#10028 boxes `Block(_, LabelDef)` from outside, so a partial function case `case x => x match { case d: Double => Meters(d); case s: String => try Meters(s.toDouble) catch {...} }` (for example in `collect`) fails.
- A type ascription in a boxed position, `def a(s: String): Any = (try Meters(...) catch {...}): Meters`. This route is older: it already fails on 2.11.12 and 2.13.8.

The guard in the hunter's program only stops patmat from making a switch. The fix belongs in erasure: bind the expression to a local before `new VC(x)`. It should be a separate pull request in the CF3 family, not part of retronym/scala#605. #605 is already on w4-int and does not fix this, and uncurry cannot predict the box.

WX1 (medium): a structural call to a Unit method through a refinement whose result is `Any` returns null. The specification rules this out: under 3.5.2 Unit conforms to Any, and `()` is the only value of Unit. CleanUp.fixResult passes the null through. Scala 3.7.4, 2.12.20 and 2.11.12 behave the same. No scala/bug ticket found. Nearest earlier finding: XC1 (Unit array elements are null). No XC family finding covers results.

WP1 (low): not a checker false positive, and the hunter's mechanism is wrong. Patmat gives the default label the type of its body (`()Double`). After erasure the body and the jumps to the label are typed Object, but the label symbol's type is still `()Double`. The same shape with no boxing checks clean. Fix: give the label `pt`. First seen in 2.13.11. Confirmed that on the base and 2.13.18 the check throws NPEs and writes no class files.

WP2 (low): not a false positive either. After extmethods, the Select `s.get` still points at the member of the old refinement, which is owned by class V0 and typed with the class's T. Erasure hides it, and I found no effect on bytecode. Also present in 2.12.20.

Ordinary repros for WP3 are in the scratchpad: `/private/tmp/claude-501/-Users-jzaugg-virtus-code--worktrees-scala-eager-jackson-1fc898/75f6ecbb-3604-4315-b92e-a7e977e7671a/scratchpad/w4-review-campaign-2/wp3v/Ord.scala`, `/private/tmp/claude-501/-Users-jzaugg-virtus-code--worktrees-scala-eager-jackson-1fc898/75f6ecbb-3604-4315-b92e-a7e977e7671a/scratchpad/w4-review-campaign-2/wp3v/M2.scala` and `/private/tmp/claude-501/-Users-jzaugg-virtus-code--worktrees-scala-eager-jackson-1fc898/75f6ecbb-3604-4315-b92e-a7e977e7671a/scratchpad/w4-review-campaign-2/wp3v/Ua.scala`.
