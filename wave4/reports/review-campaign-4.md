eager-jackson-1fc898 · claude/eager-jackson-1fc898 · local only (reviews uncommitted) · PR none · unverified: WP7 widen fix not built or tested; WP6 mechanism taken from the finding

# Review campaign-4 (WP4 to WP9)

Verdicts: real 4 (WP4, WP7, WP8, WP9), real and known 2 (WP5: scala/bug#10183; WP6: remaining part of retronym/scala#460). Severity: critical 1 (WP5), high 1 (WP7), low 4. No doubtful, by design, wrong or duplicate.

Most important:

- WP7 (high): `(if (c) Meters(d) else Meters(0)).value` or `== other` crashes erasure (mergePrefixAndArgs: unsupported tps) on w4-int, w3-base, 2.13.18; Scala 3.7.4 compiles it. The InjectDerivedValue case of `Eraser.typed1` gives `V0(7)` the type `ErasedValueType(V0, Int(7))`; when erasure re-types the If/match as the qualifier of `==`, `!=` or a field accessor (pt=?), lub cannot merge it with `ErasedValueType(V0, Int)`. No `==` rewrite is involved. A final val constant triggers it too; vals in both branches, a val binding, an argument position and equals/hashCode/toString (extension calls) do not. `V0(7: Int)` avoids it, so widening the argument type should fix it. Regression in 2.11.0 from SI-6385 (scala/scala PR 3082); 2.10.7 compiles it. No ticket or earlier finding.
- WP5 (critical): this is lrytz's minimization in scala/bug#10183, open since 2017. #618 is on w4-int and says it does not cover this form. The fix belongs in a separate pull request, not a wider #618. The crash on w4-int against the VerifyError on 2.13.18 comes from the upstream ASM 9.11.1 upgrade (c8bc25a6d0), not from a draft fix: the 2.13.18 compiler with scala-asm 9.11.1 crashes and with 9.10.1 does not. Some inputs go the other way. `try { log.append(if (s == null) return -1 else s); if (s.isEmpty) return 0; s.length } finally println("fin")` compiles and fails verification on w4-int and w3-base, and crashes on 2.13.18 and Scala 3.7.4. The finding's `getOrElse(return x)` example is wrong: a by-name argument compiles correctly.

-Ycheck reports, all with no effect outside -Ycheck on w4-int:

- WP4: real ill-typed tree (thunk result typed as the singleton type of what fields turns into a method). Belongs with retronym/scala#460 (its by-name widening part).
- WP6: checker false positive (re-typing uses applyOrElse's result type). Belongs with #460.
- WP8: real ill-typed tree made by uncurry: the handler uses type parameter B where the body uses its skolem (confirmed with -uniqid). Belongs with #509 (skolem part).
- WP9: checker false positive (typer's `List()` to `Nil` rewrite keeps List[Int]; re-typing gives Nil.type). Belongs with #509.
