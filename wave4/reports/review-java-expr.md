scala/scala · claude/eager-jackson-1fc898 (worktree eager-jackson-1fc898) · local only, nothing committed · PR none · unverified: the GJ1 one-line fix was not built (no sbt); GF1's 2.13.0 origin is inferred (2.12.20 compiles, 2.13.12 onwards crash)

Review java-expr: GJ1 to GJ4, GF1, GF2. All six reproduce on the integration build, the base and 2.13.18 as their findings state.

Verdicts: 6 real; none doubtful, by design, wrong or duplicate. Severity: 1 high, 2 medium, 3 low.

Most important:

- GJ1 (medium, regression in unreleased 2.13.19). Introduced by scala/scala PR 11329 (lrytz, sealed Java children computed from permitted classes). The cause is in the Java parser, not the namer or the pattern matcher: `classDecl` and `interfaceDecl` test `mods.hasFlag(SEALED)`, where `SEALED` is the token number 45 (PROTECTED | PRIVATE | ABSTRACT | FINAL bits), a mistake from PR 10348 (2.13.11) that was harmless until 11329 made the per-unit class set the only source of children. A sealed interface without `permits` gets children only if some class in the file is final, abstract, private or protected; records, enums and `non-sealed` classes all fail. Fix: `Flags.SEALED` at two sites. The hunter's stated cause is wrong.
- GF1 (high, crash, since 2.13.0). `RefChecks.transformIf` replaces a constant-condition `if` by its branch without keeping the `if`'s type; patmat, after refchecks since PR 7260, binds a `Nothing` scrutinee and `3 == x1` fails overload resolution silently. Ordinary program: `object Config { final val Enabled = false }` with `(if (Config.Enabled) level() else sys.error("disabled")) match { case 0 => "off"; case 1 => "on"; case _ => "other" }`.
- GJ2 (medium). Understated: `c.args.toList`, `.map`, `.mkString` on a varargs record accessor all fail in joint compilation.

Low: GJ3 (known parser limitation; fix together with J5, separate from PR 288), GJ4 (no one writes it; Scala 3 rejects it too), GF2 (missing error only; really any literal-typed expression such as `K.three()` with `def three(): 3` is accepted; 2.12 and Scala 3 reject; separate from PR 606; since 2.13.0 literal types).
