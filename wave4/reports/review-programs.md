scala (worktree eager-jackson-1fc898) · claude/eager-jackson-1fc898 · local only, nothing committed · PR none · unverified: that one change fixes both GE3 and GE8 (by code reading only); the changes behind the 2.13.0 regressions GE1 and GE8 not identified

Review of GE1 to GE8 (generated programs). All eight reproduce on the integration build, the base and 2.13.18, except GE6 as claimed.

Verdicts: real 5 (GE1, GE2, GE3, GE5, GE6); real, known 2 (GE4, GE7); duplicate 1 (GE8 of GE3). No doubtful, by design or wrong. Severity: high 6, medium 2 (GE4, GE5).

Most important:
- GE6: regression in unreleased 2.13.19 from the ASM 9.11.1 upgrade (scala/scala#11327). ASM counts every frame allocated, garbage included, in an int; scalac can only raise the limit to Int.MaxValue. The failing method is 8153 instructions with 158 handlers. A hand-written method of 220 consecutive try/catch lines crashes too; 2.13.18 compiles it in 66 s.
- GE7, shrunk: a guarded match, or a block with a local val, in the constructor of a class with a specialized field. Known as scala/bug#7574 and #10096. Constructor statements copied into the specialized subclass are retyped with the class as owner.
- GE8, shrunk: `trait M[@specialized(Int) K, @specialized(Int) V] { def getOrElseUpdate(k: K, default: => V): Unit }` crashes. Same cause as GE3: a by-name parameter's type variable is not counted before uncurry and is counted after. Regression from 2.12.
- GE2: belongs on retronym/scala#354 as a wider fix; that case matches `super.m(args)` but not `super.m[X](args)`. A trait parent with a specialized method type parameter loops as well; the pull request tests that shape only with a class parent.
- GE1: regression from 2.12; the super accessor keeps the @specialized annotation and is itself specialized.

Known: GE4 is scala/bug#8633 (any `collect { case ... }` in a duplicated body).

Hunter errors: GE8's method has no type parameter of its own; GE6's method is not large; GE5 needs no local def; GE4 needs no guard.

Changed in finding directories: `repro/Test.scala` of GE7 and GE8, `actual.txt` of GE8 (GE7's is unchanged).
