# CI triage 2, retronym/scala PRs 747-785 (except 769)

Single check `validate` (JDK 8, fatal warnings), read for the current head commit. Read-only; nothing re-run. 38 PRs: 26 pass, 5 fail, 7 still in progress.

Labels omit `code-review-2026-11-19`. Base is `2.13.x` unless stated.

| PR | State | Labels | validate (current head) | Class | First error |
|---|---|---|---|---|---|
| 747 | ready | medium, collections | fail (library compile) | (a) | `NumericRange.scala:535:17` and `:36`: `method signum in trait Numeric is deprecated (since 2.13.0): use sign method instead`. Same error on the new head f6563d5 as on the old one |
| 748 | ready | low, collections | pass | - | - |
| 749 | ready | medium, no-fix, collections | fail (testAll1) | (b) | `HashMapTest.updateWithModifyingTheMap` and `LinkedHashMapTest.updateWithModifyingTheMap`: `remove the key, Some expected:<List((1,7))> but was:<List()>`. Only these two fail; both files are PR-added tests |
| 750 | ready | high, typechecker | in progress | - | - |
| 751, 752 | ready | medium/low, java-sources | pass | - | - |
| 753 | ready | upstreamed, medium, java-sources, regression | pass | - | - |
| 754 | ready, base `review/w2-li1-range-indexof-overflow` | low, collections | pass (CI did run on the stacked base) | - | - |
| 755, 756 | ready | low | pass | - | - |
| 757 | ready | low, no-fix, java-sources | pass | no-fix but green | Test.scala has `//> using jvm 9+` (uses `@Deprecated(since=...)`), so partest skips it on JDK 8; the bug is not exercised on CI |
| 758 | draft | high, collections | pass | - | allocation tests pass on JDK 8 |
| 759 | draft, base `review/w2-lm3-unrolledbuffer-insertall-self` | high, collections | pass (CI ran) | - | - |
| 760 | ready | high, collections | pass | - | - |
| 761 | ready | high, no-fix, collections | fail (testAll1) | (b) | 8 failures, all PR-added tests: `HashMapTest`/`LongMapTest.bulkOperationsWithMappedViewOfSelf`, `{TreeSet,TreeMap,LinkedHashSet,LinkedHashMap,HashSet}Test.subtractAllMappedViewOfSelf` (e.g. `expected:<HashSet(1)> but was:<HashSet(1, 3)>`). Nothing else failed |
| 762, 763, 764 | ready | high, specialization | pass | - | - |
| 765 | ready | medium, no-fix, specialization | fail (testAll2) | (b) | `run/specialized-partial-function-literal.scala [compilation failed]`: `type mismatch; found: x1.type (with underlying type Int) required: A1`. The only partest failure; the PR adds this test |
| 766 | ready | medium, specialization | pass | - | - |
| 767 | ready | high, no-fix, optimizer, regression | fail (testAll2) | (b) | `run/opt-analysis-frame-limit.scala [output differs]`: `error: Error while emitting C$ / Too many allocated bytes / false` instead of `true`. The only partest failure; PR adds it |
| 768 | ready | high, specialization | pass | - | - |
| 770 | ready | medium, collections | pass | - | - |
| 771 | ready | low, library | pass | - | - |
| 772 | ready | medium, no-fix, java-interop | fail (testAll1) | (b) | `CollectionConvertersTest.asJava of an Int or Long keyed collection does not read a null key as 0`: `expected:<false> but was:<true>` (line 85). Total 3468, Failed 1; PR adds only this test file |
| 773, 774, 775, 776, 777 | ready | various | pass | - | - |
| 778 | ready | medium, value-classes-sam-lambdas | in progress | - | - |
| 779 | ready, base `review/ycheck-nondestructive-stacked` | low, types | in progress (CI is running on the stacked base) | - | - |
| 780, 781, 782, 783 | ready | low, no-fix | in progress | - | - |
| 784 | draft | critical, backend, port-to-scala3 | in progress | - | - |
| 785 | draft | high, value-classes-sam-lambdas | in progress | - | - |

No (c) infrastructure failures and no (d) unrelated failures. Nothing JDK 8 specific besides 757 (skipped by directive). The scripted `[PENDING]` entries and OSGi `BundleException` lines in the logs are present in every run and are not failures. 758, 759 and 760 allocation assertions pass.

## Pending (not waited for)

750, 778, 779, 780, 781, 782, 783, 784, 785. Re-check later; for the no-fix ones (780-783) expect a failure only in the added test, and for 757-style cases check for a `jvm` directive if one passes.

## PRs that need a change

- 747: replace the two `num.signum(...)` calls at `NumericRange.scala:535` with `num.sign(...)`. The follow-up commit did not fix it; the library still fails to compile, so none of the PR's tests ran.
- 757: no-fix, green because of `//> using jvm 9+`. Either accept (the bug only shows on JDK 9+, where the nightly/other CI would fail it) or leave as is; no change needed on this fork's CI.
- 749, 761, 765, 767, 772: no change; failures are the intended failing tests.
