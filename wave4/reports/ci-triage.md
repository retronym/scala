# CI triage, retronym/scala PRs 747-768

Each PR has one check, `validate`. 13 of 22 are still pending; 9 have finished. Read-only; nothing was waited for or re-run.

| PR | State | Labels (besides code-review-2026-11-19) | validate | Class | First error |
|---|---|---|---|---|---|
| 747 | ready | medium, collections | fail (1m, build job) | (a) | `NumericRange.scala:535:17: method signum in trait Numeric is deprecated (since 2.13.0): use sign method instead` (also col 36); fatal warning in library compile |
| 748 | ready | low, collections | pass | - | - |
| 749 | ready | medium, no-fix, collections | fail (testAll1) | (b) | `HashMapTest.updateWithModifyingTheMap` and `LinkedHashMapTest.updateWithModifyingTheMap`: `AssertionError: remove the key, Some expected:<List((1,7))> but was:<List()>`. Only these two fail; both are in files the PR adds tests to |
| 750 | draft | high, typechecker | pending | - | - |
| 751 | ready | medium, java-sources | pass | - | - |
| 752 | ready | low, java-sources | pass | - | - |
| 753 | ready | upstreamed, medium, java-sources, regression-since-2.13.18 | pending | - | - |
| 754 | ready | low, collections | pass | - | - |
| 755 | ready | low, collections | pass | - | - |
| 756 | ready | low, typechecker | pending | - | - |
| 757 | ready | low, no-fix, java-sources | pass | see note | A no-fix PR passed: its test passes, so the bug it documents is not reproduced here (or the test does not exercise it) |
| 758 | draft | high, collections | pending | - | - |
| 759 | draft | high, collections | pending | - | - |
| 760 | ready | high, collections | pending | - | - |
| 761 | ready | high, no-fix, collections | fail (testAll1) | (b) | 8 failures, all in tests the PR adds: `HashMapTest.bulkOperationsWithMappedViewOfSelf` (expected HashMap(1->2,2->12,3->13,4->14)), `LongMapTest.bulkOperationsWithMappedViewOfSelf`, `{TreeSet,TreeMap,LinkedHashSet,LinkedHashMap,HashSet}Test.subtractAllMappedViewOfSelf` (e.g. expected HashSet(1) but was HashSet(1, 3)). Nothing else failed |
| 762 | ready | high, specialization | pending | - | - |
| 763 | draft | high, specialization | pending | - | - |
| 764 | ready | high, specialization | pending | - | - |
| 765 | ready | medium, no-fix, specialization | pending | - | - |
| 766 | ready | medium, specialization | pending | - | - |
| 767 | ready | high, no-fix, optimizer, regression-since-2.13.18 | pending | - | - |
| 768 | ready | high, specialization | pending | - | - |

No (c) infrastructure failures, no (d) unrelated test failures, and nothing JDK 8 specific (no AllocationTest or Java 9+ API failures) in the finished runs. Pending jobs may still show some.

## PRs that need a change

- 747: replace `num.signum(...)` with `num.sign(...)` at `NumericRange.scala:535` (two calls). The library fails to compile under fatal deprecation warnings, so nothing else in the PR ran.
- 749, 761: no change needed; the failures are the intended failing tests (no-fix).
- 757: no-fix but green. Check whether the test actually reproduces the bug on JDK 8, or whether the bug needs a newer JDK, and drop the no-fix label or fix the test accordingly.
- The 13 pending PRs (750, 753, 756, 758, 759, 760, 762, 763, 764, 765, 766, 767, 768) need a re-check later; for the no-fix ones (765, 767) expect a failure in the added test only.
