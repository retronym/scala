# CI triage 3: PR 784 validate

PR 784 head c1f6d4dc3e, run 45851791764 is its run (FAILURE). Failed step: testAll1, `junit/test:compileIncremental` ("1 of 12 test tasks failed", then `(testAll1) Failure due to previous errors`). No test ran and failed; the JUnit sources did not compile.

```
[error] .../test/junit/scala/tools/nsc/backend/jvm/BytecodeTest.scala:1160:18: a type was inferred to be `Any`; this may indicate a programming error.
[error]     assertEquals(List(
[error]                  ^
[error] one error found
```

Line 1160 is the `assertEquals(List(ILOAD, IFEQ, ..., "f", ..., "fin", ...), summary("empty"))` in `t10183ReturnUnderFinallyDropsOperands`. The list mixes Int opcodes and String method names, so `List` is inferred as `List[Any]`, and the -Xlint infer-any warning is fatal under CI's -Werror. Not a JDK 8 vs 21 difference.

Classification: caused by the PR's own change (the new JUnit test). The `[PENDING]` scripted tests in the log are the usual fork noise. Fix: write `List[Any](...)`, or build the list with an explicit element type.

PR 785 (head ed0f2e3a09): validate run 45857030596 is IN_PROGRESS (started 2026-11-26T16:15:26Z), no conclusion yet. Not triaged.
