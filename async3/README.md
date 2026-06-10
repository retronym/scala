# async3 prototype

Standalone exploration of a **bytecode-level** async/await state-machine transform, as an
alternative to the tree-level `async` compiler phase
(`scala.tools.nsc.transform.async.AsyncPhase`). Design rationale, prior art, and the phased
plan live in [../ASYNC3-DESIGN.md](../ASYNC3-DESIGN.md).

Fully independent of the Scala build:

```
mvn test
```

## Layout

- `async3.runtime.AsyncRT` — the `await` marker; its default implementation blocks ("tier 0").
  Methods calling it run correctly with no transformation at all.
- `async3.runtime.FutureStateMachine` — state machine base class mirroring the ABI of
  `scala.tools.testkit.async.AsyncStateMachine`, plus the generic two-array frame
  (`Object[] refs` / `long[] prims`) used for captured locals and operand stack.
- `async3.samples.Samples` — source-shape methods (plain synchronous Java calling `await`).
- `async3.samples.HandWrittenSumTwice` — Phase 0: the expected transform output, by hand.
- `async3.transform.AsyncTransformer` — Phase 1: the ASM transform. Derives a
  `<name>$async` entry point + `Owner$async$<name>$i` state machine class per marked method;
  leaves the original method untouched as the synchronous tier.
- `src/test/java` — semantic equivalence matrix (fast path vs. real suspension), rejection
  tests (monitors, uninitialized `new` across a suspension), debug-metadata check.

## Current limitations

Static methods only; `new Foo(await(f))` is detected and rejected (needs Kotlin-style
NEW-sinking); spills all assigned locals rather than only live ones; no
`LocalVariableTable`/line-table surgery for the debugger yet; no lazy/`invokedynamic` tier
switch yet. See the design doc's phase list.
