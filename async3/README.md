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

## Status

Working: suspension with non-empty operand stacks, loops, try/catch (failed futures reach the
user's handler), two-slot primitives, `new Foo(await(f))` via Kotlin-style NEW-sinking
(`sinkUninitializedNews`, including nested/conditional/statement shapes), per-state
`$asyncDebug` frame metadata with source variable names, `AsyncDebug.describe` for rendering
suspended frames, and the original `LocalVariableTable`/line numbers carried into the
generated `apply` (restores target the original slots, so a debugger sees named locals).

## Current limitations

Static methods only; spills all assigned locals rather than only live ones; no
lazy/`invokedynamic` tier switch yet; no JMH numbers yet. See the design doc's phase list.
