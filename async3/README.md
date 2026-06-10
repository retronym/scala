# async3 prototype

Standalone exploration of a **bytecode-level** async/await state-machine transform, as an
alternative to the tree-level `async` compiler phase
(`scala.tools.nsc.transform.async.AsyncPhase`). Design rationale, prior art, and the phased
plan live in [../ASYNC3-DESIGN.md](../ASYNC3-DESIGN.md).

Fully independent of the Scala build:

```
mvn test
```

## Debugging in IntelliJ

1. Open `async3/pom.xml` as a (separate) Maven project — don't import it into the Scala
   project model.
2. Open `async3.demo.Demo` and **Debug `main()`** (it also works via
   `mvn -q compile exec:java -Dexec.mainClass=async3.demo.Demo` on the command line).
3. Set line breakpoints in [Samples.java](src/main/java/async3/samples/Samples.java) inside
   `sumTwice`, one per line. The generated state machine
   (`Samples$async$sumTwice$N`) declares `SourceFile: Samples.java` and keeps the original
   line numbers, and its name matches IntelliJ's `Samples$*` inner-class pattern, so the
   breakpoints bind to the transformed code. (Verified over plain JDWP with jdb: the deferred
   breakpoint resolves at class load, hits after a resume, and `locals` shows `x = 5` under
   its source name.)

What to expect:

- **Scenario 2** of the demo is the interesting one: the futures are completed manually from
  the main thread, so each resume runs synchronously on the main thread — a linear,
  single-threaded debug story. The first breakpoint hits in state 0; after `fa.complete(5)`
  the breakpoint on the next line hits *inside the resumed state machine*, and the Variables
  view shows `x = 5` under its source name (the original `LocalVariableTable` is carried over
  and restores target the original slots).
- `this` in those frames is the state machine; expand `refs` / `prims` to see the raw spilled
  frame, or evaluate `async3.runtime.AsyncDebug.describe(this)` in the Evaluate dialog to get
  `...sumTwice(...) suspended at state 2 (line 23): fa = ..., x = 5`.
- **Stepping over an await that actually suspends steps out of the method** — `apply` parks
  and returns. That is faithful to what the machine does; to follow the logical flow, put a
  breakpoint on the line after the await (this is the same experience as debugging Kotlin
  coroutines without their debugger plugin; an IntelliJ plugin that auto-plants those
  breakpoints is the eventual answer).
- The demo also dumps all transformed classes to `target/transformed-classes/` for
  `javap -c -l -p` inspection.

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

Instance methods are supported: `this` is just the entry local in slot 0, captured like any
other value, and the generated state machine joins the host class's **nest** so private
field/method access in the transformed body keeps working.

## Current limitations

Awaits in constructors are rejected; methods of inner classes (host not its own nest host)
lose private-member access from the transformed body; spills all assigned locals rather than
only live ones; no lazy/`invokedynamic` tier switch yet; no JMH numbers yet (deferred). See
the design doc's phase list.
