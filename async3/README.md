# async3 prototype

Standalone exploration of a **bytecode-level** async/await state-machine transform, as an
alternative to the tree-level `async` compiler phase
(`scala.tools.nsc.transform.async.AsyncPhase`). Design rationale, prior art, and the phased
plan live in [../ASYNC3-DESIGN.md](../ASYNC3-DESIGN.md).

Fully independent of the Scala build:

```
mvn test
```

## Lambda API

No pre-compiled sample classes or reflection needed — the transform is triggered at runtime by
*cracking* the (serializable) lambda:

```java
CompletableFuture<String> r = Async.async(() -> {
    int x = Async.await(fa);
    String s = Async.await(fb);
    return s + ":" + (x * 2);
});
```

`Async.async` cracks the lambda via `writeReplace` → `SerializedLambda`, which names the
synthetic `lambda$...` impl method (where javac put the body's bytecode, in the capturing
class) and carries the captured arguments. The impl method goes through
`AsyncTransformer.transformMethod` and the state machine is defined as a **hidden class with
the NESTMATE option**, so the body keeps access to private members of the capturing class.
Captured locals (including a captured `this`) are just the impl method's leading parameters.
Compiled once per lambda, cached. This is a faithful stand-in for the eventual compiler
integration, where `async { ... }` compiles to `invokedynamic` and the bootstrap receives the
caller's `Lookup` for free.

### Lifting method references

`Async.lift(C::m)` derives the suspending variant of an existing method — possibly from
another class — at runtime: the runtime analogue of the annotation-driven (Optimus-style)
frontend's direct/`$queued` method pair, with no build-time sibling generation:

```java
// Samples.sumTwice is an ordinary method that calls the await markers;
// called directly it blocks (tier 0), lifted it suspends:
var sumTwice = Async.lift(Samples::sumTwice);
CompletableFuture<String> r = sumTwice.apply(fa, fb);
```

All reference shapes work: static, unbound instance (`C::m` — receiver becomes the first
parameter), bound instance (`obj::m` — receiver is a captured argument), private targets (the
hidden state machine joins the target's nest), and plain lambdas with parameters. Captured and
applied arguments concatenate into the target's entry locals, so one mechanism covers every
shape. Constructor references are rejected (no await in constructors). A lifted handle is also
the natural installation point for the phase-4 profiling-driven tier flip
(`MutableCallSite`: start blocking, swap in the transformed version when hot).

## Debugging in IntelliJ

1. Open `async3/pom.xml` as a (separate) Maven project — don't import it into the Scala
   project model.
2. Open `async3.demo.Demo` and **Debug `main()`** (it also works via
   `mvn -q compile exec:java -Dexec.mainClass=async3.demo.Demo` on the command line).
3. For the **lambda API** (Demo scenario 0): make sure `-Dasync3.lambda.debuggable=true` is
   set (Demo sets it programmatically) and put line breakpoints inside the lambda body.
4. For the class-based samples: set line breakpoints in
   [Samples.java](src/main/java/async3/samples/Samples.java) inside `sumTwice`, one per line.

**Why the lambda case needs its own mode.** IntelliJ resolves a line inside a lambda body only
against classes named like the *enclosing source class* — javac emits lambda bodies as
`lambda$...` methods of that very class, so IJ registers an exact-name class-prepare filter
(no `$*` wildcard as for anonymous classes). A state machine named `Owner$async$lambda$m$0` is
therefore invisible to the breakpoint, which instead binds into the original — now dead —
`lambda$` method and never fires. In debuggable mode the state machine is defined **under the
capturing class's own name** in a throwaway loader; JDI matches class names across loaders, so
the IDE finds it and the breakpoint binds and fires. Verified with a JDI program
(`JdiShadowCheck`) that performs exactly IntelliJ's procedure: `classesByName` →
`locationsOfLine` → breakpoint → hit after a real resume, named locals (`x = 41`) intact.
The trade-off: inside the shadow class the host's name resolves to the shadow itself, so
lambdas that reference the host class (captured `this`, same-class helpers, nested lambdas)
are rejected in this mode with an explanatory error — use the default hidden-nestmate mode for
those. The clean fix belongs to the compiler/agent integration: emit the resumable body as a
sibling method *of the capturing class itself* (the `externalFsmMethod` shape), and the whole
issue disappears.

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
- `async3.runtime.Async` — the lambda front end (`Async.async(() -> ... Async.await(f) ...)`)
  via lambda cracking + `defineHiddenClass(NESTMATE)`, with the shadow-named debuggable mode.
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

The lambda front end works end-to-end: cracking, hidden-nestmate definition (private state of
the capturing class accessible across suspension points), constructor caching, and the
shadow-named debuggable mode with JDI-verified breakpoint binding.

## Current limitations

Awaits in constructors are rejected; methods of inner classes (host not its own nest host)
lose private-member access from the transformed body; spills all assigned locals rather than
only live ones; no lazy/`invokedynamic` tier switch yet; no JMH numbers yet (deferred). See
the design doc's phase list.
