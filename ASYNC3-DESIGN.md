# async3: a bytecode-level async transform (design notes & prototype plan)

Status: exploratory prototype. See `./async3` for the standalone Maven project.

## 1. Motivation

The current async support is split between:

- a user-writable frontend (macro or compiler plugin) that marks methods for
  transformation and lifts them into a state-machine wrapper class
  (`src/testkit/scala/tools/testkit/async/Async.scala`,
  `test/junit/scala/tools/nsc/async/AnnotationDrivenAsyncTest.scala`), and
- the built-in `async` compiler phase
  (`src/compiler/scala/tools/nsc/transform/async/AsyncPhase.scala`) that turns
  the marked method body into a resumable state machine, working on typed trees.

The tree-level transform is extremely non-trivial (ANF transform, local
lifting, symbol re-ownership, live-variable analysis, patmat interaction,
value-class boxing workarounds). We already run it late in the pipeline to see
erased types and expanded pattern matches, but it remains the most complex
transform in the compiler.

**Idea (inspired by Kotlin coroutines):** perform the state-machine
transformation *after* lowering to bytecode. A stack machine makes the problem
dramatically simpler. Taken further, the transform can be deferred to **class
load time or even runtime**, driven by profiling: run the synchronous
(blocking) version until it proves to be a bottleneck, then swap in the
suspendable version.

## 2. Prior art

| System | Where the transform runs | Notes |
|---|---|---|
| Kotlin coroutines | compiler backend, on bytecode-level IR (`CoroutineTransformerMethodVisitor`) | spills to fields of the continuation object; `@DebugMetadata` annotation maps state → spilled variable names + line numbers |
| Quasar | java agent / AoT ASM instrumentation | generic `Stack` object: `long[]` for primitives + `Object[]` for refs |
| Kilim, Apache Javaflow | AoT ASM instrumentation | same family; Javaflow documents the uninitialized-object problem |
| Project Loom | inside the VM (native stack copying) | the "do nothing, block on a virtual thread" baseline |

Everything below has been done before in some form; the novelty is wiring it to
Scala's existing frontend-pluggable async ABI and the *lazy, profile-driven*
variant.

## 3. Why bytecode level is easier

Almost everything hard in `AsyncPhase` is an artifact of working on trees:

- **ANF transform disappears.** Its entire purpose is to forbid `await` in
  expression position so that evaluation-in-progress never needs to be
  reified. On the JVM, evaluation-in-progress *is* the operand stack, and
  ASM's `Analyzer` reports the exact type of every stack slot and local at
  every instruction.
- **Lifter / `UseFields` disappear.** No symbol re-ownership,
  `makeNotPrivate`, or outer-path reconstruction. Locals are numbered slots to
  spill and restore.
- **Patmat, by-name, named/default args, value classes, closures** are already
  lowered. The `{ expr: Any }` boxing hack and `ForceMatchDesugar` attachment
  go away. `await` in extractor calls and guards — which the annotation-driven
  frontend needs and which requires patmat-desugar trickery today — becomes a
  non-event: by codegen time it is just a call instruction inside ordinary
  branches.
- **`finally` is already duplicated** by codegen; the messiest control-flow
  case in the tree transform needs no special handling.
- **Live-variable analysis** (the `LiveVariables` null-out machinery) becomes
  a textbook bitvector dataflow over instructions.

### 3.1 The transform, in one paragraph

For each method containing calls to the `await` marker: run ASM's analyzer to
get the frame (types of locals and operand stack) at each await call site
*i* ∈ 1..N. Replace the call with: fast-path check `getCompleted(f)`; if
already complete, proceed; otherwise spill live locals and the operand stack
below the awaited future into a frame object, set `state = i`, register
`onComplete(f, this)`, and return. The method prologue becomes a
`TABLESWITCH` on `state` whose case *i* restores the spilled slots and jumps
to the instruction after the call, pushing the result. Returns become
`completeSuccess`; an outermost handler routes exceptions to
`completeFailure`. The existing `AsyncStateMachine` ABI (`state`,
`onComplete`, `getCompleted`, `tryGet`, `completeSuccess/Failure`) is
preserved, so existing frontends keep working: the frontend's only remaining
body-level job is to leave a real `await` call instruction in the bytecode.

### 3.2 Known hard corners (all solved in prior art)

1. **Uninitialized objects on the stack**: `new Foo(await(f))` has an
   uninitialized reference on the stack at the suspension point, which cannot
   be stored in a field or array. Kotlin sinks the `NEW`/`DUP` pair past the
   suspension and re-materializes it before `INVOKESPECIAL <init>`. The one
   genuinely fiddly piece. ✅ implemented in the prototype
   (`sinkUninitializedNews`): the analyzer marks NEW results
   uninitialized-until-`<init>` (with verifier-style replacement of all copies
   upon initialization, keyed by allocation site so loop-fixpoint re-execution
   doesn't lose the marking); the pre-pass deletes the `NEW`(+`DUP`) and
   re-materializes it below the constructor args at the `<init>` site. Handles
   nested news, multiple awaits among arguments, conditional argument
   expressions, and the statement (`POP`) idiom; exotic stack-shuffled shapes
   are still rejected rather than miscompiled.
2. **`await` under `MONITORENTER`**: detect and reject with a clear error
   (trivially detectable at bytecode level; silently broken today).
3. **Stack map frames**: jumping from the dispatch switch into the middle of
   try regions is verifier-legal as long as frames match. Use
   `COMPUTE_FRAMES` with a custom `getCommonSuperClass`, and validate every
   output with `CheckClassAdapter` in tests.
4. **long/double two-slot handling, 64KB method limit** — mechanical, but
   needs tests.

## 4. The annotation-driven frontend (Optimus-style)

The biggest production use case is annotation-driven: users mark methods
`@async`; a compiler plugin emits a *pair* of methods — the direct
(synchronous) one and a lifted/suspendable one — and retargets calls between
async contexts to the lifted variants. This is approximated by
`AnnotationDrivenAsyncTest` (`@customAsync` / `@autoawait`,
`externalFsmMethod = true`).

This doesn't change the design; it sharpens the division of labour:

- **Stays in the frontend (typed work):** generating the lifted method's
  *signature* (`f` vs. `f$queued: Node[T]`), deciding which callee variant
  each call site targets, inserting the `await` marker around calls to lifted
  methods, type checking the user-facing API.
- **Moves to bytecode (untyped work):** everything `markForAsyncTransform`
  triggers today — the state machine, lifting, liveness. The plugin emits the
  lifted method's body as a *plain synchronous body with marker calls*
  (`await(g$queued(args))`), and the bytecode stage rewrites it. The frontend
  no longer wraps bodies into state-machine classes at all.
- The `externalFsmMethod` shape in the test (FSM body as a sibling method
  taking `(self, tr)` rather than an `apply` override) is the natural shape
  for the bytecode transform too: it transforms an arbitrary method and
  generates the state-machine class separately.
- **Forking:** Optimus also supports forking — queuing a child computation
  (`g$queued(...)` returns a Node immediately) separately from awaiting it, so
  siblings run in parallel under the graph scheduler. This costs the bytecode
  transform nothing: `await` takes any `F[_]` value, wherever it was created
  (a local, a parameter, a Node queued many statements earlier), so fork/join
  shapes transform identically to sequential ones. The prototype's samples
  already await futures received as parameters, which is the same shape.
- **Optional further step:** since the direct and lifted bodies differ only in
  call targets (`g(...)` vs. `await(g$queued(...))`) and that retargeting is
  *descriptor-mechanical* once the frontend has recorded which methods are
  async, the bytecode stage could derive the entire lifted method from the
  direct one. Then the frontend emits one body + metadata, and lifted variants
  can be derived lazily at runtime — which is exactly the
  "run sync with blocking until profiling says otherwise" mode this user has
  asked for. Caveat, because of forking: a purely mechanical per-call-site
  rewrite to `await(g$queued(...))` produces *sequential* semantics — correct,
  but it forfeits fork opportunities. Where forking matters, either the
  frontend keeps emitting the lifted body (placing queue and await points
  deliberately), or the mechanical derivation is followed by an
  await-sinking/batching optimization that separates the queue point from the
  await point when dataflow allows (Optimus has exactly this kind of analysis;
  at bytecode level it is a code-motion pass over the marker calls).
- The ABI stays generic in `F[_]`/`R[_]` (`Future`/`Try`, `CustomFuture`/
  `Either`, `Node`/...): the transform only emits calls to the abstract
  `getCompleted`/`onComplete`/`tryGet` methods and never inspects `F`.

## 5. State capture: the frame representation

| Representation | Pros | Cons |
|---|---|---|
| Generic frame: `long[] prims` + `Object[] refs` (Quasar style) | no classgen → works for the lazy/runtime variant; one allocation per call | bounds checks, card marks on ref stores; names live in side metadata |
| Generated fields on the SM class, named after source locals (Kotlin style, but with real names instead of `L$0`) | fastest spills; debugger sees named fields | class per method (fine AoT; needs `defineHiddenClass` at runtime) |
| Boxed `Object[]` only | simplest | boxes every primitive spill — rejected |

**Prototype choice: the generic two-array frame**, because it is the only one
that works unchanged in both the compile-time and runtime-deferred modes and
keeps the transformer free of field-layout decisions. All primitives normalize
to `long` slots (`floatToRawIntBits` etc.). One mutable frame per
state-machine instance, slots addressed positionally (frame slot *v* = local
slot *v*; stack entry *j* = `maxLocals + j`), reused across states, with
liveness-driven nulling of dead ref slots (the bytecode analogue of
`fieldsToNullOut`). "Typed fields on the SM class" is the planned optimization
for the AoT path — decide with JMH numbers, not vibes.

Note the layout is per-state: slot *k* can hold different variables in
different states. Debug metadata must be keyed by state.

## 6. Debuggability

Two distinct problems:

1. **While executing** (between suspensions): restore spilled values into
   their *original local slots* on resume. Then the original
   `LocalVariableTable` entries remain truthful; we extend each entry's range
   (or emit a duplicate entry) to cover resumed regions. A debugger stepping
   through resumed code sees ordinary named locals. Line numbers carry over
   untouched because the original instructions are preserved.
2. **While suspended** (the frame is heap data): emit per-method static
   metadata mapping `state → [(name, descriptor, frameKind, frameIndex,
   sourceLine)]`, sourced from the original LVT + line number table. This is
   Kotlin's `@DebugMetadata` design. A class annotation (or constant field)
   travels with the class and is readable by a debugger plugin, a `toString`,
   or an async-stack-trace dumper:
   `suspended at Foo.scala:42: x=1, items=List(...)`.

Bonus: give frames a `parent` pointer to the awaiting frame and logical async
stack traces (Kotlin's `CoroutineStackFrame`) fall out nearly for free.

## 7. The runtime-deferred ("tiered") variant

The marker design makes this clean: the compiler emits the method in its
**synchronous shape**, where `await(f)` is a real static method whose default
implementation blocks, plus a `@Suspendable` annotation so the transformer
doesn't scan the world. That class runs correctly with zero transformation —
the "interpreter tier".

The dispatch mechanism matters more than the transform:

- `Instrumentation.retransformClasses` can only replace method bodies — it
  cannot add methods or fields, so a state machine cannot be grafted onto the
  original class after the fact. Awkward.
- **Better: `invokedynamic` indirection.** Compile entry to `@Suspendable`
  methods through an indy bootstrap backed by a `MutableCallSite` /
  `SwitchPoint`, initially bound to the blocking version. When profiling (a
  counter in the blocking `await`, or observed contention) says
  hot-and-blocking, generate the state machine via
  `Lookup.defineHiddenClass` and flip the call site. The JIT inlines through
  stable call sites, so the un-flipped path costs ~nothing.
- As with JIT tiering there is no on-stack replacement: threads already parked
  in a blocking await stay parked; only subsequent invocations suspend
  cooperatively. Acceptable; state it.

**The Loom question, answered up front:** this runtime variant is userland
Loom. On JDK 21+, "run the sync version, block on a virtual thread" is already
a fine steady state. The case for the transform is everything Loom doesn't
cover: Scala.js and Scala Native, pre-21 JVMs, pinning-sensitive or
allocation-sensitive environments, and runtimes (Optimus-style graph
schedulers) that need their own notion of suspension and scheduling rather
than thread semantics. **Forking is the sharpest example**: Optimus separates
*queuing* a computation (`g$queued(...)` returns a Node immediately) from
*awaiting* it, so several child computations can be forked before the first
suspension — which is what makes auto-parallelism, batching, deduplication,
and distribution visible to the scheduler as a dataflow graph. Blocking on a
virtual thread collapses that back to one sequential thread of execution
unless the user manually spawns threads; the queued/await split keeps the
graph explicit at zero syntax cost. The transform is agnostic to this by
construction — `await` accepts any `F[_]` value regardless of where or when it
was created, so fork/join shapes (queue several, await later, awaitables held
in locals or passed as arguments) need nothing beyond what sequential code
needs. The AoT variant additionally needs no runtime classgen at all.

## 7.5 Lambda front end (prototype of the runtime-triggered transform)

`Async.async(() -> { ... Async.await(f) ... })` triggers the transform at
runtime via **lambda cracking**: the body interface extends `Serializable`, so
the proxy's `writeReplace` yields a `SerializedLambda` naming the synthetic
`lambda$...` impl method (where javac placed the body's bytecode, in the
capturing class) plus the captured arguments. The impl method goes through the
single-method transformer entry point and is defined with
`Lookup.defineHiddenClass(bytes, false, NESTMATE)` — the hidden class joins
the capturing class's nest dynamically, so private access works with zero
host patching (the runtime twin of the AoT path's `NestMembers` patching).
Captured locals, including a captured `this`, are simply the impl method's
leading parameters; the constructor handle is cached per impl method. This is
a faithful stand-in for the eventual compiler integration, where
`async { ... }` compiles to `invokedynamic` and the bootstrap receives the
caller's `Lookup` without any cracking.

**Debugger finding (empirical):** IntelliJ resolves a line breakpoint inside a
lambda body only against classes named exactly like the enclosing source
class — javac puts lambda bodies *in* that class, so IJ registers an
exact-name class-prepare filter, with no `$*` wildcard (that is reserved for
anonymous/local classes). Consequently a state machine named
`Owner$async$lambda$m$0` never receives the breakpoint; it silently binds into
the original, now-dead `lambda$` method instead. Workaround implemented
(`-Dasync3.lambda.debuggable=true`): define the state machine *under the
capturing class's own name* in a throwaway loader — JDI matches names across
loaders, so the IDE finds it; verified with a JDI program replaying exactly
IntelliJ's procedure (`classesByName` → `locationsOfLine` → breakpoint → hit
after a real resume with named locals intact). The shadow trade-off: the
host's name resolves to the shadow from inside it, so bodies referencing the
host class (captured `this`, same-class helpers, nested lambdas) are rejected
in this mode. The clean resolution belongs to the compiler/agent integration:
emit the resumable body as a sibling method *of the capturing class itself*
(the `externalFsmMethod` shape §4 already uses) — then breakpoint binding,
nest access, and stepping all work with no shadowing and no mode split.

## 8. Prototype plan (`./async3`, standalone Maven project)

Plain Java + upstream ASM (`asm-tree`/`asm-analysis`/`asm-util`); no scalac
involvement until the final phase. The repo's shaded `scala.tools.asm` is
deliberately not used, to keep iteration friction low.

- **Phase 0 — fix the ABI by hand.** `AsyncRT.await` (blocking default),
  `FutureStateMachine` base (mirrors `AsyncStateMachine`:
  `state`/`onComplete`/`getCompleted`/`tryGet`/`completeSuccess`/`completeFailure`),
  the two-array frame. Hand-write one sample twice: the source shape (sync,
  calls `await`) and the expected state-machine output. Nails the calling
  convention, the already-completed fast path, and exception semantics before
  any ASM is written. ✅ scaffolded
- **Phase 1 — the transformer.** Standalone: read the source-shape `.class`,
  find `INVOKESTATIC AsyncRT.await` sites, run `Analyzer` for frame types,
  emit the spill/restore/switch rewrite into a generated SM class; add a
  sibling `m$async` entry point to the original class. Every output goes
  through `CheckClassAdapter`. ✅ scaffolded (static methods, no
  uninitialized-`new` handling yet)
- **Phase 2 — semantic test matrix.** Each case runs blocking and transformed,
  asserting identical results, with both already-completed futures (fast
  path) and futures completed later from another thread (suspension path):
  sequential awaits; deep operand stack `1 + (2 * await(f)) + g(await(h), 3)`;
  await in a loop; try/catch/finally around and containing awaits; long/double
  locals and operands; exception thrown by the future; `new Foo(await(f))`
  (the milestone test); `synchronized` rejection. ✅ green, including
  NEW-sinking (simple/nested/two-arg/statement/conditional shapes)
- **Phase 3 — debuggability.** Emit the state → names metadata; restore into
  original slots + LVT extension; verify with a debugger; write the
  suspended-frame pretty-printer. ✅ mostly done: per-state `$asyncDebug`
  metadata with source names; original `LocalVariableTable` and line numbers
  carried into the generated `apply` (restores target the original slots, so
  entries stay truthful); `AsyncDebug.describe` renders e.g.
  `Samples.sumTwice(...) suspended at state 2 (line 23): fa = ..., x = 5`.
  Verified over JDWP with jdb: a line breakpoint in `Samples.java` defers,
  resolves into the generated `Samples$async$sumTwice$N.apply` at class load,
  hits after a resume, and `locals` shows `x = 5` under its source name.
  `async3.demo.Demo` is a single-threaded debugger walkthrough for IntelliJ
  (see the README's "Debugging in IntelliJ").
- **Phase 3.5 — lambda front end.** ✅ see §7.5: cracking + hidden-nestmate
  definition + constructor caching, and the shadow-named debuggable mode with
  JDI-verified breakpoint binding inside lambda bodies.
- **Phase 4 — lazy variant + numbers.** `defineHiddenClass` +
  `MutableCallSite` flip; JMH: blocking vs. AoT-transformed vs. lazily flipped
  vs. the current compiler phase's output; generic frame vs. typed fields.
- **Phase 5 — integration sketch only.** Post-`jvm` hook in `GenBCode` (where
  shaded ASM lives) vs. shipping as a build/agent step; shrink
  `markForAsyncTransform` to "keep the await call + annotate".

**Risk order:** uninitialized-object sinking ✅, stack-map correctness around
try regions ✅ (JVM verifier accepts all generated classes, exercised on every
test load), generic-frame spill cost vs. Kotlin-style typed fields — phase 4
answers this with data.
