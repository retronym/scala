# Direct emission from GenBCode

## Problem

Codegen builds a `ClassNode` tree through ASM's visitor API and post-processing replays it into a `ClassWriter`. The tree exists for the optimiser and inliner. Without `-opt`, the only pass that reads it is unreachable-code removal, which retronym/scala#200 has mostly made unnecessary, plus `setInnerClasses`. On a default compile we build the tree, walk it for inner classes and walk it again to write.

## The bound

Campaign `m5-backend-bound`, m5-pro, on top of #200 at `b8c499db39`. No compiler change. All workloads read the same classfiles, so differences separate the parts:

Measuring: job queued. Numbers to follow here.

## Key decisions

**Codegen stops reading the tree, in both modes.** This is what keeps the fork small. Everything codegen asks of the tree today has an O(1) answer it can keep itself: `lastInsn`'s opcode (`ATHROW`, `ACONST_NULL` checks in `emitAdapt`, `isAtProgramPoint`), the pending line number that `lineNumber` overwrites, and `isAtUnreachablePoint`, which walks back through labels. A small `MethodVisitor` wrapper (call it the cursor) holds that state and forwards to whatever is underneath: a `MethodNode1` with `-opt`, a `MethodWriter` without. `jmethod` and `mnode` become the cursor, typed as `MethodVisitor`. `LabelNode1`'s flags move to a `Label` subclass, since `Label.info` is taken by `MethodNode`. This refactoring is worth a commit of its own, and should leave output byte-identical.

Remaining readers, each with a direct answer:

- `appendToStaticCtor` inserts the module init before each `RETURN` after the fact. The cursor emits it when the `RETURN` goes by.
- `emitInvoke` builds a `MethodInsnNode` for `callsitePositions`, and `recordInlineAnnotated` reads `lastInsn`. Both matter only to the inliner, so they stay behind `optInlinerEnabled`.
- `genInvokeDynamicLambda` reads the last `InvokeDynamicInsnNode` for the `indyLambdaImplMethods` registry. Direct mode only needs the handles, kept on the builder; `addLambdaDeserialize` already takes `Iterable[Handle]` and `ClassVisitor` methods, so it runs at the end of the class.
- `cnode.name` and `cnode.interfaces` come from `thisBType`. `addClassFields` uses `visitField`. Mirror classes and forwarders are already `ClassVisitor`-typed.

**The fork.** With the above, `-opt` and non-`-opt` differ in four places: which `ClassVisitor` `genPlainClass` and `genMirrorClass` create, `GeneratedClass` holding a node or a finished writer, the branch in `sendToDisk`, and how `setInnerClasses` gets its names. I expect a few hundred changed lines, most of them the mechanical refactoring. The long-term cost is that both paths stay supported, though the codegen above them is shared: bootstrapping exercises the tree path (the release build uses `-opt`) and most of partest the direct one.

**When direct applies.** `-opt` not set by the user (or `-opt:none`), `-Ybackend-parallelism` 1, which is the default, and no ASM tracing. With parallelism the frame work would move under the frontend lock and undo what the threads are for, so that stays on the tree path. An explicit `-opt:unreachable-code` takes the tree path, as `UnreachableCodeTest` and friends expect.

**Unreachable code: accept ASM's `nop; athrow` for what codegen misses.** After #200 that is about 0.18% of methods in library and reflect, and `-opt:none` already emits exactly this today, so it is an output we already ship and test. No scan, because the scan needs a tree. The prototype measures how many methods and classes are affected across library, reflect and compiler, and whether that changes the answer.

**Redo at class level, not method level, and for errors only for now.** On Jason's parked optimistic-write-and-redo idea: what this needs is small. A `ClassWriter` is per class, so discarding one needs no scala-asm change, whereas method-level redo does: `MethodWriter` is linked into the `ClassWriter` at `visitMethod`, and an abandoned attempt leaves orphan constant pool entries in the output. Class-level redo needs `genClass` to be re-runnable. `BackendBenchmark.phase` is evidence that it nearly is, since rerunning the phase reproduces every classfile byte for byte once the pickles are put back. The effects to undo are the pickle taken from `symData` by `getAnnotPickle`, the lambda handles, the `icode` shim and duplicate reports (the reporter filters repeats at one position).

The prototype uses this for one thing: if direct emission throws (a too-long UTF8 constant throws at `visit*` rather than at serialization), regenerate that class on the tree path, so that errors keep today's messages and `checkConformance`. A second possible trigger is an online version of `nothingToRemove` in the cursor, redoing classes that may have dead code to keep today's output exactly. That needs handler ranges in the scan, and costs a second codegen of up to 3.6% of classes (247 methods at most, of 6,860 classes), probably the bigger ones. Parked unless the `nop; athrow` count argues for it. `fatcode`-style retry isn't needed for jumps, as `ClassWriter` already handles wide jumps with its own re-read.

**Inner classes: collect during emission.** `NestedClassesCollector` gets an incremental entry point, fed by the cursor for instructions and by `initJClass` and `initJMethod` for declarations and signatures, with a per-class identity check on the strings to skip repeats. Annotations go through a thin `AnnotationVisitor` wrapper or from `emitAnnotations`. The visit to `visitInnerClass` happens at the end of the class, and `ClassWriter` is indifferent to when. The `nestedClasses` workload above says how much the walk costs, and so how much is at stake.

**Writer work moves into codegen; `toByteArray` stays in `sendToDisk`.** `MethodTooLargeException` and friends are thrown from `toByteArray`, so keeping it there keeps their handling and messages. `getCommonSuperClass` runs in `visitMaxs`, under the lock codegen already holds, and every type a method mentions has its `ClassBType` by then, as today.

**ASM ordering.** `MethodWriter` tolerates `visitTryCatchBlock` and `visitLocalVariable` mid-body: it stores labels for handlers and reads resolved offsets for local variables, and codegen places those labels first. Direct mode needs `visitMaxs` and `visitEnd` for non-abstract methods, which `MethodNode.accept` does for us today. I'll read the rest of `MethodWriter` for assumptions as I go; `visitTryCatchBlock`'s documentation asks for the call before its labels are visited, which `MethodWriter` doesn't enforce but `CheckMethodAdapter` would.

**Verifying: a normalised byte comparison, not only linking.** The constant pool follows visit order, so output is deterministic but differs from today's. But `replay` shows that reading a classfile and writing it again reproduces it exactly, so the normal form is cheap: read each class from both paths into a `ClassNode` and write it with `ClassWriter(0)`, keeping frames. Equal bytes then mean equal classes, independent of pool order. Expected differences are only the `nop; athrow` methods, which get listed and inspected. On top of that: library, reflect and compiler link under verification, bootstrap with the direct path, backend JUnit, `partest --run --jvm --pos --neg`, and `-Ybackend-parallelism` taking the tree path.

## Parked

- A debug-only label for line numbers and variable ranges. `MethodWriter` starts a basic block at every label except those, and only `ClassReader` can make them. `replayBlocks` against `replay` is what exposing that in scala-asm would give, on either path, with no need for direct emission.
- Per-method compute mode, so branch-free methods skip frames (about 54 MB/op on `readFrames`). Direct emission makes "no branch so far" known online, but not at `visitMethod`, where the mode is chosen.
- Collecting nested classes from `SymbolTable` at the end of the class, which dedupes for free, but needs a scala-asm hook.
- `MethodWriter`'s `ByteVector` growth, and method-level redo.

## Steps

- DONE bound: workloads in `BackendBenchmark`, campaign `m5-backend-bound`
- TODO review of this plan
- TODO cursor refactoring: codegen stops reading the tree, output byte-identical (commit, A/B on its own)
- TODO `GeneratedClass` holding a node or a writer; direct path behind a `-Y` flag, off by default, inner classes still via a re-read to get it working
- TODO incremental nested class collection
- TODO class-level redo on exception
- TODO normalised comparison over library, reflect and compiler; count `nop; athrow`
- TODO flag on by default where it applies; backend JUnit, partest, bootstrap
- TODO before and after from `BackendBenchmark` with an A/A, and a full compile; conclusion
