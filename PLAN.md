# Direct emission from GenBCode

## Problem

Codegen builds a `ClassNode` tree through ASM's visitor API and post-processing replays it into a `ClassWriter`. The tree exists for the optimiser and inliner. Without `-opt`, the only pass that reads it is unreachable-code removal, which retronym/scala#200 has mostly made unnecessary, plus `setInnerClasses`. On a default compile we build the tree, walk it for inner classes and walk it again to write.

## The bound

Campaign `m5-backend-bound`, m5-pro, on top of #200 at `b8c499db39`. No compiler change. All workloads read the same classfiles, so differences separate the parts:

- `decode`: 52.4 ms/op, 102.2 MB/op (`ClassReader` into nothing)
- `decodeTree`: 61.9 ms/op, 220.1 MB/op (into a new `ClassNode`)
- `replay`: 172.3 ms/op, 481.5 MB/op (into a `ClassWriter`)
- `replayBlocks`: 172.1 ms/op, 494.1 MB/op (the same, with every label starting a basic block as codegen's do)
- `readFrames`: 132.5 ms/op, 410.4 MB/op (`ClassNode` into a `ClassWriter`)
- `write`: 149.6 ms/op, 428.9 MB/op (codegen's `ClassNode`, with its extra labels)
- `codegen`: 409.5 ms/op, 442.4 MB/op
- `phase`: 769 ms/op, 1050.6 MB/op

Subtracting: the writer's own work is about 120 ms/op and 392 MB/op (`replayBlocks` less `decode`). Walking a tree is about 12 ms/op and 18 MB/op (`readFrames` less that). Building one is about 10 ms/op and 118 MB/op (`decodeTree` less `decode`). So a tree costs about 22 ms/op and 136 MB/op: about 3% of the phase's time and 13% of its allocation. That excludes `setInnerClasses` and the scan in `localOptimizations`, which go or change too, and what codegen's extra labels cost on top.

This is one fork per workload, cancelled before round 2 and before `nestedClasses`, and the machine had my builds and tests running on it during some of them (`phase` was 684 ms/op on a quiet machine in `m5-backend-ceiling`). The time is weak; the allocation is much more stable. The debug-only label is worth nothing in time and 12.6 MB/op, so it stays parked.

## Result

All on m5-pro, comparing #200 at `b8c499db39` with the direct path at `456838f7e0`.

`phase` (`m5-direct-backend`, stopped after two forks per build): time -9.0% ±6.9 (A/A -1.1% ±7.1), allocation -17.7% ±2.1 (A/A -0.6% ±2.2). The forks were 607.8 and 611.3 ms/op against 660.3 to 679.3, and 879.6 and 877.5 MB/op against 1059.3 to 1071.9.

A full compile of the scala corpus (`m5-direct-compile`, stopped at 14 of 30 cells): allocation 6114 MB/op against 6299 and 6307, -3%. Time is unresolved. Cells of one build ranged by about 4% with load of up to 2.6 cores, under the gate of 4, and the expected effect is under 1%.

In profiled forks of that compile (`m5-direct-compile-prof`, two forks per build), the jvm phase is 810 and 843 samples/op against 726 and 771, about -78 or -9%, which is about 0.8% of the compile. Its allocation is 1112 and 1089 MB/op against 905 and 895. The rest of the compile differs by more than that between forks of one build (8953 and 9146 for `base`, 8606 and 9067 for `direct`), which is why the total doesn't show it. Samples are about 1 ms each, judging by their total against the score.

Where it goes, in samples/op on that compile: `sendToDisk` 271 to about 31, and `genPlainClass` 534 to about 670, since the writer's work moved there. `setInnerClasses` at 79 becomes about 24 in `collectNestedClasses` plus part of the 33 under `DirectClassVisitor`, so resolving names still costs something. The largest allocations gone are `LabelNode1`, `MethodNode1` and the instruction nodes. `CodegenLabel` just replaces `Label` in the profile.

The conclusion so far: worth upstreaming if a quiet A/B confirms the time, by the standard of other changes of this size. It saves several times what #200 does in allocation for about 330 lines of new classes, with codegen shared by both paths.

## Key decisions

**Codegen stops reading the tree, in both modes.** This is what keeps the fork small. Everything codegen asks of the tree today has an O(1) answer it can keep itself: `lastInsn`'s opcode (`ATHROW`, `ACONST_NULL` checks in `emitAdapt`, `isAtProgramPoint`), the pending line number that `lineNumber` overwrites, and `isAtUnreachablePoint`, which walks back through labels. A small `MethodVisitor` wrapper (call it the cursor) holds that state and forwards to whatever is underneath: a `MethodNode1` with `-opt`, a `MethodWriter` without. `jmethod` and `mnode` become the cursor, typed as `MethodVisitor`. `LabelNode1`'s flags move to a `Label` subclass, since `Label.info` is taken by `MethodNode`. This refactoring is worth a commit of its own, and should leave output byte-identical.

Remaining readers, each with a direct answer:

- `appendToStaticCtor` inserts the module init before each `RETURN` after the fact. The cursor emits it when the `RETURN` goes by.
- `emitInvoke` builds a `MethodInsnNode` for `callsitePositions`, and `recordInlineAnnotated` reads `lastInsn`. Both matter only to the inliner, so they stay behind `optInlinerEnabled`.
- `genInvokeDynamicLambda` reads the last `InvokeDynamicInsnNode` for the `indyLambdaImplMethods` registry. Direct mode only needs the handles, kept on the builder; `addLambdaDeserialize` already takes `Iterable[Handle]` and `ClassVisitor` methods, so it runs at the end of the class.
- `cnode.name` and `cnode.interfaces` come from `thisBType`. `addClassFields` uses `visitField`. Mirror classes and forwarders are already `ClassVisitor`-typed.

**The fork.** With the above, `-opt` and non-`-opt` differ in four places: which `ClassVisitor` `genPlainClass` and `genMirrorClass` create, `GeneratedClass` holding a node or a finished writer, the branch in `sendToDisk`, and how `setInnerClasses` gets its names. I expect a few hundred changed lines, most of them the mechanical refactoring. The long-term cost is that both paths stay supported, though the codegen above them is shared: bootstrapping exercises the tree path (the release build uses `-opt`) and most of partest the direct one.

**When direct applies.** `-opt` not set by the user, `-Ybackend-parallelism` 1, which is the default. `-Ybackend-tree` turns it off. `-opt:none` stays on the tree, because unlike a default compile it keeps empty line numbers. With parallelism the frame work would move under the frontend lock and undo what the threads are for, so that stays on the tree path. An explicit `-opt:unreachable-code` takes the tree path, as `UnreachableCodeTest` and friends expect.

**Unreachable code: accept ASM's `nop; athrow` for what codegen misses.** After #200 that is about 0.18% of methods in library and reflect, and `-opt:none` already emits exactly this today, so it is an output we already test. Default compiles also run `removeEmptyLineNumbers`, which `MethodEmitter` does online, and `removeEmptyExceptionHandlers`, whose absence shows in one method, `ClassBType.info`, where the handler stays around the `nop` of `synchronized {}`. No scan, because the scan needs a tree. Measured across library, reflect and compiler: 243 methods in 159 of 6,846 classes.

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

- DONE bound: workloads in `BackendBenchmark`, campaign `m5-backend-bound` (round 1 only)
- DONE cursor refactoring: codegen stops reading the tree, output byte-identical
- DONE direct path with `-Ybackend-tree`, recorded nested classes, class-level redo on exception
- DONE normalised comparison over library, reflect and compiler
- DONE all JUnit, partest `--run --jvm --pos --neg`, bootstrap to a byte-identical fixpoint, every class links with `-Xverify:all`
- DONE before and after on `phase`, allocation on a full compile, profiles
- TODO a full-compile A/B on a quiet machine, for the time
