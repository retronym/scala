# JPMS support in scalac: status and plan

## Problem

scalac has no notion of the Java Platform Module System. It compiles against the module path as if it were a flat class path, so it accepts references to packages that a module does not export (for example `jdk.internal.misc`), or to modules that are not in the resolved graph, and these fail later with `IllegalAccessError` or `NoClassDefFoundError`. It cannot compile a `module-info.java`, cannot emit `module-info.class`, and cannot honour the javac options that build tools pass (`--module-path`, `--add-modules`, `--add-exports`, `--add-reads`, `--patch-module`). Scala projects that want to ship as named modules, or test against them, have to fall back to javac or the class path.

## Where the prototype came from

- `retronym/topic/jpms` (2018-07): first prototype on 2.13.x. Introduced a `JpmsModuleSymbol` per module and computed the module graph in `Global.Run`.
- `retronym/topic/nio-file-tooling-jpms` (2018-09 to 2019-02): reworked and simplified version. Dropped `JpmsModuleSymbol` in favour of a module name string per top level class, moved graph handling into `JpmsClassPath`, added `module-info.java` parsing, JAR package indexing and a `-Yjpms` gate. This is the branch that was rebased.

Both had WIP status and were never opened as PRs.

## Design of the prototype

- `-Yjpms` gates everything. Setting `--module-path` replaces the usual `AggregateClassPath` with `JpmsClassPath`.
- `JpmsClassPath` wraps javac's `StandardJavaFileManager` configured with the javac module options, and `java.lang.module.Configuration` to resolve the graph. Sources being compiled form either the named module declared in a jointly compiled `module-info.java`, the module named in `--patch-module`, or the unnamed module.
- The graph is resolved lazily, after the parser has seen any `module-info.java`.
- Each top level `ClassSymbol` carries `associatedJpmsModuleName`, set from the class path location (binaries) or from the module graph (sources).
- `Contexts.isAccessible` additionally requires that the accessed top level class is in the same module, or in a package exported to the accessing module by a module it reads. Automatic modules export everything.
- The build requires JDK 17, so the Java 9+ API usage lives directly in `src/compiler` under `scala.tools.nsc.jpms`.

## DONE

- Rebased onto current 2.13.x as a single commit (the original history was WIP). Conflicts resolved against the `PackageName` based classpath API, `ClassRepresentation.fileName`, the `nameOf` interning in `SymbolLoaders`, compact Java source files in `JavaParsers`, and the sbt 1.x build syntax.
- `module-info.java` gets its own `modularCompilationUnit`, selected by file name, so it no longer competes with compact source file parsing. Imports and leading annotations are accepted. Fixed `requires static` parsing and the printer for `requires static`.
- Module declaration keywords moved to `JavaRestrictedIdentifiers`.
- Classes compiled from source are now assigned their module in `Namers`. Previously only `SourcefileLoader` did this, so sources passed on the command line were always treated as members of the unnamed module, and the `--patch-module` and "missing requires" cases passed or failed for the wrong reason.
- Non-existent entries on the class or module path no longer crash `JpmsClassPath`.
- `initializeFromClassPath` keeps the upstream source-versus-binary selection; the 2019 branch had changed it in passing.
- Dropped changes that upstream already has (public `PlainNioFile`) or that were unused (`ZipArchive.dirName`/`baseName` visibility, which also tripped MiMa).
- Regression: MiMa clean, full `junit/test` (3417) and partest pos/neg/jvm (3392) pass on JDK 21.
- Tests: `JpmsTest` rewritten to be self contained (its own `Global`, ASM-written `module-info.class`), and switched from `java.xml.bind` (removed in JDK 11) to `jdk.incubator.vector`, which is observable but not resolved by default. Covers unresolved module, unexported package, `--add-modules`, `--add-exports`, joint compilation with `module-info.java` with and without the needed `requires`, clients of a named module on the module path, `--patch-module`, `module-info.java` parsing and printing, and `module-info.java` being harmless without `-Yjpms`. `neg/jpms_unsafe` now uses `jdk.internal.misc.Unsafe`, since `sun.misc` is exported by `jdk.unsupported`.
- JDK 17 is the build baseline (`-source 17 -target 17` for Java sources). The `compiler-jdk11`/`compiler-pre-jdk11` split and the `FileManagerJava9Api` shim are gone. Multi-JDK support in the build is out of scope.
- `--add-reads` and `--add-exports` now apply to the module being compiled from source, and the unnamed module exports all its packages, so `--add-reads m=ALL-UNNAMED` works. Test: a named module does not read the class path unless told to.

## Two tracks

The work splits into two mostly independent tracks.

- **Track 1, JPMS semantics.** scalac implements the module system as specified, for both joint compilation (a `module-info.java` among the sources) and separate compilation (modules arriving as `module-info.class` on the module path, including modules that scalac itself produced). This is the main deliverable and needs matching changes in Zinc and sbt.
- **Track 2, javac's classpath as an option.** Reusing javac's `StandardJavaFileManager` for locating classes, which is what the prototype does today, becomes an opt-in implementation. Its main value is as an oracle: it lets us check the scalac implementation against javac's view of the same command line.

The seam between the tracks is `scala.tools.nsc.jpms.ModuleTable`, the module structure seen by one run. It answers which module a source file being compiled belongs to, and whether module `n` can access package `p` of module `m` (that is, `n` reads `m` and `m` exports `p` to `n`). The module of a class loaded from the class or module path is a property of its class path entry. A class path that knows about modules implements `ModuleResolver`, which builds a fresh `ModuleTable` for each `Run`, taking the run's `module-info.java` into account. Track 1 consumes the table in the typer and backend. Track 2 is one implementation of the resolver, plus class lookup.

## Track 1: JPMS semantics

### Gaps in scalac

1. **No `module-info.class` output.** A jointly compiled `module-info.java` feeds the access checks but is then dropped. For separate compilation the descriptor has to be written out with ASM's `ModuleVisitor`, with every directive and modifier, the `ModulePackages` attribute (computed from the classes emitted), and `MainClass` if applicable. `JpmsModuleDescriptor` currently carries only the `requires` names, without `transitive` and `static`. That doesn't matter for the module's own compilation, which reads its direct dependencies either way and, like javac, treats `requires static` as mandatory at compile time. It does matter once the descriptor is written.
2. **Module graph lifetime (fixed in step 1).** The graph is now resolved per `Run` (`Run.jpmsModuleTable`), so Zinc's cached compiler, the REPL and the presentation compiler see edits to `module-info.java`. A second `module-info.java` in one run is reported as an error. Multiple modules per run waits for `--module-source-path` (gap 6).
3. **Access check coverage.** The check works at top level class granularity in `Contexts.isAccessible`. Not yet considered:
   - type aliases and forwarders in an exported package that expose a type from a non-exported one;
   - implicit search turning up inaccessible implicits;
   - members inherited through a public supertype;
   - Java statics and package objects.
4. **Optimizer.** Inlining code from another module can copy references to packages that are not exported to the caller, or to modules the caller does not read, which fails at run time with `IllegalAccessError`. The inliner's accessibility checks (`classIsAccessible`, `memberIsAccessible` in `Inliner.scala`) have to consult the `ModuleTable`, and `-opt-inline-from` defaults need a look.
5. **`scala.library`.** Every Scala module needs `requires scala.library`. The question is whether scalac should report its absence with a hint, or add it implicitly. Implicit addition is a deviation from the spec that javac clients would not see. The 2.13 library is an automatic module, `scala.library`, through its `Automatic-Module-Name`. There are no split packages between library, reflect and compiler, which I checked against the build output.
6. **Options.** `--upgrade-module-path` and `--limit-modules` are declared but not wired in. `--module-source-path` (multi-module compilation) and multiple `module-info.java` files in one run are unsupported. `-Yjpms` must precede the module options because the check is a post-set hook. `-release` with modules relies on a `ct.sym` reader written for JDK 9/10, while upstream now has `CtSymClassPath` and `-system`.
7. **`-Yjpms` with only a class path does nothing.** Code in the unnamed module still gets no protection against JDK internals. This is probably the most common case in practice.
8. **Diagnostics.** The error is the generic "cannot be accessed as a member of". javac says which module declares the package and that it is not exported or not read, and suggests a fix; scalac should match. Module directives carry no positions, module annotations are dropped, and the declaration isn't validated (exported packages exist, `provides` implementations exist and conform, no duplicate directives).
9. **Run-time concerns outside the compiler.** These need `opens` or exports in user modules, and are probably documentation rather than compiler work:
   - structural types (`reflectiveSelectable`), which use Java reflection on the receiver's class;
   - Scala runtime reflection (`scala.reflect.runtime`).

### Gaps in Zinc and sbt

From a survey of the current Zinc (main) and sbt checkouts:

- **Zinc has no module path.** `CompileOptions` and `ClasspathOptions` carry only a class path, and `CompilerArguments` emits `-classpath` and, when `autoBoot` is set, a `-bootclasspath` holding scala-library. Under JPMS that puts the library in the unnamed module, where `requires scala.library` cannot see it. Module options can only be smuggled in through `scalacOptions`/`javacOptions`.
- **Smuggled module-path jars are invisible to invalidation.** Any change to the options forces a full recompile, which is safe. But the module path jars are left out of the classpath hash, and `Locate`'s flat lookup returns nothing for them, so a changed dependency on the module path is either missed or causes spurious invalidation.
- **Mixed compilation and `module-info.java`.** In `Mixed` order Zinc passes all sources to scalac, including `module-info.java`, and then the Java sources to javac, with the Scala output on javac's `-classpath`. With a named module, javac would then see the Scala classes in the unnamed module, which the named module cannot read. It needs `--patch-module <module>=<scala output>` instead. Only one of the two compilers should emit `module-info.class`.
- **`module-info.java` edits are not tracked.** No class depends on `module-info.java`, so editing `requires` or `exports` recompiles only that file, although it changes accessibility for the whole module. A dependency's `module-info.class` is likewise only noticed indirectly. This needs whole-module invalidation.
- **`module-info.class` is not recorded as a product.** Zinc's `JavaAnalyze` deliberately skips it, and the bridge records only classes derived from symbols. The class file manager never deletes or restores it, so it goes stale when `module-info.java` is removed.
- **javac output analysis uses a flat `URLClassLoader`.** Classes that need module options to load fall back to class-file-only analysis, which loses API and inheritance dependencies.
- **sbt has no module-path keys.** Nothing splits `dependencyClasspath` into class path and module path, and forked `run` and `test` launch with `-classpath` only.

So Zinc needs: module-path inputs in `CompileOptions`/`ClasspathOptions` and `CompilerArguments`; module path in hashing and lookup; `--patch-module` for javac in mixed builds; `module-info.java` as a module-wide invalidation trigger; `module-info.class` as a product; and module-aware analysis of javac output. sbt needs keys for the module path and module options, a policy for splitting the dependency classpath, `autoBoot` off for modular builds, and module-aware forked run and test.

### Plan

Each step leaves the branch green.

1. DONE: Introduced `ModuleTable`/`ModuleResolver` and routed the parser, `Namers`, `SymbolLoaders` and `Contexts` through `Run.jpmsModuleTable`, removing the `jpms*` methods from `ClassPath`. The accessibility cache moved from the class path to the per-run table. Tests: one `Global` across four runs that add, change and remove `module-info.java`; two `module-info.java` files in one run.
2. TODO: Native implementation of `ModuleTable` over scalac's existing classpath. Each `ClassPath` entry is tagged with its module: from `ModuleFinder.of(path)` for module path entries, `jrt:/modules/<name>` for the system image, overlays for `--patch-module`, unnamed for the class path. `java.lang.module.Configuration` handles resolution only. This keeps scalac's classpath caching and fixes the empty `asURLs`, which today breaks macros from the module path. It becomes the default under `-Yjpms`.
3. TODO: Emit `module-info.class` and complete `JpmsModuleDescriptor` (gap 1). Then test separate compilation end to end: scalac module A, then scalac module B against A's output, then javac clients of A, then scalac clients of a javac-built module, and run each on `java --module-path`.
4. TODO: Access check coverage with a neg test per case (gap 3), and the inliner (gap 4).
5. TODO: Options and `-release` (gap 6), diagnostics (gap 8), and the `scala.library` question (gap 5).
6. TODO: Zinc prototype on a branch of the local checkout: the module-path inputs, `--patch-module` for javac, `module-info.java` invalidation and the product. Use it to settle the command line interface before it hardens in scalac. Then the sbt keys, with a scripted test of a two-module mixed Scala/Java build that runs on the module path.

## Track 2: javac's classpath, opt-in

The current `JpmsClassPath` is this track. Using javac's file manager gives javac's exact interpretation of the module options and locations. Its costs are why it should not be the default:

- it ties scalac to the `java.compiler` module, which is missing from minimal runtimes;
- it duplicates and bypasses scalac's classpath caching, listing every location per package and indexing every JAR eagerly at startup;
- `asURLs` and `asClassPathStrings` are empty.

### Plan

1. TODO: Once Track 1 step 2 lands, keep `JpmsClassPath` behind `-Yjpms-classpath:javac`, as one `ModuleResolver`. Implement `asURLs` so that macros work in this mode too.
2. TODO: Oracle mode, `-Yjpms-classpath:check`. It runs the native implementation and consults the javac one in parallel, comparing package listings, the module assigned to each class looked up, and every accessibility answer, and reporting discrepancies as internal warnings. This is cheap to add once both implement `ModuleResolver`.
3. TODO: A junit sweep that runs the oracle over the whole JDK image plus a module path of representative jars (automatic, explicit, multi-release, patched), and fails on any discrepancy.
4. TODO: Measure both implementations on a large module path. If the javac implementation turns out competitive, revisit which one is the default.

## Future work

- Access checks for the unnamed module on a plain class path (Track 1 gap 7).
- `--module-source-path` and multi-module compilation.
- REPL and Scaladoc with a module path.
- Scala 3 has no JPMS support either. The design of `ModuleTable` and of the Zinc interface should not be specific to scalac 2.
