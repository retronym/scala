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

1. **`module-info.class` output (done in step 3).** Under `-Yjpms`, a jointly compiled `module-info.java` is written as `module-info.class` (class file version at least 53), with every directive and modifier. `uses` and `provides` type names are resolved against the symbol table and the file's imports, to binary names. Not written: the `ModulePackages` attribute, which needs all of the module's packages and which javac also leaves to the `jar` tool; `MainClass`; the compiled versions of required modules; and module annotations. `requires static` is dropped from the descriptor used for resolution, because javac treats it as mandatory at compile time.
2. **Module graph lifetime (fixed in step 1).** The graph is now resolved per `Run` (`Run.jpmsModuleTable`), so Zinc's cached compiler, the REPL and the presentation compiler see edits to `module-info.java`. A second `module-info.java` in one run is reported as an error. Multiple modules per run waits for `--module-source-path` (gap 6).
3. **Access check coverage (done in step 4).** `Contexts.isJpmsAccessible` follows JLS 6.6.1 and JVMS 5.4.4 with modules:
   - a class or object is checked by the package of its top level class;
   - a type alias is checked itself, and so is the class it stands for, because any use of the alias in bytecode names that class;
   - a method or field is checked through its qualifying type, so that public members which an exported class inherits from an unexported one are accessible, as in Java. The backend already uses the qualifying class as the owner of the member reference;
   - members of package objects are checked by the package object's class.

   The accessing module is the compilation unit's, not the enclosing class's, because top level code in a file has a package as its enclosing class. Binary classes now get their module when they are entered from the class path, not when they are completed. Before, a symbol could be access checked before completion, or, for package objects, never tagged at all. Implicits from companions in unexported packages drop out of implicit search, because they fail the same check. Not yet handled: casts to an inaccessible class that erasure inserts for an inferred type, which would fail at run time where javac reports an error. A backend check of the classes referenced from emitted bytecode would catch those, and the remaining cases too.
4. **Optimizer (done in step 4).** The inliner's `classIsAccessible` adds the module condition of JVMS 5.4.4: a public class is accessible only if its package is accessible from the module of the destination class. `memberIsAccessible` applies it to the class in the member reference, not the declaring class, matching the typer. The module of a class compiled in the run comes from its source, otherwise from its class path entry.
5. **`scala.library` (decided in step 5).** Every named module with Scala sources must read the module of package `scala`, which is checked once, at the start of namer, with an error at the module declaration that says how to fix it. Explicit uses of the library would be caught one by one, but scalac's own references (`Product` for case classes, `scala.runtime` helpers) are not access checked, so a module could compile and then fail at run time. scalac does not add the `requires` itself, because `module-info.class` must say what `module-info.java` says, for javac and the JVM. Kotlin takes the same line with `kotlin.stdlib`. With the library on the class path, the error says to move it to the module path, where it is the automatic module `scala.library`. The compiler-synthesized `Any`, `AnyRef`, `Nothing` and so on belong to no module and are always accessible. There are no split packages between the library, reflect and compiler JARs.
6. **Options (mostly done in step 5).** The module options have javac's names (`--module-path`/`-p`, `--add-modules`, `--add-exports`, `--add-reads`, `--patch-module`, ...) and syntax: one value per occurrence, commas kept, and `--opt=value` accepted. Using them without `-Yjpms` is an error, whatever the order. `--release N` (N >= 9) resolves against Java SE N's modules, read from `ct.sym` (`SystemModules.ofRelease`), and `-system <jdk>` against that JDK's. `--release 8` is an error. Still not wired in: `--upgrade-module-path`, `--limit-modules`, and `--module-source-path` (multi-module compilation). The javac class path implementation still uses the JDK 9/10 `ct.sym` layout, so `-release` only works with the native one.
7. **`-Yjpms` with only a class path (fixed in step 2).** The native class path is used whenever `-Yjpms` is set, so code in the unnamed module is checked against the JDK's exports even without `--module-path`. `neg/jpms_unsafe` covers this.
8. **Diagnostics (mostly done in step 5).** JPMS access errors carry javac's explanation and a fix: "package p is declared in module m, which does not export it [to module n]", "... but module n does not read it", "... which is not in the module graph", followed by the `--add-exports`, `--add-reads`, `--add-modules` or `requires` that would help. Module directives now carry positions. Names, duplicate directives and the existence of `uses`/`provides` types are validated, but not that exported and opened packages exist in the module, nor that providers implement the service and have a public no-argument constructor or `provider()` method.
9. **Run-time concerns outside the compiler.** These need `opens` or exports in user modules, and are probably documentation rather than compiler work:
   - structural types (`reflectiveSelectable`), which use Java reflection on the receiver's class;
   - Scala runtime reflection (`scala.reflect.runtime`).

### Lookup edge cases (deliberately not handled in step 2)

Lookup through `ModulePathClassPath` is flat. A class is taken from the first entry that has it, and the module graph is consulted afterwards for accessibility. This matches javac whenever no package appears in more than one observable module. It differs where a package does:

- **An unresolved module shadows a readable package.** If a package is in a module that is observable but not resolved (on the module path but not a root or required), and the same package is also on the class path, javac uses the class path copy. scalac finds the unresolved module's class first and then reports it inaccessible. Example: the scala library patched into `scala.library` on the module path and also present through `-usejavacp`, with code in the unnamed module and no `--add-modules scala.library`. `macroFromModulePath` in `JpmsTest` needed `--add-modules` for this reason.
- **A named module's package also on the class path.** JLS 7.4.3: a package in a named module that the accessing code reads takes precedence over the same package on the class path. scalac picks whichever entry comes first. Tagged entries come before the class path, so this works today, but only because of the ordering.
- **Split packages between unread modules.** If two observable modules contain the same package and only one is read, scalac may find the class in the unread one.

The fix, when it is needed, is for the class path to drop a package's entries that belong to modules the run cannot see, given the run's `ModuleTable`. That is a per-run filter, not per-module lookup scopes. The Track 2 oracle is the tool for finding further divergences.

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
2. DONE: `ModulePathClassPath`, the default under `-Yjpms`, builds on the ordinary `AggregateClassPath`. `ModuleTaggedClassPath` wraps entries to tag their classes: system classes by their `jrt:/modules/<m>` path, module path entries by the name `ModuleFinder` gives them, and `--patch-module` paths by the patched module, placed ahead of everything else. The class path stays untagged, which means unnamed. Resolution moved out of the javac-specific code into `ModuleGraphResolver`, which both implementations share. `asURLs` works, so macros load from the module path. `-Yjpms-classpath:javac` selects the old implementation. Fixed on the way: default root modules for the unnamed module (JEP 261), `ALL-DEFAULT`/`ALL-SYSTEM`/`ALL-MODULE-PATH`, several `--add-exports` for one module, sources in the module of a `module-info.class` already in the output directory, and `--patch-module` with several paths. `JpmsTest` runs against both implementations.
3. DONE: `module-info.class` is emitted from GenBCode by `ModuleInfos`. `JpmsModuleDescriptor` is gone. The parser's `Jpms.ModuleDef` becomes a `java.lang.module.ModuleDescriptor` directly, through the strict builder, which also rejects invalid names and duplicate directives. A `module-info.class` already in the output directory makes the sources members of that module, for incremental compilation, and a `module-info.java` among the sources takes precedence over it. The parser now accepts `requires static transitive` in either order, and `requires transitive;` naming a module. `separateCompilationEndToEnd` covers: scalac module `a` (its descriptor read back with `ModuleFinder`), scalac module `b` against `a`, javac module `c` against `a` (javac rejects `a`'s unexported package), scalac against javac-built module `d`, and finally `java --module-path … -m b/b.main.Main`, including a `ServiceLoader` lookup of `a`'s provider.
4. DONE: Access checks and the inliner, as described under gaps 3 and 4. `JpmsTest` has a test per case against a module `a` that exports `a.api` but not `a.impl`: a public member inherited through an exported class (also run on the module path), a member of an inaccessible qualifying type, type aliases, package object members, implicits, Java statics, and the inliner (not inlining a method that refers to `a.impl`, inlining one that only refers to `a.api`).
5. DONE: Option syntax and `-release`/`-system` (gap 6), diagnostics (gap 8), and the `scala.library` policy (gap 5). Tests: option syntax, options without `-Yjpms`, exact diagnostics for each kind of denial, a case class in a module that doesn't require the library (and with the library on the class path), `--release 11` (Nashorn is observable, `java.sql` is a default root, internal packages are not observable, `--release 8` is rejected), and `-system`.
6. TODO: Zinc prototype on a branch of the local checkout: the module-path inputs, `--patch-module` for javac, `module-info.java` invalidation and the product. Use it to settle the command line interface before it hardens in scalac. Then the sbt keys, with a scripted test of a two-module mixed Scala/Java build that runs on the module path.

## Track 2: javac's classpath, opt-in

The current `JpmsClassPath` is this track. Using javac's file manager gives javac's exact interpretation of the module options and locations. Its costs are why it should not be the default:

- it ties scalac to the `java.compiler` module, which is missing from minimal runtimes;
- it duplicates and bypasses scalac's classpath caching, listing every location per package and indexing every JAR eagerly at startup;
- `asURLs` and `asClassPathStrings` are empty.

### Plan

1. TODO: `JpmsClassPath` is now behind `-Yjpms-classpath:javac` and shares `ModuleGraphResolver`. Still to do: implement `asURLs`, so that macros work in this mode too, and stop it listing the same class twice when a `--patch-module` path overlaps the class path. That currently crashes with "Found scala.Function2$mcIII$sp as class Function2$mcIII$sp" (the `-usejavacp` macro test, run with this implementation).
2. TODO: Oracle mode, `-Yjpms-classpath:check`. It runs the native implementation and consults the javac one in parallel, comparing package listings, the module assigned to each class looked up, and every accessibility answer, and reporting discrepancies as internal warnings. This is cheap to add once both implement `ModuleResolver`.
3. TODO: A junit sweep that runs the oracle over the whole JDK image plus a module path of representative jars (automatic, explicit, multi-release, patched), and fails on any discrepancy.
4. TODO: Measure both implementations on a large module path. If the javac implementation turns out competitive, revisit which one is the default.

## Future work

- Access checks for the unnamed module on a plain class path (Track 1 gap 7).
- `--module-source-path` and multi-module compilation.
- REPL and Scaladoc with a module path.
- Scala 3 has no JPMS support either. The design of `ModuleTable` and of the Zinc interface should not be specific to scalac 2.
