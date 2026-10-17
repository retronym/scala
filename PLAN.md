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
- Java 9+ API usage is isolated in `src/compiler-jdk11`, which is only added to the compiler's sources when building on JDK 11+. `src/compiler-pre-jdk11` holds stubs.

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

## Gap analysis

### Blocking for any merge

1. **Build produces a JDK-dependent compiler jar.** Whether `compiler-jdk11` is compiled in depends on the JDK running sbt. 2.13.x CI and releases build on JDK 8, so the published compiler would never contain JPMS support, and a JDK 11 build would contain classes that reference Java 9 APIs. The 2019 branch worked around this with a `-jdk11` version suffix, which is not an option. The Java 9 code needs to be compiled with `--release 11` into a separate output that is always built (CI would need a JDK 11+ toolchain for that step) and merged into the compiler jar, and reached from the main compiler only reflectively or through a small interface so that nothing on JDK 8 links against it. The `compiler-pre-jdk11` stubs are also stale (`RequireDirective` is nested in the real class but top level in the stub) and have not been compiled.
2. **Macros and plugins on the module path do not load.** `JpmsClassPath.asURLs` and `asClassPathStrings` return `Nil`. The macro class loader (`Plugins.findMacroClassLoader`) is built from `classPath.asURLs`, so macro implementations from libraries on the module path are not found. `-Ylogcp` and anything else that reports the class path also sees nothing.
3. **No `module-info.class` output.** A jointly compiled `module-info.java` is parsed and used for access checks but dropped. Users still need javac to produce the descriptor, which defeats the purpose for pure Scala modules. The backend should emit it with ASM's `ModuleVisitor`, including the `ModulePackages` attribute and the `MainClass` attribute when there is a main class.
4. **Resident compilation is unsupported.** The module graph is resolved once per `JpmsClassPath` and `jpmsRegisterModuleInfo` asserts it is called at most once. A second `Run` (sbt's cached compiler, the REPL, the presentation compiler) that contains `module-info.java` will fail the assertion, and a change to `module-info.java` is never picked up. The graph belongs to the `Run`, as the 2018 branch had it.

### Correctness

5. `JpmsModuleDescriptor.create` only transfers `requires`, without the `transitive` and `static` modifiers, and `exports`, `opens`, `uses` and `provides` are parsed but unused. For compiling the module itself this is harmless: it reads its direct dependencies either way, and javac also treats `requires static` as mandatory at compile time. It matters as soon as the descriptor is written out (item 3), where every directive and modifier must survive.
6. `--upgrade-module-path` and `--limit-modules` are declared but not passed to the file manager or the resolver. `--module-source-path` (multi-module compilation) is not supported at all, nor are multiple `module-info.java` files in one run.
7. The interaction with `-release` is untested on modern JDKs. `CtSymClassAndModulePath` was written against the JDK 9/10 `ct.sym` layout. Upstream has since gained `CtSymClassPath` and `-system`; the JPMS path should reuse those instead of carrying its own copy.
8. The access check works at top level class granularity. Not yet considered: type aliases and `val` forwarders in an exported package that expose a type from a non-exported one, implicit search returning inaccessible implicits, members inherited through a public supertype, Java static members, and package objects. The relevant `TODO JPMS` comments are in `Contexts.scala`.
9. `-Yjpms` must precede `--module-path` and friends on the command line, because the check is a post-set hook. It should be validated once all arguments are processed.
10. `-Yjpms` with a plain class path does nothing, so it gives no protection against JDK internals for code in the unnamed module. That case is probably the most common one in practice and should get access checks too.
11. The `ClassAndSourceFilesEntry` merge in `AggregateClassPath` has `TODO JPMS` markers and loses the module name in one branch.
12. Module annotations (`@Deprecated module m {}`) are parsed and dropped. Directives carry no positions, so errors cannot point into `module-info.java`. There is no validation of the declaration itself (exported package exists, `provides` implementation exists and is a subtype, no duplicate directives).
13. Error messages are the generic "cannot be accessed as a member of". javac reports "package p is declared in module m, which does not export it to module n" and suggests `--add-exports` or `requires`; scalac should do the same.

### Performance

14. `JpmsClassPath.classes` calls `fileManager.list` for every location for every package, and `findClassFile` scans every location. The JAR package index opens and walks every JAR eagerly at startup. Upstream's classpath has had significant work on caching (`ZipAndJarFileLookupFactory`, shared `JrtClassPath`) that the JPMS path bypasses.
15. Every `ClassSymbol` gains a field for the module name, and every `PackageClassSymbol` a cached `fullNameString`. This should be measured, or the module name derived from the class path entry via `associatedFile` instead of stored per symbol.

### Design questions

16. **Keep javac's file manager or not.** Using `StandardJavaFileManager` buys exact javac semantics for option parsing and location handling, but it ties scalac to the `java.compiler` module (absent from minimal jlinked runtimes), duplicates the classpath implementation, and is the source of items 2 and 14. The alternative is to keep scalac's existing `ClassPath` implementations, tag each entry with its module (from `ModuleFinder` over the module path, `jrt:/modules/<name>` for the system image, `--patch-module` for overlays), and use `java.lang.module.Configuration` only for resolution and the exports table. I would lean towards the second option.
17. **Build tool story.** sbt/Zinc, Mill, Maven and Gradle all need to pass the module path and options through, and Zinc needs to track `module-info.java` as a source whose change invalidates everything. This should be scoped before the compiler side is finalised so the command line interface matches what tools can produce.
18. **Scope for Scala 2.13.** Whether this lands as an experimental `-Y` option on 2.13.x, and how it relates to Scala 3, which has no JPMS support either.

## Plan

Each step should leave the branch compiling with green tests.

1. TODO: Move the Java 9+ code into its own sbt project compiled with `--release 11`, merged into the compiler jar and loaded reflectively; delete `compiler-pre-jdk11`. Verify the compiler still builds and runs on JDK 8 with `-Yjpms` reporting a clear error. (Gap 1)
2. TODO: Decide gap 16. Prototype tagging the existing classpath entries with modules and resolving with `Configuration`, and compare on a real project such as a module-path build of a library with many dependencies. Either way, implement `asURLs`/`asClassPathStrings` and add a test with a macro from a module path library. (Gaps 2, 14, 16)
3. TODO: Move module graph state from `JpmsClassPath` to `Run`, so each run sees its own `module-info.java`. Test with two runs on one `Global`. (Gap 4)
4. TODO: Emit `module-info.class` from `Jpms.ModuleDef` in the backend. Complete `JpmsModuleDescriptor` with all directives and modifiers. Test by loading the output with `ModuleFinder` and by running javac clients against it. (Gaps 3, 5)
5. TODO: Wire `--upgrade-module-path`, `--limit-modules`; validate options after argument processing; reuse upstream `CtSymClassPath`/`-system` for `-release`. (Gaps 6, 7, 9)
6. TODO: JPMS-specific error messages with positions in `module-info.java`; validate the module declaration. (Gaps 12, 13)
7. TODO: Work through the access-check cases in gap 8 with neg tests for each.

Future work:

- Access checks for the unnamed module on a plain class path (gap 10).
- `--module-source-path` and multi-module compilation.
- Zinc / sbt integration (gap 17).
- REPL and Scaladoc with a module path.
