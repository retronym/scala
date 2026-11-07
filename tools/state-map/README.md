# State map

A model of the mutable state of the Scala 2 compiler (`src/reflect` and `src/compiler`), extracted from its typed trees, with a class diagram and a work list. It is the Scala 2 counterpart of the Scala 3 state map (retronym/scala3 `tools/state-map`), and supports porting the parallel batch typechecker (`scalac -Yparallel-typer -Ystop-after:pickler`, retronym/scala3#84) to scala/scala 2.13.x. Each field is sorted by kind (A per-operation scratch, B lazy facts with a black hole, C caches of derived values, D interning and counters, E trees, units, run and universe state, L lazy vals), by the discipline that protects it from concurrent threads, by its reach (typer or post-pickler), by the instance it lives on, and by hazard. The main use right now is `worklist.md`: the shared mutable state reachable during parser → namer → packageobjects → typer → superaccessors → extmethods → pickler that nothing protects.

- `StateMap.scala`: the extractor, a scala-cli script on Scala 2.13.19-M1 (the STARR of `versions.properties`) that depends on the same `scala-compiler`. It runs a `Global` to `-Ystop-after:typer` over every `.scala` and `.java` file of `src/reflect` and `src/compiler`, with the library's classes on the class path, then walks the typed trees twice: once to find the methods that run a by-name argument only under `synchronized`, then for the fields and every read and write site with the enclosing method and class. It writes `state-map.json` (every site), `state-map.data.js` (the same JSON as a script for the page, at most 60 sites per field) and `worklist.md`, and prints the kind × protection table and the fields no kind rule places.
- `run.sh`: asks sbt for `compiler/dependencyClasspath` (this builds the library and reflect classes) and `compiler/Compile/scalacOptions`, drops the reflect classes from the class path (their sources are typechecked instead), and runs the extractor. Typing takes about 10 s and the whole run two to three minutes, most of it sbt.
- `annotations.tsv`: declared overrides, one row per field (owner, field, kind, protection, note). A row takes precedence over the rules (`kindSource`/`protectionSource: annotated`; the rules' answer is kept as `inferredKind`/`inferredProtection`), and its note becomes the field's suggested discipline in the work list. It stands in for source annotations (`@scratch`, `@lazyFact`, `@cache`, `@interned`, `@treeState`, `@guardedBy`, `@confined`, `@publishedBy`, `@benignRace`, as in Scala 3), which are the follow-up: annotating the sources now would conflict with the parallel work in other worktrees.
- `index.html`: the diagram, reading `state-map.data.js`, so it works from `file://`. It loads elkjs from jsDelivr for the layout and draws plain SVG.
- `worklist.md`: generated; committed because other agents read it.

```bash
tools/state-map/run.sh
open tools/state-map/index.html
```

## Rules

Each rule is listed with the same text in the JSON's header and in the page's Rules panel; this is a summary. The first rule that matches sets the kind, the protection and the reach. A field no kind rule matches is `unclassified`.

Fields: every `var` (including `private[this]` and constructor `var`s), every `val` holding a mutable structure (`scala.collection.mutable`/`concurrent`, `java.util` collections and maps, `java.util.concurrent`, arrays, `ThreadLocal`, `StringBuilder`, `WeakHashSet`, `Scope`, `IdCounter`, `ReusableInstance`, `UndoLog`, the `Statistics` counters) and every `lazy val` member of a class, trait or object. A getter, a setter and the field behind them are one field; in a trait, where typer has no field yet, the getter stands for it. A write is an assignment, a setter call, or a call of a mutating method (`+=`, `update`, `getOrElseUpdate`, `enter`, `clear`, …) on the field. A read-modify-write is a write whose right-hand side reads the same field of the same receiver. The internals of `internal.util.WeakHashSet`, `HashSet`, `Set`, `Collections` and `IdCounter` are left out (modelled where held).

Kinds: `L.lazy` (lazy vals, with a coarse flag for an initialiser that can force symbol completion: it calls `info`, `tpe`, `baseClasses`, `getRequiredClass`, `member`, … or another lazy val of the compiler); `A.scratch` (`SymbolTable#Scratch`, `Frame` and its guards, from retronym/scala#212), `A.traversal` (type maps, folders, collectors, tree traversers and transformers, `FindMemberBase`, printers), `A.typer` (`Typer`, `Context`, `Inferencer`, `ImplicitSearch`, `Namer`, `MacroExpander` and traits mixed into them, then any other non-singleton class of `nsc.typechecker`), `A.inference` (`TypeVar`, `TypeConstraint`, `UndoLog`), `A.iterator`, `A.parser`, `A.macro`; `B.symbol` (`_rawflags`, `infos`, `_validTo`, `_rawowner`, `_rawname`, `_annotations`, `_privateWithin`, …, and `TypeHistory`), `B.completer` (`LazyType` subclasses, `SymbolLoader`, `LazyAnnotationInfo`), `B.unpickler` (`internal.pickling`, `nsc.symtab.classfile`, `nsc.tasty`, `tools.tasty`, `SymbolLoaders`), `B.scope` (`Scope`, `ScopeEntry`); `E.ambient` (a var or collection on a universe singleton named `ph`, `per`, `phase…`, `cur…`, `current…`, `last…`, `…Depth`, `…Stack`, …; tried before C and D); `C.stamp` (`Period`/`RunId` or named so), `C.type-memo` (any other field of a `Type` but `TypeVar`), `C.bts` (`BaseTypeSeq`), `C.per-run` (made by `perRunCaches.new…`), `C.name`; `D.uniques`, `D.names`, `D.counter`; `E.tree`, `E.unit`, `E.run` (run, reporting, statistics, profiler), `E.settings`, `E.universe` (anything else on a universe singleton).

Protection: `thread-confined` (a `ThreadLocal`; `A.scratch`; any other kind A whose owner is not a universe singleton), then by reach `post-pickler`, `runtime-reflection`, `driver`; `volatile`; `concurrent` (a concurrent type or atomic, or `Collections.synchronized…`); `seam` (declared `IdCounter`, or initialised by an expression that tests `isConcurrentUniverse` or `synchronizeNames`: the universe picks a thread-safe implementation when it is concurrent); `lazy-monitor` (lazy vals); `init-only` (no write outside initialisation); else by the accesses outside initialisation, innermost first: under `x.synchronized` or as its receiver → synchronized; in the by-name argument of a method that runs it only under `synchronized` → synchronized; under `gilSynchronized` → gil; in the else branch of `if (isConcurrentUniverse)`/`if (synchronizeNames)` → batch; in a private or local method, the one protection every call of it has; else none. The field's protection is the one protection of its non-batch accesses, `batch-only`, `partial:P` (some P, some none), `mixed:P,Q`, or `unknown` (no access outside initialisation).

Reach: `runtime` for `scala.reflect.runtime`; `driver` for the compile server, command line, class path, settings, plugins, `PipelineMainClass`, the interactive scheduler and the ToolBox; `post-pickler` for `nsc.transform` (but `ExtensionMethods`, `Transform`, `TypingTransformers`), `nsc.backend`, `RefChecks`, `Duplicators`, `TreeCheckers`, `TreeBrowsers`, `NodePrinters`, or when every access outside initialisation is in such an owner; `typer` otherwise.

Instance: `universe` for the cake slices of `Global` and `SymbolTable` (their base classes, the objects they declare, the components held in their vals and lazy vals such as `lazy val analyzer = new { … } with Analyzer`, and classes whose every concrete subclass is such a singleton, such as `DefinitionsClass`); `static` for top-level objects; else `symbol`, `type`, `tree`, `scope`, `context`, `typer`, `completer`, `traversal`, `name`, `run`, `unit`, `annotation`, `position`, `source`, `phase`, or `instance`, by base class. `sharedVia` lists the universe singletons that are instances of a per-instance class (`object typer extends analyzer.Typer`, `object` type maps).

Hazards: `ambient`, `shared-scratch` (kind A on a universe singleton), `shared-singleton` (kind A whose class also has a universe singleton instance), `context-escape` (a typer `Context` field written after construction: completers capture contexts and run on whichever thread forces the symbol), `black-hole` (kind B), `sentinel` (written in a method that throws a cyclic-reference error, or for B and C read in one), `rmw`, `stamp-pair` (a stamp, or a field sharing a stem with one: `parentsCache`/`parentsPeriod`), `collection` (an unsynchronised mutable collection with no protection), `lazy-monitor` (a lazy val on a shared instance whose initialiser can force completion).

The work list: fields with reach `typer` and protection `none` or `partial`, plus fields with the `lazy-monitor`, `shared-singleton` or `context-escape` hazard, each once under its most severe hazard, in this order: ambient globals; scratch on a shared object; context state reachable from completers; per-instance state reached through a universe singleton; sentinels; black-hole protocols; read-modify-write; stamp pairs; collections; lazy-val monitors; the rest.

## Counts

At `fd95c5c1c1` (retronym/scala#212 and its two follow-ups): 1,951 fields in 597 classes, 58,813 access sites. Reach: typer 1,294, post-pickler 479, driver 139, runtime 39. The work list has 656 fields: 20 ambient, 3 shared scratch, 13 context, 6 shared singleton, 12 sentinel, 81 black hole, 42 read-modify-write, 12 stamp pairs, 74 collections, 302 lazy-val monitors (256 of them in `DefinitionsClass`), 91 others.

| protection | A | B | C | D | E | L | unclassified | total |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| batch-only |  |  | 1 | 1 |  |  |  | 2 |
| concurrent |  | 1 |  | 2 | 9 |  | 3 | 15 |
| driver |  |  | 1 |  | 61 | 34 | 43 | 139 |
| init-only | 3 | 12 | 9 | 1 | 75 |  | 10 | 110 |
| lazy-monitor |  |  |  |  |  | 551 |  | 551 |
| none | 3 | 83 | 70 | 2 | 95 |  | 73 | 326 |
| partial |  |  |  | 4 | 5 |  |  | 9 |
| post-pickler | 6 |  | 27 | 4 | 49 | 165 | 138 | 389 |
| runtime-reflection |  | 4 |  |  | 1 | 23 | 10 | 38 |
| seam |  |  |  | 2 |  | 1 |  | 3 |
| synchronized |  |  |  | 1 |  |  |  | 1 |
| thread-confined | 341 |  | 1 |  | 3 |  | 1 | 346 |
| unknown |  | 1 | 2 |  | 12 |  | 2 | 17 |
| volatile |  |  | 4 |  |  |  | 1 | 5 |
| total | 353 | 101 | 115 | 17 | 310 | 774 | 281 | 1,951 |

92 fields with reach typer are unclassified (the run prints them by package): class loaders and zip archives in `reflect.io` and `internal.util`, printers' flags, `Flags`' tables, `Importers`, `InfoTransformer` links, `Phase#nx`, `Mirrors.RootsBase#initialized`, `SymbolTrackers`, `DocComments.UseCase`, `ResetAttrs`. Most are `init-only` or one-off tools; the zip archives are read by classfile loading and belong on the work list's radar.

## Known gaps

- Hand-written accessor pairs are not fields: `lockedCount += 1` through `def lockedCount_=` is a read-modify-write of `_lockedCount` that the rules do not see (annotated instead). The same holds for any field reached only through explicit `def`s.
- Protection is lexical, plus the one-level propagation through private and local methods. A lock held further up the stack is invisible, and so is code run in a closure later or on another thread.
- Reach is by owner (and "every access is post-pickler"), not by a call graph from the typer: a typer-owned field touched only by later phases through a public method stays `typer`, and a post-pickler owner's field read by the typer is `post-pickler`.
- `forcesCompletion` on lazy vals is a name-based heuristic over the initialiser's direct calls; it does not follow calls. `sentinel` is a heuristic too, and misses in-progress markers checked without a cyclic-reference error.
- Object (module) initialisation also runs under a monitor and can force completion; it is not modelled. Nor are Java fields (Java sources are parsed but have no bodies at typer), or static state in Java classes.
- The mutating-method list is by name, so a mutation through an unlisted method counts as a read.
- `isConcurrentUniverse` alternatives other than `uniques` are not paired with their seam: a batch-only implementation class shows as unprotected unless `annotations.tsv` says `batch-only` (as for `PerRunUniques`).
- No checks against an `expected.tsv` yet: Scala 2 has no design notes like Scala 3's §8 to validate against.
