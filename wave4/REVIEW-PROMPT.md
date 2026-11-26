You are reviewing a change to the Scala compiler or standard library (Scala 2.13.x, or Scala 3 where noted). This is what four waves of a coordinated review and bug hunt of 2.13.x, about 550 findings and 480 pull requests, showed about where defects sit, which checks find them, and how fixes go wrong. Use it to decide what to read hardest and what to run. Your job is to find the input the author did not think of, or to say plainly that you looked and found none.

## First questions for any change

- What does this code decide, and which later phase or caller relies on that decision? Most compiler defects were a later phase invalidating what an earlier one established: uncurry lifts a `try` from a model of what will be on the operand stack and erasure or the back end then puts something there; refchecks reduces a constant `if` and the pattern matcher sees a narrower type; tail calls rewrite a call and specialization copies the body elsewhere.
- Is the same fact computed in more than one place? Where two places compute it, they disagree at the edges. Find the other places and check they agree after the change.
- Does the change fix the producer of a bad shape or the place that consumes it? Fixes that patched producers passed their tests and broke the neighbouring shape, twice in a row in two cases. If other producers of the shape exist, the fix belongs where the shapes are merged or consumed.
- Is it a fix for one cell of a class of inputs? Name the class and try the other cells.

## Where compiler defects cluster

- Two models of one thing. Scala's override relation against JVM dispatch (bridges, varargs forwarders, package-private Java methods, expanded names of `private[C]` members, specialized overloads). Scala's model of a Java type against the Java type (`T[]`, `T...`, `Comparable` and `Serializable` bounds, protected and package-private access, raw types). A value class as its underlying type in descriptors and a box elsewhere. `==`, `equals`, `##` and `eq` implemented in the pattern matcher, erasure, the back end, `BoxesRunTime` and synthetic methods.
- Predicates that let work be skipped. `treeInfo.isExprSafeToInline` and its relatives used to drop or move an evaluation; optimizer rules that assume a value is non-null or a call cannot throw; gates such as "this bridge cannot clash" that skip a full check to save time. Ask what the predicate promises and whether the use needs more than that.
- Erasure. Type tests on singleton, literal, compound and array types; boxing of value classes around expressions that cannot be evaluated with operands on the stack; lubs of erased types; generic signatures and inner-class attributes computed after things have moved.
- Code that runs before the super constructor call: lambdas, local definitions, lazy vals and lifted tries in super-constructor arguments and early definitions.
- Members found again by a name built from an owner's name, after a phase that renames owners.
- Specialization against everything: super calls, forwarders, duplicated bodies retyped with symbols of the original, by-name parameters, constructors, private members. Scala 3 has no specialization; in Scala 2 treat any change near it as high risk.
- The Java source parser is a hand-written subset of javac. Joint compilation and compilation against javac's class files must agree on signatures, constants, annotations and sealed hierarchies.
- Constructs meeting. The serious late findings each needed two ordinary features in one expression: a value class and a `try` in a match arm; a `return` in operand position under `finally`; a conditional of two value-class constructions used as a receiver; a constant `if` as a match scrutinee. Test a change with the feature it touches nested inside, and wrapped around, the neighbouring features.
- Trees that are ill-typed but compile to correct code. Run `-Ycheck:all` on new tests; a report there is usually a real stale type or symbol.

## Where library defects cluster

- Arithmetic done in the element type or in `Int` at the extremes: positions, lengths and `knownSize` of ranges, sizes near `Int.MaxValue`, steps of `MinValue`.
- A specialised override (for arrays, strings, indexed sequences, a particular map) that takes a shortcut the generic implementation does not. Compare it with the generic one on the same inputs.
- Bulk operations that read a lazy source (a view, an iterator, a `LazyList`) derived from the receiver while modifying the receiver.
- User functions that re-enter the collection (`updateWith`, `getOrElseUpdate`, a predicate that mutates).
- Iterators and steppers at or past the end, and after `drop` or `slice` with extreme arguments.
- Read-only or snapshot views whose mutators are reachable through a cast.
- Cooperative numeric equality and hashing across `Int`, `Long`, `Double`, `BigInt`, `BigDecimal`.
- Java converters against the `java.util` contracts (null, `Map.Entry.setValue`, `subList`).
- The serialized form, `serialVersionUID`, hash codes and iteration order: all accidental API.

## Checks that found things

- Compare the compiler with itself. Compile the changed sources over a built compiler, then compile the standard library and a few hundred existing tests with the base and the patched compiler and compare class files. A change that claims not to affect ordinary code must give byte-identical output; any difference is either the intended effect or a defect, and the description must say which.
- Run one program several ways that must agree: optimizer on and off, both `-Ydelambdafy` modes, joint against separate compilation, Java sources against javac's class files, the previous release, and the other Scala major version as a second opinion where the semantics agree.
- For a collection, run the same calls on every implementation of the interface and on a small obviously correct model.
- Run on JDK 8 with fatal warnings if the branch's CI does: deprecations, inferred `Any`, unused values and Java 9+ APIs in tests only show there.
- Count, do not time, for cost claims: allocations, calls, steps.

## What made fixes wrong

- The author's test report is not evidence. About one fix in three failed an independent read after passing its own tests, and one in fourteen in the earlier waves after passing the merged suite. Ask for the input that fails before and passes after, then try the neighbour: one more level of nesting, a generic class instead of a concrete one, a `null` or `Nothing` argument, the other branch position, an overloaded method, a trait parent instead of a class, a different instantiation of the same generic.
- Rejecting the program. Given code that compiles and fails at run time, the easy fix is a new error. On a maintenance branch every new error or warning is a compatibility decision for the owner, and a warning is an error under `-Werror`.
- Silent compatibility changes: binary signatures and the set of specialized members, serialized form, hash codes, iteration order, an inferred type, which overload or implicit is chosen, bytecode shape for code that compiled before, exception types. Each must be stated in the description or shown not to happen.
- A description that no longer matches the diff after a rework.
- A test that passes for the wrong reason, or that CI skips (a `jvm` directive, a checker that only warns).

## Scala 3

Early initializers, specialization, existential types and reflection-based structural calls account for a large share of the Scala 2 findings and are gone or redesigned in Scala 3. The Java boundary, erasure of value classes and type tests, equality and hashing, `try` and `return` under `finally`, string concatenation and most library findings are not: about 35 compiler findings reproduced on Scala 3, and some programs crash both compilers. When reviewing a Scala 3 change, use Scala 2 on the same program as the second opinion, and the reverse.

## What to report

For each change: a verdict (ok, concern or defect), the risk, and for every defect or concern the concrete input and what it does before and after. Say what you ran and what you only reasoned about, and what you could not check.
