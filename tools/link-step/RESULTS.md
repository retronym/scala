# Link step spike: ancestor-derived members from pickles

The question is whether the parts of a Scala 2 class file that depend on the class's ancestors can be regenerated from pickles alone, without the class's own typed trees. For classes that have a pickle, they can, with two exceptions that both come from specialization and one naming detail. Local and anonymous classes have no pickle and are the largest gap.

## Method

For every top-level class and object with a pickle, the tool unpickles the symbol from the class path and builds a stub tree: the class, a primary constructor whose only statement is `super.<init>(zeros)`, and the same stub for each member class and object. None of the class's own methods, fields or initializers are in the tree. The stub runs through the compiler's own phases from uncurry to jvm, into memory. Everything the backend emits beyond the stub is derived from symbols, that is from the class's pickle and its ancestors' pickles.

The tool then compares each regenerated class file with the one the compiler produced, using ASM:

- Level 1: name, descriptor and access flags of every regenerated member; the sequence of `$init$` calls in the constructor; and, in the other direction, every member of the real class that looks ancestor-derived (bridge, trait setter, trait super accessor, mixin-forwarder body, specialized variant) but was not regenerated.
- Level 2: the instructions of every regenerated method, with labels, line numbers and frames dropped.

The class's own fields are left out on both sides: the backend emits them from the class's decls even without a tree. Static forwarders are in, since they are generated from the object's whole member list, inherited members included.

Corpora: reflect, compiler, interactive, scaladoc and scalap from `build/quick` of this branch, and cats (commit 813f1bc8e3) kernel, core and free, compiled with the quick compiler and cats's own options (`-Xsource:3 -release 8`, kind-projector, better-monadic-for). Regenerating a corpus takes 0.5 to 3.5 seconds.

## Results

"Derived" is the number of regenerated classes with at least one ancestor-derived member or `$init$` call. "Fresh" is level 1 exact when the compilation unit's fresh-name counter is ignored (see below). "Local/anon" are class files with no pickle of their own, and "with signs" those of them that have a `$init$` call, a bridge, a trait setter, a trait super accessor or a mixin forwarder.

| corpus | class files | regenerated | derived | level 1 exact | level 2 exact | fresh | local/anon | with signs |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| reflect | 1599 | 1432 | 839 | 838 | 838 | 838 | 167 | 122 |
| compiler | 2511 | 2124 | 1327 | 1324 | 1324 | 1326 | 387 | 266 |
| interactive | 169 | 143 | 87 | 76 | 76 | 76 | 26 | 14 |
| scaladoc | 403 | 318 | 214 | 214 | 214 | 214 | 85 | 73 |
| scalap | 201 | 192 | 161 | 161 | 161 | 161 | 9 | 8 |
| cats-kernel | 1138 | 570 | 219 | 191 | 191 | 191 | 568 | 568 |
| cats-core | 2425 | 1697 | 547 | 547 | 547 | 547 | 728 | 647 |
| cats-free | 125 | 81 | 11 | 11 | 11 | 11 | 44 | 37 |
| total | 8571 | 6557 | 3405 | 3362 | 3362 | 3364 | 2014 | 1735 |

The local/anon column also holds 32 other class files: Java-defined classes, classes nested inside a local class, and `CallGraph` with its 16 nested classes (see below).

Members regenerated, over all corpora:

| kind | members |
|---|---:|
| mixin forwarder | 10929 |
| static forwarder (companion or mirror class) | 8072 |
| specialized forwarder (to an ancestor's `m$mcI$sp`) | 8034 |
| bridge | 3454 |
| inherited object accessor | 1797 |
| trait setter (`T$_setter_$x_$eq`) | 1644 |
| trait field | 1291 |
| trait outer accessor (`T$$$outer`) | 1228 |
| trait field accessor | 1035 |
| inherited object field (`O$module`) | 688 |
| trait lazy val (accessor and `$lzycompute`) | 512 |
| lazy val bitmap | 188 |
| trait super accessor | 107 |
| trait field setter | 92 |

Wherever level 1 matched, level 2 matched too: no regenerated method with the right name, descriptor and flags had a different body.

Some of the steps that looked likely to need C's own trees do not:

- `$init$` calls. 1009 regenerated classes call `$init$`, and every sequence matches the real constructor. Erasure adds them from `clazz.mixinClasses`.
- Trait lazy vals. The class's `x$lzycompute` calls `T.x$(this)`, so the initializer stays in the trait. The bitmap field is shared with the class's own lazy vals, and its width and bit positions still come out right, because the allocation counts the pickled lazy decls, not trees. A sample class with two trait lazy vals and eight of its own matches exactly.
- Trait super accessors, outer accessors of inner traits, and accessors of objects inherited from traits all match.

Running one class per `Global` gives the same numbers as 400 classes per `Global` for reflect and cats-kernel. So the results do not depend on whether an ancestor in the same module is compiled in the same run or read from the class path.

## Mismatches

### Specialized variants of the class's own methods (175 classes)

This kind needs C's trees. There are three cases.

- A class overrides a method that an ancestor specializes (26 classes: 13 in cats-kernel, `EmptyAction`, `Flags.MapFlags` and 11 `CompilerControl` work items in interactive). Example: `scala.tools.nsc.util.EmptyAction extends (() => Unit)`. The real class has `apply$mcV$sp()V`, which holds the body of the class's own `apply`, and `apply()V` becomes `invokevirtual apply$mcV$sp`. The specialize phase moves the class's own body into the specialized method. If the ancestor's specialization changes, both the class's own `apply` and the new method change.
- A trait overrides a method that its parent specializes (36 traits in cats-kernel). Example: `cats.kernel.Band` overrides `repeatedCombineN`, which `Semigroup` specializes. The real trait has `repeatedCombineN$mcI$sp(II)I` and its siblings, each boxing its arguments and calling the generic `repeatedCombineN`. The body depends only on symbols, but the specialize phase creates these methods from the class's own `DefDef`, so the stub does not get them.
- Specialized subclasses and subtraits (113 in cats-kernel, such as `cats.kernel.Eq$mcJ$sp`). The stub produces these classes and their ancestor-derived members match, but their specialized methods are copies of the generic class's trees. `Eq$mcJ$sp` lacks `neqv$mcJ$sp(JJ)Z`, among others.

The same cause makes the one failure in the scala corpora. `scala.tools.nsc.backend.jvm.opt.CallGraph` crashes in constructors with "no constructor in template" for `CallGraph$FLazy$mcI$sp`, because the specialized subclass's template is built from `FLazy`'s trees. The whole top-level unit is lost, so `CallGraph` and its 16 nested classes are not regenerated.

### Fresh-name counter in inherited object accessors (2 classes)

Example: `scala.tools.nsc.classpath.ZipAndJarSourcePathFactory$` regenerates `ZipSettings$lzycompute$1()V`. The real class has `ZipSettings$lzycompute$2()V`, and the accessor `ZipSettings()` calls it. The accessor for an object inherited from a trait contains a local `def O$lzycompute`. Lambdalift lifts it and appends the compilation unit's fresh-name counter, so the number depends on how many local functions come before it in the source file. The member is private, so a link step can rename it to whatever the class already uses, or the compiler could name it deterministically. Ignoring the counter makes both classes exact. The sample corpus (`test/sample.scala`) shows the same mismatch in 7 of 7 such accessors.

### Specialization of class-path ancestors depends on what the run asked for

This one is fixed in the tool, but a link step has to handle it too. `SpecializeTypes.transformInfo` skips classes that are not compiled in the run if their info is first asked for after the specialize phase. In a normal compile, the class's own code asks for most of its ancestors. The stub does not, so without help, specialized members of class-path ancestors and the bridges erasure derives from them go missing. The tool asks for the info of every base class during the specialize phase. Without that, cats-core drops from 547 to 531 exact classes and cats-kernel from 191 to 190. Example: `cats.ContravariantMonoidalMonoid` loses the bridge `reverse$mcJ$sp()Lcats/kernel/Semigroup;`, which calls `reverse$mcJ$sp()Lcats/kernel/Monoid;`, and `cats.data.NonEmptyListOrder` loses 135 forwarders such as `compare$mcZ$sp`. The scala corpora are unaffected. The real compile has the same exposure: whether an ancestor's specialized members exist in C depends on what C's code, or anything else in the run, referenced.

### Local and anonymous classes (2014 class files, 1735 with ancestor-derived members)

These are not mismatches but a gap. A local or anonymous class is not in any pickle, so there is no symbol to unpickle, and this method cannot regenerate it. In cats almost every anonymous instance (`new Semigroup[A] { ... }`) gets mixin forwarders, specialized forwarders and bridges from its parents. To relink them, a link step needs either a pickle for each local class (the class file's `Signature` attribute carries generic parents, but not Scala types such as refinements, and not the members' Scala signatures that bridges are computed from) or the enclosing top-level class's trees.

## Not measured

The comparison covers the members the stub produces. It does not look at the class's own method bodies, so it says nothing about ancestor facts that reach C's own call sites, for example:

- expanded names of trait members that C's code calls (`p$T$$x`)
- `invokestatic T.m$` for `super[T].m`
- calls that specialization redirects to `m$mcI$sp`

Reasoning suggests those need C's trees whenever the ancestor's change renames something C calls; this spike did not check it. Changes to how a referenced type erases, and inlining with `-opt`, are out of scope as planned. Neither the quick build nor the cats build uses `-opt`.

## Running it

`tools/link-step/run --corpus DIR --cp DEPS [--label NAME] [--out DIR] [--batch N] [--force-specialized-ancestors false] [-- scalac options]` compiles the tool against `build/quick` and checks one corpus. It writes `<label>-summary.txt`, `<label>.tsv` (one line per mismatch), `<label>-classes.tsv` and `<label>-members.tsv`. `tools/link-step/corpora.sh OUT_DIR [CATS_BUILD_DIR]` reproduces the tables above. `Relinker` is the regeneration step, kept apart from the comparison so that a later agent can reuse it.
