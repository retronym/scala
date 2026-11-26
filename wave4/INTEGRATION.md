# Wave four integration build

Status at 2026-11-26 23:17 AEST: **built and smoke-tested.** All seventeen wave-four fix PRs are in; the build under the path below matches the branch head. No partest or JUnit run on this build yet.

- Compiler and runner: `/Users/jzaugg-virtus/code/.worktrees/scala/w4-int/build/pack/bin/scalac` and `/Users/jzaugg-virtus/code/.worktrees/scala/w4-int/build/pack/bin/scala` (built with `dist/mkPack` on JDK 21.0.12 Temurin, no optimizer flags, the same way as the wave-three build; `scalac -version` reports `2.13.19-20261126-131344-7e15d53`)
- Branch: `integration/w4-draft` in `/Users/jzaugg-virtus/code/.worktrees/scala/w4-int`, local only, head `7e15d53368`
- Base: `integration/w3-draft` at `dd21445a93` (the wave-three integration build, see `wave3/INTEGRATION.md` in the bug-hunting-second-wave worktree), so everything from waves one to three is in, including its three known partest failures
- Built by merging each wave-four fix PR head into the base with its own merge commit, in the order of the table. The build started from APFS clones of `w3-int`'s build outputs; it compiled without the `CyclicReference` problem.

Re-run a finding against it, from the finding directory:

```
SCALAC=/Users/jzaugg-virtus/code/.worktrees/scala/w4-int/build/pack/bin/scalac SCALA=/Users/jzaugg-virtus/code/.worktrees/scala/w4-int/build/pack/bin/scala bash cmd.sh
```

## Merged

| PR | Finding | Head merged | Title |
|---|---|---|---|
| retronym/scala#747 | GC1, GC2 | `f6563d5abb` | Fix knownSize of wide Range and NumericRange iterators |
| retronym/scala#748 | GC3 | `6efa557745` | Keep an exhausted Range iterator empty after drop |
| retronym/scala#755 | GC5 | `6f3bbcd104` | Count steps towards zero from a fractional start in NumericRange.count |
| retronym/scala#754 | GC4 | `e505773516` | Return -1 from Range.indexOf for a position beyond Int indices |
| retronym/scala#753 | GJ1 | `b241e118db` | Test the sealed flag, not the token number, in the Java parser |
| retronym/scala#751 | GJ2 | `d1a25a8a49` | Type a varargs record component's accessor as an array |
| retronym/scala#752 | GJ4 | `320dc981d5` | Accept a lone comma in a Java enum body |
| retronym/scala#756 | GF2 | `5df1f5051a` | Reject a non-constant annotation argument of literal type |
| retronym/scala#750 (draft) | GF1 | `73152c2f02` | Keep the type of an if reduced by its constant condition |
| retronym/scala#760 | GC7 (flatMapInPlace) | `52609571e2` | Read flatMapInPlace results that may view the buffer before clearing it |
| retronym/scala#758 (draft) | GC7, GA1 | `32900eb7cf` | Read a source that may view a buffer before changing the buffer |
| retronym/scala#759 (draft) | GC7, GA1 | `74bdd1ad98` | Read a view or iterator of an UnrolledBuffer before changing it |
| retronym/scala#762 | GE1 | `d297eff207` | Don't specialize a super accessor's method type parameters |
| retronym/scala#763 (draft) | GE2 | `eea8616593` | Call the specialized variant from a super call with type arguments |
| retronym/scala#764 | GE3, GE8 | `2d9107e8cf` | Don't count a by-name parameter's type variable in specialization |
| retronym/scala#766 | GE5 | `cc5ca8940c` | Widen a local lazy val's singleton type when duplicating a body |
| retronym/scala#768 | GE7 | `b998546c65` | Keep locals of copied constructor statements local to the constructor |

The drafts were merged as they stood at the time above; a later push to them is not in the build.

## Left out

Test-only and design PRs hold failing tests and no fix, so their findings still reproduce: retronym/scala#749 (GC6, design), #757 (GJ3, test only), #761 (GC7 sets and maps, design), #765 (GE4, test only), #767 (GE6, design).

No fix PR was left out: every merge was clean and the build passed.

## Merges

None conflicted. Two were expected to and did not:

- retronym/scala#754 is stacked on #555 (LI1), whose earlier form the base has. Git merged it without a conflict; the result in `Range.indexOf`/`lastIndexOf` is the union (Long arithmetic from #555, a position beyond `Int.MaxValue` gives -1 from #754, through a shared `posOf`).
- retronym/scala#759 is stacked on #538 (LM3), which the base has together with wave-one #321. It merged cleanly; `UnrolledBuffer.unaliased` now copies any source that may read the buffer, not only the buffer itself, and the #321 change to `patchInPlace` is untouched.

retronym/scala#758, #759 and #760 each carry their own copy of the same "may this source read the buffer" classification (`ArrayDeque`, `IndexedBuffer.patchInPlace`, `Shrinkable`, `UnrolledBuffer`, `IndexedBuffer.flatMapInPlace`). They edit different places, so they compose; the duplication is in the PRs, not introduced here.

## Findings on the build

Each finding's `cmd.sh` run from its directory against the build (all exited 0). "Matches" is byte comparison with the file; where `expected.txt` has prose around the output, the output was compared by hand.

| Finding | Fix | Result |
|---|---|---|
| GA1 | #758, #759 | Fixed for `ArrayDeque`, `UnrolledBuffer`, `ListBuffer`. **Not fixed:** `ArrayBuffer(-2).addAll(self.view.reverse)` and the `self.view` control still throw `ConcurrentModificationException`, as on the wave-three build; no wave-four PR touches `ArrayBuffer.addAll` |
| GC1 | #747 | Fixed (differs from `expected.txt` only in the padding of the first line) |
| GC2 | #747 | Fixed in the PR's chosen direction: `knownSize` is now -1 (unknown) where it was wrong; `expected.txt` has the exact counts. Elements and sizes are right |
| GC3 | #748 | Matches `expected.txt` |
| GC4 | #754 | Fixed (all five lines as expected) |
| GC5 | #755 | Matches `expected.txt` |
| GC6 | #749, design | Still reproduces (matches `actual.txt`) |
| GC7 | #758, #759, #760; #761 design | `flatMapInPlace` line fixed. **Not fixed:** the `HashSet`, `TreeSet`, `LinkedHashSet`, `HashMap`, `TreeMap`, `LinkedHashMap`, `LongMap` lines, which are the design part (GC7d, #761) |
| GE1 | #762 | Matches `expected.txt` |
| GE2 | #763 | Matches `expected.txt` |
| GE3 | #764 | Matches `expected.txt` |
| GE4 | #765, test only | Still reproduces (matches `actual.txt`) |
| GE5 | #766 | Matches `expected.txt` |
| GE6 | #767, design | Still reproduces (matches `actual.txt`) |
| GE7 | #768 | Matches `expected.txt` |
| GE8 | #764 | Matches `expected.txt` |
| GF1 | #750 | Matches `expected.txt` |
| GF2 | #756 | Matches `expected.txt` |
| GJ1 | #753 | Fixed: both compile orders print the exhaustivity warning and no "unreachable code" warning |
| GJ2 | #751 | Fixed: both orders compile and print 2 |
| GJ3 | #757, test only | Still reproduces (matches `actual.txt`) |
| GJ4 | #752 | Fixed: both orders compile and print 1 |

## Not run

Partest, JUnit, scalacheck, TastyTestJUnit, MiMa, JDK 8, a build with the optimizer and fatal warnings. The PRs' own tests have not been run on this composition.
