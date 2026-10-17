import json, sys, subprocess
d = sys.argv[1]
subprocess.run([sys.executable, f"{d}/fit.py", f"{d}/results/all.csv", f"{d}/results/fit.json"], check=True)
data = json.load(open(f"{d}/results/fit.json"))
meta = {
 "hardSecs": 120,
 "summary": [
  '<b>Scala 2.13 base is cubic</b> on every shape over a sealed type: k ≈ 2.5 to 2.9 at the largest sizes. Where measured, a 1024-case match takes 37 to 56 s in the pattern matcher (<span class="num">43.9 s</span> for constructor patterns, <span class="num">56 s</span> for sequence heads).',
  '<b>#142 makes it about N<sup>1.4–1.7</sup></b>, with small constants: a 1024-case match takes 0.28 to 0.68 s, about 120 to 160× faster than 2.13.x (constructor patterns <span class="num">43.9 s → 0.33 s</span>). Warnings and bytecode are unchanged.',
  '<b>Scala 3 is quadratic</b> (k ≈ 2) on most shapes. At N=1024 it is close to #142 on constructor patterns (<span class="num">256 ms</span> vs <span class="num">325 ms</span>) and slower on the pair (<span class="num">2.3 s</span> vs <span class="num">0.66 s</span>). retronym/scala3#23 lowers its constant by 1.3 to 9× at N=2048 but leaves the exponent near 2, except for constructor patterns.',
  '<b>A field of the sealed type only matters for Scala 3.</b> With <code>C_i(a: Int, b: T)</code>, as in the #23 benchmark, Scala 3 base takes <span class="num">4.5 s</span> on constructor patterns at N=2048, against <span class="num">0.9 s</span> with <code>b: Int</code>. Scala 2 shows no difference.',
  '<b>Guards:</b> Scala 3 is linear on guarded cases (<span class="num">72 ms</span> at N=1024), as it never decomposes the sealed type for them. #142 is still superlinear there (<span class="num">276 ms</span>).',
  '<b>Int literals:</b> Scala 2 skips their analysis (<span class="num">44 ms</span> for 4096 literals). Scala 3 base is quadratic (<span class="num">2.7 s</span>), and ConstantSkip in #23 brings it to <span class="num">115 ms</span>.',
  '<b>Ordinary code is not slower:</b> on cats-core and scalaz-core the <code>patmat</code> phase of #142 is about 10 to 18% faster than 2.13.x and allocates about 7% less. No compile reached the 120 s timeout; above about 2000 cases Scala 2 gives up the analysis (“Cannot check match”), so its N=2048 points are marked × and left out of the fits.',
 ],
 "shapes": [
  {"id": "ctor",  "label": "Constructor patterns", "code": "t match { case C_i(_, _) => i }"},
  {"id": "typ",   "label": "Type patterns",        "code": "t match { case _: C_i => i }"},
  {"id": "tup",   "label": "Pair of constructors", "code": "(a, b) match { case (C_i(_, _), C_i(_, _)) => i; case _ => }"},
  {"id": "opt",   "label": "Option of constructor","code": "o match { case Some(C_i(_, _)) => i; case None => }"},
  {"id": "guard", "label": "Guarded constructors", "code": "t match { case C_i(_, _) if g => i; case _ => }"},
  {"id": "seq",   "label": "Sequence head",        "code": "s match { case Vector() =>; case C_i(_, _) +: _ => i; case _ => }"},
  {"id": "ints",  "label": "Int literals (2N)",    "code": "x match { case 0 => .. case 2N-1 =>; case _ => }"},
  {"id": "ctorR", "label": "Constructor patterns, b: T", "code": "C_i(a: Int, b: T); t match { case C_i(_, _) => i }"},
  {"id": "tupR",  "label": "Pair of constructors, b: T", "code": "C_i(a: Int, b: T); (a, b) match { case (C_i(_, _), C_i(_, _)) => i; case _ => }"},
  {"id": "optR",  "label": "Option of constructor, b: T", "code": "C_i(a: Int, b: T); o match { case Some(C_i(_, _)) => i; case None => }"},
 ],
 "variants": {
  "scala2-stack": '<a href="https://github.com/retronym/scala/pull/142">retronym/scala#142</a> (draft) at <a href="https://github.com/retronym/scala/commit/58d365caee">58d365caee</a>: 12 commits on 2.13.x (linear unit propagation and incremental CNF; domain lookups, linked-hash-set domains and implications indexed by class; a per-match type cache and restricted substitution; indexes for hash-consed trees, constants and CSE; skipping exhaustivity with an irrefutable case). Measured with a build of the identical tree f906eaab25.',
  "scala2-base": 'scala/scala <code>2.13.x</code> at <a href="https://github.com/scala/scala/commit/3ab962e19048eddeb4958207d4098c5a7adcd5b7">3ab962e1</a> <span class="sha">3ab962e19048eddeb4958207d4098c5a7adcd5b7</span> (patmat sources identical to the 2.13.x tip <a href="https://github.com/scala/scala/commit/2a48eb879e">2a48eb87</a>). Open report: <a href="https://github.com/scala/bug/issues/12873">scala/bug#12873</a>.',
  "scala3-base": 'retronym/scala3 at <a href="https://github.com/retronym/scala3/commit/c2a635e80356b72f7b0942ef2edced5a030a0eb6">c2a635e8</a> <span class="sha">c2a635e80356b72f7b0942ef2edced5a030a0eb6</span> with no <code>-Ddotty.patmat*</code> flags, which behaves as its base <a href="https://github.com/retronym/scala3/commit/672837f83ee71101e9996effcf070ad3b0b05991">672837f8</a> (<code>patmat-space-bench-base</code>).',
  "scala3-patch": 'Same build with the <code>best</code> flags of <a href="https://github.com/retronym/scala3/pull/23">retronym/scala3#23</a> (decompose cache per run, signature cache, PrevUnion, HashMemo, SameUnapplySym, ConstantSkip, ClassFastFail).',
 },
 "method": [
  "Each source is one <code>object O</code> with one match over a <code>sealed trait T</code> with N <code>final case class C_i(a: Int, b: Int)</code> children (Int literals: no sealed type, 2N cases). The same file is compiled by both compilers. The <code>b: T</code> shapes give each child a field of the sealed type, as in the <code>sealed&lt;N&gt;</code> corpus of retronym/scala3#23's PatmatBenchmark; they reproduce the Scala 3 findings exactly and run after the other shapes.",
  "Scala 2: phase time from <code>-Vstatistics:patmat</code> (the <code>patmat</code> phase: translation and analysis). Scala 3: <code>runNs</code> from <code>-Yprofile-enabled</code> for the MegaPhase containing <code>patternMatcher</code> (also protectedAccessors, elimByName and others, all linear here).",
  "One JVM per compiler (JDK 21, -Xmx4g), 40 s of warm-up compiles at N=64, then for each N the minimum over 2 to 9 in-process compiles stopping after that phase (a single compile once one takes over 20 s).",
  "Scala 2 abandons the analysis of a match whose formula exceeds its budget (<code>-Ypatmat-exhaust-depth</code> × 100 = 2000 conjuncts), with a \u201cCannot check match\u201d warning; for the sealed shapes that happens at N=2048 for every Scala 2 variant. Those points are drawn as × and excluded from the fitted exponents.",
  "A shape stops growing N once a compile takes over 20 s, or when the growth over the last doubling predicts more than 90 s for the next size. A compile still running after 120 s is recorded as a timeout, and the remaining shapes continue in a fresh JVM. (The Scala 2.13 base sweep ran before the prediction and the timeout were added.)",
  "Scala 2.13 base: 2.13.x at 3ab962e190 (same patmat sources as the tip). #142: retronym/scala#142 at 58d365caee (measured with a build of the identical tree f906eaab25). Scala 3: retronym/scala3 c2a635e803 (non-bootstrapped build); base runs with no flags, #23 with the <code>best</code> flags of micro-patmat.json (DecomposeCache=run, SignatureCache, PrevUnion, HashMemo, SameUnapplySym, ConstantSkip, ClassFastFail).",
  "The machine was shared (load average around 4), so expect some noise, mostly at small N.",
 ],
}
import re
LINKS = [
  (r'retronym/scala3#23(?!</a>)', '<a href="https://github.com/retronym/scala3/pull/23">retronym/scala3#23</a>'),
  (r'retronym/scala#142(?!</a>)', '<a href="https://github.com/retronym/scala/pull/142">retronym/scala#142</a>'),
  (r'(?<![\w/">])#142\b(?!</a>)|(?<=<b>)#142\b', '<a href="https://github.com/retronym/scala/pull/142">#142</a>'),
  (r'(?<![\w/">])#143\b(?!</a>)|(?<=<b>)#143\b', '<a href="https://github.com/retronym/scala/pull/143">#143</a>'),
  (r'(?<![\w/">])#23\b(?!</a>)|(?<=<b>)#23\b', '<a href="https://github.com/retronym/scala3/pull/23">#23</a>'),
  (r'\b3ab962e190\b', '<a href="https://github.com/scala/scala/commit/3ab962e19048eddeb4958207d4098c5a7adcd5b7">3ab962e190</a>'),
  (r'\b58d365caee\b(?!</a>)', '<a href="https://github.com/retronym/scala/commit/58d365caee">58d365caee</a>'),
  (r'\bc2a635e803\b', '<a href="https://github.com/retronym/scala3/commit/c2a635e80356b72f7b0942ef2edced5a030a0eb6">c2a635e803</a>'),
  (r'\bmicro-patmat\.json\b', '<a href="https://github.com/retronym/scala3/blob/c2a635e80356b72f7b0942ef2edced5a030a0eb6/bench-lab/ab/micro-patmat.json">micro-patmat.json</a>'),
  (r'\bscala/bug#12873\b(?!</a>)', '<a href="https://github.com/scala/bug/issues/12873">scala/bug#12873</a>'),
]
def linkify(h):
    for pat, rep in LINKS: h = re.sub(pat, rep, h)
    return h
meta["summary"] = [linkify(x) for x in meta["summary"]]
meta["method"] = [linkify(x) for x in meta["method"]]
steps = [{"id": "scala2-base", "label": "2.13.x"}]
for i, line in enumerate(open(f"{d}/steps.tsv").read().splitlines(), 1):
    sha, subj = line.split("\t", 1)
    steps.append({"id": "scala2-stack" if i == 12 else f"c{i:02d}", "label": subj.replace("`", "")})
meta["evo"] = {"shapes": ["ctor", "typ", "tup", "opt", "guard", "seq"], "steps": steps}
html = open(f"{d}/page.template.html").read().replace("/*DATA*/", json.dumps(data)).replace("/*META*/", json.dumps(meta))
open(f"{d}/patmat-scaling.html", "w").write(html)
