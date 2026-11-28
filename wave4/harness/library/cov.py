#!/usr/bin/env python3
# usage: cov.py <jacoco.xml> [more.xml ...]: lines covered per scope file (one column per report), then the largest unreached methods of the last report
import sys, xml.etree.ElementTree as ET
SCOPE = ["scala/collection/mutable/LongMap.scala", "scala/collection/immutable/LongMap.scala", "scala/collection/immutable/IntMap.scala",
  "scala/collection/mutable/AnyRefMap.scala", "scala/collection/mutable/CollisionProofHashMap.scala", "scala/collection/mutable/OpenHashMap.scala",
  "scala/collection/concurrent/TrieMap.scala", "scala/collection/immutable/TreeSeqMap.scala", "scala/collection/immutable/VectorMap.scala",
  "scala/collection/immutable/ListMap.scala", "scala/collection/mutable/PriorityQueue.scala", "scala/collection/mutable/Stack.scala",
  "scala/collection/mutable/Queue.scala", "scala/collection/immutable/Queue.scala", "scala/collection/convert/JavaCollectionWrappers.scala",
  "scala/collection/convert/AsJavaConverters.scala", "scala/collection/convert/AsScalaConverters.scala", "scala/collection/convert/StreamExtensions.scala",
  "scala/jdk/Accumulator.scala", "scala/jdk/AnyAccumulator.scala", "scala/jdk/IntAccumulator.scala", "scala/jdk/LongAccumulator.scala", "scala/jdk/DoubleAccumulator.scala",
  "scala/collection/Stepper.scala", "scala/collection/Iterator.scala", "scala/collection/immutable/LazyList.scala", "scala/util/Sorting.scala",
  "scala/math/BigInt.scala", "scala/math/BigDecimal.scala", "scala/util/Random.scala", "scala/collection/StringOps.scala"]
PREFIXES = ("scala/collection/convert/impl/",)
def load(f):
    d = {}; meth = []
    root = ET.parse(f).getroot()
    for pkg in root.iter("package"):
        for sf in pkg.findall("sourcefile"):
            for c in sf.findall("counter"):
                if c.get("type") == "LINE": d[pkg.get("name") + "/" + sf.get("name")] = (int(c.get("covered")), int(c.get("covered")) + int(c.get("missed")))
        for cl in pkg.findall("class"):
            src = pkg.get("name") + "/" + (cl.get("sourcefilename") or "")
            for m in cl.findall("method"):
                for c in m.findall("counter"):
                    if c.get("type") == "LINE" and int(c.get("covered")) == 0: meth.append((int(c.get("missed")), src, cl.get("name").split("/")[-1] + "." + m.get("name")))
    return d, meth
cols = [load(f) for f in sys.argv[1:]]
inscope = lambda f: f in SCOPE or f.startswith(PREFIXES)
files = sorted({f for d, _ in cols for f in d if inscope(f)})
print("| file | " + " | ".join(a.split("/")[-1].replace(".xml", "") for a in sys.argv[1:]) + " | total |")
print("|---|" + "---|" * (len(cols) + 1))
tot = [0] * len(cols); T = 0
for f in files:
    print(f"| {f.replace('scala/collection/', '')} | " + " | ".join(str(d.get(f, (0, 0))[0]) for d, _ in cols) + f" | {cols[0][0][f][1]} |")
    for i, (d, _) in enumerate(cols): tot[i] += d.get(f, (0, 0))[0]
    T += cols[0][0][f][1]
print("| total | " + " | ".join(map(str, tot)) + f" | {T} |")
print("\nlargest wholly unreached methods in scope (last report):")
for n, src, m in sorted([x for x in cols[-1][1] if inscope(x[1])], reverse=True)[:25]: print(f"  {n:4d} {src} {m}")
