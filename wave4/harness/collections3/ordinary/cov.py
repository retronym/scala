#!/usr/bin/env python3
# usage: cov.py <jacoco.xml> [more.xml ...] : lines covered / total for the chosen source files, one column per report
import sys, xml.etree.ElementTree as ET
FILES = ["scala/collection/immutable/Vector.scala", "scala/collection/immutable/VectorMap.scala", "scala/collection/ArrayOps.scala", "scala/collection/StringOps.scala", "scala/collection/StringParsers.scala",
         "scala/collection/immutable/HashMap.scala", "scala/collection/immutable/HashSet.scala", "scala/collection/immutable/ChampCommon.scala", "scala/collection/LazyZipOps.scala",
         "scala/collection/immutable/ArraySeq.scala", "scala/collection/mutable/ArraySeq.scala", "scala/collection/mutable/ArrayBuilder.scala", "scala/collection/mutable/StringBuilder.scala",
         "scala/collection/immutable/Range.scala", "scala/collection/immutable/NumericRange.scala"]
cols = []
for f in sys.argv[1:]:
    d = {}
    for pkg in ET.parse(f).getroot().iter("package"):
        for sf in pkg.findall("sourcefile"):
            for c in sf.findall("counter"):
                if c.get("type") == "LINE": d[pkg.get("name") + "/" + sf.get("name")] = (int(c.get("covered")), int(c.get("covered")) + int(c.get("missed")))
    cols.append(d)
print("| file | " + " | ".join(a.split("/")[-1].replace(".xml", "") for a in sys.argv[1:]) + " | total |")
print("|---|" + "---|" * (len(cols) + 1))
for f in FILES:
    if f in cols[0]: print(f"| {f.replace('scala/collection/', '')} | " + " | ".join(str(c.get(f, (0, 0))[0]) for c in cols) + f" | {cols[0][f][1]} |")
