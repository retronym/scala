#!/usr/bin/env python3
"""covsum.py <xml> : lines covered/total per scope, and five largest unreached methods (by missed instructions) in the scopes."""
import sys, xml.etree.ElementTree as ET, json
scopes = {"transform": "scala/tools/nsc/transform", "backend_jvm": "scala/tools/nsc/backend/jvm", "typechecker": "scala/tools/nsc/typechecker", "all_compiler": "scala/tools/nsc"}
root = ET.parse(sys.argv[1]).getroot()
res = {}; miss = []
for pkg in root.iter("package"):
    pn = pkg.get("name")
    for k, p in scopes.items():
        if pn.startswith(p):
            for c in pkg.findall("counter"):
                if c.get("type") == "LINE" and k != "all_compiler":
                    a = res.setdefault(k, [0, 0]); a[0] += int(c.get("covered")); a[1] += int(c.get("covered")) + int(c.get("missed"))
    if pn.startswith("scala/tools/nsc/transform") or pn.startswith("scala/tools/nsc/backend/jvm"):
        for cls in pkg.findall("class"):
            for m in cls.findall("method"):
                for c in m.findall("counter"):
                    if c.get("type") == "INSTRUCTION": miss.append((int(c.get("missed")), cls.get("name").rsplit("/", 1)[-1] + "." + m.get("name")))
miss.sort(reverse=True)
print(json.dumps({"coverage": {k: {"covered": v[0], "total": v[1]} for k, v in res.items()}, "largest_unreached": [n for _, n in miss[:5]]}))
