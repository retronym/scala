#!/usr/bin/env python3
"""Line coverage per scope of a JaCoCo XML, and the largest unreached methods. usage: cov.py <cov.xml> [--json]"""
import json, sys
import xml.etree.ElementTree as ET

SCOPES = {"JavaParsers": ("scala/tools/nsc/javac", "JavaParsers.scala"), "JavaScanners": ("scala/tools/nsc/javac", "JavaScanners.scala"),
          "JavaConstantFolder": ("scala/tools/nsc/javac", "JavaConstantFolder.scala"), "Namers": ("scala/tools/nsc/typechecker", "Namers.scala"),
          "ClassfileParser": ("scala/tools/nsc/symtab/classfile", "ClassfileParser.scala")}

def main(path, as_json):
    root = ET.parse(path).getroot()
    cov, unreached = {}, []
    for pkg in root.iter("package"):
        for name, (pn, fn) in SCOPES.items():
            if pkg.get("name") != pn: continue
            for sf in pkg.findall("sourcefile"):
                if sf.get("name") != fn: continue
                for c in sf.findall("counter"):
                    if c.get("type") == "LINE": cov[name] = [int(c.get("covered")), int(c.get("covered")) + int(c.get("missed"))]
            for cls in pkg.findall("class"):
                if cls.get("sourcefilename") != fn: continue
                for m in cls.findall("method"):
                    for c in m.findall("counter"):
                        if c.get("type") == "INSTRUCTION" and int(c.get("covered")) == 0:
                            nm = cls.get("name").rsplit("/", 1)[-1] + "." + m.get("name")
                            if name == "Namers" and "ava" not in nm: continue
                            unreached.append((int(c.get("missed")), name, nm))
    unreached.sort(reverse=True)
    if as_json: print(json.dumps({"coverage": cov, "largest_unreached": [u[2] for u in unreached[:5]]}))
    else:
        for k, v in cov.items(): print("%-20s %d/%d" % (k, v[0], v[1]))
        for u in unreached[:25]: print("  unreached %5d %s %s" % u)

def lines(path, scope, src):
    """Print the unreached source lines of one scope, with their text from the scala source tree src."""
    pn, fn = SCOPES[scope]
    for pkg in ET.parse(path).getroot().iter("package"):
        if pkg.get("name") != pn: continue
        for sf in pkg.findall("sourcefile"):
            if sf.get("name") != fn: continue
            text = open("%s/src/%s/%s/%s" % (src, "reflect" if False else "compiler", pn, fn)).read().split("\n")
            for l in sf.findall("line"):
                if int(l.get("ci")) == 0 and int(l.get("mi")) > 0:
                    n = int(l.get("nr")); print("%5d %s" % (n, text[n - 1].rstrip()))

if __name__ == "__main__":
    if len(sys.argv) > 3 and sys.argv[2] == "--lines": lines(sys.argv[1], sys.argv[3], sys.argv[4])
    else: main(sys.argv[1], "--json" in sys.argv)
