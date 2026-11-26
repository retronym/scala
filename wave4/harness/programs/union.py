"""Union line coverage of several JaCoCo XML reports, per scope: python3 union.py a.xml b.xml ... (last-named reports listed as 'new' lines)."""
import sys, xml.etree.ElementTree as ET
SCOPES = ["scala/tools/nsc/transform", "scala/tools/nsc/transform/patmat", "scala/tools/nsc/backend/jvm"]
def lines(path):
    cov, tot = set(), set()
    for pkg in ET.parse(path).getroot().iter('package'):
        n = pkg.get('name')
        if n not in SCOPES: continue
        for sf in pkg.findall('sourcefile'):
            for l in sf.findall('line'):
                k = (n, sf.get('name'), l.get('nr')); tot.add(k)
                if int(l.get('ci')) > 0: cov.add(k)
    return cov, tot
split = sys.argv.index('--') if '--' in sys.argv else len(sys.argv)
old, new = sys.argv[1:split], sys.argv[split + 1:]
oc, tot = set(), set()
for p in old: c, t = lines(p); oc |= c; tot |= t
nc = set()
for p in new: c, t = lines(p); nc |= c; tot |= t
for s in SCOPES:
    f = lambda xs: len([k for k in xs if k[0] == s])
    print(f"{s}: before {f(oc)}, after {f(oc | nc)}, newly reached {f(nc - oc)}, total {f(tot)}")
from collections import Counter
print("newly reached by file:", Counter(k[1] for k in nc - oc).most_common(8))
