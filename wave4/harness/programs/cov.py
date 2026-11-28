import sys, json, xml.etree.ElementTree as ET
SCOPES = ["scala/tools/nsc/transform", "scala/tools/nsc/transform/patmat", "scala/tools/nsc/backend/jvm", "scala/tools/nsc/backend/jvm/opt", "scala/tools/nsc/typechecker"]
root = ET.parse(sys.argv[1]).getroot()
cov, meths = {}, []
for pkg in root.iter('package'):
    name = pkg.get('name')
    if name in SCOPES:
        for c in pkg.findall('counter'):
            if c.get('type') == 'LINE':
                cov[name] = [int(c.get('covered')), int(c.get('covered')) + int(c.get('missed'))]
    if name in SCOPES[:3]:
        for cls in pkg.findall('class'):
            for m in cls.findall('method'):
                for c in m.findall('counter'):
                    if c.get('type') == 'LINE' and int(c.get('missed')) > 0:
                        meths.append((int(c.get('missed')), int(c.get('covered')), cls.get('name').rsplit('/', 1)[-1] + '.' + m.get('name')))
meths.sort(reverse=True)
print(json.dumps({"coverage": cov, "largest_unreached": [f"{n} ({mi} of {mi+co} lines missed)" for mi, co, n in meths[:25]]}, indent=1))
