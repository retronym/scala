# Turns Layout.scala's report (out/layout.txt) into out/data.json for gen.py.
import re, json
t = open('out/layout.txt').read()
out = {}
for blk in t.split('#### corpus ')[1:]:
    name = blk.split()[0]
    heapmb = int(re.search(r'live heap after typer: (\d+) MB', blk).group(1))
    lines = blk.splitlines()
    hdr = lines[1].split('\t')
    rows = []
    for l in lines[2:]:
        if l.startswith('TOTAL'): break
        f = l.split('\t')
        rows.append(dict(cls=f[0], heapN=int(f[1]), walkN=int(f[2]), bytes=int(f[3]), sizes=list(map(int, f[4:-1])), sym=f[-1] == 'true'))
    tot = {}
    for m in re.finditer(r'  (\S+) heap-weighted: (.*)', blk):
        tot[m.group(1)] = {k: int(v) for k, v in re.findall(r'(\S+?)=(\d+) \(', m.group(2))}
    pos = [(m.group(1).split('.')[-1], int(m.group(2)), int(m.group(3))) for m in re.finditer(r'  scala\.\S+\.(\w+)\s+n=\s*(\d+) bytes=\s*(\d+)', blk)]
    m = re.search(r'TOTAL heap trees: n=(\d+) bytes=(\d+).*walk trees n=(\d+)', blk)
    out[name] = dict(heapMB=heapmb, cols=hdr[4:-1], rows=rows, tot=tot, pos=pos, treeN=int(m.group(1)), treeB=int(m.group(2)), walkN=int(m.group(3)))
json.dump(out, open('out/data.json', 'w'))
