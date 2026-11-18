# Turns Layout.scala's report (out/layout.txt) into out/data.json for gen.py.
import re, json
t = open('out/layout.txt').read()
out = {}
for blk in t.split('#### objects ')[0].split('#### corpus ')[1:]:
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
# Symbol, Type, Name and Scope objects: Layout.scala's "#### objects" sections.
objs = {}
for blk in t.split('#### objects ')[1:]:
    name = blk.split()[0]
    o = dict(heapMB=int(re.search(r'live heap after typer: (\d+) MB', blk).group(1)), rows=[], owned=[], tot={}, layout={}, use={})
    for l in blk.splitlines():
        f = l.split('\t')
        if f[0] == 'SCN': o['scn'] = f[1:]
        elif f[0] == 'R':
            k = len(o['scn'])
            sz = list(map(int, f[7:]))
            o['rows'].append(dict(fam=f[1], cls=f[2], n=int(f[3]), b12=int(f[4]), b8=int(f[5]), walkN=int(f[6]), s12=sz[:k], s8=sz[k:]))
        elif f[0] == 'O':
            k = len(o['scn'])
            b = list(map(int, f[5:]))
            o['owned'].append(dict(fam=f[1], name=f[2], arrays=int(f[3]), elems=int(f[4]), b12=b[:k], b8=b[k:]))
        elif f[0] == 'TOT': o['tot'].setdefault(f[1], {})[f[2]] = list(map(int, f[3:]))
        elif f[0] == 'L':
            o['layout'].setdefault(f[1], {})[f[2]] = [(int(a), int(b), c) for a, b, c in (x.split(':', 2) for x in f[3].split(';'))]
        elif f[0] == 'U':
            o['use'].setdefault(f[1], []).append(dict(field=f[2], type=f[3], nonDefault=int(f[4]), n=int(f[5]), sentinels=f[6]))
        elif l.startswith('# nameTableSize'): o['nameTableSize'] = int(f[1])
    objs[name] = o
out['objects'] = objs
json.dump(out, open('out/data.json', 'w'))
