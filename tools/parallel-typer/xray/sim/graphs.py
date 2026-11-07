#!/usr/bin/env python3
# Real module graphs on disk -> graphs.json {module: {lines, deps:[...], macroDeps:[...]}}, for sizes and shape.
# Run from a machine with the checkouts: CODE (default ~/code) holds morganstanley/optimus-cirrus and PEKKO the pekko
# repository, SCALATEST a clone of scalatest. Sources are counted and build definitions read as text; nothing of theirs runs.
import json, os, re, statistics as st, subprocess, sys
OUT = os.path.dirname(os.path.abspath(__file__))
H = os.path.expanduser(os.environ.get('CODE', '~/code'))
PEKKO = os.path.expanduser(os.environ.get('PEKKO', H + '/.integration/repos/pekko'))
SCALATEST = os.path.expanduser(os.environ.get('SCALATEST', '/tmp/parallel-typer-xray/src-scalatest'))
ROOT = os.path.abspath(os.path.join(OUT, '..', '..', '..', '..'))


def lines(d):
    n = 0
    for root, _, fs in os.walk(d):
        for f in fs:
            if f.endswith('.scala') or f.endswith('.template'):
                with open(os.path.join(root, f), errors='ignore') as h: n += sum(1 for _ in h)
    return n


def pekko():
    root = PEKKO
    src = open(root + '/build.sbt').read()
    mods = {}
    for m in re.finditer(r'^lazy val (\w+)\s*(?::[^=]*)?=\s*pekkoModule\("([\w-]+)"\)(.*?)(?=^lazy val |\Z)', src, re.M | re.S):
        val, name, body = m[1], m[2], m[3]
        deps = []
        for d in re.finditer(r'\.dependsOn\((.*?)\)\s*$', body, re.M | re.S):
            for part in re.split(r',\s*(?=\w+\s*(?:%|,|$))', d[1].replace('\n', ' ')):
                p = part.strip()
                if not p: continue
                t = re.match(r'(\w+)(?:\s*%\s*"([^"]*)")?', p)
                if t and (t[2] is None or re.search(r'compile', t[2])): deps.append(t[1])
        mods[val] = dict(name=name, deps=deps)
    out = {}
    for val, m in mods.items():
        n = lines(f"{root}/{m['name']}/src/main")
        if n: out[val] = dict(lines=n, deps=[d for d in m['deps'] if d in mods], macroDeps=[])
    for v in out.values(): v['deps'] = [d for d in v['deps'] if d in out]
    return out


def optimus():
    root = H + '/morganstanley/optimus-cirrus'
    obts = subprocess.run(['find', root, '-name', '*.obt'], capture_output=True, text=True).stdout.split()
    mods = {}
    for f in obts:
        rel = os.path.relpath(f, root)
        # optimus/platform/projects/core/core.obt -> optimus.platform.core; optimus/stratosphere/common/common.obt -> optimus.stratosphere.common
        p = rel.split('/')
        mid = '.'.join(x for x in p[:-1] if x != 'projects')
        s = open(f, errors='ignore').read()
        main = re.search(r'^main\s*\{(.*?)^\}', s, re.M | re.S)
        body = main[1] if main else ''
        deps = []
        comp = re.search(r'compile\s*\{(.*?)^\s*\}', body, re.M | re.S)
        if comp:
            for m in re.finditer(r'modules\s*=\s*\[(.*?)\]', comp[1], re.S):
                deps += [re.sub(r'//.*', '', x).strip() for x in m[1].split('\n')]
        deps = [d.strip(', ') for d in deps if d.strip(', ')]
        mods[mid] = dict(dir=os.path.dirname(f), deps=deps, implementsMacros=bool(re.search(r'implementsMacros\s*=\s*true', body)))
    out = {}
    for mid, m in mods.items():
        n = lines(m['dir'] + '/src/main') or lines(m['dir'])
        if n:
            ds = [d for d in m['deps'] if d in mods]
            out[mid] = dict(lines=n, deps=ds, macroDeps=[d for d in ds if mods[d]['implementsMacros']])
    for v in out.values():
        v['deps'] = [d for d in v['deps'] if d in out]; v['macroDeps'] = [d for d in v['macroDeps'] if d in out]
    return out


def scala():
    root = ROOT + '/src'
    g = {'library': [], 'reflect': ['library'], 'compiler': ['reflect'], 'interactive': ['compiler'], 'repl': ['compiler'], 'scaladoc': ['compiler'], 'scalap': ['compiler'], 'testkit': ['compiler'], 'partest': ['testkit'], 'repl-frontend': ['repl']}
    return {k: dict(lines=lines(f'{root}/{k}'), deps=d, macroDeps=[]) for k, d in g.items() if lines(f'{root}/{k}')}


def scalatest():
    root = SCALATEST
    src = open(root + '/project/scalatest.scala').read()
    mods = {}
    for m in re.finditer(r'lazy val (\w+)\s*=\s*(?:project\.in\(file\("(jvm/[\w-]+)"\)\)|Project\("[\w-]+",\s*file\("(jvm/[\w-]+)"\)\))(.*?)(?=\n\s*lazy val |\Z)', src, re.S):
        deps = []
        for d in re.finditer(r'\.dependsOn\((.*?)\)\s*(?://.*)?$', m[4], re.M | re.S):
            for part in d[1].split(','):
                t = re.match(r'\s*(?:LocalProject\(")?(\w+)', part)
                if t: deps.append(t[1])
        mods[m[1]] = dict(dir=root + '/' + (m[2] or m[3]), deps=deps)
    out = {}
    for k, m in mods.items():
        ds = sorted({d for d in m['deps'] if d in mods})
        out[k] = dict(lines=lines(m['dir']), deps=ds, macroDeps=[d for d in ds if d == 'scalacticMacro'])
    return out


def depth(g):
    memo = {}
    def d(m):
        if m not in memo: memo[m] = 1 + max((d(x) for x in g[m]['deps']), default=0)
        return memo[m]
    return max(d(m) for m in g)


def main():
    gs = dict(pekko=pekko(), optimus=optimus(), scala=scala(), scalatest=scalatest())
    json.dump(gs, open(OUT + '/graphs.json', 'w'), indent=0)
    for k, g in gs.items():
        ls = sorted(v['lines'] for v in g.values())
        tot = sum(ls); import math
        logs = [math.log(x) for x in ls if x]
        dep = [len(v['deps']) for v in g.values()]
        print(k, 'modules', len(g), 'lines', tot, 'median', st.median(ls), 'max', ls[-1], 'top share %.0f%%' % (100 * ls[-1] / tot), 'log-sigma %.2f' % (st.pstdev(logs)), 'deps mean %.1f max %d' % (st.mean(dep), max(dep)), 'depth', depth(g), 'macro edges', sum(len(v['macroDeps']) for v in g.values()), 'of', sum(dep))
        print('  biggest', sorted(((v['lines'], m) for m, v in g.items()), reverse=True)[:5])


if __name__ == '__main__':
    main()
