#!/usr/bin/env python3 -I
# A project's Bloop export -> the module list XrayBuild.scala reads: one record per module, in dependency order.
#
#   bloop_modules.py <.bloop dir> <out file> [--scala 2.13] [--exclude regex] [--no-tests] [--sbt-sources file]
#
# Keeps JVM projects of that Scala version. A module provides macros if a source imports scala.reflect.macros. Lines of
# the record: M name, L scala lines, P (provides macros), D direct dependency, U module on its classpath, J other
# classpath entry, S source, O scalac option, R resource directory.
import argparse, glob, json, os, re, sys

MACROS = re.compile(r'scala\.reflect\.macros|\.blackbox\b|\.whitebox\b')
# options of the build tool's or another compiler's, not of this compile
DROP = re.compile(r'^(-Xplugin|-Yrangepos|-Xsemanticdb|-P:|-Ybackend-parallelism)')


def sources(p):
    seen = []
    def walk(d, pred):
        if os.path.isfile(d):
            if d not in seen: seen.append(d)
            return
        for r, _, fs in os.walk(d):
            for x in sorted(fs):
                q = os.path.join(r, x)
                if pred(x) and q not in seen: seen.append(q)
    for s in p.get('sources', []): walk(s, lambda x: x.endswith(('.scala', '.java')))
    for g in p.get('sourcesGlobs', []):
        walk(g['directory'], lambda x: x.endswith(('.scala', '.java')))
    return seen


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('bloop'); ap.add_argument('out')
    ap.add_argument('--scala', default='2.13'); ap.add_argument('--exclude', default=''); ap.add_argument('--no-tests', action='store_true')
    ap.add_argument('--sbt-sources', help="output of sbt 'show <project>/Test/sources' ...: a Bloop project lists directories, which can hold files the build does not compile, so sbt's own list wins")
    a = ap.parse_args()
    ps = {}
    for f in glob.glob(a.bloop + '/*.json'):
        p = json.load(open(f))['project']
        if p.get('platform', {}).get('name') != 'jvm' or not p.get('scala', {}).get('version', '').startswith(a.scala): continue
        if a.exclude and re.search(a.exclude, p['name']): continue
        if a.no_tests and p['name'].endswith('-test'): continue
        ps[p['name']] = p
    dirs = {os.path.realpath(p['classesDir']): n for n, p in ps.items()}
    # A generator may write into one scope's directory for another's use, naming each file there: such a file belongs to
    # the project that names it, not to the one that merely lists the directory.
    named = {}
    for n, p in ps.items():
        for x in p.get('sources', []):
            if os.path.isfile(x): named.setdefault(x, set()).add(n)
    sbt_lists = []
    if a.sbt_sources:
        cur = None
        for l in open(a.sbt_sources):
            m = re.match(r'\[info\] \* (.+\.(?:scala|java))\s*$', l)
            if m:
                cur = cur or set()
                cur.add(os.path.realpath(m[1]))
            else:
                if cur: sbt_lists.append(cur)
                cur = None
        if cur: sbt_lists.append(cur)
    recs = {}
    for n, p in ps.items():
        srcs = [x for x in sources(p) if x not in named or n in named[x]]
        # sbt's list for this project: the one that overlaps it most
        best = max(sbt_lists, key=lambda b: len(b & set(srcs)), default=None)
        if best and len(best & set(srcs)) * 2 >= len(best): srcs = sorted(best)
        lines = 0; macros = False
        for s in srcs:
            if s.endswith('.scala'):
                t = open(s, errors='ignore').read()
                lines += t.count('\n')
                if MACROS.search(t): macros = True
        up, jars = [], []
        for c in p['classpath']:
            r = os.path.realpath(c)
            if r in dirs: up.append(dirs[r])
            elif os.path.exists(c): jars.append(c)
        opts = []
        skip = False
        for o in p['scala'].get('options', []):
            if skip: skip = False; continue
            if o in ('-d', '-classpath', '-cp', '-sourcepath'): skip = True; continue
            if not DROP.match(o): opts.append(o)
        recs[n] = dict(name=n, lines=lines, macros=macros, res=[r if r.endswith('/') else r + '/' for r in p.get('resources', []) if os.path.isdir(r)], deps=[d for d in p.get('dependencies', []) if d in ps], up=up, jars=jars, srcs=srcs, opts=opts)
    # a macro implementation runs against the class files of whatever its module has on its classpath
    for n in list(recs):
        if recs[n]['macros']:
            for u in recs[n]['up']: recs[u]['macros'] = recs[u]['macros'] or recs[u]['lines'] > 0
    # dependency order, and only what has something to compile or is passed through
    order, seen = [], set()
    def visit(n):
        if n in seen: return
        seen.add(n)
        for d in recs[n]['deps']: visit(d)
        order.append(n)
    for n in sorted(recs): visit(n)
    with open(a.out, 'w') as f:
        for n in order:
            r = recs[n]
            f.write(f"M {n}\nL {r['lines']}\n")
            if r['macros']: f.write('P\n')
            for k, v in (('D', r['deps']), ('U', r['up']), ('J', r['jars']), ('S', r['srcs']), ('O', r['opts']), ('R', r['res'])):
                for x in v: f.write(f'{k} {x}\n')
    for n in sorted(order, key=lambda n: -recs[n]['lines']):
        r = recs[n]
        print(f"{r['lines']:8d} {len(r['srcs']):5d} {'macros' if r['macros'] else '      '} {n} <- {','.join(r['deps'])}"[:150])
    print(sum(r['lines'] for r in recs.values()), 'lines in', len(recs), 'modules')


if __name__ == '__main__':
    main()
