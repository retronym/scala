#!/usr/bin/env python3
# The page's scaling data: scaling-results.jsonl -> indep-scaling.data.js. Of the rows of one configuration, only those
# of its latest run count, so a re-run replaces what it re-measures and leaves the rest.
#
#   scale_report.py [--check]        write the data file; with --check also print the headline numbers
#   scale_report.py --floor <corpus> print the smallest heap of the floor search with no full collection in a round
import json, os, re, statistics, sys, time

HERE = os.path.dirname(os.path.abspath(__file__))
RESULTS = os.environ.get('RESULTS', os.path.join(HERE, 'scaling-results.jsonl'))
OUT = os.environ.get('INDEP_DATA', os.path.join(HERE, 'indep-scaling.data.js'))
med = statistics.median


def load():
    rows = [json.loads(l) for l in open(RESULTS)]
    latest = {}
    def key(r):
        if r.get('kind') == 'hw': return ('hw', r['corpus'], r['setup'], r['n'])
        if r.get('kind') == 'spin': return ('spin', r['name'], r['k'])
        return ('r', r['corpus'], r['tag'], r['token'])
    for r in rows: latest[key(r)] = max(latest.get(key(r), -1), r.get('run', 0))
    return [r for r in rows if r.get('run', 0) == latest[key(r)]]


def stat(rows, corpus, tag, token):
    g = [r for r in rows if not r.get('kind') and r['corpus'] == corpus and r['tag'] == tag and r['token'] == token and not r['void']]
    if not g: return None
    n = int(token[1:]) if token[0] == 'k' else 1
    after = [x for r in g for x in r['afterMB']] or [0]
    return dict(
        n=n, heap=g[0]['heap'], rounds=len(g), wall=round(med(r['batch'] for r in g)), lo=round(min(r['batch'] for r in g)), hi=round(max(r['batch'] for r in g)),
        cpu=round(med(x for r in g for x in r['cpus'])), allocGB=round(med(sum(r['allocs']) / len(r['allocs']) / 1e9 for r in g), 2),
        nonGcCpu=round(med((r['procCpu'] - r['gcCpu']) / n for r in g)), gcCpu=round(med(r['gcCpu'] / n for r in g)),
        young=med(r['young'] for r in g), full=med(r['full'] for r in g), stopped=round(med(r['stoppedMs'] for r in g)),
        largest=round(med(r['largestMs'] for r in g)), afterMed=round(med(after)), afterMax=max(after), errors=sum(r['errors'] for r in g),
        load=round(med(r['load'] for r in g), 1), systemGcCounted=any('System.gc()' in r.get('fullCauses', []) for r in g))


FLOOR = re.compile(r'floor-(\d+)g$')


def floors(rows, corpus):
    out = []
    for tag in sorted({r['tag'] for r in rows if not r.get('kind') and r['corpus'] == corpus and FLOOR.match(r['tag'])}, key=lambda t: int(FLOOR.match(t)[1])):
        s = stat(rows, corpus, tag, 'k8')
        # every round, not the median: a heap with a full collection in any round is below the floor
        fulls = max(r['full'] for r in rows if not r.get('kind') and r['corpus'] == corpus and r['tag'] == tag and r['token'] == 'k8')
        out.append(dict(s, gb=int(FLOOR.match(tag)[1]), maxFull=fulls))
    return out


def floor_of(fl):
    ok = [f for f in fl if f['maxFull'] == 0 and f['errors'] == 0]
    return ok[0]['gb'] if ok else None


SETUPS = [('sharedfloor', 'one JVM, floor heap'), ('shared12', 'one JVM, 12 GB, default young'), ('young16', 'one JVM, 24 GB, young 16 GB'),
          ('nopause', 'one JVM, 31 GB, young 30 GB, no collections'), ('fork4', '8 JVMs, 4 GB each, default young'), ('fork4-xmn3', '8 JVMs, 4 GB each, young 3 GB')]


def corpus(rows, c):
    curves = {t: [s for s in (stat(rows, c, t, f'k{n}') for n in (1, 2, 4, 6, 8)) if s] for t in ('shared12', 'sharedfloor', 'fork4')}
    fl = floors(rows, c)
    setups = []
    for tag, label in SETUPS:
        k1, k8 = stat(rows, c, tag, 'k1'), stat(rows, c, tag, 'k8')
        if k1 and k8: setups.append(dict(tag=tag, label=label, k1=k1, k8=k8, ratioOwn=round(8 * k1['wall'] / k8['wall'], 2)))
    best_serial = min(s['k1']['wall'] for s in setups)
    # the machine's ceiling: independent compilations in one heap, at the best heap measured
    one_jvm = [s for s in setups if s['tag'] in ('sharedfloor', 'shared12', 'young16', 'nopause')]
    ceil = min(one_jvm, key=lambda s: s['k8']['wall'])
    d1, d8 = stat(rows, c, 'shared12', 'k1'), stat(rows, c, 'shared12', 'k8')
    added = d8['wall'] - d1['wall']
    stop, cpu = d8['stopped'] - d1['stopped'], d8['cpu'] - d1['cpu']
    ps, pk1, pk8 = stat(rows, c, 'par12', 'p8'), stat(rows, c, 'par12', 'k1'), stat(rows, c, 'par12', 'k8')
    par = None
    if ps:
        par = dict(wall=ps['wall'], lo=ps['lo'], hi=ps['hi'], serialWall=pk1['wall'], ratioOwn=round(pk1['wall'] / ps['wall'], 2), ratioBest=round(best_serial / ps['wall'], 2),
                   ratioUnmixed=round(d1['wall'] / ps['wall'], 2), indepWall=round(pk8['wall'] / 8), indepRatio=round(8 * pk1['wall'] / pk8['wall'], 2),
                   nonGc=ps['nonGcCpu'], nonGcSerial=pk1['nonGcCpu'], nonGcIndep=pk8['nonGcCpu'], stopped=ps['stopped'], stoppedSerial=pk1['stopped'], stoppedIndep=round(pk8['stopped'] / 8),
                   busyPct=round(100 * (ps['nonGcCpu'] / 8 + ps['stopped']) / ps['wall']), extraCpuPct=round(100 * (ps['nonGcCpu'] / pk8['nonGcCpu'] - 1)),
                   overCeilingPct=round(100 * (ps['wall'] / (ceil['k8']['wall'] / 8) - 1)), load=ps['load'], rounds=ps['rounds'],
                   machineShare=round(100 * (8 - 8 * pk1['wall'] / pk8['wall']) / (8 - pk1['wall'] / ps['wall'])))
    return dict(
        curves=curves, setups=setups, floor=fl, floorGb=floor_of(fl), bestSerial=best_serial,
        ceiling=dict(tag=ceil['tag'], label=ceil['label'], wall=ceil['k8']['wall'], perCompile=round(ceil['k8']['wall'] / 8), ratioBest=round(8 * best_serial / ceil['k8']['wall'], 2), ratioOwn=ceil['ratioOwn'],
                     cpuPct=round(100 * (ceil['k8']['cpu'] / ceil['k1']['cpu'] - 1)), serialSlowerPct=round(100 * (ceil['k1']['wall'] / d1['wall'] - 1))),
        default=dict(wall=d8['wall'], perCompile=round(d8['wall'] / 8), ratioBest=round(8 * best_serial / d8['wall'], 2), ratioOwn=round(8 * d1['wall'] / d8['wall'], 2), serial=d1['wall'], cpuPct=round(100 * (d8['cpu'] / d1['cpu'] - 1))),
        gap=dict(added=added, stop=stop, cpu=cpu, other=added - stop - cpu, stopPct=round(100 * stop / added), cpuPct=round(100 * cpu / added),
                 stoppedTimes=round(d8['stopped'] / max(d1['stopped'], 1)), liveFrom=d1['afterMed'], liveTo=d8['afterMed'], collectionsFrom=d1['young'], collectionsTo=d8['young'], gcCpuFrom=d1['gcCpu'], gcCpuTo=d8['gcCpu']),
        allocGB=d8['allocGB'], par=par)


def build(rows):
    cs = sorted({r['corpus'] for r in rows if not r.get('kind')}, key=lambda c: ('pekko', 'compiler').index(c) if c in ('pekko', 'compiler') else 9)
    hw = [dict(r, ipc=round(r['insG'] / r['cycG'], 2), ghz=round(r['cycG'] / r['cpu'] * 1000, 2), mpki=round(r['missM'] / r['insG'], 2)) for r in rows if r.get('kind') == 'hw']
    spin = {}
    for r in rows:
        if r.get('kind') == 'spin': spin.setdefault(r['name'], []).append(dict(k=r['k'], total=r['total'], lo=r['lo'], hi=r['hi']))
    for v in spin.values():
        v.sort(key=lambda x: x['k'])
        one = v[0]['total']
        for x in v: x['ratio'] = round(x['total'] / one, 1)
    stamps = [r['t0'] for r in rows if 't0' in r]
    return dict(
        measuredFrom=time.strftime('%Y-%m-%d', time.gmtime(min(stamps) / 1000)), measuredTo=time.strftime('%Y-%m-%d', time.gmtime(max(stamps) / 1000)),
        commits=sorted({r['commit'] for r in rows if r.get('commit')}), corpora={c: corpus(rows, c) for c in cs}, hw=hw, spin=spin,
        void=sum(1 for r in rows if r.get('void')), rounds=sum(1 for r in rows if 't0' in r))


def main():
    rows = load()
    if len(sys.argv) > 2 and sys.argv[1] == '--floor':
        gb = floor_of(floors(rows, sys.argv[2]))
        if gb is None: sys.exit(f'no heap of the floor search for {sys.argv[2]} ran without a full collection')
        print(gb)
        return
    d = build(rows)
    with open(OUT, 'w') as f:
        f.write('// Written by scale_report.py from scaling-results.jsonl. Do not edit.\nwindow.XRAY_INDEP = ' + json.dumps(d, separators=(',', ':')) + ';\n')
    if '--check' in sys.argv:
        for c, v in d['corpora'].items():
            print(c, 'floor', v['floorGb'], 'best serial', v['bestSerial'], 'ceiling', v['ceiling'], '\n default', v['default'], '\n gap', v['gap'], '\n par', v['par'])
        print(d['measuredFrom'], d['measuredTo'], d['commits'], 'void', d['void'], 'of', d['rounds'])


if __name__ == '__main__':
    main()
