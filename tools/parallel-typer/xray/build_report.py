#!/usr/bin/env python3
# The page of real builds: XrayBuild output and its module file -> build.data.js beside build.html. Of several outputs the
# later ones win, token by token, so a pilot shows until the full matrix has measured the same token.
#
#   build_report.py <module file> <XrayBuild output>...
import json, os, statistics as st, sys, time

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, 'sim'))
from sim import Curves, simulate
from calibrate import graph, modfile, policy, serial_times

SERIAL = 'trad:1x1'


def rows(path):
    if not os.path.exists(path): return []
    return [json.loads(l[2:]) for l in open(path) if l.startswith('R ') and l.rstrip().endswith('}')]


def reduced_graph(mods, times):
    """The modules with sources, each with its nearest such dependencies and the depth of its longest chain below."""
    keep = [n for n, m in mods.items() if m['lines'] > 0]
    memo = {}
    def below(n):
        if n not in memo:
            memo[n] = set()
            for d in mods[n]['deps']:
                if d in mods: memo[n] |= below(d) | ({d} if mods[d]['lines'] > 0 else set())
        return memo[n]
    near = {}
    for n in keep:
        ds = below(n)
        near[n] = sorted(d for d in ds if not any(d in below(o) for o in ds if o != d))
    depth = {}
    def dp(n):
        if n not in depth: depth[n] = 1 + max((dp(d) for d in near[n]), default=-1)
        return depth[n]
    return [dict(n=n, lines=mods[n]['lines'], macros=mods[n]['macros'], depth=dp(n), deps=near[n], ms=(round(times[n][0]) if n in times else None), pre=(round(times[n][1]) if n in times else None)) for n in keep]


def main():
    mods = modfile(sys.argv[1])
    latest = {}
    sources = {}
    for path in sys.argv[2:]:
        by = {}
        for r in rows(path):
            by.setdefault(r['token'], []).append(r)
        for t, rs in by.items():
            latest[t] = rs; sources[t] = os.path.basename(path).rsplit('.', 1)[0]
    all_rows = [r for rs in latest.values() for r in rs]
    times = serial_times(all_rows)
    serial = st.median(r['batch'] for r in latest[SERIAL]) if SERIAL in latest else None
    tokens = []
    for t, rs in latest.items():
        rec = dict(token=t, source=sources[t], rounds=len(rs), wall=round(st.median(r['batch'] for r in rs)), lo=round(min(r['batch'] for r in rs)), hi=round(max(r['batch'] for r in rs)),
                   errors=max(r['errors'] for r in rs), cpu=round(st.median(r['procCpu'] for r in rs)), gc=round(st.median(r['gcCpu'] for r in rs)), load=round(st.median(r['load'] for r in rs), 1))
        rec['speedup'] = round(serial / rec['wall'], 2) if serial else None
        if times:
            g, _ = graph(mods, times)
            pipelined, budget, pol = policy(t)
            rec['predicted'] = {c: round(simulate(g, Curves(c), budget, pol, pipelined=pipelined)) for c in ('compiler', 'pekko')}
        # the round to draw: the one of median length
        mid = sorted(rs, key=lambda r: r['batch'])[len(rs) // 2]
        rec['mods'] = [dict(n=m['n'], t=m['t'], s=round(m['s']), p=round(m['p']), e=round(m['e'])) for m in mid['mods'] if m['e'] - m['s'] >= 30]
        tokens.append(rec)
    # each module's speed-up at 8 threads alone against its serial time
    scale = []
    one8 = 'pipe:1x8' if 'pipe:1x8' in latest else 'trad:1x8'  # the same with no dependencies to wait for
    if one8 in latest:
        for n, (w, _) in times.items():
            ds = [m['e'] - m['s'] for r in latest[one8] for m in r['mods'] if m['n'] == n]
            if w >= 100 and ds: scale.append(dict(n=n, ms=round(w), s8=round(w / st.median(ds), 2)))
    # FIT= (empty) for a project the fit was not made for
    fit_path = os.environ.get('FIT', os.path.join(HERE, 'sim', 'fit.json'))
    fit = json.load(open(fit_path)) if fit_path and os.path.exists(fit_path) else None
    for t in tokens:
        if fit and t['token'] in fit['predicted']: t['fit'] = fit['predicted'][t['token']]
    # FIT_FIXED: a fit with another project's terms applied unchanged, for its predictions alone
    fixed_path = os.environ.get('FIT_FIXED')
    fixed = json.load(open(fixed_path)) if fixed_path and os.path.exists(fixed_path) else None
    d = dict(generated=time.strftime('%Y-%m-%d %H:%M'), graph=reduced_graph(mods, times), tokens=tokens, serial=serial, scale=scale, fit=fit,
             fitFixed=fixed and {k: fixed[k] for k in ('predicted', 'a', 'p', 'c', 'q', 'heldMeanPct', 'heldWorstPct')},
             totalLines=sum(m['lines'] for m in mods.values()), modules=sum(1 for m in mods.values() if m['lines'] > 0))
    with open(os.environ.get('BUILD_DATA', os.path.join(HERE, 'build.data.js')), 'w') as f:
        # BUILD_KEY: one of several datasets the page switches between
        key = os.environ.get('BUILD_KEY')
        target = f'(window.XRAY_BUILDS = window.XRAY_BUILDS || {{}})[{json.dumps(key)}]' if key else 'window.XRAY_BUILD'
        f.write(f'// Written by build_report.py. Do not edit.\n{target} = ' + json.dumps(d, separators=(',', ':')) + ';\n')
    print('tokens', [(t['token'], t['rounds'], t['wall']) for t in tokens], 'serial', serial)


if __name__ == '__main__':
    main()
