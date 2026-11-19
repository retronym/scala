#!/usr/bin/env python3
# N independent compilations at once against one alone (XrayScale.scala), with each round's collections read from the
# JVM's log. Appends one JSON line per round to RESULTS; scale_report.py turns them into the page's data. scale.sh runs
# the whole sweep.
#
#   scale.py shared --args A --heap 12g --tokens k1,k2,k4,k6,k8 [--rounds 3] [--warmup 8]    one JVM
#   scale.py fork   --args A --heap 4g --n 8 [--rounds 3] [--warmup 8]                       N JVMs, started together
#   scale.py hw     --args A --heap 12g --tokens k1,k8 --setup "12 GB"                       shared, with hardware counters
#   scale.py spin                                                                            no compiler: 1 to 18 threads
#
# Environment: JAVA, SNAP (the classpath: compiler snapshot, its jars, XrayScale's classes and, for hw, async-profiler's
# jar), JAVA_OPTS, RESULTS, TAG, COMMIT, LOGDIR, PERFLAB (default ~/code/retronym/jvm-perf-lab), KPERF_SOCK.
# A round is void if the machine slept in it: the wall clock moved and the monotonic clock did not.
import argparse, collections, json, os, re, shlex, statistics, subprocess, sys, tempfile, time

GC = re.compile(r'^\[(\d+)ms\]\[gc\s*\] GC\(\d+\) Pause (Young|Full) \((.*?)\) (\d+)M->(\d+)M\((\d+)M\) ([\d.]+)ms')
SP = re.compile(r'^\[(\d+)ms\]\[safepoint\s*\] Safepoint "([^"]+)".*Total: (\d+) ns')
HERE = os.path.dirname(os.path.abspath(__file__))
RUN = int(time.time())


def results_path():
    return os.environ.get('RESULTS', os.path.join(HERE, 'scaling-results.jsonl'))


def read_log(path):
    gcs, sps = [], []
    with open(path) as f:
        for line in f:
            m = GC.match(line)
            if m:
                gcs.append(dict(t=int(m[1]), kind=m[2], cause=m[3], before=int(m[4]), after=int(m[5]), cap=int(m[6]), ms=float(m[7])))
                continue
            m = SP.match(line)
            if m:
                sps.append(dict(t=int(m[1]), op=m[2], ms=int(m[3]) / 1e6))
    return gcs, sps


def window(log, t0, t1):
    gcs, sps = log
    # the System.gc that precedes a round can log within the millisecond of its start
    g = [x for x in gcs if t0 <= x['t'] <= t1 and x['cause'] != 'System.gc()']
    s = [x for x in sps if t0 <= x['t'] <= t1 and x['op'] != 'ParallelGCCollect']
    return dict(
        young=sum(x['kind'] == 'Young' for x in g), full=sum(x['kind'] == 'Full' for x in g),
        fullCauses=[x['cause'] for x in g if x['kind'] == 'Full'],
        pauseMs=sum(x['ms'] for x in g), largestMs=max((x['ms'] for x in g), default=0.0),
        stoppedMs=sum(x['ms'] for x in s), safepoints=len(s),
        afterMB=[x['after'] for x in g], beforeMB=[x['before'] for x in g], capMB=max((x['cap'] for x in g), default=0))


def jvm(a, log):
    return [os.environ.get('JAVA', 'java'), '-Xss64m', f'-Xms{a.heap}', f'-Xmx{a.heap}', '-XX:+UseParallelGC',
            f'-Xlog:gc,safepoint:file={log}:tm,tags', '-Dxray.full=true', *shlex.split(os.environ.get('JAVA_OPTS', '')),
            '-cp', os.environ['SNAP'], 'XrayScale']


def parse(line):
    return json.loads(line[2:]) if line.startswith('R ') else None


def append(rec):
    with open(results_path(), 'a') as f:
        f.write(json.dumps(rec, separators=(',', ':')) + '\n')


def emit(a, rec):
    rec.update(tag=os.environ.get('TAG', ''), corpus=os.path.basename(a.args), heap=a.heap, run=RUN, commit=os.environ.get('COMMIT', ''))
    rec['void'] = abs((rec['t1'] - rec['t0']) - rec['nanoMs']) > 500
    append(rec)


def shared(a):
    with tempfile.TemporaryDirectory(dir=os.environ.get('LOGDIR')) as d:
        log = f'{d}/gc.log'
        cmd = jvm(a, log) + [a.args, str(a.warmup), str(a.rounds)] + a.tokens.split(',')
        p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        rs = []
        for line in p.stdout:
            r = parse(line)
            if r: rs.append(r)
            else: print(line.rstrip(), flush=True)
        status = p.wait()
        l = read_log(log)
        for r in rs:
            r.update(window(l, r['t0'], r['t1']), mode='shared', n=len(r['walls']))
            emit(a, r)
        # what stays after a System.gc between rounds: the classpath in memory and whatever else survives a compilation
        base = [x['after'] for x in l[0] if x['kind'] == 'Full' and x['cause'] == 'System.gc()']
        print(f'exit {status}; retained after System.gc: {base[-1] if base else "?"} MB', flush=True)
        if status: sys.exit(status)
        return rs


def fork(a):
    with tempfile.TemporaryDirectory(dir=os.environ.get('LOGDIR')) as d:
        ps = []
        for i in range(a.n):
            cmd = jvm(a, f'{d}/gc{i}.log') + ['--child', a.args, str(a.warmup)]
            ps.append(subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, bufsize=1))

        def until(p, want):
            for line in p.stdout:
                if line.startswith(want): return line
                if not line.startswith('warm-up'): print(line.rstrip(), flush=True)
                elif p is ps[0]: print(line.rstrip(), flush=True)
            sys.exit(f'a child ended waiting for {want}')

        for p in ps: until(p, 'ready')
        rounds = []
        for r in range(1, a.rounds + 1):
            for p in ps: p.stdin.write('gc\n'); p.stdin.flush()
            for p in ps: until(p, 'collected')
            t0 = time.monotonic()
            for p in ps: p.stdin.write('go\n'); p.stdin.flush()
            ones = [parse(until(p, 'R ')) for p in ps]
            rounds.append((r, (time.monotonic() - t0) * 1000, ones))
        for p in ps: p.stdin.close(); p.wait()
        logs = [read_log(f'{d}/gc{i}.log') for i in range(a.n)]
        for r, wall, ones in rounds:
            ws = [window(l, o['t0'], o['t1']) for l, o in zip(logs, ones)]
            rec = dict(token=f'k{a.n}', round=r, batch=wall, mode='fork', n=a.n,
                       walls=[o['walls'][0] for o in ones], cpus=[o['cpus'][0] for o in ones], allocs=[o['allocs'][0] for o in ones],
                       totalAlloc=sum(o['totalAlloc'] for o in ones), errors=sum(o['errors'] for o in ones),
                       procCpu=sum(o['procCpu'] for o in ones), gcCpu=sum(o['gcCpu'] for o in ones),
                       t0=min(o['t0'] for o in ones), t1=max(o['t1'] for o in ones), nanoMs=max(o['nanoMs'] for o in ones),
                       load=ones[0]['load'], young=sum(w['young'] for w in ws), full=sum(w['full'] for w in ws),
                       fullCauses=[c for w in ws for c in w['fullCauses']],
                       pauseMs=sum(w['pauseMs'] for w in ws), largestMs=max(w['largestMs'] for w in ws),
                       stoppedMs=sum(w['stoppedMs'] for w in ws), safepoints=sum(w['safepoints'] for w in ws),
                       afterMB=[x for w in ws for x in w['afterMB']], beforeMB=[x for w in ws for x in w['beforeMB']],
                       capMB=max(w['capMB'] for w in ws))
            emit(a, rec)


def hw(a):
    """Rounds of `shared` under the kperf helper, each recorded; a row of counters per N, per compile on the compile threads."""
    lab = os.path.expanduser(os.environ.get('PERFLAB', '~/code/retronym/jvm-perf-lab')) + '/tools/build/async-profiler/build'
    sock = os.environ.get('KPERF_SOCK', '/tmp/asprof-kperf.sock')
    with tempfile.TemporaryDirectory(dir=os.environ.get('LOGDIR')) as d:
        os.environ['JAVA_OPTS'] = ' '.join([os.environ.get('JAVA_OPTS', ''), f'-agentpath:{lab}/lib/libasyncProfiler.dylib',
            '-XX:+UnlockDiagnosticVMOptions', '-XX:+DebugNonSafepoints', '-XX:+PreserveFramePointer',
            f'-Dxray.asprof={d},event=cycles@2000000+instructions@2000000+cache-misses@50000,kperf={sock},jstackdepth=8192'])
        rs = shared(a)

        def total(jfr, event):
            out = jfr + '.collapsed'
            subprocess.run([f'{lab}/bin/jfrconv', '--hw', event, '-t', '--total', '-o', 'collapsed', jfr, out], check=True, capture_output=True)
            n = 0
            with open(out) as f:
                for line in f:
                    if 'indep-' in line.split(';', 1)[0]: n += int(line.rsplit(' ', 1)[1])
            os.remove(out)
            return n

        med = statistics.median
        for token in a.tokens.split(','):
            k = int(token[1:])
            rows = [r for r in rs if r['token'] == token]
            cs = [[total(f"{d}/{token}-{r['round']}.jfr", e) / k for r in rows] for e in ('cycles', 'instructions', 'cache-misses')]
            append(dict(kind='hw', corpus=os.path.basename(a.args), setup=a.setup, n=k, batch=med(r['batch'] for r in rows),
                        cpu=med(x for r in rows for x in r['cpus']), cycG=round(med(cs[0]) / 1e9, 2), insG=round(med(cs[1]) / 1e9, 1),
                        missM=round(med(cs[2]) / 1e6), commit=os.environ.get('COMMIT', ''), run=RUN))


def spin(a):
    cmd = [os.environ.get('JAVA', 'java'), '-Xmx12g', '-XX:+UseParallelGC', '-cp', os.environ['SNAP'], 'XrayScale',
           '--spin', '3000', '3', '1', '2', '4', '6', '8', '10', '12', '18']
    out = subprocess.run(cmd, stdout=subprocess.PIPE, text=True, check=True).stdout
    by = collections.defaultdict(list)
    for line in out.splitlines():
        r = parse(line)
        if r: by[(r['kind'], r['k'])].append(r['rates'])
    for (name, k), v in sorted(by.items()):
        flat = [x for rates in v for x in rates]
        append(dict(kind='spin', name=name, k=k, total=round(statistics.median(sum(x) for x in v)), lo=round(min(flat)), hi=round(max(flat)), run=RUN))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('mode', choices=['shared', 'fork', 'hw', 'spin'])
    ap.add_argument('--args'); ap.add_argument('--heap')
    ap.add_argument('--tokens', default='k1,k2,k4,k6,k8'); ap.add_argument('--n', type=int, default=1)
    ap.add_argument('--rounds', type=int, default=3); ap.add_argument('--warmup', type=int, default=8)
    ap.add_argument('--setup', default='')
    a = ap.parse_args()
    dict(shared=shared, fork=fork, hw=hw, spin=spin)[a.mode](a)


if __name__ == '__main__':
    main()
