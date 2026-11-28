#!/usr/bin/env python3
"""GC pauses and heap after young collections over a perflab campaign's measured iterations.

  gcsummary.py <campaign dir> [<corpus>]

For each variant and cell, reads iterations.tsv (the measured iterations' wall-clock windows) and
gc.log (-Xlog:gc with time decorations), and prints per variant: ms/op, the share of measured
wall time spent in GC pauses, young pauses per op, their mean length, and the heap after young
collections (mean and max), over all cells.
"""
import datetime
import os
import re
import statistics
import sys

PAUSE = re.compile(r'^\[([^\]]+)\]\[[^\]]+\] GC\(\d+\) Pause (\w+)[^\n]*? (\d+)M->(\d+)M\((\d+)M\) ([\d.]+)ms')


def epoch_ms(ts):
    return datetime.datetime.strptime(ts, '%Y-%m-%dT%H:%M:%S.%f%z').timestamp() * 1000


def cell(path):
    windows, ops, scores = [], 0, []
    with open(os.path.join(path, 'iterations.tsv')) as f:
        header = f.readline().rstrip('\n').split('\t')
        for line in f:
            r = dict(zip(header, line.rstrip('\n').split('\t')))
            if r['kind'] == 'measure':
                windows.append((float(r['start_ms']), float(r['end_ms'])))
                ops += int(r['ops'] or 1)
                scores.append(float(r['score']))
    pauses, young_after, young_ms = 0.0, [], []
    with open(os.path.join(path, 'gc.log')) as f:
        for line in f:
            m = PAUSE.match(line)
            if not m:
                continue
            t = epoch_ms(m.group(1))
            if not any(a <= t <= b for a, b in windows):
                continue
            ms = float(m.group(6))
            pauses += ms
            if m.group(2) == 'Young':
                young_after.append(int(m.group(4)))
                young_ms.append(ms)
    if not scores:
        return None
    wall = sum(b - a for a, b in windows)
    return dict(score=statistics.mean(scores), wall=wall, pauses=pauses, ops=ops,
                young=len(young_ms), young_ms=young_ms, after=young_after)


def main():
    root = sys.argv[1]
    corpus = sys.argv[2] if len(sys.argv) > 2 else 'scala'
    base = os.path.join(root, corpus)
    print(f'{"variant":12} {"cells":>5} {"ms/op":>9} {"pause%":>7} {"young/op":>8} {"young ms":>8} {"after MB":>8} {"max MB":>7}')
    for variant in sorted(os.listdir(base)):
        cells = []
        for c in sorted(os.listdir(os.path.join(base, variant))):
            p = os.path.join(base, variant, c)
            if os.path.exists(os.path.join(p, 'gc.log')) and os.path.exists(os.path.join(p, 'result.json')):
                c = cell(p)
                if c: cells.append(c)
        if not cells:
            continue
        after = [a for c in cells for a in c['after']]
        yms = [y for c in cells for y in c['young_ms']]
        print(f'{variant:12} {len(cells):5} {statistics.mean(c["score"] for c in cells):9.0f} '
              f'{100 * sum(c["pauses"] for c in cells) / sum(c["wall"] for c in cells):6.1f}% '
              f'{sum(c["young"] for c in cells) / sum(c["ops"] for c in cells):8.1f} '
              f'{statistics.mean(yms) if yms else 0:8.1f} {statistics.mean(after) if after else 0:8.0f} {max(after) if after else 0:7}')


if __name__ == '__main__':
    main()
