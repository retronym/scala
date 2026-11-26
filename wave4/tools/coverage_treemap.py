#!/usr/bin/env python3
"""Round-by-round coverage treemap from JaCoCo XML reports.

usage: coverage_treemap.py --prefix scala/collection --out treemap.html "round 1=r1.xml" "round 2=r2.xml" ...

Each rectangle is a method, sized by its instruction count and shaded by the share of its
instructions a round reached; methods are grouped by source file. One panel per round, so the
panels show what each change to the generator opened up. Self-contained HTML, no dependencies.
"""
import argparse, html, sys
import xml.etree.ElementTree as ET

def load(path, prefix):
    files = {}  # source file -> [(method, total, covered)]
    for pkg in ET.parse(path).getroot().iter('package'):
        if not pkg.get('name').startswith(prefix): continue
        for cls in pkg.findall('class'):
            src = pkg.get('name') + '/' + (cls.get('sourcefilename') or '?')
            for m in cls.findall('method'):
                for c in m.findall('counter'):
                    if c.get('type') == 'INSTRUCTION':
                        mi, co = int(c.get('missed')), int(c.get('covered'))
                        name = cls.get('name').rsplit('/', 1)[-1] + '.' + m.get('name')
                        files.setdefault(src, []).append((name, mi + co, co))
    return files

def squarify(items, x, y, w, h):
    """items: [(key, size)] sorted descending; returns [(key, x, y, w, h)]."""
    out, items = [], [i for i in items if i[1] > 0]
    total = sum(s for _, s in items)
    if w <= 0 or h <= 0: return out
    while items and w > 1e-6 and h > 1e-6:
        short = min(w, h)
        row, rest, best = [], items, None
        scale = w * h / total if total else 0
        while rest:
            cand = row + [rest[0]]
            s = sum(v for _, v in cand) * scale
            worst = max(max(short * short * v * scale / (s * s), s * s / (short * short * v * scale)) for _, v in cand)
            if best is not None and worst > best: break
            row, rest, best = cand, rest[1:], worst
        s = sum(v for _, v in row) * scale
        thick = s / short if short else 0
        off = 0
        for k, v in row:
            length = v * scale / thick if thick else 0
            out.append((k, x, y + off, thick, length) if w >= h else (k, x + off, y, length, thick))
            off += length
        if w >= h: x, w = x + thick, w - thick
        else: y, h = y + thick, h - thick
        total -= sum(v for _, v in row)
        items = rest
    return out

def shade(total, covered):
    if covered == 0: return '#ffffff'
    r = covered / total
    return '#bfe3f7' if r <= 0.4 else '#5bb8ec' if r <= 0.8 else '#1583c9'

def panel(label, files, layout_files, W, H):
    tot = sum(t for ms in files.values() for _, t, _ in ms); cov = sum(c for ms in files.values() for _, _, c in ms)
    svg = [f'<h3>{html.escape(label)}: {100 * cov / max(tot, 1):.1f}% of instructions reached</h3>',
           f'<svg viewBox="0 0 {W} {H}" width="100%" style="max-width:{W}px;background:#fff" stroke="#fff" stroke-width="0.4">']
    for src, fx, fy, fw, fh in layout_files:
        ms = sorted(files.get(src, []), key=lambda m: -m[1])
        by = {m[0] + str(i): m for i, m in enumerate(ms)}
        for key, x, y, w, h in squarify([(k, m[1]) for k, m in by.items()], fx + 1, fy + 12, max(fw - 2, 0), max(fh - 13, 0)):
            n, t, c = by[key]
            if w * h < 20: continue  # too small to see; keeps the file small
            svg.append(f'<rect x="{x:.1f}" y="{y:.1f}" width="{w:.1f}" height="{h:.1f}" fill="{shade(t, c)}" ><title>{html.escape(n[:70])} {c}/{t}</title></rect>')
        if fw > 40 and fh > 14:
            svg.append(f'<text x="{fx + 3:.1f}" y="{fy + 10:.1f}" font-size="9" font-family="sans-serif" font-weight="bold">{html.escape(src.rsplit("/", 1)[-1][:int(fw / 5.5)])}</text>')
        svg.append(f'<rect x="{fx:.1f}" y="{fy:.1f}" width="{fw:.1f}" height="{fh:.1f}" fill="none" stroke="#888" stroke-width="0.8"/>')
    return '\n'.join(svg) + '</svg>'

def main():
    ap = argparse.ArgumentParser(); ap.add_argument('--prefix', default='scala/'); ap.add_argument('--out', required=True)
    ap.add_argument('--title', default='Coverage by file, round by round'); ap.add_argument('rounds', nargs='+')
    a = ap.parse_args()
    rounds = [(r.split('=', 1)[0], load(r.split('=', 1)[1], a.prefix)) for r in a.rounds]
    W, H = 1100, 620
    sizes = sorted(((s, sum(t for _, t, _ in ms)) for s, ms in rounds[-1][1].items()), key=lambda kv: -kv[1])
    layout = squarify(sizes, 0, 0, W, H)  # same layout in every panel, from the last round
    body = [f'<h2>{html.escape(a.title)}</h2><p>Each rectangle is a method, sized by its instruction count and grouped by source file ({html.escape(a.prefix)}). '
            'Shade: none, 1&ndash;40%, 41&ndash;80%, 81&ndash;100% of the method\'s instructions reached. Hover for the method.</p>']
    body += [panel(l, f, layout, W, H) for l, f in rounds]
    open(a.out, 'w').write('<!doctype html><meta charset="utf-8"><title>' + html.escape(a.title) + '</title><body style="font-family:sans-serif;margin:16px">' + '\n'.join(body))

if __name__ == '__main__': main()
