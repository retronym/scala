#!/usr/bin/env python3 -I
# A synthetic multi-module build with no dependencies between modules: K copies of one corpus, each in packages of its
# own, and the module file XrayBuild.scala reads, with K modules and no D or U lines.
#
#   synth_modules.py <corpus args file> <K> <out dir> [--package scala.tools.scalap] [-J <classpath entry>]...
#
# The corpus args file is one of /tmp/parallel-typer-xray/corpora/<name>: options, then source paths. Copy NN goes to
# <out dir>/synthNN with the same relative paths, and <out dir>/modules is the module file.
#
# Renaming: in the corpus's own package P (by default the longest common prefix of its package clauses), synthNN goes
# before P's last segment: scala.tools.scalap becomes scala.tools.synthNN.scalap, regex becomes synthNN.regex. So P
# keeps its own name and every package that encloses it, which `private[scala]` and the like need. The leading package
# clauses keep their structure, and every other occurrence of P (imports, qualified references) is replaced.
import argparse, os, re, sys

CLAUSE = re.compile(r'^\s*package\s+([\w.]+)\s*;?\s*$')


def read_args(path):
    opts, srcs = [], []
    for l in open(path):
        l = l.strip()
        if not l: continue
        (srcs if l.endswith(('.scala', '.java')) and not l.startswith('-') else opts).append(l)
    return opts, srcs


def leading_clauses(lines):
    """The leading package clauses (no braces) of a file: their names and the indices of their lines."""
    names, idx = [], []
    for i, l in enumerate(lines):
        s = l.strip()
        if not s or s.startswith('//'): continue
        if s.startswith('/*') or s.startswith('*'): continue  # block comments before the first clause
        m = CLAUSE.match(l)
        if m: names.append(m[1]); idx.append(i); continue
        break
    return names, idx


def common_package(srcs):
    full = []
    for s in srcs:
        names, _ = leading_clauses(open(s).read().split('\n'))
        if names: full.append('.'.join(names).split('.'))
    if not full: sys.exit('no package clauses: pass --package')
    p = full[0]
    for f in full[1:]:
        n = 0
        while n < min(len(p), len(f)) and p[n] == f[n]: n += 1
        p = p[:n]
    if not p: sys.exit('no common package: pass --package')
    return '.'.join(p)


def rewrite(text, pkg, tag):
    new = renamed(pkg, tag)
    lines = text.split('\n')
    names, idx = leading_clauses(lines)
    full = '.'.join(names)
    if names and (full == pkg or full.startswith(pkg + '.')):
        # the clause that names P's last segment gets the tag before it
        last, seen = len(pkg.split('.')) - 1, 0
        for n, i in zip(names, idx):
            parts = n.split('.')
            if seen <= last < seen + len(parts):
                parts.insert(last - seen, tag)
                lines[i] = re.sub(r'package\s+[\w.]+', 'package ' + '.'.join(parts), lines[i], count=1)
                break
            seen += len(parts)
    # everything else that names P: imports, qualified references, _root_.P
    return re.sub(rf'(?<![\w]){re.escape(pkg)}(?!\w)', new, '\n'.join(lines))


def renamed(pkg, tag):
    ps = pkg.split('.')
    return '.'.join(ps[:-1] + [tag, ps[-1]])


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('args'); ap.add_argument('k', type=int); ap.add_argument('out')
    ap.add_argument('--package', help="the corpus's own package (default: the common prefix of its package clauses)")
    ap.add_argument('-J', action='append', default=[], help='a classpath entry of every module (a J line)')
    a = ap.parse_args()
    opts, srcs = read_args(a.args)
    root = os.path.commonpath([os.path.dirname(os.path.realpath(s)) for s in srcs])
    pkg = a.package or common_package(srcs)
    texts = {s: open(s, encoding='utf-8').read() for s in srcs}
    lines = sum(t.count('\n') for t in texts.values())
    os.makedirs(a.out, exist_ok=True)
    with open(os.path.join(a.out, 'modules'), 'w') as mf:
        for k in range(1, a.k + 1):
            tag = f'synth{k:02d}'
            mf.write(f'M {tag}\nL {lines}\n')
            for j in a.J: mf.write(f'J {j}\n')
            for s in srcs:
                rel = os.path.relpath(os.path.realpath(s), root)
                dst = os.path.join(a.out, tag, rel)
                os.makedirs(os.path.dirname(dst), exist_ok=True)
                with open(dst, 'w', encoding='utf-8') as f: f.write(rewrite(texts[s], pkg, tag))
                mf.write(f'S {dst}\n')
            for o in opts: mf.write(f'O {o}\n')
    print(f'{a.k} copies of {len(srcs)} files, {lines} lines, package {pkg} -> {renamed(pkg, 'synthNN')}, in {a.out}')


if __name__ == '__main__':
    main()
