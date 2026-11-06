#!/usr/bin/env python3
"""Count interface dispatch and type check samples in collapsed async-profiler profiles (cstack=vm, features=vtable).
   dispatch.py [top] <run.collapsed>...
   With features=vtable a megamorphic call shows as `...;caller;itable stub;<receiver class>`, so the receiver is the
   leaf and the caller is the frame before the stub. Percentages are of samples on stacks with a Java frame, which
   leaves out GC workers and the JIT; the file's whole total is printed too."""
import collections, re, sys
args = sys.argv[1:]
top = int(args.pop(0)) if args[0].isdigit() else 12
JAVA = re.compile(r'_\[[ij0-9]\]$')
SECONDARY = re.compile(r'secondary_super|partial_subtype', re.I)
TYPECHECK = re.compile(r'slow_subtype|subtype_check|checkcast_arraycopy|Runtime1::(checkcast|instanceof)|OptoRuntime::.*(check|subtype)|is_subtype_of|is_subclass_of', re.I)
def strip(f): return re.sub(r'\.0x[0-9a-f]+', '', JAVA.sub('', f))
def classify(frames):
    """(kind, receiver or leaf, caller)"""
    tail = frames[-3:]
    for name, kind in (('itable stub', 'itable stub'), ('vtable stub', 'vtable stub')):
        if name in tail:
            i = len(frames) - 1 - frames[::-1].index(name)
            return kind, (strip(frames[i + 1]) if i + 1 < len(frames) else '(unknown)'), frames[i - 1] if i else '?'
    for pattern, kind in ((SECONDARY, 'secondary supers'), (TYPECHECK, 'other type check')):
        for f in frames[-4:]:
            if pattern.search(f) and not JAVA.search(f):
                return kind, f, next((x for x in reversed(frames) if JAVA.search(x)), '?')
    return None
for path in args:
    whole = java = 0
    kinds, leaf, by = collections.Counter(), collections.defaultdict(collections.Counter), collections.defaultdict(collections.Counter)
    for line in open(path):
        stack, _, n = line.rpartition(' ')
        n = int(n); whole += n
        frames = stack.split(';')
        if not any(JAVA.search(f) for f in frames): continue
        java += n
        c = classify(frames)
        if c:
            kinds[c[0]] += n; leaf[c[0]][c[1]] += n; by[c[0]][strip(c[2])] += n
    print(f'\n# {path}\nall samples {whole}, with a Java frame {java}')
    for k, c in kinds.most_common():
        print(f'{k:18} {c:7} {100 * c / java:5.2f}%')
    print(f'{"together":18} {sum(kinds.values()):7} {100 * sum(kinds.values()) / java:5.2f}%')
    for k, _ in kinds.most_common():
        print(f'\n## {k}: receiver or leaf')
        for f, c in leaf[k].most_common(top): print(f'{c:7} {f}')
        print(f'## {k}: caller')
        for f, c in by[k].most_common(top): print(f'{c:7} {f}')
