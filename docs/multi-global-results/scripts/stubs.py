import sys, collections, re
# collapsed stacks -> what remains in dispatch/type-check overhead and the top self frames
f = sys.argv[1]; tot = 0
leaf = collections.Counter(); slow = collections.Counter(); recv = collections.Counter(); vt = collections.Counter()
for l in open(f):
    st, n = l.rstrip().rsplit(' ', 1); n = int(n); tot += n
    fr = st.split(';'); top = fr[-1]; caller = fr[-2] if len(fr) > 1 else '?'
    leaf[top] += n
    if 'secondary_supers' in top or 'slow_subtype' in top or 'partial_subtype' in top: slow[caller] += n
    # async-profiler names an itable stub frame after its receiver class: a class name with no method
    if re.match(r'^[\w/$]+_\[i\]$', top) and '.' not in top: recv[top] += n
    if 'vtable stub' in top: vt[caller] += n
pc = lambda v: f"{100*v/tot:5.2f}%"
print(f"total samples {tot}")
print("\n-- itable stubs by receiver class:", pc(sum(recv.values())));  [print(pc(v), k) for k, v in recv.most_common(10)]
print("\n-- vtable stubs by caller:", pc(sum(vt.values())));  [print(pc(v), k) for k, v in vt.most_common(8)]
print("\n-- secondary supers / subtype slow paths by caller:", pc(sum(slow.values())));  [print(pc(v), k) for k, v in slow.most_common(8)]
print("\n-- top self frames"); [print(pc(v), k) for k, v in leaf.most_common(15)]
