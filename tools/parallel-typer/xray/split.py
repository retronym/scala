#!/usr/bin/env python3 -I
# Split the recordings of profile.sh for the page: tools/parallel-typer/xray/split.py [profiles dir] [out dir]
# For every corpus under profiles/, writes data/<corpus>.js with the timelines of its runs, and each run's profile as
# a file of its own, data/<corpus>.<config>.profile.js, which the page loads when the run is first shown. data/manifest.js
# names the timeline files. A thread sweep of a few corpora with two profiles per count is hundreds of MB in one file.
import json, os, re, sys
here = os.path.dirname(os.path.abspath(__file__))
src = sys.argv[1] if len(sys.argv) > 1 else os.path.join(here, 'profiles')
out = sys.argv[2] if len(sys.argv) > 2 else os.path.join(here, 'data')
os.makedirs(out, exist_ok=True)
for f in os.listdir(out):
    if f.endswith('.js'): os.remove(os.path.join(out, f))
def order(name):
    m = re.match(r'run-(serial|\d+)(-wall)?\.json$', name)
    return (0 if m.group(1) == 'serial' else int(m.group(1)), 1 if m.group(2) else 0)
manifest = []
for corpus in sorted(os.listdir(src)):
    d = os.path.join(src, corpus)
    runs = sorted((f for f in os.listdir(d) if re.match(r'run-(serial|\d+)(-wall)?\.json$', f)), key=order) if os.path.isdir(d) else []
    if not runs: continue
    slug = re.sub(r'[^A-Za-z0-9_.-]', '_', corpus)
    with open(os.path.join(out, slug + '.js'), 'w') as tl:
        for f in runs:
            r = json.load(open(os.path.join(d, f)))
            p = r.pop('profile', None)
            if p is not None:
                ref = 'data/%s.%s.profile.js' % (slug, r['meta']['config'])
                r['profileRef'] = ref
                with open(os.path.join(out, os.path.basename(ref)), 'w') as pf:
                    pf.write('(window.XRAY_PROFILES = window.XRAY_PROFILES || {})[%s] = %s;\n' % (json.dumps(ref), json.dumps(p, separators=(',', ':'))))
            tl.write('window.XRAY_RUNS = (window.XRAY_RUNS || []).concat([\n%s\n]);\n' % json.dumps(r, separators=(',', ':')))
    manifest.append('data/' + slug + '.js')
with open(os.path.join(out, 'manifest.js'), 'w') as m:
    m.write('window.XRAY_MANIFEST = %s;\n' % json.dumps(manifest))
print('%d corpora: %s' % (len(manifest), ', '.join(manifest)))
