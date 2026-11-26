#!/usr/bin/env python3
"""Check or publish wave-four branches recorded by fix workers in wave4/fixes/<ID>/.

  publish_fix4.py check ID...   outcome, commit hygiene, labels and diff stat for each finding
  publish_fix4.py push ID...    push the branch to retronym/scala, open the fork PR, record it in wave4/state.json
  publish_fix4.py pending       ids with a result.json and no PR yet

Fork PRs only (retronym/scala). Labels: code-review-2026-11-19, severity:<the reviewer's level>, area:<area>, no-fix for test-only,
port-to-scala3 for a compiler finding that Scala 3 shares. ID=severity overrides the level."""
import json, os, re, subprocess, sys, glob
W = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
BASE = "4a67cb8202"
STATE = os.path.join(W, "state.json")
LIB = {"GA", "GC"}
# the fork's area:* labels, as sync_labels.py derives them from the dashboard areas in merge_wave4.py
AREA_LABEL = dict(GA="area:collections", GC="area:collections", GJ="area:java-sources", GF="area:typechecker", GE="area:specialization", GE6="area:optimizer")
def sh(*a, cwd=None, check=True):
    r = subprocess.run(a, cwd=cwd, capture_output=True, text=True)
    if check and r.returncode: raise SystemExit("FAILED %s\n%s%s" % (" ".join(a), r.stdout, r.stderr))
    return r.stdout.strip()
def state(): return json.load(open(STATE))
def level(): return {os.path.basename(p)[:-5]: str(json.load(open(p)).get("severity", "low")).lower().split(" ")[0].strip(":,.") for p in glob.glob(os.path.join(W, "reviews", "*.json"))}
def result(i):
    p = os.path.join(W, "fixes", i, "result.json")
    if not os.path.exists(p): return None
    r = json.load(open(p))
    if not os.path.isdir(r.get("workspace", "")) or not os.path.isabs(r.get("workspace", "")): r["workspace"] = os.path.dirname(W)
    return r
def problems(i, r):
    out = []; ws, br = r.get("workspace", ""), r.get("branch", "")
    if sh("git", "rev-parse", "--verify", "-q", br, cwd=ws, check=False) == "": return ["branch missing: %r" % br]
    if sh("git", "rev-list", "--count", "%s..%s" % (BASE, br), cwd=ws) != "1": out.append("not exactly one commit on top of base")
    if sh("git", "merge-base", BASE, br, cwd=ws)[:10] != BASE: out.append("not based on " + BASE)
    msg = sh("git", "log", "-1", "--format=%B", br, cwd=ws)
    if re.search(r"co-authored-by|generated with|claude", msg, re.I): out.append("attribution or 'Claude' in commit message")
    if len(msg.split("\n")[0]) > 72: out.append("subject over 72 chars")
    if re.search(r"(^|\s)@[A-Za-z]", re.sub(r"`[^`]*`", "", msg)): out.append("@mention in commit message")
    if re.search(r"(^|[^\w/])#\d+", msg): out.append("bare #NNN in commit message")
    if sh("git", "diff", "--name-only", BASE, br, "--", "wave4", cwd=ws): out.append("commit touches wave4/")
    d = os.path.join(W, "fixes", i)
    for f in ("pr-title.txt", "pr-body.md"):
        if not os.path.exists(os.path.join(d, f)): out.append("missing " + f)
    if os.path.exists(os.path.join(d, "pr-body.md")):
        b = open(os.path.join(d, "pr-body.md")).read()
        if "Prepared with Claude." not in b: out.append("PR body lacks the closing line")
        if re.search(r"\b(?=[0-9a-f]*[a-f])(?=[0-9a-f]*[0-9])[0-9a-f]{10,40}\b", b): out.append("PR body has a SHA-like token")
        if re.search(r"tests? pass(ed|es)?\b|suite passes|\d+ tests (pass|ran|run|fail)", b, re.I): out.append("PR body describes testing")
        if re.search(r"(^|[^\w/])#\d+", b): out.append("bare #NNN in PR body")
        if re.search(r"generator|seed \d|fuzz", b, re.I): out.append("PR body talks about the generator")
    return out
def labels_for(i, lv, testonly, override):
    f = json.load(open(os.path.join(W, "findings", i, "finding.json")))
    rvp = os.path.join(W, "reviews", i + ".json"); rv = json.load(open(rvp)) if os.path.exists(rvp) else {}
    l = override.get(i, lv.get(i, "low"))
    out = ["code-review-2026-11-19", "performance" if l == "performance" else "severity:" + l]
    area = AREA_LABEL.get(i) or AREA_LABEL.get(re.sub(r"\d+$", "", i))  # campaign findings get theirs from sync_labels.py afterwards
    if area: out.append(area)
    if testonly: out.append("no-fix")
    s3 = (str(f.get("scala3", "")) + " | " + str(rv.get("scala3", ""))).lower()
    if re.sub(r"\d+$", "", i) not in LIB and re.search(r"(^|\| )(same|present)", s3): out.append("port-to-scala3")
    return out
cmd, args = sys.argv[1], sys.argv[2:]
override = dict(a.split("=") for a in args if "=" in a); ids = [a.split("=")[0] for a in args]
st = state()
if cmd == "pending":
    for d in sorted(os.listdir(os.path.join(W, "fixes"))):
        r = result(d) if os.path.isdir(os.path.join(W, "fixes", d)) else None
        if r and not st.get("fix", {}).get(d, {}).get("prs"): print(d, r.get("outcome"), r.get("branch", ""))
    sys.exit(0)
lv = level()
for i in ids:
    r = result(i)
    if not r: print(i, "NO result.json"); continue
    oc = r.get("outcome", "").upper()
    print("== %s %s [%s] %s" % (i, oc, lv.get(i), r.get("branch", "")))
    if oc not in ("FIXED", "TEST ONLY", "DESIGN", "STUCK"): print("   no branch expected:", r.get("note", "")[:300]); continue
    pr = problems(i, r)
    for p in pr: print("   PROBLEM:", p)
    ws, br = r["workspace"], r["branch"]; testonly = oc != "FIXED"; labels = labels_for(i, lv, testonly, override)
    if cmd == "check":
        print("   labels:", ",".join(labels)); print("   unsure:", r.get("unsure", "")[:300])
        if not any(p.startswith("branch") for p in pr): print("   " + sh("git", "show", "--stat", "--format=%s", br, cwd=ws).replace("\n", "\n   "))
        continue
    if pr: print("   not pushed"); continue
    if st.get("fix", {}).get(i, {}).get("prs"): print("   already has PR", st["fix"][i]["prs"]); continue
    sh("git", "push", "-q", "origin", br, cwd=ws)
    d = os.path.join(W, "fixes", i)
    out = sh("gh", "pr", "create", "--repo", "retronym/scala", "--base", "2.13.x", "--head", br, "--title", open(os.path.join(d, "pr-title.txt")).read().strip(),
             "--body-file", os.path.join(d, "pr-body.md"), *sum((["--label", l] for l in labels), []), cwd=ws)
    n = int(re.search(r"/pull/(\d+)", out).group(1))
    st = state()
    st.setdefault("fix", {})[i] = {"prs": [n], "state": "fixed", "note": ("Test only, no fix yet: " if testonly else "Fix: ") + r.get("note", "")}
    json.dump(st, open(STATE, "w"), indent=2, ensure_ascii=False)
    print("   PR", n, "labels", ",".join(labels))
