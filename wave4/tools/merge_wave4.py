#!/usr/bin/env python3
"""Fold wave four into the review dashboard. Run after merge_wave2.py and merge_extra.py:

    python3 -I merge_wave4.py [dashboard dir]

Reads findings/*/finding.json and state.json beside this script's parent directory and writes one
generated block (// wave4-begin .. // wave4-end) into index.html, plus a wave filter value and area
names. Idempotent; the template patches apply once."""
import json, os, re, glob, html, sys
W4 = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DASH = sys.argv[1] if len(sys.argv) > 1 else os.path.expanduser("~/.claude/scala-2-13-review-dashboard")
P = os.path.join(DASH, "index.html")
e = html.escape
def natkey(s):
    m = re.match(r"([A-Z]+)(\d+)", s); return (m.group(1), int(m.group(2))) if m else (s, 0)
# Wave four uses the page's existing areas, so the fork's area:* labels stay meaningful; the Wave filter says how a finding was found.
# Keys are id prefixes or, for an exception, a full id.
AREA4 = dict(GA="M", GC="M", GJ="J", GF="C", GE="SPZ", GE6="O")
AREA4.update(WJ="J", WP="X", WX="X", WL="M", WS="X")  # campaign prefixes; a finding's own area_code overrides
CODES = set("M K J JI C T Y F B O SPZ VCL ER EV OVR L AS R X".split())
OLD_AREAS4 = 'GENLIB: "generated: library calls", GENJAVA: "generated: Java sources", GENEXPR: "generated: expressions", GENPROG: "generated: programs", '
st = json.load(open(os.path.join(W4, "state.json"))) if os.path.exists(os.path.join(W4, "state.json")) else {}
def level4(f):
    k = str(f.get("kind", "")).lower()
    if re.search(r"wrong result|non-termination|miscompile|crash|regression|unsound|verifyerror", k): return "high"
    if re.search(r"performance|cost|budget", k): return "perf"
    return "medium"
rows, level, scala3, related, ids, regress = [], {}, {}, {}, [], {}
for d in sorted(glob.glob(W4 + "/findings/*/"), key=lambda p: natkey(os.path.basename(p.rstrip("/")))):
    fj = os.path.join(d, "finding.json")
    if not os.path.exists(fj): continue
    try: f = json.load(open(fj))
    except Exception as ex: print("bad json", fj, ex); continue
    fid = str(f.get("id") or os.path.basename(d.rstrip("/")))
    if re.sub(r"\d+$", "", fid) not in AREA4: print("unknown wave-four prefix", fid); continue
    fx = st.get("fix", {}).get(fid, {}); v = st.get("verify", {}).get(fid, {})
    det = "".join('<a href="https://github.com/retronym/scala/pull/%d">retronym/scala#%d</a>. ' % (n, n) for n in fx.get("prs", []))
    if fx.get("note"): det += e(fx["note"]) + "<br>"
    det += e(str(f.get("detail", "")))
    for lab, key in (("Oracle", "oracle"), ("Generator", "generator"), ("Signature", "signature"), ("Seed", "seed"), ("Measured", "measure"), ("Fix sketch", "fix_sketch"),
                     ("On the unfixed base", "on_base"), ("On 2.13.18", "on_2_13_18"), ("Prior work", "prior")):
        val = str(f.get(key, "")).strip()
        if val and val.lower() not in ("n/a", "none found", "none", "not recorded"): det += "<br><b>%s:</b> %s" % (lab, e(val))
    if f.get("confidence") and f["confidence"] != "confirmed": det += "<br><b>Hunter's confidence:</b> " + e(str(f["confidence"]))
    if v.get("note"): det += "<br><b>Coordinator re-run:</b> " + e(v["note"])
    rv = {}
    rp = os.path.join(W4, "reviews", fid + ".json")
    if os.path.exists(rp):
        try:
            rv = json.load(open(rp))
            det += "<br><b>Independent review:</b> %s; %s. %s" % (e(str(rv.get("verdict", ""))), e(str(rv.get("severity", ""))), e(str(rv.get("note", ""))))
            for lab, key in (("Reviewer re-run", "reran"), ("Who hits it", "user_impact"), ("Fixability", "fixability"), ("Regression", "regression")):
                val = str(rv.get(key, "")).strip()
                if val and val.lower() not in ("no", "none", "n/a"): det += "<br><b>%s:</b> %s" % (lab, e(val))
        except Exception as ex: print("bad review", rp, ex)
    det += "<br>Reproduction: <code>wave4/findings/%s/cmd.sh</code> (retronym/scala#745)" % e(fid)
    vd = str(rv.get("verdict", "")).lower()
    rstate = "verified" if vd.startswith(("real", "duplicate")) else "unverified" if vd else None
    state = fx.get("state") or rstate or v.get("state") or "reported"
    if str(fx.get("note", "")).startswith("Test only"): state = "test-only"
    rows.append([fid, str(f.get("area", "")), str(f.get("title", "")), str(f.get("kind", "")), state, det])
    rsev = str(rv.get("severity", "")).lower().split(" ")[0].strip(":,.")
    level[fid] = st.get("level_override", {}).get(fid) or {"critical": "critical", "high": "high", "medium": "medium", "low": "low", "performance": "perf"}.get(rsev) or level4(f)
    rg = str(rv.get("regression", "")).strip()
    if rg.lower().startswith("in unreleased"): regress[fid] = rg
    scala3[fid] = str(f.get("scala3", "not run")); ids.append(fid)
    if str(f.get("area_code", "")) in CODES: AREA4[fid] = str(f["area_code"])
    pr = str(f.get("prior", "none found"))
    related[fid] = {"verdict": "none found" if re.match(r"none", pr, re.I) else "see note", "refs": [], "note": "" if re.match(r"none", pr, re.I) else pr}
    if vd.startswith("real, known"): related[fid]["verdict"] = "known"; related[fid]["note"] = str(rv.get("verdict", "")) + (" | " + related[fid]["note"] if related[fid]["note"] else "")
    elif vd.startswith(("duplicate", "by design", "doubtful", "wrong")): related[fid]["verdict"] = str(rv.get("verdict", ""))[:90]
    # "Regression since 2.13.18" comes from the independent review once there is one: a different symptom on 2.13.18
    # (the hunter's on_2_13_18) is not by itself a regression.
    o18 = str(f.get("on_2_13_18", "")).lower()
    if not rv and (o18.startswith(("different", "absent")) or "regression" in str(f.get("kind", "")).lower()): regress[fid] = str(f.get("on_2_13_18", ""))
regress.update(st.get("regress", {}))
# campaign tracks append one object per round to rounds/<track>.json; show the rounds in the track's agent row ("w4c <track>")
agents = [list(r) for r in st.get("agents", [])]
for r in agents:
    m = re.match(r"w4c (\S+)", r[0])
    rp = os.path.join(W4, "rounds", m.group(1) + ".json") if m else None
    if rp and os.path.exists(rp):
        try:
            rounds = json.load(open(rp)); out = []
            for x in rounds:
                cov = "; ".join("%s %s/%s" % (k, v.get("covered", v.get("lines_covered", "?")), v.get("total", v.get("lines_total", "?"))) if isinstance(v, dict) else "%s %s" % (k, v) for k, v in (x.get("coverage") or {}).items())
                out.append("Round %s: %s. %s cases, %s new and %s known signatures%s%s" % (x.get("round", "?"), str(x.get("change", "")).rstrip("."), x.get("cases", "?"), x.get("signatures_new", "?"), x.get("signatures_known", "?"), ("; coverage " + cov) if cov else "", ("; next: " + str(x.get("next"))) if x.get("next") else ""))
            if out: r[4] = (r[4] + " " if r[4] else "") + " | ".join(out)
        except Exception as ex: print("bad rounds file", rp, ex)
DR = {}
for fp in sorted(glob.glob(os.path.join(DASH, "diffreview-w4-*.json")), key=lambda p: p.endswith("-rework.json")):  # a rework file is applied last
    try: DR.update(json.load(open(fp)))
    except Exception as ex: print("bad diff review", fp, ex)
W4D = {"diffreview": DR, "findings": rows, "level": level, "scala3": scala3, "related": related, "area": AREA4, "ids": ids, "regress": regress,
       "phases": st.get("phases", []), "meta": st.get("meta", []), "agents": agents, "log": st.get("log", [])}
js = "const WAVE4 = " + json.dumps(W4D, ensure_ascii=False).replace("</", "<\\/") + ";\n" \
    "WAVE2.w4 = new Set(WAVE4.ids); WAVE4.ids.forEach(i => WAVE2.ids.add(i)); Object.assign(WAVE2.level, WAVE4.level); Object.assign(WAVE2.area, WAVE4.area);\n" \
    "DATA.findings.push(...WAVE4.findings); DATA.phases.push(...WAVE4.phases); DATA.meta.push(...WAVE4.meta); DATA.agents.unshift(...WAVE4.agents); DATA.log.unshift(...WAVE4.log);\n" \
    'for (const k of ["related", "scala3", "regress", "diffreview"]) DATA[k] = Object.assign(DATA[k] || {}, WAVE4[k]);\n' \
    'DATA.base += " · wave four (26 November: generated inputs and oracles) on the integration build of draft fixes";\n' \
    r'''// Anchor links: every panel (a family, a meta-finding, a phase card) and every section heading gets an id from its
// title and a "#" link that copies a shareable URL. Runs after the page has rendered; idempotent.
const PAGE_URL = "https://claude.ai/artifact/TnFx6zWQH3byJErfZE2uYg";
const slugOf = t => t.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-+|-+$/g, "").slice(0, 80);
function addAnchors() {
  if (!document.getElementById("anchor-style")) { const st = document.createElement("style"); st.id = "anchor-style";
    st.textContent = "a.anchor{margin-left:6px;opacity:.35;text-decoration:none;font-weight:400;font-size:.9em}a.anchor:hover,a.anchor:focus-visible{opacity:1}.anchor-hit{outline:2px solid var(--info);outline-offset:3px}";
    document.head.appendChild(st); }
  const used = new Set();
  const heads = [...document.querySelectorAll("main section > h2, .meta-g > h3, .phase > b")];
  for (const h of heads) {
    if (h.querySelector("a.anchor")) { used.add((h.closest(".meta-g, .phase") || h.parentElement).id); continue; }
    const title = [...h.childNodes].filter(n => n.nodeType === 3).map(n => n.textContent).join(" ").trim() || h.textContent.trim();
    let id = slugOf(title) || "panel", k = 2; while (used.has(id) || (document.getElementById(id) && document.getElementById(id) !== h)) id = slugOf(title) + "-" + k++;
    used.add(id);
    const box = h.closest(".meta-g, .phase") || h.parentElement; box.id = id;
    const a = document.createElement("a"); a.className = "anchor"; a.href = "#" + id; a.textContent = "#"; a.title = "Copy a link to this panel"; a.setAttribute("aria-label", "Link to " + title);
    a.addEventListener("click", () => { try { navigator.clipboard.writeText(PAGE_URL + "#" + id); } catch (e) {} });
    h.appendChild(a);
  }
}
function goToHash() { let id = ""; try { id = decodeURIComponent((location.hash || "").slice(1)); } catch (e) {}
  const el = id && document.getElementById(id); if (!el) return;
  el.scrollIntoView({ block: "start" }); el.classList.add("anchor-hit"); setTimeout(() => el.classList.remove("anchor-hit"), 2500); }
const anchorsReady = () => { addAnchors(); goToHash(); };
if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", anchorsReady); else setTimeout(anchorsReady, 0);
window.addEventListener("load", anchorsReady); window.addEventListener("hashchange", goToHash);
'''
s = open(P).read()
assert "// wave3-end\n" in s and "// wave2-end\n" in s, "run merge_wave2.py and merge_extra.py first"
s = re.sub(r"// wave4-begin.*?// wave4-end\n", "", s, flags=re.S)
s = s.replace("// wave2-end\n", "// wave4-begin: generated by wave4/tools/merge_wave4.py\n" + js + "// wave4-end\n// wave2-end\n")
def sub1(old, new):
    global s
    assert s.count(old) == 1, (s.count(old), old[:70])
    s = s.replace(old, new)
if "WAVE2.w4.has" not in s:  # template patches, once
    sub1('wave: WAVE2.w3.has(f[0]) ? "three" :', 'wave: WAVE2.w4.has(f[0]) ? "four" : WAVE2.w3.has(f[0]) ? "three" :')
    sub1('["one", "two", "three"], v => v === "one" ? "one: code review" :', '["one", "two", "three", "four"], v => v === "four" ? "four: generated inputs and oracles" : v === "one" ? "one: code review" :')
s = s.replace(OLD_AREAS4, "")  # an earlier version of this script added wave-four areas of its own
OLDA, NEWA = 'WAVE2.ids.has(f[0]) ? WAVE2.area[p] || p : p', 'WAVE2.ids.has(f[0]) ? WAVE2.area[f[0]] || WAVE2.area[p] || p : p'
if NEWA not in s: sub1(OLDA, NEWA)
open(P, "w").write(s)
print(len(rows), "wave-four findings:", " ".join(ids), ";", len(s), "bytes")
