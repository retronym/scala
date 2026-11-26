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
PROMPT = open(os.path.join(W4, "REVIEW-PROMPT.md")).read() if os.path.exists(os.path.join(W4, "REVIEW-PROMPT.md")) else ""
js = "const REVIEW_PROMPT = " + json.dumps(PROMPT, ensure_ascii=False).replace("</", "<\\/") + ";\n" + \
    "const WAVE4 = " + json.dumps(W4D, ensure_ascii=False).replace("</", "<\\/") + ";\n" \
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
// Index: a menu button opens a sidebar listing the page's sections and the panels inside each, linked by the anchors above.
function buildIndex() {
  if (document.getElementById("ix-btn")) return;
  const st = document.createElement("style");
  st.textContent = `
#ix-btn{position:fixed;top:calc(env(safe-area-inset-top,0px) + 10px);left:max(10px,env(safe-area-inset-left,0px));z-index:30;width:36px;height:36px;display:grid;place-items:center;
  font:inherit;color:var(--fg);background:var(--panel);border:1px solid var(--line);border-radius:8px;cursor:pointer;box-shadow:0 1px 3px rgba(0,0,0,.12)}
#ix-btn:focus-visible,#ix a:focus-visible,#ix-close:focus-visible{outline:2px solid var(--info);outline-offset:2px}
#ix-btn svg{width:18px;height:18px;fill:none;stroke:currentColor;stroke-width:2;stroke-linecap:round}
/* wide: the button has its own gutter beside the column; narrow: it floats at the bottom so it never covers a heading */
@media (min-width: 900px){html body{padding-left:max(62px, env(safe-area-inset-left, 0px))}}
@media (max-width: 899.98px){#ix-btn{top:auto;bottom:calc(env(safe-area-inset-bottom,0px) + 14px);width:42px;height:42px;border-radius:50%}}
#ix-shade{position:fixed;inset:0;z-index:31;background:rgba(0,0,0,.35);opacity:0;pointer-events:none;transition:opacity .15s}
#ix{position:fixed;top:0;bottom:0;left:0;z-index:32;width:min(340px,88vw);background:var(--panel);border-right:1px solid var(--line);overflow-y:auto;overscroll-behavior:contain;
  padding:calc(env(safe-area-inset-top,0px) + 12px) 14px 24px max(14px,env(safe-area-inset-left,0px));transform:translateX(-102%);transition:transform .18s ease;visibility:hidden}
body.ix-open #ix{transform:none;visibility:visible} body.ix-open #ix-shade{opacity:1;pointer-events:auto}
@media (prefers-reduced-motion: reduce){#ix,#ix-shade{transition:none}}
#ix .ix-top{display:flex;align-items:center;justify-content:space-between;margin-bottom:8px}
#ix .ix-top b{font-size:13px;text-transform:uppercase;letter-spacing:.06em;color:var(--muted);font-weight:600}
#ix-close{font:inherit;background:none;border:0;color:var(--muted);cursor:pointer;font-size:20px;line-height:1;padding:4px 8px;border-radius:6px}
#ix ul{list-style:none;margin:0;padding:0} #ix li{margin:0}
#ix a{display:flex;gap:8px;justify-content:space-between;align-items:baseline;padding:5px 8px;border-radius:6px;color:var(--fg);text-decoration:none;line-height:1.3}
#ix a:hover{background:var(--bg)} #ix a.ix-cur{background:var(--bg);box-shadow:inset 3px 0 0 var(--info)}
#ix .ix-sec>a{font-weight:600;margin-top:6px} #ix .ix-sub a{padding-left:20px;font-size:13px;color:var(--fg)}
#ix .ix-n{flex:none;font-size:12px;color:var(--muted);font-variant-numeric:tabular-nums;white-space:nowrap}
#ix details>summary{list-style:none;cursor:pointer} #ix details>summary::-webkit-details-marker{display:none}
#ix .ix-tog{flex:none;width:14px;color:var(--muted);font-size:11px;text-align:center}`;
  document.head.appendChild(st);
  const textOf = h => [...h.childNodes].filter(n => n.nodeType === 3).map(n => n.textContent).join(" ").trim() || h.textContent.replace(/#$/, "").trim();
  const btn = document.createElement("button"); btn.id = "ix-btn"; btn.type = "button"; btn.setAttribute("aria-label", "Open the index"); btn.setAttribute("aria-expanded", "false"); btn.setAttribute("aria-controls", "ix");
  btn.innerHTML = '<svg viewBox="0 0 18 18" aria-hidden="true"><path d="M2 4.5h14M2 9h14M2 13.5h14"/></svg>';
  const shade = document.createElement("div"); shade.id = "ix-shade";
  const nav = document.createElement("nav"); nav.id = "ix"; nav.setAttribute("aria-label", "Index of this page");
  let html = '<div class="ix-top"><b>Index</b><button id="ix-close" type="button" aria-label="Close the index">&times;</button></div><ul>';
  for (const sec of document.querySelectorAll("main > section, main section")) {
    const h2 = sec.querySelector(":scope > h2"); if (!h2 || !sec.id) continue;
    const subs = [...sec.querySelectorAll(".meta-g, .phase")].filter(x => x.id).map(x => { const h = x.querySelector(":scope > h3, :scope > b"); const tag = x.querySelector(":scope > h3 > .tag, :scope > .pill");
      return `<li><a href="#${x.id}"><span>${esc(textOf(h))}</span>${tag ? `<span class="ix-n">${esc(tag.textContent.trim())}</span>` : ""}</a></li>`; });
    const head = `<a href="#${sec.id}"><span>${esc(textOf(h2))}</span>${subs.length ? `<span class="ix-n">${subs.length}</span>` : ""}</a>`;
    html += subs.length ? `<li class="ix-sec"><details${subs.length <= 12 ? " open" : ""}><summary>${head.replace("<a ", '<a data-sec="1" ').replace("<span>", '<span><span class="ix-tog" aria-hidden="true">▸</span> ')}</summary><ul class="ix-sub">${subs.join("")}</ul></details></li>`
                        : `<li class="ix-sec">${head}</li>`;
  }
  nav.innerHTML = html + "</ul>";
  document.body.append(btn, shade, nav);
  const open = on => { document.body.classList.toggle("ix-open", on); btn.setAttribute("aria-expanded", String(on)); if (on) { mark(); (nav.querySelector("a.ix-cur") || nav.querySelector("a")).focus({ preventScroll: true }); } else btn.focus({ preventScroll: true }); };
  const mark = () => { let id = ""; try { id = decodeURIComponent(location.hash.slice(1)); } catch (e) {} nav.querySelectorAll("a.ix-cur").forEach(a => a.classList.remove("ix-cur"));
    const a = id && nav.querySelector(`a[href="#${CSS.escape(id)}"]`); if (a) { a.classList.add("ix-cur"); const d = a.closest("details"); if (d) d.open = true; a.scrollIntoView({ block: "nearest" }); } };
  btn.addEventListener("click", () => open(!document.body.classList.contains("ix-open")));
  shade.addEventListener("click", () => open(false)); nav.querySelector("#ix-close").addEventListener("click", () => open(false));
  document.addEventListener("keydown", ev => { if (ev.key === "Escape" && document.body.classList.contains("ix-open")) open(false); });
  nav.addEventListener("click", ev => { const a = ev.target.closest("a"); if (!a) return;
    if (a.dataset.sec && ev.target.closest(".ix-tog")) { ev.preventDefault(); const d = a.closest("details"); d.open = !d.open; return; }
    ev.preventDefault(); const id = a.getAttribute("href").slice(1); if (location.hash.slice(1) !== id) location.hash = id; else goToHash(); open(false); });
  nav.querySelectorAll("details").forEach(d => { const t = d.querySelector(".ix-tog"); const set = () => { if (t) t.textContent = d.open ? "▾" : "▸"; }; set(); d.addEventListener("toggle", set); });
  nav.querySelectorAll("summary").forEach(sm => sm.addEventListener("click", ev => { if (!ev.target.closest(".ix-tog")) ev.preventDefault(); }));
  window.addEventListener("hashchange", mark);
}
// Prompt: the review's lessons as context for a later code-review session, from wave4/REVIEW-PROMPT.md, with a copy button.
function addPrompt() {
  if (!REVIEW_PROMPT || document.getElementById("prompt-body")) return;
  const st = document.createElement("style");
  st.textContent = `#prompt-body{background:var(--panel);border:1px solid var(--line);border-radius:8px;padding:14px 18px;max-width:92ch}
#prompt-body h4{font-size:14px;font-weight:600;margin:16px 0 6px} #prompt-body p{margin:0 0 8px} #prompt-body ul{margin:0 0 8px;padding-left:20px} #prompt-body li{margin:0 0 5px}
#prompt-copy{font:inherit;font-size:12.5px;color:var(--fg);background:var(--panel);border:1px solid var(--line);border-radius:999px;padding:3px 12px;cursor:pointer;margin:0 0 8px}
#prompt-copy:focus-visible{outline:2px solid var(--info);outline-offset:2px}`;
  document.head.appendChild(st);
  const inline = t => esc(t).replace(/`([^`]+)`/g, "<code>$1</code>");
  let out = "", list = false;
  for (const line of REVIEW_PROMPT.split("\n")) {
    const isLi = line.startsWith("- ");
    if (list && !isLi) { out += "</ul>"; list = false; }
    if (line.startsWith("## ")) out += `<h4>${inline(line.slice(3))}</h4>`;
    else if (isLi) { if (!list) { out += "<ul>"; list = true; } out += `<li>${inline(line.slice(2))}</li>`; }
    else if (line.trim()) out += `<p>${inline(line)}</p>`;
  }
  if (list) out += "</ul>";
  const sec = document.createElement("section");
  sec.innerHTML = `<h2>Prompt</h2><p class="sub" style="margin:0 0 8px">Context for a later code-review session assessing any change to Scala 2 or Scala 3: what the four waves showed about where defects sit, which checks find them and how fixes go wrong. Source: <code>wave4/REVIEW-PROMPT.md</code> (retronym/scala#745).</p>` +
    `<button id="prompt-copy" type="button">Copy the prompt</button><div id="prompt-body">${out}</div>`;
  const meta = document.getElementById("meta"); const after = meta && meta.closest("section");
  if (after && after.parentNode) after.parentNode.insertBefore(sec, after.nextSibling); else document.querySelector("main").appendChild(sec);
  const btn = sec.querySelector("#prompt-copy");
  btn.addEventListener("click", async () => { let ok = false;
    try { await navigator.clipboard.writeText(REVIEW_PROMPT); ok = true; } catch (e) {
      try { const ta = document.createElement("textarea"); ta.value = REVIEW_PROMPT; ta.style.position = "fixed"; ta.style.opacity = "0"; document.body.appendChild(ta); ta.select(); ok = document.execCommand("copy"); ta.remove(); } catch (e2) {} }
    btn.textContent = ok ? "Copied" : "Copy failed: select the text below"; setTimeout(() => { btn.textContent = "Copy the prompt"; }, 2500); });
}
const anchorsReady = () => { addPrompt(); addAnchors(); buildIndex(); goToHash(); };
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
