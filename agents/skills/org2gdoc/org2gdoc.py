#!/usr/bin/env -S uv run --script --quiet
# /// script
# requires-python = ">=3.11"
# dependencies = []
# ///
"""org2gdoc: .org -> native, linkable, tidy Google Doc.

  org2gdoc run in.org [folder-id] [--emacs]   export, import, polish, check; prints "0 bad", URL, one #heading= link
  org2gdoc check <doc-id | get.json>          list defects of an existing Doc, then "<n> bad"

Needs pandoc and a logged-in gws (Google Workspace CLI); --emacs uses emacsclient + ox-pandoc instead of pandoc.
Deterministic; nothing here reads the org except to find verbatim blocks and #+title.
"""
import json, os, re, shutil, subprocess, sys, tempfile, time
from pathlib import Path

HERE = Path(__file__).resolve().parent
DOCX = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
SPACING = {"HEADING_1": (28, 8), "HEADING_2": (18, 6), "HEADING_3": (12, 4)}  # spaceAbove/spaceBelow, PT
SPLIT = " — "  # ' — ': TITLE tail after it becomes the SUBTITLE
BORDER = {"color": {"color": {"rgbColor": {}}}, "width": {"magnitude": 1, "unit": "PT"}, "dashStyle": "SOLID"}
PAD = {"magnitude": 5, "unit": "PT"}
TABLE_CELL = {**{f"border{s}": BORDER for s in ("Top", "Bottom", "Left", "Right")},
              **{f"padding{s}": PAD for s in ("Top", "Bottom", "Left", "Right")}}  # Docs' own table defaults
VERBATIM = {"indentStart": {"magnitude": 24, "unit": "PT"},
            "borderLeft": {"color": {"color": {"rgbColor": {"red": 0.6, "green": 0.6, "blue": 0.6}}},
                           "width": {"magnitude": 2, "unit": "PT"}, "padding": {"magnitude": 10, "unit": "PT"},
                           "dashStyle": "SOLID"}}


# ---- gws ------------------------------------------------------------------

def gws(*args, cwd=None):
    r = subprocess.run(["gws", *args], capture_output=True, text=True, cwd=cwd)
    try:
        out = json.loads(r.stdout)
    except json.JSONDecodeError:
        sys.exit(f"gws {' '.join(args[:3])}: {r.stdout.strip()}\n{r.stderr.strip()}")
    if "error" in out:
        sys.exit(f"gws {' '.join(args[:3])}: {json.dumps(out['error'])[:600]}")
    return out


def get(doc_id):
    return gws("docs", "documents", "get", "--params", json.dumps({"documentId": doc_id}))


def batch(doc_id, doc, reqs):
    if reqs:
        gws("docs", "documents", "batchUpdate", "--params", json.dumps({"documentId": doc_id}),
            "--json", json.dumps({"requests": reqs, "writeControl": {"requiredRevisionId": doc["revisionId"]}}))


# ---- document walk ----------------------------------------------------------

def paragraphs(doc):
    for el in doc["body"]["content"]:
        if "paragraph" in el:
            yield el["startIndex"], el["endIndex"], el["paragraph"]


def tables(doc):
    for el in doc["body"]["content"]:
        if "table" in el:
            yield el["startIndex"], el["table"]


def text(p):
    return "".join(e.get("textRun", {}).get("content", "") for e in p.get("elements", []))


def style(p):
    return p.get("paragraphStyle", {})


def headings(doc):
    for s, e, p in paragraphs(doc):
        t = style(p).get("namedStyleType", "")
        if t.startswith("HEADING_"):
            yield s, e, p, t


def ups(s, e, ps, fields):
    return {"updateParagraphStyle": {"range": {"startIndex": s, "endIndex": e}, "paragraphStyle": ps, "fields": fields}}


# ---- request builders (all highest index first) ----------------------------------

def ids(doc):
    """Drive import mints no headingId; the API mints one only on a style transition, so toggle via NORMAL_TEXT."""
    reqs = []
    for s, e, p, t in sorted(headings(doc), key=lambda h: -h[0]):
        if not style(p).get("headingId"):
            reqs.append(ups(s, e, {"namedStyleType": "NORMAL_TEXT"}, "namedStyleType"))
            reqs.append(ups(s, e, {"namedStyleType": t}, "namedStyleType"))
    return reqs


def polish(doc):
    items = []
    for s, e, p, t in headings(doc):
        if t in SPACING:
            a, b = SPACING[t]
            items.append((s, ups(s, e, {"keepWithNext": True, "spaceAbove": {"magnitude": a, "unit": "PT"},
                                        "spaceBelow": {"magnitude": b, "unit": "PT"}},
                                 "keepWithNext,spaceAbove,spaceBelow")))
    for s, _ in tables(doc):
        items.append((s, {"updateTableCellStyle": {"tableStartLocation": {"index": s}, "tableCellStyle": TABLE_CELL,
                                                   "fields": ",".join(TABLE_CELL)}}))
    reqs = [r for _, r in sorted(items, key=lambda x: -x[0])]
    for s, e, p in paragraphs(doc):  # TITLE has the lowest indices: last in the batch
        if style(p).get("namedStyleType") == "TITLE":
            k = text(p).find(SPLIT)
            if k >= 0:
                at = s + k
                reqs.append({"deleteContentRange": {"range": {"startIndex": at, "endIndex": at + len(SPLIT)}}})
                reqs.append({"insertText": {"location": {"index": at}, "text": "\n"}})
                reqs.append(ups(at + 1, e - len(SPLIT) + 1, {"namedStyleType": "SUBTITLE"}, "namedStyleType"))
            break
    return reqs


def org_blocks(path):
    """First non-empty line of each #+begin_src / #+begin_example / ': ' block."""
    needles, inside, first, fixed = [], False, None, []
    for line in Path(path).read_text(encoding="utf-8").splitlines():
        low = line.strip().lower()
        if low.startswith(("#+begin_src", "#+begin_example")):
            inside, first = True, None
        elif low.startswith(("#+end_src", "#+end_example")):
            inside = False
        elif inside:
            if line.strip() and first is None:
                first = line.strip()
                needles.append(first)
        elif line.startswith(": "):
            fixed.append(line[2:].strip())
        elif fixed:
            needles += [x for x in fixed if x][:1]
            fixed = []
    return needles


def blocks(doc, org):
    """Verbatim blocks import as one NORMAL_TEXT paragraph each; find them by their first line."""
    needles = org_blocks(org)
    return [ups(s, e, VERBATIM, "indentStart,borderLeft")
            for s, e, p in sorted(paragraphs(doc), key=lambda x: -x[0]) if any(n in text(p) for n in needles)]


def check(doc):
    bad = []
    for s, e, p, t in headings(doc):
        st = style(p)
        if not st.get("headingId"):
            bad.append(f"no-id {s} {text(p).strip()[:60]}")
        if t in SPACING:
            a, b = SPACING[t]
            got = (st.get("spaceAbove", {}).get("magnitude"), st.get("spaceBelow", {}).get("magnitude"), st.get("keepWithNext"))
            if got != (a, b, True):
                bad.append(f"spacing {s} {t} got {got} want {(a, b, True)}")
    for s, t in tables(doc):
        cell = t["tableRows"][0]["tableCells"][0].get("tableCellStyle", {})
        if not cell.get("borderTop", {}).get("width", {}).get("magnitude"):
            bad.append(f"table {s} no borders")
    for s, e, p in paragraphs(doc):
        if style(p).get("namedStyleType") == "TITLE" and SPLIT in text(p):
            bad.append(f"title {s} not split")
    return bad


# ---- export + import --------------------------------------------------------------

def export(org, work, emacs):
    out = work / "out.docx"
    if emacs:
        subprocess.run(["emacsclient", "-e",
                        f'(with-current-buffer (find-file-noselect "{org}") (org-pandoc-export-to-docx))'],
                       check=True, capture_output=True)
        produced = org.with_suffix(".docx")
        for _ in range(60):
            if produced.exists():
                break
            time.sleep(0.5)
        else:
            sys.exit("emacs export produced no .docx")
        time.sleep(1)
        shutil.move(produced, out)
    else:
        subprocess.run(["pandoc", str(org), "-o", str(out), "--lua-filter", str(HERE / "strip-anchors.lua")], check=True)
    xml = subprocess.run(["unzip", "-p", str(out), "word/document.xml"], capture_output=True, text=True).stdout
    if "bookmarkStart" in xml:
        sys.exit("bookmarks survived export")
    return out


def title_of(org):
    m = re.search(r"^#\+title:\s*(.+)$", Path(org).read_text(encoding="utf-8"), re.I | re.M)
    return m.group(1).strip() if m else Path(org).stem


def run(org, folder, emacs):
    for tool in ("pandoc", "gws") + (("emacsclient",) if emacs else ()):
        if not shutil.which(tool):
            sys.exit(f"missing: {tool}")
    org = Path(org).resolve()
    with tempfile.TemporaryDirectory() as tmp:
        work = Path(tmp)
        docx = export(org, work, emacs)
        meta = {"name": title_of(org), "mimeType": "application/vnd.google-apps.document"}
        if folder:
            meta["parents"] = [folder]
        # gws refuses --upload paths outside its cwd, hence cwd=work
        doc_id = gws("drive", "files", "create", "--json", json.dumps(meta), "--params",
                     json.dumps({"supportsAllDrives": True}), "--upload", docx.name, cwd=work)["id"]
    url = f"https://docs.google.com/document/d/{doc_id}/edit"
    for build in (ids, polish, lambda d: blocks(d, org)):
        doc = get(doc_id)
        batch(doc_id, doc, build(doc))
    doc = get(doc_id)
    bad = check(doc)
    print("\n".join(bad + [f"{len(bad)} bad"]), file=sys.stderr)
    print(url)
    hid = next((style(p).get("headingId") for *_, p, t in headings(doc) if style(p).get("headingId")), None)
    if hid:
        print(f"{url}#heading={hid}")
    sys.exit(1 if bad else 0)


if __name__ == "__main__":
    a = sys.argv[1:]
    if len(a) >= 2 and a[0] == "run":
        emacs = "--emacs" in a
        rest = [x for x in a[1:] if x != "--emacs"]
        run(rest[0], rest[1] if len(rest) > 1 else None, emacs)
    elif len(a) == 2 and a[0] == "check":
        src = a[1]
        doc = json.load(open(src)) if os.path.exists(src) else get(src)
        bad = check(doc)
        print("\n".join(bad + [f"{len(bad)} bad"]))
        sys.exit(1 if bad else 0)
    else:
        sys.exit(__doc__)
