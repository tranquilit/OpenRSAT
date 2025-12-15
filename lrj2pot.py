#!/usr/bin/env python3
"""
lrj2pot.py - Build/update a GNU gettext .pot file from Lazarus .lrj JSON files.

Typical usage:
  python3 lrj2pot.py --pot locale/messages.pot path/to/forms/**/*.lrj

Notes:
- .lrj files are Lazarus i18n resource files (JSON-based) used to generate .po files. (FPC/Lazarus)
- This script extracts likely UI strings and writes them as msgid entries in a .pot.

No external dependencies.
"""

from __future__ import annotations

import argparse
import datetime as _dt
import glob
import json
import os
import re
import sys
from dataclasses import dataclass
from typing import Any, Dict, Iterable, List, Optional, Sequence, Set, Tuple


# ----------------------------
# POT helpers
# ----------------------------

def _pot_escape(s: str) -> str:
    # Escape for PO/POT quoted strings
    s = s.replace("\\", "\\\\").replace('"', r"\"")
    s = s.replace("\t", r"\t").replace("\r", r"\r").replace("\n", r"\n")
    return s

def _wrap_po_string(s: str, width: int = 76) -> List[str]:
    """
    Wrap a msgid/msgstr into PO syntax lines:
      "...."
      "...."
    Keeps it simple: split into chunks not exceeding width.
    """
    esc = _pot_escape(s)
    if len(esc) <= width:
        return [f"\"{esc}\""]
    out = []
    i = 0
    while i < len(esc):
        out.append(f"\"{esc[i:i+width]}\"")
        i += width
    return out

def _is_probably_translatable(s: str) -> bool:
    """
    Heuristics to ignore obvious non-UI strings.
    Tune as needed for your project.
    """
    if not s:
        return False
    if s.strip() == "":
        return False
    if len(s.strip()) == 1 and s.strip() in "|-_/\\:;":
        return False
    if re.fullmatch(r"[0-9]+", s.strip()):
        return False
    if re.fullmatch(r"[0-9]+\.[0-9]+", s.strip()):
        return False
    # looks like a key/identifier (no spaces, mostly [A-Za-z0-9_.])
    if " " not in s and re.fullmatch(r"[A-Za-z0-9_.]+", s) and len(s) > 12:
        return False
    # skip pure URLs
    if re.match(r"^https?://", s.strip()):
        return False
    return True

def _default_header(project: str) -> str:
    now = _dt.datetime.utcnow().strftime("%Y-%m-%d %H:%M+0000")
    header_lines = [
        'msgid ""',
        'msgstr ""',
        f"\"Project-Id-Version: {project}\\\\n\"",
        f"\"POT-Creation-Date: {now}\\\\n\"",
        "\"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\\\n\"",
        "\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\\\n\"",
        "\"Language-Team: LANGUAGE <LL@li.org>\\\\n\"",
        "\"MIME-Version: 1.0\\\\n\"",
        "\"Content-Type: text/plain; charset=UTF-8\\\\n\"",
        "\"Content-Transfer-Encoding: 8bit\\\\n\"",
        "",
    ]
    return "\n".join(header_lines)


# ----------------------------
# LRJ parsing / extraction
# ----------------------------

@dataclass(frozen=True)
class Extracted:
    msgid: str
    refs: Tuple[str, ...]       # "#: file:jsonpath" style
    msgctxt: Optional[str] = None


def _load_json_loose(path: str) -> Any:
    """
    Loads JSON, with a small fallback for some non-strict cases.
    """
    raw = open(path, "r", encoding="utf-8", errors="replace").read()

    # Normal JSON first
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        pass

    # Fallback: remove trailing commas before ] or }
    raw2 = re.sub(r",(\s*[\]}])", r"\1", raw)
    try:
        return json.loads(raw2)
    except json.JSONDecodeError as e:
        raise ValueError(f"Failed to parse {path} as JSON (.lrj): {e}") from e


def _json_path_join(path_parts: List[str]) -> str:
    # Create a readable json-path-ish reference
    # Example: $.forms[0].components.Button1.Caption
    return "$." + ".".join(path_parts) if path_parts else "$"


def _walk(obj: Any, path_parts: List[str], name_stack: List[str]):
    """
    Walk JSON and propagate Lazarus 'name' hierarchy.
    Yields (path_parts, value, name_stack)
    """
    if isinstance(obj, dict):
        # If this object has a Lazarus name, push it
        if "name" in obj and isinstance(obj["name"], str):
            name_stack = name_stack + [obj["name"]]

        yield (path_parts, obj, name_stack)

        for k, v in obj.items():
            yield from _walk(v, path_parts + [str(k)], name_stack)

    elif isinstance(obj, list):
        for i, v in enumerate(obj):
            yield from _walk(v, path_parts + [f"[{i}]"], name_stack)

    else:
        yield (path_parts, obj, name_stack)



LIKELY_STRING_KEYS = {
    # common Lazarus/UI-ish property names you might see in LRJ-like resources
    "Caption", "Text", "Hint", "Title", "Label", "Description",
    "DisplayName", "Prompt", "Message", "Value",
    # lowercase variants
    "caption", "text", "hint", "title", "label", "description",
    "displayname", "prompt", "message", "value",
}

def _extract_strings_from_lrj(path: str, use_msgctxt: bool) -> List[Extracted]:
    data = _load_json_loose(path)
    by_msgid: Dict[Tuple[Optional[str], str], Set[str]] = {}

    for pparts, node, name_stack in _walk(data, [], []):
        if not isinstance(node, dict):
            continue

        for key, value in node.items():
            if not isinstance(value, str):
                continue
            if key not in LIKELY_STRING_KEYS:
                continue
            if not _is_probably_translatable(value):
                continue

            # Reference = file + JSON path
            ref = f"{os.path.basename(path)}:{_json_path_join(pparts + [key])}"

            # msgctxt = name-based Lazarus path
            msgctxt = None
            if use_msgctxt and name_stack:
                msgctxt = ".".join(name_stack)

            by_msgid.setdefault((msgctxt, value), set()).add(ref)

    return [
        Extracted(msgid=msgid, msgctxt=ctx, refs=tuple(sorted(refs)))
        for (ctx, msgid), refs in by_msgid.items()
    ]



# ----------------------------
# POT writing
# ----------------------------

def _write_pot(
    pot_path: str,
    project_name: str,
    entries: List[Extracted],
    sort: bool,
) -> None:
    os.makedirs(os.path.dirname(pot_path) or ".", exist_ok=True)

    if sort:
        entries = sorted(entries, key=lambda e: (e.msgctxt or "", e.msgid))

    with open(pot_path, "w", encoding="utf-8", newline="\n") as f:
        f.write(_default_header(project_name))

        for e in entries:
            # References
            for r in e.refs:
                f.write(f"#: {r}\n")

            # Context (optional)
            if e.msgctxt:
                for line in _wrap_po_string(e.msgctxt):
                    # msgctxt can be wrapped too; first line uses keyword
                    if line.startswith('"'):
                        pass
                wrapped = _wrap_po_string(e.msgctxt)
                f.write("msgctxt " + wrapped[0] + "\n")
                for extra in wrapped[1:]:
                    f.write(extra + "\n")

            # msgid
            wrapped_id = _wrap_po_string(e.msgid)
            f.write("msgid " + wrapped_id[0] + "\n")
            for extra in wrapped_id[1:]:
                f.write(extra + "\n")

            # msgstr empty in POT
            f.write('msgstr ""\n\n')


# ----------------------------
# CLI
# ----------------------------

def _expand_inputs(inputs: Sequence[str]) -> List[str]:
    out: List[str] = []
    for inp in inputs:
        # allow glob patterns
        matches = glob.glob(inp, recursive=True)
        if matches:
            out.extend(matches)
        else:
            out.append(inp)
    # keep only .lrj files
    out2 = [p for p in out if os.path.isfile(p) and p.lower().endswith(".lrj")]
    # de-dup preserving order
    seen: Set[str] = set()
    final: List[str] = []
    for p in out2:
        ap = os.path.abspath(p)
        if ap not in seen:
            seen.add(ap)
            final.append(ap)
    return final


def main(argv: Optional[Sequence[str]] = None) -> int:
    ap = argparse.ArgumentParser(description="Update a .pot file from Lazarus .lrj files")
    ap.add_argument("--pot", required=True, help="Output POT path (e.g. locale/messages.pot)")
    ap.add_argument("--project", default="PROJECT", help="Project name for POT header")
    ap.add_argument("--no-sort", action="store_true", help="Do not sort entries (keep extraction order)")
    ap.add_argument("--msgctxt", action="store_true", help="Use msgctxt derived from JSON path")
    ap.add_argument("lrj", nargs="+", help="Input .lrj files or glob patterns (recursive globs allowed)")
    args = ap.parse_args(argv)

    lrj_files = _expand_inputs(args.lrj)
    if not lrj_files:
        print("No .lrj files found from inputs.", file=sys.stderr)
        return 2

    all_entries: List[Extracted] = []
    for p in lrj_files:
        try:
            all_entries.extend(_extract_strings_from_lrj(p, use_msgctxt=args.msgctxt))
        except Exception as e:
            print(f"ERROR: {e}", file=sys.stderr)
            return 2

    # Merge duplicates across files (same msgid + same msgctxt)
    merged: Dict[Tuple[Optional[str], str], Set[str]] = {}
    for e in all_entries:
        merged.setdefault((e.msgctxt, e.msgid), set()).update(e.refs)

    final_entries = [
        Extracted(msgctxt=ctx, msgid=msgid, refs=tuple(sorted(refs)))
        for (ctx, msgid), refs in merged.items()
    ]

    _write_pot(
        pot_path=os.path.abspath(args.pot),
        project_name=args.project,
        entries=final_entries,
        sort=not args.no_sort,
    )

    print(f"Wrote {len(final_entries)} entries to {args.pot}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
