#!/usr/bin/env python3
"""Report docstrings whose subject changed between a base revision and the tree.

An F# `///` block binds to the declaration that *follows* it. Move a definition
to another file without taking its docstring and the prose stays behind, silently
re-binding to whatever now follows; insert a declaration between a docstring and
its subject and the same thing happens in place. The compiler cannot see either,
the tests cannot see either, and the diff looks local and reasonable in both
cases. This is the oracle for that, and it is the companion to
check-move-is-rename-only.sh: that one says the moved text is unchanged, this one
says the moved text still documents the same thing.

Usage: scripts/check-docstring-attachment.py <base-ref> <file>...

Pass the union of the paths involved on *both* sides, since a docstring that
moves between files is only recognised as unmoved when both ends are in the list.
The output names each block by its first 150 characters, which is enough to grep
for.

Two shapes are reported:
  MOVED   the same block now precedes a differently-named declaration
  MERGED  the block's text survives only inside a larger block, which is what
          happens when a stranded docstring abuts the next one and the two fuse

WHAT THIS CANNOT SEE, and so must be checked by hand:
  * A docstring that was reworded in the same commit that moved it. Rewording
    changes the key, so the block reads as deleted-and-added and no pairing is
    attempted. Reword in a separate commit from the move if you want cover.
  * Which of a merged pair is the intruder. MERGED names the block that lost its
    identity and the declaration the survivor now documents; deciding which half
    belongs where is a reading task.
  * Prose that was already attached to the wrong declaration at the base
    revision. This compares two revisions, so it inherits the base's mistakes.
  * Anything below a `let`: docstrings on local bindings inside a function body
    are not tracked, because the declaration regex is anchored at module level.
"""

import re
import subprocess
import sys
from pathlib import Path

DECL = re.compile(
    r"^\s*(?:\[<[^\]]*>\]\s*)?"
    r"(?:let\s+(?:private\s+|rec\s+|inline\s+|mutable\s+)*(?P<let>[a-zA-Z_][A-Za-z0-9_']*)"
    r"|type\s+(?P<type>[A-Za-z_][A-Za-z0-9_']*)"
    r"|module\s+(?P<module>[A-Za-z_][A-Za-z0-9_']*)"
    r"|(?:member|override)\s+(?:this\.|_\.|val\s+)?(?P<member>[A-Za-z_][A-Za-z0-9_']*)"
    r"|(?P<field>[A-Z][A-Za-z0-9_]*)\s*:\s"
    r"|\|\s*(?P<case>[A-Z][A-Za-z0-9_]*))"
)


def pairs(text: str) -> dict[str, list[str | None]]:
    """(normalised docstring text -> names of the declarations it precedes).

    A block whose subject cannot be identified maps to None, which is itself
    worth reporting: it usually means the declaration went away.
    """
    lines = text.split("\n")
    out: dict[str, list[str | None]] = {}
    i = 0
    while i < len(lines):
        if not lines[i].strip().startswith("///"):
            i += 1
            continue
        j = i
        block = []
        while j < len(lines) and lines[j].strip().startswith("///"):
            block.append(lines[j].strip()[3:].strip())
            j += 1
        # Attributes, blank lines and `//` mechanism comments all sit
        # legitimately between a docstring and its subject.
        k = j
        while k < len(lines) and (
            lines[k].strip().startswith("[<")
            or lines[k].strip().startswith("//")
            or lines[k].strip() == ""
        ):
            k += 1
        subject = None
        if k < len(lines):
            m = DECL.match(lines[k])
            if m:
                subject = next(v for v in m.groupdict().values() if v)
        key = " ".join(w for w in block if w)
        if key:
            out.setdefault(key, []).append(subject)
        i = j
    return out


def side(files: list[str], ref: str | None = None) -> dict[str, list[str | None]]:
    acc: dict[str, list[str | None]] = {}
    for f in files:
        if ref is not None:
            r = subprocess.run(["git", "show", f"{ref}:{f}"], capture_output=True, text=True)
            if r.returncode:
                continue
            text = r.stdout
        else:
            p = Path(f)
            if not p.exists():
                continue
            text = p.read_text()
        for k, v in pairs(text).items():
            acc.setdefault(k, []).extend(v)
    return acc


def main() -> int:
    if len(sys.argv) < 3:
        print("usage: check-docstring-attachment.py <base-ref> <file>...", file=sys.stderr)
        return 2

    ref, files = sys.argv[1], sys.argv[2:]
    old, new = side(files, ref), side(files)

    def names(subs: list[str | None]) -> list[str]:
        return sorted(x or "<none>" for x in subs)

    bad = 0
    new_keys = list(new)
    for key, subs in old.items():
        if key not in new:
            # A block stranded immediately above another docstring fuses with
            # it, so its old text survives only as a substring of a new block.
            # That is a detachment rather than a deletion, and it is the shape
            # the eye misses.
            merged = [k for k in new_keys if key in k]
            if merged:
                bad += 1
                print(f"MERGED  into a larger block, which now documents {names(new[merged[0]])}")
                print(f"        {key[:150]}")
            continue
        if names(subs) != names(new[key]):
            bad += 1
            print(f"MOVED   {names(subs)} -> {names(new[key])}")
            print(f"        {key[:150]}")

    print()
    if bad:
        print(f"docstring-attachment: {bad} block(s) changed subject", file=sys.stderr)
        return 1
    print(f"docstring-attachment: {len(old)} block(s) still document what they did at {ref}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
