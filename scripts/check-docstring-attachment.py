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
  * A declaration form the regex does not know is named by its own source line
    rather than by an identifier, which keeps it distinguishable but means an
    unrelated edit to that line reads as a changed subject. Teaching the regex
    the form removes the noise; the check is correct either way.
  * A short docstring shared verbatim by several declarations. Blocks are keyed
    by text, so the subjects are compared as a multiset and adding a *new*
    declaration that reuses an existing one-liner is reported as MOVED. Read the
    report: if every old name is still there, nothing was detached.
  * A same-named pair that is not adjacent. Overloads carry their signature in
    the subject only when two successive declarations share a name and kind,
    because qualifying every repeated name buries a move audit in subjects that
    differ solely in whether they were qualified. Two overloads with an
    unrelated declaration between them can therefore still swap prose unseen.
  * A fusion in which one half was also reworded. Fusion is recognised by the
    fused text being exactly the two originals joined, so an edit to either half
    hides it; the block then reads as deleted-and-added like any other rewording.
"""

import os
import re
import subprocess
import sys
from pathlib import Path

# Accessibility and binding modifiers must be consumed rather than captured:
# `let internal fractionBits` whose subject reads as `internal` compares equal to
# every other internal binding, which is exactly the rebinding this looks for.
MODS = r"(?:private\s+|internal\s+|public\s+|rec\s+|inline\s+|mutable\s+|static\s+|abstract\s+|override\s+|default\s+)*"

DECL = re.compile(
    r"^\s*(?:\[<[^\]]*>\]\s*)?"
    r"(?:let\s+" + MODS + r"\((?P<active>\|[^)]*\|)\)"
    r"|let\s+" + MODS + r"``(?P<quoted>[^`]+)``"
    r"|let\s+" + MODS + r"(?P<let>[a-zA-Z_][A-Za-z0-9_']*)"
    r"|type\s+" + MODS + r"(?P<type>[A-Za-z_][A-Za-z0-9_']*)"
    r"|module\s+" + MODS + r"(?P<module>[A-Za-z_][A-Za-z0-9_']*)"
    r"|(?:static\s+|abstract\s+|default\s+)*(?:member|override)\s+"
    + MODS
    + r"(?:this\.|_\.|val\s+)?(?P<member>[A-Za-z_][A-Za-z0-9_']*)"
    r"|(?P<field>[A-Z][A-Za-z0-9_]*)\s*:\s"
    r"|\|\s*(?P<case>[A-Z][A-Za-z0-9_]*))"
)


def subjects(text: str) -> list[tuple[str, str, str]]:
    """(kind, declaration name, normalised declaration line) for each declaration."""
    out = []
    for line in text.split("\n"):
        m = DECL.match(line)
        if m:
            kind, name = next(kv for kv in m.groupdict().items() if kv[1])
            out.append((kind, name, " ".join(line.split())))
    return out


def ambiguous_names(*texts: str) -> set[str]:
    """Names borne by two *successive* declarations in any of these texts.

    An overload set is the one place a name is not identity, so it is the one
    place the subject has to carry the signature too. But qualifying every
    repeated name is worse than useless here: a name repeats across a file all
    the time (a field in two records, a DU case in two unions), a definition
    that moves between files has different neighbours on each side, and the
    audit of a move — the thing this exists for — then fills with subjects that
    differ only in whether they were qualified.

    Adjacency is the whole of the risk. A stranded docstring binds to the
    declaration that follows it, so a name can absorb another's prose only when
    a declaration of that name is the next one along. Restricting to that is
    exact for the failure and silent everywhere else.
    """
    repeated: set[str] = set()
    for text in texts:
        # Only the kinds that can *be* an overload set. A record cannot repeat a
        # field name nor a union a case name, and `case` in particular also
        # matches `match`-expression arms, whose repeated heads are not
        # declarations at all — feeding those in qualified whole types by
        # accident.
        seen = [
            (kind, name)
            for kind, name, _ in subjects(text)
            if kind in ("let", "member", "quoted", "active")
        ]
        repeated.update(a[1] for a, b in zip(seen, seen[1:]) if a == b)
    return repeated


def pairs(text: str, ambiguous: set[str] = frozenset()) -> dict[str, list[str | None]]:
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
                if subject in ambiguous:
                    subject = " ".join(lines[k].split())
            elif lines[k].strip():
                # A form the regex does not know is still a distinguishable
                # subject: use the declaration line itself. Letting every
                # unrecognised form share one `None` is what makes a regex gap
                # fail *open* — cut a definition from under its docstring, and
                # if what follows is also unrecognised the two holes compare
                # equal and the detachment passes. Keyed by text, they do not.
                subject = "<unparsed> " + " ".join(lines[k].split())
        key = " ".join(w for w in block if w)
        if key:
            out.setdefault(key, []).append(subject)
        i = j
    return out


def exists_exactly(f: str) -> bool:
    """Whether `f` exists spelt exactly that way.

    `Path.exists()` is not that question on macOS: the default filesystem is
    case-insensitive, so a mis-cased argument passes it while `git show` — which
    is case-sensitive — finds nothing at the base ref, and the file goes
    silently unexamined. Compare each component against its parent's listing.
    """
    p = Path(f)
    if p.is_absolute():
        return False
    cur = Path(".")
    for part in p.parts:
        try:
            if part not in os.listdir(cur):
                return False
        except OSError:
            return False
        cur = cur / part
    return True


def read(f: str, ref: str | None) -> str | None:
    if ref is not None:
        r = subprocess.run(["git", "show", f"{ref}:{f}"], capture_output=True, text=True)
        return None if r.returncode else r.stdout
    # Exact spelling here too: on a case-insensitive filesystem the two halves
    # of a case-only rename both open the destination, so the current side
    # would read one file twice and report the phantom duplicate.
    return Path(f).read_text() if exists_exactly(f) else None


def sides(
    files: list[str], ref: str
) -> tuple[dict[str, list[str | None]], dict[str, list[str | None]]]:
    """The old and new (docstring -> subjects) maps, built together."""
    old: dict[str, list[str | None]] = {}
    new: dict[str, list[str | None]] = {}
    for f in files:
        was, now = read(f, ref), read(f, None)
        ambiguous = ambiguous_names(*(t for t in (was, now) if t is not None))
        for text, acc in ((was, old), (now, new)):
            if text is None:
                continue
            for k, v in pairs(text, ambiguous).items():
                acc.setdefault(k, []).extend(v)
    return old, new


def split_into(text: str, old: dict[str, list[str | None]], memo) -> list[str] | None:
    """`text` as a concatenation of blocks that stood on their own before, if it is."""
    if text in memo:
        return memo[text]
    if text in old:
        memo[text] = [text]
        return memo[text]
    memo[text] = None  # guard against a pathological re-entry
    for i in range(len(text) - 1, 0, -1):
        if text[i] != " " or text[:i] not in old:
            continue
        rest = split_into(text[i + 1 :], old, memo)
        if rest:
            memo[text] = [text[:i]] + rest
            return memo[text]
    return None


def main() -> int:
    if len(sys.argv) < 3:
        print("usage: check-docstring-attachment.py <base-ref> <file>...", file=sys.stderr)
        return 2

    ref, files = sys.argv[1], sys.argv[2:]

    # An oracle that passes when it read nothing is worse than no oracle: a
    # misspelt ref would silently retire the check.
    if subprocess.run(
        ["git", "rev-parse", "--verify", "--quiet", f"{ref}^{{commit}}"],
        capture_output=True,
    ).returncode:
        print(f"{ref} does not name a commit", file=sys.stderr)
        return 2

    unknown = [
        f
        for f in files
        if not exists_exactly(f)
        and subprocess.run(
            ["git", "cat-file", "-e", f"{ref}:{f}"], capture_output=True
        ).returncode
    ]
    if unknown:
        # Skipping a path silently is how a move goes unchecked: the file that
        # received the definition is exactly the one a typo omits.
        print(
            f"these path(s) exist neither at {ref} nor in the working tree: "
            + ", ".join(unknown),
            file=sys.stderr,
        )
        return 2

    old, new = sides(files, ref)
    if not old:
        print(
            f"none of the {len(files)} file(s) given exist at {ref}, so there is "
            "nothing to compare against; pass the union of the paths involved on "
            "both sides",
            file=sys.stderr,
        )
        return 2

    def names(subs: list[str | None]) -> list[str]:
        return sorted(x or "<none>" for x in subs)

    bad = 0
    for key, subs in old.items():
        if key in new and names(subs) != names(new[key]):
            bad += 1
            print(f"MOVED   {names(subs)} -> {names(new[key])}")
            print(f"        {key[:150]}")

    # A block stranded above another fuses with it, and the fused text is
    # exactly the two originals joined. That concatenation is the signature, and
    # it is what separates a fusion from an ordinary expansion: an expanded
    # docstring also contains its old self, but the added prose is new text
    # rather than another block that used to stand on its own. Testing for it
    # exactly also means a fusion between two declarations of the *same* name —
    # F# overloads, of which this repository has several — is still caught, and
    # that each fused block is reported once rather than once per half.
    memo: dict[str, list[str] | None] = {}
    for key in new:
        if key in old:
            continue
        parts = split_into(key, old, memo)
        if parts and len(parts) > 1:
            bad += 1
            was = [names(old[k])[0] for k in parts]
            print(f"MERGED  {was} fused into one block, which documents {names(new[key])}")
            for k in parts:
                print(f"        {k[:120]}")

    print()
    if bad:
        print(f"docstring-attachment: {bad} block(s) changed subject", file=sys.stderr)
        return 1
    print(f"docstring-attachment: {len(old)} block(s) still document what they did at {ref}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
