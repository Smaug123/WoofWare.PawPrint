#!/usr/bin/env python3
"""Pin the set of WoofWare.PosixKernel definitions that speak CoreCLR's PAL.

WoofWare.PosixKernel is meant to be a POSIX simulator that a client other than
WoofWare.PawPrint could use, so its vocabulary should be POSIX's: raw errno
numbers, `SIGTERM`, `AF_INET`. That is now true -- the allowlist is empty, and
all four clusters this was written to retire have gone to WoofWare.PawPrint
(`UnixErrorPal`, `SocketEventsPal`, `SocketArgumentsPal`, `PosixSignalPal`).

So this is a ratchet rather than a countdown. Every marker below names an
encoding *nothing* in the library speaks any more, and the check's whole job is
to notice if one comes back.

A definition is taken to speak the PAL if its own name says so, or if its body
mentions one of the PAL's encodings. That is a proxy rather than a proof: a
PAL-encoded `int` is indistinguishable from any other `int`.

What it does not see, accepted deliberately:

  * PAL vocabulary introduced as a discriminated-union case (`| ToPal of int`),
    as a `static member`, or as an instance member (`member _.ToPal`, whose name
    this does not parse).
  * A new definition that merely *delegates* to an allowlisted one, such as
    `let managedError e = UnixError.toPal e`. Detecting those means a transitive
    closure over call sites, which is a bigger tool than this.
  * A bare numeric table with no PAL word anywhere in it. This used to be
    largely covered by the library's PAL constants living in one
    `module private Pal`, so that an adapter essentially had to write `Pal.` to
    exist at all; stage 9h moved that module out, so the socket numbering is now
    guarded by the name markers alone -- weaker than the distinctive word that
    guards the retired `SocketEvents` cluster. A re-accreted `AF_*` table under
    an innocent name would pass.
  * Prose, and this is the one that actually keeps happening. Docstrings are
    excluded before the body scan, so a declaration *documented* in the PAL's
    terms reads as clean however it is defined. Each of the last three stages
    found an instance while retiring its cluster, and each would have turned
    this check green over a residue it cannot see:
      - 9g: `SocketEventInterest`'s fields were the PAL's five bits by name.
      - 9h: `SocketDomain` and siblings were POSIX-named and PAL-defined
        ("`AF_INET`, PAL 2").
      - 9i: `Signal.Other` claimed to carry "the raw managed `PosixSignal`
        enum value", which it never did.
    Three for three says this is how residue comes back. It is *not* fixable
    here: the check would have to tell "cites the PAL to explain where a POSIX
    value came from", which the header above explicitly blesses, from "defines
    this thing in the PAL's terms". That is a judgement, not a regex, and
    pretending otherwise would make this dishonest in a new way. Read the prose
    when you retire something.

And one thing it can see for a weak reason: a conversion whose body is bare hex
masks is recognised only if a *string literal* in it names the encoding, which
is how `SocketEventInterest.ofBits` was caught until stage 9g retired it.
Rewording such a message reports the entry as stale rather than removing the
conversion; the stale message below says so.

None of these is closed because the library is written in module-and-`let` style
throughout, so the accretion this guards against would have to arrive in a form
nothing here uses. Widen the parser if that stops being true.

Usage:  check-pal-residue.py <library-dir> <allowlist-file>

Both arguments may be store paths (this runs as a flake check), so messages name
the allowlist by its place in the repository rather than by the path passed in.
Exit 0 when the detected set is exactly the allowlist; 1 otherwise.
"""

import re
import sys
from pathlib import Path

# A definition whose *name* contains one of these is speaking the PAL by its own
# admission. `pal` is matched on camelCase boundaries, so `Pal`, `toPal`,
# `palSuccess` and `addressFamilyPalToPlatform` would hit while `Palette` and
# `principal` do not.
NAME_MARKERS = [
    re.compile(r"(?<![A-Za-z])pal(?![a-z])"),
    re.compile(r"Pal(?![a-z])"),
    re.compile(r"PosixSignalEnum"),
]

# A definition whose *body* mentions one of these is reading or writing a PAL
# encoding. Comment lines are excluded before this runs: docstrings citing the
# PAL to explain where a POSIX value came from are documentation, not vocabulary.
BODY_MARKERS = re.compile(
    r"""(?<![A-Za-z0-9_])Pal\.       # the `module private Pal` constant table
       |\bPal\s*=                    # constructing the PAL column
       |\.Pal\b                      # reading the PAL column
       |SocketEvents                 # the PAL's socket-event bit names
       |PosixSignal                  # the managed enum, in code rather than prose
       |\b(?:portable|platformDependent)\s+0x   # a row of the errno table below
    """,
    re.VERBOSE,
)

# `let`/`member`/`module` bindings, and record fields -- which is how
# `UnixErrorNumbering.Pal`, a PAL number in a public record, gets seen. Union
# cases are not parsed; see the note above.
ALLOWLIST_IN_REPO = "scripts/pal-residue-allowlist.txt"

BINDING = re.compile(
    r"^(\s*)(let|member|module|type)\s+"
    r"(?:private\s+|internal\s+|public\s+|rec\s+|mutable\s+|inline\s+)*"
    r"([A-Za-z_][A-Za-z0-9_']*)"
)
FIELD = re.compile(r"^(\s*)\|?\s*([A-Z][A-Za-z0-9_']*)\s*:")


def is_comment(line: str) -> bool:
    return line.lstrip().startswith("//")


def qualify(stack: list[tuple[int, str]]) -> str:
    """Name a definition by its top-level container and the member inside it.

    That is the granularity the allowlist wants: a helper nested inside a
    function is part of that function rather than a separate entry to approve,
    while two functions of one module are two independent things.
    """
    return ".".join(name for _, name in stack[:2])


def scan(path: Path) -> set[str]:
    """Return the qualified name of each PAL-speaking definition."""
    lines = path.read_text().split("\n")
    stack: list[tuple[int, str]] = []  # (indent, name), outermost first
    found: set[str] = set()

    for line in lines:
        if not line.strip():
            continue

        binding = BINDING.match(line)
        field = None if binding else FIELD.match(line)
        match = binding or field
        if match:
            indent = len(match.group(1))
            name = match.group(3) if binding else match.group(2)
            while stack and stack[-1][0] >= indent:
                stack.pop()
            stack.append((indent, name))
            if any(m.search(name) for m in NAME_MARKERS):
                found.add(qualify(stack))

        if is_comment(line):
            continue
        if BODY_MARKERS.search(line) and stack:
            found.add(qualify(stack))

    return found


def read_allowlist(path: Path) -> set[str]:
    entries = set()
    for raw in path.read_text().split("\n"):
        line = raw.split("#", 1)[0].strip()
        if line:
            entries.add(line)
    return entries


def main() -> int:
    if len(sys.argv) != 3:
        print(__doc__, file=sys.stderr)
        return 2
    library, allowlist_path = Path(sys.argv[1]), Path(sys.argv[2])

    detected = set()
    # Recursive: the library is flat today, but a file added under a new
    # subdirectory must not escape the check by being somewhere unexpected.
    for source in sorted(library.rglob("*.fs")):
        for name in scan(source):
            detected.add(f"{source.relative_to(library)}:{name}")

    allowed = read_allowlist(allowlist_path)
    grew = sorted(detected - allowed)
    stale = sorted(allowed - detected)

    for name in grew:
        print(
            f"PAL residue grew: {name} speaks WoofWare.PawPrint's PAL encoding, but "
            f"WoofWare.PosixKernel's vocabulary is meant to be POSIX's.",
            file=sys.stderr,
        )
    if grew:
        print(
            "\nPut the conversion in WoofWare.PawPrint, beside UnixErrorPal, "
            "SocketEventsPal, SocketArgumentsPal and PosixSignalPal in Native/, "
            "and give the library the POSIX value. The residue is currently "
            f"empty and {ALLOWLIST_IN_REPO} says what it takes to add to it; "
            "docs/plans/2026-08-23-posix-kernel-extraction.md is why it is "
            "empty.",
            file=sys.stderr,
        )

    for name in stale:
        print(
            f"PAL residue allowlist is stale: {name} is listed in "
            f"{ALLOWLIST_IN_REPO} but no longer detected. If it stopped "
            "converting PAL values, delete the line -- that is the win. If it "
            "still converts them and the detector merely stopped seeing it "
            "(some entries are recognised only by a word in a diagnostic "
            "message), fix the detector instead.",
            file=sys.stderr,
        )

    if not grew and not stale:
        if detected:
            print(f"PAL residue: {len(detected)} definitions, exactly as allowlisted.")
        else:
            print("PAL residue: none, and the allowlist is empty.")
        return 0
    return 1


if __name__ == "__main__":
    sys.exit(main())
