#!/usr/bin/env python3
"""Fail if a WoofWare.PosixKernel source file names WoofWare.PawPrint or its symbols.

WoofWare.PosixKernel is meant to read as a POSIX simulator that knows nothing of
the client it was extracted from. `TestNoPawPrintReference` stops it *referencing*
the PawPrint assemblies; this stops its prose doing so, which the compiler cannot
see. Every `.fs` line is scanned -- code, docstrings, comments and string
literals alike -- because a `failwith` message naming `KernelConfig` sends a
client to a type it has never heard of just as surely as a docstring does.

The markers are the PawPrint names the library's prose had actually accumulated
when this check was written (`KernelConfig.X` as the way to set a field,
`EmulatedKernel` as where state lives, `NativeSystemNative` as who decides, and
so on), plus the project's own name. Each is matched as the start of an
identifier, so `EmulatedKernelDefect` is caught along with `EmulatedKernel`;
matching is case-sensitive. There is no allowlist: nothing in the library has a
reason to name them. Say "a client", "the caller", or name the library's own
function instead; if something genuinely belongs to PawPrint, move it there.

What it does not see, accepted deliberately:

  * A PawPrint concept described without its name ("the interpreter's handler
    for this entry point"). Prose is a judgement, not a regex.
  * CoreCLR's names (`SystemNative_*`, CoreLib). Those are not PawPrint's, and
    the library may legitimately contrast the kernel with what a runtime's shim
    does; only a mention that *justifies* a kernel fact by one is wrong, and
    that too is a judgement.
  * Non-`.fs` files. The README's history section names PawPrint on purpose.
  * Build output under `obj/` or `bin/`, such as the generated
    `AssemblyInfo.fs`, whose repository URL names PawPrint. A flake check sees
    only tracked files anyway; this is for running it in a working tree.

Usage:  check-pawprint-references.py <library-dir>
"""

import re
import sys
from pathlib import Path

MARKERS = [
    "PawPrint",
    "KernelConfig",
    "EmulatedKernel",
    "IlMachineState",
    "NativeSystemNative",
    "NativeEnvironment",
    "NativeDispatch",
    "HostConfig",
    "DirectoryStreamBlocks",
    "NextLowLevelMonitorId",
    "LowLevelMonitor",
    r"Program\.prepare",
    r"Program\.run",
]

PATTERN = re.compile(r"(?<![A-Za-z0-9_])(?:" + "|".join(MARKERS) + r")")


def main() -> int:
    if len(sys.argv) != 2:
        print(__doc__, file=sys.stderr)
        return 2

    library = Path(sys.argv[1])
    sources = sorted(
        path
        for path in library.rglob("*.fs")
        if not {"obj", "bin"} & set(path.relative_to(library).parts[:-1])
    )

    # A mistyped directory would otherwise pass vacuously.
    if not sources:
        print(f"PawPrint references: no .fs files under {library}", file=sys.stderr)
        return 2

    hits = []
    for source in sources:
        for number, line in enumerate(source.read_text().splitlines(), start=1):
            for match in PATTERN.finditer(line):
                hits.append(f"{source.relative_to(library)}:{number}: {match.group(0)}: {line.strip()}")

    if hits:
        print(
            f"PawPrint references: {len(hits)} in WoofWare.PosixKernel, which must not name its client.",
            file=sys.stderr,
        )
        for hit in hits:
            print(f"  {hit}", file=sys.stderr)
        return 1

    print(f"PawPrint references: none in {len(sources)} files.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
