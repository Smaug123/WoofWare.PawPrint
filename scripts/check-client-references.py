#!/usr/bin/env python3
"""Fail if a WoofWare.PosixKernel source file names a client: PawPrint or CoreCLR.

WoofWare.PosixKernel is meant to read as a POSIX simulator that knows nothing of
the client it was extracted from, nor of the runtime that client emulates.
`TestNoPawPrintReference` stops it *referencing* the PawPrint assemblies; this
stops its prose doing so, which the compiler cannot see. Every `.fs` line is
scanned -- code, docstrings, comments and string literals alike -- because a
`failwith` message naming `KernelConfig` sends a client to a type it has never
heard of just as surely as a docstring does.

Three kinds of marker, all matched case-sensitively and never mid-word:

  * PawPrint's names: the ones the library's prose had actually accumulated
    (`KernelConfig.X` as the way to set a field, `EmulatedKernel` as where
    state lives, `NativeSystemNative` as who decides, and so on), plus the
    project's own name. Each is matched as the start of an identifier, so
    `EmulatedKernelDefect` is caught along with `EmulatedKernel`.
  * CoreCLR's names: CoreCLR and CoreLib, its `System.Native` shim and that
    shim's `SystemNative_*` entry points, `Interop.*`, `SocketAsyncEngine`,
    the BCL and the PAL. A kernel fact is measured, so it never needs a
    runtime's behaviour to justify it, and a contrast with one is a fact about
    that runtime, which belongs in its client.
  * "guest", the client's word for the program it runs. To this library it is
    a process, or the caller of a syscall.

The last two are whole words, so `shimmer`, `PALETTE` and `guesthouse` are
silent, as is "a .NET string": the library is written in .NET, and its own API
takes and returns .NET values.

There is no allowlist: nothing in the library has a reason to name them. Say "a
client", "the caller", "the process", or name the library's own function
instead; if something genuinely belongs to a client, move it there.

What it does not see, accepted deliberately:

  * A client's concept described without its name ("the interpreter's handler
    for this entry point", "the managed caller"). Prose is a judgement, not a
    regex; "managed" and "runtime" are ordinary English too often to ban.
  * Non-`.fs` files. The README's history section names PawPrint on purpose.
  * Build output under `obj/` or `bin/`, such as the generated
    `AssemblyInfo.fs`, whose repository URL names PawPrint. A flake check sees
    only tracked files anyway; this is for running it in a working tree.

Usage:  check-client-references.py <library-dir>
"""

import re
import sys
from pathlib import Path

PAWPRINT_MARKERS = [
    "PawPrint",
    "KernelConfig",
    "EmulatedKernel",
    "IlMachineState",
    "NativeSystemNative",
    "NativeEnvironment",
    "NativeDispatch",
    "HostConfig",
    "DirectoryStreamFds",
    "NextLowLevelMonitorId",
    "LowLevelMonitor",
    r"Program\.prepare",
    r"Program\.run",
]

CORECLR_MARKERS = [
    "CoreCLR",
    "CoreLib",
    "SystemNative",
    r"System\.Native",
    "SocketAsyncEngine",
    r"Interop\.",
    r"BCL\b",
    r"PAL\b",
    r"shims?\b",
]

PROCESS_MARKERS = [
    r"[Gg]uests?\b",
]

MARKERS = PAWPRINT_MARKERS + CORECLR_MARKERS + PROCESS_MARKERS

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
        print(f"Client references: no .fs files under {library}", file=sys.stderr)
        return 2

    hits = []
    for source in sources:
        for number, line in enumerate(source.read_text().splitlines(), start=1):
            for match in PATTERN.finditer(line):
                hits.append(f"{source.relative_to(library)}:{number}: {match.group(0)}: {line.strip()}")

    if hits:
        print(
            f"Client references: {len(hits)} in WoofWare.PosixKernel, which must not name its client.",
            file=sys.stderr,
        )
        for hit in hits:
            print(f"  {hit}", file=sys.stderr)
        return 1

    print(f"Client references: none in {len(sources)} files.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
