#!/usr/bin/env python3
"""Is the connectSocket move content-preserving?

`scripts/check-move-is-rename-only.sh` cannot answer this: it compares whole
files across a git rename, and here a function moved between two files that both
still exist. So this does the same job by hand — take the block out of
`EmulatedKernel.fs` at the base ref, apply the stated mechanical substitutions,
and compare against what is in `UnixSystem.fs` now.

A clean run means every line of the 757 either matched, or differs only by a
substitution named in `transform.py` (whose own asserts pin how many times each
applies).
"""

import re
import subprocess
import sys

BASE = "origin/main"
OLD_FILE = "WoofWare.PawPrint/EmulatedKernel.fs"
NEW_FILE = "WoofWare.PosixKernel/UnixSystem.fs"
ROOT = subprocess.run(["git", "rev-parse", "--show-toplevel"],
                      capture_output=True, text=True, check=True).stdout.strip()


def show(ref, path):
    return subprocess.run(["git", "show", f"{ref}:{path}"], cwd=ROOT,
                          capture_output=True, text=True, check=True).stdout


def block(text, start_pred, end_pred):
    lines = text.split("\n")
    starts = [i for i, l in enumerate(lines) if start_pred(l)]
    assert len(starts) == 1, f"start matched {len(starts)} lines"
    ends = [i for i, l in enumerate(lines) if end_pred(l)]
    ends = [i for i in ends if i > starts[0]]
    assert ends, "no end line after start"
    return "\n".join(lines[starts[0]:ends[0]]).rstrip("\n")


old = block(
    show(BASE, OLD_FILE),
    lambda l: l.startswith("    /// `connect(2)` past the wrapper's screens"),
    lambda l: l.startswith("    /// `UnixSystem.acceptConnection`"),
)

new = block(
    open(f"{ROOT}/{NEW_FILE}").read(),
    lambda l: l.startswith("    /// `connect(2)` past the wrapper's screens"),
    lambda l: l.startswith("    /// Dequeue the oldest completed connection"),
)

# `signalSocketDataReady` moved with it: `connectSocket` was its only caller.
old_signal = block(
    show(BASE, OLD_FILE),
    lambda l: l.startswith("    /// A *data-ready* wake on `socketId`"),
    lambda l: l.startswith("    /// `SystemNative_TryChangeSocketEventRegistration` past the wrapper's"),
)

new_signal = block(
    open(f"{ROOT}/{NEW_FILE}").read(),
    lambda l: l.startswith("    /// A *data-ready* wake on `socketId`"),
    lambda l: l.startswith("    /// `connect(2)` past the wrapper's screens"),
)

# The substitutions, applied to the OLD text. Each is a claim about the move;
# if one stops applying, the counts below change and this reports it.
SUBS = [
    ("kernel.Sockets", "system.Machine.Sockets"),
    ("kernel.SoMaxConn", "system.Machine.SoMaxConn"),
    ("kernel.Connections", "system.Machine.Connections"),
    ("kernel.UnixPlatform", "system.Machine.UnixPlatform"),
    ("kernel.NextConnectionId", "system.Machine.NextConnectionId"),
    ("kernel.LocalRoutes", "system.Machine.LocalRoutes"),
    ("kernel.LocalAddresses", "system.Machine.LocalAddresses"),
    ("kernel.EphemeralPortRange", "system.Machine.EphemeralPortRange"),
    ("EmulatedKernelDefect.SocketPhaseKindMismatch exists to make this unreachable, so this is an interpreter bug.",
     "this kernel's socket invariants forbid that pairing, so this is a bug in the caller's state construction."),
    ("(kernel : EmulatedKernel)", "(system : UnixSystem<'Task, 'Handler>)"),
    ("EmulatedKernel", "UnixSystem<'Task, 'Handler>"),
    ("SystemNative_Connect:", "UnixSystem.connectSocket:"),
    ("Widen KernelConfig.EphemeralPortRange", "Widen the machine's EphemeralPortRange"),
    ("about guest memory, which this module", "about the client's memory, which this library"),
    ("let connectSocket", "let connectSocket<'Task, 'Handler when 'Task : comparison and 'Handler : equality>"),
]

expected = old
for a, b in SUBS:
    expected = expected.replace(a, b)


def rename_in_code(text):
    out = []
    for line in text.split("\n"):
        if line.lstrip().startswith("//"):
            out.append(line)
            continue
        parts = line.split('"')
        for i in range(0, len(parts), 2):
            parts[i] = re.sub(r"\bkernel\b", "system", parts[i])
        out.append('"'.join(parts))
    return "\n".join(out)


expected = rename_in_code(expected)


def normalise(text):
    """Collapse whitespace: fantomas re-wraps, and re-wrapping is not content."""
    return re.sub(r"\s+", " ", text).strip()


expected_signal = old_signal
for a, b in SUBS:
    expected_signal = expected_signal.replace(a, b)
expected_signal = rename_in_code(expected_signal)
# The one difference beyond the shared substitutions: `EmulatedKernel` had a
# forwarding member for the descriptor table and `UnixSystem` does not.
expected_signal = expected_signal.replace("system.FileDescriptors", "system.Process.FileDescriptors")
# ...and the library states its generics explicitly, as every function here does.
expected_signal = expected_signal.replace(
    "let signalSocketDataReady",
    "let signalSocketDataReady<'Task, 'Handler when 'Task : comparison and 'Handler : equality>")

ok = True
if normalise(expected_signal) != normalise(new_signal):
    print("DIVERGES: signalSocketDataReady")
    print("  expected:", normalise(expected_signal))
    print("  actual  :", normalise(new_signal))
    ok = False

if ok and normalise(expected) == normalise(new):
    print(f"connectSocket move: content-preserving ({old.count(chr(10)) + 1} lines at {BASE}).")
    print(f"signalSocketDataReady move: content-preserving ({old_signal.count(chr(10)) + 1} lines).")
    sys.exit(0)
if not ok:
    sys.exit(1)

# Report the first divergence at word granularity.
ew = normalise(expected).split(" ")
nw = normalise(new).split(" ")
for i, (a, b) in enumerate(zip(ew, nw)):
    if a != b:
        print("DIVERGES at word", i)
        print("  expected:", " ".join(ew[max(0, i - 12):i + 12]))
        print("  actual  :", " ".join(nw[max(0, i - 12):i + 12]))
        break
else:
    print(f"length differs: expected {len(ew)} words, actual {len(nw)}")
sys.exit(1)
