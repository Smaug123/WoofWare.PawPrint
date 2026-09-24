#!/usr/bin/env bash
# Executable contract for check-pawprint-references.py.
#
# Builds a throwaway library directory holding every shape the check has an
# opinion about, and asserts the report names exactly the lines that name
# PawPrint, in code, docstrings, comments and strings alike. The silent shapes
# are as much the point: a check that fires on `WoofWare.PosixKernel` or on the
# word "program" is a check nobody keeps. The exit status is asserted too, since
# that is what the flake check branches on.
set -euo pipefail

# The checker is normally its sibling; the flake check passes it in from the
# store, where the two have no directory in common.
checker="${1:-$(cd "$(dirname "$0")" && pwd)/check-pawprint-references.py}"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

mkdir -p "$tmp/lib/Nested"

cat > "$tmp/lib/A.fs" <<'EOF'
namespace WoofWare.PosixKernel

/// Set this with `KernelConfig.Umask`.
let docstring = 1
// Held on `EmulatedKernel` for now.
let comment = 2
let message () = failwith "see NativeSystemNative.fs (this is a PawPrint bug)"
let code = IlMachineState.foo
/// Checked by `EmulatedKernelDefect.DirectoryStreamBlockDangling`.
let prefix = 3
/// Applied by `Program.prepare` before the `.cctor` runs.
let program = 4
/// The program. A Program is not a PawPrint name, and neither is `Program.fs`.
let silentProgram = 5
/// Lives in WoofWare.PosixKernel, as does `UnixMachineState.withUmask`.
let silentLibrary = 6
/// Lower case pawprint is not the identifier; nor is aPawPrint mid-word.
let silentCase = 7
EOF

cat > "$tmp/lib/Nested/B.fs" <<'EOF'
module B
/// `NativeEnvironment` asserts it; `HostConfig` launches it.
// A `DIR*` is keyed through DirectoryStreamBlocks.
let nested = 1
EOF

cat > "$tmp/lib/README.md" <<'EOF'
Extracted from WoofWare.PawPrint, whose KernelConfig configures it.
EOF

mkdir -p "$tmp/lib/obj/Debug"
cat > "$tmp/lib/obj/Debug/Generated.fs" <<'EOF'
// RepositoryUrl https://github.com/Smaug123/WoofWare.PawPrint
EOF

set +e
out="$(python3 "$checker" "$tmp/lib" 2>&1)"
status=$?
set -e

expected_hits=(
  "A.fs:3: KernelConfig:"
  "A.fs:5: EmulatedKernel:"
  "A.fs:7: NativeSystemNative:"
  "A.fs:7: PawPrint:"
  "A.fs:8: IlMachineState:"
  "A.fs:9: EmulatedKernel:"
  "A.fs:11: Program.prepare:"
  "A.fs:13: PawPrint:"
  "Nested/B.fs:2: NativeEnvironment:"
  "Nested/B.fs:2: HostConfig:"
  "Nested/B.fs:3: DirectoryStreamBlocks:"
)

fail=0
if [ "$status" -ne 1 ]; then
  echo "FAIL: expected exit 1 on a library with references, got $status" >&2
  fail=1
fi
for hit in "${expected_hits[@]}"; do
  if ! grep -qF "  $hit" <<<"$out"; then
    echo "FAIL: not reported: $hit" >&2
    fail=1
  fi
done
reported="$(grep -c '^  ' <<<"$out" || true)"
if [ "$reported" -ne "${#expected_hits[@]}" ]; then
  echo "FAIL: expected ${#expected_hits[@]} hits, got $reported" >&2
  fail=1
fi

# Line 13 is the one line that mixes silent and reported shapes: `PawPrint` there
# is a real reference, so the line is reported once, for that token alone.
if grep -qE 'A\.fs:(4|6|10|12|14|15|16|17|18): ' <<<"$out"; then
  echo "FAIL: a silent line was reported" >&2
  fail=1
fi
if grep -q 'README' <<<"$out"; then
  echo "FAIL: a non-.fs file was scanned" >&2
  fail=1
fi
if grep -q 'Generated' <<<"$out"; then
  echo "FAIL: build output under obj/ was scanned" >&2
  fail=1
fi

# A clean library passes, and says how much it read.
mkdir -p "$tmp/clean"
printf 'module C\n/// A client sets this with `UnixProcessState.withUmask`.\nlet c = 1\n' > "$tmp/clean/C.fs"
if ! python3 "$checker" "$tmp/clean" | grep -q 'none in 1 files'; then
  echo "FAIL: a clean library was not reported clean" >&2
  fail=1
fi

# A directory with no sources is a usage error, not a pass.
mkdir -p "$tmp/empty"
set +e
python3 "$checker" "$tmp/empty" >/dev/null 2>&1
empty_status=$?
set -e
if [ "$empty_status" -ne 2 ]; then
  echo "FAIL: expected exit 2 on a directory with no .fs files, got $empty_status" >&2
  fail=1
fi

if [ "$fail" -ne 0 ]; then
  echo "--- checker output ---" >&2
  echo "$out" >&2
  exit 1
fi
echo "pawprint-references contract: ${#expected_hits[@]} references reported, silent shapes silent"
