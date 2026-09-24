#!/usr/bin/env bash
# Executable contract for check-docstring-attachment-branch.sh, the wrapper CI
# runs: which files it hands the checker, and that it never passes without
# having compared anything.
#
# Usage: scripts/test-docstring-attachment-branch.sh [wrapper] [checker]
set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
wrapper="${1:-$here/check-docstring-attachment-branch.sh}"
export DOCSTRING_ATTACHMENT_CHECKER="${2:-$here/check-docstring-attachment.py}"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
cd "$tmp"

git init -q .
git config user.email t@example.invalid
git config user.name t
git config commit.gpgsign false

fail=0
expect () { # <expected status> <description> [wrapper args...]
    local want="$1" what="$2"
    shift 2
    set +e
    out="$(bash "$wrapper" "$@" 2>&1)"
    got=$?
    set -e
    if [ "$got" -eq "$want" ]; then
        echo "ok       exit $got: $what"
    else
        echo "WRONG    exit $got, expected $want: $what"
        # shellcheck disable=SC2001
        echo "$out" | sed 's/^/         /'
        fail=1
    fi
}

# A documented declaration near the top of a file large enough that a reader
# stopping at the first docstring leaves `git show` writing into a closed pipe.
{
    echo "module Big"
    echo
    echo "/// Documents the first declaration."
    echo "let first (x : int) : int = x"
    for i in $(seq 1 20000); do
        echo "let filler$i (x : int) : int = x + $i"
    done
} > Big.fs
echo "module Plain" > Plain.fs
echo "let undocumented = 1" >> Plain.fs
git add -A
git commit -qm base
base="$(git rev-parse HEAD)"

expect 2 "a base that names no commit is refused, not read as no change" not-a-real-base
expect 0 "nothing changed" "$base"

echo "let added = 2" >> Plain.fs
expect 0 "the only changed file had no docstring at the base" "$base"
git checkout -q -- Plain.fs

cat > New.fs <<'EOF'
module New

/// Brand new.
let brandNew = 1
EOF
expect 0 "the only changed file is new, and untracked" "$base"
rm New.fs

python3 - <<'PY'
from pathlib import Path
p = Path("Big.fs")
s = p.read_text()
p.write_text(s.replace("/// Documents the first declaration.\n", "/// Documents the first declaration.\nlet intruder = 0\n", 1))
PY
expect 1 "a detachment in a file too large to read through a pipe that closes early" "$base"
git add -A
git commit -qm detach
expect 1 "the same detachment, committed" "$base"

git mv Big.fs Moved.fs
expect 1 "the detachment is still seen when its file is renamed away" "$base"

mkdir -p sub
touch sub/.keep
(cd sub && expect 1 "the detachment is still seen when run from a subdirectory" "$base")

git reset -q --hard "$base"
rm -rf sub
printf 'module Cafe\n\n/// Documents the one declaration.\nlet only (x : int) : int = x\n' > Café.fs
git add -A
git commit -qm cafe
cafe="$(git rev-parse HEAD)"
printf 'module Cafe\n\n/// Documents the one declaration.\nlet intruder = 0\nlet only (x : int) : int = x\n' > Café.fs
expect 1 "a detachment in a file whose name git would quote" "$cafe"

echo
if [ "$fail" -ne 0 ]; then
    exit 1
fi
echo "check-docstring-attachment-branch: every case behaves"
