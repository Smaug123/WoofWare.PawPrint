#!/usr/bin/env bash
# Verify that the file moves in this branch changed nothing but their namespace
# and `open` lines.
#
# This is the oracle for the WoofWare.PosixKernel extraction stages (see
# docs/plans/2026-08-23-posix-kernel-extraction.md). Those stages move thousands
# of lines between assemblies; a reviewer cannot read that diff, so the check has
# to say mechanically that there is nothing in it to read.
#
# Usage: scripts/check-move-is-rename-only.sh <base-ref> [extra-old:new pair]...
#
# Renames are discovered with `git diff -M`. A pair may also be given explicitly
# as old:new, which is how a *split* is checked -- when a file is divided in two,
# git detects no rename at all, so the caller states the correspondence.
#
# WHAT THIS CANNOT SEE, and so must be checked by hand:
#   * Code that depends on its own assembly identity. A file reading an embedded
#     resource through Assembly.GetExecutingAssembly() with a hard-coded logical
#     name is not a rename however identical its text, because the name changes
#     with the assembly. Grep the movers for GetExecutingAssembly.
#   * Project files: compile order, target framework, resource items.
#   * The consuming side. Added, removed and reordered `open` lines are exactly
#     what this strips, so a consumer whose name resolution changed looks clean
#     here.

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "usage: $0 <base-ref> [old:new]..." >&2
    exit 2
fi

base="$1"
shift

strip() {
    # Drop namespace and open lines; they are the only difference a move is
    # allowed to make. Everything else, including blank lines and indentation,
    # must match exactly.
    grep -vE '^[[:space:]]*(namespace|open)[[:space:]]' || true
}

failures=0
checked=0

check_pair() {
    local old="$1" new="$2"
    if ! git cat-file -e "${base}:${old}" 2>/dev/null; then
        echo "MISSING  ${old} does not exist at ${base}" >&2
        failures=$((failures + 1))
        return
    fi
    if [ ! -f "${new}" ]; then
        echo "MISSING  ${new} does not exist in the working tree" >&2
        failures=$((failures + 1))
        return
    fi
    checked=$((checked + 1))
    if diff -u <(git show "${base}:${old}" | strip) <(strip < "${new}") > /tmp/rename-check.$$ 2>&1; then
        echo "ok       ${old} -> ${new}"
    else
        echo "CHANGED  ${old} -> ${new}" >&2
        cat /tmp/rename-check.$$ >&2
        failures=$((failures + 1))
    fi
    rm -f /tmp/rename-check.$$
}

while IFS=$'\t' read -r status old new; do
    case "${status}" in
        R*) check_pair "${old}" "${new}" ;;
    esac
done < <(git diff -M --diff-filter=R --name-status "${base}")

for pair in "$@"; do
    check_pair "${pair%%:*}" "${pair##*:}"
done

echo
if [ "${failures}" -eq 0 ]; then
    echo "rename-only: ${checked} file(s) differ only in namespace/open lines"
else
    echo "rename-only: ${failures} file(s) changed beyond namespace/open lines" >&2
    exit 1
fi
