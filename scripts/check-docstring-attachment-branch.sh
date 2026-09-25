#!/usr/bin/env bash
# Run check-docstring-attachment.py over every .fs file a change touches, against
# the commit the change starts from.
#
# Usage: scripts/check-docstring-attachment-branch.sh [base-commit]
#
# The base defaults to the merge base of HEAD and origin/main, which is what a
# branch should be checked against before it is pushed. CI passes the base
# explicitly. The working tree is compared, not HEAD, so uncommitted and
# untracked files count.
#
# The checker needs the union of the paths on *both* sides of a move, so a file
# that was deleted, or renamed away, is passed as well as the one that received
# its contents (`--no-renames` lists both halves of a rename). A change none of
# whose .fs files had a docstring at the base has nothing to compare against,
# and passes.
#
# Exit status: 0 when nothing changed subject, 1 when something did, 2 when the
# comparison could not be made. Every failure to enumerate or read is a 2: a
# wrapper that passes when it compared nothing retires the check silently.
#
# DOCSTRING_ATTACHMENT_CHECKER overrides where the checker is found, for callers
# (the flake check) that do not keep the two scripts side by side.
set -euo pipefail

checker="${DOCSTRING_ATTACHMENT_CHECKER:-$(cd "$(dirname "$0")" && pwd)/check-docstring-attachment.py}"
# git names files relative to the repository root and the checker reads them
# relative to its working directory, so the two must agree; from a subdirectory
# the pathspec below would also see only that subtree.
cd "$(git rev-parse --show-toplevel)"
base="${1:-$(git merge-base origin/main HEAD)}"

if ! git rev-parse --verify --quiet "$base^{commit}" > /dev/null; then
    echo "docstring-attachment: $base does not name a commit" >&2
    exit 2
fi

# Written to a file rather than read through a process substitution, whose exit
# status nothing observes: a `git diff` that fails would otherwise read as no
# change. NUL-delimited, because git quotes a name with a non-ASCII byte in it
# unless asked not to, and a quoted name is not a path.
listing="$(mktemp)"
trap 'rm -f "$listing"' EXIT
git diff -z --no-renames --name-only "$base" -- '*.fs' > "$listing" || exit 2
git ls-files -z --others --exclude-standard -- '*.fs' >> "$listing" || exit 2
sort -z -u -o "$listing" "$listing" || exit 2

files=()
while IFS= read -r -d '' f; do
    files+=("$f")
done < "$listing"
rm -f "$listing"

if [ "${#files[@]}" -eq 0 ]; then
    echo "docstring-attachment: no .fs file changed since $base"
    exit 0
fi

documented=0
for f in "${files[@]}"; do
    # A file absent at the base (new, or untracked) has no docstring there.
    git cat-file -e "$base:$f" 2> /dev/null || continue
    # Read whole before matching: `grep -q` stops at the first match, and a
    # `git show` still writing into the closed pipe dies of SIGPIPE, which
    # `pipefail` turns into "no match" for exactly the files that have one.
    content="$(git show "$base:$f")" || exit 2
    if grep -q '^[[:space:]]*///' <<< "$content"; then
        documented=1
        break
    fi
done

if [ "$documented" -eq 0 ]; then
    echo "docstring-attachment: no changed .fs file had a docstring at $base, so none can have moved"
    exit 0
fi

exec python3 "$checker" "$base" "${files[@]}"
