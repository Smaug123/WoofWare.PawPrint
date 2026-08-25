#!/usr/bin/env bash
# Executable contract for check-docstring-attachment.py.
#
# Builds a throwaway repository whose one commit exercises every shape the check
# has an opinion about, and asserts the report names exactly the three that are
# detachments. The five silent shapes are the point: a check that reports an
# ordinary docstring edit is a check nobody runs, and three of the five were
# false positives at some stage of #1173, found by review rather than by reading.
set -euo pipefail

checker="$(cd "$(dirname "$0")" && pwd)/check-docstring-attachment.py"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
cd "$tmp"

git init -q .
git config user.email t@example.invalid
git config user.name t
git config commit.gpgsign false

cat > A.fs <<'EOF'
module A

/// Places the rotation-th thread.
let cpuForRotation (r : int) : int = r

/// Gains a paragraph, keeps its subject.
let expanded (x : int) : int = x

/// Loses its subject to an undocumented intruder.
let displaced (x : int) : int = x

/// Shared opening sentence.
let keepsItsDocstring (x : int) : int = x

/// Prefix.
let alpha (x : int) : int = x

/// Prefix. Suffix.
let beta (x : int) : int = x

/// Shared trunk.
let trunkOwner (x : int) : int = x
EOF
git add -A
git commit -qm base

cat > A.fs <<'EOF'
module A

/// Places the rotation-th thread.
/// The error a thread would read.
let lastError (t : int) : int = t

let cpuForRotation (r : int) : int = r

/// Gains a paragraph, keeps its subject.
/// And here is the added paragraph.
let expanded (x : int) : int = x

/// Loses its subject to an undocumented intruder.
let intruder (x : int) : int = x

let displaced (x : int) : int = x

/// Shared opening sentence.
let keepsItsDocstring (x : int) : int = x

/// Shared opening sentence.
/// A distinct API that merely quotes it.
let quotesIt (x : int) : int = x

/// Prefix.
/// Suffix. Extra.
let nestedIntruder (x : int) : int = x

let alpha (x : int) : int = x

/// Prefix. Suffix.
let beta (x : int) : int = x

/// Shared trunk.
/// Detail for the owner.
let trunkOwner (x : int) : int = x

/// Shared trunk.
/// Detail for a different API.
let trunkQuoter (x : int) : int = x
EOF

report="$(python3 "$checker" HEAD A.fs 2>&1 || true)"

fail=0
expect_reported () { # <subject the block now wrongly documents> <why this shape matters>
  if grep -q "documents \['let $1'\]\|-> \['let $1'\]" <<<"$report"; then
    echo "ok       detachment onto $1 reported ($2)"
  else
    echo "MISSING  detachment onto $1 not reported ($2)"; fail=1
  fi
}
expect_silent () {
  if grep -q "let $1" <<<"$report"; then
    echo "SPURIOUS $1 reported, but nothing detached ($2)"; fail=1
  else
    echo "ok       $1 silent ($2)"
  fi
}

expect_reported intruder       "undocumented declaration inserted above the block's subject"
expect_reported lastError      "inserted declaration brought a docstring of its own, so the two fused"
expect_reported nestedIntruder "the fused block's longest old opening is a block still standing elsewhere"
expect_silent   expanded       "a paragraph appended to a docstring keeps its subject"
expect_silent   quotesIt       "a new docstring quoting one that is still in place detaches nothing"
expect_silent   keepsItsDocstring "the quoted block itself never moved"
expect_silent   trunkQuoter    "the opening it quotes is still on its own subject, inside that subject's expanded block"
expect_silent   trunkOwner     "expanding a docstring in place is not a detachment"

echo
if [ "$fail" -ne 0 ]; then
  echo "report was:"; echo "$report"
  exit 1
fi
echo "check-docstring-attachment: all eight shapes behave"
