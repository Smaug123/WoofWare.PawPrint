#!/usr/bin/env bash
# Executable contract for check-docstring-attachment.py.
#
# Builds a throwaway repository whose one commit exercises every shape the check
# has an opinion about, and asserts the report names exactly the seven that are
# detachments. The eight silent shapes are as much the point: a check that reports
# an ordinary docstring edit is a check nobody runs, and most of the eight were
# false positives at some stage of #1173, found by review rather than by reading.
# The exit status is asserted too, since that is what callers branch on.
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

/// Twin sentence.
let twinOne (x : int) : int = x

/// Twin sentence.
let twinTwo (x : int) : int = x

module Twins1 =
    /// Duplicated across modules.
    let sameName (x : int) : int = x

module Twins2 =
    /// Duplicated across modules.
    let sameName (x : int) : int = x

/// Body kept intact.
let prependee (x : int) : int = x

/// Doubled block.
let dblOne (x : int) : int = x

/// Doubled block.
let dblTwo (x : int) : int = x

/// Read the mask
let maskee (x : int) : int = x
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

/// Twin sentence.
/// Intruder detail.
let twinIntruder (x : int) : int = x

let twinOne (x : int) : int = x

/// Twin sentence.
/// Paragraph added.
let twinTwo (x : int) : int = x

module Twins1 =
    /// Duplicated across modules.
    /// Intruder in the first module.
    let dupIntruder (x : int) : int = x

    let sameName (x : int) : int = x

module Twins2 =
    /// Duplicated across modules.
    /// A paragraph only the second module wanted.
    let sameName (x : int) : int = x

/// Introductory line added above.
/// Body kept intact.
let prependee (x : int) : int = x

/// Body kept intact.
/// Quoted by an unrelated API.
let prependQuoter (x : int) : int = x

/// Doubled block.
/// Same added text.
let dblIntruder (x : int) : int = x

let dblOne (x : int) : int = x

/// Doubled block.
/// Same added text.
let dblTwo (x : int) : int = x

/// Read the masked value entirely
let maskee (x : int) : int = x

/// Read the mask
/// Quoted by an unrelated API.
let maskQuoter (x : int) : int = x
EOF

# The exit status is what callers branch on, so it is part of the contract: a
# regression that still prints the findings but stops reporting failure would
# pass every grep below.
set +e
report="$(python3 "$checker" HEAD A.fs 2>&1)"
status=$?
set -e

fail=0
if [ "$status" -ne 1 ]; then
  echo "STATUS   checker exited $status, expected 1 (findings present)"; fail=1
else
  echo "ok       checker exited 1, as a run with findings must"
fi

expect_reported () { # <subject the block now wrongly documents> <why this shape matters>
  if grep -q "'let $1'" <<<"$report"; then
    echo "ok       detachment onto $1 reported ($2)"
  else
    echo "MISSING  detachment onto $1 not reported ($2)"; fail=1
  fi
}
expect_silent () {
  if grep -q "'let $1'" <<<"$report"; then
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
expect_reported twinIntruder   "one of the two declarations a shared block documented has lost it"
expect_silent   twinTwo        "the other one kept it, inside a block it merely expanded"
expect_reported dupIntruder    "a block borne by two same-named declarations is kept by only one of them"
expect_reported dblIntruder    "the fused block also lands on a declaration that kept it, but another lost it"
expect_silent   prependQuoter  "the block it quotes is still inside its own subject's docstring, below a prepended line"
expect_silent   prependee      "prose prepended above a docstring leaves the docstring where it was"
expect_reported maskQuoter     "its subject's new docstring merely starts with the old block's characters, mid-word"

echo
if [ "$fail" -ne 0 ]; then
  echo "report was:"; echo "$report"
  exit 1
fi
echo "check-docstring-attachment: all fifteen shapes behave, and the exit status with them"
