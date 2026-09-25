#!/usr/bin/env bash
# Executable contract for check-docstring-attachment.py.
#
# Builds a throwaway repository whose one commit exercises every shape the check
# has an opinion about, and asserts the report names exactly the eighteen that are
# detachments. The eighteen silent shapes are as much the point: a check that reports
# an ordinary docstring edit is a check nobody runs. Most of the first eight were
# false positives at some stage of #1173, found by review rather than by reading;
# the renames and shared one-liners were found by sweeping the checker over three
# hundred of main's commits, each against its parent.
# The exit status is asserted too, since that is what callers branch on.
set -euo pipefail

# The checker is normally its sibling; the flake check passes each in from the
# store, where they have no directory in common.
checker="${1:-$(cd "$(dirname "$0")" && pwd)/check-docstring-attachment.py}"
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

/// Renamed along with its docstring.
let oldName (x : int) : int = x

/// Renamed, and its docstring extended.
let oldExtended (x : int) : int = x

/// Reused one-liner.
let reuseOne (x : int) : int = x

/// Shared by two, one of which is deleted.
let sharedKept (x : int) : int = x

/// Shared by two, one of which is deleted.
let sharedDeleted (x : int) : int = x

/// Shared by two, one of which is reworded.
let rewordKept (x : int) : int = x

/// Shared by two, one of which is reworded.
let reworded (x : int) : int = x

/// Belongs to movedAway, wherever it goes.
let movedAway (x : int) : int = x

type Measured =
    {
        /// The size of the thing measured.
        NativeSize : int
    }

/// Deleted, leaving this behind.
let deletedAbove (x : int) : int = x

let undocumentedNeighbour (x : int) : int = x

/// Documents an exception, a form the declaration regex does not know.
exception FirstError of string

/// Split: this prose follows the implementation to a new, general function.
let splitOld (x : int) : int = x

/// Prose that should have stayed with its subject.
let keeper (x : int) : int = x

/// Shared by one that is renamed and one that is split.
let renamedA (x : int) : int = x

/// Shared by one that is renamed and one that is split.
let splitB (x : int) : int = x

type Overloads () =
    /// The int overload.
    member _.Foo (x : int) : int = x

    /// The string overload.
    member _.Foo (x : string) : int = 0

module ReadsA =
    /// Reads from the first source.
    let read (x : int) : int = x

module ReadsB =
    /// Returns the count.
    let read (x : int) : int = x

type Counted =
    {
        /// The count, in a field the declaration regex does not know.
        _Count : int
    }

type Holder =
    {
        /// Documents the field that is deleted.
        Gone : int
        _Held : int
    }

/// Pins a behaviour, under a name that is about to change.
let ``the old test name`` () = ()

/// The current offset.
let position (x : int) : int = x

type Attributed () =
    /// The int overload, with an attributed parameter.
    member _.Baz ([<Param(Name = "value")>] x : int) : int = x

    /// The string overload, with an attributed parameter.
    member _.Baz ([<Param(Name = "value")>] x : string) : int = 0

/// Deleted from the end of the file, leaving this.
let deletedLast (x : int) : int = x
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

/// Renamed along with its docstring.
let newName (x : int) : int = x

/// Renamed, and its docstring extended.
/// With a paragraph the rename brought.
let newExtended (x : int) : int = x

/// Reused one-liner.
let reuseOne (x : int) : int = x

/// Reused one-liner.
let reuseTwo (x : int) : int = x

/// Shared by two, one of which is deleted.
let sharedKept (x : int) : int = x

/// Shared by two, one of which is reworded.
let rewordKept (x : int) : int = x

/// Given prose of its own.
let reworded (x : int) : int = x

/// Belongs to movedAway, wherever it goes.
let replacement (x : int) : int = x

type Measured =
    {
        Raw : int
    }

    /// The size of the thing measured.
    member this.NativeSize = this.Raw

/// Deleted, leaving this behind.
let undocumentedNeighbour (x : int) : int = x

/// Documents an exception, a form the declaration regex does not know.
let exceptionIntruder (x : int) : int = x

exception FirstError of string

/// Split: this prose follows the implementation to a new, general function.
/// With a paragraph about the generality.
let splitGeneral (y : int) (x : int) : int = x + y

/// `splitGeneral` with no offset.
let splitOld (x : int) : int = splitGeneral 0 x

/// Prose that should have stayed with its subject.
let keep (x : int) : int = x

/// Counts the keepers.
let keeper (x : int) : int = x

/// Shared by one that is renamed and one that is split.
let splitC (y : int) (x : int) : int = x + y

/// Shared by one that is renamed and one that is split.
let renamedD (x : int) : int = x

/// `splitC` with no offset.
let splitB (x : int) : int = splitC 0 x

type Overloads () =
    /// The int overload.
    member _.Bar : int = 0

    member _.Foo (x : int) : int = x + 1

    /// The string overload.
    member _.Foo (x : string) : int = 0

module ReadsA =
    /// Reads from the first source.
    let count (x : int) : int = x

    let read (x : int) : int = x

module ReadsB =
    /// Returns the count.
    let read (x : int) : int = x

type Counted =
    {
        /// The count, in a field the declaration regex does not know.
        Extra : bool
        _Count : int64
    }

type Holder =
    {
        /// Documents the field that is deleted.
        _Held : int64
    }

/// Pins a behaviour, under a name that is about to change.
let ``the new test name`` () = ()

/// The current offset.
let rewind () = 1

/// The current offset after the last rewind.
let position (x : int) : int = x

type Attributed () =
    member _.Baz ([<Param(Name = "value")>] x : int) : int = x

    /// The int overload, with an attributed parameter.
    member _.Baz ([<Param(Name = "value")>] x : string) : int = 0

/// Deleted from the end of the file, leaving this.
EOF

cat > B.fs <<'EOF'
module B

let movedAway (x : int) : int = x
EOF

# The exit status is what callers branch on, so it is part of the contract: a
# regression that still prints the findings but stops reporting failure would
# pass every grep below.
# One record per detachment listed below, and no more.
expected=18

set +e
report="$(python3 "$checker" HEAD A.fs B.fs 2>&1)"
status=$?
set -e

fail=0
if [ "$status" -ne 1 ]; then
  echo "STATUS   checker exited $status, expected 1 (findings present)"; fail=1
else
  echo "ok       checker exited 1, as a run with findings must"
fi

# A crash after the findings are printed also exits 1, and extra findings for
# declarations nobody listed below would go unread, so pin the summary line the
# checker prints last and the number of records it counted.
found="$(grep -cE '^(MOVED|MERGED)' <<<"$report" || true)"
if grep -qx "docstring-attachment: $expected block(s) changed subject" <<<"$report"; then
  echo "ok       checker ran to completion and counted $expected findings"
else
  echo "SUMMARY  no 'docstring-attachment: $expected block(s) changed subject' line; it crashed, or found a different number"; fail=1
fi
if [ "$found" -eq "$expected" ]; then
  echo "ok       exactly $expected findings printed, so none is unaccounted for"
else
  echo "COUNT    $found MOVED/MERGED records printed, expected $expected"; fail=1
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
expect_silent   newName        "a declaration renamed with its docstring: the old name is gone and the new one is new"
expect_silent   newExtended    "a rename that also added a paragraph to its docstring"
expect_silent   reuseTwo       "a new declaration reusing a one-liner that its first holder keeps"
expect_silent   sharedKept     "one of two declarations sharing a one-liner is deleted, and the other keeps it"
expect_silent   rewordKept     "one of two declarations sharing a one-liner is given prose of its own"
expect_silent   splitGeneral   "the implementation moved to a new function with its prose, and the old name was given prose of its own"
expect_silent   splitC         "a split beside a rename of another holder of the same one-liner, whichever is declared first"
expect_silent   renamedD       "a rename beside a split of another holder of the same one-liner, whichever is declared first"
expect_reported replacement    "a definition moved to another file without its docstring, which a new declaration picked up"
expect_reported undocumentedNeighbour "a definition deleted, and its docstring left on the undocumented declaration below it"
expect_reported exceptionIntruder "inserted above an unparsed declaration form, which still exists and so was not renamed"
expect_reported keep           "the displaced declaration's new prose names it only inside a longer word, so it is no split"
expect_reported count          "the displaced declaration has no new prose; a same-named one elsewhere merely mentions it"
expect_reported rewind         "the displaced declaration's new prose uses the intruder's name as an ordinary word, not a reference"
if grep -q "The int overload, with an attributed parameter" <<<"$report"; then
  echo "ok       Baz reported (overloads whose parameters carry an attribute with an = are still told apart)"
else
  echo "MISSING  Baz not reported (overloads whose parameters carry an attribute with an = are still told apart)"; fail=1
fi
if grep -q "'field Extra'" <<<"$report"; then
  echo "ok       Extra reported (an unparsed subject whose line changed has not been renamed)"
else
  echo "MISSING  Extra not reported (an unparsed subject whose line changed has not been renamed)"; fail=1
fi
if grep -q "_Held : int64" <<<"$report"; then
  echo "ok       _Held reported (a deleted field's docstring stranded on an unparsed neighbour whose line changed is no rename)"
else
  echo "MISSING  _Held not reported (a deleted field's docstring stranded on an unparsed neighbour whose line changed is no rename)"; fail=1
fi
if grep -q "the new test name" <<<"$report"; then
  echo "SPURIOUS the new test name reported, but a double-backticked name renamed with its docstring detaches nothing"; fail=1
else
  echo "ok       the new test name silent (a double-backticked name renamed with its docstring detaches nothing)"
fi
if grep -q "'let deletedLast'\] -> \['<none>'\]" <<<"$report"; then
  echo "ok       deletedLast reported (a definition deleted from under its docstring is not a rename to nothing)"
else
  echo "MISSING  deletedLast not reported (a definition deleted from under its docstring is not a rename to nothing)"; fail=1
fi
if grep -q "'member Bar'" <<<"$report"; then
  echo "ok       Bar reported (an overload displaced while its body changed is still the same overload, not renamed)"
else
  echo "MISSING  Bar not reported (an overload displaced while its body changed is still the same overload, not renamed)"; fail=1
fi
if grep -q "NativeSize" <<<"$report"; then
  echo "SPURIOUS NativeSize reported, but a field that became a same-named member kept its docstring"; fail=1
else
  echo "ok       NativeSize silent (a field that became a same-named member kept its docstring)"
fi

echo
if [ "$fail" -ne 0 ]; then
  echo "report was:"; echo "$report"
  exit 1
fi
echo "check-docstring-attachment: all thirty-six shapes behave, and the exit status and finding count with them"
