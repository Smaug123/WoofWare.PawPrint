#!/usr/bin/env python3
"""Report docstrings whose subject changed between a base revision and the tree.

An F# `///` block binds to the declaration that *follows* it. Move a definition
to another file without taking its docstring and the prose stays behind, silently
re-binding to whatever now follows; insert a declaration between a docstring and
its subject and the same thing happens in place. The compiler cannot see either,
the tests cannot see either, and the diff looks local and reasonable in both
cases. This is the oracle for that, and it is the companion to
check-move-is-rename-only.sh: that one says the moved text is unchanged, this one
says the moved text still documents the same thing.

Usage: scripts/check-docstring-attachment.py <base-ref> <file>...

Pass the union of the paths involved on *both* sides, since a docstring that
moves between files is only recognised as unmoved when both ends are in the list.
The output names each block by its first 150 characters, which is enough to grep
for.

Two shapes are reported:
  MOVED   the same block now precedes a differently-named declaration
  MERGED  the block's text survives only inside a larger block, which is what
          happens when a stranded docstring abuts the next one and the two fuse.
          The larger block's second half need not have existed before: inserting
          a declaration that brings its own, freshly-written docstring is the
          commonest way to strand one, and is reported too

Three ways prose deliberately changes subject are not reported:
  * A rename. The declaration takes its docstring to a name that did not exist
    before, and the name it had exists nowhere afterwards.
  * A split. The implementation moves to a new declaration, taking the prose,
    and the old name stays behind as a wrapper whose own new docstring refers to
    the new declaration, in backticks or a `cref`. (A wrapper left with no
    docstring, or with one that does not refer to where the prose went, is
    reported.)
  * A one-line docstring shared by several declarations, when one of them is
    added, deleted or given prose of its own while the others keep the text.
A detachment reported as MOVED is therefore both a loss and a gain: some
declaration lost the block and some other declaration gained it. MERGED is
subject to the rename and split exceptions too.

WHAT THIS CANNOT SEE, and so must be checked by hand:
  * A docstring that was reworded in the same commit that moved it. Rewording
    changes the key, so the block reads as deleted-and-added and no pairing is
    attempted. Reword in a separate commit from the move if you want cover.
  * Which of a merged pair is the intruder. MERGED names the block that lost its
    identity and the declaration the survivor now documents; deciding which half
    belongs where is a reading task.
  * A docstring only *partly* stranded, which is what a definition removed
    along with some of its prose leaves behind. Blocks are keyed by their whole
    text, so a surviving prefix matches no block at the base revision: it reads
    as one block deleted and another added, and neither is a reportable shape.
    Strand the *whole* block and MERGED fires as it should -- measured both
    ways. Removing a definition is therefore the one edit where the remaining
    `///` lines have to be read by hand.
  * Prose that was already attached to the wrong declaration at the base
    revision. This compares two revisions, so it inherits the base's mistakes.
  * Anything below a `let`: docstrings on local bindings inside a function body
    are not tracked, because the declaration regex is anchored at module level.
  * A declaration form the regex does not know is named by its own source line
    rather than by an identifier, which keeps it distinguishable but means an
    unrelated edit to that line reads as a changed subject. Teaching the regex
    the form removes the noise; the check is correct either way.
  * A definition deleted, and a new one written where it stood beneath the
    orphaned docstring. That is indistinguishable from a rename, and passes.
  * A rename or split is recognised only when the paths passed include every
    file the change touched, and when the old name is declared nowhere else
    afterwards and the new one nowhere else before, local bindings and other
    types' fields included. Where a name is reused like that, a rename is
    reported as MOVED: read the report, and if the prose went with its
    declaration, nothing was detached. The same goes for renaming an overloaded
    member or a declaration form the regex does not know, whose subject changes
    whenever its text does.
  * A change that repairs a detachment. Moving prose back to its subject is a
    loss and a gain like any other, and is reported.
  * A same-named, same-kind pair that is not adjacent. A subject is its kind and
    its name, which separates `type Foo` from `module Foo`; the signature is
    added only where two *successive* declarations share a name and kind,
    because qualifying every repeated name buries a move audit in subjects that
    differ solely in whether they were qualified. Two overloads with an
    unrelated declaration between them can therefore still swap prose unseen.
  * A fusion in which the *stranded* half was also reworded. A fusion is
    recognised by the fused text opening with a block that stood on its own
    before, so an edit to that half hides it and the block reads as
    deleted-and-added like any other rewording. Rewording the intruder's half is
    fine: only the opening has to survive verbatim.
"""

import os
import re
import subprocess
import sys
from collections import Counter
from pathlib import Path

# Accessibility and binding modifiers must be consumed rather than captured:
# `let internal fractionBits` whose subject reads as `internal` compares equal to
# every other internal binding, which is exactly the rebinding this looks for.
MODS = r"(?:private\s+|internal\s+|public\s+|rec\s+|inline\s+|mutable\s+|static\s+|abstract\s+|override\s+|default\s+)*"

DECL = re.compile(
    r"^\s*(?:\[<[^\]]*>\]\s*)?"
    r"(?:let\s+" + MODS + r"\((?P<active>\|[^)]*\|)\)"
    r"|let\s+" + MODS + r"``(?P<quoted>[^`]+)``"
    r"|let\s+" + MODS + r"(?P<let>[a-zA-Z_][A-Za-z0-9_']*)"
    r"|type\s+" + MODS + r"(?P<type>[A-Za-z_][A-Za-z0-9_']*)"
    r"|module\s+" + MODS + r"(?P<module>[A-Za-z_][A-Za-z0-9_']*)"
    r"|(?:static\s+|abstract\s+|default\s+)*(?:member|override)\s+"
    + MODS
    + r"(?:this\.|_\.|val\s+)?(?P<member>[A-Za-z_][A-Za-z0-9_']*)"
    r"|(?P<field>[A-Z][A-Za-z0-9_]*)\s*:\s"
    r"|\|\s*(?P<case>[A-Z][A-Za-z0-9_]*))"
)


def signature(lines: list[str], k: int) -> str:
    """The declaration at line `k`, to the end of its signature.

    F# puts a long parameter list on its own lines, so two overloads can share
    a first line of just `member _.Foo` and differ only below it. Taking one
    line would make them the same subject, which is the thing the signature is
    there to prevent. The signature ends at the `=` that starts the body, and
    excludes it, so that editing an overload's body leaves it the same subject.
    That `=` is the first one outside brackets: an attribute's arguments, as in
    `[<Param(Name = "value")>]`, can hold one too. An `abstract` member has no
    body, so a blank line or the next docstring stops the signature as well.
    """
    out = []
    depth = 0
    for line in lines[k : k + 24]:
        if out and (not line.strip() or line.strip().startswith("///")):
            break
        for i, ch in enumerate(line):
            if ch in "([{":
                depth += 1
            elif ch in ")]}":
                depth -= 1
            elif ch == "=" and depth == 0:
                out.append(line[:i])
                return " ".join(" ".join(out).split())
        out.append(line)
    return " ".join(" ".join(out).split())


def subjects(text: str) -> list[tuple[str, str, str, int]]:
    """(kind, name, signature, indent) for each declaration."""
    lines = text.split("\n")
    out = []
    for k, line in enumerate(lines):
        m = DECL.match(line)
        if m:
            kind, name = next(kv for kv in m.groupdict().items() if kv[1])
            out.append((kind, name, signature(lines, k), len(line) - len(line.lstrip())))
    return out


def ambiguous_names(*texts: str) -> set[str]:
    """Names borne by two *successive* declarations in any of these texts.

    An overload set is the one place a name is not identity, so it is the one
    place the subject has to carry the signature too. But qualifying every
    repeated name is worse than useless here: a name repeats across a file all
    the time (a field in two records, a DU case in two unions), a definition
    that moves between files has different neighbours on each side, and the
    audit of a move — the thing this exists for — then fills with subjects that
    differ only in whether they were qualified.

    Adjacency is the whole of the risk. A stranded docstring binds to the
    declaration that follows it, so a name can absorb another's prose only when
    a declaration of that name is the next one along. Restricting to that, and
    to the one declaration form F# lets you overload, is exact for the failure
    and silent everywhere else.
    """
    repeated: set[str] = set()
    for text in texts:
        # Only the kinds that can *be* an overload set. A record cannot repeat a
        # field name nor a union a case name, and `case` in particular also
        # matches `match`-expression arms, whose repeated heads are not
        # declarations at all — feeding those in qualified whole types by
        # accident.
        # Only `member`. F# has no overloading anywhere else: two module-level
        # `let`s of one name are a duplicate-definition error, so a repeated
        # `let` is always shadowing inside a body — and those are common enough
        # (`let block = ...` twice in a test) that admitting them qualified
        # unrelated definitions in other files and reported their signature
        # changes.
        seen = [
            (kind, name, indent)
            for kind, name, _, indent in subjects(text)
            if kind == "member"
        ]
        # Per indentation level, so a `let` inside an overload's body does not
        # come between the two members and hide the overload set. Two members of
        # one type sit at one indent; their locals sit deeper.
        for indent in {i for _, _, i in seen}:
            at = [(kind, name) for kind, name, i in seen if i == indent]
            repeated.update(a[1] for a, b in zip(at, at[1:]) if a == b)
    return repeated


def pairs(text: str, ambiguous: set[str] = frozenset()) -> dict[str, list[str | None]]:
    """(normalised docstring text -> names of the declarations it precedes).

    A block whose subject cannot be identified maps to None, which is itself
    worth reporting: it usually means the declaration went away.
    """
    lines = text.split("\n")
    out: dict[str, list[str | None]] = {}
    i = 0
    while i < len(lines):
        if not lines[i].strip().startswith("///"):
            i += 1
            continue
        j = i
        block = []
        while j < len(lines) and lines[j].strip().startswith("///"):
            block.append(lines[j].strip()[3:].strip())
            j += 1
        # Attributes, blank lines and `//` mechanism comments all sit
        # legitimately between a docstring and its subject.
        k = j
        while k < len(lines) and (
            lines[k].strip().startswith("[<")
            or lines[k].strip().startswith("//")
            or lines[k].strip() == ""
        ):
            k += 1
        subject = None
        if k < len(lines):
            m = DECL.match(lines[k])
            if m:
                kind, name = next(kv for kv in m.groupdict().items() if kv[1])
                # The kind belongs in the identity: `type Foo` and its companion
                # `module Foo` are the same name, so a stranded docstring moving
                # between them would otherwise compare equal, and that pairing
                # is everywhere in this repository.
                subject = f"{kind} {name}"
                if name in ambiguous:
                    subject = signature(lines, k)
            elif lines[k].strip():
                # A form the regex does not know is still a distinguishable
                # subject: use the declaration line itself. Letting every
                # unrecognised form share one `None` is what makes a regex gap
                # fail *open* — cut a definition from under its docstring, and
                # if what follows is also unrecognised the two holes compare
                # equal and the detachment passes. Keyed by text, they do not.
                subject = "<unparsed> " + " ".join(lines[k].split())
        key = " ".join(w for w in block if w)
        if key:
            out.setdefault(key, []).append(subject)
        i = j
    return out


def declared(text: str, ambiguous: set[str] = frozenset()) -> Counter:
    """Every declaration in `text`, as the subject `pairs` would give it.

    Also every non-blank line under the `<unparsed>` prefix, since that is how
    `pairs` names a subject the regex does not know: whether such a subject still
    exists is then a question about whether its line does.
    """
    lines = text.split("\n")
    out: Counter = Counter()
    for k, line in enumerate(lines):
        m = DECL.match(line)
        if m:
            kind, name = next(kv for kv in m.groupdict().items() if kv[1])
            out[signature(lines, k) if name in ambiguous else f"{kind} {name}"] += 1
        elif line.strip():
            out["<unparsed> " + " ".join(line.split())] += 1
    return out


def exists_exactly(f: str) -> bool:
    """Whether `f` exists spelt exactly that way.

    `Path.exists()` is not that question on macOS: the default filesystem is
    case-insensitive, so a mis-cased argument passes it while `git show` — which
    is case-sensitive — finds nothing at the base ref, and the file goes
    silently unexamined. Compare each component against its parent's listing.
    """
    p = Path(f)
    if p.is_absolute():
        return False
    cur = Path(".")
    for part in p.parts:
        try:
            if part not in os.listdir(cur):
                return False
        except OSError:
            return False
        cur = cur / part
    return True


def read(f: str, ref: str | None) -> str | None:
    if ref is not None:
        r = subprocess.run(["git", "show", f"{ref}:{f}"], capture_output=True, text=True)
        return None if r.returncode else r.stdout
    # Exact spelling here too: on a case-insensitive filesystem the two halves
    # of a case-only rename both open the destination, so the current side
    # would read one file twice and report the phantom duplicate.
    return Path(f).read_text() if exists_exactly(f) else None


def sides(
    files: list[str], ref: str
) -> tuple[dict[str, list[str | None]], dict[str, list[str | None]], Counter, Counter]:
    """The old and new (docstring -> subjects) maps, built together, and the
    declarations present on each side."""
    texts = [(read(f, ref), read(f, None)) for f in files]

    # One ambiguity set over every path and both revisions. Deriving it per file
    # would qualify a name in the file it moved out of (where its overload pair
    # still stands) and not in the file it moved into, so a definition that
    # moved *correctly, with its docstring* would be reported as changing
    # subject.
    ambiguous = ambiguous_names(*(t for pair in texts for t in pair if t is not None))

    old: dict[str, list[str | None]] = {}
    new: dict[str, list[str | None]] = {}
    declared_old: Counter = Counter()
    declared_new: Counter = Counter()
    for was, now in texts:
        for text, acc, decls in ((was, old, declared_old), (now, new, declared_new)):
            if text is None:
                continue
            for k, v in pairs(text, ambiguous).items():
                acc.setdefault(k, []).extend(v)
            decls.update(declared(text, ambiguous))
    return old, new, declared_old, declared_new


def split_into(text: str, old: dict[str, list[str | None]], memo) -> list[str] | None:
    """`text` as a concatenation of blocks that stood on their own before, if it is."""
    if text in memo:
        return memo[text]
    if text in old:
        memo[text] = [text]
        return memo[text]
    memo[text] = None  # guard against a pathological re-entry
    for i in range(len(text) - 1, 0, -1):
        if text[i] != " " or text[:i] not in old:
            continue
        rest = split_into(text[i + 1 :], old, memo)
        if rest:
            memo[text] = [text[:i]] + rest
            return memo[text]
    return None


def bare_name(subject: str | None) -> str | None:
    """The name in a `kind name` subject, or None for any other form.

    A double-backticked name (`quoted`) is an identity like any other, and is
    how every test in this repository is named.
    """
    if subject is None or subject.startswith("<unparsed>"):
        return None
    parts = subject.split(" ", 1)
    if len(parts) != 2:
        return None
    if parts[0] == "quoted":
        return parts[1]
    return parts[1] if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_']*", parts[1]) else None


def unexplained(
    lost: Counter,
    gained: Counter,
    declared_old: Counter,
    declared_new: Counter,
    old: dict[str, list[str | None]],
    new_by_subject: dict[str, list[str]],
) -> tuple[Counter, Counter]:
    """`lost` and `gained` with every deliberate move of prose taken out of both.

    Prose can leave a declaration for one that did not exist before in two ways
    that are not detachments. A rename: the name it had exists nowhere afterwards.
    A split: the implementation moved to a new, more general declaration, which
    took its prose, and the old name stayed behind as a thin wrapper whose own new
    docstring names the new declaration. A detachment looks like neither when the
    paths passed include every file the change touched. The declaration that lost
    its prose still exists, having moved or been displaced, and has no docstring
    that refers onward; or it is gone and the prose has landed on one that already
    existed. Each lost declaration is paired with at most one gained one. Prose
    that now precedes nothing (`None`) is never the far end of either: that is a
    definition deleted from under its docstring.

    Both ends must be subjects named by kind and identifier. An overload's
    signature or an unparsed line changes whenever its text does, so its absence
    from one side says nothing about whether the declaration went away. The
    wrapper's docstring must be new to it in this change, because a subject names
    every same-named declaration at once, and one elsewhere that always said as
    much is no evidence about this one.
    """
    fresh = [s for s in gained.elements() if bare_name(s) is not None and declared_old[s] == 0]

    def refers_to(was: str, now: str) -> bool:
        # An explicit reference, in backticks or a `cref`: the name used as an
        # ordinary word is no evidence that the prose went on purpose.
        name = bare_name(now)
        if name is None:
            return False
        reference = re.compile(
            r"`" + re.escape(name) + r"`"
            + r'|cref="[^"]*(?<![A-Za-z0-9_\'])' + re.escape(name) + r'"'
        )
        return any(
            reference.search(block)
            for block in new_by_subject.get(was, ())
            if was not in old.get(block, ())
        )

    # A maximum matching, not a greedy one: one block can be shared by a
    # declaration that was renamed and one that was split, and a rename may pair
    # with any fresh declaration while a split only with the one it names, so
    # taking the first fit lets the rename claim the split's partner.
    losers = list(lost.elements())
    fits = [
        [
            j
            for j, now in enumerate(fresh)
            if bare_name(was) is not None and (declared_new[was] == 0 or refers_to(was, now))
        ]
        for was in losers
    ]
    partner: dict[int, int] = {}

    def augment(i: int, seen: set[int]) -> bool:
        for j in fits[i]:
            if j in seen:
                continue
            seen.add(j)
            if j not in partner or augment(partner[j], seen):
                partner[j] = i
                return True
        return False

    for i in range(len(losers)):
        augment(i, set())
    explained_lost = Counter(losers[i] for i in partner.values())
    explained_gained = Counter(fresh[j] for j in partner)
    return lost - explained_lost, gained - explained_gained


def names(subs: list[str | None]) -> list[str]:
    return sorted(x or "<none>" for x in subs)


def stranded_prefix(
    text: str,
    subjects: list[str | None],
    old: dict[str, list[str | None]],
    new_by_subject: dict[str, list[str]],
    declared_old: Counter,
    declared_new: Counter,
) -> list[str] | None:
    """`text` as a block that stood on its own before, plus prose that is new.

    `split_into` asks for a concatenation of blocks that *all* stood on their own
    before, which misses the commonest way to strand a docstring: insert a
    declaration carrying its own, freshly-written docstring between an existing
    block and the declaration that block documents. The two fuse, and because the
    second half never existed at the base revision there is nothing to split it
    against.

    What survives is that the fused text opens with the stranded block verbatim.
    That alone is not evidence — a docstring that gains a paragraph opens with its
    old self too, and so does an unrelated one that quotes it — so the two things
    that make it one are asked of the *declarations* rather than of the text.
    `text` must precede at least one declaration the opening did not already
    document, since a declaration that had this prose and still has it has been
    left alone; and some declaration that did have it must no longer have it
    anywhere in its docstring, since that is what having been detached from it
    means. Both are counted per declaration rather than per block, because one
    normalised block can precede several.

    Openings are tried longest first, that being the most specific account of
    where the new text starts, and one that fails either test does not end the
    search: two old blocks can be nested prefixes of each other, and then the
    shorter is stranded while the longer stays where it was.
    """

    def holds(block: str, head: str) -> bool:
        # Anywhere in the block, not only at its start: prose is as often
        # prepended above an existing docstring as appended below it, and either
        # way the declaration still has what it had. Padded so the span has to be
        # whole words.
        return f" {block} ".find(f" {head} ") >= 0

    def detached(head: str) -> Counter:
        # Per declaration and with multiplicity, because a subject is a kind and
        # a name: one block documenting two same-named declarations must be found
        # inside two docstrings still, or one of the two has lost it.
        lost: Counter = Counter()
        for subject, count in Counter(old[head]).items():
            if subject is None:
                lost[None] += count
                continue
            kept = sum(1 for t in new_by_subject.get(subject, ()) if holds(t, head))
            if kept < count:
                lost[subject] += count - kept
        return lost

    for i in range(len(text) - 1, 0, -1):
        head = text[:i]
        if text[i] != " " or head not in old:
            continue
        # Per occurrence, not per key: one normalised block can precede several
        # declarations, and the ones this opening already documented say nothing
        # about the ones it did not.
        gained = Counter(subjects) - Counter(old[head])
        if not +gained:
            continue
        lost = detached(head)
        if not +lost:
            continue
        # A declaration renamed in the same change that added a paragraph to its
        # docstring: the old name is gone, the new one is new, and the prose is
        # its own. Stranded only if some loss is not accounted for that way.
        lost, _ = unexplained(lost, gained, declared_old, declared_new, old, new_by_subject)
        if not +lost:
            continue
        return [head, text[i + 1 :]]
    return None


def main() -> int:
    if len(sys.argv) < 3:
        print("usage: check-docstring-attachment.py <base-ref> <file>...", file=sys.stderr)
        return 2

    ref, files = sys.argv[1], sys.argv[2:]

    # An oracle that passes when it read nothing is worse than no oracle: a
    # misspelt ref would silently retire the check.
    if subprocess.run(
        ["git", "rev-parse", "--verify", "--quiet", f"{ref}^{{commit}}"],
        capture_output=True,
    ).returncode:
        print(f"{ref} does not name a commit", file=sys.stderr)
        return 2

    unknown = [
        f
        for f in files
        if not exists_exactly(f)
        and subprocess.run(
            ["git", "cat-file", "-e", f"{ref}:{f}"], capture_output=True
        ).returncode
    ]
    if unknown:
        # Skipping a path silently is how a move goes unchecked: the file that
        # received the definition is exactly the one a typo omits.
        print(
            f"these path(s) exist neither at {ref} nor in the working tree: "
            + ", ".join(unknown),
            file=sys.stderr,
        )
        return 2

    old, new, declared_old, declared_new = sides(files, ref)
    # Every docstring each declaration now has, by subject.
    new_by_subject: dict[str, list[str]] = {}
    for key, subs in new.items():
        for sub in subs:
            if sub is not None:
                new_by_subject.setdefault(sub, []).append(key)
    if not old:
        print(
            f"none of the {len(files)} file(s) given exist at {ref}, so there is "
            "nothing to compare against; pass the union of the paths involved on "
            "both sides",
            file=sys.stderr,
        )
        return 2

    bad = 0
    for key, subs in old.items():
        if key not in new:
            continue
        # Prose that moved leaves one declaration and arrives at another, so a
        # detachment is a loss *and* a gain. A gain alone is a one-line docstring
        # reused by a new declaration beside the ones that still have it; a loss
        # alone is a declaration deleted, or its docstring reworded, while others
        # keep the shared text. Prose left documenting nothing gains `None`.
        lost, gained = unexplained(
            Counter(subs) - Counter(new[key]),
            Counter(new[key]) - Counter(subs),
            declared_old,
            declared_new,
            old,
            new_by_subject,
        )
        if +lost and +gained:
            bad += 1
            print(f"MOVED   {names(subs)} -> {names(new[key])}")
            print(f"        {key[:150]}")

    # A block stranded above another fuses with it, and the fused text opens with
    # the stranded block verbatim. That opening is the signature. Where the rest
    # is also blocks that stood on their own, `split_into` accounts for the whole
    # text, which catches a fusion between two declarations of the *same* name —
    # F# overloads, of which this repository has several — and reports each fused
    # block once rather than once per half. Where it is not, `stranded_prefix`
    # takes the opening alone and asks the declarations rather than the text:
    # whether this block documents any of the ones that opening already did, and
    # whether any of those has stopped opening its docstring with it.
    memo: dict[str, list[str] | None] = {}

    for key in new:
        if key in old:
            continue
        parts = split_into(key, old, memo) or stranded_prefix(
            key, new[key], old, new_by_subject, declared_old, declared_new
        )
        if parts and len(parts) > 1:
            bad += 1
            was = [names(old[k])[0] if k in old else "<written here>" for k in parts]
            print(f"MERGED  {was} fused into one block, which documents {names(new[key])}")
            for k in parts:
                print(f"        {k[:120]}")

    print()
    if bad:
        print(f"docstring-attachment: {bad} block(s) changed subject", file=sys.stderr)
        return 1
    print(f"docstring-attachment: {len(old)} block(s) still document what they did at {ref}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
