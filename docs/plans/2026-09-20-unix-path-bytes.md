# A Unix path is a byte string, not a .NET string

`WoofWare.PosixKernel` models four path-shaped things — `UnixPath`,
`DirectoryEntryName`, `SymlinkTarget`, `AbsoluteUnixPath` — and all four store a
.NET `string`. That was never a decision anybody took; it fell out of
`EmulatedKernel.fs` having been written against `System.IO`, and it survived the
extraction into the library unexamined.

It is wrong, and measurably so. On Linux a path is *an arbitrary sequence of
non-NUL bytes*, and the kernel neither knows nor cares whether those bytes decode
as anything. Today the library cannot represent such a path at all: it decodes at
the syscall boundary and **crashes the interpreter** when the decode fails.

```fsharp
// NativeSystemNative.parseGuestPathBytes, today
| Error PathArgumentRefusal.NotUtf8 ->
    failwith
        $"%s{operation}: the guest passed a path that is not valid UTF-8 ..."
```

That is a `failwith` on a guest-controlled input. A guest that hand-rolls a
P/Invoke and passes `open("/tmp/\xff")` — which on Linux is a perfectly ordinary
call that creates a perfectly ordinary file — takes the whole interpreter down.
Worse, it is a *silent modelling hole*: PawPrint claims to be a Linux kernel, and
on the flavour it claims to be, the call succeeds.

This plan replaces the string with a byte string.

## Scope note: what the user asked for versus what is here

The request named "a `UnixPath` type, which is genuinely a string of non-nul
bytes which is not `.`". Two adjustments, because the type already exists and the
restrictions are split across two of them:

- `UnixPath` already exists and already permits `.` — a guest may legally pass
  `open(".")`, and it resolves. Its invariant after this work is exactly
  *non-NUL bytes*, nothing more.
- The "not `.`" restriction belongs to `DirectoryEntryName`, which is the name a
  directory *binds*. It excludes `.`, `..`, the empty name, and any name
  containing a separator, because a directory binds none of those —
  `VirtualFileSystem` derives both dots from the graph rather than storing them.

So there is one new primitive and four types that change representation, not one
new type.

---

## 1. Measured ground truth

Everything in this section was measured for this plan, on the two flavours
PawPrint models. Nothing here is inferred.

- **Darwin 25.6.0 / macOS 26.6, APFS** (`/dev/disk3s1s1 on / (apfs)`), via a C
  probe.
- **Linux 6.18.5, ext4** (`statfs f_type = 0xef53`), via `container run
  python:3-slim` with `bytes` paths.

The probe sources are in `docs/plans/2026-09-20-unix-path-bytes/`, each carrying
the platform it was run on and the command to re-run it. Each runs entirely
inside its own `mkdtemp` directory and removes only that, so re-running one from
anywhere is safe. Re-run them rather than trusting the tables if a stage's
result disagrees — two of these tables were wrong in the first draft of this
plan and were caught exactly that way.

| probe | produces |
| --- | --- |
| `darwin-which-names-bind.c` | §1.1, Darwin half of §1.5 |
| `darwin-utf8-is-not-the-rule.c` | §1.1's refutation of "strict UTF-8" and of "not a noncharacter" |
| `darwin-admissibility-is-not-per-codepoint.c` | §1.1's combining-sequence limit, which descopes Stage 7 |
| `darwin-where-the-check-applies.c` | §1.2 |
| `darwin-rule-ordering.c` | §1.3, Darwin rows of §1.4 |
| `darwin-name-max-vs-permission.c` | §1.3's two-by-two |
| `darwin-lookup-vs-name-max.c` | §1.4's lookup claim |
| `linux-names-are-bytes.py` | the Linux column throughout |

### 1.1 Which byte strings can be a directory entry name?

| | Linux / ext4 | Darwin / APFS |
| --- | --- | --- |
| `open("d/\xff", O_CREAT)` | **OK** | **EILSEQ** (92) |
| `mkdir("d/\xe4\xb8")` (truncated UTF-8) | **OK** | **EILSEQ** |
| `symlink("t", "d/\xc0\x80")` (overlong) | **OK** | **EILSEQ** |
| `open("d/\xed\xa0\x80", O_CREAT)` (surrogate encoding) | *not probed* | **EILSEQ** |
| `mkdir("d/\xef\xbf\xbf")` (U+FFFF, **valid** UTF-8) | *not probed* | **EILSEQ** |
| `readdir` round-trip of the above | exact bytes (`FF`, `E4 B8`, `C0 80`) | n/a |

ext4 stores bytes and asks no questions. APFS demands more than valid UTF-8, and
**how much more is not determined**.

It certainly refuses everything a strict UTF-8 decoder refuses — truncated
sequences, overlong forms, CESU-8 surrogate encodings. But it also refuses byte
strings that decode perfectly well. Probed (`darwin-utf8-is-not-the-rule.c`):

| code point | class | APFS |
| --- | --- | --- |
| U+FDCF, U+FDF0, U+FFFD, U+E000, U+200B, U+1F600, U+10000 | ordinary | accept |
| U+FDD0, U+FDEF, U+FFFE, U+FFFF, U+1FFFE, U+1FFFF, U+10FFFE, U+10FFFF | Unicode noncharacter | **EILSEQ** |
| U+10FFFD | private use, plane 16 | accept |
| U+1FFFD | **unassigned**, plane 1 | **EILSEQ** |

"APFS accepts iff the name is not a Unicode noncharacter" fits sixteen of those
seventeen and is **refuted** by the last: U+1FFFD is not a noncharacter and is
refused, while the unassigned U+FDCF is accepted.

And admissibility is not a per-code-point property **at all**. APFS limits a
*combining sequence* — a base character plus its combining marks — to 32
characters (`darwin-admissibility-is-not-per-codepoint.c`):

| name | bytes | APFS |
| --- | --- | --- |
| 32 × U+0301 (bare combining acute) | 64 | accept |
| 33 × U+0301 | 66 | **EILSEQ** |
| `b` + 31 × U+0301 | 63 | accept |
| `b` + 32 × U+0301 | 65 | **EILSEQ** |
| 33 × U+0308 (a different mark) | 66 | **EILSEQ** |
| 33 × U+4E2D (ordinary, non-combining) | 99 | accept |

So the limit is 32 characters *per combining sequence*, independent of the
mark, and unrelated to byte length or NAME_MAX. This is XNU's decomposition
limit. **No table of admissible code points can express it**, which is what
descopes Stage 7 — see there.

Two consequences for the design, and the second is the important one.

- `tryToString` succeeding and APFS accepting a name are **not** the same
  predicate. An earlier draft of this plan claimed they were and built §2.3 on
  it; the U+FFFF row is the counterexample. Do not re-derive one from the other.
- **The Darwin predicate is not yet known well enough to implement.** Stage 7
  therefore begins by mapping it, which is tractable by brute force: sweep all
  1,114,112 code points through `mkdir` and record the accepted set. Until that
  sweep exists, the Darwin arm must refuse the region it cannot speak for rather
  than guess — this is "correctness over availability" applied to a filesystem
  rule.

### 1.2 Where does APFS apply that check?

Not to the pathname, and not to lookups. Measured on Darwin:

| call | errno |
| --- | --- |
| `open("d/\xff", O_RDONLY)` | ENOENT |
| `access("d/\xff", F_OK)` / `stat` / `readlink` | ENOENT |
| `open("\xff/f", O_RDONLY)` (non-final component) | ENOENT |
| `unlink("d/\xff")` / `rmdir` / `chdir` | ENOENT |
| `rename("d/\xff", "d/g")` (bad **source**) | ENOENT |
| `open("d/f/\xff", O_RDONLY)` (parent is a file) | ENOTDIR |
| `mkdir("d/\xff")` | **EILSEQ** |
| `open("d/\xff", O_CREAT)` | **EILSEQ** |
| `symlink("t", "d/\xff")` (bad **link name**) | **EILSEQ** |
| `rename("d/f", "d/\xff")` (bad **destination**) | **EILSEQ** |
| `mkdir("d/\xff/")` (trailing separator) | **EILSEQ** |

So the rule is about **binding a name**, never about reading a pathname. A
lookup of an unrepresentable name simply misses, exactly as a lookup of any
absent name does — which is the correct answer, because on APFS such a name
cannot be bound and therefore is absent by construction.

### 1.3 Ordering against the other rules

Darwin, creating operations. Every row is measured against a control, because an
ordering claim means nothing without one:

| # | step | errno | evidence |
| --- | --- | --- | --- |
| 1 | PATH_MAX (at `getname`) | ENAMETOOLONG | 2047-byte path containing `\xff` → ENAMETOOLONG, not EILSEQ |
| 2 | walk to the parent | ENOENT / ENOTDIR | `mkdir("nodir/" + 300×'a')` → ENOENT, not ENAMETOOLONG |
| 3 | NAME_MAX — **decodable names only** | ENAMETOOLONG | `mkdir("ro/" + 300×'a')` → ENAMETOOLONG on an unwritable parent, beating step 4 |
| 4 | parent's write permission | EACCES | `mkdir("ro/" + 300×0xff)` → EACCES, i.e. step 3 was skipped and step 5 did not run |
| 5 | **encoding** | **EILSEQ** | `mkdir("d/" + 300×0xff)` → EILSEQ on a *writable* parent |
| 6 | the operation | — | |

Rows 3–5 are the subtle ones, and they only separate under a two-by-two:
writable/unwritable parent against decodable/undecodable name.

| parent | name | result | why |
| --- | --- | --- | --- |
| writable `d` | 300 × `'a'` | ENAMETOOLONG | row 3 |
| writable `d` | 300 × `0xFF` | **EILSEQ** | row 3 skipped, row 4 passes, row 5 fires |
| unwritable `ro` | 300 × `'a'` | **ENAMETOOLONG** | row 3 fires *before* row 4 |
| unwritable `ro` | 300 × `0xFF` | **EACCES** | row 3 skipped, row 4 fires *before* row 5 |

So NAME_MAX precedes the permission check and the encoding check follows it —
they are not adjacent, and an implementation that puts them together will get one
of those four cells wrong. This also matches where the code already is:
`PathWalk` checks NAME_MAX during the walk, while the permission check lives in
the verdict files, so the encoding check is a new *last* step of the verdict and
NAME_MAX does not move at all.

Row 3's "decodable names only" is the same skip §1.2 established for lookups:
an undecodable component is never measured for length, on either path.

Linux has no encoding step at all. Its order is unchanged.

### 1.4 NAME_MAX's unit, re-measured with bytes

| | Linux / ext4 | Darwin / APFS |
| --- | --- | --- |
| 255 × `0xFF` | **OK** | EILSEQ (encoding first) |
| 256 × `0xFF` | **ENAMETOOLONG** | EILSEQ |
| 255 × `中` (765 bytes, 255 UTF-16 units) | *n/a* | **OK** |
| 256 × `中` (768 bytes, 256 UTF-16 units) | *n/a* | **ENAMETOOLONG** |

Two conclusions.

`NameLengthLimit.Utf8Bytes` is misnamed: measured with 255 raw `0xFF` bytes,
which are not UTF-8 at all, Linux's limit is a **raw byte count**. Rename it
`NameLengthLimit.Bytes`.

`NameLengthLimit.Utf16CodeUnits` survives, and is now *better defined than it
was*: it can only ever be asked about a **decodable** name, because both the
lookup path (§1.2) and the creating path (§1.3 row 3) skip the length rule
entirely for a name that has no UTF-16 form. Note this is the *decodability*
skip, not the APFS-admissibility rule — U+FFFF has a perfectly good UTF-16
length and is measured for it, then refused at row 5. Measured, a lookup
confirms the skip — on Darwin,
`open(300 × 0xff, O_RDONLY)` is **ENOENT**, where the valid control `open(300 ×
'a', O_RDONLY)` is **ENAMETOOLONG**. A non-decodable component never reaches the
length rule at all.

### 1.5 The things that are just bytes everywhere

| | Linux / ext4 | Darwin / APFS |
| --- | --- | --- |
| `symlink("/tmp/\xff", "d/lnk")` (bad **target**) | OK, `readlink` → `2F 74 6D 70 2F FF` | **OK**, exact round-trip |
| `getcwd` inside a directory named `\xff\xfe` | raw bytes, ending `… 2F 64 2F FF FE` | n/a (cannot be created) |

A symlink *target* is opaque bytes even on APFS — it is file content, not a
name. `SymlinkTarget` therefore gets no flavour rule at all, and `getcwd` returns
whatever bytes the names were made of.

---

## 2. Design

### 2.1 One new primitive

```fsharp
/// A NUL-free sequence of bytes, which is what a Unix kernel means by "a string".
[<Struct>]
[<CustomEquality; CustomComparison>]
type UnixByteString =
    private { Bytes : ImmutableArray<byte> }
```

All four path types wrap this rather than a `string`. Two options were weighed:

- **Each type carries `ImmutableArray<byte>` itself.** Rejected. `ImmutableArray<T>`
  implements `IEquatable<ImmutableArray<T>>` as *reference equality on the
  underlying array*, and implements no `IComparable` at all. So a
  `DirectoryEntryName` wrapping one naively would make `Map<DirectoryEntryName,
  InodeNumber>` — which is what a directory **is** — fail to find a name that was
  looked up through different bytes. Defusing that per-type means writing the
  same custom `Equals`/`GetHashCode`/`CompareTo` four times.
- **Keep `string`, store bytes as PEP-383-style surrogate escapes.** Rejected.
  Much the smaller diff, since every `Map` key, ordering and `%s` format keeps
  working — but that is exactly the problem: they keep working and are *almost*
  right. It also makes `UnixPathText`'s "an unpaired surrogate is illegal" rule
  a lie, which is the rule the whole module exists to state.

The chosen shape puts the landmine in one place and defuses it once.

**Comparison is lexicographic on the bytes, unsigned.** `GetHashCode` is computed
on demand rather than stored: F# `Map` is a balanced tree and uses comparison, not
hashing, so nothing in the hot path hashes a name, and a stored hash is one more
field a forged `Unchecked.defaultof` could make inconsistent.

**`Unchecked.defaultof<UnixByteString>` carries a default `ImmutableArray`, whose
underlying array is null.** That is the same hazard the four types already guard
with `assertValid`, and it stays guarded the same way — but note it moves: today
the forged value has a null `string`, tomorrow a default `ImmutableArray`, and
`.Length` on the latter throws rather than returning 0. Every `assertValid` must
check `IsDefault` *first*, before anything reads a length.

### 2.2 The reading functions, and why `tryToString` is not enough

There are ~90 existing `*.toString` call sites. Overwhelmingly they are inside
`failwith` messages:

```fsharp
$"VirtualFileSystem.unbind: directory inode %O{directory} bound
  \"%s{DirectoryEntryName.toString name}\" to inode %O{target}, ..."
```

`tryToString` is the wrong tool for every one of them: a diagnostic that cannot
render the thing it is diagnosing is useless, and making each site invent its own
fallback would give ninety different fallbacks. So the primitive exposes two
readers with different jobs:

```fsharp
/// The .NET string this names, if it has one.
/// `None` exactly when the bytes are not strict UTF-8. This is *not* the same
/// question as "may APFS bind this as a name" — APFS refuses names this
/// accepts (§1.1) — so do not use it for that.
val tryToString : UnixByteString -> string option

/// Total. Valid UTF-8 runs verbatim; any other byte as `\xNN`; a literal
/// backslash as `\\`, so the rendering is injective and two distinct byte
/// strings never print the same.
val toEscaped : UnixByteString -> string
```

Escaping the backslash matters: without it a diagnostic cannot distinguish the
four-byte name `a\x41` from the two-byte name `aA`, and error messages in this
repo are routinely compared against each other.

Most current `toString` sites become `toEscaped` — but **not all of them, and
the exception matters**. `VirtualFileSystem.pathOfDirectory` is `getcwd`'s
implementation: it climbs to the root collecting `DirectoryEntryName`s, then
`List.map DirectoryEntryName.toString` and concatenates them with separators
into an `AbsoluteUnixPath`. That is path *reconstruction*, not diagnostics.
Rewriting it to `toEscaped` would make `getcwd` answer `\xff` as six literal
characters, and would double the backslash in any name containing one.

`pathOfDirectory` therefore migrates to **byte** concatenation — join the names'
bytes with the separator byte and build the `AbsoluteUnixPath` from those — in
the same stage that makes `DirectoryEntryName` byte-valued. Before applying the
`toString` → `toEscaped` rewrite anywhere, audit each site for this shape: the
question is whether the result is *read by a human* or *parsed back into a path*.

`tryToString` has exactly two real consumers, and they are the two places a .NET
string is genuinely required: the differential oracle materialising a seed onto
the *host* filesystem (`RealRuntime.materialiseSeed`, which calls
`File.WriteAllBytes(path : string, …)`), and any host-supplied configuration
round-tripping back out.

### 2.3 The flavour rule

One new accessor, stating one fact:

```fsharp
/// Which entry names this flavour's filesystem is willing to bind.
val bindableEntryNames : SimulatedUnixPlatform -> BindableEntryNames

[<RequireQualifiedAccess>]
type BindableEntryNames =
    /// Any NUL-free byte string. Linux/ext4.
    | AnyBytes
    /// The set APFS admits. **Not implemented** — §1.1 shows it is not a
    /// per-code-point property (a combining sequence is capped at 32
    /// characters), so it needs XNU's conversion transcribed rather than a
    /// table. Stage 7 ships Darwin as `AnyBytes` and records the gap as a
    /// divergence; this case is the shape the fix would take.
    | AppleUnicode
```

`AppleUnicode` rather than `StrictUtf8`: the rule is Apple's, it is narrower than
UTF-8 in at least two unrelated ways, and naming it after an encoding would
invite exactly the `tryToString`-shaped shortcut §1.1 refutes.

It has **two** measured consequences, and they are different consequences of the
same fact rather than two rules:

1. *Binding* an inadmissible name is refused with EILSEQ, as the **last** step
   of the verdict — after the permission check, per §1.3 row 5. This lands in
   `CreatingOpenRules`, `MkDirRules` and `RenameRules`, the files that already
   exist to hold exactly this kind of ordered rule table. `RemovalRules` needs
   nothing: measured, removal of such a name is plain ENOENT. **NAME_MAX does
   not move**; it stays in `PathWalk` where it is, which §1.3 row 3 confirms is
   the right side of the permission check.
2. *Looking up* an inadmissible name misses, and its length is never examined.
   This needs no code on the happy path — such a name cannot be in the `Map`, so
   the lookup misses naturally — but the NAME_MAX check must be skipped for a
   name with no UTF-16 form, because §1.4 measured `open(300 × 0xff, O_RDONLY)`
   as ENOENT where the valid control is ENAMETOOLONG.

   **The skip belongs to `NameLengthLimit.Utf16CodeUnits`, not to the walk and
   not to the flavour.** A `Bytes` limit can measure any byte string and must
   always do so: §1.4 measured Linux answering ENAMETOOLONG for a 256-byte
   `0xFF` component, so a walk that skipped undecodable names outright would get
   Linux wrong. Put it inside `PathLimits.nameWithinLimit`'s `Utf16CodeUnits`
   arm — the arm that needs a UTF-16 length and, for such a name, has none.

   Note also that the skip is keyed on *decodability*, which is a strictly
   weaker condition than admissibility: U+FFFF is decodable, so it is
   length-checked normally and only then refused at binding.

Like `pathLimits`, this is really a property of the *mount* rather than the
kernel, and it goes in `SimulatedUnixPlatform` for the same stated reason: PawPrint
models one filesystem per flavour. It gets the same named trigger for becoming
configuration — a second filesystem — recorded beside it, and it must not be
cited as precedent for putting a machine-dependent fact in the platform. (See the
`emulated-posix-kernel` skill, §1.)

A **seed** bypasses this, exactly as it already bypasses NAME_MAX: a host can
seed a Darwin-flavour filesystem with a name APFS could not hold. That
pre-existing looseness is unchanged and stays documented where it already is,
in `NativeSystemNative`'s `readdir` handler.

### 2.4 What gets simpler

This is a net deletion in several places.

- **`UnixPathTextDefect.UnpairedSurrogate` stops being a rule about *paths***
  and becomes a rule about *encoding a .NET string*. It cannot simply be
  deleted: §2.5 keeps `parseOrFail : string -> _` for host configuration, and
  such a caller can still pass `"/\uD800"`, which has no UTF-8 encoding at all.
  Dropping the check would make the strict encoder throw
  `EncoderFallbackException` from inside a function whose contract is that it
  returns an error instead (`TestAbsoluteUnixPath`'s "parse never throws"), and
  a *lenient* encoder would silently substitute U+FFFD and create a differently
  named file. So the check survives on the string-taking constructors only; the
  byte-taking ones have no such failure mode, because bytes are already bytes.
  What does go is its presence in the *path* types' own invariants — after this
  work a `UnixPath` is NUL-free bytes and nothing else.
- **`PathArgumentRefusal.NotUtf8` disappears entirely**, and with it both
  `failwith`s quoted at the top of this document. `PathArgumentRefusal` becomes a
  single case, `InteriorNul` — which is an interpreter bug, never a guest input —
  so the refusal is no longer something a guest can trigger.
- **`AbsoluteUnixPathError` loses `UnpairedSurrogate`**, and its `ContainsNul`
  index becomes a byte offset rather than a UTF-16 index.
- **`UnixPathResolution.withinPathMax`** stops being
  `utf8.GetByteCount (UnixPath.toString path)` and becomes a length read.
- **`PathCursor`** stops counting UTF-16 offsets that it then converts to byte
  counts for `remainingBytes`; buffer, offset and limit are all bytes, in the
  same unit XNU's `cn_pnbuf`/`ni_pathlen` use. This removes the one genuine
  impedance mismatch in the resolution walk.
- **`getcwd` and `GetProcessPath`** stop going
  `AbsoluteUnixPath.toString |> allocateNativeHeapNullTerminatedUtf8` and hand
  over `AbsoluteUnixPath.toUtf8` directly. (`readlink` and `readdir` already do
  the right thing — they were written against `toUtf8` from the start.)

### 2.5 What deliberately stays a .NET string

- **`KernelConfig.CurrentDirectory`, `ProcessPath`, and the `FileSystem` seed.**
  A host supplies these from F# source, where a literal is a `string`. They keep
  `string`-taking constructors (`AbsoluteUnixPath.parseOrFail`,
  `DirectoryEntryName.parseOrFail`), which now mean "encode this string as UTF-8,
  then apply the byte rules". This keeps the 202 test call sites that pass
  literals compiling unchanged (of 222 in the repo), which is most of what keeps
  this refactor reviewable. A byte-taking `ofBytes` sits beside each.
- **Everything outside the emulated filesystem**: assembly paths, runtime
  directories, `AppContext` properties. These are host paths handled through
  `System.IO` and never enter the kernel.
- **The `container`/host differential tests' P/Invokes**, which declare `string
  path` and marshal as UTF-8. Testing a non-UTF-8 name against the host requires
  `byte[]` declarations; that is a cost carried by Stage 7, not by everything.

---

## 3. Implementation plan

Implement this plan with each stage on its own branch, stacked as necessary on
previous branches, so that a reviewer can review each branch in isolation.

A note on ordering: **three of the four types must move together** (Stage 3),
because each pair is joined by a function that would otherwise have to encode or
decode in the middle.

- `PathCursor.next` slices a `DirectoryEntryName` straight out of a `UnixPath`'s
  buffer, so those two cannot differ.
- `VirtualFileSystem.pathOfDirectory` — which *is* `getcwd` — collects
  `DirectoryEntryName`s and returns an `AbsoluteUnixPath`, so that pair cannot
  differ either: a string-backed return type cannot carry a byte-valued name,
  whatever the concatenation does.

`SymlinkTarget` is the one that genuinely separates, and it gets its own stage.
Measured, a target is opaque bytes on both flavours and nothing turns one into
an `AbsoluteUnixPath`.

PR #1449 renames `AbsoluteUnixPathError` to `GetcwdResultParseError` and touches
the same DU that Stage 5 rewrites. Whichever lands first, the other rebases; the
conflict is confined to one type's case names.

---

### Stage 1: `UnixByteString`

**Dependencies**: none.

**Implements**: §2.1, §2.2.

New file `UnixByteString.fs`, first in the compile order (before
`UnixPathText.fs`). Nothing consumes it yet; this stage is the primitive and its
tests.

**Correctness oracle** — property-based, against `System.String` as the
reference implementation for the ASCII subset and against raw bytes elsewhere:

- `ofBytes >> toBytes = id` for every NUL-free byte array.
- **Equality is structural**: for all byte arrays `b`, two independently
  constructed `UnixByteString`s over copies of `b` are equal and hash equally.
  *This is the property that would have caught the `ImmutableArray` landmine.*
- **Comparison is a total order** consistent with equality, and agrees with
  `Array.compareWith compare` on the unsigned bytes.
- **`Map` round-trip**: for all lists of distinct byte arrays, inserting each as
  a key and looking each up through freshly-built values retrieves every one.
- `tryToString` agrees with `UTF8Encoding(false, true).GetString`: `Some` iff
  that call does not throw. Two separate properties, because one equation cannot
  cover both halves of the domain —
  - over byte strings that **are** valid UTF-8:
    `tryToString >> Option.map ofString = Some`, i.e. it round-trips;
  - over byte strings that are **not**: `tryToString` is `None`.

  Stating the round-trip over the whole NUL-free domain would be unsatisfiable:
  for bytes containing `0xFF` the left side is `None` and the right side is
  `Some`, so a correct implementation would fail it.
- `toEscaped` is **total** (never throws, for every NUL-free byte array) and
  **injective** (for all pairs of distinct byte strings, the renderings differ).
- `assertValid` rejects `Unchecked.defaultof<UnixByteString>` with a message
  naming the context, and does so *without* throwing a
  `NullReferenceException` from an `.IsDefault`-less length read.

---

### Stage 2: `NameLengthLimit.Utf8Bytes` → `Bytes`

**Dependencies**: none (independent of Stage 1; can go in parallel).

**Implements**: §1.4.

Pure rename plus a docstring correction: the limit was never about UTF-8, it is
a raw byte count, and §1.4 measured that with bytes that are not UTF-8 at all.
Six call sites, all listed by `rg NameLengthLimit`.

**Correctness oracle**: the existing suite passes unchanged. This stage changes
no behaviour, so `scripts/check-move-is-rename-only.sh` applies — and
`scripts/check-docstring-attachment.py` must run against the branch point,
because the docstring is being edited next to its declaration.

---

### Stage 3: `UnixPath`, `DirectoryEntryName` and `AbsoluteUnixPath` become bytes

**Dependencies**: Stage 1.

**Implements**: §2.1, §2.2, §2.4 (the `UnixPathText`, `PathArgumentRefusal`,
`PathCursor` and `AbsoluteUnixPathError` changes).

The big one, and it is big on purpose — see the ordering note above for why
these cannot be separated. `UnixPath`, `DirectoryEntryName`, `AbsoluteUnixPath`
and `PathCursor` wrap `UnixByteString`; `PathArgument.parse` stops decoding and
`PathArgumentRefusal.NotUtf8` is deleted along with both of PawPrint's
`failwith`s for it; `UnixPathTextDefect.UnpairedSurrogate` is **kept**, but
demoted to a rule the *string-taking* constructors apply (§2.4) rather than part
of any path type's invariant — the byte-taking constructors have no such failure
mode; every `toString` in a diagnostic becomes `toEscaped`, with the
`pathOfDirectory` exception §2.2 names. `parseOrFail : string -> _`
survives on both types so the test corpus does not churn.

`NameLengthLimit.Utf16CodeUnits` gains the §2.3(2) decodability skip — inside
that arm only, never in the walk, since the `Bytes` arm must keep measuring
undecodable names.

**This stage opens a hole that Stage 7 closes, and it must be plugged meanwhile.**
Deleting `PathArgumentRefusal.NotUtf8` removes the only thing standing between a
Darwin-flavour kernel and a name APFS would refuse: after this stage
`mkdir("/tmp/\xff")` runs through `MkDirRules.verdict` into
`VirtualFileSystem.createDirectory`, neither of which checks encoding, and the
new skip removes the incidental length obstacle. Stages 4–6 would then model a
macOS kernel creating files macOS cannot hold — silently, and in a way the Linux
tests would never show.

So Stage 3 adds a **temporary refusal** on the Darwin creating path: a
`failwith` naming Stage 7, at exactly the point in the verdict where Stage 7's
real rule goes. A crash is the right placeholder rather than "allow it for now",
per correctness over availability — and siting it where the real rule lands
means Stage 7 replaces it rather than hunting for it.

If that placeholder feels like enough friction to be worth avoiding, the
alternative is to reorder: land Stage 7's structure before Stage 3. That costs
more (Stage 7 wants byte-valued names to be *useful*) and is not recommended,
but it is the honest way to avoid the window.

**Correctness oracle**:

- Every existing `WoofWare.PosixKernel.Test` and `WoofWare.PawPrint.Test` test
  passes, including the Guest fixtures (`--filter "TestCategory=Guest"`).
- Property: for every byte string that *is* valid UTF-8, the new
  `PathArgument.parse` agrees with the old one on `Parsed`/`Failed` — i.e. the
  representable cases are unchanged. Assert this against a recorded table
  captured from the pre-change build, not against a reimplementation.
- Property: `PathCursor` traversal of a path agrees with `UnixPath.components`,
  and `remainingBytes` equals the literal remaining buffer length (the UTF-16
  conversion is gone, so this becomes an identity — assert it).
- **New, and only now expressible**: a guest path of `/tmp/\xff` parses rather
  than crashing. A test at the `parseGuestPathBytes` level, replacing
  `TestGuestPathBytes`'s "cannot be represented names the caller" case, which is
  about a refusal that no longer exists.
- `pathOfDirectory` round-trips: for a directory whose name is `\xff\xfe`,
  `getcwd` answers those bytes rather than an escaped rendering. This is the
  §2.2 audit made executable, and it fails loudly if the `toEscaped` rewrite was
  applied blindly.
- The Darwin placeholder is reachable and crashes: `mkdir` of an undecodable
  name on a `macOsArm64` kernel hits the Stage 7 `failwith`, rather than
  succeeding.
- Mutation-test the structural-equality property from Stage 1 by reverting
  `UnixByteString` to `ImmutableArray`'s own `Equals` and confirming the
  directory tests go red. (See the `mutation-testing` skill.)

---

### Stage 4: `SymlinkTarget` becomes bytes

**Dependencies**: Stage 3.

**Implements**: §1.5, §2.1.

Smaller and self-contained: a target is opaque bytes on *both* flavours
(measured), so there is no flavour rule and no ordering question.
`SymlinkTarget.toUnixPath` becomes a no-op rewrap rather than an encode.

**Correctness oracle**:

- Property: `symlink` then `readlink` round-trips the exact bytes, for every
  non-empty NUL-free byte string. Measured on both hosts (§1.5) as the reference.
- Property: `lstat`'s `st_size` for a link equals its target's byte length —
  which for a non-UTF-8 target is now a different number from anything the old
  model could produce.
- Existing symlink tests pass unchanged.

---

### Stage 5: (folded into Stage 3)

`AbsoluteUnixPath` was originally its own stage. It is not separable:
`VirtualFileSystem.pathOfDirectory` — which *is* `getcwd` — collects
`DirectoryEntryName`s and **returns an `AbsoluteUnixPath`**, so the moment
Stage 3 makes entry names byte-valued, that function cannot preserve them
through a string-backed return type no matter how its concatenation is written.
Leaving the two apart would mean a window in which `getcwd` has to crash on a
name a guest can legitimately create.

Its content moves into Stage 3: `AbsoluteUnixPathError` loses
`UnpairedSurrogate` from the byte-taking path (keeping it on the string-taking
one, per §2.4); `ContainsNul` carries a byte offset; `getcwd` and
`SystemNative_GetProcessPath` stop round-tripping through a string. Stage 3's
oracle gains:

- Property: `AbsoluteUnixPath.parse >> toUtf8` round-trips for every byte string
  satisfying the absolute-path shape.
- **Differential, and only now expressible**: `chdir` into a directory whose name
  is `\xff\xfe`, then `getcwd`, yields those bytes (§1.5).

  This one needs care about *which* Linux. `$DOTNET_LINUX_FRAMEWORK_DIR` will
  not do: it changes only which managed assemblies PawPrint interprets, and the
  host filesystem underneath is still APFS, which cannot hold the directory —
  the same oracle limit `AGENTS.md` states for `RealRuntime`. So either run it
  on a genuine Linux host or container (CI is Linux), or make it a
  PawPrint-only test against an explicitly `linuxX64` simulated platform with
  no host side at all. The PawPrint-only form is the cheaper one and is enough
  for this stage, since what is being asserted is that bytes survive the model;
  the host comparison belongs to Stage 7, which already needs a Linux runner.
- Existing `TestAbsoluteUnixPath`, `TestProcessPath` and
  `TestEmulatedKernelCurrentDirectory` pass unchanged — including the
  `UnpairedSurrogate` cases, which §2.4 keeps on the string-taking constructors.

`SymlinkTarget` (Stage 4) stays its own stage: measured, a target is opaque
bytes on both flavours and nothing converts one into an `AbsoluteUnixPath`, so
it carries none of this coupling.

---

### Stage 6: the oracle and the seed learn to refuse

**Dependencies**: Stages 3 and 4.

**Implements**: §2.2 (`tryToString`'s real consumers).

`RealRuntime.validateSeedForOracle` and `canMaterialise` must now refuse a seed
they cannot put on the host: `File.WriteAllBytes` takes a `string`, so a seed
entry whose name has no `tryToString` cannot be materialised. This is the stage
where `tryToString` earns its existence.

Refuse, loudly, rather than silently skipping: a differential test whose two
sides were seeded differently is worse than one that does not run.

**Correctness oracle**:

- A seed containing a non-UTF-8 name is rejected by `canMaterialise`, and the
  rejection names the entry (via `toEscaped`) and says why.
- Every existing oracle-backed test still materialises and still agrees.
- Mutation test: make `canMaterialise` return `true` unconditionally and confirm
  a new non-UTF-8-seeded case fails rather than passing vacuously.

---

### Stage 7: the EILSEQ structure, and Darwin's gap as a documented divergence

**Dependencies**: Stages 3 and 4. (Independent of Stage 6.)

**Implements**: §1.1, §1.2, §1.3, §2.3.

The modelling gap that becomes real once bytes are representable.

**Descoped, deliberately: this stage does not model APFS's predicate.** An
earlier draft had it sweep all 1,114,112 code points and build a table. §1.1
killed that method: admissibility is not a per-code-point property, because the
32-character combining-sequence limit is a fact about the whole name. Modelling
APFS faithfully means transcribing XNU's UTF-8 conversion including its
normalisation behaviour — and having found two unrelated rules by probing, the
honest expectation is that there are more. That is the rabbithole `AGENTS.md`
says to stay out of.

So this stage lands the **structure and the Linux arm**, and records the Darwin
gap as a divergence rather than a crash:

- `BindableEntryNames.AnyBytes` for Linux, which is fully known and fully
  measured.
- The Darwin arm is **also** `AnyBytes` for now, with a `docs/divergences.md`
  entry stating precisely what that over-admits: PawPrint's Darwin flavour binds
  names APFS refuses (invalid UTF-8, noncharacters, over-long combining
  sequences), with §1.1's two tables as the evidence.
- The EILSEQ vocabulary still lands, because it costs almost nothing and is what
  any later fidelity work needs.

Why a documented divergence rather than a `failwith`: a crash here would fire on
*ordinary* filenames. `é` typed as NFD is a combining sequence, and a guest doing
perfectly normal things on the Darwin flavour would meet it. Refusing to answer
is right when the alternative is a *wrong* answer a guest can act on; here the
alternative is being more permissive than one of two flavours, in a direction
that cannot corrupt anything, on the flavour neither CI nor production uses.

**If fidelity is wanted later**, the shape is a `BindableEntryNames.AppleUnicode`
arm carrying a transcription of XNU's `utf8_decodestr`, validated against a
macOS host oracle. That is its own project, and it should start by reading the
XNU source rather than by probing — probing here found two rules and suggests
there are more.

- `UnixError.EILSEQ`, `platformDependent 84 92` — the numbers are already
  written down in `UnixError.fs`'s own comment on `EOVERFLOW` ("raw 84 is
  `EOVERFLOW` on Darwin and `EILSEQ` on Linux"). The PAL mapping is
  `Error_EILSEQ = 0x10019`, which exists upstream
  (`pal_error_common.h:62`), so `UnixErrorPal` needs one arm.
- `SimulatedUnixPlatform.bindableEntryNames`, per §2.3 — with the rule applied
  as the last step of the verdict in `CreatingOpenRules`, `MkDirRules` and
  `RenameRules`, so §1.3's measured position is encoded in the structure even
  though both flavours currently answer `AnyBytes`. Nothing in `RemovalRules`,
  and nothing moves in `PathWalk`.
- Stage 3's temporary Darwin `failwith` is removed, the answer now being "allow,
  and document the divergence" rather than "crash".

**Correctness oracle**:

- Property: on an `AnyBytes` flavour, no operation ever reports EILSEQ, for any
  NUL-free byte string. Both shipped flavours are `AnyBytes`, so this is the
  whole behavioural claim of the stage, and it is what says the structure is
  inert.
- The §1.3 two-by-two, all four cells, against a **synthetic**
  `BindableEntryNames` value rather than either shipped flavour. The ordering is
  measured and worth encoding even with no flavour using it — and three of the
  four cells are indistinguishable if NAME_MAX and the encoding check are
  implemented as adjacent steps, which is exactly how an earlier draft of this
  plan got it wrong.
- `UnixError.EILSEQ` round-trips through `toRawErrnoUnder` as 84 on Linux and 92
  on Darwin, and through the PAL as `0x10019`.
- Extend `TestVirtualFileSystemAgainstHost` with `byte[]`-declared P/Invokes so
  the host comparison covers non-UTF-8 names **on Linux**, where the model now
  claims to agree. It must not make that comparison against an APFS host; the
  divergence entry is why.

---

### Stage 8: documentation

**Dependencies**: Stage 7.

- `docs/divergences.md`'s directory-enumeration entry says the order among names
  is "F# ordinal (UTF-16) order" and notes that sorting by UTF-8 bytes "was
  considered … but it differs only above the BMP and buys nothing a test could
  observe". After Stage 3 the order **is** byte order, so that passage is
  rewritten — and its own argument now favours what we have.
- A new `docs/divergences.md` entry for §1.1–§1.3, with the measured tables, and
  a second recording Stage 7's deliberate over-admission on the Darwin flavour.
  The second matters more: it is a standing divergence, not a description of
  agreement.
- `WoofWare.PosixKernel/README.md` and `AGENTS.md`: the library's model of a
  filename is bytes, not characters. `PathArgumentRefusal`'s "This kernel models
  a filename as a string of characters (where real Linux models it just as a
  stream of non-NUL bytes)" is the sentence that stops being true.
- The `emulated-posix-kernel` skill gains the §1.3 ordering table, per its own
  "carries measured divergence tables — consult them rather than re-measuring".

**Correctness oracle**: `scripts/check-docstring-attachment.py`, and a read-through
of every `rg -i "utf-?8"` hit in `docs/` and in the two `AGENTS.md` files.

---

## 4. Risks

**The `ImmutableArray` equality landmine is silent.** It does not fail to
compile, and it does not throw; a directory simply stops finding its own
entries. Stage 1's structural-equality and `Map`-round-trip properties exist
specifically for this, and Stage 3 mutation-tests them. Do not skip that.

**`Unchecked.defaultof` changes shape.** Today a forged value holds a null
`string` and `assertValid` matches on it; tomorrow it holds a default
`ImmutableArray` whose `.Length` *throws*. Every `assertValid` must test
`IsDefault` before reading anything. There are four of these
(`UnixPath`, `DirectoryEntryName`, `SymlinkTarget`, `AbsoluteUnixPath`) plus
`PathCursor.bufferOf`.

**Stage 3 is large and cannot usefully be split.** Three of the four types move
in it, for the reasons in §3's ordering note — this was tried as two stages and
the split does not exist. The mitigation is that the `string`-taking
`parseOrFail` constructors survive, so the 202 test call sites that pass
literals do not change at all; the diff is concentrated in the library.
If it still comes out unreviewable, the fallback is to land `UnixByteString`
behind the existing string API first (Stage 1 + an adapter) and flip the
representation in a second pass — but that is two migrations where one will do,
and a half-finished migration is the thing to avoid.

**Stage 7 changes guest-visible behaviour on the Darwin flavour.** A guest that
today gets a `failwith` will get an errno; a guest that today creates a file
will, on the Darwin flavour, get EILSEQ. Both are corrections, but they are
behaviour changes and belong in `docs/divergences.md` (Stage 8) rather than only
in a commit message.

**`Utf16CodeUnits` must skip an undecodable name, not crash on one.** An
undecodable name reaching `PathLimits.nameWithinLimit` is *expected* — an
ordinary `open("d/\xff", O_RDONLY)` on a Darwin-flavour kernel walks straight
into it — so that arm answers "within the limit" and lets the lookup miss, which
§1.2 measures as ENOENT. An earlier draft of this plan said to `failwith` there
instead; doing that would recreate exactly the guest-triggerable crash this
whole refactor exists to remove. The `Bytes` arm, by contrast, measures
everything and never skips.
