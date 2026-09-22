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
| `darwin-which-names-bind.c` | §1.1, Darwin half of §1.6 |
| `darwin-utf8-is-not-the-rule.c` | §1.1's refutation of "strict UTF-8" and of "not a noncharacter" |
| `darwin-admissibility-is-not-per-codepoint.c` | §1.1's combining-sequence limit, which rules out modelling APFS exactly |
| `darwin-where-the-check-applies.c` | §1.2 |
| `darwin-rule-ordering.c` | §1.3, Darwin rows of §1.4 |
| `darwin-name-max-vs-permission.c` | §1.3's two-by-two |
| `darwin-lookup-vs-name-max.c` | §1.4's lookup claim |
| `darwin-name-max-boundary-per-unit.c` | §1.5's boundary table |
| `darwin-name-max-which-unit.c` | §1.5's classification |
| `linux-names-are-bytes.py` | the Linux column throughout |
| `fsharp-immutablearray-semantics.fsx` | §2.1's F# equality/comparison table |

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
limit. **No table of admissible code points can express it.**

Two consequences for the design.

- `tryToString` succeeding and APFS accepting a name are **not** the same
  predicate. The U+FFFF row is the counterexample. Anything this plan says about
  the two coinciding is a statement about *PawPrint's model*, never about APFS.
- **PawPrint does not model APFS's predicate, and deliberately approximates it
  as "strictly valid UTF-8".** Reproducing it faithfully means transcribing
  XNU's converter with its normalisation behaviour, and the three refutations
  above — strict UTF-8, noncharacters, per-code-point-ness — are good evidence
  there is more to find. §2.3 states the approximation and §1.1.1 states exactly
  what it over-admits.

#### 1.1.1 The chosen approximation, and what it gets wrong

Darwin's modelled rule is: **a name may be bound iff its bytes are strictly
valid UTF-8.** That is narrower than Linux (which admits any non-NUL bytes) and
broader than APFS. The gap is entirely inside the valid-UTF-8 set:

| name | APFS | PawPrint's Darwin | |
| --- | --- | --- | --- |
| `\xff`, `\xe4\xb8`, `\xc0\x80`, `\xed\xa0\x80` | EILSEQ | **EILSEQ** | agrees |
| `a`, `中`, emoji, `é` (NFC, short) | accept | **accept** | agrees |
| U+FFFF, U+FFFE, U+FDD0–U+FDEF and the other noncharacters | EILSEQ | accept | **over-admits** |
| U+1FFFD (unassigned) | EILSEQ | accept | **over-admits** |
| a combining sequence over 32 characters | EILSEQ | accept | **over-admits** |

Why this is the right trade. The rows it gets right are the ones a guest can
plausibly reach: a hand-rolled P/Invoke passing bytes that are not UTF-8 is
exactly the case that crashes the interpreter today, and it is the whole reason
for this refactor. The rows it gets wrong need a guest to deliberately construct
a Unicode noncharacter or a 33-mark combining sequence, and getting them wrong
costs only that PawPrint *permits* what macOS refuses — a direction that cannot
corrupt anything and cannot mislead a guest into believing a file was refused
when it was not.

The predicate is also cheap and total: it is `tryToString`'s own success
condition, so there is nothing to maintain and nothing to drift. But note that
sharing it is a **modelling choice** rather than a measured fact; §1.1's first
consequence is why.

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

One row of that table is ground truth PawPrint cannot yet act on: **there is no
guest-reachable `symlink`**. `SystemNative_SymLink` does not exist, and
`VirtualFileSystem.createSymlink` is reached only from `ofFileSystemSeed`, so a
link enters this filesystem through a seed and never through a verdict. (The
`emulated-posix-kernel` skill names the same gap, as the trigger that would make
`symlinkPermissions` configuration rather than platform.) The row is recorded
here because it is what a future `symlink` must do, not because this plan can
test it — see Stage 7.

### 1.3 Ordering against the other rules

Darwin, creating operations. Every row is measured against a control, because an
ordering claim means nothing without one:

| # | step | errno | evidence |
| --- | --- | --- | --- |
| 1 | PATH_MAX (at `getname`) | ENAMETOOLONG | 2047-byte path containing `\xff` → ENAMETOOLONG, not EILSEQ |
| 2 | walk to the parent | ENOENT / ENOTDIR | `mkdir("nodir/" + 300×'a')` → ENOENT, not ENAMETOOLONG |
| 3 | NAME_MAX — **decodable names only** | ENAMETOOLONG | `mkdir("ro/" + 300×'a')` → ENAMETOOLONG on an unwritable parent, beating step 4 |
| 4 | parent's write permission | EACCES | `mkdir("ro/" + 300×0xff)` → EACCES, i.e. step 3 passed under the byte limit and step 5 did not run |
| 5 | **encoding** | **EILSEQ** | `mkdir("d/" + 300×0xff)` → EILSEQ on a *writable* parent |
| 6 | the operation | — | |

Rows 3–5 are the subtle ones, and they only separate under a two-by-two:
writable/unwritable parent against decodable/undecodable name.

| parent | name | result | why |
| --- | --- | --- | --- |
| writable `d` | 300 × `'a'` | ENAMETOOLONG | row 3 |
| writable `d` | 300 × `0xFF` | **EILSEQ** | row 3 passes (300 < 765 bytes), row 4 passes, row 5 fires |
| unwritable `ro` | 300 × `'a'` | **ENAMETOOLONG** | row 3 fires *before* row 4 |
| unwritable `ro` | 300 × `0xFF` | **EACCES** | row 3 passes, row 4 fires *before* row 5 |

So NAME_MAX precedes the permission check and the encoding check follows it —
they are not adjacent, and an implementation that puts them together will get one
of those four cells wrong. This also matches where the code already is:
`PathWalk` checks NAME_MAX during the walk, while the permission check lives in
the verdict files, so the encoding check is a new *last* step of the verdict and
NAME_MAX does not move at all.

Row 3 says "decodable names only" because an undecodable component is measured
against a *different* limit — 765 raw bytes rather than 255 UTF-16 units — which
§1.5 bisects. At 300 × `0xFF` (300 bytes) that limit is not reached, which is why
rows 4 and 5 get to run at all.

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

`NameLengthLimit.Utf16CodeUnits` survives, but it is only half of Darwin's
rule: §1.5 shows an undecodable name is measured against a 765-byte cap instead
of being exempt. Note the split is on *decodability*, not on
APFS-admissibility — U+FFFF has a perfectly good UTF-16 length, is measured in
units, and is only then refused at §1.3 row 5. Measured, a lookup shows the
split — on Darwin,
`open(300 × 0xff, O_RDONLY)` is **ENOENT**, where the valid control `open(300 ×
'a', O_RDONLY)` is **ENAMETOOLONG**: 300 bytes is over the unit limit but under
the 765-byte one, and it is the latter that applies to a non-decodable name.

### 1.5 Darwin has *two* NAME_MAX limits, not one

§1.4 said `Utf16CodeUnits` is only ever asked about a decodable name, and that
an undecodable one is skipped. The first half is right; the second is wrong.
An undecodable name is not skipped — it gets a **different limit**.

Bisecting, per repeated unit, the count at which a lookup flips ENOENT →
ENAMETOOLONG (`darwin-name-max-boundary-per-unit.c`):

| unit | bytes/unit | flips at | = bytes | = UTF-16 units |
| --- | --- | --- | --- | --- |
| `a` | 1 | 256 | 256 | **256** |
| `E4 B8 AD` (CJK) | 3 | 256 | 768 | **256** |
| `F0 9F 98 80` (emoji) | 4 | 128 | 512 | **256** |
| `ED A0 80` (surrogate enc) | 3 | 256 | **768** | 256 |
| `E0 80 81` (overlong 3-byte) | 3 | 256 | **768** | 256 |
| `F5 80 80 80` (> U+10FFFF) | 4 | 192 | **768** | 384 |
| `C0 80` (overlong 2-byte) | 2 | 383 | **766** | 383 |
| `E4 B8` (truncated) | 2 | 383 | **766** | 383 |
| `FF`, `80` | 1 | 766 | **766** | 766 |

The emoji row is what proves a unit count exists at all: 512 bytes is nowhere
near any byte cap, so the limit that bound it counts UTF-16 units. The bottom
rows cluster at 766 bytes, which is a raw byte cap of **765** — and 765 = 255 ×
3, so it reads as the conversion buffer's bound.

A 3-byte unit cannot tell the two apart, since 255 units and 765 bytes flip at
the same count. Prefixing 300 ASCII characters breaks the tie — 301 characters
is over the unit cap while ~303 bytes is far under the byte cap
(`darwin-name-max-which-unit.c`):

| name | counted in |
| --- | --- |
| 300 × `a` + `E4 B8 AD` | **units** (ENAMETOOLONG) |
| 300 × `a` + `F0 9F 98 80` | **units** |
| 300 × `a` + `EF BF BF` (U+FFFF) | **units** |
| 300 × `a` + `ED A0 80` | bytes (ENOENT) |
| 300 × `a` + `E0 80 81` | bytes |
| 300 × `a` + `C0 80` | bytes |
| 300 × `a` + `F5 80 80 80` | bytes |
| 300 × `a` + `FF` | bytes |
| 300 × `a` + `E4 B8` | bytes |

So the unit-counted set is **exactly strictly-valid UTF-8**, and everything else
falls back to 765 raw bytes. Note U+FFFF is counted in units even though §1.1
measures APFS refusing to *bind* it: the length rule and the admissibility rule
are independent, and only the latter is the rabbithole.

**Consequence for §2.3(2).** There is no skip. `NameLengthLimit` gains a Darwin
shape carrying both numbers — 255 UTF-16 units for valid UTF-8, else 765 bytes —
and `nameWithinLimit` picks between them on strict decodability. This is a
smaller and better-defined change than the "skip" an earlier draft proposed, and
unlike that skip it cannot silently admit an over-long undecodable name.

*(An earlier draft of this section claimed the scanner was a lax "structural"
one that counted `ED A0 80` in units. That was measured at 300 repetitions,
where the name is 900 bytes and already over the byte cap — the probe confounded
the two limits. The bisection above is what separates them. See the
`probe-methodology` skill: a probe that varies two things at once measures
neither.)*

### 1.6 The things that are just bytes everywhere

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

- **Each type carries `ImmutableArray<byte>` itself.** Rejected, for the reason
  measured below: `ImmutableArray<T>`'s comparison is unusable as a `Map` key, and
  a directory **is** a `Map<DirectoryEntryName, InodeNumber>`. Defusing that
  per-type means writing the same `CompareTo` four times, and the four types also
  want one shared home for `tryToString`, `toEscaped` and the NUL-free invariant.
- **Keep `string`, store bytes as PEP-383-style surrogate escapes.** Rejected.
  Much the smaller diff, since every `Map` key, ordering and `%s` format keeps
  working — but that is exactly the problem: they keep working and are *almost*
  right. It also makes `UnixPathText`'s "an unpaired surrogate is illegal" rule
  a lie, which is the rule the whole module exists to state.

#### What `ImmutableArray<byte>` actually does under F# (measured)

Measured with `dotnet fsi` on this repo's toolchain, because this is the premise
the whole primitive rests on and it is not what one would guess:

| operation | result |
| --- | --- |
| `=`, same length | structural — `True` |
| `=`, different lengths | structural — `False` |
| `hash` | structural |
| `compare`, **same length** | works, `-1` |
| `compare`, **different lengths** | **throws `ArgumentException`** |
| `Map` with same-length keys | works |
| `Map` with **different-length** keys | **throws `ArgumentException`** |
| `Set` with different-length elements | **throws `ArgumentException`** |
| `List.sort` | **throws `InvalidOperationException`** |

So equality and hashing are fine — `ImmutableArray<T>` implements
`IStructuralEquatable`, which F#'s generic equality honours, and the
`IEquatable<ImmutableArray<T>>` reference comparison never gets a look in.
**Comparison is the problem.** `IStructuralComparable` delegates to
`Array`'s, which refuses two arrays of unequal length outright.

A `Map` keyed on a bare `ImmutableArray<byte>` therefore throws the moment a
directory holds two names of different lengths — which is to say, immediately.
That is a loud failure rather than a silent one, but it is fatal and entirely
non-obvious, and it is why `UnixByteString` writes its own `CompareTo`.

*(An earlier draft of this section asserted from memory that the landmine was
`ImmutableArray`'s reference equality silently breaking `Map` lookups. Measuring
it showed equality is structural and fine, and that the real hazard is comparison
throwing. Worse, the first probe of it used two 3-byte keys and "passed" — the
bug needs unequal lengths to show. Two lessons already in the `probe-methodology`
skill: measure instead of asserting, and make sure the probe can fail.)*

**Comparison is therefore lexicographic on the bytes, unsigned, and total across
lengths** — a shorter proper prefix sorts first. `GetHashCode` is computed on
demand rather than stored: F# `Map` is a balanced tree and uses comparison, not
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
/// `None` exactly when the bytes are not strict UTF-8.
///
/// PawPrint's Darwin flavour happens to admit exactly the names for which this
/// is `Some` (§1.1.1), but that is a chosen approximation of APFS's rule, not
/// APFS's rule: real APFS also refuses Unicode noncharacters and over-long
/// combining sequences, which this accepts.
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
the same stage that makes `DirectoryEntryName` byte-valued.

`UnixNamespace.readdir` is the same shape and is easier to miss, because from
PawPrint's side it already looks byte-valued: `ReadDirAnswer.Entry` carries an
`ImmutableArray<byte>`. But it *builds* those bytes with
`UnixPathText.utf8.GetBytes (name.ToString ())`, so it is a string round-trip
wearing a byte-shaped coat. It becomes a raw byte read.

Before applying the `toString` → `toEscaped` rewrite anywhere, audit each site:
the question is whether the result is *read by a human* or *handed back to the
guest / parsed into a path*. Three sites are known to be the latter —
`pathOfDirectory`, `readdir`, and the `getcwd`/`GetProcessPath` pair — and the
audit exists because that list was wrong twice while this plan was reviewed.

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
    /// Strictly-valid UTF-8 only; anything else is EILSEQ *on binding*.
    ///
    /// Darwin/APFS, approximately: APFS additionally refuses Unicode
    /// noncharacters, at least one unassigned code point, and combining
    /// sequences over 32 characters, all of which this admits. See §1.1.1 for
    /// the measured gap and why PawPrint accepts it.
    | StrictUtf8
```

`StrictUtf8` names what the *model* does, which is the honest thing to name: an
`AppleUnicode` case would claim to be Apple's rule, and §1.1 measures three ways
it would not be. A future fidelity project adds that case beside this one rather
than redefining this one.

It has **two** measured consequences, and they are different consequences of the
same fact rather than two rules:

1. *Binding* an inadmissible name is refused with EILSEQ, as the **last** step
   of the verdict — after the permission check, per §1.3 row 5. This lands in
   `CreatingOpenRules`, `MkDirRules` and `RenameRules`, the files that already
   exist to hold exactly this kind of ordered rule table. `RemovalRules` needs
   nothing: measured, removal of such a name is plain ENOENT. **NAME_MAX does
   not move**; it stays in `PathWalk` where it is, which §1.3 row 3 confirms is
   the right side of the permission check.
2. *Looking up* an inadmissible name misses. This needs no code on the happy
   path — such a name cannot be in the `Map`, so the lookup misses naturally.

   What *does* need code is the length rule, and §1.5 is the correction here:
   Darwin has **two** NAME_MAX limits rather than one plus an exemption. So
   `NameLengthLimit` grows a shape carrying both numbers, and
   `PathLimits.nameWithinLimit` picks between them on strict decodability:

   ```fsharp
   /// 255 UTF-16 code units for a strictly-valid-UTF-8 name; 765 raw bytes
   /// for anything else. Darwin/APFS — see §1.5 for the bisection.
   | Utf16CodeUnitsOrBytes of units : int * fallbackBytes : int
   ```

   This lives entirely in `PathLimits`, not in the walk and not in the flavour
   dispatch. Linux's `Bytes` arm is unaffected: it measures every byte string
   the same way, which §1.4 measures (a 256-byte `0xFF` component is
   ENAMETOOLONG there).

   Note the decodability split is independent of admissibility in both
   directions: U+FFFF is decodable, so it is measured in units and *then*
   refused at binding; `ED A0 80` is undecodable, so it is measured in bytes and
   also refused at binding.

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
  over `AbsoluteUnixPath.toUtf8` directly. (`readlink` already does the right
  thing; it was written against `SymlinkTarget.toUtf8` from the start.)
- **`UnixNamespace.readdir` is a third non-diagnostic `toString` consumer**, and
  the least obvious of them: it looks byte-shaped from PawPrint's side, since
  `ReadDirAnswer.Entry` already carries an `ImmutableArray<byte>`, but it builds
  those bytes with `UnixPathText.utf8.GetBytes (name.ToString ())`, and
  `DirectoryStreamName.ToString` delegates to `DirectoryEntryName.toString`. So
  enumeration cannot preserve a non-UTF-8 name today, and `toEscaped` there would
  hand the guest literal backslashes. It becomes a raw byte read.

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

New file `UnixByteString.fs`, immediately *after* `UnixPathText.fs` in the
compile order: its string-taking constructor `ofString` refuses exactly what
`UnixPathText.firstDefect` refuses (NUL, unpaired surrogate; §2.4) and encodes
with `UnixPathText.utf8`. Nothing consumes it yet; this stage is the primitive
and its tests.

**Correctness oracle** — property-based, against `System.String` as the
reference implementation for the ASCII subset and against raw bytes elsewhere:

- `ofBytes >> toBytes = id` for every NUL-free byte array.
- **Equality is structural**: for all byte arrays `b`, two independently
  constructed `UnixByteString`s over copies of `b` are equal and hash equally.
- **Comparison is a total order** consistent with equality, total **across
  differing lengths**, and agreeing with `Array.compareWith compare` on the
  unsigned bytes. The generator must produce unequal lengths, and must include a
  proper-prefix pair — `[1]` vs `[1; 2]`, *not* `[1; 0]`, since a `UnixByteString`
  is NUL-free by invariant and that fixture would be rejected by the constructor
  before `CompareTo` ever ran. §2.1's table shows unequal length is precisely
  what a bare `ImmutableArray` throws on, and a same-length generator passes
  against the broken implementation.
- **`Map`, `Set` and `List.sort` round-trip**: for all lists of distinct byte
  arrays **of differing lengths**, inserting each as a key and looking each up
  through freshly-built values retrieves every one, and sorting does not throw.
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
no behaviour: reversing the substitution on every touched file must leave a diff
against the branch point consisting of the docstring edit alone.
(`scripts/check-move-is-rename-only.sh` does not apply; it checks file moves,
and this stage moves none.) `scripts/check-docstring-attachment.py` must run
against the branch point, because the docstring is being edited next to its
declaration.

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

`NameLengthLimit` gains Darwin's second limit, per §2.3(2) and §1.5: 255 UTF-16
units for a strictly-valid-UTF-8 name, 765 raw bytes for anything else. Both
numbers live in `PathLimits`; nothing changes in the walk, and Linux's `Bytes`
arm measures every byte string exactly as it does today.

**This stage opens a hole that Stage 7 closes, and it must be plugged meanwhile.**
Deleting `PathArgumentRefusal.NotUtf8` removes the only thing standing between a
Darwin-flavour kernel and a name APFS would refuse: after this stage
`mkdir("/tmp/\xff")` runs through `MkDirRules.verdict` into
`VirtualFileSystem.createDirectory`, neither of which checks encoding, and 765
bytes is far above an ordinary name so the length rule does not stop it either.
Stages 4–6 would then model a macOS kernel creating files macOS cannot hold —
silently, and in a way the Linux tests would never show.

So Stage 3 adds a **temporary refusal** on the Darwin creating path: a
`failwith` naming Stage 7, at exactly the point in the verdict where Stage 7's
real rule goes. A crash is the right placeholder rather than "allow it for now",
per correctness over availability — and siting it where the real rule lands means
Stage 7 replaces it rather than hunting for it.

The placeholder only ever fires on a name that is *not valid UTF-8*, which no
ordinary filename is — an NFD-typed `é` is perfectly good UTF-8 and passes
straight through. So it is narrow as well as short-lived, and Stage 7 converts it
into the EILSEQ a real APFS would have given.

**As built**, three things differ from the text above.

- The placeholder sits in `UnixNamespace`, straight after each verdict decides
  to bind, not inside the three verdicts. That is the same point in the
  ordering, because the refusal is the verdicts' last rule. It needs the
  flavour, which `MkDirRules.verdict` does not take, so `BindableEntryNames`
  and `SimulatedUnixPlatform.bindableEntryNames` land here, not in Stage 7.
- `AbsoluteUnixPathError` keeps `ContainsNul` and `UnpairedSurrogate`, with
  UTF-16 indices. Only the string-taking `parse` reports them; the byte-taking
  `ofByteString` cannot. Its segment indices become byte offsets.
- A directory's names now sort in unsigned byte order, not .NET's UTF-16
  ordinal order. The two differ only above the BMP, and `readdir` hands names
  back in this order, so `docs/divergences.md` says so.

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
- **Byte round-trips through all three non-diagnostic sites**, which is the §2.2
  audit made executable and fails loudly if the `toEscaped` rewrite was applied
  blindly:
  - `pathOfDirectory`: a directory named `\xff\xfe` makes `getcwd` answer those
    bytes;
  - `readdir`: enumerating a directory holding `\xff` and `\xe4\xb8` yields
    exactly those bytes in `d_name`, which Linux is measured to do (§1.1);
  - `readlink`: unchanged, and asserted so.
- `nameWithinLimit` reproduces §1.5's classification table: every unit measured
  in units there is, and every unit measured in bytes there is. The emoji row is
  mandatory — it is the only one that distinguishes a unit count from a byte
  count — and so is the `300 × 'a'` prefix form, which is the only thing that
  separates the two limits for a 3-byte unit.
- The Darwin placeholder is reachable and crashes: `mkdir` of an undecodable
  name on a `macOsArm64` kernel hits the Stage 7 `failwith`, rather than
  succeeding.
- Mutation-test the comparison by deleting `UnixByteString`'s `CompareTo` and
  letting `ImmutableArray`'s `IStructuralComparable` take over: the directory
  tests must go red with `ArgumentException`, not merely fail an assertion. Per
  §2.1 this is the mutation that matters — reverting `Equals` instead would *not*
  go red, because `ImmutableArray`'s structural equality is already correct.
  (See the `mutation-testing` skill.)

---

### Stage 4: `SymlinkTarget` becomes bytes

**Dependencies**: Stage 3.

**Implements**: §1.6, §2.1.

Smaller and self-contained: a target is opaque bytes on *both* flavours
(measured), so there is no flavour rule and no ordering question.
`SymlinkTarget.toUnixPath` becomes a no-op rewrap rather than an encode.

**Correctness oracle**:

- Property: a link seeded with any non-empty NUL-free target reads back
  through `readlink` as exactly those bytes, on both flavours. There is no
  `symlink` syscall (see Stage 7's oracle), so a seed is the only way to make a
  link, and a seed's target is not length-checked, so the generator needs no
  PATH_MAX bound.
- A walk through a link splices the target's own bytes: a link to a directory
  named `\xff` resolves to it.
- Darwin's splice limit counts the target's bytes. Test it with units that a
  lenient decode shortens (`E4 B8` becomes one U+FFFD); with `0xFF`, bytes and
  characters agree and the test proves nothing.
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
  is `\xff\xfe`, then `getcwd`, yields those bytes (§1.6).

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

### Stage 7: Darwin refuses to bind a name that is not valid UTF-8

**Dependencies**: Stages 3 and 4. (Independent of Stage 6.)

**Implements**: §1.1.1, §1.2, §1.3, §2.3.

The modelling gap that becomes real once bytes are representable.

**Scope: the approximation of §1.1.1, not APFS's real predicate.** Darwin admits
exactly the strictly-valid-UTF-8 names. That is a deliberate approximation —
APFS also refuses Unicode noncharacters, at least one unassigned code point, and
combining sequences over 32 characters — and the gap goes in
`docs/divergences.md` rather than being chased. §1.1 is why: three successive
closed forms were refuted by probing, so faithful modelling means transcribing
XNU's converter, which is its own project.

What this buys is the row that matters. A guest hand-rolling a P/Invoke with
bytes that are not UTF-8 is the case that crashes the interpreter today, and
after this stage the Darwin flavour answers it with EILSEQ exactly as APFS does.

- `UnixError.EILSEQ`, `platformDependent 84 92` — the numbers are already
  written down in `UnixError.fs`'s own comment on `EOVERFLOW` ("raw 84 is
  `EOVERFLOW` on Darwin and `EILSEQ` on Linux"). The PAL mapping is
  `Error_EILSEQ = 0x10019`, which exists upstream (`pal_error_common.h:62`), so
  `UnixErrorPal` needs one arm.
- `SimulatedUnixPlatform.bindableEntryNames`, per §2.3: `AnyBytes` for
  `linuxX64`, `StrictUtf8` for `macOsArm64`.
- The rule as the **last** step of the verdict in `CreatingOpenRules`,
  `MkDirRules` and `RenameRules`, at §1.3 row 5 — after the parent's write
  check, not beside NAME_MAX. Nothing in `RemovalRules`: measured, removing such
  a name is plain ENOENT. Nothing moves in `PathWalk`.
- Stage 3's temporary Darwin `failwith` is removed, replaced by the real rule.

**Correctness oracle** — Darwin now rejects something, so the ordering is
directly testable against the shipped flavour rather than deferred:

- **The §1.3 two-by-two, all four cells, on `macOsArm64`.** This is the one that
  matters. Three of the four cells are indistinguishable if NAME_MAX and the
  encoding check are implemented as adjacent steps, and the fourth — unwritable
  parent, undecodable name → EACCES — is what separates them. An earlier draft
  of this plan got exactly that ordering wrong.
- The §1.2 table: on `macOsArm64`, every listed lookup and removal of an
  undecodable name is ENOENT, and every listed binding is EILSEQ — **except the
  `symlink` row, which is deferred**. PawPrint has no guest-reachable symlink
  creation to apply a verdict to (§1.2), and adding one is a separate feature
  that `AGENTS.md` says to raise rather than fold into this work. Leave a test
  parked, or a comment in `UnixNamespace`, so that whoever implements
  `SystemNative_SymLink` finds the measured answer instead of re-deriving it.

  The lookup rows are as load-bearing as the binding rows — they are what says
  the rule is about binding rather than about reading a pathname.
- Property: on `linuxX64`, no operation ever reports EILSEQ, for any NUL-free
  byte string.
- Property: on `macOsArm64`, **given a binding that every earlier rule permits**
  — a free name in a writable, searchable parent, within whichever of §1.5's two
  length limits applies — it is refused with EILSEQ iff
  `UnixByteString.tryToString` is `None`. The precondition is not decoration:
  §1.3 puts EILSEQ *last*, so `mkdir("ro/" + 300 × 0xFF)` is EACCES and a
  766-byte undecodable component is ENAMETOOLONG, both with `tryToString` `None`.
  An unconditional "iff" contradicts the very ordering the cell above asserts.
  Assert it as the *model's* rule (§1.1.1), and site the §1.1.1 divergence table
  next to it so the next reader does not mistake it for APFS's.
- Regression tests for the three over-admitted rows, asserting that PawPrint
  **accepts** U+FFFF, U+1FFFD and a 33-mark combining sequence, each citing
  §1.1.1. They pin the approximation deliberately: without them, someone
  "fixing" one of these would change behaviour with nothing going red, and a
  later reader cannot tell a chosen approximation from an oversight.
- `UnixError.EILSEQ` round-trips through `toRawErrnoUnder` as 84 on Linux and 92
  on Darwin, and through the PAL as `0x10019`.
- Extend `TestVirtualFileSystemAgainstHost` with `byte[]`-declared P/Invokes.
  Against a Linux host it may compare the full rule. Against an APFS host it may
  compare only the rows §1.1.1 marks "agrees" — which is most of them, and is
  the first time this suite can check the Darwin encoding rule against a real
  kernel at all.

**If fidelity is wanted later**: add a `BindableEntryNames.AppleUnicode` case
beside `StrictUtf8`, carrying a transcription of XNU's `utf8_decodestr`,
validated against a macOS host oracle. Start by reading the XNU source, not by
probing — probing here found three rules and suggests there are more. The
regression tests above are the ones that would flip.

---

### Stage 8: documentation

**Dependencies**: Stage 7.

- `docs/divergences.md`'s directory-enumeration entry says the order among names
  is "F# ordinal (UTF-16) order" and notes that sorting by UTF-8 bytes "was
  considered … but it differs only above the BMP and buys nothing a test could
  observe". After Stage 3 the order **is** byte order, so that passage is
  rewritten — and its own argument now favours what we have.
- A new `docs/divergences.md` entry for §1.1–§1.5, with the measured tables, and
  a second recording §1.1.1's deliberate over-admission on the Darwin flavour.
  The second matters more: it is a standing divergence rather than a description
  of agreement, and it must say plainly that PawPrint's Darwin rule is
  "strictly-valid UTF-8" *chosen as an approximation*, listing the three classes
  APFS refuses and PawPrint does not. The Stage 7 regression tests are its
  executable half.
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

**The `ImmutableArray` hazard is comparison, not equality, and it throws.**
§2.1 measures it: equality and hashing are structural and fine, but `compare` on
two arrays of *different lengths* raises `ArgumentException`, so a `Map`, `Set`
or `List.sort` over bare `ImmutableArray<byte>` keys fails the moment the lengths
differ. Stage 1's comparison property must therefore generate unequal lengths —
a same-length generator passes against the broken implementation — and Stage 3
mutation-tests it by deleting `CompareTo` rather than `Equals`. Do not skip
either; this plan asserted the opposite hazard from memory for several drafts.

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
today gets a `failwith` will get an errno, and a guest that today creates a file
with non-UTF-8 bytes will get EILSEQ. Both are corrections, but they are
behaviour changes and belong in `docs/divergences.md` (Stage 8) rather than only
in a commit message.

**The Darwin rule is a stated approximation, and the danger is that it stops
being stated.** `StrictUtf8` looks like a complete rule, and nothing about a
`tryToString` call at the use site says "this over-admits three classes of name
that macOS refuses". That is what the §1.1.1 table, the divergence entry and the
three Stage 7 regression tests are for — between them a reader meets the
approximation whether they start from the code, the docs or a failing test. Do
not land the rule without all three.

**An undecodable name must be measured, not skipped and not crashed on.** Such
a name reaching `PathLimits.nameWithinLimit` is *expected* — an ordinary
`open("d/\xff", O_RDONLY)` on a Darwin-flavour kernel walks straight into it.
Two earlier drafts of this plan got this wrong in opposite directions: one said
to `failwith` there, which would recreate the very guest-triggerable crash this
refactor exists to remove; the other said to treat the name as within the limit,
which would admit a 766-byte `0xFF` component that §1.5 measures as
ENAMETOOLONG. The answer is the 765-byte fallback limit, and the test that keeps
it honest is the boundary at 765/766.
