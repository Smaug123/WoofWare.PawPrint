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

The probe sources are in `docs/plans/2026-09-20-unix-path-bytes/`, one per
subsection below, each carrying the platform it was run on and the command to
re-run it. Re-run them rather than trusting the tables if a stage's result
disagrees.

### 1.1 Which byte strings can be a directory entry name?

| | Linux / ext4 | Darwin / APFS |
| --- | --- | --- |
| `open("d/\xff", O_CREAT)` | **OK** | **EILSEQ** (92) |
| `mkdir("d/\xe4\xb8")` (truncated UTF-8) | **OK** | **EILSEQ** |
| `symlink("t", "d/\xc0\x80")` (overlong) | **OK** | **EILSEQ** |
| `open("d/\xed\xa0\x80", O_CREAT)` (surrogate encoding) | *not probed* | **EILSEQ** |
| `readdir` round-trip of the above | exact bytes (`FF`, `E4 B8`, `C0 80`) | n/a |

APFS demands **strict** UTF-8: it refuses truncated sequences, overlong forms and
CESU-8 surrogate encodings alike. ext4 stores bytes and asks no questions.

This matters more than it looks, because .NET's strict decoder
(`UTF8Encoding(false, true)` — already `UnixPathText.utf8`) rejects exactly those
same three classes. **`tryToString` succeeding and APFS accepting the name are
the same predicate.** One function, two consumers; see §2.3.

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

Darwin, creating operations, each row measured against a control:

| step | errno | evidence |
| --- | --- | --- |
| PATH_MAX (at `getname`) | ENAMETOOLONG | 2047-byte path containing `\xff` → ENAMETOOLONG, not EILSEQ |
| walk to the parent | ENOENT / ENOTDIR | `open("d/nodir/\xff", O_CREAT)` → ENOENT |
| parent's write permission | EACCES | `mkdir("ro/\xff")` → EACCES (control `mkdir("ro/plain")` → EACCES) |
| **encoding** | **EILSEQ** | `mkdir(300 × 0xff)` → EILSEQ (control `mkdir(300 × 'a')` → ENAMETOOLONG) |
| NAME_MAX | ENAMETOOLONG | control above |
| the operation | — | |

The fourth row is the load-bearing one: a name that is *both* over-long *and*
invalid reports EILSEQ, so the encoding check strictly precedes NAME_MAX. And
the third row bounds it from the other side: an unwritable parent reports EACCES,
so it strictly follows the permission check.

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
was*: it can only ever be asked about a name APFS would accept, because §1.2 and
§1.3 put the encoding check in front of it on both the creating path (EILSEQ) and
the lookup path (ENOENT). Measured, a lookup confirms this — on Darwin,
`open(300 × 0xff, O_RDONLY)` is **ENOENT**, where the valid control `open(300 ×
'a', O_RDONLY)` is **ENAMETOOLONG**. A non-decodable component never reaches the
length rule at all.

### 1.5 The things that are just bytes everywhere

| | Linux / ext4 | Darwin / APFS |
| --- | --- | --- |
| `symlink("/tmp/\xff", "d/lnk")` (bad **target**) | OK, `readlink` → `2F 74 6D 70 2F FF` | **OK**, exact round-trip |
| `getcwd` inside a directory named `\xff\xfe` | `2F 77 6F 72 6B 2F 64 2F FF FE` | n/a (cannot be created) |

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
/// `None` exactly when the bytes are not strict UTF-8 — which is also exactly
/// when APFS refuses to bind them as a name (measured; see §1.1).
val tryToString : UnixByteString -> string option

/// Total. Valid UTF-8 runs verbatim; any other byte as `\xNN`; a literal
/// backslash as `\\`, so the rendering is injective and two distinct byte
/// strings never print the same.
val toEscaped : UnixByteString -> string
```

Escaping the backslash matters: without it a diagnostic cannot distinguish the
four-byte name `a\x41` from the two-byte name `aA`, and error messages in this
repo are routinely compared against each other.

Every current `toString` site becomes `toEscaped`. `tryToString` has exactly two
real consumers, and they are the two places a .NET string is genuinely required:
the differential oracle materialising a seed onto the *host* filesystem
(`RealRuntime.materialiseSeed`, which calls `File.WriteAllBytes(path : string,
…)`), and any host-supplied configuration round-tripping back out.

### 2.3 The flavour rule

One new accessor, stating one fact:

```fsharp
/// Which entry names this flavour's filesystem can bind.
val entryNameEncoding : SimulatedUnixPlatform -> EntryNameEncoding

[<RequireQualifiedAccess>]
type EntryNameEncoding =
    /// Any NUL-free byte string. Linux/ext4.
    | AnyBytes
    /// Strict UTF-8 only; anything else is EILSEQ on binding. Darwin/APFS.
    | StrictUtf8
```

It has **two** measured consequences, and they are different consequences of the
same fact rather than two rules:

1. *Binding* an unrepresentable name is refused with EILSEQ, at the position
   §1.3 measured (after EACCES, before NAME_MAX). This lands in
   `CreatingOpenRules`, `MkDirRules` and `RenameRules` — the files that already
   exist to hold exactly this kind of ordered rule table. `RemovalRules` needs
   nothing: measured, removal of a bad name is plain ENOENT.
2. *Looking up* an unrepresentable name misses, and its length is never examined.
   In the model this needs no code at all on the happy path — such a name cannot
   be in the `Map`, so the lookup misses naturally — but `PathWalk` must skip its
   NAME_MAX check for it, because §1.4 measured `open(300 × 0xff, O_RDONLY)` as
   ENOENT where the valid control is ENAMETOOLONG.

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

- **`UnixPathTextDefect.UnpairedSurrogate` disappears**, and with it most of
  `UnixPathText.firstDefect` — the function exists to find the two ways a .NET
  string fails to be a byte string, and one of those ways stops existing. What is
  left is a NUL scan.
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

A note on ordering: **`UnixPath` and `DirectoryEntryName` must move together**
(Stage 3). `PathCursor.next` slices a `DirectoryEntryName` straight out of the
path's buffer, so one cannot be bytes while the other is a string without an
encode/decode in the middle of the resolution walk — which is the thing this
refactor exists to delete. `SymlinkTarget` and `AbsoluteUnixPath` are genuinely
separable and get their own stages.

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
- `tryToString` agrees with `UTF8Encoding(false, true).GetString` — `Some` iff
  that call does not throw — and round-trips: `tryToString >> Option.map ofString
  = Some << id`.
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

### Stage 3: `UnixPath` and `DirectoryEntryName` become bytes

**Dependencies**: Stage 1.

**Implements**: §2.1, §2.2, §2.4 (the `UnixPathText`, `PathArgumentRefusal` and
`PathCursor` deletions).

The big one. `UnixPath`, `DirectoryEntryName` and `PathCursor` wrap
`UnixByteString`; `PathArgument.parse` stops decoding and
`PathArgumentRefusal.NotUtf8` is deleted along with both of PawPrint's
`failwith`s for it; `UnixPathTextDefect.UnpairedSurrogate` is deleted; every
`toString` in a diagnostic becomes `toEscaped`. `parseOrFail : string -> _`
survives on both types so the test corpus does not churn.

`PathWalk`'s NAME_MAX check gains the §2.3(2) skip — but on `AnyBytes` only,
since there is no `StrictUtf8` flavour rule until Stage 7. On Darwin this stage
therefore leaves the walk measuring a non-UTF-8 name in UTF-16 units, which is
undefined; it must `failwith` naming Stage 7, not guess. That is reachable only
through a seed until Stage 7 lands.

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

### Stage 5: `AbsoluteUnixPath` becomes bytes

**Dependencies**: Stage 3.

**Implements**: §2.4 (`AbsoluteUnixPathError`), §2.5.

`AbsoluteUnixPathError` loses `UnpairedSurrogate`; `ContainsNul` carries a byte
offset. `getcwd` and `SystemNative_GetProcessPath` stop round-tripping through a
string.

**Correctness oracle**:

- Property: `parse >> toUtf8` round-trips for every byte string satisfying the
  absolute-path shape.
- **Differential, and only now expressible**: `chdir` into a directory whose name
  is `\xff\xfe`, then `getcwd`, yields those bytes. Measured on Linux (§1.5:
  `2F 77 6F 72 6B 2F 64 2F FF FE`). Linux-only — the directory cannot exist on
  APFS — so this is a `container`-hosted or `$DOTNET_LINUX_FRAMEWORK_DIR` test,
  and it must `Assert.Ignore` where it cannot run.
- Existing `TestAbsoluteUnixPath`, `TestProcessPath`,
  `TestEmulatedKernelCurrentDirectory` pass with only the `UnpairedSurrogate`
  cases removed.

---

### Stage 6: the oracle and the seed learn to refuse

**Dependencies**: Stages 3–5.

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

### Stage 7: APFS refuses to bind a non-UTF-8 name

**Dependencies**: Stages 3–5. (Independent of Stage 6.)

**Implements**: §1.1, §1.2, §1.3, §2.3.

The modelling gap that becomes real once bytes are representable.

- `UnixError.EILSEQ`, `platformDependent 84 92` — the numbers are already
  written down in `UnixError.fs`'s own comment on `EOVERFLOW` ("raw 84 is
  `EOVERFLOW` on Darwin and `EILSEQ` on Linux"). The PAL mapping is
  `Error_EILSEQ = 0x10019`, which exists upstream
  (`pal_error_common.h:62`), so `UnixErrorPal` needs one arm.
- `SimulatedUnixPlatform.entryNameEncoding`, per §2.3.
- The rule at its measured position in `CreatingOpenRules`, `MkDirRules`,
  `RenameRules`. Nothing in `RemovalRules`.
- `PathWalk`'s NAME_MAX skip from Stage 3 becomes real rather than a `failwith`.

**Correctness oracle** — this stage has a genuine host oracle on *both*
platforms, which is rare here, so use it:

- The §1.3 ordering table, as a test per row, each with its stated control. The
  controls are what make the rows load-bearing: `mkdir(300 × 0xff)` → EILSEQ is
  only meaningful next to `mkdir(300 × 'a')` → ENAMETOOLONG.
- The §1.2 table: every listed lookup/removal is ENOENT on a `StrictUtf8`
  flavour, and every listed binding is EILSEQ.
- Property: on an `AnyBytes` flavour, no operation ever reports EILSEQ, for any
  NUL-free byte string.
- Property: `EntryNameEncoding.StrictUtf8` admits a name iff
  `UnixByteString.tryToString` returns `Some` — the §1.1 claim that these are one
  predicate, asserted rather than assumed.
- Extend `TestVirtualFileSystemAgainstHost` with `byte[]`-declared P/Invokes so
  the host comparison covers non-UTF-8 names. This is the stage that pays that
  cost.

---

### Stage 8: documentation

**Dependencies**: Stage 7.

- `docs/divergences.md`'s directory-enumeration entry says the order among names
  is "F# ordinal (UTF-16) order" and notes that sorting by UTF-8 bytes "was
  considered … but it differs only above the BMP and buys nothing a test could
  observe". After Stage 3 the order **is** byte order, so that passage is
  rewritten — and its own argument now favours what we have.
- A new `docs/divergences.md` entry for §1.1–§1.3, with the measured tables.
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

**Stage 3 is large and cannot usefully be split.** The mitigation is that the
`string`-taking `parseOrFail` constructors survive, so the 202 test call sites
that pass literals do not change at all; the diff is concentrated in the library.
If it still comes out unreviewable, the fallback is to land `UnixByteString`
behind the existing string API first (Stage 1 + an adapter) and flip the
representation in a second pass — but that is two migrations where one will do,
and a half-finished migration is the thing to avoid.

**Stage 7 changes guest-visible behaviour on the Darwin flavour.** A guest that
today gets a `failwith` will get an errno; a guest that today creates a file
will, on the Darwin flavour, get EILSEQ. Both are corrections, but they are
behaviour changes and belong in `docs/divergences.md` (Stage 8) rather than only
in a commit message.

**The `Utf16CodeUnits` limit is only well-defined because of the ordering.** If a
future change lets a non-decodable name reach `PathLimits.nameWithinLimit` on a
`StrictUtf8` flavour, the function has no answer. It should `failwith` saying so
rather than pick one — measured, the real kernel never asks.
