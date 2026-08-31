# `SystemNative_Rename`: the last namespace mutation

The slice after directory enumeration (#1141, #1142). `open(O_CREAT)` (#1080),
`unlink` (#1129), `mkdir` (#1112), `rmdir` (#1134) and `opendir`/`readdir`
(#1141) between them cover every way a name can be *created* or *destroyed*;
`rename(2)` is the only one left that *moves* one, and it is the last operation
in the namespace family.

## The blocker set, measured

A guest calling `File.Move("/f", "/g")` aborts at:

```
Unimplemented native method (PInvokeImpl libSystem.Native!SystemNative_Rename):
System.Private.CoreLib .Sys::<Rename>g____PInvoke|105_0(*(Byte), *(Byte)) -> Int32
```

`FileSystem.Unix.cs` calls twelve distinct `Interop.Sys` functions across the
whole file — `CopyFile`, `GetLastError`, `GetLastErrorInfo`, `Link`, `LStat`,
`MkDir`, `ReadLink`, `Rename`, `RmDir`, `Stat`, `SymLink`, `Unlink` — and the
ones on the paths this slice unlocks are `LStat`, `Stat`, `GetLastErrorInfo`
and `Rename`, the first three already implemented. So:

| managed API | kernel touches | blocked on |
| --- | --- | --- |
| `File.Move(src, dst, overwrite: true)` | `Rename`; `CopyFile`+`DeleteFile` only on EXDEV | `Rename` alone |
| `File.Move(src, dst)` (no overwrite) | `LStat`, `LStat`, `Rename` | `Rename` alone *on the success path* |
| `Directory.Move` | `LStat`, `LStat`, `Rename` | `Rename` alone |
| `File.Replace` | `Rename`, `Link`, `Unlink` | also `Link` — **out of scope** |

One caveat that shapes the test plan rather than the implementation:
**`File.Move` without `overwrite` falls back to `LinkOrCopyFile` whenever the
rename does not happen**, which needs `SystemNative_Link` and
`SystemNative_CopyFile`. So the *managed* API can only exercise rename's happy
path; every refusal row has to be driven through a raw `[DllImport]` guest, as
the `unlink` and `rmdir` wiring guests already are. PawPrint has one filesystem
and therefore never answers EXDEV, so the `overwrite: true` fallback is
unreachable and `File.Move(…, overwrite: true)` is covered end to end.

**The committed follow-up slice is `SystemNative_Link`.** It is cheap —
`VirtualFileSystem.hardLink` already exists — and it is what turns
`File.Move`'s no-overwrite refusal paths from an interpreter abort into an
ordinary `IOException`. It is named here rather than left in the
undifferentiated out-of-scope list because rename creates the reachable
crash that it closes.

## The managed contract

`Interop.Rename.cs` declares two overloads onto one entry point:

```csharp
[LibraryImport(…, "SystemNative_Rename", StringMarshalling = Utf8, SetLastError = true)]
                                       int Rename(string oldPath, string newPath);
[LibraryImport(…, "SystemNative_Rename", SetLastError = true)]
                                       int Rename(ref byte oldPath, ref byte newPath);
```

Both arrive at the handler as two pointers to NUL-terminated UTF-8, so the
handler sees no difference; `MoveDirectory` uses the second (via the
`ReadOnlySpan<char>` helper) because it must trim a trailing separator before
calling. `SetLastError` is set on both, and the PAL is a bare passthrough that
retries on EINTR (`pal_io.c:1200`), so every errno below reaches managed code
unmodified.

## Measured, on both kernels

macOS 26.6 / APFS locally at uid 501, and Linux 6.x / aarch64 through
`container run --rm -v … python:3-slim`, dropped to uid 1000 (the container is
root by default, which makes every permission row vacuous) and separately left
at uid 0 for the privileged column.

Probes: `scratchpad/rename/probe{,2,3,4,5}.py`.

### The type matrix — unanimous

Source down, destination across; no trailing separators anywhere. Every row
below is the same on both kernels.

| src \ dst | file | emptydir | fulldir | symfile | symdir | dangling | fifo | absent |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| **file** | ok | EISDIR | EISDIR | ok | ok | ok | ok | ok |
| **emptydir** | ENOTDIR | ok | ENOTEMPTY | ENOTDIR | ENOTDIR | ENOTDIR | ENOTDIR | ok |
| **fulldir** | ENOTDIR | ok | ENOTEMPTY | ENOTDIR | ENOTDIR | ENOTDIR | ENOTDIR | ok |
| **symfile** | ok | EISDIR | EISDIR | ok | ok | ok | ok | ok |
| **symdir** | ok | EISDIR | EISDIR | ok | ok | ok | ok | ok |
| **dangling** | ok | EISDIR | EISDIR | ok | ok | ok | ok | ok |
| **fifo** | ok | EISDIR | EISDIR | ok | ok | ok | ok | ok |
| **absent** | ENOENT | ENOENT | ENOENT | ENOENT | ENOENT | ENOENT | ENOENT | ENOENT |

Read off it: the *only* thing that matters about the source is whether it is a
directory, and the same of the destination. A non-directory over a directory is
EISDIR; a directory over a non-directory is ENOTDIR; a directory over a
non-empty directory is ENOTEMPTY; a symlink is a non-directory *whatever it
points at* (`symdir` behaves as `symfile` throughout), because the walk is
`SymlinkPolicy.NoFollowFinal` on both sides.

`rename(lf, f)` where `lf -> f` succeeds and leaves `f` a symlink pointing at
itself, on both. That is the mechanical replacement with no special case, and
it is worth a test precisely because it looks like one.

### Structural rows — unanimous except where marked

| row | Linux | Darwin |
| --- | --- | --- |
| `rename(a, a/b)`, `rename(a/b, a/b/c/x)` — into own subtree | EINVAL | EINVAL |
| `rename(a, link/inner)` where `link -> a/b` | EINVAL | EINVAL |
| `rename(a, a/b/../c)` — dest parent resolves back to `a` | EINVAL | EINVAL |
| `rename(a/b/c, a/x)` — out of own subtree | ok | ok |
| `rename(a, ab)` — prefix, not ancestor | ok | ok |
| `rename(d, d)` — self, file or directory | ok, nothing stamped | ok |
| `rename(f, g)` where `g` is a hard link to `f` | ok, **both names survive**, nothing stamped | ok |
| `rename(f, ./f)` | ok, nothing stamped | ok |
| source absent | ENOENT | ENOENT |
| destination's parent absent | ENOENT | ENOENT |
| destination's parent is a regular file | ENOTDIR | ENOTDIR |
| destination component 300 bytes long | ENAMETOOLONG | ENAMETOOLONG |
| **destination's parent is an orphan** (cwd `rmdir`'d out from under the guest) | **ENOENT** | **ENOENT** |
| **`rename(d/., x)`, `rename(d/.., x)`, `rename(x, d/.)`, `rename(x, d/..)`** | **EBUSY** | **EINVAL** |
| **`rename("/", x)`** | **EBUSY** | **EISDIR** |
| **`rename(dir, "/")`, `rename(dir, "e/..")`** | **EBUSY** | **EINVAL** |
| **`rename(file, "/")`** | **EBUSY** | **EISDIR** |

The subtree rule is on *inodes*, not on path text: the `link -> a/b` row and the
`a/b/../c` row both refuse, and neither destination path has the source as a
textual prefix. The rule is "the source is the destination's parent directory,
or an ancestor of it".

Note the two Darwin root rows disagree with each other by *source kind*:
`rename(dir, "/")` is EINVAL while `rename(file, "/")` is EISDIR, where the
source-side `rename("/", x)` is EISDIR. Recorded as measured rather than
rationalised into one rule.

**An orphaned directory is necessarily empty**, because `rmdir` refuses a
populated one — so the only rename-and-orphan shape that exists is "the
destination's parent is the orphan", and it answers ENOENT on both, exactly as
`mkdir`, `open(O_CREAT)` and `symlink` already do (#1134's decision B). A
*source* inside an orphan cannot exist, so it needs no rule. This makes rename
the third guest-reachable operation that adds a name, and #1134's plan says
there are two; that sentence goes stale with this slice.

### Permissions — unanimous

| row | both, unprivileged |
| --- | --- |
| write missing on the source's parent | EACCES |
| write missing on the destination's parent | EACCES |
| search missing on either parent | EACCES |
| moving a **directory to a different parent**, write missing on the moved directory itself | EACCES |
| renaming a directory **within** its parent, write missing on the moved directory | ok |
| source absent under a parent that cannot be searched | EACCES, not ENOENT |
| source absent under a parent that can be searched but not written | ENOENT, not EACCES |
| a sticky destination parent | never refuses — see below |

The directory-to-a-new-parent row is the `..` rewrite: the moved directory's own
`..` entry changes, so the kernel demands write on it. Within one parent nothing
in the directory changes and no permission is asked for. Unanimous, so it is not
a flavour fact.

The sticky bit can never refuse, for the reason `RemovalChecks.lacksWrite`
already records: POSIX permits the operation when the caller owns the file *or*
the directory, and PawPrint reports `Kernel.UserId` as every inode's `st_uid`,
so one identity owns both.

**Privileged (uid 0). Measured on both**, against one script
(`probe-root.py`, which touches nothing outside a fresh temp directory and in
particular never names "/"):

| row | uid 501 / 1000 | uid 0, both kernels |
| --- | --- | --- |
| every permission row above | EACCES | **ok** |
| src parent unwritable + dst is a directory | EACCES (Linux) / EISDIR (Darwin) | EISDIR |
| dst parent unwritable + dst is a directory | EACCES (Linux) / EISDIR (Darwin) | EISDIR |
| dst parent unwritable + dst dir non-empty | EACCES (Linux) / ENOTEMPTY (Darwin) | ENOTEMPTY |
| same-inode no-op + parent unwritable | ok (Linux) / **EACCES (Darwin)** | **ok** |
| self-rename + parent unwritable | ok (Linux) / **EACCES (Darwin)** | **ok** |
| directory self-rename + parent unwritable | ok (Linux) / **EACCES (Darwin)** | **ok** |
| into own subtree + parent unwritable | EINVAL | EINVAL |
| src absent + src parent unsearchable | EACCES | ENOENT |
| src parent unsearchable × dst parent absent | EACCES | ENOENT |
| dst name 300 bytes + dst parent unwritable | ENAMETOOLONG | ENAMETOOLONG |

Privilege does exactly one thing — it stops the permission arm firing — and
nothing reorders. The consequence worth stating, because Stage 2's tests can
check it: **at uid 0 the two flavours agree on every measured row.** The whole
of rename's ordering divergence lives in the unprivileged column, and a
privileged guest cannot tell the two kernels apart.

An earlier draft *derived* the Darwin column from the Linux one. The derivation
turned out to be right, and was still worth replacing: the three rows where
Darwin answers EACCES and Linux answers `ok` are exactly the rows it would have
got wrong had privilege participated in the ordering rather than
short-circuiting one arm of it.

### Ordering — **this is where the flavours split**

Two checks that both refuse, and which errno comes out (unprivileged):

| row | Linux | Darwin |
| --- | --- | --- |
| source's parent unwritable **and** destination is a directory | **EACCES** | **EISDIR** |
| destination's parent unwritable **and** destination is a directory | **EACCES** | **EISDIR** |
| destination's parent unwritable **and** destination is a non-empty directory | **EACCES** | **ENOTEMPTY** |
| same-inode no-op (`f` and a hard link to it) **and** parent unwritable | **ok** | **EACCES** |
| self-rename (`f` to `f`) **and** parent unwritable | **ok** | **EACCES** |
| directory self-rename **and** parent unwritable | **ok** | **EACCES** |
| same-inode no-op **and** a trailing separator on the destination | ENOTDIR | ENOTDIR |
| self-rename **and** a trailing separator on the destination | ENOTDIR | ENOTDIR |
| into own subtree **and** parent unwritable | EINVAL | EINVAL |
| into own subtree **and** destination non-empty | EINVAL | EINVAL |
| non-directory source **and** non-empty directory destination | EISDIR | EISDIR |

So the two orderings, above the arms they agree on, are:

* **Linux**: trailing-separator demand → same-inode no-op → permission → type.
* **Darwin**: trailing-separator demand and type → permission → same-inode no-op.

EINVAL (the subtree rule) beats everything below it on each.

### Which walk runs first — **also divergent**

Rename is the first syscall PawPrint models that resolves **two** paths, so
"which failure wins when both paths are bad" is a new question, and the two
kernels answer it differently. Measured with pairs that *disagree* — a pair
that answers the same errno either way proves nothing:

| row | Linux | Darwin |
| --- | --- | --- |
| source absent **×** destination's parent is a regular file | **ENOTDIR** | **ENOENT** |
| source name 300 bytes **×** destination's parent absent | **ENOENT** | **ENAMETOOLONG** |
| source absent **×** destination name 300 bytes | ENOENT | ENOENT |
| source's parent unsearchable **×** destination's parent absent | EACCES | EACCES |
| source's parent unsearchable **×** destination's parent absent, **at uid 0** | ENOENT | ENOENT |
| source's parent is a regular file **×** destination is a directory | ENOTDIR | ENOTDIR |

The Linux rows are only consistent with a **four-phase** order — resolve the
source's *parent*, resolve the destination's *parent*, look the source up, look
the destination up (which is where a name's length is checked). That is the
shape of `do_renameat2`, which calls `filename_parentat` on each path before
looking either final component up:

* `nope` × `g/x` where `g` is a file: the destination *parent* resolution fails
  ENOTDIR before the source is ever looked up.
* `<300 bytes>` × `nodir/x`: the destination parent resolution fails ENOENT
  before the source's final component is length-checked.
* `nope` × `<300 bytes>`: both parents resolve, then the source lookup answers
  ENOENT before the destination's length is checked.
* the uid-0 row: with the search check passing, the source parent resolves, and
  the destination parent's ENOENT surfaces — which pins the *order* of the two
  parent resolutions, not merely that they precede the lookups.

Darwin's rows are consistent with the simpler order: resolve the source
completely, then the destination completely.

This also **scopes a claim the first draft of this plan overstated**:
ENAMETOOLONG does not "beat everything". It was measured only *within* one
path's own walk (`<300 bytes>` destination beats an unwritable destination
parent). Across the two paths it loses to Linux's earlier phases.

### Trailing separators — **destructive divergence, as for `rmdir`**

The walk is `unlink`'s walk, run twice: `SymlinkPolicy.NoFollowFinal` on both
kernels, with `TrailingSeparatorPolicy.Ignore` on Linux and `Demand` on Darwin.
Everything below follows from that pair plus one verdict arm per flavour.

**Linux** never traverses a final symlink, and the verdict enforces the demand
from `Resolution.TrailingSeparatorDemanded`:

* a separator on the **source** demands the source be a directory — a regular
  file, a symlink (to anything) and a dangling link are all ENOTDIR;
* a separator on the **destination** demands that *the source* be a directory
  **and** that any existing destination be one. `rename(f, absent/)` is ENOTDIR
  (the source is not a directory) and `rename(d, ld/)` with `ld -> realdir` is
  ENOTDIR (the existing destination is a link, and Linux will not follow it).

**Darwin** traverses the final symlink when a separator is present, and then
demands a directory:

* `rename("s/", "moved")` with `s -> real` **moves `real`**, leaving `s` a
  dangling link;
* `rename("src", "s/")` with `s -> real` **replaces `real`** with `src`;
* a separator on the destination additionally demands that it *exist*:
  `rename(f, "absent/")` is ENOENT where every other kernel row is a type error;
* the source having a separator imposes nothing on the destination, so
  `rename(f, "d/")` is the ordinary EISDIR rather than ENOTDIR.

Both columns are measured at rename's own scale, so this dispatches on the
flavour rather than failing loudly — the rule `Resolution.FinalSymlinkFollowed`
states, satisfied the way `rmdir` satisfies it. The two choices destroy
different objects, which is exactly why an unconditional column would be wrong.

### Timestamps, link counts and identity — unanimous

| row | answer |
| --- | --- |
| the moved inode | `ctime` moves; `mtime` and `atime` do not — file or directory, same parent or different |
| both parents | `mtime` **and** `ctime` move; `atime` does not |
| a rename within one directory | that one directory's `mtime` moves, and the moved inode's `ctime` moves |
| a displaced destination **file**, watched through a surviving hard link | `ctime` moves, `mtime` does not, `st_nlink` drops by one — exactly `UnbindTargetEffect.LostALink` |
| a displaced destination **empty directory**, watched through a held descriptor | `ctime` moves, `mtime` does not — the *same* answer, and measured separately rather than generalised, because `rmdir` disagrees: there Darwin leaves the removed directory's inode alone (`RmDirRules.RemovedDirectoryEffect` is `Untouched`) where Linux stamps it. Under `rename` both kernels stamp, so no per-flavour effect parameter is needed. (`st_nlink` on it: Darwin keeps 2, Linux drops to 0 — the `rmdir` shape again, and of no consequence since `st_nlink` is derived and unreported.) |
| a no-op (self, or two names for one inode) | **nothing is stamped at all**, and both names survive |
| directory moved to a new parent | source parent `st_nlink` −1, destination parent +1, and `..` is the new parent |
| directory renamed within one parent | parent `st_nlink` unchanged |
| a descriptor held over a moved directory | still lists its entries |
| the current directory inside a moved directory | follows; `getcwd` reports the new path |

The link counts and the `getcwd` row need no code: `st_nlink` is derived from
the binding graph rather than stored, and #1126 holds the current directory as
an inode rather than as a name to re-walk. They are tests, not work.

## Design

### Where each fact lives

Nothing here is a `KernelConfig` fact: two machines running one kernel image
agree on every row above. The flavour-dependent rows are
`SimulatedUnixPlatform` facts and go in a `RenameRules` record beside
`MkDirRules`, `UnlinkRules` and `RmDirRules`:

```fsharp
type RenameRules =
    {
        /// `Ignore` on Linux, `Demand` on Darwin — `unlink`'s walk exactly,
        /// and for the same reason. One field for both walks: measured, the
        /// source path and the destination path are resolved under the same
        /// policy on each kernel.
        TrailingSeparator : TrailingSeparatorPolicy
        /// Which path is resolved first, and how far, before the other is
        /// looked at. Linux interleaves — both parents, then both final
        /// components — while Darwin finishes the source before starting the
        /// destination. Not derivable from `TrailingSeparator`, and
        /// guest-observable: `rename(absent, file/x)` is ENOTDIR on Linux and
        /// ENOENT on Darwin.
        WalkOrder : RenameWalkOrder
    }
```

with `RenameRules.linuxVerdict` / `darwinVerdict` holding the two arm orderings,
for the reason `UnlinkRules.verdict` gives: what diverges is the *order* of
arms, which a record of flags cannot express without inventing a flag per pair.
`WalkOrder` is a field rather than part of the verdict because it decides which
resolutions are *performed at all*, before any verdict has two `Resolution`s to
judge.

`FinalNavigation`'s three cases already carry what the `.`/`..`/`/` rows need,
and rename is the fourth operation to want them — now in *both* path positions,
which is new, and which the destination-side rows show is not a mirror of the
source side (Darwin answers EISDIR for a `/` source and EINVAL for a `/`
destination when the source is a directory).

### The verdict

```fsharp
[<RequireQualifiedAccess>]
type RenameVerdict =
    /// Answer the guest with this errno.
    | Refuse of error : UnixError
    /// Both paths name the same inode. Succeed, and change nothing at all —
    /// not a binding, not a timestamp. Its *position* in the arm ordering is
    /// one of the two things the flavours disagree about, which is why it is a
    /// case here rather than a short-circuit inside the mover.
    | NoOp
    /// Move `sourceName` out of `sourceDirectory` and bind it as
    /// `destinationName` in `destinationDirectory`, displacing whatever is
    /// bound there.
    | Move of
        sourceDirectory : InodeNumber * sourceName : FileName *
        destinationDirectory : InodeNumber * destinationName : FileName
```

`NoOp` is a case no prior `*Verdict` DU has needed. It exists because Linux
answers success for a no-op whose parent the caller may not write, and Darwin
answers EACCES for the same call: the two can only be told apart by where the
no-op check sits in the chain, so it cannot be hoisted out of the verdict into
either the handler or the graph primitive.

### The graph primitive

`VirtualFileSystem` gains two functions:

```fsharp
/// Whether `candidate` is `root` itself or lies beneath it, by walking
/// `DirectoryContent.Parent` up to the filesystem root.
val isWithinSubtree : root : InodeNumber -> candidate : InodeNumber -> VirtualFileSystem -> bool

/// What a rename displaced, if anything. A named record rather than a bare
/// `InodeNumber option` in a tuple, so the caller cannot mistake it for the
/// inode that moved.
type RenameOutcome =
    {
        /// The inode that lost its last-named binding at the destination, for
        /// the caller to reap if nothing holds it. `None` when the destination
        /// name was free.
        Displaced : InodeNumber option
    }

val rename :
    sourceDirectory : InodeNumber -> sourceName : FileName ->
    destinationDirectory : InodeNumber -> destinationName : FileName ->
    now : UnixTimestamp -> vfs : VirtualFileSystem ->
        Result<RenameOutcome * VirtualFileSystem, UnixError>
```

`rename` does the reparenting (`DirectoryContent.Parent` on the moved inode when
it is a directory and the parent changed) and the stamps. It does **not** free
the displaced inode, for the reason `unbind` does not: whether anything still
holds it is a fact about the descriptor table, which this module cannot see.

The moved inode is deliberately *not* returned. It never loses its last name, so
no caller has to decide anything about it, and descriptors key on inode numbers,
which a rename does not change. Returning it would be a value with no consumer.

**It is mechanical about policy but not about graph soundness.** Four conditions
would leave a filesystem no kernel could produce, and the primitive refuses them
itself — with a `failwith` naming the condition, in the style of `unbind`'s
broken-graph arm, because the verdict is supposed to have excluded them:

* the two paths **naming one inode** — `rename(2)`'s no-op, which changes
  nothing at all. Its *position in the arm ordering* is itself one of the
  flavour divergences, so it belongs to the verdict rather than to a
  short-circuit in the primitive.
* a **populated directory** at the destination. Reaping it would strand its
  children as `UnreachableFromRoot`: `forgetIfUnheld` climbs parents, not
  children.
* a destination directory **within the source's own subtree**. The result is a
  detached cycle.
* a destination directory whose own **last name has gone**. Binding into an
  orphan strands the moved inode — it keeps a name, so nothing reaps it, and no
  path reaches it — and a *pinned* orphan is a perfectly sound input, which is
  what makes this the primitive's problem rather than the caller's. The verdict
  owes ENOENT, as it already does for `mkdir`, `open(O_CREAT)` and `symlink`.
  This refusal is also what keeps `isOrphanedDirectory`'s stated invariant true:
  an orphan is empty because `rmdir` refuses a populated directory *and* nothing
  can afterwards put an entry into one. Rename was the missing half of that
  argument, and this condition was found by review rather than by the sweep —
  see the generator note in Stage 1.

All are guest-*reachable* conditions whose errno and ordering are the verdict's
business (ENOTEMPTY and EINVAL, at measured positions); what the primitive owes
is that no caller can reach past the verdict and corrupt the graph. Making them
refusals rather than preconditions also makes Stage 1's central property
*total*: for **all** quadruples, either an error with the filesystem unchanged,
or a filesystem `checkInvariants` accepts. Leaving them as preconditions would
push the word "legal" into the property, and the test generator would have to
re-implement the verdict to honour it.

### Why one primitive rather than `unbind` + `hardLink`

Because the composition cannot express a directory move at all: `bind` is
`private`, so there is no public way to attach a directory to a new parent, and
`hardLink` refuses a directory with EPERM by design (the `EPERM` arm at
`VirtualFileSystem.fs:1746`).

For a **non-directory** source the composition is not merely close but *exact* —
`[unbind destination LostALink]?` then `hardLink` then `unbind source LostALink`
produces the identical graph, timestamps included, because every stamp in one
rename uses the same `now` and stamping twice at one instant equals stamping
once. That is what makes it a good reference oracle for half the domain and
useless for the other half, and it is the honest form of the argument. (An
earlier draft of this plan claimed the composition had an unreachable-subtree
window and disagreeing timestamps. Neither is true: `hardLink`-then-`unbind`
never detaches anything, and the file-case timestamps agree exactly.)

## What implementing Stage 2a changed — read this before the tables above

The tables above were measured by an earlier session and are right about
everything they say. They are also **incomplete in three ways that each change
the design**, and one section of them is **wrong**. Found while implementing the
verdict (2026-08-23); the probe that settles all of it is committed at
`docs/probes/rename/`, with four measured columns beside it.

**Three findings the tables do not contain:**

1. **The flavours disagree about *which object* a check is about**, not only
   about the order of checks. When a directory replaces a directory, Darwin
   consults the write bit of the **displaced directory** and never looks at the
   directory holding it; Linux does the exact opposite. Measured eight ways with
   a control. No record of flags can express this, which settles the
   "two whole functions" question the design section leaves open.
2. **There are three permission checks at three positions**, not one: the two
   parents' write bits, and the moved directory's own write bit for the `..`
   rewrite — which sits *below* the type rule on both kernels, where the
   parents' sit above it on Linux.
3. **Linux's subtree EINVAL beats both write checks.**

**The section that is wrong: "Trailing separators".** It says a separator on the
destination "demands that *the source* be a directory **and** that any existing
destination be one". The second half does not exist. Only an unwritable parent
can see it, because with a writable one both readings answer ENOTDIR — the
demand if it asked, and the ordinary type rule below if it did not:
`rename(d, "q/l/")` with `l` a symlink to a directory is ENOTDIR when `q` is
writable and **EACCES** when it is not.

**The `.`/`..`/`/` rows needed three attempts and are not what they look like.**
The plan records `rename("/", x)` as EISDIR on Darwin and stops there. Whether
Darwin gives the *root* its own arm — as its `unlink` and `rmdir` both do — is a
separate question, and the obvious measurement is masked: a filesystem root that
is not `/` is a *mount* root, and renaming one is liable to EXDEV. The
discriminator turned out to be neither `.` nor `..` but whether the source's and
destination's parent directories coincide; where EXDEV stays quiet the root
answers EINVAL for both, exactly as an ordinary directory does. **The root is
not a special case**, and no arm exists for it. See `docs/probes/rename/README.md`.

**One row is nondeterministic on macOS** and must never enter the host-equality
tier: `rename("l/", "g")` with `l` a symlink to a regular file and an
**absolute** target succeeds 1–20% of the time, moving the link's target. A
relative target is stable, which is why the corpus uses one.

**Stage 2 split into three PRs**, not the two this plan proposes, and
`RenameRules` carries only `TrailingSeparator` for now:

- **Stage 2a** (done): `RenameRules`, `RenameVerdict`, both verdicts,
  `TestRenameRules`, and the probe. No guest can see any of it.
- **Stage 2b** (done): `PathWalk.resolveParent` /
  `completeResolution` as a *pure* refactor. See below for what implementing it
  changed.
- **Stage 2c**: `WalkOrder`, the handler, the guests.

**Why 2b exists**, which this plan does not anticipate: Linux's four-phase walk
order cannot be built out of `resolveFull`, which does the parent walk and the
final lookup in one indivisible call. `<300 bytes>` × `nodir/x` is ENOENT while
`nope` × `<300 bytes>` is ENOENT the other way round, and one `Result` cannot say
which phase failed. Synthesising `dirname` and resolving it as its own path
looks cheap and is wrong: `src parent unsearchable × dst parent absent` is
EACCES, so the parent's own search check happens in phase 1, which a `dirname`
resolution does not perform.


## What implementing Stage 2b changed

**The property this plan asks for cannot exist.** It names "a property
asserting they compose back to `resolveFull`", but `resolveFull` is now
*defined* as the composition, so any such property is true by construction and
sees nothing. Making the two halves a second implementation beside the old one,
so that a property could relate them, would leave two versions of the truth
alive in shipped code. The oracle therefore comes from outside: the pre-split
walk is transcribed into `TestResolutionSplit.referenceResolveFull` against
public API alone, and both a fixed cross-product and a generated corpus assert
the new composition agrees with it. That file's docstring says not to tidy the
transcription up, since its whole value is being the old code.

**`completeResolution` is not one lookup.** Under a policy that follows the
final symlink, the lookup splices a target into the pathname buffer and the walk
continues to a *different* directory — so the second half re-enters the first
and finishes again. That is also why `PausedResolution` is opaque: there is no
"the parent directory" to read off it, because which directory the name is
looked up in is not settled until the second half has run.

**Where the pause falls is measured, and both sides of it are.** The holding
directory's search bit is checked *above* the pause and the final name's own
length *below* it. The first is not an artefact of where the split fell:
Linux's `link_path_walk` calls `may_lookup` at the top of each iteration, before
it discovers whether the component it is about to hash is the last one, and
`rename(unsearchable/f, absent/x)` is EACCES on both kernels rather than the
ENOENT a below-the-pause check would give. The second is this plan's own
`<300 bytes>` × `nodir/x` row.

**The paused value carries the rules and the filesystem**, not just a position,
so a resumption cannot be given different limits, a different privilege, a
different policy pair or a different filesystem. Those states are unrepresentable
rather than checked.

**Mutation-tested, 11 mutations and no survivors** — including both directions
the pause point could move. Three of the eleven do not fail so much as *hang*:
removing the symlink traversal count from either half, or disabling ELOOP,
removes the bound that makes the walk terminate at all, and a self-referential
link then recurses forever. Budget a per-mutation fuel cap that kills the whole
process group; a plain `subprocess` timeout leaves the test host orphaned and
spinning.


## The option set for splitting this up

Rename is bigger than any prior slice in this workstream: `unlink` was 3106
lines and `rmdir` 3336, and rename is both of them plus the subtree rule, the
reparenting, a second walk and a walk-order divergence. Three genuinely
different ways to cut it:

1. **One PR.** Smallest total cost, worst review: the arm ordering, the walk
   ordering, the graph surgery and the flavour dispatch all land together, and a
   reviewer has to hold four measured tables and the graph invariants in mind at
   once.

2. **Split at the graph/policy line the codebase already draws** — the
   `unbind`-is-mechanical, `UnlinkRules`-is-policy boundary. Stage 1 is
   `VirtualFileSystem.rename`, `RenameOutcome` and `isWithinSubtree` with unit,
   property and host-equality oracles and no guest able to see them; Stage 2 is
   the verdict, the walk order, the handler and the guests. Precedent:
   #978/#979/#983 landed the filesystem graph with no guest-observable behaviour
   at all, and #990 made it visible. Blast radius of getting Stage 1 wrong is
   contained — one new function, with an invariant checker already written for
   it and a reference implementation available for half its domain.

3. **Split by source kind**: non-directory sources first, `failwith` on a
   directory source, directories second. Rejected. It splits the verdict's
   *ordering* across two PRs, so Stage 2 re-opens every arm Stage 1 wrote and
   the ordering has to be reviewed twice; it leaves `Directory.Move` — an
   entirely ordinary call — crashing the interpreter for a release cycle; and
   the type matrix shows that "is the source a directory" is a single
   discriminator threaded through every arm, so the two halves are not
   separable concerns.

**Chosen: option 2.** Two stages, and the first is a self-contained PR against
`main` rather than a stack, so it gets CI. The three parts of Stage 1's contract
that Stage 2 would otherwise re-open — where the no-op lives, what the primitive
does with a populated or in-subtree destination, and the return type — are
pinned above rather than left to be discovered.

If Stage 2 turns out to be as large as this plan suggests, it splits again along
the same line the codebase already draws: `RenameRules` plus `TestRenameRules`
first, handler plus guests second. Not committed to up front — `rmdir` landed
the equivalent as one PR — but named so the decision is cheap if it is needed.

---

Implement this plan with each stage on its own branch, stacked as necessary on
previous branches, so that a reviewer can review each branch in isolation.

## Stage 1: the graph primitive

**Dependencies**: none.

**Implements**: "The graph primitive" above.

`VirtualFileSystem.isWithinSubtree`, `RenameOutcome` and
`VirtualFileSystem.rename`, including the four graph-soundness refusals and the
timestamp rules. No `EmulatedKernel` change, no handler, no guest.

**Correctness oracle**:

- Property (total, no "legal" precondition): for **any** filesystem and **any**
  quadruple, `rename` either fails with the filesystem unchanged, or succeeds
  and `checkInvariants` reports no defect. This is the property #978 built the
  invariant checker for, and the four soundness refusals are what make it total.
- Generator alphabet, and the reason to state it: the sweep's filesystems must
  *contain* orphans. `filesystemGen` only ever creates, so nothing it builds has
  one — and "the destination directory has lost its last name" is invisible to
  every other check, so the sweep was structurally blind to it. The scenario
  generator orphans an empty directory (the only kind `rmdir` could have
  produced), pins it as a descriptor would, and biases the destination towards
  it, because a class that rare is a flake rather than a guard.
- Property: a successful rename preserves the inode set exactly — none created,
  none destroyed (the displaced one is *returned*, not freed).
- Property (reference implementation): for a **non-directory** source, including
  when the destination name is occupied, the resulting filesystem is **exactly
  equal**, `Times` included, to `[unbind destination LostALink]?` then
  `hardLink` then `unbind source LostALink`. Not "modulo timestamps": per the
  design section the two agree exactly, so any inequality is a discovery rather
  than a nuisance. It does not extend to directory sources, which is the point
  of having the primitive at all.
- Property: `isWithinSubtree root candidate` agrees with an independent oracle
  built from `pathOfDirectory` — `root`'s absolute path is a **component-wise**
  prefix of `candidate`'s. Component-wise, not string-wise: the corpus's own
  `rename(a, ab)` row is the counterexample, since `/a` is a string prefix of
  `/ab`. Two computations over different representations, per
  [[invariant-tests-need-an-outside-oracle]]. `pathOfDirectory` answers `None`
  for an orphan, so the generator either excludes orphans or the oracle handles
  the case explicitly rather than treating `None` as "not within".
- Generator alphabet, stated rather than left to chance: cross-parent directory
  moves, occupied destinations (both kinds), destinations one step outside the
  source's subtree, and same-parent renames must each appear with non-vanishing
  frequency, asserted with a coverage check. A generator that never moves a
  directory across parents cannot see a wrong reparent, and a generator that
  never occupies a destination cannot see a missing `LostALink` stamp. Cf.
  [[generator-alphabet-can-hide-divergence]].
- Unit, against **literals** rather than against the graph under test: after a
  cross-parent directory move, the entry is present in the destination
  directory, absent from the source directory, and the moved directory's
  `Parent` is the destination directory. A wrong-but-self-consistent reparent —
  bind into the wrong directory *and* set `Parent` to match — passes
  `checkInvariants` and is invisible to the non-directory reference property, so
  this is the arm that catches it.
- Host-equality (`TestVirtualFileSystemAgainstHost`): the before/after **tree
  delta** of a successful rename, **plus every directory's ".."** through
  `realpath`. The second is the strongest wrong-reparent oracle available — a
  rename that binds the moved directory into the wrong parent *and* sets its
  `Parent` to match is self-consistent, passes `checkInvariants`, and is
  invisible to the non-directory reference property.

  The timestamp and `st_nlink` rows are **not** in this tier, and cannot be: no
  .NET API exposes `st_ctime`, and this fixture deliberately avoids `struct
  stat` (platform-specific layout, versioned symbol on macOS). They are probe-
  measured and pinned by unit tests instead, which is what `unlink` and `rmdir`
  already do with their own timestamp facts. Stating the tier a fact *can* be
  checked in is part of the fact.
- Mutation: each stamp, the reparenting, and each of the four soundness refusals
  must have a test that goes red when it is removed. Per the `mutation-testing`
  skill; the reparenting especially, since `..` has no other reader in this
  stage.

## Stage 2: the verdict, the walk order, the handler and the guests

**Dependencies**: Stage 1.

**Implements**: everything else above.

`RenameRules` (with `TrailingSeparator` and `WalkOrder`) on
`SimulatedUnixPlatform`; `RenameVerdict`; `RenameRules.linuxVerdict` and
`darwinVerdict`; `SystemNative_Rename` in `NativeSystemNative.fs`, including the
orphaned-destination-parent arm and the reap-and-pin integration #1129/#1134
already wrote for a displaced destination (with its transitive ancestor pinning
when the displaced destination is a directory).

**Correctness oracle**:

- `TestRenameRules.fs`: the ordering tables above, arm by arm, both flavours, in
  the style of `TestUnlinkRules.fs` and `TestRmDirRules.fs`. Every row the two
  orderings disagree on gets a test under *each* flavour — neither alone can
  catch a verdict that hardcodes one column.
- A test per row of the walk-order table, both flavours, built from the
  *disagreeing* pairs only: a pair that answers the same errno either way cannot
  tell the two orders apart. Cf. [[ordered-guards-need-a-disagreeing-input]].
- Host-equality: the type matrix and the unanimous structural rows.
- `sourcesPure/RenameSeeded.cs`: the unanimous rows through the differential
  oracle, plus `File.Move(overwrite: true)` and `Directory.Move` end to end.
- `sourcesImpure/RenameWiring{Linux,Darwin}Seeded.cs`: the ordering divergence,
  the walk-order divergence and the trailing-separator divergence, driven
  through a raw `[DllImport]` because no differential test can reach them — the
  real runtime answers for the machine it is on, not for the flavour the kernel
  claims. Registered with `Umask` and `UserId` set away from their defaults, so
  a handler that reads a `KernelConfig` field it should not is caught.
- `AssertTerminalState`: a displaced destination whose last name has gone is
  reaped iff nothing holds it, and the filesystem is left with no orphan.
- Mutation: swapping the two verdicts' arm orders must fail a test under each
  flavour; making the walk unconditionally `Demand` must fail a Linux row;
  making `WalkOrder` unconditionally Darwin's must fail a Linux row.

## Deliberately out of scope

- `SystemNative_CopyFile`, and therefore `File.Replace`. (`SystemNative_Link` is
  the committed follow-up rather than merely out of scope — see above.)
- `renameat2`, `RENAME_EXCHANGE` and `RENAME_NOREPLACE`: CoreLib never calls
  them.
- EXDEV. PawPrint models one filesystem, so no path can produce it; the arm does
  not exist rather than existing and being unreachable.
- Whether a directory stream open across a rename of one of its entries sees the
  old name or the new one. POSIX leaves it unspecified and #1141 already chose a
  name cursor; this slice inherits that choice and adds no rule.
