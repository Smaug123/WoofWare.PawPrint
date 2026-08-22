# `SystemNative_Unlink`

Issue #956's next slice, and the first operation in this codebase that destroys
anything.

Everything under "Measured" was probed on macOS 26.6/APFS at uid 501 and 0, and
Linux 6.x arm64/overlayfs (via `container`) at uid 1000 and 0, one fresh tree per
row, on 2026-08-21, and re-measured against this implementation before the
verdicts were written.

## Measured

`SystemNative_Unlink` (pal_io.c:368) is an EINTR-retrying `unlink(2)` and
nothing else, taking a UTF-8 `const char*` — so the guest-side stub is the same
shape as `SystemNative_MkDir`'s path argument.

### The walk

`SymlinkPolicy.NoFollowFinal` on both platforms; the trailing separator splits by
flavour into policies that already exist.

| | Linux | Darwin |
| --- | --- | --- |
| `TrailingSeparatorPolicy` | `Ignore` | `Demand` |

Linux's `do_unlinkat` takes a parent and a name and then inspects the byte after
the name, so the separator neither dereferences a final symlink nor is enforced
by the walk: it is reported on `Resolution.TrailingSeparatorDemanded` and
enforced by the verdict. Darwin's `namei` resolves it like any other lookup.

The row that proves it is `unlink("lroot/")` with `lroot -> "/"`: **ENOTDIR** on
Linux, which cannot have traversed the link, against **EISDIR** on Darwin, which
did.

This **falsified the prediction written on `TrailingSeparatorPolicy.Ignore`**,
which said a deletion slice would force that DU into a two-axis product because
Linux wants no-follow *with* the directory demand. It does want that, but the
demand is a verdict-level fact rather than a walk-level one, so the DU stays
one-dimensional. That docstring is corrected in this change rather than acted on.

### The orderings

**Linux** — directory target (any `FinalNavigation`) → EISDIR; free name →
ENOENT; trailing separator → EISDIR if a directory else ENOTDIR; parent not
writable → EACCES; target is a directory → EISDIR; else remove.

**Darwin** — `FinalNavigation.Root` → EISDIR; the root reached by `.`/`..` →
EBUSY; any other directory target → EPERM; free name → ENOENT; target is a
directory → EPERM; parent not writable → EACCES; else remove.

The rows that pin the ordering, each of which differs from the naive guess:

| path (`nowrite` is 0o555, `nosearch` is 0o666) | Linux | Darwin |
| --- | --- | --- |
| `d`, `dfull`, `.`, `..`, `./`, `d/.`, `d/..` | EISDIR | EPERM |
| `/` | EISDIR | EISDIR |
| `lroot/` | ENOTDIR | EISDIR |
| `/.`, `/..`, `lroot/.`, `lroot/..` | EISDIR | **EBUSY** |
| `nowrite/kdir` | EACCES | EPERM |
| `nowrite/kdir/` | EISDIR | EPERM |
| `nowrite/kid` | EACCES | EACCES |
| `nowrite/kid/`, `nowrite/klink/` | ENOTDIR | ENOTDIR |
| `nowrite/nx`, `nowrite/nx/` | ENOENT | ENOENT |
| `nosearch/kid`, `nosearch/kid/` | EACCES | EACCES |
| `f` (mode 000, writable parent) | OK | OK |
| `ld/` | ENOTDIR | EPERM |
| `dang/` | ENOTDIR | ENOENT |
| `cyc/` | ENOTDIR | ELOOP |
| 300-byte name, with or without a separator | ENAMETOOLONG | ENAMETOOLONG |

### Facts both kernels agree on

- **Timestamps.** The directory losing an entry gets `mtime` *and* `ctime`; the
  inode losing a link gets `ctime` only; no `atime` moves. Identical for a
  survivor with links left and for one dropping to zero, measured through a held
  descriptor's `fstat`.
- **A held descriptor keeps the inode alive.** After `unlink`, `fstat` still
  reports it and `read` still returns its bytes.
- **The sticky bit needs no code.** POSIX permits the removal when the caller
  owns the file *or* the directory, and PawPrint gives every inode one
  kernel-wide identity, so the caller owns both and the rule can never refuse.
  The same argument that made per-inode uid dead state in #983.
- **The target's own permission bits are irrelevant.**
- **`CallerPrivilege` gates the parent-write bit and nothing else.** Measured at
  uid 0 on both: Linux still answers EISDIR for a directory and Darwin still
  answers EPERM. The `unlink(2)` man page's "and the effective user ID of the
  process is not the super-user" is stale relative to modern XNU's `unlink1`,
  which refuses unconditionally.

## Decisions

### A. Where the inode-lifetime decision lives

`VirtualFileSystem.checkInvariants` reported `UnreachableFromRoot` as a defect,
and its own docstring said the rule would relax once the model grew open file
descriptions. It has them, and deletion is what makes an unreachable inode
legitimate — but the graph alone cannot decide, because the reason an orphan is
alive lives in `FileDescriptorRegistry`.

- **A1 (chosen) — the kernel composes two VFS primitives.**
  `VirtualFileSystem.unbind` removes one entry and `forget` removes one inode;
  `EmulatedKernel.forgetIfUnheld` calls `forget` when nothing names the inode and
  nothing holds it, at both `unlink` and `close`. `checkInvariants` gains
  `pinned : Set<InodeNumber>` and excuses exactly those. Lifetime policy stays
  out of the VFS, and the two facts meet in the one place that can see both.
- **A2 — the VFS owns an `Orphans` set.** `checkInvariants` stays
  one-argument, but an orphan nothing holds becomes undetectable, so this
  relocates the imprecision rather than removing it — and it puts a fact the VFS
  cannot maintain into the VFS.
- **A3 — store `Links : int` on the inode.** O(1) "last link", and `st_nlink`
  for free. But `FileStatus` has no `nlink` field, so nothing observes it; it is
  a denormalised field needing its own chokepoint check, and a wrong one is
  silently wrong rather than loudly wrong.

A3's only advantage is the cost of a scan a deliberately slow IL interpreter does
not care about, and it buys a field no guest can read.

`EmulatedKernelDefect.DanglingOpenInode` is the mirror-image rule, and the two
bracket the reaping: a `forget` that fires too late is caught by
`UnreachableFromRoot`, and one that fires too early by `DanglingOpenInode`.

### B. How the flavour divergence is spelled

- **B1 — a rules record and one shared `verdict`,** as `MkDirRules` does. It
  would need at least
  `{ TrailingSeparator; DirectoryErrno; RootNavigationErrno; TypeCheckPrecedesPermission : bool }`,
  and most of those inhabitants describe a kernel nobody ships.
- **B2 (chosen) — one `verdict` per flavour,** each a straight transcription of
  its own measured column, selected by `SimulatedUnixFlavour`. What diverges here
  is the *order* of the checks and the errno vocabulary rather than a constant
  they both consult, and a boolean that reorders control flow is exactly the
  illegal-state-representable shape this codebase avoids. The residual
  `UnlinkRules` record keeps the one genuine datum: which walk to take.
- **B3 — `SimulatedUnixPlatform.bindFaultOrder`'s shape:** compute the fault set,
  pick the first by a per-flavour order. That works for `bind` because both
  flavours agree on the faults and on the errno each carries. Here they agree on
  neither, so it would carry three parallel per-flavour tables and be two
  functions wearing a record costume.

### C. Slicing

`unlink` first, carrying the lifetime machinery; `rmdir` second, adding only
`UnixError.ENOTEMPTY`, its own verdict, and the pinned-orphan-directory question
(an orphaned directory whose parent is later reaped leaves a dangling `Parent`).
Stacked PRs get no CI here, so the second waits for this one to merge.

## Testing, and the destructive-host hazard

`TestVirtualFileSystemAgainstHost` compares the model against **real** `unlink`
calls on the machine running the tests. That is the one fixture here where a path
bug deletes the developer's files rather than failing a test.

The guarantee is structural rather than lexical: the comparison is on a
before/after **tree delta**, and one `unlink` removes at most one name, so a call
that succeeded while the row's own tree is unchanged must have removed something
outside it — which fails the row loudly. No `Path.GetFullPath` containment check
is involved, and deliberately: it does not traverse intermediate symlinks, so it
would call `lroot/..` contained while the kernel resolves it to the machine's
root.

The delta is also what makes the comparison able to see a model that destroys the
*wrong* object. `unlink` never does — Darwin answers EPERM for the directory a
followed link named — but `rmdir("ld/")` will, and the fixture is built for it.

Two further rules, both already in force elsewhere in that fixture:

- Every row is a **relative name of a corpus object**, and every corpus symlink
  target is relative, so no path can name anything outside the row's tree.
- Each row builds its **own** tree; `unlink` mutates.

### What the host oracle structurally cannot ask

`hostPath` re-roots every path under the row's temporary directory, so the host's
stand-in root is an ordinary directory with a real parent while the model's is a
genuine filesystem root. Deletion is the first operation whose answer depends on
the difference: Darwin answers EBUSY for the root reached by `.` and EPERM for
any other directory. Those rows are excluded and pinned in `TestUnlinkRules`
instead, on **both** flavours — the host oracle only ever falsifies its own
column. ``the excluded paths are exactly the ones that reach the model's root``
keeps the exclusion list honest as the corpus grows.

`mkDirProbePaths` already excludes `"/"` for the containment half of the same
reason, and the corpus comment refuses `"/.."` for the mirror image; this is the
third instance of one rule.

## Test tiers

| tier | what only it can see |
| --- | --- |
| `TestUnlinkRules` | both flavours at once, and the root-navigation arms the host oracle cannot reach |
| `TestVirtualFileSystem` | `unbind`/`bindingCount`/`forget`, and the pinned-inode excuse |
| `TestVirtualFileSystemAgainstHost` | that the model agrees with a real kernel, and destroys the same object |
| `sourcesPure/UnlinkSeeded.cs` | the facts both kernels share, against real .NET — including that a descriptor outlives the name |
| `sourcesImpure/UnlinkWiring{Linux,Darwin}Seeded.cs` | that the handler reads `SimulatedUnixPlatform.unlinkRules` and `Kernel.UserId` rather than hardcoding either |
| `sourcesImpure/UnlinkReapSeeded.cs` | the reaping rule, which no guest can read: asserted on the terminal state |
