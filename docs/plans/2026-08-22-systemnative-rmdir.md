# `SystemNative_RmDir`

Issue #956's next slice, and the one that makes an *orphaned directory* a state
this model can reach.

Everything under "Measured" was probed on macOS 26.6/APFS at uid 501, and Linux
6.x arm64/overlayfs (via `container`) at uid 1000 and uid 0, one fresh tree per
row, on 2026-08-22, and re-measured against this implementation before the
verdicts were written.

## Measured

`SystemNative_RmDir` (pal_io.c) is an EINTR-retrying `rmdir(2)` and nothing
else, taking a UTF-8 `const char*` — the same guest-side stub shape as
`SystemNative_Unlink`.

### The walk

`SymlinkPolicy.NoFollowFinal` on both, and the trailing separator splits by
flavour exactly as `unlink`'s does:

| | Linux | Darwin |
| --- | --- | --- |
| `TrailingSeparatorPolicy` | `Ignore` | `Demand` |

Under `Ignore`, `rmdir` never has to *enforce* the demand the way `unlink` does:
the demand is "the final component must be a directory", which `rmdir` owes
anyway. Measured, every Linux `X/` row equals its `X` row, so the Linux verdict
never reads `Resolution.TrailingSeparatorDemanded` at all.

### The destructive divergence

This is the operation `Resolution.FinalSymlinkFollowed` has been warning about
since the walk landed. With `ld -> d` and `d` an empty directory:

| | Linux | Darwin |
| --- | --- | --- |
| `rmdir("ld/")` | ENOTDIR | **succeeds, and removes `d`** |

Darwin's `Demand` walk traverses the final symlink; Linux's `Ignore` walk cannot.
So the two flavours destroy different objects, and a handler that picked one
unconditionally would delete a guest's data on the other. Both columns are now
measured, so the handler dispatches, and `FinalSymlinkFollowed`'s docstring loses
its last "not measured yet".

### The orderings

**Linux** — `Root` → EBUSY; `Current` → EINVAL; `Parent` → ENOTEMPTY; free name →
ENOENT; parent not writable → EACCES; target not a directory → ENOTDIR; target
not empty → ENOTEMPTY; else remove.

**Darwin** — `Root` → EISDIR; the root reached by `.` → EBUSY; the root reached
by `..` → EBUSY; any other `Current` → EINVAL; any other `Parent` → ENOTEMPTY;
free name → ENOENT; target not a directory → ENOTDIR; parent not writable →
EACCES; target not empty → ENOTEMPTY; else remove.

The rows that pin them, each differing from the naive guess:

| path (`nowrite` is 0o555, `nosearch` is 0o666) | Linux | Darwin |
| --- | --- | --- |
| `/` | EBUSY | **EISDIR** |
| `.`, `./`, `d/.`, `nest/inner/.` | EINVAL | EINVAL |
| `..`, `d/..`, `nest/inner/..` | ENOTEMPTY | ENOTEMPTY |
| `/.`, `/..` | EINVAL, ENOTEMPTY | **EBUSY** |
| `lroot/.`, `lroot/..` | EINVAL, ENOTEMPTY | **EBUSY** |
| `lroot/` | ENOTDIR | EISDIR |
| `ld/` | ENOTDIR | **OK, removes `d`** |
| `lfull/` | ENOTDIR | ENOTEMPTY |
| `lcur/` | ENOTDIR | EINVAL |
| `lpar/` (`lpar -> "d/.."`) | ENOTDIR | ENOTEMPTY |
| `dang/` | ENOTDIR | ENOENT |
| `cyc/` | ENOTDIR | ELOOP |
| `nowrite/kdir`, `nowrite/kdir/` | EACCES | EACCES |
| `nowrite/kdir/.` | EINVAL | EINVAL |
| `nowrite/kdir/..` | ENOTEMPTY | ENOTEMPTY |
| `nowrite/kfull` | EACCES | EACCES |
| `nowrite/kid`, `nowrite/klink` | **EACCES** | **ENOTDIR** |
| `nowrite/nx`, `nowrite/nx/` | ENOENT | ENOENT |
| `nosearch/kdir` | EACCES | EACCES |
| `d`, `d/`, `nest/inner` | OK | OK |
| `dfull`, `dfull/` | ENOTEMPTY | ENOTEMPTY |
| `f`, `f/`, `lf`, `lf/`, `ld`, `lfull`, `dang`, `cyc`, `lroot`, `lcur`, `lpar` | ENOTDIR | ENOTDIR |
| 300-byte name, with or without a separator | ENAMETOOLONG | ENAMETOOLONG |

Three of those deserve calling out:

- **`nowrite/kdir/.` is EINVAL, not EACCES**, on both. The navigation arms beat
  the parent-write check, which is why they sit at the top of both orderings.
- **`nowrite/kid` splits.** Linux checks the parent's write bit before the
  target's type; Darwin checks the type first. Same split as `unlink`, opposite
  errnos.
- **Linux gives `/` EBUSY where Darwin gives EISDIR**, and Darwin gives `/.`
  EBUSY where Linux gives EINVAL. Neither flavour is "the root is special"
  uniformly: Linux specialises the *no-component* path and Darwin specialises
  the *root inode*.

### The privileged column

Measured at uid 0 on Linux, every row: the EACCES rows become their next check
(`nowrite/kdir` → OK, `nowrite/kfull` → ENOTEMPTY, `nowrite/kid` → ENOTDIR,
`nosearch/kdir` → OK) and **nothing else changes**. So `CallerPrivilege` gates
the parent-write bit and nothing else, exactly as for `unlink` and `mkdir`.

Darwin at uid 0 was measured on 2026-08-21 for the two decisive rows
(`rmdir("d")` and `rmdir("nowrite/kdir")`, both OK where the latter was EACCES
at uid 501); this box has no passwordless `sudo`, so the rest of that column is
argued from the Linux column and from the code shape rather than measured. The
implementation makes that argument checkable: `CallerPrivilege` reaches exactly
one function, `RemovalChecks.lacksWrite`.

### Timestamps, and where the flavours split again

The directory losing an entry gets `mtime` **and** `ctime`, never `atime`, on
both — same as `unlink`. The *removed directory* does not agree:

| | Linux | Darwin |
| --- | --- | --- |
| removed directory's `ctime` | moves | **does not move** |
| removed directory's `st_nlink` | 2 → 0 | 2 → 2 |

Reproduced 3/3 on each, watched through a descriptor held open across the
`rmdir`. The two columns are one fact: Darwin does not decrement the removed
directory's link count, so nothing about that inode changed and its `ctime` has
no reason to move. `unlink` of a *file* moves `ctime` on both, so this is
specific to `rmdir`.

It is guest-observable: `SystemNative_FStat` on a directory descriptor writes
`times.StatusChange` into `FileStatus`. (`st_nlink` is not in `FileStatus`, so
the link count itself is unobservable — only its shadow on `ctime` is.)

### What a real kernel does with an orphaned directory

`rmdir` can remove a directory that something still holds — a descriptor, or the
process's own current directory. Probed on both, unanimously:

- The descriptor keeps working: `fstat` reports the inode, `readdir` reports an
  empty directory.
- **`..` still resolves, to the parent's inode — even after that parent is itself
  removed.** With `a/b`, cwd `b`: `rmdir(b)` then `rmdir(a)` both succeed, and
  from the orphaned cwd `stat("..")` is still `a` and `stat("../..")` is still
  the live grandparent. So the parent chain of a held orphan stays alive all the
  way to the root.
- `getcwd()` from an orphan is ENOENT — with one flavour split, measured by
  sweeping the buffer size from 1 past the length of the path that used to be
  there. A zero-length buffer is EINVAL on both (the shim's own guard, before
  `getcwd` is called at all). Linux is ENOENT at *every* other size. Darwin is
  ERANGE at size 1 and ENOENT at every size from 2 up — including sizes far
  below the old path's length, so it is a minimum ("room for `/` and a
  terminator", which is what it writes before it starts climbing) rather than a
  comparison against a path that no longer exists. The first model written here
  compared against the stale path and was wrong for every size in between; the
  sweep is what caught it.
- **Creating anything inside an orphan is ENOENT** — `mkdir`, `open(O_CREAT)`,
  `symlink`, all of them, on both flavours.
- Every *other* name operation inside an orphan is ENOENT too, but for the
  ordinary reason: an orphaned directory is necessarily empty (a non-empty one
  is ENOTEMPTY), and it can never gain an entry, so it stays empty forever.

The middle two facts are what this slice has to build; the last is why it needs
to build so little.

## Decisions

### A. Keeping a held orphan's ancestors alive

`EmulatedKernel.pinnedInodes` names the inodes an open description or the current
directory holds. `unlink` needed no more than that, because a file inode records
no parent. A directory does: `DirectoryContent.Parent` is what `..` resolves to,
and a held orphan whose parent is later reaped would leave that field naming an
inode the graph no longer contains — `VirtualFileSystemDefect.DanglingParent`.

- **A1 (chosen) — pin transitively, and reap transitively.** `heldInodes` is the
  direct set; `pinnedInodes` is its closure under `Parent`. `forgetIfUnheld`
  consults the closure, so a held orphan's ancestors survive; and after it frees
  a directory it recurses onto that directory's former parent, so the chain is
  collected as soon as its last holder goes. This is not an approximation of the
  kernel — it is what both kernels were measured doing, including the "`../..`
  still resolves after the grandparent is gone" row.
- **A2 — clear the orphan's `Parent`,** making the field an option, so an orphan
  has no `..` to dangle. Measured wrong: `stat("..")` from an orphan answers the
  old parent on both flavours. It would also make `Parent` optional at every
  directory site to model a state that never arises.
- **A3 — refuse to `rmdir` anything held.** Measured wrong (it succeeds), and it
  would make the answer depend on descriptor state no real `rmdir` consults.

A1 costs a graph walk up the parent chain per reap, in an interpreter that is
deliberately slow. Termination is by construction: each recursive step has
removed one inode, and the root is refused.

### B. Creating a name inside an orphan

A1 makes an orphaned current directory survivable, which makes `mkdir("x")` from
inside one reachable for the first time. Left alone, PawPrint would create the
entry — and the new inode would be unreachable from the root and unpinned, i.e.
`VirtualFileSystemDefect.UnreachableFromRoot`. A legal guest program would make
the graph unsound.

- **B1 (chosen) — the creating verdicts answer ENOENT.**
  `VirtualFileSystem.isOrphanedDirectory` is one predicate, consulted by
  `MkDirRules.verdict` and `CreatingOpenRules.verdict` — the only two
  guest-reachable places that add an entry. Measured, unanimous across flavours,
  and it is the rule that keeps "an orphan is always empty" true.
- **B2 — `failwith` at those two sites.** The same blast radius as answering
  correctly, for a worse answer, when the measurement is unambiguous.
- **B3 — leave it, and let `checkInvariants` report it.** Only tests run that
  check, so in a real run the divergence is silent — and it is a divergence that
  *destroys the model's soundness* rather than merely answering one call wrongly.

This is why an `rmdir` change touches `mkdir` and `open`: it is not scope creep
but the other half of what orphans cost. Nothing else in the model creates a
name — `FileSystemSeed` builds downward from the root, before any guest exists.

### C. `getcwd` from an orphan

The same reachability argument as B, with a weaker consequence: nothing becomes
unsound, PawPrint just answers a path nothing reaches.

- **C1 (chosen) — answer it, per flavour.** `SimulatedUnixPlatform.getCwdOrphanAnswer`
  is one two-case DU saying what a removed current directory answers, and the
  handler gains one arm. Both columns are measured, so there is no open question to
  defer; the only cost of doing it now is the DU itself.
- **C2 — `failwith`.** The codebase's rule is "refuse rather than invent", but
  that is for facts it has not measured. These are measured, and a guest that
  reads `Environment.CurrentDirectory` after deleting its own working directory
  should get an error rather than take the interpreter down.
- **C3 — leave the stale path.** Definitely wrong on both flavours, where a
  measured answer costs a dozen lines.

### D. How the flavour divergence is spelled

`unlink` settled this: one `verdict` per flavour, each a transcription of its own
measured column, plus a rules record for the data both consult. `rmdir` follows
it, and its record has two fields rather than one, because the removed
directory's `ctime` is a genuine per-flavour datum rather than an ordering.

`RemovalChecks` hoists the two predicates `unlink` and `rmdir` share —
"does the caller lack write on the holding directory" and "is this inode a
directory" — out of `UnlinkRules`, where they were private. Both are partial in
the same way, and the partiality is the walk's guarantee rather than either
syscall's.

### E. Telling `unbind` what happened to the target

`VirtualFileSystem.unbind` stamps the target with `statusChangedAt`, which is
right for `unlink` and for Linux's `rmdir` and wrong for Darwin's.

- **E1 (chosen) — `unbind` takes an `UnbindTargetEffect`.** Two cases,
  `LostALink` and `Untouched`, naming the *mechanism* the measurement exposed
  rather than the stamp it produces: Darwin leaves `st_nlink` at 2, so the inode
  genuinely did not change. `RmDirRules` carries which one, so the handler never
  chooses.
- **E2 — a `bool` on `unbind`.** Same information, no name for it at the call
  site, and it reads as "should I stamp" rather than "what happened".
- **E3 — stamp always, and un-stamp in the Darwin `rmdir` handler.** Requires the
  handler to have saved the old value, and makes the primitive's contract untrue.

## Testing

The host oracle (`TestVirtualFileSystemAgainstHost`) already runs real deletions
against a per-row tree, and already compares a before/after **tree delta** rather
than a lexical containment check. `rmdir` is what that delta was built for: it is
the first operation where the model can destroy the *wrong* object, and
`rmdir("ld/")` is the row that would do it.

The exclusions the `unlink` slice established carry over unchanged: rows that
reach the model's root are excluded (the host's stand-in root is an ordinary
directory with a real parent) and pinned in the unit tier on both flavours
instead, with ``the excluded paths are exactly the ones that reach the model's
root`` keeping the list honest.

| tier | what only it can see |
| --- | --- |
| `TestRmDirRules` | both flavours at once, and the root-navigation arms the host oracle cannot reach |
| `TestVirtualFileSystem` | `unbind`'s two target effects, and `isOrphanedDirectory` |
| `TestVirtualFileSystemAgainstHost` | that the model agrees with a real kernel, and destroys the same object |
| `sourcesPure/RmDirSeeded.cs` | the facts both kernels share, against real .NET |
| `sourcesImpure/RmDirWiring{Linux,Darwin}Seeded.cs` | that the handler reads `SimulatedUnixPlatform.rmDirRules` and `Kernel.UserId` rather than hardcoding either — including the row where Darwin removes `d` through `ld/` and Linux removes nothing |
| `sourcesImpure/RmDirOrphan{Linux,Darwin}Seeded.cs` | standing in a removed directory: what still works, what is ENOENT, and — on the terminal state — that its ancestors survive and are collected together |
