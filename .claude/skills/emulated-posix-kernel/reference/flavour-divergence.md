# Measured Linux/Darwin divergence

## What these rows do and do not cover

Every row here is an answer a real kernel gave, not a rule derived from a header.
The envelope they were measured in:

| varied | held fixed |
| --- | --- |
| platform (macOS 26.6 / APFS, Linux 6.x arm64 via `container`) | one filesystem per platform |
| uid, where it changes the answer (root and 1000 on Linux, 501 on Darwin) | the caller's supplementary groups |
| the flag or argument under test | capabilities — every Linux row is `CAP_FSETID`-less and `CAP_NET_RAW`-less unless it says root |

**"Unanimous" means measured on both platforms and found equal**, not deduced.
A claim outside the envelope above — a second filesystem, a mount option, a
group the caller is not in, a capability — is **unmeasured**, and the honest
response to needing one is to measure it and add the row, not to extrapolate
from a neighbouring one.

## `open(O_CREAT)`

**Linux adds exactly two rules over the ordinary walk.**

1. A final component carrying a trailing separator is `EISDIR`, checked *before*
   `NAME_MAX`, before the lookup, and before any symlink traversal — so `cyc/` is
   `EISDIR` not `ELOOP`, and a 300-character name with a trailing slash is
   `EISDIR` not `ENAMETOOLONG`. It does not pre-empt earlier components
   (`nodir/new/` is `ENOENT`), and it fires on a separator arriving from a
   *spliced* symlink target.
2. Landing on an existing directory is `EISDIR`, but `O_EXCL`'s `EEXIST` beats it.

**Darwin's rule is "`O_CREAT` changes nothing about the walk"** — except that a
path consuming *no component at all* (`/`) is `EEXIST` even without `O_EXCL`.
That keys on `FinalNavigation.Root`, not on the root inode: `/.`, `/../`,
`/private/..` reach the same inode and open fine, as does `/System/Volumes/Data`.

| rule | Linux | Darwin |
| --- | --- | --- |
| the *requested mode* is masked with `ACCESSPERMS` | no, all twelve bits reach the file | **yes**, so no Darwin guest can create a setuid/setgid/sticky file |
| `umask(2)` stores | `mask & 0777` (`umask(0o4000)` reads back 0) | all twelve, but creation cannot see them |

Masking the umask to `0o777` is exact for both, for two different reasons.

That first row is about the *mask applied to the caller's mode*, and is not the
whole story of what mode a new file ends up with. Linux's VFS has a separate rule
— a file created in a setgid directory whose group the caller does not belong to
loses `S_ISGID` unless the caller holds `CAP_FSETID` — which is **not measured
here and not modelled**: PawPrint gives every inode `Kernel.GroupId`, so no guest
can currently be outside the group of the directory it creates in. That
approximation is what makes `CreatingOpenRules.createdPermissions` exact today;
it stops being exact the moment inodes can carry a group of their own.

**Unanimous:** a fresh inode bypasses the mode check — `open(free, O_CREAT|O_RDWR, 0)`
succeeds and stores mode 0, while re-opening that file `O_RDONLY` is `EACCES`. So
"create" is a distinct verdict, not a step before the existing-file permission
check. Binding a name needs write *and* search (`0o300`) on the parent.

No trailing-separator row creates anything on either kernel, so the
`TrailingSeparatorDemanded && FinalSymlinkFollowed` combination does not arise
for `open`; that debt belongs to `mkdir`/`rmdir`/`unlink`.

## Truncation

**Unanimous, `ftruncate(2)`:** a negative length is `EINVAL` and is checked
**before the descriptor**, so a bad fd with a negative length is `EINVAL`, not
`EBADF`. A read-only, directory, pipe (either end), socket or epoll/kqueue
descriptor is `EINVAL` — *not* `EBADF`, which is where it differs from `write(2)`,
and not `EISDIR`, which is path-based `truncate(2)`'s answer for a directory.
Standard streams answer `EINVAL` too, but that is a fact about what backs them
(both launchers hand the guest a pipe) rather than about the fd number: redirect
one to a writable regular file and `ftruncate` succeeds. Extension zero-fills.
The description's offset never moves, even when truncated below it.
`INT64_MAX` is `EFBIG`, but the real threshold is
per-filesystem (ext4 between 2^43 and 2^44, APFS between 2^46 and 2^62), so
PawPrint refuses loudly above `Array.MaxLength` rather than picking one.

**Unanimous, `O_TRUNC`:** it adds the write permission bit to whatever the access
mode demands and nothing else, so `open(f, O_RDONLY|O_TRUNC)` empties a writable
file while the same call on `0444` is `EACCES`. A directory is `EISDIR` at *any*
access mode. A refused open truncates nothing.

**A no-op truncation is still a truncation.** `ftruncate(fd, n)` on a file
already `n` bytes long moves mtime and ctime, and applies whatever set-ID rule the
platform has — so it strips on Linux and preserves on Darwin, exactly as a
size-changing truncation would. It does not become a no-op by changing no bytes.
This is the opposite of a zero-length write, which is not a write at all, and it
is the rule most likely to be got wrong by copying `writeFile`'s shape.

**The one divergence is the set-ID bits.**

| | Linux | Darwin |
| --- | --- | --- |
| `ftruncate` clears `S_ISUID` | always (`04644` → `0644`) | never |
| `ftruncate` clears `S_ISGID` | only alongside `S_IXGRP` (`02755` → `0755`, `02644` survives) | never |
| sticky | never moves | never moves |
| as root | everything preserved | everything preserved |

Isolated rather than inferred: in one process on one file, Darwin's `write` takes
`04755` to `0755` while `ftruncate` leaves it `04755`.

`PAL_O_APPEND` does not exist (`pal_io.h` stops at `NOFOLLOW`); CoreLib emulates
append in managed code, so probe rows for it are inert — a guest passing the
platform bit gets `EINVAL` from the unknown-flag check.

## `fstatfs` / `SystemNative_GetFileSystemType`

Returns **0 for every failure** rather than -1. Its only managed consumer is
`SafeFileHandle.CanLockTheFile`, which refuses a *shared* lock under write access
on nfs/smb/smb2/cifs — the combination `File.WriteAllBytes` asks for.
`File.Create` is `FileShare.None` → `LOCK_EX` → never reaches it.

| descriptor | Linux | Darwin |
| --- | --- | --- |
| regular file *and* directory | the mount's `f_type` | `f_fstypename`, mapped by name |
| pipe, either end | `0x50495045` pipefs | `EINVAL` → 0 |
| socket, INET or UNIX | `0x534F434B` sockfs | `EINVAL` → 0 |
| epoll port / kqueue | `0x09041934` anoninode | `EINVAL` → 0 |
| unknown fd | `EBADF` → 0 | `EBADF` → 0 |

One Linux process reported `0xEF53`, `0x01021994` and `0x9FA0` for three
directories, which is why the value cannot be a platform constant.

`Ext4` is deliberately not a case: CoreLib's `UnixFileSystemTypes` has no `ext4`
member (it is `ext2 = 0xEF53`, with `ext4` commented out as an alias), so the
managed layer cannot distinguish it. Darwin's name table *does* carry a `tmpfs`
row in `pal_io.c`, so "the table doesn't know it" is the wrong reason to exclude
`Tmpfs` there — the right one is that nothing on Darwin ever reaches that row.

## The environment is a list, not a map

A process's environment is a *list* of `name=value` strings; the name→value map
every .NET API presents is a **view**, split at each entry's first `=`. CoreCLR
makes the view total from both ends: `GetEnvironmentVariableA`
(`pal/src/misc/environ.cpp`) refuses a lookup name that is empty or contains `=`
*above* its matching loop, and `Environment.GetEnvironmentVariables` discards any
entry whose first `=` is not after the first character. `SetEnvironmentVariable`
validates; the getter only null-checks, so illegal names do reach the runtime.

Measured on real .NET via `execve` with a hand-built envp: `A=B=C` gives
`"A"` = `"B=C"` and `"A=B"` = null; `=C` is invisible to both APIs; `DUP=1` with
`DUP=2` gives `DUP` = `"1"` to both.

PawPrint stores the *map*, so the lossiness runs the other way: it could hold
names the view could never yield, which have no consistent behaviour to model.
`environmentEntryProblem` rejects them in `withEnvironment`, and that invariant is
what makes `GetEnvironmentVariableW`'s plain `Map.tryFind` faithful without the
PAL's guards — a refused name misses, reporting the same `ERROR_ENVVAR_NOT_FOUND`
the PAL's `value == nullptr` tail returns.
