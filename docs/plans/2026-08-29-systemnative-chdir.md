# `SystemNative_ChDir`: letting a guest change directory

Part of the emulated filesystem (issue #956). The slice after `rename`.

## What is blocked, measured

`Environment.CurrentDirectory`'s setter and `Directory.SetCurrentDirectory`
reach `SystemNative_ChDir` and nothing else: probed under PawPrint with a
seeded filesystem, the guest stops at

```
Unimplemented native method (PInvokeImpl libSystem.Native!SystemNative_ChDir):
  Sys::<ChDir>g____PInvoke|8_0(*(System.Byte)) -> System.Int32
```

`Environment.CurrentDirectory`'s *getter* already works, `SystemNative_GetCwd`
having landed earlier. So this is one native, and the guest-visible change is
that a relative path can be made to mean something different.

Three neighbours were probed at the same time and are **not** available as
alternatives, which is why this slice is the one:

| API | first blocker | why it is not this slice |
| --- | --- | --- |
| `File.SetLastWriteTimeUtc` | *not* a filesystem gap — `FileStatus.SetAccessOrWriteTimeCore` stops on "typed read of local memory … needs a byte-view byref shape" | an interpreter gap; `UTimensat` is never reached |
| `File.Copy` | `libc!clonefile` | a Darwin CoreLib P/Invokes it directly, so it needs a different primitive per flavour |
| `File.SetUnixFileMode` | `SystemNative_ChMod` | available, but the set-ID rules are where the flavours diverge; wants its own measurement pass |

## Measured, on both kernels

`docs/probes/chdir/` holds the probe and both columns. **They are identical on
every row.** `chdir` is the first filesystem syscall in this workstream with no
flavour divergence, and the rows cover the whole arm space — object type,
final-symlink following, trailing separator, which permission bit, name length,
navigation, and the current directory removed underneath the process — so that
is a measured absence rather than an unexamined one.

| row | both kernels |
| --- | --- |
| a directory, and `d/` | ok |
| a regular file, and `f/` | ENOTDIR |
| a symlink to a directory, and `ld/` | ok, and `getcwd` reports the **target's** path |
| a symlink to a file | ENOTDIR |
| a dangling symlink | ENOENT |
| absent | ENOENT |
| the empty path | ENOENT |
| a directory with the search bit only (0o100) | ok |
| a directory with the read bit only (0o400) | EACCES |
| a 300-byte component | ENAMETOOLONG |
| `.` and `..` | ok |
| `chdir(".")` with the current directory `rmdir`'d | **ok**, though `getcwd` there fails ENOENT |

Two of these decide the design:

* **`getcwd` reports the physical path.** So the cached
  `UnixProcessState.CurrentDirectory` is `VirtualFileSystem.pathOfDirectory` of
  the inode landed on, never the path the guest passed.
* **`chdir` wants the search bit, not the read bit** — the opposite of
  `opendir`. The walk checks search on the directories it *traverses*; the
  target's own bit is a separate check this syscall makes.

## Design

No `ChDirRules`, and no entry in `SimulatedUnixPlatform`. Those types exist to
hold divergence, and there is none: an empty record would be a claim that
something varies. The measurement is cited at the entry point instead.

```fsharp
/// `chdir(2)`: make `path` the directory relative paths resolve from.
///
/// Never refused: every outcome is a success or an errno.
val chdir : UnixPath -> UnixSystem<'Task, 'Handler> -> SyscallAnswer * UnixSystem<'Task, 'Handler>
```

The body is four steps, each pinned by a row above:

1. `resolvePath SymlinkPolicy.Follow`, which is `TrailingSeparatorPolicy.Demand`
   underneath. That alone produces ENOENT for an absent name and for a dangling
   link, ENOTDIR for a file and for `f/`, ENAMETOOLONG for an over-long
   component, and follows `ld` to what it names.
2. The target must be a directory: ENOTDIR. Reached by `lf`, which resolves to
   a regular file.
3. The target's own search bit: EACCES. This is the check the walk does not
   make, and 0o400-vs-0o100 is the pair that shows it.
4. Set `CurrentDirectoryInode`, and set `CurrentDirectory` to
   `pathOfDirectory` of it — falling back to the previous value when that is
   `None`, which is what `rename` already does for the same reason: a real
   `getcwd` fails rather than answering there, so the stale path is no worse
   than what the process already had.

### The old current directory must be reaped

`UnixProcessState.heldInodes` includes `CurrentDirectoryInode`, so the current
directory is pinned. `chdir` is therefore a reference-dropping operation, and
`forgetIfUnheld` on the *old* inode belongs after the move — otherwise a guest
that `rmdir`s its own current directory and then leaves it strands the inode
for the run. This is the same integration `unlink`, `rmdir` and `rename`
already do, and `forgetIfUnheld`'s own docstring asks for it.

### The native handler

`pathSyscall` already covers the shape: one NUL-terminated path pointer, zero
or -1 with errno. `SystemNative_ChDir` is a one-line dispatch arm beside
`SystemNative_MkDir`, `SystemNative_Unlink` and `SystemNative_RmDir`.

## Correctness oracle

* `TestUnixSystemStep`: a row per line of the measured table, run under **both**
  flavours from one list — the table is unanimous, so a row that answered
  differently on one of them would be a regression in exactly the fact the
  probe establishes.
* The physical-path row: `chdir("ld")` leaves `CurrentDirectory` at the
  target's path, not the link's.
* The reaping row: `mkdir d; chdir d; rmdir d; chdir /` leaves nothing behind,
  checked with `UnixSystem.checkInvariants` rather than by looking for the
  inode.
* The orphan row: `chdir(".")` in an `rmdir`'d current directory succeeds and
  changes no cached path.
* `TestVirtualFileSystemAgainstHost`: `chdir` over the fixture's existing
  `probePaths`, comparing errno-or-success against the real kernel, **in a
  child process**.

  The host side must not be run in the test host. A working directory is
  process-global, and forty-one fixtures in this assembly are
  `[<Parallelizable(ParallelScope.All)>]`, so any of them could observe the
  moved directory while a row is in flight. Doing it and restoring afterwards
  is not a fix — it narrows the window rather than closing it, and the failure
  it leaves would be a rare, load-dependent break in an unrelated fixture,
  which is the worst shape of test flake to own.

  The child is `python3`, given the tree root and the whole path list, printing
  one line per path: the physical `getcwd` on success, or `OSError.errno` on
  failure. `os.chdir` is `chdir(2)`, and Python is the only thing to hand that
  reports errno *numerically* — a shell's `cd` reports a message. One
  invocation for the whole corpus rather than one per path, so this costs a
  single process start.

  **Containment is a checked invariant, not a sandbox.** `chroot(2)` wants
  root and these tests run unprivileged, so it is not available; the Linux-only
  substitutes (`unshare -r` with `pivot_root`, or `bwrap`) would protect CI and
  do nothing on macOS, which is where the mistakes get made. So instead the
  operands are joined with the fixture's own `hostPath`, which makes an escape
  impossible to express, and the child refuses in both directions anyway — an
  operand whose `realpath` is not under the tree, and a directory it somehow
  lands in that is not either — exiting rather than reporting a measurement.
  That is not hypothetical: the first version of this test passed the bare
  operand, so `"/"` named the *real* filesystem root, and the guard is what
  turns that from a silent misreading into a dead child. `chdir` mutates
  nothing, so the exposure was a wrong answer rather than damage — but the
  destructive comparisons in this fixture share the shape and are safe only
  because they go through the same helper.

  **`python3` must be added to the flake devshell's `packages`.** It is used
  today only inside flake checks, so it is on `PATH` in a developer's shell by
  accident of the system and not at all by declaration. CI runs
  `nix develop --command dotnet test`, so the addition is what makes this test
  run there rather than silently `Assert.Ignore`. The `Assert.Ignore` guard
  stays, as the repo's other environment-dependent fixtures have it, for a
  non-Nix checkout — but it must not be the CI path.
* `sourcesPure/ChDirSeeded.cs`: `Environment.CurrentDirectory` set and read
  back, and a relative open afterwards resolving somewhere different. Through
  the differential oracle, so the real runtime checks the same claims.
* Mutation: dropping the search-bit check must fail the 0o400 row; using the
  passed path rather than the physical one must fail the `ld` row; dropping the
  reap must fail the invariant check.

## Deliberately out of scope

* **`UnixProcessState.CurrentDirectory` on an orphaned current directory.**
  *(Confirmed as a separate slice; see
  docs/plans/2026-08-30-detached-current-directory.md, which carries it out.)*
  The field keeps a path that no longer reaches `CurrentDirectoryInode`. Giving
  it a representation for "no path" changes a record every syscall touches, so
  it is its own change, and this plan's tests pin the current behaviour so that
  change has something to break.

  This bullet originally claimed that PawPrint therefore "answers the stale
  cached path where a real `getcwd` fails ENOENT". That was **wrong**:
  `UnixPathResolution.getcwd` has guarded the orphan since #1196, and
  `sourcesImpure/RmDirOrphanLinuxSeeded.cs` asserts the ENOENT end-to-end. No
  guest can observe the stale field; what is wrong with it is that its stated
  contract is false, and that `checkInvariants` is silent for exactly as long as
  it is false.
* `fchdir(2)`: no CoreLib caller reaches it.
* `chroot(2)`: PawPrint models one filesystem with one root.
