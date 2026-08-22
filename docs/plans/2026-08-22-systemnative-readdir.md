# Directory enumeration: `SystemNative_OpenDir` / `ReadDir` / `CloseDir`

The slice after `rmdir` (#1134). It is the read side's last structural piece,
and it is what makes `Directory.GetFiles`, `Directory.EnumerateFileSystemEntries`,
`DirectoryInfo` traversal and `Directory.Delete(recursive: true)` work — the
last of those being the payoff for `unlink` (#1129) and `rmdir` (#1134), which
until now can only delete things a guest already knows the names of.

## The blocker set is exactly three natives, measured

A `sourcesPure` guest calling `Directory.GetFileSystemEntries("d")` aborts at
`SystemNative_OpenDir`. With `OpenDir`, `ReadDir` and `CloseDir` stubbed to
"fake handle, immediate end-of-stream", the same guest runs to completion and
returns normally: nothing else in the managed enumeration path is missing.
`Path.GetFullPath`, `EnumerationOptions`, `ArrayPool<char>`,
`FileSystemEnumerableFactory` and `FileSystemEntry.Initialize` all already run.

To reproduce: branch off `main`, add a `sourcesPure` guest calling the BCL
method, register a seed for it in `TestPureCases.seededCases`, and run
`dotnet test --filter "Name~<guest>"`. To size what lies *behind* a native,
add a handler arm that answers it plausibly and re-run. The spike branch this
was measured on has been deleted rather than left lying about: a branch whose
interpreter answers natives wrongly is the hazard `mutation-testing` warns of.

## The managed contract

`Interop.ReadDir.cs`:

```
[LibraryImport(..., "SystemNative_OpenDir",  StringMarshalling = Utf8, SetLastError = true)]
                                                    IntPtr OpenDir(string path);
[LibraryImport(..., "SystemNative_ReadDir")] int ReadDir(IntPtr dir, DirectoryEntry* outputEntry);
[LibraryImport(..., "SystemNative_CloseDir", SetLastError = true)] int CloseDir(IntPtr dir);

struct DirectoryEntry { byte* Name; int NameLength; NodeType InodeType; }
```

Three things about it that are easy to get wrong:

- **`ReadDir` does not use `SetLastError`.** It returns `0` for an entry, `-1`
  for end-of-stream, and **the raw platform errno** for a failure, which
  `FileSystemEnumerator.FindNextEntry` feeds to `new Interop.ErrorInfo(result)`
  — a constructor that converts through `ConvertErrorPlatformToPal`. So the
  flavour's raw numbering is guest-observable on this path even though PawPrint
  never touches `LastSystemError` for it.
- **`Name` points into the stream's own buffer**, valid only until the next
  `readdir` on that stream (`ConvertDirent` sets it to `entry->d_name`). So
  PawPrint must hand back guest-readable memory with that lifetime, not a fresh
  allocation per call that nothing frees.
- **The buffer must be NUL-terminated.** `MoveNext` reads `_entry.Name[1]` and
  `_entry.Name[2]` whenever `Name[0]` is `'.'`, to spot `.` and `..`. Both are
  in bounds for a real `d_name` because it is a fixed array; under PawPrint they
  are in bounds only if the terminator is written.

## Measured, on both kernels

macOS 26.6 / APFS locally, Linux 6.x / aarch64 through
`container run --rm -v … python:3-slim`, unprivileged on both (the Linux
container is root by default and every permission row is vacuous there — the
probes take a `drop` argument that `setuid`s to 1000).

Probes: `scratchpad/readdir/probe.py`, `probe2.py`, `probe3.py`.

| fact | Linux | Darwin |
| --- | --- | --- |
| `opendir` consumes a file descriptor | yes (fd 4, between an `open` at 3 and one at 5) | same |
| `opendir("d")`, `opendir("d/")` | ok | ok |
| `opendir` on a regular file, with or without trailing `/` | ENOTDIR | ENOTDIR |
| `opendir` on a nonexistent name, or a dangling symlink | ENOENT | ENOENT |
| `opendir` through a symlink to a directory, with or without trailing `/` | ok (it follows) | ok |
| `opendir("d/.")`, `opendir("d/..")`, `opendir("/")` | ok | ok |
| directory mode `0o111` (search, no read) | **EACCES** | **EACCES** |
| directory mode `0o444` (read, no search) | ok, lists every name | ok |
| `0o000` | EACCES | EACCES |
| `.` and `..` present, and first, in that order | yes | yes |
| `d_type` for regular / directory / symlink / fifo / socket | `DT_REG` / `DT_DIR` / `DT_LNK` / `DT_FIFO` / `DT_SOCK` | same |
| order of the remaining names | arbitrary (`m a z` for that seed) | arbitrary (`z a m` for the same seed) |
| `DirectoryEntry.NameLength` | **-1** | **the byte length** |

Two things the probes ruled *out* as rules:

- **Whether a stream sees a mutation made after `opendir` is a buffering
  artifact, not a contract.** Removing the directory before the first `readdir`
  gives immediate end-of-stream on both; reading one entry first and *then*
  removing it yields the whole listing on both. POSIX leaves it unspecified and
  the measurement only describes when `getdents` happened to run. PawPrint must
  therefore choose a model and say so, rather than claim to match.
- `opendir` needs **read** on the target directory and search only on its
  ancestors. This is the first place in this codebase where the read bit and the
  search bit come apart; the walk already checks search, and nothing yet checks
  read.

### The one divergence

`NameLength` is `-1` on Linux and the real byte length on Darwin. The PAL sets
it from `d_namlen` where the libc has that member and to `-1` otherwise
(`pal_io.c:497`, guarded by `HAVE_DIRENT_NAME_LEN`). Confirmed by compiling
rather than by reading: `struct dirent` in glibc has no `d_namlen`
(`error: 'struct dirent' has no member named 'd_namlen'` under `gcc:14`), while
macOS's `sys/dirent.h` declares one.

It is invisible to managed code — `DirectoryEntry.GetName` handles both, taking
`CreateReadOnlySpanFromNullTerminated` for `-1` — so it belongs to a
`sourcesImpure` guest with its own `[DllImport]` declaration of the struct. It
is a fact about the PAL as compiled for a target, so it is
`SimulatedUnixPlatform`, not `KernelConfig`.

## Design decisions

### A. What a `DIR*` is

The measured fd consumption settles the substance: a stream holds a descriptor.
The pleasant surprise is that it needs **no new kernel state at all**.
`OpenFileTarget.File` already says "a regular file **or directory**, and where
in it this description is positioned", `open(dir, O_RDONLY)` already succeeds,
and `EmulatedKernel.heldInodes` already pins a `File` inode — which is exactly
the pinning an open enumerator over a directory that `rmdir` then removes
requires, and it came for free with #1134.

- **Chosen: `opendir` performs the `open`,** yielding an ordinary descriptor
  whose target is `OpenFileTarget.File (dir, cursor)`. Zero new DU cases, zero
  blast radius across the 51 `OpenFileTarget` sites, and the orphan case is
  already handled.
- Rejected: a separate `DirectoryStreams` map in the kernel. Simpler in
  isolation, but it consumes no fd (observably wrong: a guest can read fd
  numbers back through `SafeFileHandle.DangerousGetHandle`) and would need its
  own pinning rule duplicating `heldInodes`.

Sub-decision, the pointer the guest actually holds. `CloseDir` must map it back
to the fd, and nothing in CoreLib or the PAL calls `dirfd`, so the mapping is
private to PawPrint:

- (α) a native heap allocation standing in for libc's `DIR` struct, which also
  gives the per-stream name buffer a home and a lifetime; the kernel keys the
  stream on its address.
- (β) a synthetic pointer value the kernel maps to the fd, with the name buffer
  allocated separately.

(α) is closer to the real thing and puts the name buffer where its lifetime
already is. Either way the guest cannot legally dereference a `DIR*`.

### B. What a mutation mid-stream does, and where the cursor lives

Measured at 5000 entries — well past glibc's 32 KB `readdir` buffer — deleting
each entry as it is returned skips **nothing** on either kernel: all 5000 come
back and the directory is left empty. So a real filesystem hands out a **stable
per-entry cookie**, not a position, and removing an already-returned entry does
not shift the ones after it.

That is not academic. CoreLib's `FileSystem.RemoveDirectoryRecursive` deletes
each child *inside* the `foreach` over the live enumerator and then `rmdir`s the
parent, so a model that skips entries makes `Directory.Delete(recursive: true)`
throw ENOTEMPTY — on BCL code, not on a guest's bug.

- **Chosen: a name-keyed cursor.** The stream remembers the last name it
  returned; each `readdir` yields the least name strictly greater than it in the
  directory's *current* map. This is exactly what a stable cookie does:
  deleting an already-returned name is invisible, deleting a not-yet-reached one
  removes it from the listing, a name inserted after the cursor appears and one
  inserted before it does not.
- Rejected: **an index into the live listing**, reusing the `int64` offset on
  `OpenFileTarget.File`. It needs no new state and is the least forgiving of the
  three — but it is less forgiving than either real kernel rather than merely
  less forgiving than a snapshot, and it breaks the BCL as above.
- Rejected: **a snapshot at `opendir`**. Legal, deterministic and simplest, but
  it hides every mutation, which is the most forgiving of the three: a guest
  relying on a consistent point-in-time listing would never be caught here and
  would break on a real kernel with a large directory.

Between the two legal models the choice follows the standing preference for the
least convenient behaviour a guest could lawfully meet, since relying on
unspecified behaviour is almost always a bug in the workload.

The cursor is a `FileName option` held with the stream and dropped at
`CloseDir`, which is where libc keeps its `DIR` buffer. That makes it
per-`opendir` rather than per-open-file-description, so two `opendir`s of one
directory advance independently and a `dup` of the descriptor would *not* share
the cursor. Unobservable: `dirfd` appears nowhere in CoreLib or the PAL, so no
managed caller can reach the descriptor to `dup` it.

### C. Enumeration order

`DirectoryContent.Entries` is a `Map<FileName, InodeNumber>`, so an order is
already available for free — F# ordinal comparison of the *UTF-16* strings.

- **Chosen: that order**, with `.` and `..` ahead of it. No real kernel's order
  can be matched (arbitrary on both, and different), so the only criteria are
  determinism and cost, and this is free.
- Rejected: sorting by UTF-8 bytes. More principled — a name *is* bytes, and it
  is how a kernel would compare — but it differs from the Map order only above
  the BMP, and it costs a re-sort on every `readdir`.

The consequence for tests: **no differential test may compare order**, and
every guest must sort.

### D. `.` and `..` are synthesised

`FileName` rejects both by construction (`FileNameError.Reserved`) and
`DirectoryContent` derives the parent from a field, so they are not in the map
and the stream must produce them. `EnumerationOptions.ReturnSpecialDirectories`
is what makes them guest-visible through the BCL.

### E. The read bit

`opendir` checks read on the target, which nothing in the resolver does today
(the walk checks search). Add it in the handler's verdict, in the `RmDirRules` /
`UnlinkRules` shape, with `RemovalChecks`-style shared predicates.

## Test plan

| tier | what it carries |
| --- | --- |
| `TestVirtualFileSystemAgainstHost` | `opendir`'s verdict against the real `opendir(3)` over the existing probe corpus, and the *set* of names returned (sorted) against the host's — never the order |
| `TestOpenDirRules` | both flavours' verdicts as literal rows, including the ones the host oracle cannot reach, and the `d_type` mapping per `InodeContent` case |
| `sourcesPure/EnumerateSeeded.cs` | what the flavours agree on: the name set, `.`/`..` under `ReturnSpecialDirectories`, ENOTDIR/ENOENT/EACCES, enumeration through a symlink, and `Directory.Delete(recursive: true)` |
| `sourcesImpure/EnumerateWiring{Linux,Darwin}Seeded.cs` | `NameLength`'s two answers through a raw `[DllImport]`, and the raw errno `ReadDir` returns |
| `TestEmulatedKernelInodeLifetime` | a stream over a directory removed by `rmdir`: the inode stays pinned, and is reaped at `CloseDir` |
| terminal-state assertions | that `CloseDir` frees the descriptor *and* the name buffer — no guest can see either |

## Out of scope

`Rename`, `ChDir`, `SymLink`, `ChMod`. Also `File.SetLastWriteTime`, which is
not a filesystem gap at all: it stops in `FileStatus.SetAccessOrWriteTimeCore`
on a byte-view byref of a stack `TimeSpec[2]` that the interpreter refuses. And
`File.Copy`, which on a Darwin CoreLib P/Invokes `libc!clonefile` directly
rather than `SystemNative_CopyFile`, so it needs a different primitive per
flavour.
