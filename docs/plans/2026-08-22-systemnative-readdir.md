# Directory enumeration: `SystemNative_OpenDir` / `ReadDir` / `CloseDir`

The slice after `rmdir` (#1134). It is the read side's last structural piece,
and it is what makes `Directory.GetFiles`, `Directory.EnumerateFileSystemEntries`,
`DirectoryInfo` traversal and `Directory.Delete(recursive: true)` work — the
last of those being the payoff for `unlink` (#1129) and `rmdir` (#1134), which
until now can only delete things a guest already knows the names of.

## How much of the blocker set is actually established

A `sourcesPure` guest calling `Directory.GetFileSystemEntries("d")` aborts at
`SystemNative_OpenDir`. With `OpenDir`, `ReadDir` and `CloseDir` stubbed to
"fake handle, immediate end-of-stream", the same guest runs to completion and
returns normally.

**That measurement proves the empty-directory path and nothing more.** Nothing
downstream of an actual entry ever executed: not `FileSystemEntry.Initialize`
with a real name, not the `DT_LNK` path, not `ToFullPath`, not the recursive
descent's second `OpenDir`. What bounds the rest is a *static* reachability
check, which is weaker and is labelled as such here: `FileSystemEnumerator.Unix.cs`
and `FileSystemEntry.Unix.cs` between them name only `OpenDir`, `ReadDir`,
`CloseDir` and `GetLastErrorInfo`; every other kernel touch goes through
`FileStatus`'s *readers*, which reach `Stat`, `LStat` and `GetEUid` — all
implemented — and `IsMemberOfGroup`, which is unreachable because its sole
caller sits behind `_fileCache.Uid == GetEUid()` and PawPrint has one
kernel-wide uid.

So the **first implementation milestone is to get one real entry, and one
symlink entry, through the managed path and re-check**, before the three-native
claim is treated as exact.

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
  — a constructor that converts through `ConvertErrorPlatformToPal`. Nothing in
  this slice can reach that arm (see the test plan), but the constraint belongs
  in a comment beside any future failure arm.
- **`Name` points into the stream's own buffer**, valid only until the next
  `readdir` on that stream (`ConvertDirent` sets it to `entry->d_name`). So
  PawPrint must hand back guest-readable memory with that lifetime, not a fresh
  allocation per call that nothing frees.
- **The buffer must be NUL-terminated.** `MoveNext` reads `_entry.Name[1]` and
  `_entry.Name[2]` when `Name[0]` is `'.'`, to spot `.` and `..`. The reads
  short-circuit (`Name[1] == 0 || …`), so they stop *at* the terminator and
  never past it — but only if one is written. Allocate the block
  zero-initialised: on the Linux flavour `NameLength` is `-1`, which sends
  `GetName` through `CreateReadOnlySpanFromNullTerminated` →
  `SpanHelpers.IndexOfNullByte`, whose byte-at-a-time path is safe only because
  guest SIMD is always `ScalarOnly`. Zero fill makes that independent of it.

## Measured, on both kernels

macOS 26.6 / APFS locally, Linux 6.x / aarch64 through
`container run --rm -v … python:3-slim`, unprivileged on both (the Linux
container is root by default and every permission row is vacuous there — the
probes take a `drop` argument that `setuid`s to 1000).

Probes: `scratchpad/readdir/probe{,2,3,4,5}.py`. Every row below agreed on both
platforms; there is no Linux/Darwin divergence anywhere in this table.

| row | answer |
| --- | --- |
| `opendir` consumes a file descriptor | yes, on both (fd 4, between an `open` at 3 and one at 5) |
| `opendir("d")`, `opendir("d/")` | ok |
| `opendir("d/.")`, `opendir("d/..")`, `opendir("/")` | ok |
| regular file, with or without trailing `/` | ENOTDIR |
| **mode-0000 regular file**, with or without trailing `/` | **ENOTDIR** — the type check beats the read check |
| symlink to a regular file, with or without trailing `/` | ENOTDIR (it follows) |
| symlink to a mode-0000 regular file | ENOTDIR |
| nonexistent name, or a dangling symlink | ENOENT |
| symlink to a directory, with or without trailing `/` | ok |
| **symlink to a `0o111` directory**, with or without trailing `/` | **EACCES** — follows, then checks read |
| directory mode `0o111` (search, no read), with or without trailing `/` | **EACCES** |
| directory mode `0o444` (read, no search) | ok, and lists every name |
| directory mode `0o000` | EACCES |
| a name under an unreadable-but-searchable directory | resolves normally |
| a **nonexistent** name under an unsearchable directory | EACCES, not ENOENT — the walk's search check wins |
| `.` and `..` present, and first, in that order | yes |
| `d_type` for regular / directory / symlink / fifo / socket | `DT_REG` / `DT_DIR` / `DT_LNK` / `DT_FIFO` / `DT_SOCK` |
| order of the remaining names | arbitrary, and different between them (`m a z` against `z a m` for one seed) |
| deleting each entry as it is returned, at 5000 entries | skips nothing; the directory ends empty |
| `DirectoryEntry.NameLength` | **-1 on Linux, the byte length on Darwin** |

The ENOTDIR-beats-EACCES row is what pins the verdict function's arm order, and
it is pleasingly symmetric with `open`'s already-recorded "EISDIR beats EACCES".

**What is *not* a rule.** Whether a stream sees a mutation made after `opendir`
is a buffering artifact. Removing the directory before the first `readdir` gives
immediate end-of-stream with no `.` or `..` on both; reading one entry *first*
and then removing it yields the whole listing, dots included, on both. POSIX
leaves it unspecified and each measurement only records when `getdents` happened
to run. PawPrint must therefore choose, and label the choice as a choice.

### The one divergence

`NameLength` is `-1` on Linux and the real byte length on Darwin. The PAL sets
it from `d_namlen` where the libc has that member and to `-1` otherwise
(`pal_io.c:497`, guarded by `HAVE_DIRENT_NAME_LEN`). Confirmed by compiling
rather than by reading: `struct dirent` in glibc has no `d_namlen`
(`error: 'struct dirent' has no member named 'd_namlen'` under `gcc:14`), while
macOS's `sys/dirent.h` declares one.

It is invisible to managed code — `DirectoryEntry.GetName` handles both — so it
belongs to a `sourcesImpure` guest with its own `[DllImport]` declaration of the
struct. It is a fact about the PAL as compiled for a target, so it is
`SimulatedUnixPlatform`, not `KernelConfig`.

## Design decisions

### A. What a `DIR*` is

The measured fd consumption settles the substance: a stream holds a descriptor.

- **Chosen: `opendir` performs the `open`,** yielding an ordinary descriptor
  whose target is `OpenFileTarget.File`. That case already says "a regular file
  **or directory**, and where in it this description is positioned",
  `open(dir, O_RDONLY)` already succeeds, and `heldInodes` already pins a `File`
  inode — which is exactly the pinning a stream over a directory that `rmdir`
  then removes requires, and it came free with #1134. **No new
  `OpenFileTarget` case and no new pinning rule**, so none of that DU's 51 sites
  move.
- Rejected: a `DirectoryStreams` map *instead of* a descriptor. Its only real
  defect is that it consumes no fd, which is observably wrong — a guest can read
  fd numbers back through `SafeFileHandle.DangerousGetHandle` — and it would
  need its own pinning rule duplicating `heldInodes`.

**This is not "no new kernel state".** The cursor, the name buffer and the
`DIR*`→descriptor mapping all need homes and none fits in
`File (inode, offset)`. Two ways:

- (α) the `DIR` block's own bytes carry the whole stream state — fd, phase,
  last name. True to libc, and it makes the no-new-state claim literally true,
  but PawPrint would then be decoding *kernel* behaviour out of guest-writable
  memory, so every read needs validating against a guest that scribbled. That
  is the opposite of making illegal states unrepresentable.
- **(β) Chosen: a kernel-side map.** `opendir` allocates one zero-initialised
  native block of `NAME_MAX + 1` bytes — the name buffer, whose address *is*
  the `DIR*` the guest holds, exactly as `d_name` is a field inside `DIR` — and
  records the stream's fd and cursor in a kernel map keyed by that address.
  Precedent: `EmulatedKernel.Sockets` keyed by `SocketId` and resolved by
  `EmulatedKernel.socket`. An absent key is not a default and must not be read
  as one: it means the guest passed a `DIR*` PawPrint never handed out, which is
  UB on a real libc, so it is a `failwith` naming that condition. `CloseDir`
  twice lands there too, deliberately.

### B. What a mutation mid-stream does, and where the cursor lives

Measured at 5000 entries — well past glibc's 32 KB `readdir` buffer — deleting
each entry as it is returned skips **nothing** on either kernel. So a real
filesystem hands out a **stable per-entry cookie**, not a position, and removing
an already-returned entry does not shift the ones after it.

That is not academic. CoreLib's `FileSystem.RemoveDirectoryRecursive` deletes
each child *inside* the `foreach` over the live enumerator and then `rmdir`s the
parent, so a model that skips entries makes `Directory.Delete(recursive: true)`
throw ENOTEMPTY — on BCL code, not on a guest's bug.

- **Chosen: a name-keyed cursor.** Each `readdir` yields the least name strictly
  greater than the last one returned, in the directory's *current* map. Deleting
  an already-returned name is invisible; deleting a not-yet-reached one removes
  it; a name inserted after the cursor appears and one inserted before it does
  not.
- Rejected: **an index into the live listing**, reusing the `int64` offset. It
  needs no new state and is the least forgiving of the three — but it is less
  forgiving than *either real kernel* rather than merely less forgiving than a
  snapshot, and it breaks the BCL as above.
- Rejected: **a snapshot at `opendir`**. Legal, deterministic and simplest, but
  the most forgiving: it hides every mutation, so a guest relying on a
  consistent point-in-time listing would never be caught here and would break on
  a real kernel with a large directory.

Between the two legal models the choice follows the standing preference for the
least convenient behaviour a guest could lawfully meet.

This is a well-chosen point in the unspecified space, **not an equivalence to a
real cookie**: hash-ordered directories make no promise about whether an
insertion before or after the cursor becomes visible. The choice goes in
`docs/divergences.md` as a choice.

The cursor cannot be a `FileName option`. `FileName` rejects `.` and `..` by
construction (`FileNameError.Reserved`), so "returned `.`, not yet `..`" has no
representation. It is a four-case DU — `Start | ReturnedDot | ReturnedDotDot |
After of FileName` — which is what making illegal states unrepresentable
demands here anyway.

It lives with the stream (decision A(β)) and is dropped at `CloseDir`, which is
where libc keeps its `DIR` buffer. That makes it per-`opendir` rather than
per-open-file-description, so a `dup` of the descriptor would not share it.
Unobservable: `dirfd` appears nowhere in CoreLib or the PAL, so no managed
caller can reach the descriptor to `dup` it.

Cost: a full enumeration is O(n²) in directory size, since each `readdir` scans
the map for the least name above the cursor — about 12.5M comparisons for the
5000-entry recursive delete. Fine for an interpreter that is explicitly not
chasing performance, but stated so nobody rediscovers it in a profile.

### C. Enumeration order

`DirectoryContent.Entries` is a `Map<FileName, InodeNumber>`, so an order is
already available for free — F# ordinal comparison of the *UTF-16* strings.

- **Chosen: that order**, with `.` and `..` ahead of it.
- Rejected: sorting by UTF-8 bytes. More principled — a name *is* bytes — and
  **not more expensive**: least-name-above-cursor is a linear scan under either
  comparator. The honest tiebreak is that the Map's order is determinism for
  free and the two differ only above the BMP.

No real kernel's order can be matched (arbitrary on both, and different), so
**no test may compare order** and every guest must sort.

### D. `.` and `..` are synthesised, except over an orphan

They are not in the map — `FileName` rejects both — and `DirectoryContent`
derives the parent from a field, so the stream produces them.
`EnumerationOptions.ReturnSpecialDirectories` is what makes them visible through
the BCL.

**A stream over a directory `rmdir` has since removed answers end-of-stream at
once, dots included.** An orphan is empty by construction (`rmdir` refuses a
populated directory, and creation inside an orphan is ENOENT since #1134), so
`VirtualFileSystem.isOrphanedDirectory` — which already exists — is the whole
check.

This matches one of the two measured orderings and not the other, and that is
the honest description: it is the "removed before the first `readdir`" answer,
chosen because it is lawful and because it is the less convenient of the two.
It is not exact agreement with the kernels, because the kernels do not agree
with themselves here.

### E. The read bit is *not* new

Correcting an earlier draft of this plan: `SystemNative_Open` already demands
the owner read bit (`0o400`) for `O_RDONLY` on the final target, directories
included, so PawPrint today already answers EACCES for
`open("d0111", O_RDONLY)`. Read and search came apart when `open` landed, not
here.

So there must **not** be a second read predicate in a new `OpenDirRules`. What
`opendir` adds over `open(…, O_RDONLY)` is exactly the `O_DIRECTORY` analogue —
ENOTDIR when the target is not a directory, where `open` succeeds — and the
measured arm order puts that check *first*. Either share the existing owner-triple
predicate or route `OpenDir` through the same decision path as `Open` plus the
ENOTDIR demand; two parallel read checks for one syscall family is the drift the
architecture guidelines warn about.

## Test plan

| tier | what it carries |
| --- | --- |
| `TestVirtualFileSystemAgainstHost` | `opendir`'s verdict against the real `opendir(3)`, and the *set* of names returned (sorted) against the host's — never the order. The corpus must carry the **asymmetric permission pair** (a readable-unsearchable directory and a search-only one), or a verdict that conflates read with search passes every row |
| `TestOpenDirRules` | the measured rows as literals, including the ones the host oracle structurally cannot reach: `opendir("/")` (every corpus path gets a temp-dir prefix, so `/` never arrives as the root), and the ENOTDIR-beats-EACCES ordering. The verdict must accept `ResolvedTarget.Directory` as well as `Entry`, since `d/.` and `d/..` resolve as navigation |
| `sourcesPure/EnumerateSeeded.cs` | the name set, `.`/`..` under `ReturnSpecialDirectories`, enumeration through a symlink, and `Directory.Delete(recursive: true)`. **Only ENOENT is reachable through the BCL**: `GetExceptionForIoErrno`'s ENOTDIR and EACCES arms both build their message through `SystemNative_StrErrorR`, which PawPrint does not implement — the same wall `RmDirSeeded.cs` and `UnlinkSeeded.cs` document. Those two errnos go through a hand-rolled `[DllImport]` with `ConvertErrorPlatformToPal`, as those guests do. Implementing `StrErrorR` is its own slice and must not sneak in here |
| `sourcesImpure/EnumerateWiring{Linux,Darwin}Seeded.cs` | `NameLength`'s two answers through a raw `[DllImport]` of the struct. First thing to park if the slice balloons |
| `TestEmulatedKernelInodeLifetime` | a stream over a directory removed by `rmdir`: the inode stays pinned, is reaped at `CloseDir`, and the stream answers end-of-stream meanwhile |
| terminal-state assertions | that `CloseDir` frees the descriptor, the name buffer *and* the kernel's stream entry — no guest can see any of the three |

Deliberately **not** a test row: the raw errno `ReadDir` returns on failure.
Under the chosen model `ReadDir` cannot fail — the cursor walk is total,
end-of-stream is `-1`, and a `DIR*` PawPrint never issued is a `failwith` rather
than an invented EBADF. There is no provoking input, so a test asserting it
would be vacuous. The constraint lives in a comment instead.

## Out of scope

`Rename`, `ChDir`, `SymLink`, `ChMod`, and `StrErrorR`. Also
`File.SetLastWriteTime`, which is not a filesystem gap at all: it stops in
`FileStatus.SetAccessOrWriteTimeCore` on a byte-view byref of a stack
`TimeSpec[2]` that the interpreter refuses. And `File.Copy`, which on a Darwin
CoreLib P/Invokes `libc!clonefile` directly rather than
`SystemNative_CopyFile`, so it needs a different primitive per flavour.
