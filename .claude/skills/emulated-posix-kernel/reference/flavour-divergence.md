# Where each measured kernel fact lives

Every Linux/Darwin divergence PawPrint models is already written down **as a
test**, with its rows in one place and an assertion that keeps them true. This
file is the index, not a second copy: a table restated here could drift from the
test that enforces it, and the restatement would be the version nobody runs.

So: to learn what a kernel answers, open the test named below and read its rows.
To change what PawPrint answers, change the test first.

## The index

| fact | owned by |
| --- | --- |
| `NAME_MAX`, `PATH_MAX`, the symlink limit | `TestVirtualFileSystemAgainstHost`: `pathLimits states this kernel's real symlink limit exactly`, `…real PATH_MAX exactly`, `pathLimits agrees with this kernel about which names are too long` |
| symlink splicing, and every path-resolution shape | `TestVirtualFileSystemAgainstHost`: `the model splices symlink targets exactly as this kernel does`, `the model resolves every probe path exactly as this kernel does`; shape-by-shape unit rows in `TestVirtualFileSystem` |
| `open(O_CREAT)` — trailing separators, existing directories, the root, the mode mask | `TestVirtualFileSystemAgainstHost`: `a creating open decides exactly as this kernel does`, kept honest by `the creating-open corpus reaches every verdict this kernel can give` |
| truncation and the set-ID bits | `TestVirtualFileSystem`: `truncation strips the set-ID bits only on Linux, and only for a non-root caller` — the measured table as octal literals, deliberately not computed from `afterTruncation`, and covering root as well as non-root. Live host comparison in `TestVirtualFileSystemAgainstHost`: `truncation moves a file's set-ID bits exactly as this kernel does`, `truncation leaves a file's ordinary permission bits alone` |
| `ftruncate` and `O_TRUNC` end-to-end, including which descriptor kinds refuse | `sourcesPure/TruncateSeeded.cs`, plus `sourcesImpure/TruncateWiring{Linux,Darwin}Seeded.cs` for the wiring |
| `fstatfs` magic numbers, and which flavour can mount what | `TestFileSystemType`: `each filesystem's magic number is the one CoreLib reads it back as` (a `DllImport` of the PAL function itself), `every flavour's default is a filesystem that flavour can mount`, `a filesystem the flavour could not mount is refused` |
| `lseek` whence handling per flavour | `TestLSeek`: `the refused whence is named per the simulated platform`, `Darwin decides seekability before the whence is validated`, `Darwin reports EOVERFLOW where Linux reports EINVAL` |
| the socket event buffer's stride and count cap | `TestLinuxEpollLimits`: `the element size is the x86-64 packed struct`, `the cap is exactly the largest count whose byte extent fits in int32` |
| which socket triples can be created | `TestSocketCreation` against `socketMatrix/{linux,darwin}.tsv` — the one table that *is* checked in as data, because its answers are privilege-dependent and no single machine can measure both columns |
| `accept(2)`'s error matrix — EAGAIN / EINVAL / EOPNOTSUPP / ENOTSOCK / EBADF and their check ordering, on which the kernels agree in classification and differ only in numbering | `sourcesPure/SocketAccept.cs` (PAL codes, differential), `sourcesImpure/SocketAccept{Linux,Darwin}.cs` (raw numbers) |
| `ENOTSOCK` from a socket syscall on a non-socket descriptor, measured per entry point (`accept`/`bind`/`listen`/`getsockname` × file/event port/pipe) | the same three guests: `SocketAccept.cs` rows 35-40, `SocketAccept{Linux,Darwin}.cs` rows 19-27 |
| `epoll_ctl`'s error matrix and check ordering — EBADF / EPERM / EINVAL / EEXIST / ENOENT, the (fd, description) registration key, and where kqueue diverges (silent-replace ADD, file targets succeeding, dead-target DEL answering ENOENT not EBADF) | `sourcesPure/SocketEventRegistration.cs` (the rows the flavours agree on, differential), `sourcesImpure/SocketEventRegistrationLinux.cs` (the epoll-only rows; the Darwin kernel arm is a refusal, so no Darwin twin exists) |
| `connect(2)`'s ladder — blocking loopback success, EINPROGRESS even on loopback, ECONNREFUSED, and where the flavours diverge (listening-socket connect EISCONN/EOPNOTSUPP, refusal delivery then reset/dead-latch, bound-not-listening RST/SYN-drop, AF_UNSPEC accepted/refused, oversized sockaddr, accept-queue capacity backlog+1/backlog) — plus `accept(2)`'s dequeue: FIFO order, peer/own addresses, the accepted fd blocking through the PAL | `sourcesPure/SocketConnect.cs` (the agreement rows, differential), `sourcesImpure/SocketConnectLinux.cs` / `SocketConnectDarwin.cs` (the divergent rows and raw errnos, each confirmed on its real runtime); `TestEmulatedKernelSockets` (connection identities, queue write-back, the close sweep, capacity boundaries) |
| environment entries a real `environ` can hold | `TestEnvironmentEntryInvariant`: `the entry rule accepts what a real environ can hold`, `the entry rule names what is wrong` |

## The envelope those measurements were taken in

| varied | held fixed |
| --- | --- |
| platform (macOS 26.6 / APFS, Linux 6.x arm64 via `container`) | one filesystem per platform |
| uid, where it changes the answer | the caller's supplementary groups |
| the flag or argument under test | capabilities — no `CAP_FSETID`, no `CAP_NET_RAW`, unless a row says root |

A claim outside that envelope — a second filesystem, a mount option, a group the
caller is not in, a capability — is **unmeasured**. Measure it and add a row;
do not extrapolate from a neighbouring one.

Note what the host oracles can and cannot reach. They compare against *this*
machine, so each platform's column is falsified only where that platform runs:
macOS locally, Linux in CI. And a privilege-dependent row is checked only at the
privilege the suite happens to run at, which is why the truncation table is
*also* written out as literals in a unit test.

## Facts no test states, because they are about upstream rather than about us

- **`SystemNative_GetFileSystemType` returns 0 for every failure**, not -1. Its
  only managed consumer is `SafeFileHandle.CanLockTheFile`, which refuses a
  *shared* lock under write access on nfs/smb/smb2/cifs — the combination
  `File.WriteAllBytes` asks for. `File.Create` is `FileShare.None` → `LOCK_EX`,
  and never reaches it.
- **`Ext4` cannot be a case.** CoreLib's `UnixFileSystemTypes` has no `ext4`
  member — it is `ext2 = 0xEF53`, with `ext4` commented out as an alias — so the
  managed layer cannot distinguish it.
- **A separate Linux rule strips `S_ISGID` on creation**, when the caller is
  outside the directory's group and lacks `CAP_FSETID`. Unmeasured here and not
  modelled: PawPrint gives every inode `Kernel.GroupId`, so no guest can be
  outside that group. That approximation is what makes
  `CreatingOpenRules.createdPermissions` exact, and it ends when inodes can carry
  a group of their own.
- **The environment is a list, not a map.** A process's environment is a list of
  `name=value` strings; the map every .NET API presents is a *view*, split at each
  entry's first `=`. CoreCLR makes the view total from both ends —
  `GetEnvironmentVariableA` (`pal/src/misc/environ.cpp`) refuses a lookup name
  that is empty or contains `=` *above* its matching loop, and
  `Environment.GetEnvironmentVariables` discards any entry whose first `=` is not
  after the first character. Measured on real .NET via `execve` with a hand-built
  envp: `A=B=C` gives `"A"` = `"B=C"` and `"A=B"` = null; `=C` is invisible to
  both APIs; `DUP=1` with `DUP=2` gives `DUP` = `"1"`. PawPrint stores the *map*,
  so its lossiness runs the other way — it could hold names the view could never
  yield — which is what `environmentEntryProblem` exists to reject.
