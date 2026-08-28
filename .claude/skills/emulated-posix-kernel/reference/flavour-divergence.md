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
| the directory *search* bit during a walk — which is **unanimous**, and is here because the corpus that pins it is shared with the rows above | the same two host-oracle tests, whose corpus carries unsearchable directories (`ns`, `cls677`, `cls500`); `TestWalkSearchPermission` for the precedence rows, the owner-triple pair and the privileged bypass; `sourcesImpure/SearchPermission{,Root,Cwd}Seeded.cs` for the wiring |
| `open(O_CREAT)` — trailing separators, existing directories, the root, the mode mask | `TestVirtualFileSystemAgainstHost`: `a creating open decides exactly as this kernel does`, kept honest by `the creating-open corpus reaches every verdict this kernel can give` |
| truncation and the set-ID bits | `TestVirtualFileSystem`: `truncation strips the set-ID bits only on Linux, and only for a non-root caller` — the measured table as octal literals, deliberately not computed from `afterTruncation`, and covering root as well as non-root. Live host comparison in `TestVirtualFileSystemAgainstHost`: `truncation moves a file's set-ID bits exactly as this kernel does`, `truncation leaves a file's ordinary permission bits alone` |
| `ftruncate` and `O_TRUNC` end-to-end, including which descriptor kinds refuse | `sourcesPure/TruncateSeeded.cs`, plus `sourcesImpure/TruncateWiring{Linux,Darwin}Seeded.cs` for the wiring |
| `mkdir(2)` — what a trailing separator costs on the final component, the mode mask, and set-group-ID inheritance | `TestVirtualFileSystemAgainstHost`: `mkdir decides exactly as this kernel does`, kept honest by `the mkdir corpus reaches every verdict this kernel can give`; the mode tables live against the host in `a new directory keeps exactly the mode bits this kernel keeps` and `a new directory inherits set-group-ID exactly as this kernel does`. `TestMkDirRules` carries the same tables as octal literals plus the rows the host oracle structurally cannot reach: the flavour it is not running on, `mkdir("/")`, the permission bits, and *which name* a creation binds (Darwin's `mkdir("dang/")` binds the link's target) |
| `mkdir` end-to-end, including the uid rule in both directions | `sourcesPure/MkDirSeeded.cs`, plus `sourcesImpure/MkDirWiring{Linux,Darwin}Seeded.cs` for the wiring and `sourcesImpure/MkDirPrivilegedSeeded.cs` for uid 0 |
| `unlink(2)` — which errno each refusal carries, the *order* the checks are made in, and whether a trailing separator resolves the final symlink | `TestVirtualFileSystemAgainstHost`: `unlink decides exactly as this kernel does, and destroys the same thing`, which compares a before/after **tree delta** rather than an errno, kept honest by `the unlink corpus reaches every verdict this kernel can give`. `TestUnlinkRules` carries both columns as measured rows, including the ones the host oracle structurally cannot reach: its stand-in root is an ordinary directory, so Darwin's EBUSY-for-the-mount-root arm is invisible there |
| `unlink` end-to-end, including the uid rule | `sourcesPure/UnlinkSeeded.cs` for what the flavours agree on, `sourcesImpure/UnlinkWiring{Linux,Darwin}Seeded.cs` for the wiring, `sourcesImpure/UnlinkReapSeeded.cs` for the inode-lifetime rule (asserted on the terminal state, since no guest can read it) |
| `opendir(3)` — which errno each refusal carries and the order the checks are made in, on which the two kernels **agree in every row measured**, which is why `OpenDirRules.verdict` takes no flavour | `TestOpenDirRules` carries the rows as literals, including the two the host oracle structurally cannot reach: the real root (`/`, `/.`, `/..` — its stand-in root is an ordinary directory) and what uid 0 sees. `TestVirtualFileSystemAgainstHost`: `opendir decides exactly as this kernel does`, kept honest by `the opendir corpus reaches every verdict this kernel can give` |
| **`opendir` demands the *read* bit, not the search bit** — the first place in this codebase where the two come apart. A `0o111` directory (search, no read) is EACCES on both; a `0o444` one (read, no search) opens and lists every name | `TestOpenDirRules`' `dr`/`dw` pair, which exists to be asymmetric: a verdict demanding search would refuse one and admit the other and every other row would still pass. The host oracle's `ns` (0o666) is the same row against a live kernel |
| **Being a file beats being unreadable**: `opendir` on a *mode-0000* regular file is ENOTDIR, not EACCES, on both — with and without a trailing separator, and through a symlink to one | `TestOpenDirRules`: `being a file beats being unreadable` |
| `readdir(3)`'s `d_type` for each inode kind, and that `.` and `..` are yielded by the *stream* rather than stored in the directory | `sourcesPure/EnumerateSeeded.cs`, which walks the raw shim. Not implied by the `GetFiles`/`GetDirectories` split: `FileSystemEntry` consults `InodeType` only when it is not `DT_UNKNOWN` and falls back to `stat` otherwise |
| **`.` and `..` have no fixed position, and neither does anything else.** Measured, a directory holding one name `z` enumerates as `. .. z` on APFS and in a Linux container, and as `z .. .` on CI's ext4 — dots last, and in the other order. The two disagreeing machines are both ext4, so this is not a flavour split | the measured table in `docs/divergences.md`, "Directory enumeration order is the model's own, not any kernel's". Nothing asserts an order: `EnumerateSeeded.cs` sorts the shim's own walk, dots included. An earlier version asserted dots-first and passed on two machines before CI falsified it — and PawPrint's own stream now puts the dots **last**, the less convenient of the two measured orders, so a guest making that assumption fails deterministically instead of on whichever machine runs it |
| **`DirectoryEntry.NameLength` diverges**: `-1` on Linux, the name's length in bytes on Darwin. `ConvertDirent` copies `d_namlen` under `HAVE_DIRENT_NAME_LEN` and writes the sentinel otherwise; established by *compiling*, since glibc's `struct dirent` has no such member. Invisible to managed code, which decodes both to the same string | `SimulatedUnixPlatform.directoryEntryNameLength`, asserted end-to-end by `sourcesImpure/EnumerateWiring{Linux,Darwin}Seeded.cs` through a hand-rolled `[DllImport]`. Their names are 1, 2 and 6 bytes long over 1, 1 and 2 characters, so a `String.Length` rule agrees on the first only |
| directory enumeration end-to-end, including `Directory.Delete(recursive: true)` and a stream over an `rmdir`'d directory | `sourcesPure/EnumerateSeeded.cs`. What a mutation mid-stream does is **not** a kernel fact — both kernels' answers are buffering artefacts — and PawPrint's choice is recorded in `docs/divergences.md` and pinned by `TestDirectoryEnumeration` |
| `rmdir(2)` — which errno each *navigation* carries, the order the checks are made in, whether a trailing separator resolves the final symlink, and whether the removed directory's own `ctime` moves | `TestRmDirRules` carries both columns as measured rows, including the root-navigation arms the host oracle structurally cannot reach. The two flavours split on *what the root is*: Linux specialises the path that consumed no component (`/` is EBUSY, `/.` is EINVAL) where Darwin specialises the root inode (`/` is EISDIR, `/.` is EBUSY). `TestVirtualFileSystemAgainstHost`: `rmdir decides exactly as this kernel does, and destroys the same thing` |
| **`rmdir` destroys different objects on the two flavours.** With `ld -> d` and `d` empty, `rmdir("ld/")` removes `d` on Darwin and is ENOTDIR on Linux — the divergence `Resolution.FinalSymlinkFollowed` warns about, and the reason that fixture compares tree deltas rather than errnos | The host oracle's `lsub` corpus row, and `sourcesImpure/RmDirWiringDarwinSeeded.cs`, which asserts the removal *and* that the link itself survives |
| `rmdir` end-to-end, and what an orphaned directory can still do | `sourcesPure/RmDirSeeded.cs` for what the flavours agree on, `sourcesImpure/RmDirWiring{Linux,Darwin}Seeded.cs` for the wiring, `sourcesImpure/RmDirOrphan{Linux,Darwin}Seeded.cs` for standing in a removed directory. The ancestor pinning and the cascading reap are asserted on the terminal state, since no guest can read them |
| `getcwd(3)` from a *removed* current directory — EINVAL on both at size 0, ENOENT on Linux at every other size, and on Darwin ERANGE at size 1 with ENOENT from 2 up (a minimum, not a comparison against the stale path) | `SimulatedUnixPlatform.getCwdOrphanAnswer`, asserted end-to-end by the last rows of each `RmDirOrphan*Seeded.cs` — including the size-5 row, which is what distinguishes the minimum from a path-length check. Only reachable since `rmdir` could orphan a current directory |
| `fstatfs` magic numbers, and which flavour can mount what | `TestFileSystemType`: `each filesystem's magic number is the one CoreLib reads it back as` (a `DllImport` of the PAL function itself), `every flavour's default is a filesystem that flavour can mount`, `a filesystem the flavour could not mount is refused` |
| `lseek` whence handling per flavour | `TestLSeek`: `the refused whence is named per the simulated platform`, `Darwin decides seekability before the whence is validated`, `Darwin reports EOVERFLOW where Linux reports EINVAL` |
| the socket event buffer's stride and count cap | `TestLinuxEpollLimits`: `the element size is the x86-64 packed struct`, `the cap is exactly the largest count whose byte extent fits in int32` |
| which socket triples can be created | `TestSocketCreation` against `socketMatrix/{linux,darwin}.tsv` — the one table that *is* checked in as data, because its answers are privilege-dependent and no single machine can measure both columns |
| `accept(2)`'s error matrix — EAGAIN / EINVAL / EOPNOTSUPP / ENOTSOCK / EBADF and their check ordering, on which the kernels agree in classification and differ only in numbering | `sourcesPure/SocketAccept.cs` (PAL codes, differential), `sourcesImpure/SocketAccept{Linux,Darwin}.cs` (raw numbers) |
| **`accept(2)`'s O_NONBLOCK inheritance**: the accepted socket inherits the listening *description*'s flag on Darwin and does not on Linux. Invisible to a guest, because `SystemNative_Accept` clears it under `#if !defined(__linux__)` — "Our socket code expects new socket to be in blocking mode by default" — which is why the kernel fact and the PAL's normalisation are modelled in different places | `SimulatedUnixPlatform.acceptedSocketInheritsNonBlocking`, measured by `docs/plans/2026-08-23-posix-kernel-extraction/accept-inherits-nonblock.c` on Linux 6.18.5 and Darwin 25.6.0; asserted by `TestAccept`'s three inheritance rows, which is the only tier that can see it |
| `ENOTSOCK` from a socket syscall on a non-socket descriptor, measured per entry point (`accept`/`bind`/`listen`/`getsockname` × file/event port/pipe) | the same three guests: `SocketAccept.cs` rows 35-40, `SocketAccept{Linux,Darwin}.cs` rows 19-27 |
| `epoll_ctl`'s error matrix and check ordering — EBADF / EPERM / EINVAL / EEXIST / ENOENT, the (fd, description) registration key, and where kqueue diverges (silent-replace ADD, file targets succeeding, dead-target DEL answering ENOENT not EBADF) | `sourcesPure/SocketEventRegistration.cs` (the rows the flavours agree on, differential), `sourcesImpure/SocketEventRegistrationLinux.cs` (the epoll-only rows; the Darwin kernel arm is a refusal, so no Darwin twin exists) |
| `connect(2)`'s ladder — blocking loopback success, EINPROGRESS even on loopback, ECONNREFUSED, and where the flavours diverge (listening-socket connect EISCONN/EOPNOTSUPP, refusal delivery then reset/dead-latch, bound-not-listening RST/SYN-drop, AF_UNSPEC accepted/refused, oversized sockaddr, accept-queue capacity backlog+1/backlog) — plus `accept(2)`'s dequeue: FIFO order, peer/own addresses, the accepted fd blocking through the PAL | `sourcesPure/SocketConnect.cs` (the agreement rows, differential), `sourcesImpure/SocketConnectLinux.cs` / `SocketConnectDarwin.cs` (the divergent rows and raw errnos, each confirmed on its real runtime); `TestEmulatedKernelSockets` (connection identities, queue write-back, the close sweep, capacity boundaries) |
| the readiness delivery — per-phase epoll levels (idle stream OUT\|HUP, datagram OUT with no HUP, listener IN iff queue nonempty, established OUT, pending refusal 0x201d, pipe ends), reporting as level ∩ (interest ∪ {ERR, HUP}), the ready list's order (edge arrival; re-signal immobility; ADD/MOD-of-ready at ADD/MOD time; same-socket dup ties LIFO by registration, MOD not moving them), which operations signal (queue push, connect resolution, refusal reset — not bind, not the SUCCESS report, not UDP re-target), truncation, and the PAL's EPOLLHUP→IN\|OUT fold at delivery | probes in `docs/plans/2026-08-21-socket-readiness-wake/`; `sourcesPure/SocketEventDelivery.cs` (portable rows plus the wake, differential), `sourcesImpure/SocketEventDeliveryLinux.cs` (exact masks and order; Linux-flavour only, since Darwin's registration arm refuses), `TestSocketEventDelivery` (the ready list itself, one test per measured row) |
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
