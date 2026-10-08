---
name: emulated-posix-kernel
description: Deciding how the emulated kernel should answer a syscall — whether a fact belongs to the flavour, to configuration, or to the interpreter; whose encoding a value is stated in; where per-thread state lives; which test tier can observe the answer. Use when adding or changing anything in WoofWare.PosixKernel (the Unix* syscall files — UnixSocket.fs, UnixConnection.fs, UnixPoll.fs, UnixKqueue.fs, UnixDescriptor.fs, UnixPathResolution.fs, UnixReadWrite.fs, UnixNamespace.fs, UnixPipe.fs, UnixSignal.fs, UnixClock.fs, UnixEntropy.fs, UnixCredentials.fs, UnixTaskLifecycle.fs, UnixWait.fs, UnixSystem.fs, UnixBootImage.fs — as well as the platform profile and the per-syscall divergence rules it selects between — SimulatedUnixPlatform.fs, SimulatedUnixFlavour.fs, StartingPointRules.fs, CreatingOpenRules.fs, MkDirRules.fs, MkNodRules.fs, RemovalRules.fs, RenameRules.fs, AccessRules.fs, StatRules.fs, AttributeChangeRules.fs, TimestampChangeRules.fs, OwnerChangeRules.fs, ProtectedFiles.fs, Sockaddr.fs, UserBuffer.fs, EmulatedFileSystemType.fs — and VirtualFileSystem.fs, PathWalk.fs, FileDescriptorRegistry.fs, Signal.fs), in EmulatedKernel.fs or SignalState.fs, in a Native/*Pal.fs adapter, or in a Native/ handler that reports kernel state. Carries measured Linux/Darwin divergence tables — consult them rather than re-measuring.
---

# Deciding what the emulated kernel says

PawPrint models a Unix kernel as *data*, never as a host read: a replay must not
depend on the machine that produced it. That constraint decides most of what
follows.

## 1. Where does this fact live?

First ask whether it is the kernel's at all. `WoofWare.PosixKernel/README.md`,
"What the kernel leaves to the client", lists what the library deliberately does
not do — user memory and `mmap`, blocks inside the process (`futex`,
`__psynch`), choosing which task runs, when time passes, the errno slot,
running a signal handler, `sigaltstack`, process creation — with why each is
the client's and how the two meet. Change that section when you move the line.

If it is the kernel's, there are four homes, and picking the wrong one is the most common mistake in this area
because three of them look alike from the call site.

| home | the fact is… | examples |
| --- | --- | --- |
| `SimulatedUnixPlatform` | true of *this kernel's source*, the same on every machine running it | the architecture and page size the image was built for, `sa_family_t` width, `AF_INET6`'s number, `sizeof(struct sockaddr_storage)`, `reportsBirthTime`, `setIdBitsOnTruncation`, `creatingOpenRules` |
| `KernelConfig` | true of *this machine or mount or process*, and a different admin could change it | `Mount`, `UserAddressLimit`, `UserId`, `Umask`, `ProcessorCount`, `WallClockEpochMs`, `ProtectedFiles` |
| CoreLib flavour | not modelled at all — it decides which *guest* code path exists | `Environment.OSVersion`'s implementation, `Lock.ThreadId.InitializeForCurrentThread` |
| the interpreter | an artefact of how PawPrint represents memory, which no real kernel has | `Int32.MaxValue / stride` limits on a native block |

The test that separates the first two: **could two machines running the same
kernel image disagree?** A sysctl, a mount option, a uid — yes, so it is
configuration. `sizeof(struct sockaddr_un)` — no, so it is the platform.

Two entries currently in `SimulatedUnixPlatform` do not pass that test, and are
there as deliberate approximations rather than as precedent. `pathLimits` holds a
`NAME_MAX` that varies per mount on Linux, and `bindableEntryNames` holds which
names APFS will bind, because PawPrint models exactly one filesystem a name can be
created in per flavour (the device filesystem at `/dev` is closed: no name is
created in it). Each has a named trigger for becoming configuration, stated
beside it: a second filesystem. Do not cite either as a reason to put a
machine-dependent fact in the platform.

A symbolic link's mode is a worked example of the line between the two: the
*rule* (`symlinkCreationPermissions`: Linux 0777, Darwin `0777 & ~umask`) is the
platform's, the *umask* it is applied to is the creating process's, and the
result is stored on the inode. A seeded link was created by some other process,
so it gets the rule applied to `SeedEntry.symlinkCreatorsUmask` (022), not to
this run's configured umask.
`SystemNative_GetFileSystemType` is the worked example: the value is a *mount*
fact, so it lives in `KernelConfig.Mount`, even though the flavour
constrains which types are possible (`Tmpfs` is Linux-only, `Apfs` Darwin-only).

Two traps when adding a `KernelConfig` field:

- **`KernelConfig.Default` is a static value with `UnixPlatform` baked in.** A
  non-optional field whose sensible default depends on the platform gets one
  fixed default, so every `{ Default with UnixPlatform = macOsArm64 }` site
  silently keeps the other platform's value. Make it an `option` and resolve it
  in `toKernel`.
- **The platform is a constructor argument, never a setter's.**
  `UnixSystem.initial` and `EmulatedKernel.image` take it, and nothing
  changes it afterwards, so a setter that reads `machine.UnixPlatform` for a
  flavour-derived default (`withSoMaxConn`, `withMount`, the limits
  `withFileSystem` admits a seed's names under) is not
  order-dependent: there is no platform setter to run after it. A field whose
  default depends on the flavour is an `option` in `KernelConfig`, resolved by
  such a setter in `toKernel`. When one knob's answer is not well-formed
  without another's, resolve it where both are final: a current directory is
  an inode of *this* filesystem, and a new filesystem invalidates every inode
  number the previous one handed out, so `ProcessLaunch` holds the directory as
  a path and the launch resolves it against the machine it starts on.
- **How a process starts is a `ProcessLaunch`, not a boot-image setter.** The
  first process (`UnixBootImage.boot`) and every later one
  (`SimulatedMachine.launch`) start from one, so a per-process knob goes on
  `ProcessLaunch` and a machine knob on `UnixBootImage`. The kernel chooses a
  later process's ID; only the first one's is configuration.

## 2. Per-thread state: field or map?

Stated on `ThreadState.Cpu`, and the criterion is **whether an absent key has a
truthful reading**:

- **Field on `ThreadState`** when it does not — `Cpu`, `OsThreadId`. There is no
  honest default processor index, and an arbitrary one aliases thread ids, which
  silently breaks `System.Threading.Lock`. As a field, the compiler asks every
  future thread-creation site which value it wants.
- **`Map<ThreadId, _>` on the kernel** when it does — `SignalState`'s handler
  frames (no frame, so an empty mask), the last-error slots (errno 0).

The map form carries an obligation that is easy to miss: **remove the entry when
the value returns to the default.** `EmulatedKernel` is compared for equality to
decide whether a step changed anything, so a stored default is a state that looks
different while behaving identically. No guest can observe it; it corrodes
determinism. `SignalState.sigreturn` drops a task's last frame rather than
storing an empty stack, and `TestSignalState.fs` / `TestLastError.fs`
property-test that against a store-everything oracle: reads must agree, **and** no
default may ever be stored.

`SignalState.Blocked` is such a map: a task absent from it blocks nothing, and
an empty mask is never stored. `sigprocmask(2)` and delivery set a task's mask,
`sigreturn` restores the mask its frame saved, a new task copies its creator's,
and a task's exit drops it. A test that needs a task to block signals calls
`UnixSignal.pthreadSigmask`; putting it in a handler (`HandlerFrames` in
`WoofWare.PosixKernel.Test`, `SignalFrames` in `WoofWare.PawPrint.Test`) still
works, and is what a test of frames wants. A mask is a `SignalMask`, not a set
of signals, because Darwin keeps bit 31, which names none.

## 3. Whose encoding is this?

`WoofWare.PosixKernel` is a published package meant to be usable by a client that
has never heard of .NET, so **it states POSIX values and no client's encoding of
them**: a raw `<errno.h>` number, epoll's own readiness conditions, a signo, the
set of sockets it will create. A conversion between one of those and a .NET
encoding goes on PawPrint's side, in `WoofWare.PawPrint/Native/`, as a
`*Pal.fs` adapter beside the seventeen that live there (count them with
`ls WoofWare.PawPrint/Native/*Pal.fs`):

- errors and signals: `UnixErrorPal` (`Interop.Error`'s numbering),
  `PosixSignalPal` (the managed `PosixSignal` enum), `KillSignalPal`
  (`Process.Kill`'s `Interop.Sys.Signals`);
- sockets and readiness: `SocketEventsPal` (the `SocketEvents` bits),
  `SocketArgumentsPal` (the `AF_*`/`SOCK_*`/`PT_*` numbering and the shim's
  argument screens), `SocketShimPal` (compile-time constants the shim
  reports, such as its socket-address sizes), `SocketOptionPal` (the
  managed `SocketOptionLevel`/`SocketOptionName` pairs, what the shim does
  with each, and its `LingerOption`), `PollEventsPal` (the `PollEvents`
  bits);
- files: `OpenFlagsPal`, `PipeFlagsPal`, `FileAdvicePal` (the shim's
  `OpenFlags`, `PipeFlags` and `FileAdvice` numberings and their screens),
  `FileStatusPal` (`st_flags` as the shim reports it), `FileSystemTypePal`
  (the shim's filesystem-type numbers), `DirectoryEntryPal` (the shim's
  `DirectoryEntry`);
- the rest: `ClockPal` (which clock each timestamp entry point reads),
  `EnvironmentPal` (the PAL's view of the environment), `OsThreadIdPal` (the
  two widths the shim reports a thread ID in).

`scripts/check-pal-residue.py` enforces this as the `pal-residue` flake check.
The allowlist is **empty**, and the check is now a ratchet: adding an entry is a
visible act you will be asked to justify.

Two things about it are worth knowing before you touch this area.

- **It cannot see prose.** Docstrings are excluded before the body scan, so a
  declaration with a POSIX name and a PAL-flavoured docstring reads as clean.
  Three consecutive stages of the extraction found one while retiring a cluster
  — `SocketEventInterest`'s fields named as the PAL's five bits, `SocketDomain`
  documented as "`AF_INET`, PAL 2", `Signal.Other` claiming to carry "the raw
  managed `PosixSignal` enum value", which it never could. Each would have turned
  the check green over a residue it cannot read. **Read the prose when you move
  or retire something**; nothing else will.
- **It cannot see a bare numeric table** under an innocent name. It used to be
  largely covered by the library's PAL constants living in one
  `module private Pal`, so an adapter essentially had to write `Pal.` to exist;
  that module has gone to `SocketArgumentsPal`, so this is now guarded by the
  name markers alone.

The script's own header lists these and the rest of what it deliberately misses.

## 4. Is this an identity, or a contention key?

`OpenFileObject` is the `flock` contention key, *not* a general-purpose identity.
Code that needs to tell two descriptions apart wants `OpenFileDescriptionId`.

Before keying it on anything per-description, ask what the *kernel* contends on
and **measure that descriptor kind**; do not generalise from a neighbouring one.
Measured, epoll descriptors and an `eventfd` share a single `anon_inodefs` inode,
so two epoll ports contend under `flock` and `OpenFileObject.AnonymousInode` is
payload-free. That is a fact about those creators, not about anonymous inodes as
a class: Linux hands a distinct inode to files created through
`anon_inode_getfile_secure` and `anon_inode_create_getfile`, so a future
descriptor kind on `anon_inodefs` may well need its own identity. Sockets are on
`sockfs` with an inode each, and do not contend.

Giving each epoll port its own identity granted two exclusive locks where Linux
grants one — guest-visible, and invisible to any test that only locks one port.

## 5. Refuse rather than invent — but check for an observer first

When the platforms disagree and PawPrint has not modelled the distinguishing
state, `failwith` naming the missing input beats returning a plausible constant:
a constant becomes a lie the moment the state it depends on lands. This is why
`SystemNative_FStat` refuses a socket — seventeen fields would be invented and
the platforms agree on none — while `SystemNative_GetFileSystemType` answers,
having one field measured on both.

Inside `WoofWare.PosixKernel`, "refuse" means a typed `Error`, never a throw,
and that holds for boot configuration too: a setter in `UnixBootImage` or
`ProcessLaunch` that rejects a well-formed value (`withBootTime`, `withMount`,
`withTcpSendSpace`, `withProcessId`, `withCredentials`, …) returns a `Result`
whose `Error` is a refusal type of that setter's own, with a `describe`, whose
cases state facts and name no knob. Each case's docstring says whether the
value is unmeasured, unmodelled or contradictory. `KernelConfig.toKernel`
matches every case and fails with the `KernelConfig` field the value came from
(`"KernelConfig.Mount: "` and so on), as
`EmulatedKernel.withFileSystemAndCurrentDirectory` does for
`FileSystemSeedFault`. Only a forged value (`Unchecked.defaultof`) still throws, and
a setter keeps a `context` parameter only to name the knob in that throw.

Before concluding a modelled constant has **no guest observer**, enumerate the
interpreter's own limits that are *arithmetic in that constant*: a byte-offset
bound, a block-count cap, `Int32.MaxValue / stride`. Each is a boundary a guest
can bisect, and such a row belongs in `sourcesImpure` by construction, because a
real 64-bit libc succeeds at every count. Keep any machine-state unit test too:
it pins the value where the boundary pins only the ratio.

## 6. Establish the fact by measuring, not by reading source

For any kernel-behaviour constant, write the measure-the-host test *first* and
let it name the value. Use source only to learn the rule's **shape** — is this a
pointer test or a range test? — which is far more stable across versions than the
constant.

`SimulatedUnixPlatform.linuxX64` names a release string, and that is what the
guest reads from `uname`; it is **not** evidence about the kernel a test runs on.
Reading `access_ok` at the preset's version gave a sign-bit split where CI
measured `TASK_SIZE_MAX`, because x86 changed the rule between 6.9 and 6.12.

No fact follows from the release string. A fact that changed between Linux
versions follows the platform's `LinuxKernelVersion` (`SimulatedUnixKernel.Linux`),
which each preset states for itself (`linuxX64` 6.17.0, `linuxArm64` 6.18.5):
`getSockNameFaultLength` is the first, after Linux 6.18 moved the length's store
ahead of the copy, which CI's 6.17 caught where 6.18.5 had been measured. A
host-equality test of such a fact models the host's *own* version
(`HostPlatform.onUnixHostKernel`), so it holds on whichever kernel runs it, and
its rows should come from kernels on both sides of the change.

See `reference/probing.md` for the probe technique, including the two ways a
set-ID measurement reads as "unsupported" when it is not.

## 7. Which test tier can see it?

Summarised here; `reference/testing.md` has the detail.

- **Host-equality test** (`TestVirtualFileSystemAgainstHost`, `TestPlatformSocketSupport`)
  — for a fact measurable on the machine running the test. Assert *equality*
  against the host and make the failure message report the measured value, so one
  CI run corrects a wrong constant rather than merely rejecting it. macOS locally
  and Linux in CI each falsify one column, so divergent rows belong here too.
- **`sourcesPure`** — differential against real .NET on the *host*, so only claims
  that hold on both flavours. `ELOOP` agrees as an errno but not as a number
  (40 against 62).
- **`sourcesImpure`** — PawPrint only. Its value is that it sees the *wiring*: a
  unit test passes the rules in by hand, so a handler that hardcodes
  `Kernel.Umask` or `Kernel.UnixPlatform` instead of reading them survives every
  such test. Set both away from their defaults in the registration. Any fixture
  that runs a guest under a chosen `KernelConfig` catches this too —
  `TestPlatformSocketSupport` drives a raw-`DllImport` guest through
  `BoundedRun.run` under both platforms for exactly that reason — so the choice
  is between hand-fed unit tests and *some* guest-running fixture, not between
  unit tests and this directory.

## Reference

- `reference/flavour-divergence.md` — an index from each measured Linux/Darwin
  fact to the test that owns its rows, the envelope those measurements were taken
  in, and the few facts no test states. **Read the test, do not re-measure.**
- `reference/descriptor-kinds.md` — the same for descriptor kinds, plus three
  places where reading the kernel source gives the wrong answer.
- `reference/testing.md` — choosing a tier, and the traps in each.
- `reference/probing.md` — running a probe on both platforms, and the two
  environment facts that make a probe lie.
