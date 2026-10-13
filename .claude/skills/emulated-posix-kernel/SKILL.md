---
name: emulated-posix-kernel
description: PawPrint's side of the emulated kernel — wiring a guest's syscall through to the WoofWare.PosixKernel package and back. Whether a fact belongs to the kernel's platform, to KernelConfig, to the CoreLib flavour or to the interpreter; which side converts a .NET encoding; where per-thread state lives; how to refuse; how to get a change into the kernel; which test tier can observe the answer. Use when touching a Native/ handler that reports or changes kernel state, a Native/*Pal.fs adapter, EmulatedKernel.fs or KernelConfig, or when PawPrint needs something the kernel package does not provide. The kernel's own internals, its measured Linux/Darwin divergence tables and its probing technique are in the WoofWare.PosixKernel repository's skill of the same name.
---

# Wiring the emulated kernel into PawPrint

PawPrint models a Unix kernel as *data*, never as a host read: a replay must not
depend on the machine that produced it. The kernel itself is the NuGet package
`WoofWare.PosixKernel`, developed at
https://github.com/Smaug123/WoofWare.PosixKernel and pinned to one version in
`WoofWare.PawPrint.fsproj` and `WoofWare.PawPrint.Test.fsproj`. This skill is
about PawPrint's side of that boundary: the `Native/` handlers that answer a
guest's call by asking the kernel, the `*Pal.fs` adapters that translate between
the kernel's POSIX values and .NET's encodings, and `EmulatedKernel` with its
`KernelConfig`.

For anything inside the kernel — how a syscall family is laid out, which
Linux/Darwin divergence it models and which test owns each row, how to probe a
fact on both platforms — read `.claude/skills/emulated-posix-kernel/` in the
kernel's repository (its `reference/flavour-divergence.md` is the index of every
measured divergence). **Read its tests rather than re-measuring.**

## 1. Where does this fact live?

First ask whether it is the kernel's at all. The kernel's `README.md`, "What the
kernel leaves to the client", lists what it deliberately does not do — user
memory and `mmap`, blocks inside the process (`futex`, `__psynch`), choosing
which task runs, when time passes, the errno slot, running a signal handler,
`sigaltstack`, process creation — with why each is the client's and how the two
meet. Those are PawPrint's to do.

If it is the kernel's, there are four homes, and picking the wrong one is the
most common mistake in this area because three of them look alike from the call
site.

| home | the fact is… | examples |
| --- | --- | --- |
| `SimulatedUnixPlatform` (in the kernel) | true of *this kernel's source*, the same on every machine running it | the architecture and page size the image was built for, `sa_family_t` width, `AF_INET6`'s number, `sizeof(struct sockaddr_storage)`, `reportsBirthTime`, `creatingOpenRules` |
| `KernelConfig` (in `EmulatedKernel.fs`) | true of *this machine or mount or process*, and a different admin could change it | `Mount`, `UserAddressLimit`, `UserId`, `Umask`, `ProcessorCount`, `WallClockEpochMs`, `ProtectedFiles` |
| CoreLib flavour | not modelled at all — it decides which *guest* code path exists | `Environment.OSVersion`'s implementation, `Lock.ThreadId.InitializeForCurrentThread` |
| the interpreter | an artefact of how PawPrint represents memory, which no real kernel has | `Int32.MaxValue / stride` limits on a native block |

The test that separates the first two: **could two machines running the same
kernel image disagree?** A sysctl, a mount option, a uid — yes, so it is
configuration. `sizeof(struct sockaddr_un)` — no, so it is the platform, and a
missing or wrong platform fact is a change to the kernel (section 5).

`SystemNative_GetFileSystemType` is the worked example of configuration: the
value is a *mount* fact, so it lives in `KernelConfig.Mount`, even though the
flavour constrains which types are possible (`Tmpfs` is Linux-only, `Apfs`
Darwin-only).

`KernelConfig` is turned into the kernel's boot image and first process by
`KernelConfig.toKernel`. Three traps when adding a field:

- **`KernelConfig.Default` is a static value with `UnixPlatform` baked in.** A
  non-optional field whose sensible default depends on the platform gets one
  fixed default, so every `{ Default with UnixPlatform = macOsArm64 }` site
  silently keeps the other platform's value. Make it an `option` and resolve it
  in `toKernel`, usually by leaving it to the kernel setter's own
  flavour-derived default.
- **The platform is a constructor argument, never a setter's.** The kernel's
  `UnixSystem.initial` and `EmulatedKernel.image` take it, and nothing changes it
  afterwards, so a kernel setter that derives a default from the platform is not
  order-dependent. When one knob's answer is not well-formed without another's,
  resolve it where both are final: a current directory is an inode of *this*
  filesystem, so it is held as a path and resolved against the machine the
  process starts on.
- **How a process starts is the kernel's `ProcessLaunch`, not a boot-image
  setter.** A per-process knob goes on `ProcessLaunch` and a machine knob on
  `UnixBootImage`. The kernel chooses a later process's ID; only the first one's
  is configuration.

## 2. Per-thread state: field or map?

Stated on `ThreadState.Cpu`, and the criterion is **whether an absent key has a
truthful reading**:

- **Field on `ThreadState`** when it does not — `Cpu`, `OsThreadId`. There is no
  honest default processor index, and an arbitrary one aliases thread ids, which
  silently breaks `System.Threading.Lock`. As a field, the compiler asks every
  future thread-creation site which value it wants.
- **A map keyed by thread** when it does — the kernel's signal handler frames
  (no frame, so an empty mask), the last-error slots (errno 0).

The map form carries an obligation that is easy to miss: **remove the entry when
the value returns to the default.** `EmulatedKernel` is compared for equality to
decide whether a step changed anything, so a stored default is a state that
looks different while behaving identically. No guest can observe it; it corrodes
determinism. The kernel holds its own maps to this; a map PawPrint keeps beside
it must be held to it too, by a property test against a store-everything oracle:
reads must agree, **and** no default may ever be stored.

A test that needs a task to block signals calls the kernel's
`UnixSignal.pthreadSigmask`; putting it in a handler (`SignalFrames` in
`WoofWare.PawPrint.Test`) still works, and is what a test of frames wants.

## 3. Whose encoding is this?

The kernel is meant to be usable by a client that has never heard of .NET, so
**it states POSIX values and no client's encoding of them**: a raw `<errno.h>`
number, epoll's own readiness conditions, a signo, the set of sockets it will
create. A conversion between one of those and a .NET encoding goes on PawPrint's
side, in `WoofWare.PawPrint/Native/`, as a `*Pal.fs` adapter beside the twenty
that live there (count them with `ls WoofWare.PawPrint/Native/*Pal.fs`):

- errors and signals: `UnixErrorPal` (`Interop.Error`'s numbering),
  `PosixSignalPal` (the managed `PosixSignal` enum), `KillSignalPal`
  (`Process.Kill`'s `Interop.Sys.Signals`);
- sockets and readiness: `SocketEventsPal` (the `SocketEvents` bits),
  `SocketArgumentsPal` (the `AF_*`/`SOCK_*`/`PT_*` numbering and the shim's
  argument screens), `SocketShimPal` (compile-time constants the shim
  reports, such as its socket-address sizes), `SocketOptionPal` (the
  managed `SocketOptionLevel`/`SocketOptionName` pairs, what the shim does
  with each, and its `LingerOption`), `PollEventsPal` (the `PollEvents`
  bits), `SocketFlagsPal` (the `SocketFlags` the shim converts to
  `recv(2)`'s and `send(2)`'s flag word), `SocketShutdownPal` (the
  `SocketShutdown` values the shim converts to `shutdown(2)`'s `how`);
- files: `OpenFlagsPal`, `PipeFlagsPal`, `FileAdvicePal` (the shim's
  `OpenFlags`, `PipeFlags` and `FileAdvice` numberings and their screens),
  `FileStatusPal` (`st_flags` as the shim reports it), `FileSystemTypePal`
  (the shim's filesystem-type numbers), `DirectoryEntryPal` (the shim's
  `DirectoryEntry`), `NotifyEventsPal` (the shim's inotify event bits);
- the rest: `ClockPal` (which clock each timestamp entry point reads),
  `EnvironmentPal` (the PAL's view of the environment), `OsThreadIdPal` (the
  two widths the shim reports a thread ID in).

If a conversion seems to need the kernel to know a .NET number, the kernel is
missing a POSIX concept: ask for that concept (section 5), never for the
encoding. The kernel's repository rejects CoreCLR vocabulary in its own sources,
so the request would be refused there anyway.

**Read the prose when you move or add an adapter.** A declaration with a POSIX
name and a PAL-flavoured docstring reads as clean to every check; three
consecutive stages of the kernel's extraction found one (`SocketEventInterest`'s
fields named as the PAL's five bits, `SocketDomain` documented as "`AF_INET`,
PAL 2", `Signal.Other` claiming to carry "the raw managed `PosixSignal` enum
value", which it never could).

## 4. Refuse rather than invent — but check for an observer first

When the platforms disagree and PawPrint has not modelled the distinguishing
state, `failwith` naming the missing input beats returning a plausible constant:
a constant becomes a lie the moment the state it depends on lands. This is why
`SystemNative_FStat` refuses a socket — seventeen fields would be invented and
the platforms agree on none — while `SystemNative_GetFileSystemType` answers,
having one field measured on both.

The kernel refuses too, and PawPrint, a client without friend access, sees only
its public entry points' exceptions: `UnmodelledException` where the model does
not cover the call, and the BCL's own exception (`ArgumentException`,
`ArgumentOutOfRangeException`, `InvalidOperationException`,
`PlatformNotSupportedException`) where no client following the documentation
could have made it. PawPrint catches it at the call it guards, in a
`with :? UnmodelledException as e ->` arm (or the BCL exception's), and fails
through `KernelRefusal.fail` or `failAdvising` (`Native/KernelRefusal.fs`),
prefixing which entry point or `KernelConfig` knob asked, and what would lift
the refusal where that is PawPrint's to say. Keep that shape: a refusal a user
can act on names the knob they set.

Before concluding a modelled constant has **no guest observer**, enumerate the
interpreter's own limits that are *arithmetic in that constant*: a byte-offset
bound, a block-count cap, `Int32.MaxValue / stride`. Each is a boundary a guest
can bisect, and such a row belongs in `sourcesImpure` by construction, because a
real 64-bit libc succeeds at every count. Keep any machine-state unit test too:
it pins the value where the boundary pins only the ratio.

## 5. When the kernel lacks something

Only the kernel's public API is visible here; its `internal`s are not. When
PawPrint needs a syscall, a field or a fact the package does not provide:

1. Make the change in the kernel's repository, with its own tests (its skill
   says which tier), and get it released to NuGet.
2. Bump the version in **both** `WoofWare.PawPrint/WoofWare.PawPrint.fsproj` and
   `WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj`, and regenerate
   `nix/deps.json` with `nix build .#default.fetch-deps && ./result nix/deps.json`.
3. Wire it up here, in a PR of its own.

Until the release lands, the PawPrint handler refuses with a `failwith` naming
the missing kernel capability, rather than modelling the fact on PawPrint's
side: a second model of a kernel fact is a second version of the truth.

For a kernel-behaviour constant, the kernel's practice is to write the
measure-the-host test *first* and let it name the value, using source only to
learn the rule's **shape**. The release string a `SimulatedUnixPlatform` preset
names is what the guest reads from `uname`, not evidence about the kernel a test
runs on.

## 6. Which test tier can see it?

Summarised here; `reference/testing.md` has the detail. The kernel's own tiers
(its unit tests, its host-equality tests) live in its repository.

- **A host oracle of the shim** — for a fact measurable on the machine running
  the test, assert *equality* against the host and make the failure message
  report the measured value, so one CI run corrects a wrong answer rather than
  merely rejecting it. macOS locally and Linux in CI each falsify one column.
  `TestSocketCreation` and `TestStrErrorR` call the host's own
  `libSystem.Native` beside tables measured on both flavours
  (`socketMatrix/*.tsv`, the `strerror-r` files), and `TestFileStatusPal`
  reads the host's `struct stat` through `HostStat`; `HostPlatform` says which
  flavour the host is.
- **`sourcesPure`** — differential against real .NET on the *host*, so only
  claims that hold on both flavours. `ELOOP` agrees as an errno but not as a
  number (40 against 62).
- **`sourcesImpure`** — PawPrint only. Its value is that it sees the *wiring*: a
  unit test passes the rules in by hand, so a handler that hardcodes
  `Kernel.Umask` or `Kernel.UnixPlatform` instead of reading them survives every
  such test. Set both away from their defaults in the registration. Any fixture
  that runs a guest under a chosen `KernelConfig` catches this too —
  `TestPlatformSocketSupport` drives a raw-`DllImport` guest through
  `BoundedRun.run` under both platforms for exactly that reason — so the choice
  is between hand-fed unit tests and *some* guest-running fixture, not between
  unit tests and this directory.
