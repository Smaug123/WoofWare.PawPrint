---
name: emulated-posix-kernel
description: Deciding how the emulated kernel should answer a syscall — whether a fact belongs to the flavour, to configuration, or to the interpreter; where per-thread state lives; which test tier can observe the answer. Use when adding or changing anything in EmulatedKernel.fs, VirtualFileSystem.fs, FileDescriptorRegistry.fs, SignalState.fs, or a Native/ handler that reports kernel state. Carries measured Linux/Darwin divergence tables — consult them rather than re-measuring.
---

# Deciding what the emulated kernel says

PawPrint models a Unix kernel as *data*, never as a host read: a replay must not
depend on the machine that produced it. That constraint decides most of what
follows.

## 1. Where does this fact live?

Four homes, and picking the wrong one is the most common mistake in this area
because three of them look alike from the call site.

| home | the fact is… | examples |
| --- | --- | --- |
| `SimulatedUnixPlatform` | true of *this kernel's source*, the same on every machine running it | `sa_family_t` width, `AF_INET6`'s number, `sizeof(struct sockaddr_un)`, `reportsBirthTime`, `setIdBitsOnTruncation`, `creatingOpenRules` |
| `KernelConfig` | true of *this machine or mount or process*, and a different admin could change it | `FileSystemType`, `UserAddressLimit`, `UserId`, `Umask`, `ProcessorCount`, `WallClockEpochMs` |
| CoreLib flavour | not modelled at all — it decides which *guest* code path exists | `Environment.OSVersion`'s implementation, `Lock.ThreadId.InitializeForCurrentThread` |
| the interpreter | an artefact of how PawPrint represents memory, which no real kernel has | `Int32.MaxValue / stride` limits on a native block |

The test that separates the first two: **could two machines running the same
kernel image disagree?** A sysctl, a mount option, a uid — yes, so it is
configuration. `sizeof(struct sockaddr_un)` — no, so it is the platform.

Two entries currently in `SimulatedUnixPlatform` do not pass that test, and are
there as deliberate approximations rather than as precedent. `pathLimits` holds a
`NAME_MAX` that varies per mount on Linux, because PawPrint models exactly one
filesystem. `symlinkPermissions` holds Darwin's `umask 022` answer, because a
symlink can only enter this filesystem through a *seed* — a tree some other
process built, to which this run's configured umask never applied. Each has a
named trigger for becoming configuration, stated beside it: a second filesystem,
and `SystemNative_SymLink` letting a guest create a link. Do not cite either as a
reason to put a machine-dependent fact in the platform.
`SystemNative_GetFileSystemType` is the worked example: the value is a *mount*
fact, so it lives in `KernelConfig.FileSystemType`, even though the flavour
constrains which types are possible (`Tmpfs` is Linux-only, `Apfs` Darwin-only).

Two traps when adding a `KernelConfig` field:

- **`KernelConfig.Default` is a static value with `UnixPlatform` baked in.** A
  non-optional field whose sensible default depends on the platform gets one
  fixed default, so every `{ Default with UnixPlatform = macOsArm64 }` site
  silently keeps the other platform's value. Make it an `option` and resolve it
  in `applyTo`.
- **Do not write a setter that reads `kernel.UnixPlatform`.** That makes two
  `with` functions order-dependent. Take the platform as a parameter instead;
  `withUnixPlatformAndFileSystemType` fuses the pair for exactly this reason.

## 2. Per-thread state: field or map?

Stated on `ThreadState.Cpu`, and the criterion is **whether an absent key has a
truthful reading**:

- **Field on `ThreadState`** when it does not — `Cpu`, `OsThreadId`. There is no
  honest default processor index, and an arbitrary one aliases thread ids, which
  silently breaks `System.Threading.Lock`. As a field, the compiler asks every
  future thread-creation site which value it wants.
- **`Map<ThreadId, _>` on the kernel** when it does — `SignalState.Blocked`
  (empty mask), the last-error slots (errno 0).

The map form carries an obligation that is easy to miss: **remove the entry when
the value returns to the default.** `EmulatedKernel` is compared for equality to
decide whether a step changed anything, so a stored default is a state that looks
different while behaving identically. No guest can observe it; it corrodes
determinism. `SignalState.unblock` says so, and `TestSignalState.fs` /
`TestLastError.fs` property-test it against a store-everything oracle: reads must
agree, **and** no default may ever be stored.

## 3. Is this an identity, or a contention key?

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

## 4. Refuse rather than invent — but check for an observer first

When the platforms disagree and PawPrint has not modelled the distinguishing
state, `failwith` naming the missing input beats returning a plausible constant:
a constant becomes a lie the moment the state it depends on lands. This is why
`SystemNative_FStat` refuses a socket — seventeen fields would be invented and
the platforms agree on none — while `SystemNative_GetFileSystemType` answers,
having one field measured on both.

Before concluding a modelled constant has **no guest observer**, enumerate the
interpreter's own limits that are *arithmetic in that constant*: a byte-offset
bound, a block-count cap, `Int32.MaxValue / stride`. Each is a boundary a guest
can bisect, and such a row belongs in `sourcesImpure` by construction, because a
real 64-bit libc succeeds at every count. Keep any machine-state unit test too:
it pins the value where the boundary pins only the ratio.

## 5. Establish the fact by measuring, not by reading source

For any kernel-behaviour constant, write the measure-the-host test *first* and
let it name the value. Use source only to learn the rule's **shape** — is this a
pointer test or a range test? — which is far more stable across versions than the
constant.

`SimulatedUnixPlatform.linuxX64` names a release string, and that is what the
guest reads from `uname`; it is **not** evidence about the kernel a test runs on.
Reading `access_ok` at the preset's version gave a sign-bit split where CI
measured `TASK_SIZE_MAX`, because x86 changed the rule between 6.9 and 6.12.

See `reference/probing.md` for the probe technique, including the two ways a
set-ID measurement reads as "unsupported" when it is not.

## 6. Which test tier can see it?

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
