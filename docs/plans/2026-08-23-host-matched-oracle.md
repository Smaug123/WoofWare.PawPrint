# A differential oracle for platform-specific end-to-end cases

## The gap

`sourcesPure` cases are differential: PawPrint's answer is compared against the same
guest run on real .NET. `sourcesImpure` cases have no oracle at all — the harness only
checks that PawPrint's exit code equals a number written down in `TestImpureCases.fs`.

Thirty-seven impure cases are impure *only because they are platform-specific*
(`SocketPollLinux.cs`, `SocketAcceptDarwin.cs`, …). They are morally pure: each one is a
guest that self-checks and returns 0, and each one's claims are checkable against the
real kernel — but only on a host running that kernel. On a macOS dev box, real .NET
cannot be asked what Linux does, so the whole class was excluded from the oracle.

The cost of that exclusion is concrete. `SocketPollLinux.cs`'s ~64 checks were verified
against real Linux exactly once, by hand, in a container. Nothing re-runs that check, so
the file's claim about what Linux does is only as good as the day it was written.

CI is Linux. On CI, every `*Linux.cs` case *could* be run against the real kernel it
describes, for free.

## Options considered

**(a) A new pair of source directories, `sourcesPureLinux` / `sourcesPureDarwin`**, with
auto-registration by directory name like `sourcesPure`. Rejected: auto-registration
means every file in the directory opts into the oracle, and that is wrong for several of
the existing platform-specific cases — `GetFileSystemTypeLinux.cs` asserts an *emulated*
filesystem type that a real Linux host will not report, and the readdir-ordering cases
depend on ext4 features a container's filesystem may not have. It also moves 37 files
and duplicates the pure harness's machinery.

**(b) A filename-keyed side set in `TestImpureCases`**, like `unimplemented` and
`customExitCodes` in `TestPureCases`. Cheapest diff, but stringly-typed and able to go
stale: a name in the set that no longer names a case is silently inert. Those existing
sets are keyed by filename because pure cases are *discovered* by resource name and have
no other key; impure cases are explicit record literals, so a field is available.

**(c) A field on `EndToEndTestCase` saying when the case has an oracle.** Chosen. The
question "is this case's answer checkable against a real kernel, and on which host?" is
a property of the case, not of the harness that happens to run it, and the compiler then
requires an answer for every case. The cost is one line in each of ~100 existing record
literals; that is a one-off mechanical edit and it cannot rot.

## The policy

```fsharp
type OraclePolicy =
    | Always
    | WhenHostMatchesEmulatedFlavour
    | Never
```

`Always` is what every `sourcesPure` case has today: compare on any host. That is only
sound for claims which hold under every flavour, which is the existing rule for that
directory.

`WhenHostMatchesEmulatedFlavour` compares only when the host's own kernel flavour is the
one `case.KernelConfig.UnixPlatform` emulates. The flavour is read off the case rather
than written down twice, so the two cannot disagree. On any other host the case falls
back to exactly what it does today: assert PawPrint's exit code against
`ExpectedReturnCode`.

`Never` is everything else in `sourcesImpure`.

### What the host-matched comparison does *not* establish

Matching flavours is necessary, not sufficient. A case must not opt in if its guest can
observe any of these, because the emulated kernel and the host kernel disagree about all
of them by construction:

- the kernel release string (`6.17.0-1022-azure` is not the CI runner's release);
- the processor count, the clock, or the seeded PRNG;
- the filesystem type under a path, or directory enumeration order;
- uid/gid and the permission consequences of running as root in a container;
- the architecture — the flavour presets are named `linuxX64` and `macOsArm64`, but only
  the flavour is consumed, so an arm64 Linux host matches `linuxX64`.

Opting a case in is therefore a claim about that guest, and each opt-in says why.

## What opted in, and what did not

Twenty platform-specific socket cases were flipped on and *measured* — the Darwin half
by running the suite on macOS, the Linux half by building each guest and running it on
real .NET in a Linux container. Nine survived:

| compared | |
| --- | --- |
| `SocketAcceptLinux` / `SocketAcceptDarwin` | errnos and returned addresses only |
| `SocketConnectLinux` / `SocketConnectDarwin` | loopback outcomes and errnos |
| `SocketAddressLinuxBytes` / `SocketAddressDarwinBytes` | the shim's own sockaddr layout |
| `SocketPollLinux` | poll's `triggered` counts, not descriptor numbers |
| `SocketEventDeliveryLinux` | registered user data, not descriptor numbers |

Twelve did not, and each exclusion is a measurement or a stated hazard rather than a
guess:

- **Descriptor numbers.** `SocketCreate*`, `SocketEventPort*` and `SocketEventsWait*`
  assert that the first socket is fd 3. The emulated fd table starts at 3 with nothing
  else open; a real process has opened files before `Main` runs. Measured on macOS:
  these three answer 5, 1 and 2 respectively, each at its first descriptor-number row.
- **The caller's uid.** `SocketBind*` asserts `EACCES` binding port 80, which is the
  answer for a non-root uid only — as the guest's own comment already said. Measured: it
  answers 34 as root in a container and 0 as `nobody`.
- **PawPrint's own int32 block offset.** `SocketEventBuffer*` measures the element count
  at which a request stops being representable, which the file's header already records
  as unreachable for any real libc, because a real 64-bit `malloc` succeeds at both
  counts by overcommit. Measured on macOS: 4.
- **A sleep that has to win a scheduling race.**
  `SocketEventWaitSurvivesCloseLinux` starts a waiter thread which sets a flag and
  *then* enters `epoll_wait`; the main thread sleeps 100ms and closes the descriptor the
  waiter was handed. A waiter descheduled across that gap enters the wait after the
  close, by which point Linux has handed the freed descriptor to the next socket, and
  the guest exits 13. Under PawPrint the sleep yields to the waiter deterministically,
  so the divergence would be a scheduling accident wearing an interpreter bug's clothes.
  This is not the same as the sleeps in `SocketConnect*`, which wait for a loopback
  handshake or RST the kernel settles in softirq context: there a late wake only makes
  the state more settled.
- **What fd 0 is.** `SocketEventRegistrationLinux` registers stdin with the port and
  expects success, which holds because PawPrint models the standard streams as pipes; a
  real process's stdin is whatever its parent handed it, and `epoll_ctl` refuses a
  `/dev/null` with EPERM.

One measurement is worth keeping for its own sake. `epoll_ctl(EPOLL_CTL_ADD)` on a
**regular file** answers EPERM on ext4, tmpfs and overlayfs — but *succeeds* on a
virtiofs bind mount, whose files carry a `->poll`. The first run of this probe put its
scratch file in the container's bind-mounted host directory and so measured the one
filesystem that disagrees, which read as a PawPrint divergence and was not one.

## The host's shape, not just its flavour

`SimulatedUnixPlatform` carries a flavour and a kernel release, and no architecture; the
two presets are *named* `linuxX64` and `macOsArm64` but only their flavour is ever read.
Matching flavours is therefore not enough on its own: the compared guests read
native-width layouts back as bytes -- a `sockaddr_in`'s fields, the 16-byte
`SocketEvent` -- so a 32-bit or big-endian Linux would disagree with PawPrint for a
reason that is not PawPrint's, and the failure would read as an interpreter bug.

`OraclePolicy.comparesHere` therefore treats a host whose shape the presets do not
describe as no host at all. `Always` is deliberately left alone by this:
`sourcesPure`'s rule that its claims hold on every host is not this policy's to narrow.

## Not attempted here

The filesystem-seeded platform pairs — `EnumerateWiring*`, `MkDirWiring*`, `RmDirOrphan*`,
`RmDirWiring*`, `TruncateWiring*`, `UnlinkWiring*`, `Write*`, `GetFileSystemType*` — are
left `Never`. `GetFileSystemType*` asserts an emulated filesystem type outright, and the
rest are exposed to directory enumeration order, to the mode bits a real host will give a
seeded file, and to the uid the suite runs as. They are a separate measurement from this
one, and each would want the same treatment: opt in, run on the real kernel, and keep
only what agrees.

## One residual risk, stated

Every compared guest binds an ephemeral port (`SetPort(…, 0)`) and reads the assigned
port back, so nothing here can collide with another process on a busy runner.
`SocketEventDeliveryLinux` and `SocketPollLinux` provoke a refused connection by binding
a port, closing it, and connecting to it; on the oracle side, that is refused unless the
kernel hands the same ephemeral port to somebody else in the microseconds in between.
Linux allocates from a ~28,000-port range with a randomised offset and a sequential scan,
so reuse inside that window would require the allocator to wrap. It is not zero, and it
is worth remembering if either case ever fails on CI and passes on a re-run.
