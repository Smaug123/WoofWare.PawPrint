# WoofWare.PosixKernel

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="logos/dark.svg">
  <source media="(prefers-color-scheme: light)" srcset="logos/light.svg">
  <img alt="Project logo: minimalistic face of a cartoon Shiba Inu, drawn in outline, sitting at the centre of three concentric rings." src="logos/light.svg" width="300">
</picture>

A deterministic, purely functional simulation of a POSIX process.

This library models what a Unix kernel tells a process about the world.
It includes:

* the filesystem, including permissions
* the file-descriptor table
* sockets and connections
* signals
* clock
* entropy

There are two tested flavours of Unix kernel: Darwin (tested on aarch64), and Linux (tested primarily on x64 but also sometimes on aarch64 when I remember).

The model of interaction is that the simulated kernel is a state machine, with transitions being pure functions from state to state.

WoofWare.PosixKernel performs no I/O and makes no host reads: all state is simulated.
Two runs from the same starting state see the same world, on any machine, in any order.

## History

This was developed incidentally during the construction of WoofWare.PawPrint,
which (being a deterministic simulation of a .NET runtime) must manufacture the results of e.g. the `open` syscall.
I expect it may be of independent interest, so it is now extracted as a standalone component which knows nothing about the CLR.
(Clients translate their own foreign-function layer into requests against the state machine, and WoofWare.PosixKernel doesn't call back into the client.)

## Status

Nothing here converts to or from the encodings designed for WoofWare.PawPrint's CoreCLR platform abstraction any more.
All four clusters have gone the other side of the boundary: errno numbering, the socket-event bits, the address-family and
socket-type numbering, and the managed `PosixSignal` enum. This library states a raw `<errno.h>` number, epoll's own
readiness conditions, the set of sockets it will create, and a signo, and each client encodes those as it pleases.
A flake check (`scripts/check-pal-residue.py`) keeps it that way.

The simulation is incomplete and expected to change wildly during development.
The syscall request/response layer has started: `UnixSystem` exposes nine syscalls
(`geteuid`, `dup`, `lseek`, `flock`, `ftruncate`, `close`, `mkdir`, `unlink`, `rmdir`)
both individually and through a `step` dispatcher.
Everything else is still reached through the state modules directly.
There is also no constructor for a fresh `UnixSystem` yet, so a client must assemble one field by field.

A syscall that would block does not block: `step` answers `SyscallOutcome.WouldBlock` carrying a
`WakeCondition`, and the client decides what to do with the calling task.
`WakeCondition.isSatisfied` answers whether the condition holds yet, so a client's scheduler can poll it;
the state that comes back with a `WouldBlock` is the state after whatever the call did before sleeping,
which is why it is returned rather than discarded.
This library has no scheduler and does not want one.

### Slop status

100% vibe-coded, by the hand of Claude Opus 4.6 through 5, Claude Fable 5, and GPT-5.5 through 5.6 Sol.

## Conformance to POSIX, and divergence from host platforms

WoofWare.PosixKernel's behaviour does not depend on the host platform - indeed, it probably works on Windows.
However, POSIX is extremely underspecified (and implementations frequently diverge from their documentation!),
and I only have easy access to a few flavours.

WoofWare.PosixKernel is intended to be fully POSIX-compliant eventually.
A design goal of WoofWare.PosixKernel is that the library throws rather than providing an answer which has not been measured on the platform it's been told to simulate.
It is fully deterministic, so e.g. it chooses a traversal order for directory listing even though that is POSIX-unspecified.

We supply different flavours of kernel (currently Linux and Darwin) and filesystem (currently at least tmpfs, APFS, and NFS);
we try very hard to return only values that we have observed from a real system using that platform, rather than just copying semantics from the docs.

Where real platforms of a given flavour have been observed to disagree, we choose a permitted answer (generally an *inconvenient* one, since I want to help you avoid accidentally relying on unspecified behaviour).
For example, directory enumeration order has been observed to be extremely odd on Linux: we've even seen `..` and `.` appear at the *end* of the enumeration, in the GitHub Actions ext4 runner!

## Licence

MIT.
