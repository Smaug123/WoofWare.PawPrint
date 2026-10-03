# What a descriptor kind answers

As with the flavour tables, the rows themselves live in tests. This file says
which test, and records the handful of things the tests cannot say because they
are facts about *upstream source being misleading*.

| descriptor kind | owned by |
| --- | --- |
| epoll instance (`epoll_create1`) | `TestLSeek` for the whence rows; `TestFileDescriptorRegistry`: `two socket event ports are two descriptions but one flock object`; `TestEpollCreate1`; `sourcesImpure/SocketEventPortLinux.cs` and `sourcesImpure/SocketEventsWaitLinux.cs` for the guest-visible answers |
| kqueue (`kqueue`) | `TestKqueue`; `sourcesImpure/SocketEventPortDarwin.cs`, `sourcesImpure/SocketEventsWaitDarwin.cs` and `sourcesImpure/KqueueWaitDarwin.cs` for the guest-visible answers |
| socket | `TestFileDescriptorRegistry`: `two sockets are two descriptions and two flock objects`, `dup of a socket names the same socket`; `sourcesImpure/SocketCreate{Linux,Darwin}.cs`; `read`/`write` with no peer in `TestUnconnectedSocketTransfer` and `sourcesImpure/SocketUnconnectedTransfer.cs` |
| the creatable socket triples | `TestSocketCreation` against `socketMatrix/{linux,darwin}.tsv` |
| standard streams, regular files, directories | `TestVirtualFileSystem` and `TestVirtualFileSystemAgainstHost` |
| pipe (`pipe2`) | `TestPipe` and `TestPipeAgainstHost`; `sourcesImpure/PipeRaw.cs` for the wiring |
| `/dev/null`, `/dev/urandom` (Linux) | `TestDeviceDescriptors` and `TestDeviceDescriptorsAgainstHost` |

## Three things reading the kernel source gets wrong

These are why the rows above were measured rather than derived, and none of them
is visible in a test's name.

1. **`lseek` on an epoll descriptor is `noop_llseek`, not `ESPIPE`.** It
   succeeds and reports 0 — for `SEEK_SET` with `-1` and with `INT64_MAX` alike.
   Darwin refuses with `ESPIPE`, so this is a divergence and not a shared
   no-op.
2. **`do_epoll_wait`'s widely-reproduced check order is stale.** The order
   commonly quoted is maxevents → `access_ok` → `fdget`; measured on 6.18.5 it is
   **EBADF → EINVAL (maxevents) → EFAULT (buffer) → EINVAL (not an epoll fd)**.
3. **`access_ok` rejects only kernel-range addresses.** A userspace-but-unmapped
   buffer passes it, and `epoll_wait` then blocks rather than faulting — so do
   not eagerly validate a buffer before parking. `UserBufferCheck.BeforeOperation`
   models this, and `TestUserBufferCheckAgainstHost` holds it to the host.

## Two scoping rules for adding a kind

**Blocking mode changes the answer, so state it.** A row measured under
`O_NONBLOCK` says nothing about the same call on a blocking descriptor, which is
what `socket(2)` and `epoll_create1` actually hand back. `read` on a fresh
unconnected *datagram* socket is the sharp case: `EAGAIN` with the flag set, and
a block with no wake source without it. So `UnconnectedSocketRules.read` takes
the flag, and the kernel answers the first and refuses the second
(`ReadRefusal.DatagramSleep`): nothing in it sends a datagram, and a sleep only a
signal could end is not modelled.

**A measurement is about the creator named, not its filesystem as a class.**
Epoll descriptors and an `eventfd` share one `anon_inodefs` inode, which is why
`OpenFileObject.AnonymousInode` is payload-free and two ports contend under
`flock`. Linux hands a *distinct* inode to files made through
`anon_inode_getfile_secure` and `anon_inode_create_getfile`, so a new descriptor
kind on that filesystem needs its own measurement rather than this verdict.
