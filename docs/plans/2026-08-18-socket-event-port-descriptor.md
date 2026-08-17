# A socket event port as an open file description

PR 1 of the two-PR split agreed in `2026-08-18-wait-for-socket-events.md` §4 Option B. This
change makes an epoll/kqueue port *exist* as a descriptor: `SystemNative_CreateSocketEventPort`
hands one out, `SystemNative_CloseSocketEventPort` destroys it, and every other file operation
PawPrint implements gives the answer a real kernel gives for one.

`SystemNative_WaitForSocketEvents` is **not** in this change. It is PR 2, and it stays
loudly unimplemented here (`NativeCall.failUnimplemented` names the entry point), so nothing
can reach a port's readiness state — which is deliberate, since this change models no
readiness at all.

## Provenance

Every row below was measured with a C probe, not read off a source tree:
**Linux 6.18.5 aarch64** under Apple's `container` CLI, and **Darwin 25.6.0 arm64** natively.
Neither is exactly `LinuxX64`, so per `kernel-source-version-vs-measured-host` these fix the
*rule shape*; the one architecture-sensitive constant in the area (`sizeof(struct epoll_event)`)
does not appear in this change.

## What a port descriptor answers

`epoll_create1` and `kqueue` both return a descriptor onto an anonymous kernel object. The two
kernels then disagree about almost everything one can ask that object:

| operation | Linux | Darwin |
| --- | --- | --- |
| `read` (any length, 0 included) | `EINVAL` | `ENXIO` |
| `write` | `EINVAL` | `ENXIO` |
| `pread` / `pwrite` | `ESPIPE` | `ESPIPE` |
| `lseek`, whence 0–4, **any** offset | succeeds, returns `0` | `ESPIPE` |
| `lseek`, whence ≥ 5 | `EINVAL` | `ESPIPE` |
| `flock` (`LOCK_SH`, `LOCK_EX`, `LOCK_UN`) | succeeds | `ENOTSUP` (raw 45) |
| `ftruncate` | `EINVAL` | `EINVAL` |
| `isatty` | `0` | `0` |
| `dup` | succeeds | succeeds |
| `fstat` | succeeds; see below | succeeds; see below |

Exactly one row agrees. Two observations that shaped the implementation:

- **`lseek` is `noop_llseek` on Linux**, under the syscall's own `whence <= SEEK_MAX` guard.
  So it returns `0` for `SEEK_SET` with offset `-1` and with `INT64_MAX` alike, and for
  `SEEK_DATA`/`SEEK_HOLE` (3 and 4) too — the offset never moves and is never consulted.
  Only whence ≥ 5 is refused. A port therefore needs no offset field.
- **Darwin refuses `lseek` before it looks at whence**: `lseek(port, -1, 99)` is `ESPIPE`,
  not `EINVAL`. That is the *existing* Darwin ordering `SystemNative_LSeek` already models
  for pipes, so no new ordering logic is needed on that side.

Together those two mean seekability is **flavour-dependent for this target kind**, which is
new: today `DescriptorFault` is computed before the flavour branch, because a pipe is
unseekable on both. A port is unseekable on Darwin and seekable-but-inert on Linux, so the
classification itself has to move under the flavour.

### `fstat`, and why it refuses

Measured, `fstat` on a port succeeds on both, and the buffers share not one field:

| field | Linux | Darwin |
| --- | --- | --- |
| `st_mode` | `0600` — permission bits, **no file-type bits at all** | `010000` — `S_IFIFO`, **no permission bits** |
| `st_nlink` | `1` | `0` |
| `st_blksize` | `4096` | `32` |
| `st_dev` / `st_ino` | real anon-inode device, sequential inode | `0` / `0` |

This change refuses it, matching what `SystemNative_FStat` **already** does for the other
inode-free descriptor kind. Its standard-stream arm says: PawPrint holds no inode for one,
every field would be invented here, and no test could say the invention was wrong. That
reasoning applies verbatim to a port, and Linux's `st_dev`/`st_ino` make it sharper — they
are the anon-inode filesystem's device number and a sequentially-allocated inode number, i.e.
facts about the machine that produced them, not portable facts a replay could depend on.
Inventing them is exactly what a deterministic runtime must not do.

So the port arm is a sibling `failwith` naming the same decision, not a new policy. When
`fstat` on an inode-free descriptor is eventually answered, streams and ports want answering
together — and sockets will join them, since `S_IFSOCK` raises the identical question.

## Design decisions

### Identity: `OpenFileObject.AnonymousInode`, payload-free

`flock` succeeding on Linux forces the question, because contention is decided by
`OpenFileObject` equality.

My first answer was wrong, and measurement caught it only after review pointed at it. I
reasoned that an epoll instance is anonymous, stands 1:1 with its description, and should
therefore be identified by that description — so two ports would compare unequal and never
contend. Measured on Linux 6.18.5, that is false:

```
epoll A   fd=3  st_dev=13  st_ino=15
epoll B   fd=4  st_dev=13  st_ino=15
eventfd   fd=5  st_dev=13  st_ino=15
flock(A, LOCK_EX|LOCK_NB) = 0
flock(B, LOCK_EX|LOCK_NB) = -1  EWOULDBLOCK
flock(A, LOCK_UN)         = 0
flock(B, LOCK_EX|LOCK_NB) = 0
```

Every file on Linux's `anon_inodefs` shares a **single** inode — epoll instances and eventfds
alike — so they all contend with one another. A per-description identity would have made
PawPrint grant two exclusive locks where Linux grants one, which is guest-visible.

So the case is payload-free, and `OpenFileDescription.object` keeps its original one-argument
signature. The lesson is about what `OpenFileObject` *is*: its own docstring says `flock`
contention is decided on it, so it is the **contention key**, not a general-purpose identity.
Code that needs to tell two ports apart wants `OpenFileDescriptionId` — which is exactly what
`ThreadStatus.BlockedOnSocketEvents` already keys on, so nothing needed a second identity in
the first place.

Named `AnonymousInode` rather than `SocketEventPort` because the measured rule is about the
filesystem, not about epoll: an `eventfd` must join this case rather than get its own. A
socket must *not* — Linux puts those on `sockfs`, with an inode each.

### The target carries no state

`OpenFileTarget.SocketEventPort` has no payload. No offset (Linux's `lseek` never moves one,
Darwin refuses), and no interest list — registration is `SystemNative_TryChangeSocketEventRegistration`,
which is a later change. A port that can hold registrations but has no `WaitForSocketEvents`
to observe them would be state nothing reads.

### Access mode: `ReadWrite`

Not cosmetic. PawPrint's `Read` checks `FileAccessMode.permitsRead` *before* the target kind
and answers `EBADF` if it fails. Measured, `read(port, buf, 8)` is `EINVAL` on Linux and
`ENXIO` on Darwin — neither is `EBADF` — so the descriptor must permit reading, and by
symmetry writing. That matches the kernels: `anon_inode_getfd` opens `O_RDWR`.

## `flock` on Darwin refuses rather than reporting `ENOTSUP`

I planned to add a `UnixError.ENOTSUP` case and report Darwin's measured answer. That would
have been inconsistent with the code around it, so it is not what landed.

`SystemNative_FLock` already refuses the *whole* of Darwin's `flock` — its `refuseDarwin`
says PawPrint models Linux's and has not modelled Darwin's, because what is measured about
Darwin is the return code and not the lock state it leaves behind. A standard stream on
Darwin already hits that refusal for exactly this errno. Reporting `ENOTSUP` for a port alone
would model one row of Darwin's `flock` while the rest stayed refused, and the `UnixError`
case would then be reachable from precisely one arm.

So the port gets a sibling `refuseDarwin` arm naming the measured divergence, and no new
`UnixError` case is added. Linux's answer falls through to the registry unchanged, which is
what makes the shared-inode contention above work without any port-specific code.

## Sites touched

| File | Change |
| --- | --- |
| `FileDescriptorRegistry.fs` | `OpenFileTarget.SocketEventPort`, `OpenFileObject.SocketEventPort`, `object` gains the id parameter, `createSocketEventPort` |
| `Native/NativeSystemNative.fs` | two new handlers; a port arm at each existing exhaustive site (`Read`, `PRead`, `PWrite`, `Write`, `LSeek`, `FStat`, `FLock`) |

`Dup`, `Close` and `IsATty` need no arm: all three are target-agnostic already, and measurement
says a port behaves like every other descriptor under them.

`SystemNative_LSeek` is the only site whose *structure* changes rather than gaining an arm.
Its `descriptorFault` classification was flavour-independent, because a pipe is unseekable on
both platforms; a port is not, so the classification moves under the flavour, and the Linux
success path is served before the `SEEK_DATA`/`SEEK_HOLE` refusal (which is a statement about
a file's sparseness, and a port has none).

## Tests

- One guest per measured row, hand-rolled `DllImport`, reporting through exit codes rather
  than `Console` (a Console guest costs ~10 minutes under the interpreter against ~3 seconds).
- The flavour-divergent rows are pinned against both `SimulatedUnixPlatform` presets as data.
  A differential guest can only ever pin the *host's* column, so macOS runs pin Darwin and
  CI's Linux runs pin Linux — `host-flavour-equality-splits-verification`.
- Descriptor mechanics that need no flavour: a port takes the lowest free fd; `dup` of a port
  yields a second fd naming the same description; closing one leaves the other usable;
  closing the last frees the fd number for reuse.
- `lseek` needs the offset-irrelevance rows specifically (`-1` and `INT64_MAX` both returning
  `0`), because an implementation that fell through to the file arithmetic would pass a
  `SEEK_SET 0` test and fail these.
### Mutation results

Each production arm was flipped in turn, rebuilt, and the two guests re-run. Every mutant
died, and the check index it died at is the row that claims to cover it:

| Mutation | Killed at |
| --- | --- |
| `read`: Linux port answers ENXIO not EINVAL | Linux check 3 |
| `write`: Linux port answers ENXIO not EINVAL | Linux check 7 |
| `lseek`: port reports 1 rather than 0 | Linux check 18 |
| `lseek`: port is seekable under Darwin too | Darwin check 8 |
| port's `AccessMode` is `ReadOnly` | Linux check 7 *and* Darwin check 5 |
| `pwrite`: port answers EINVAL not ESPIPE | Linux check 13 |
| `pread`: port answers EINVAL not ESPIPE | Linux check 11 |
| `CloseSocketEventPort`: success reports EBADF | Linux check 44 |
| `CreateSocketEventPort`: null screen reports EBADF not EFAULT | Linux check 51 |

The `AccessMode` mutant killing *both* files is the interesting one: it confirms that
`ReadWrite` is doing work rather than being a default, since `ReadOnly` turns the
wrong-object-kind answer into `EBADF` on both platforms.

Three further rows were added after review. Two are about `CloseSocketEventPort`'s errno: the
PAL code is the *return value*, but `close(2)` still sets `errno` on the way past, so a caller
reading `Marshal.GetLastSystemError` must see `EBADF` after a failed close and its own prior
value after a successful one. The third is the shared-inode contention, whose mutant is the
first design this PR shipped:

| Mutation | Killed at |
| --- | --- |
| failed close leaves `LastSystemError` stale | Linux check 47 |
| successful close sets `LastSystemError` | Linux check 45 |
| two ports never contend (the per-description identity) | Linux check 33 |

### What no test here can catch

Description *freshness* for ports — two `createSocketEventPort` calls yielding one description
identity — has no guest observer. Their `flock` contention is now guest-visible, but that goes
through `OpenFileObject`, which deliberately collapses them; nothing a guest can call tells the
two *descriptions* apart. Pinned by unit tests in `TestFileDescriptorRegistry.fs` instead, and
it matters for PR 2 rather than here: `ThreadStatus.BlockedOnSocketEvents` keys a parked thread
on the port's `OpenFileDescriptionId`.

`CreateSocketEventPort`'s split between a **null** out-pointer and a **non-null but unbacked**
one is likewise unasserted. Null is `EFAULT` — the C wrapper's only screen — and is covered by
check 43. Anything else passes that screen, so the real code creates the descriptor and then
faults on its unconditional `*port = fd` store; PawPrint refuses loudly rather than inventing
an `EFAULT`, because answering one would turn a process-killing SIGSEGV into a value the guest
can catch and carry on from. No exit-code guest can assert an interpreter refusal, and
extracting a classifier to unit-test a two-line match (the shape `TestNativeHeapFree.fs` uses)
would be generality for one call site. Widening the arm back to `RawAddress _` is therefore a
silent regression, and this paragraph is what review should enforce.
