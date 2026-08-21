# `SystemNative_FcntlSetIsNonBlocking` / `SystemNative_FcntlGetIsNonBlocking`

## Where this sits in the sockets plan

Re-measured on main at `456cf540` (Bind/Listen/GetSockName merged): rung D of the
ASP.NET ladder now passes end-to-end (exit 42), and rung I — the traffic rung —
stops at `SystemNative_FcntlSetIsNonBlocking`, reached nine frames under
`Socket.AcceptAsync` when `SocketAsyncContext` switches the listener to
non-blocking before registering it with the event port. This is the last entry
point before `SystemNative_Accept`, where the readiness architecture starts.

The two entry points are one feature: the `O_NONBLOCK` status flag, its setter
and its getter. The getter is included not for the BCL's sake but because it is
the *observer*: without it no guest can see the stored flag at all, and the
whole slice would be assertable only by reaching into the registry from F#.
With it, a differential guest can pin the round-trip against real .NET.

Upstream (`pal_io.c:656` and `:677`):

* `SetIsNonBlocking(fd, v)` is `fcntl(F_GETFL)`, toggle `O_NONBLOCK`,
  `fcntl(F_SETFL)`; returns 0, or -1-and-errno. Any nonzero `v` sets.
* `GetIsNonBlocking(fd, out)` answers `Error_EFAULT` — the PAL enum value, from
  a function whose other answers are 0/-1-and-errno — for a NULL pointer;
  stores 0 through the pointer and returns -1-and-errno when `fcntl` fails;
  otherwise stores the flag and returns 0.
* The shim's own `Accept` (`pal_networking.c:1739`) *clears* the flag on every
  accepted socket, so the future Accept slice consumes this one.

## Where the flag lives

Options considered:

1. **On `OpenFileDescription`.** POSIX puts the status flags on the open file
   description: a `dup(2)` pair shares them, `F_SETFL` through one fd is
   visible through the other. This is the true home.
2. **On the socket state in `EmulatedKernel.Sockets`.** Beside `Binding`, where
   the future Accept/Receive handlers will look. Rejected: it is a projection —
   the flag belongs to the *description*, and storing it per-socket builds in
   the assumption that a socket has exactly one description, which `dup` on a
   socket fd already falsifies.

Chosen: option 1, a `NonBlocking : bool` field. This falsifies the type's
existing "status flags are absent because no modelled syscall can change them"
docstring, which is rewritten (O_APPEND remains absent, and remains true).

## Stored state must not outrun its readers

A flag that is stored but never consulted is a lie in waiting: a guest sets it,
a later transfer blocks anyway, and the divergence is silent. Per target kind:

* **Socket** — record. No socket transfer syscall exists yet, so no modelled
  operation can ignore the flag today; the very next slice (`Accept`) is its
  first consumer, and every subsequent transfer handler must consult it.
* **File** — record. Both kernels give `O_NONBLOCK` no effect on regular-file
  reads and writes, so a reader that never consults the flag is *correct*, not
  negligent.
* **StandardStream** — refuse to set, loudly. PawPrint models the streams as
  pipes, and a real pipe honours `O_NONBLOCK` (an empty read becomes EAGAIN),
  but no modelled stream transfer consults the flag. Recording it would keep
  blocking semantics silently. Crash names the missing work instead.
* **SocketEventPort** — refuse to set, loudly. Nothing modelled consults it,
  the BCL never sets it on the port, and what `F_SETFL` even does to an epoll
  or kqueue descriptor has not been measured here.

*Clearing* the flag is recorded for every target kind: `false` cannot be a lie,
because the refusal above means no refused target can ever be `true`. This also
matches the shim's own `Accept`, which clears unconditionally.

The refusal is enforced twice: at the handler (user-facing, names the gap) and
as a backstop inside `FileDescriptorRegistry.setNonBlocking` (interpreter-bug
`failwith`, the same shape as `setOffset`'s target screens), so a future caller
cannot store an unhonoured `true` by skipping the screen.

`GetIsNonBlocking` reads for every target kind: `false` is the truth for the
targets that can never be set.

## Tests

* `TestFileDescriptorRegistry.fs`: a fresh socket/file description starts
  blocking; set/clear round-trips; a `dup` pair shares the flag both ways;
  `close` of one half leaves the other's flag; the stream/port backstops fire.
* `sourcesPure/FcntlNonBlocking.cs`: raw-P/Invoke differential guest — create a
  socket, get (expect blocking), set, get, set with a nonzero-non-one value,
  clear, get; NULL-pointer row pinning the `Error_EFAULT` oddity; EBADF row on
  a closed fd (return codes only — no errno reads, which raw `DllImport`
  guests cannot do faithfully under PawPrint).
* Re-run rung I and record the next frontier below.

## Measured after landing

Rung I now stops at `SystemNative_Accept`, still nine frames under
`AcceptAsync`, with the `SocketAsyncEngine` thread parked in
`WaitForSocketEvents` as before. That is where the readiness architecture
starts: `Accept` answering `EAGAIN` on an empty backlog is what drives
`TryChangeSocketEventRegistration` and the wake path.
