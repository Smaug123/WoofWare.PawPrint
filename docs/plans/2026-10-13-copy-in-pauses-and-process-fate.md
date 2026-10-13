# One way to pause for user memory, and one way to end the process

## Goal

Two reshapings of `WoofWare.PosixKernel`'s public surface before version 1,
each typed, each useful without any trace or replay machinery:

1. Every syscall that stops partway through to read the caller's memory does
   so in the same way: it answers a `CallProgress`, whose paused value holds
   the system and everything the call has decided, and a resume function
   takes the bytes and finishes the call.
2. Every syscall that can end the process answers a `ProcessFate`: the call's
   own outcome and the system it left, or the ended process.

Both are breaking, so they must land before 1.0 or wait for 2.0. Neither
changes what any call answers. This is the groundwork a recorder of kernel
inputs would sit on (see "Not in scope").

## 1. What exists

On main (`55e2622d`) a call pauses for the caller's memory in one of three
ways.

- **A paused value carrying the system.** `link` and `linkat`
  (`linkSourcePhase` → `LinkProgress.NeedsDestination of PausedLink` →
  `linkWithDestination`), `rename` and `renameat`, `symlink` and `symlinkat`,
  `clonefile` (two pauses: flags, then source, then destination), `access`
  and `faccessat`, `inotify_add_watch`. Each has its own progress union, and
  they disagree about whether `Answered` carries the system: `RenameProgress`
  and the clonefile ones do, `LinkProgress`, `SymlinkProgress`,
  `AccessProgress` and `InotifyAddWatchProgress` do not. Beside each sits a
  public one-shot form taking every path up front (`link`, `rename`, …).
- **An admission, then a second call against the system the admission
  returned.** `admitWrite` → `write` or `writeThenSleep`; `admitSend` →
  `send` or `sendThenSleep`; `admitPWrite` → `pwrite`; `admitFinishWrite` →
  `finishWrite`. The second call re-derives the target from the descriptor,
  and its docstring requires "the system the one it came with" and bytes of
  the admitted length; a client that gets either wrong fails loudly at run
  time, or not at all for the system.
- **A screen that changes nothing, then an unrelated call.**
  `admitSockaddrCopy` → `bind` or `connect`; `admitSetSockOpt` →
  `setsockopt`; `admitGetSockOpt` → `getsockopt`. The second call re-runs the
  screen and checks the supplied length against it.

All three are the same event: the kernel reaches the point where it copies
from the caller's memory, the client produces the bytes, and the kernel
carries on. In this model nothing else happens in between: the admission's
system is the one the transfer runs against.

A call that can end the process says so in four different types:
`WriteOutcome` (`Returns`, `ReturnsRaising`, `WouldBlock`, `Restarts`, each
carrying a system, and `ProcessEnded`), `KillOutcome`, `ReturnToUserOutcome`
and `TaskOutcome`.

## 2. The two shapes

```fsharp
/// How far a call got: over, or stopped where the kernel copies from the
/// caller's memory.
type CallProgress<'Paused, 'Answer> =
    | Answered of answer : 'Answer
    | NeedsCopyIn of paused : 'Paused

/// What a call that can end the process did to it.
type ProcessFate<'Outcome, 'Task, 'Handler> =
    | Continues of outcome : 'Outcome * system : UnixSystem<'Task, 'Handler>
    | ProcessEnded of EndedProcess<'Task, 'Handler>
```

The laws, which every pausing call keeps:

- **A call answers `CallProgress<P, A>`, and resuming a `P` answers `A`**, or,
  where another copy follows (clonefile), `CallProgress<P', A>`. `Answered`
  carries the same `A` that resuming does, so a call answered before the copy
  and one answered after it are handled by the same code. Where `A` includes a
  system today on one side and not the other, it includes it on both.
- **The paused type says what to copy.** It is opaque and holds the system, so
  no other system can be passed back with it. A paused call that copies bytes
  exposes `copyRange : CopyRange` (an offset into the buffer the call was made
  with, and a count); one that copies a path needs no accessor, since the
  type names the path argument; `getsockopt`'s reads the length cell, a
  `socklen_t`, which the client passes as the `uint32` it holds.
- **A resume checks only what the type cannot**: that the supplied bytes are
  exactly `copyRange.Count` long.

There is deliberately no `CopyInRequest` union. Which argument to read is
determined by the paused type, and a union would restate the type as a value
that could disagree with it.

A paused value is a value: resuming it twice resumes the same call from the
same system twice, which is pure and harmless.

## 3. Per call

Every pausing call takes the syscall's own name and answers `CallProgress`;
each resume is named `<call>With<Operand>`.

| call | starts with | paused type | copies | resumed with | answers |
| --- | --- | --- | --- | --- | --- |
| `write(2)` | `UnixReadWrite.write task fd buffer count` | `PausedWrite` | `copyRange` | `writeWithBytes` | `ProcessFate<WriteOutcome>` |
| `send(2)` | `UnixReadWrite.send task fd buffer count flags` | `PausedWrite` | `copyRange` | `writeWithBytes` | `ProcessFate<WriteOutcome>` |
| a sleeping `write` or `send`, woken | `UnixReadWrite.finishWrite task` | `PausedWrite` | `copyRange` | `writeWithBytes` | `ProcessFate<WriteOutcome>` |
| `pwrite(2)` | `UnixReadWrite.pwrite task fd buffer count offset` | `PausedPWrite` | `copyRange` | `pwriteWithBytes` | `WriteAnswer * UnixSystem` |
| `bind(2)` | `UnixSocket.bind fd address declaredLength` | `PausedBind` | `copyRange` | `bindWithAddress` | `BindAnswer * UnixSystem` |
| `connect(2)` | `UnixConnection.connect fd address declaredLength` | `PausedConnect` | `copyRange` | `connectWithAddress` | `ConnectOutcome * UnixSystem` |
| `setsockopt(2)` | `UnixSocket.setsockopt fd level name value optionLength` | `PausedSetSockOpt` | `copyRange` | `setsockoptWithValue` | `SetSockOptAnswer * UnixSystem` |
| `getsockopt(2)` | `UnixSocket.getsockopt fd level name value length` | `PausedGetSockOpt` | the length cell | `getsockoptWithLength` | `GetSockOptAnswer * UnixSystem` |
| `link(2)`, `linkat(2)` | `UnixNamespace.link oldpath`, `linkat olddirfd oldpath newdirfd flags` | `PausedLink` | `newpath` | `linkWithDestination` | `SyscallAnswer * UnixSystem` |
| `rename(2)`, `renameat(2)` | `UnixNamespace.rename source`, `renameat olddirfd oldpath newdirfd` | `PausedRename` | the destination | `renameWithDestination` | `SyscallAnswer * UnixSystem` |
| `symlink(2)`, `symlinkat(2)` | `UnixNamespace.symlink target`, `symlinkat target dirfd` | `PausedSymlink` | the link's path | `symlinkWithPath` | `SyscallAnswer * UnixSystem` |
| `clonefile(2)` | `UnixNamespace.cloneFile flags` | `PausedCloneFileSource`, then `PausedCloneFile` | the source, then the destination | `cloneFileWithSource`, then `cloneFileWithDestination` | `SyscallAnswer * UnixSystem` |
| `access(2)`, `faccessat(2)` | `UnixPathResolution.access mode`, `faccessat dirfd mode flags` | `PausedAccess` | the path | `accessWithPath` | `SyscallAnswer` |
| `inotify_add_watch(2)` | `UnixInotify.inotifyAddWatch fd mask` | `PausedInotifyAddWatch` | the path | `inotifyAddWatchWithPath` | `SyscallAnswer * UnixSystem` |

Notes:

- `send`'s pause is a write's: the paused value remembers which call it was,
  and `writeThenSleep`, `sendThenSleep` and `admitFinishWrite` disappear into
  `writeWithBytes`. A write that takes part of its bytes and sleeps for the
  rest is a `WriteOutcome.WouldBlock` from `writeWithBytes`.
- `setsockopt` and `getsockopt` that copy nothing (Linux's null
  `IPV6_V6ONLY` value; Darwin's null value buffer, whose length cell it
  never reads) answer at once rather than asking for nothing.
- `access` changes nothing, so it carries no system on either side, as
  today.
- The public one-shot forms of the path calls (`link oldpath newpath` and so
  on) go, so each call has one public entry. The eager form, for a client
  that has every argument up front, is `UnixSystem.step`'s `Syscall`, which
  keeps using the internal one-shot functions.
- The internal step functions (`tryAdmitWrite`, `tryWrite`,
  `tryAdmitSockaddrCopy`, `tryBind`, …) stay as they are and become the
  implementation of the pauses. The test suite probes each step through them,
  and keeps doing so. `WriteAdmission`, `WriteResumption`, `PWriteAdmission`,
  `SockaddrCopyAdmission`, `SetSockOptAdmission` and `GetSockOptAdmission`
  become internal. The write family's internal generic outcome, which carries
  those admissions as its answer, is renamed `WriteStep` and made internal;
  each public write converts it to `ProcessFate<WriteOutcome>` in one place.

## 4. Outcomes that can end the process

| call | today | after |
| --- | --- | --- |
| `write`, `send`, `writeWithBytes` | `WriteOutcome<WriteAnswer, 'Task, 'Handler>` | `ProcessFate<WriteOutcome<'Task>>` |
| `kill`, `pthreadKill` | `Result<KillOutcome, UnixError>` | `Result<ProcessFate<KillOutcome>, UnixError>` |
| `onReturnToUser` | `ReturnToUserOutcome` | `ProcessFate<ReturnToUserOutcome>` |
| `exitThread` | `TaskOutcome` | `ProcessFate<unit>` |

Each outcome type loses its `ProcessEnded` case and the system in its other
cases:

- `WriteOutcome<'Task>`: `Returns of WriteAnswer`, `ReturnsRaising of
  WriteAnswer * PendingSignal`, `WouldBlock of WakeCondition`, `Restarts`.
- `KillOutcome`: `ProcessRuns`, `ProcessStopped of Signal`. (`ProcessContinues`
  is renamed, since `ProcessFate.Continues` now says that the process
  carries on; a stopped one carries on too.)
- `ReturnToUserOutcome<'Task, 'Handler>`: `Resumes`, `RunHandlers of frames`,
  `ProcessStopped of Signal`, `ContinueDiscarded of Signal`.
- `TaskOutcome` goes: it was `ProcessFate<unit>` already.

`exitGroup` still answers an `EndedProcess`, since it always ends the process.
`kill`'s `Error` for a failed call stays outside the fate, as today.

## 5. Order of work

Each step builds and passes the kernel's tests before the next.

1. `ProcessFate`, and `KillOutcome`, `ReturnToUserOutcome` and `TaskOutcome`
   moved onto it, with PawPrint and both test projects.
2. `CallProgress`, `CopyRange`, and the write family (`write`, `send`,
   `finishWrite`, `pwrite`), with `WriteOutcome` on `ProcessFate`. The test
   helpers `WriteOutcomes.admitThenWrite` and `admitThenSend` drive the new
   public protocol through internal `try` forms of it, so every existing write
   test exercises the paused values.
3. The socket calls (`bind`, `connect`, `setsockopt`, `getsockopt`).
4. The path calls renamed and moved onto `CallProgress`.
5. `WoofWare.PosixKernel/README.md`: "Making a call", the tables, "Moving
   bytes" and "Sleeping and waking".

## 6. Tests

The existing suites are the main oracle: they pin every answer the calls
give, and after step 2 the write tests run through the paused values. On top
of them:

- Properties over the generated write and socket scenarios the suites already
  have: every `copyRange` lies inside the buffer the call was made with, and
  resuming with exactly those bytes answers what the internal admit-then-call
  path answers.
- A resume given bytes of the wrong length fails loudly.
- PawPrint's Guest fixtures, for the client side of the migration.

## Not in scope

- A recorder of kernel inputs, or any union of every call. With these two
  shapes in place, a recorder in a separate package can wrap the typed
  functions, and its entries are the call's arguments plus what was supplied
  at each pause.
- A single `finish` for every parked call. The client knows which call it is
  finishing, and a single function would answer a union of every family's
  outcome.
- Every call taking the calling task.
- Boot and launch configuration as data.
