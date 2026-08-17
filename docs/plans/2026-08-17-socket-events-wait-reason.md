# A wait reason for a blocking `epoll_wait`

Scope: representation only. After this change PawPrint can *express* "this thread is parked
inside `SystemNative_WaitForSocketEvents`", and every classifier that must answer a question
about a thread status answers it for the new one. No production code constructs the state, no
socket native is implemented, and no observable behaviour changes.

## Why a new `ThreadStatus` variant is required

`ThreadStatus` has nine blocked variants today and none of them fits.

Every existing wait is woken either by the virtual clock crossing a deadline or by another
thread performing a *synchronisation* operation on a named primitive (a monitor release, a
`Pulse`, a `SetEvent`, a thread terminating). An epoll wait is woken by the kernel's *I/O
readiness* state changing — which in a fully-emulated kernel is still ultimately caused by
another thread, but the object waited on is an epoll instance, not a synchronisation
primitive, and the wake condition is "the port's ready set became non-empty".

`Parked` is the closest existing thing — a thread woken by a kernel-side event source polled
between IL steps, exactly like `SignalDispatch.trySpawnHandler` does for signals — but its
docstring (`ThreadStatus.Parked`'s docstring) is explicit that `Parked` means *no managed `Thread` heap
object backs the thread*. Kestrel's engine thread does have one: `SocketAsyncEngine`'s
constructor starts a plain managed `Thread` with a `ParameterizedThreadStart` (read from the
shipped `System.Net.Sockets.dll` IL — `SocketAsyncEngine/<>c::<>9__15_0 :
ParameterizedThreadStart`, with `EventLoop()` as the target; `System.Net.Sockets` is not in
the sparse runtime checkout, so the DLL is the only available source). So the waiter is an
ordinary guest thread and `Parked` is disqualified.

## The wait cannot time out — and that is flavour-independent

The strongest form of this fact is the *managed* extern, because PawPrint reimplements the
native side and so it is the managed signature that constrains PawPrint. From
`$DOTNET_RUNTIME_SRC/src/libraries/Common/src/Interop/Unix/System.Native/Interop.SocketEvent.cs:48`:

```csharp
[LibraryImport(Libraries.SystemNative, EntryPoint = "SystemNative_WaitForSocketEvents")]
internal static unsafe partial Error WaitForSocketEvents(IntPtr port, SocketEvent* buffer, int* count);
```

There is no timeout parameter, on any CoreLib flavour. AGENTS.md warns that CoreLib is
`#if`-split per target, so it matters that this is the same on all of them.

The native side agrees, on both flavours PawPrint can present.
`pal_networking.c` has three `#if`-selected `WaitForSocketEventsInner` definitions:

- epoll (line 3175): `epoll_wait(port, events, *count, -1)` — infinite; `EINTR` retried in a
  `while` loop.
- kqueue (line 3348): `kevent(port, NULL, 0, events, GetKeventNchanges(*count), NULL)` — the
  trailing `NULL` is the `struct timespec *timeout`, so also infinite; `EINTR` retried in the
  same shape.
- neither (line 3393): `return Error_ENOSYS` without blocking. Not a flavour PawPrint models.

Checking kqueue is not academic: `SimulatedUnixFlavour` has `Darwin` as a first-class case
(`SimulatedUnixFlavour`), `SimulatedUnixPlatform.macOsArm64` is a shipped preset
(`SimulatedUnixPlatform.macOsArm64`), and natives already branch on the flavour
(`NativeSystemNative`). "The emulated kernel is Linux" is a default, not a guarantee,
and a flavour-dependent answer here would have forced a deadline into the design.

Both real variants also carry the same verbatim comment that a zero-event return is
impossible: with an infinite timeout the wait blocks until an fd is added to the port *and* an
event occurs on it. So a registration performed by another thread is itself a wake edge, not
only data arrival.

Consequences: the variant carries no `deadlineTicks`, `waitDeadline` puts it in the `None`
group, and there is no `FiredDeadline` case and no fire function. This is the one wait reason
in the system that cannot time out, and making that structural is strictly better than
carrying a `None` nothing can set.

(One flavour divergence to record for the *future handler*, not for this change: the two error
paths write different sentinels into `*count` — `0` for epoll, `-1` for kqueue.)

## What the variant carries

`SystemNative_CloseSocketEventPort(intptr_t port)` is `close(ToFileDescriptor(port))`, and
`SystemNative_CreateSocketEventPort` hands back an fd from `epoll_create1`. An epoll port
*is* a file descriptor, so the identity already exists in this codebase:

```fsharp
| BlockedOnSocketEvents of port : OpenFileDescriptionId
```

`OpenFileDescriptionId` rather than the raw `int` fd, for the reason `OpenFileDescription`'s
own docstring gives (the `OpenFileDescription` docstring): everything `dup(2)` shares lives on the
description, and a `dup`'d port fd waits on the same epoll instance. Keying on the descriptor
*number* would break under `dup`; keying on the description is what the kernel does. Minting a
fresh `SocketEventPortId` would give one kernel object two identities, and the wake path would
end up keyed on the wrong one.

`OpenFileTarget` has no epoll-instance case, so no description in the registry can presently
*be* a port: the variant names a description whose kind does not exist yet. That is deliberate
— `OpenFileTarget.SocketEventPort` forces an `OpenFileObject` case too (the exhaustive
projection at `OpenFileDescription.object`) plus `read`/`lseek`/`flock`/`fstat` semantics
for an epoll fd, none of which the wait reason needs. There is precedent both for the
representation preceding its transition (`Parked`, `ThreadStatus.Parked`) and for a status
payload carrying an address with no type-level proof the kernel object exists at it
(`BlockedOnSyncBlockAcquire`'s `ManagedHeapAddress`, `ThreadStatus.BlockedOnSyncBlockAcquire`). The variant's
docstring must carry that admission explicitly, so the gap is honest in the code and not only
in this document.

## The design fork this change has to resolve

The new variant must answer `ThreadStatus.parksPastTheBlockingCall`, and the answer depends on
how the future handler blocks. Two genuinely different shapes, both already in the codebase.

### Resume-style (what `Sleep`, `Join`, `WaitHandle` do)

The handler advances the caller's PC past the blocking call, the dispatcher pops the native
frame, and the thread parks — the contract `blockOnSleep` states at `Scheduler.blockOnSleep`'s docstring. On
wake the thread resumes *after* the call, so the wake path must produce the call's results.
For `WaitHandle` that is an optimistic eval-stack push rewritten by the fire; for
`WaitForSocketEvents` the results are two *memory writes* (fill the caller's `SocketEvent*`
buffer, store the count through `int32*`). Those pointers are meaningless once the thread is
Runnable, so by the same argument the existing variants use for `deadlineTicks` they would
have to live in the status:

```fsharp
| BlockedOnSocketEvents of
    port : OpenFileDescriptionId * eventBuffer : ... * maxEvents : int * countLocation : ...
```

`parksPastTheBlockingCall` would be `true`.

### Re-entrant (what `BlockedOnClassInit` does)

The handler leaves its native frame on the stack and parks without advancing the PC. When the
port becomes ready the dispatcher re-enters the handler, which re-reads its own arguments and
completes down its ordinary success path. `NativeHandlerResult.BlockedOnClassInit`
(`NativeHandlerResult.BlockedOnClassInit`'s docstring) is exactly this: "Dispatcher leaves the native frame on the
stack so the handler can be re-entered when the lock is released."

`parksPastTheBlockingCall` would be `false`, and the status carries only the port.

### Choice: re-entrant

1. **It keeps every guest-memory write on the writing thread's own step.** Resume-style makes
   the *waking* thread fill the waiter's event buffer and count location — cross-thread
   guest-memory writes performed inside another thread's step. That is hostile to the
   heap-access-chokepoint and race-detector direction this codebase is building toward, and to
   scheduling-visible-dependence banding generally. This is the reason that survives even if
   every other consideration changes.
2. **It is a faithful model of the PAL**, which is literally a retry loop around
   `epoll_wait`/`kevent`; "block, be re-entered, try again" is what the primitive does, and
   the `EINTR` retry falls out rather than needing a special case.
3. **One completion path instead of two.** Resume-style needs the buffer-filling logic in both
   the "ready immediately" branch and the wake branch, and those two must agree forever.
4. **Less baked-in commitment**: one payload field instead of four, and the three extra are
   exactly the ones this change cannot test.

Two notes for whoever writes the handler. It needs no eval-stack re-entry markers — unlike
`PushedManagedCallee` (`NativeHandlerResult.PushedManagedCallee`), this handler is stateless until the port
is ready, so re-entry is indistinguishable from first entry and can share one code path. And
the representation forecloses neither wake *mechanism*: waking by edge from the natives that
mutate readiness, and waking by polling in the driver preamble the way
`SignalDispatch.trySpawnHandler` does, both work off "enumerate the `BlockedOnSocketEvents`
threads whose port is ready". That choice belongs to the handler's change, not this one.

Reversibility: if re-entrant turns out wrong, adding payload is a compile error at the two
classifiers and at every construction site, of which there will be exactly one. That is the
main reason to be comfortable deciding before the handler exists.

## Sites touched

| File | Change |
| --- | --- |
| `WoofWare.PawPrint/ThreadState.fs` | the new variant, plus arms in the two exhaustive classifiers |
| `WoofWare.PawPrint/Program.fs` | `waitDeadline` — into the `None` group, no `FiredDeadline` case |
| `WoofWare.PawPrint.App/DebuggerServer.fs` | render as `{"kind": "blockedOnSocketEvents", "port": N}` |

`ThreadState.fs` (fsproj line 65) compiles after `FileDescriptorRegistry.fs` (line 47), so
`OpenFileDescriptionId` is in scope. Every other `ThreadStatus` match in the library and App
has a wildcard or a fail-loud `other` arm, so the compile-forced set is exactly these four
matches. `Scheduler.runnableThreads` (`Scheduler.runnableThreads`) and `Scheduler.hasAnyRunnable`
(`Scheduler.hasAnyRunnable`) match `Runnable` against a wildcard, so a new blocked state is
automatically neither runnable nor schedulable — correct, and worth an assertion rather than an
edit. `GuestLocation.renderThread` interpolates the status with `%O`,
so the stuck-thread description picks the variant up with no edit; the implementation checks
the rendered text is legible rather than assuming it.

## Deliberately not in this change

- Any park or wake function in `Scheduler.fs`. A park function would have no production
  caller, and a wake function that only flipped the status back to `Runnable` would be *wrong*
  — the wake must also deliver the event batch — so shipping one would invite a future caller
  to use a half-implemented primitive.
- Any `NativeHandlerResult` / `WhatWeDid` case. A new `NativeHandlerResult` case's only
  consumer is `dispatchNative`'s exhaustive match, so adding one forces implementing the
  dispatcher's park behaviour, which is the whole feature. Unlike the classifiers it cannot be
  reached from a test at all.
- `OpenFileTarget.SocketEventPort`, and every socket native.

## Tests

`TestLowLevelMonitor.fs` / `TestWaitHandle.fs` / `TestSyncBlockMonitor.fs` build `ThreadState`
records directly with an arbitrary `ThreadStatus`, so the variant is exercisable without any
production code constructing it. New fixture in that style:

1. **`hasNoActiveFrame` is `false`.** A thread parked in a blocking epoll has a live frame, so
   frame-reading callers must not skip it. Under re-entrant parking that active frame is the
   *native interop frame* (`EventLoop` is an outer frame) — which is also why the blocked
   thread in any test that reaches `GuestLocation` needs a real frame, per the trap documented
   at `TestLowLevelMonitor`'s stub-thread comment. Kills a `true` answer, which would make
   `positionOfThread` report `NoFrame` for a thread that has one.
2. **`parksPastTheBlockingCall` is `false`.** A direct constant pin, and the test comment must
   say why it can only be direct: the active frame is a native frame, `MethodInfo.tryIlBody`
   returns `None` for it (`MethodInfo.tryIlBody` → `MethodBody.tryIl`), so
   `GuestLocation.precedingCallOffset` finds no candidate and `reportableOffset` falls back to `IlOpIndex` identically under both answers. There is
   therefore no behavioural observer, in the same way class-layout rules have no guest
   observer. The pin exists to force a re-decision if the handler ever becomes resume-style.
3. **Not chosen by the scheduler** when another thread is Runnable, and **nothing runnable at
   all** when it is the only thread. Split in two because they exercise different functions
   (`chooseNext` via `runnableThreads`, and `hasAnyRunnable`), and a single test would leave one
   unmutated. Labelled in the test comments as Scheduler *regression* tripwires, naming the
   mutant they exist for (`| ThreadStatus.BlockedOnSocketEvents _ -> tid :: acc` added to
   `runnableThreads`' fold) — because no way of writing *this* diff can make the status
   schedulable, so they are not coverage of this change.
4. **`renderThread` renders the port.** Public, in the style of
   `TestGuestLocation`'s pure-renderer tests. Asserted as the whole string rather than by substring: a
   `shouldContainText "7"` also passes on a renderer that drops the port and emits a 7 from an
   IL offset or a line number. Kills a payloadless variant, which could not render the port at
   all, and pins that a deadlocked engine thread is diagnosable.

### Mutation results

Each production arm was flipped in turn, rebuilt, and the fixture re-run. Every mutant died,
and each killed only the tests that claim to cover it:

| Mutation | Tests killed |
| --- | --- |
| `hasNoActiveFrame` → `true` | live-frame test only |
| `parksPastTheBlockingCall` → `true` | parking test only |
| `runnableThreads` treats the status as runnable | both scheduler tests |
| `hasAnyRunnable` treats the status as runnable | nothing-runnable test only |

The `waitDeadline` arm and the `DebuggerServer` arm have no mutant to run, for the reasons
below.

### Two things deliberately not tested, and why

**No test that `waitDeadline` returns `None`.** The property is enforced by the type, not by a
test: the variant has no `int64 option` field, so no clock wake is *representable*. The only
arm that would compile and be wrong is one that unwrapped `OpenFileDescriptionId`'s `int64` and
paired it with an unrelated `FiredDeadline` kind — and every fire function `failwith`s on the
status mismatch (`Scheduler.fireSleepTimeout`, `Scheduler.fireSleepTimeout`, and analogues), so even that
is loud rather than silent. Testing it would mean widening two deliberately-private things —
`waitDeadline` (`Program.waitDeadline`) and `FiredDeadline` (`Program.FiredDeadline`, `type private`) — or
hand-building a `PreparedProgram` and arranging the clock to provoke a crash. Neither is worth
it against an unrepresentable bug; the reasoning goes in a comment on the `waitDeadline` arm
instead.

**No test of the debugger's status JSON.** `DebuggerServer.writeThreadStatus` is `let private`, which F# scopes to its enclosing module, so the App assembly's
`InternalsVisibleTo("WoofWare.PawPrint.Test")` does not reach it; and `TestDebuggerServer.fs`
drives the server only through Roslyn-compiled guests, none of which can reach a state no
production code constructs. Widening it just for this arm would pin one of thirteen arms and
leave the rest as they are. The arm is compile-forced, and test 4 covers the
diagnostic-legibility concern through the public renderer.

### A wrong implementation the tests cannot catch

Declaring the payload as a raw `int`/`int64` instead of `OpenFileDescriptionId`, or adding a
spurious `deadlineTicks : int64 option` that every test constructs as `None`, would defeat the
representation goals above and be invisible to every test — they are representation choices,
not behaviour. The payload identity and the field set are enforced by review, not by the test
suite, and this document is the statement of what review should enforce.
