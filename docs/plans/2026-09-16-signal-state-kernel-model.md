# SignalState: from pal_signal.c shim to kernel model

Issue #1380 found that `WoofWare.PosixKernel.SignalState` re-encodes CoreCLR's
`pal_signal.c` shim under POSIX names rather than modelling a kernel's signal
state. Two of its bullets were fixable without an architectural decision, and
the two PRs below this one in the stack (`issue-1380-signalstate-numbering`,
`issue-1380-coalesce-standard-signals`) fix them:

* **Numbering-blindness** (`block` accepting SIGKILL, `Other 17` being a
  second identity for SIGCHLD): `SignalState.initial` now takes the platform's
  `SignalNumbering`, every operation parses its signal at that boundary, and
  the unblockable/uncatchable screens are measured facts
  (`Signal.isUnblockableUnder`, `TestSignalMaskAgainstHost`).
* **Queue-not-coalesce**: standard signals now coalesce per
  (canonical signal, pending set), real-time signals queue under Linux
  numbering, both measured.

What remains is the architectural core, and each piece below either relitigates
a decision `2026-08-23-posix-kernel-extraction.md` Stage 4 records taking
deliberately, or changes the published package's shape enough that the options
should be chosen consciously rather than by whichever agent got there first.

## The remaining misalignments

1. `Enabled : Set<Signal>` is `SystemNative_EnablePosixSignalHandling`'s
   per-signo bit; `Handler : 'Handler option` is `g_posixSignalHandler`;
   `SignalInitState.Initialized of dispatcher` is the `SignalHandlerLoop`
   pthread. All three are facts about CoreCLR's shim, not about a kernel.
2. Nothing applies `DefaultDisposition`: a pending, non-enabled SIGTERM stays
   queued forever where a kernel applies SIG_DFL and terminates the process.
3. `UnixTaskTable.register` does not inherit the creator's mask
   (pthread_create semantics), and nothing clears a dead task's mask.
4. PawPrint never calls `enqueue`, `block` or `unblock`, so the pending/mask
   logic has no end-to-end consumer: every fix above is observable only
   through the model's own tests.
5. (Recorded, not previously written down:) delivery is FIFO where Linux
   delivers lowest-signo-first among pending; and a single-threaded Darwin
   process merges a process-directed with a thread-directed pending instance
   (xnu assigns process-directed signals to a thread at generation), which
   the (signal, target) coalescing key does not model.

## Decision 1: dispositions — table, bit, or status quo

**(a) Kernel-authentic disposition table.**

```fsharp
type SignalDisposition<'Handler> =
    | Ignore              // SIG_IGN
    | Handle of 'Handler  // a sigaction handler
// absent from the map = SIG_DFL, which is a truthful default
Dispositions : Map<Signal, SignalDisposition<'Handler>>
```

`enable` becomes `setDisposition signal (Handle h)`; the enabled set is the
projection "keys whose disposition is `Handle`". Delivery returns a
description rather than only handler runs:

```fsharp
type SignalDelivery<'Task, 'Handler> =
    | RunHandler of PendingSignal<'Task> * receiver : 'Task * 'Handler
    | DefaultTerminate of Signal
    | DefaultStop of Signal
    | DefaultContinue of Signal
```

and generation (`enqueue`) discards a signal whose disposition at that moment
is Ignore (explicit or default) — with a **measured flavour divergence** in
what a block does to that rule. On Linux a blocked ignored signal stays
pending (`sigpending` reports it, and a handler installed before the unblock
receives it); on Darwin it is discarded at generation despite the block
(never pending, and never delivered however the disposition changes before
the unblock). Both columns measured 2026-09-16 on Linux 6.18.5 and Darwin
25.6.0, two runs each, with SIG_IGN'd SIGUSR1, default-ignored SIGWINCH, and
a SIG_DFL SIGUSR2 control that stayed pending on both — process-directed and
thread-directed generation alike. The rows are probe-pinned in `TestSignal`
rather than host-equality-measured, for the reason `TestSignalAgainstHost`'s
header gives for the sigaction facts: asking in-process means changing the
test host's own dispositions. This fixes misalignments 1 and 2 in one
shape, and per-signal handlers are what `sigaction` actually holds.

Cost: the largest API change; `SystemNative_EnablePosixSignalHandling`'s arm
must produce a `'Handler` at enable time, which forces Decision 2's
relocation of the global handler to PawPrint (the shim's `Enable` and
`SetPosixSignalHandler` arrive separately).

**(b) Keep the `Enabled` bit; add default application beside it.**

`Enabled` stays (documented as "a handler is installed", which is what the
shim's sigaction install makes true of the kernel), and delivery gains the
`SignalDelivery` cases above, driven by `Signal.defaultDispositionUnder` for
non-enabled pending signals; `enqueue` drops unblocked default-Ignore
signals at generation. Fixes misalignment 2; leaves 1 in place but makes the
docstrings honest about it.

**(c) Status quo plus documentation.** Record the shim-shape as a deliberate
approximation (the issue's complaint is partly that it was *not* recorded)
and wait for a consumer to force the design.

**Recommendation: (a), but only landed together with Decision 4's consumer**
— without a consumer the extra cases are dead code whose correctness only the
model's own tests can see, which is exactly the state the issue objects to.
If Decision 4 is deferred, do (c) now: one docstring PR, no new mechanism.

## Decision 2: where the shim's own state lives

Stage 4 kept the dispatcher on `SignalInitState.Initialized` deliberately
("honest at the library's altitude... records *which* task dispatches
without claiming to know what a task is"), and the "exists iff initialised"
invariant is real. But `Init` and `Handler` are still facts about CoreCLR's
shim, and a non-.NET client of the package inherits them as vocabulary.

**(a) Move both to PawPrint.** A `PosixSignalShim` record in
`WoofWare.PawPrint` holding `Init : SignalInitState<ThreadId>` (the DU moves
wholesale, keeping its invariant) **and the global callback slot**. The slot
cannot dissolve into the per-signal disposition table:
`PosixSignalRegistration.Initialize` installs `OnPosixSignal` through
`SetPosixSignalHandler` *before* `Register` enables any signal, so at
install time there is no disposition row to hang it on, and disabling the
last signal would lose it. The enable arm reads the shim's global and writes
`Handle shimHandler` into the table (redundant for PawPrint, whose handler
is always that one global, but the honest shape for the library). The
library keeps dispositions, masks, pending. `UnixSystem.checkInvariants`
loses the dispatcher rule; `EmulatedKernel`'s own invariant check gains it
(it can see both sides).

**(b) Keep them, renamed and documented as client state.** The library keeps
a `ClientData : 'Client option`-shaped slot rather than signal-specific
fields. Weaker: it stops the names lying without making the model truer.

**Recommendation: (a), as part of the same stage as Decision 1(a)** — the
enable arm needs the handler at enable time, and the shim record is where it
reads it from.

## Decision 3: per-task masks — storage and lifecycle

**(a) `BlockedSignals : Set<Signal>` on `UnixTaskState`** (the extraction
plan's own sketch). `UnixTaskTable.register` takes the initial mask, so the
compiler asks every creation site "inherited from whom?" — `pthread_create`
inheritance becomes unforgettable, and `SignalMaskWithoutTask` becomes
unrepresentable and its defect case is deleted. Note what this does *not*
buy: PawPrint retains a terminated thread's record and its kernel task
(`checkTaskInvariants` documents that nothing removes a thread today), so
the mask does not die with the thread by itself — the thread-termination
transition must explicitly clear `BlockedSignals`, and that clear is a
stated part of this option, with its own test that a masked worker's mask
is empty once the thread terminates. Mask *writes* (`sigprocmask`) become
`UnixSystem`-level operations, which is where the numbering lives for
canonicalisation and the unblockable screen; delivery takes the mask beside
each live task (`live : ('Task * Set<Signal>) ...`).

PawPrint's sites: `Program.fs` main-thread creation inherits nothing (an
execed process's initial thread); `allocateUnstartedThread` copies at
*Start* time (`startUnstartedThread`, where the real pthread_create
happens), not construction; `allocateParkedThread` inherits from the thread
that called `InitializeTerminalAndSignalHandling`, which is what the real
`SignalHandlerLoop` pthread does.

**(b) Masks stay on `SignalState`; add `inheritMask creator child` and
`clearMasks task`,** called from the same PawPrint sites. Smaller diff; but
inheritance is then a discipline ("remember to call it"), which is the
failure mode the issue documents, and the mask/task-table agreement stays a
test-time invariant instead of a type-level fact.

**Recommendation: (a).** Note that today no mask is ever non-empty in
production: the BCL exposes no managed path to `pthread_sigmask` at all
(verified against the pinned runtime source — no `SigProcMask` shim exists
anywhere in `$DOTNET_RUNTIME_SRC`'s CoreLib or coreclr trees), so either
option is unobservable until fork/exec or a raw-syscall guest arrives; (a)
is preferred *because* it is the option that does not rely on being
remembered when that day comes.

## Decision 4: the consumer

Everything above is invisible until something generates a signal. Two
generation boundaries exist, and they are not interchangeable — the shim
one is narrower than it looks. `SystemNative_Kill` (which backs
`Process.Kill`) screens its `PosixSignal` argument down to None, SIGKILL
and SIGSTOP in `pal_process.c`, refusing everything else with EINVAL, so it
can never generate a catchable signal and cannot exercise a registered
handler:

* Implementing `SystemNative_Kill` faithfully gives `Process.Kill` on self:
  an `enqueue` of SIGKILL with `Target = ValueNone` whose delivery is
  `DefaultTerminate` (128 + 9), differential against the real runtime. Its
  screening rule is the shim's, so it lives beside `PosixSignalPal`.
* The catchable-signal generator is a guest's own raw `DllImport` of libc
  `kill(2)`/`raise(3)` — the pattern `TestPlatformSocketSupport` already
  uses for raw syscalls — landing in `Native/` as a POSIX-side boundary:
  `kill(getpid(), signo)` becomes `enqueue` with `Target = ValueNone`, and
  the signo is read under the platform numbering via `ofRawSignoUnder`.
* A signal whose delivery answers `RunHandler` wakes the existing dispatcher
  (`SignalDispatch.trySpawnHandler` already consumes `nextDelivery`);
* `DefaultTerminate` surfaces as `ExecutionResult.SignalTerminated`, which
  already exists and already computes the `128 + signo` exit code;
* `DefaultStop`/`DefaultContinue` stay refused loudly, as the
  `HandleNonCanceledPosixSignal` arm refuses them today.

That makes bullet 2's scenario (`kill -TERM` self, through the raw-libc
boundary, with no handler registered) an end-to-end differential test
against the real runtime, which is the standard this project holds kernel
behaviour to. One prerequisite: `DifferentialOracle.compareOutcomes` today
fails unconditionally on `RunOutcome.SignalTerminated`, so the oracle needs
an explicit extension (or the tests a dedicated fixture) that compares the
signal-derived exit code against the real process while still asserting
PawPrint's outcome *is* signal termination.

## Suggested stages, if 1(a)/2(a)/3(a)/4 are chosen

1. `SignalDelivery` + generation-time Ignore drop + default actions in the
   library, property-tested against an extended oracle (Decision 1's
   mechanics, still driven off the `Enabled` bit so the stage is small).
2. Extend `DifferentialOracle` (or add a dedicated fixture) so a
   signal-terminated guest can be compared against the real process's exit
   code rather than failing unconditionally.
3. The generation boundaries consuming it end-to-end: `SystemNative_Kill`
   with its real None/SIGKILL/SIGSTOP screen, and the raw-libc `kill(2)`
   handler for catchable signals, with `sourcesImpure` guests for handler
   delivery and differential guests for default termination exit codes.
4. Disposition table replacing `Enabled`, shim state (`Init`, the global
   callback) relocating to PawPrint (Decisions 1(a) + 2(a) proper).
5. Masks onto `UnixTaskState` with register-time inheritance (Decision 3(a)).

Each stage keeps the suite green on its own and none blocks the others'
review.

## Approximations to record either way

* FIFO delivery order where Linux picks lowest-signo-first among deliverable
  pending signals (`next_signal`); unobservable until two signals can be
  pending at once through a modelled path.
* The (signal, target) coalescing key delivers twice where a single-threaded
  Darwin process delivers once (xnu assigns process-directed signals to a
  thread at generation; measured 2026-09-16, three runs each shape). Revisit
  if `kill(2)`/`pthread_kill(2)` are modelled for Darwin flavours with
  thread-directed use.
* `SignalState` models the libc-mediated surface, not raw syscalls: glibc's
  screening of signals 32/33 is part of `isUncatchableUnder` and
  `isUnblockableUnder`, and their kernel-level blockability/queueability is
  deliberately not represented.
