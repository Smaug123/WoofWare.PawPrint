namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging
open WoofWare.PosixKernel

/// What the signal half of a scheduler tick did to the process.
[<RequireQualifiedAccess>]
type SignalPoll =
    /// The process carries on, in this state.
    | Continues of IlMachineState
    /// A signal killed the process: one System.Native re-raised at its
    /// default, or the SIGABRT of an abort a native handler called. The state
    /// is the machine as it stood then, and `ended` the kernel's answer to the
    /// signal, whose `EndedProcess.termination` is `ProcessTermination.Signaled`.
    | ProcessKilled of IlMachineState * ended : ProcessEnd

/// System.Native's signal handling, between two guest instructions: its native
/// handler, which the kernel runs on the leader when a caught signal is
/// delivered, and its dispatcher thread (`SignalHandlerLoop`), which PawPrint
/// allocates at `SystemNative_InitializeTerminalAndSignalHandling` (see
/// `PosixSignalShim`). The two are joined by the shim's signal pipe, a real
/// pipe in the kernel model: the handler writes each signal's number into it
/// as one byte, and the dispatcher reads them out in the order written.
///
/// `poll` runs once per tick, before the scheduler picks a thread:
///
///   * The leader returns to user mode and takes the signals the kernel
///     delivers to it now (see `UnixSignal.onReturnToUser`), with a handler
///     frame for each caught one; for each one System.Native catches, its
///     handler writes the signal's number into the pipe and returns through
///     `sigreturn`. For one CoreCLR's hardware-fault handler catches, that
///     handler restores the default, or aborts the process (see
///     `runPalFaultHandler`). No guest code runs, and the dispatcher need not
///     be idle.
///     Several signals delivered at one return are written in the reverse of
///     the order the kernel took them in, as a real kernel's handler frames
///     run them.
///   * If the dispatcher is Parked, it is blocked reading the pipe, its task
///     asleep in the kernel in that read: when the kernel wakes it, it reads
///     one byte. For a signal with a registration it
///     calls the managed callback, as a fresh bottom frame on the dispatcher
///     taking `(int signo, int posixSignalEnumValue)`; the frame has no
///     `ReturnState`, so its `ret` surfaces as `ExecutionResult.Terminated`,
///     which is the cue for `reParkAfterHandler`. For a signal without one it
///     calls `SystemNative_HandleNonCanceledPosixSignal` itself, and stays
///     Parked.
///
/// The kernel delivers a signal sent to the process to its leader, and there
/// System.Native's native handler passes it on to the dispatcher, which runs
/// the managed handler: so the leader is the task asked, and the dispatcher,
/// which is never the leader, never receives a signal itself. Only the leader
/// is asked, because nothing PawPrint answers leaves a signal pending on any
/// other thread: `kill(2)` aims at the whole process; and `raise(3)`, which
/// aims at the raising thread, and the SIGPIPE a write into a pipe with no
/// reader raises, which Linux aims at the writing thread, are refused by
/// `NativeLibc.raiseSignal` and `SystemNative_Write` when the signal would
/// stay pending on a thread other than the leader.
///
/// A thread's mask can now hold a signal back (`NativeLibcSignalMask`), and
/// the leader is still the only task asked. A signal sent to the process that
/// the leader blocks and another thread does not is refused by the kernel
/// (`SignalReceiverRefusal.LeaderBlocks`), when it is sent or at this poll,
/// and the refusal fails the run.
///
/// A default action the leader's return applies is refused loudly when it
/// terminates or stops (`ProcessFate.ProcessEnded` and
/// `ReturnToUserOutcome.ProcessStopped`): such a default is applied when the signal is generated
/// (see `NativeLibc.kill` and `NativeLibc.raiseSignal`), so one reaches this
/// poll only by becoming receivable later, as a handler frame's mask is popped
/// or a mask call unblocks it, and applying it at delivery is not modelled.
/// `ReturnToUserOutcome.ContinueDiscarded` is a SIGCONT at its default, which
/// the kernel has discarded as the leader took it: the process was never
/// stopped (PawPrint refuses a stop), so nothing happens, and the leader's
/// return goes on to whatever else it takes.
[<RequireQualifiedAccess>]
module SignalDispatch =

    /// Build the arguments the handler expects: the modelled `OnPosixSignal`
    /// shape is `static int OnPosixSignal(int signo, PosixSignal signal)`.
    /// `PosixSignal` is a managed enum and crosses the IL boundary as its
    /// underlying `int`, so both arguments are plain `CliType.Numeric Int32`.
    /// `signo` is the signal's number under the simulated platform's
    /// numbering, from `Signal.toRawSignoUnder`;
    /// `posixSignalEnumValue` is the negative enum identity from
    /// `PosixSignalPal.toEnum` for the modelled cross-platform signals or
    /// `PosixSignalInvalid` (0) for signals with no managed enum value
    /// (matching real CoreCLR `pal_signal.c`, which overwrites the
    /// out-parameter with `PosixSignalInvalid` when
    /// `TryConvertSignalCodeToPosixSignal` returns `false`).
    let private buildArgs (numbering : SignalNumbering) (signal : Signal) : ImmutableArray<CliType> =
        let signo = Signal.toRawSignoUnder numbering signal
        let posixEnum = PosixSignalPal.toEnum signal

        ImmutableArray.CreateRange (
            [
                CliType.Numeric (CliNumericType.Int32 signo)
                CliType.Numeric (CliNumericType.Int32 posixEnum)
            ]
            : CliType list
        )

    /// Loose signature gate on the registered handler. Real CoreCLR installs
    /// `PosixSignalRegistration.OnPosixSignal` with exactly the
    /// `(int, PosixSignal) -> int` shape, but PawPrint's tests want to
    /// substitute simpler stand-ins (e.g. `Math.Max(int, int) -> int`) so the
    /// dispatch-wiring test can drive the handler frame without dragging in
    /// the whole PosixSignal type. We therefore check:
    ///   * exactly two declared parameters (the BCL handler is static, but a
    ///     stand-in instance method with two params is fine because
    ///     `MethodState.Empty` will be told it is static and the
    ///     parameter-count check there is independent of `this`);
    ///   * the return type is `MethodReturnType.Returns` of a primitive
    ///     `Int32` — anything else can't be the modelled PosixSignal handler
    ///     and we'd discard the return value at a non-`int` width.
    /// Parameter types are not strictly checked: the real handler takes an
    /// `int` and a `PosixSignal` enum (which is `int` at the IL level), and
    /// a permissive gate lets tests use any `(?, ?) -> int` method.
    let private validateHandlerSignature
        (concreteTypes : AllConcreteTypes)
        (mi : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : unit
        =
        if MethodInfo.arity mi <> 2 then
            failwith
                $"SignalDispatch.poll: registered handler %s{mi.Name} on type %s{MethodOwner.describe mi.Owner} declares %d{MethodInfo.arity mi} parameters; expected exactly 2 ((int signo, PosixSignal signal) -> int)."

        match mi.Signature.ReturnType with
        | MethodReturnType.Void ->
            failwith
                $"SignalDispatch.poll: registered handler %s{mi.Name} on type %s{MethodOwner.describe mi.Owner} returns void; expected Int32 (the 'should run default disposition?' flag)."
        | MethodReturnType.Returns ret ->
            match ret with
            | ConcretePrimitive concreteTypes PrimitiveType.Int32 -> ()
            | _ ->
                failwith
                    $"SignalDispatch.poll: registered handler %s{mi.Name} on type %s{MethodOwner.describe mi.Owner} returns a non-Int32 type; expected Int32 (the 'should run default disposition?' flag)."

    /// The byte System.Native's handler writes for `signal`: its number, as
    /// `uint8_t`.
    let private signalByte (numbering : SignalNumbering) (signal : Signal) : byte =
        byte (Signal.toRawSignoUnder numbering signal)

    /// CoreCLR's PAL's handler for a hardware-fault signal, run on the leader
    /// for `frame`'s signal, which a process sent rather than a fault raised;
    /// `replaced` is the disposition the PAL saved when it installed the
    /// handler. `frame` is the handler's own, or System.Native's, whose
    /// handler calls it first.
    ///
    /// Finding no fault in managed code to handle (SIGABRT's handler does not
    /// look), the handler calls `invoke_previous_action`
    /// (pal/src/exception/signal.cpp). Over
    /// the default, that restores it and returns, expecting a faulting
    /// instruction to raise the signal again; a sent signal is not raised
    /// again, so the process carries on with the default installed, which
    /// replaces whichever handler `frame` belongs to. Over an ignore, it calls
    /// `PROCAbort`, and the process dies of SIGABRT.
    let private runPalFaultHandler
        (replaced : PalReplacedDisposition)
        (frame : HandlerFrame<ThreadId, NativeSignalHandler>)
        (state : IlMachineState)
        : SignalPoll
        =
        let signal = frame.Entry.Signal

        match replaced with
        | PalReplacedDisposition.Default ->
            // Before the restore, `invoke_previous_action` runs the runtime's
            // one-shot shutdown notification, which cleans up the debugger
            // transport, and writes a crash dump if one is configured; PawPrint
            // models neither, and the guest sees neither.
            let numbering = SimulatedUnixPlatform.signalNumbering state.Kernel.UnixPlatform

            match
                UnixSignal.sigaction
                    (state.Kernel.Caller state.Kernel.Leader)
                    (Signal.toRawSignoUnder numbering signal)
                    (Some SignalDisposition.Default)
                    state.Kernel.System
            with
            | Ok (_, system) -> state.MapKernel (EmulatedKernel.withUnix system) |> SignalPoll.Continues
            | Error errno ->
                failwith
                    $"SignalDispatch.poll: the PAL's restore of %O{signal}'s default was refused (%O{errno}), but the PAL catches only signals sigaction accepts."
        | PalReplacedDisposition.Ignore ->
            // `PROCAbort` calls `abort` from inside the handler, which never returns through
            // `frame`: `abort` itself unblocks SIGABRT, which `frame`'s mask holds if `frame` is
            // SIGABRT's own, and the process dies of whichever instance it then takes.
            SignalPoll.ProcessKilled (state, EmulatedKernel.abort state.Kernel.Leader state.Kernel)

    /// System.Native's native handler for `frame`'s signal, run on the leader:
    /// write the signal's number into the shim's pipe, having first run the
    /// handler it replaced (see `PosixSignalShim.chainsToNativeHandler`).
    ///
    /// The write goes to the descriptor number the shim was given, through the
    /// kernel, so a guest that has put something else at that number gets the
    /// byte written there, as the real handler would write it.
    ///
    /// Fails where the real handler would do more than that: where the
    /// handler it replaced is not the PAL's hardware-fault handler, and where
    /// its `write` would not take the byte. The handler retries only `EINTR`
    /// and calls `abort()` on any other failure, and a write into a full pipe
    /// blocks the leader until the dispatcher makes room. Fails, too, where the
    /// byte reaches one of the standard output streams the host drains, which
    /// would be guest output that no step streams.
    let private runNativeHandler
        (frame : HandlerFrame<ThreadId, NativeSignalHandler>)
        (state : IlMachineState)
        : SignalPoll
        =
        let signal = frame.Entry.Signal
        let numbering = SimulatedUnixPlatform.signalNumbering state.Kernel.UnixPlatform
        let shim = state.Kernel.PosixSignalShim

        let chained =
            match PosixSignalShim.chainsToNativeHandler signal shim with
            | None -> SignalPoll.Continues state
            | Some (NativeSignalHandler.CoreClrPalFault replaced) -> runPalFaultHandler replaced frame state
            | Some chained ->
                failwith
                    $"SignalDispatch.poll: System.Native's handler for %O{signal} would first run the handler it replaced (%O{chained}), which PawPrint does not model."

        match chained with
        | SignalPoll.ProcessKilled _ -> chained
        | SignalPoll.Continues state ->

        let pipe =
            match PosixSignalShim.signalPipe shim with
            | Some pipe -> pipe
            | None ->
                // Reachable only through a hand-rolled
                // `SystemNative_EnablePosixSignalHandling`: the BCL initialises
                // signal handling before it enables any signal.
                failwith
                    $"SignalDispatch.poll: %O{signal} is caught by System.Native's handler, but signal handling was never initialised, so the handler would write it to descriptor -1, fail, and abort() the process; PawPrint does not model that abort."

        let system = state.Kernel.System
        let bytes = ImmutableArray.Create (signalByte numbering signal)

        let refuse (what : string) : 'a =
            failwith
                $"SignalDispatch.poll: System.Native's handler for %O{signal} writes to descriptor %d{pipe.WriteEnd}, which it was given as its signal pipe's write end, and %s{what}; the real handler abort()s the process, or blocks until the dispatcher reads, and PawPrint models neither."

        // The handler runs on the leader, so the leader makes the write. It
        // writes to the number it was given, whatever the guest has since put
        // there, and the kernel answers for whatever that is.
        let leader = state.Kernel.Leader

        let describe (fate : ProcessFate<WriteOutcome<ThreadId>, ThreadId, NativeSignalHandler>) : string =
            match fate with
            | ProcessFate.Continues (WriteOutcome.Returns answer, _) -> $"%O{answer}"
            | ProcessFate.Continues (WriteOutcome.ReturnsRaising (answer, raised), _) ->
                $"%O{answer}, raising %O{raised.Signal}"
            | ProcessFate.ProcessEnded (ended, _) -> $"the end of the process (%O{EndedProcess.termination ended})"
            | ProcessFate.Continues (WriteOutcome.WouldBlock _, _) -> "that the write sleeps, the pipe being full"
            | ProcessFate.Continues (WriteOutcome.Interrupted, _) -> "that a signal interrupted it"

        let refused (e : exn) : 'a =
            KernelRefusal.fail
                $"SignalDispatch.poll: System.Native's handler for %O{signal} writes to descriptor %d{pipe.WriteEnd}, which it was given as its signal pipe's write end, and the kernel will not answer the write; the real handler abort()s the process, or blocks until the dispatcher reads, and PawPrint models neither"
                e

        let progress =
            try
                UnixReadWrite.write (state.Kernel.Caller leader) pipe.WriteEnd UserBuffer.Mapped 1UL system
            with :? UnmodelledException as e ->
                refused e

        match progress with
        | CallProgress.NeedsCopyIn paused ->

            let outcome =
                try
                    UnixReadWrite.writeWithBytes bytes paused
                with :? UnmodelledException as e ->
                    refused e

            match outcome with
            | ProcessFate.Continues (WriteOutcome.Returns (WriteAnswer.Completed 1L), written) ->
                // A byte that reached a pipe the host drains, one of the
                // standard output streams, is guest-visible output, which only
                // a step's effect streams; this poll has no step to carry one.
                if
                    DeliveryLog.count (UnixSystem.delivered written)
                    <> DeliveryLog.count (UnixSystem.delivered system)
                then
                    failwith
                        $"SignalDispatch.poll: System.Native's handler for %O{signal} writes to descriptor %d{pipe.WriteEnd}, which the guest has replaced with one of the standard output streams the host drains; PawPrint would record the byte as output without streaming it."

                state.MapKernel (EmulatedKernel.withUnix written) |> SignalPoll.Continues
            | outcome -> refuse $"the kernel answers %s{describe outcome}"
        | CallProgress.Answered outcome -> refuse $"the kernel answers %s{describe outcome} without taking the byte"

    /// Whether the leader owes a return to user mode in `system` (`UnixSignal.owesReturn`): a
    /// signal is deliverable to it, or one interrupted the call it is returning from.
    let private leaderOwesReturn (system : UnixSystem<ThreadId, NativeSignalHandler>) (state : IlMachineState) : bool =
        UnixSignal.owesReturn (state.Kernel.Caller state.Kernel.Leader) system

    /// Fails loudly if a signal is deliverable to any of `threads`, none of which is the
    /// leader: PawPrint returns the leader alone to user mode, so such a thread would make its
    /// next call owing the return. Under Linux, a signal sent to the process that the leader
    /// blocks and another thread does not is that thread's to take.
    let private refuseOwedByOthers (threads : ThreadId seq) (state : IlMachineState) : unit =
        for thread in threads do
            if UnixSignal.owesReturn (state.Kernel.Caller thread) state.Kernel.System then
                failwith
                    $"SignalDispatch.poll: a signal is deliverable to thread %O{thread}, which is not the process's leader %O{state.Kernel.Leader}: the process's leader blocks it while another thread does not, so a real kernel would have that thread take it, and PawPrint delivers a process's signals to its leader alone."

    /// `refuseOwedByOthers` of every thread of the process but its leader.
    let private refuseOwedByNonLeaders (state : IlMachineState) : unit =
        refuseOwedByOthers
            (state.Kernel.Tasks
             |> Map.keys
             |> Seq.filter (fun thread -> thread <> state.Kernel.Leader))
            state

    /// The leader's return to user mode: whatever the kernel delivers to it now,
    /// each through its disposition. A handler frame's handler runs, innermost
    /// first, and each returns through `sigreturn`, after which the leader
    /// returns to user mode again and may take more. System.Native's handler
    /// writes its signal into the pipe, and the PAL's hardware-fault handler
    /// restores the default or aborts the process (see `runPalFaultHandler`);
    /// every other handler is refused.
    ///
    /// Made only where the leader owes it (`leaderOwesReturn`), and fails loudly,
    /// whether or not the leader owes one, where another thread owes one
    /// (`refuseOwedByOthers`). A leader asleep
    /// in a syscall is not in user mode, and owes nothing. A signal it would
    /// take wakes it instead (`WakePrimitive.SignalDeliverable`), the
    /// syscall's finishing call answers that it was interrupted, and the
    /// return here ends the call, failing it with `EINTR` (which this writes to
    /// errno) or restarting it: either way the shim function that made it calls
    /// it again once the leader runs (see `NativeSystemNative`), except a
    /// `sigsuspend` or `pause`, which answers as the return ended it
    /// (`NativeLocals.ReturnedToUser`, which this records in its frame). A
    /// leader in one of PawPrint's own waits (a monitor, `Sleep`, a wait
    /// handle) is asked: CoreCLR's PAL carries those waits on across a signal
    /// handler.
    let private deliverToLeader (state : IlMachineState) : SignalPoll =
        let leader = state.Kernel.Leader

        match PosixSignalShim.signalThread state.Kernel.PosixSignalShim with
        | Some dispatcher when dispatcher = leader ->
            failwith
                $"SignalDispatch.poll: the dispatcher %O{dispatcher} is the process's leader, which the kernel delivers the process's signals to; the dispatcher runs handlers for the leader and is always a thread of its own."
        | _ -> ()

        // This runs between every two instructions, so it answers at once when
        // the leader owes nothing. No frame outlives a poll, so none is waiting
        // for a sigreturn either.
        if not (leaderOwesReturn state.Kernel.System state) then
            // A leader whose `sigsuspend` has ended (it holds a mask to restore
            // and is no longer parked) owes its return, which is what gives
            // back the mask the call replaced.
            match SignalState.maskToRestore leader state.Kernel.Signals with
            | Some mask when (UnixThreadState.parkedIn (EmulatedKernel.taskOf leader state.Kernel.Tasks)).IsNone ->
                failwith
                    $"SignalDispatch.poll: the leader %O{leader} returns from sigsuspend owing no return to user mode, so it would run on with the call's temporary mask rather than %O{mask} (this is an interpreter bug)."
            | Some _
            | None ->
                // A signal the leader blocks may still be another thread's to take.
                refuseOwedByNonLeaders state
                SignalPoll.Continues state
        else

        let returnToUser
            (state : IlMachineState)
            : ProcessFate<ReturnToUserOutcome<ThreadId, NativeSignalHandler>, ThreadId, NativeSignalHandler>
            =
            try
                UnixSignal.onReturnToUser (state.Kernel.Caller leader) state.Kernel.System
            with :? UnmodelledException as e ->
                KernelRefusal.fail "SignalDispatch.poll: the kernel will not say what the leader takes" e

        // Persisted whether or not the return produced an action: discarding a
        // receivable ignored signal is a state change with no delivery, and the
        // return itself is one, which settles what the leader owed.
        let persist
            (systemAfter : UnixSystem<ThreadId, NativeSignalHandler>)
            (state : IlMachineState)
            : IlMachineState
            =
            state.MapKernel (EmulatedKernel.withUnix systemAfter)

        // How the call the leader returns from ended, where a signal interrupted
        // it: errno holds EINTR once it fails with it, and a `sigsuspend` or
        // `pause` waiting to answer is told.
        let interruptedCallEnded (interrupted : InterruptedCall option) (state : IlMachineState) : IlMachineState =
            match interrupted with
            | None -> state
            | Some ended ->

            let state =
                match ended with
                | InterruptedCall.FailsWithEintr ->
                    let numbering = SimulatedUnixPlatform.rawErrnoNumbering state.Kernel.UnixPlatform

                    state.MapKernel (
                        EmulatedKernel.withLastSystemError leader (UnixError.toRawErrnoUnder numbering UnixError.EINTR)
                    )
                | InterruptedCall.Restarts -> state

            let thread = state.ThreadState.[leader]

            match thread.MethodState.NativeLocals with
            | Some NativeLocals.AwaitingReturnToUser ->
                state
                |> IlMachineState.mapFrame
                    leader
                    thread.ActiveMethodState
                    (MethodState.withNativeLocals (Some (NativeLocals.ReturnedToUser ended)))
            | _ -> state

        // Run `frames`, innermost first, each followed by its `sigreturn` and a
        // return to user mode, which may push frames that run before the rest.
        let rec runFrames
            (frames : HandlerFrame<ThreadId, NativeSignalHandler> list)
            (state : IlMachineState)
            : SignalPoll
            =
            match frames with
            | [] -> SignalPoll.Continues state
            | frame :: outer ->

            let handled =
                match frame.Action.Handler with
                | NativeSignalHandler.SystemNative -> runNativeHandler frame state
                | NativeSignalHandler.CoreClrPalFault replaced -> runPalFaultHandler replaced frame state
                | NativeSignalHandler.CoreClrPalTrap
                | NativeSignalHandler.CoreClrPalActivation
                | NativeSignalHandler.GlibcSetXid ->
                    // `NativeLibc.kill` and `NativeLibc.raiseSignal` refuse to
                    // generate these, so this is a test driving the queue by
                    // hand.
                    failwith
                        $"SignalDispatch.poll: %O{frame.Entry.Signal} is caught by a native handler the runtime or libc installed before Main (%O{frame.Action.Handler}), which PawPrint does not model."

            match handled with
            | SignalPoll.ProcessKilled _ -> handled
            | SignalPoll.Continues state ->

            let state =
                state.MapKernel (fun kernel ->
                    EmulatedKernel.withUnix (UnixSignal.sigreturn (kernel.Caller leader) frame.Id kernel.System) kernel
                )

            state |> returnToUserThen (runFrames outer)

        and returnToUserThen (continuation : IlMachineState -> SignalPoll) (state : IlMachineState) : SignalPoll =
            match returnToUser state with
            | ProcessFate.Continues (ReturnToUserOutcome.Resumes interrupted, after) ->
                continuation (persist after state |> interruptedCallEnded interrupted)
            | ProcessFate.Continues (ReturnToUserOutcome.RunHandlers (frames, interrupted), after) ->
                match runFrames frames (persist after state |> interruptedCallEnded interrupted) with
                | SignalPoll.Continues state -> continuation state
                | killed -> killed
            // The kernel has discarded it: the process was never stopped, so
            // there is nothing to resume. The return goes on.
            | ProcessFate.Continues (ReturnToUserOutcome.ContinueDiscarded _, after) ->
                returnToUserThen continuation (persist after state)
            // A pending signal at its default disposition, whose kernel default
            // is to terminate or stop the process. `SignalState.generate`
            // applies such a default at generation whenever some thread can
            // receive the signal, so it is pending here only if none could
            // then: every thread blocked it, through a mask call, and one has
            // since unblocked it. Applying a default at delivery is refused
            // rather than half-modelled, a termination's included, though the
            // kernel has ended the process.
            | ProcessFate.ProcessEnded (ended, _) ->
                failwith
                    $"SignalDispatch.poll: a pending signal at its default disposition, whose kernel default is to terminate, ended the process as the leader took it (%O{EndedProcess.termination ended}); applying a default disposition at delivery rather than at generation is not modelled."
            | ProcessFate.Continues (ReturnToUserOutcome.ProcessStopped signal, _) ->
                failwith
                    $"SignalDispatch.poll: pending %O{signal} is at its default disposition, and its kernel default is to terminate or stop the process; applying such a default at delivery rather than at generation is not modelled."

        match returnToUserThen SignalPoll.Continues state with
        | SignalPoll.Continues state as polled ->
            // The frames the return pushed can come to block a signal of the process's that
            // another thread does not block, which that thread then takes.
            refuseOwedByNonLeaders state
            polled
        | killed -> killed

    /// Start the managed callback for `signal` on the parked dispatcher: the
    /// shim's `g_posixSignalHandler(signo, posixSignal)`. The first callback
    /// gives the dispatcher its managed thread id.
    ///
    /// Fails if no callback was installed by `SystemNative_SetPosixSignalHandler`:
    /// the shim asserts there is one, and the BCL installs it before it enables
    /// any signal.
    let private startCallback
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (dispatcher : ThreadId)
        (signal : Signal)
        (state : IlMachineState)
        : IlMachineState
        =
        let handler =
            match PosixSignalShim.handler state.Kernel.PosixSignalShim with
            | Some handler -> handler
            | None ->
                // The shim's `SignalHandlerLoop` asserts `g_posixSignalHandler
                // != NULL` before calling through it, and a build without
                // asserts calls a null function pointer: there is no
                // behaviour here to model.
                failwith
                    $"SignalDispatch.poll: the dispatcher read %O{signal}, which has a registration, but no handler has been installed with SystemNative_SetPosixSignalHandler; the real shim asserts one has."

        let mi = SignalHandler.methodInfo handler
        validateHandlerSignature state.TypeSystem.ConcreteTypes mi

        let containingAssembly =
            state.LoadedAssembly mi.DeclaringAssemblyFullName
            |> Option.defaultWith (fun () ->
                failwith
                    $"SignalDispatch.poll: assembly %s{AssemblyDefinitionName.simpleName mi.DeclaringAssemblyFullName} for handler %s{mi.Name} is not loaded; the SetPosixSignalHandler QCall should have loaded it."
            )

        let numbering = SimulatedUnixPlatform.signalNumbering state.Kernel.UnixPlatform
        let args = buildArgs numbering signal

        // `MethodState.Empty` enforces an arity check against
        // `MethodInfo.arity mi` (plus 1 if non-static). The handler is
        // expected to be the static `OnPosixSignal`; if a test installs an
        // instance stand-in, that's a configuration error in the test, not
        // something this dispatch path should silently paper over.
        let newMethodState =
            match
                MethodState.Empty
                    state.TypeSystem.ConcreteTypes
                    baseClassTypes
                    state.TypeSystem._LoadedAssemblies
                    containingAssembly
                    mi
                    mi.Generics
                    args
                    None
            with
            | Ok ms -> ms
            | Error _ ->
                failwith
                    $"SignalDispatch.poll: failed to build MethodState for handler %s{mi.Name} on type %s{MethodOwner.describe mi.Owner}."

        // The dispatcher is a native thread until it first calls the callback, when CoreCLR's
        // reverse P/Invoke attaches it (`JIT_ReversePInvokeEnterRare`'s `SetupThread`): it takes
        // a managed thread id, and `SetThread` gives it its `t_currentManagedThreadId`. It stays
        // attached through every later callback.
        let state =
            if state.ManagedThreadObjects.ContainsKey dispatcher then
                state
            else
                let _, state =
                    IlMachineState.getOrAllocateManagedThreadObject loggerFactory baseClassTypes dispatcher state

                IlMachineState.seedManagedThreadId loggerFactory baseClassTypes dispatcher state

        state.MapKernel (fun kernel ->
            { kernel with
                PosixSignalShim =
                    PosixSignalShim.beginCallback (Signal.toRawSignoUnder numbering signal) kernel.PosixSignalShim
            }
        )
        |> IlMachineState.startParkedDispatcher dispatcher newMethodState

    /// The loop's own `SystemNative_HandleNonCanceledPosixSignal(signo)`, on
    /// the dispatcher: no guest code runs, and the dispatcher stays where it is.
    let private handleNonCanceledOnDispatcher
        (dispatcher : ThreadId)
        (signo : int)
        (state : IlMachineState)
        : SignalPoll
        =
        match
            NativeSystemNative.handleNonCanceledPosixSignal
                "SignalHandlerLoop's SystemNative_HandleNonCanceledPosixSignal"
                dispatcher
                signo
                state
        with
        | NonCanceledPosixSignal.Continues state -> SignalPoll.Continues state
        | NonCanceledPosixSignal.ContinuesWithErrno (state, error) ->
            let numbering = SimulatedUnixPlatform.rawErrnoNumbering state.Kernel.UnixPlatform

            state.MapKernel (EmulatedKernel.withLastSystemError dispatcher (UnixError.toRawErrnoUnder numbering error))
            |> SignalPoll.Continues
        | NonCanceledPosixSignal.Terminated (state, ended) -> SignalPoll.ProcessKilled (state, ended)

    /// System.Native's dispatcher and its signal pipe, if the dispatcher is idle, `Parked` in
    /// its loop's read of the pipe, so that a poll may finish that read or make it afresh. A
    /// dispatcher that is not idle is running the callback for an earlier signal, and reads the
    /// next one only once that returns, exactly as the single-threaded `SignalHandlerLoop`
    /// does. Read from the process's own state alone.
    let private idleDispatcher (state : IlMachineState) : (ThreadId * SignalPipe) option =
        match
            PosixSignalShim.signalThread state.Kernel.PosixSignalShim,
            PosixSignalShim.signalPipe state.Kernel.PosixSignalShim
        with
        | None, _
        | _, None -> None
        | Some dispatcher, Some pipe ->
            match Map.tryFind dispatcher state.ThreadState with
            | Some ts when ts.Status = ThreadStatus.Parked -> Some (dispatcher, pipe)
            | Some _ -> None
            | None ->
                failwith
                    $"SignalDispatch.poll: dispatcher thread %O{dispatcher} recorded in PosixSignalShim but no ThreadState entry exists — the initialisation path should always allocate both."

    /// What an open file description refers to, for a refusal's message.
    let private describeTarget (target : OpenFileTarget) : string =
        match target with
        | OpenFileTarget.File (inode, _) -> $"a regular file (inode %O{inode})"
        | OpenFileTarget.Directory (inode, _) -> $"a directory (inode %O{inode})"
        | OpenFileTarget.Epoll _ -> "an epoll instance"
        | OpenFileTarget.Kqueue _ -> "a kqueue"
        | OpenFileTarget.Socket socket -> $"a socket (%O{socket})"
        | OpenFileTarget.Pipe (pipe, pipeEnd) -> $"the %O{pipeEnd} end of pipe %O{pipe}"
        | OpenFileTarget.CharacterDevice (_, device, _) -> $"the character device %O{device}"
        | OpenFileTarget.Fifo (inode, _) -> $"a FIFO (inode %O{inode})"
        | OpenFileTarget.Inotify _ -> "an inotify instance"

    /// `woken`, threads the kernel has woken (`UnixWait.wake`), flipped back to `Runnable` where
    /// they are this process's threads parked in `ThreadStatus.BlockedInSyscall`, with the
    /// threads of other processes among them, which only their own programs can flip. The
    /// dispatcher, idle in its read of the signal pipe, is left `Parked`: its read is finished
    /// by its own poll.
    let private wakeOwnThreads
        (woken : (Caller<ThreadId> * Set<WakePrimitive>) list)
        (state : IlMachineState)
        : IlMachineState * Caller<ThreadId> list
        =
        ((state, []), woken)
        ||> List.fold (fun (state, elsewhere) (caller, _) ->
            if Caller.processId caller <> state.Kernel.ProcessId then
                state, caller :: elsewhere
            else
                let thread = Caller.thread caller

                match (Map.find thread state.ThreadState).Status with
                | ThreadStatus.Parked -> state, elsewhere
                | _ -> Scheduler.wakeFromSyscall thread state, elsewhere
        )
        |> fun (state, elsewhere) -> state, List.rev elsewhere

    /// The dispatcher's blocking `read(pipeFd, &signalCode, 1)`, if it is
    /// Parked there: made afresh if its task is not yet asleep in it, and
    /// finished once the kernel wakes it if it is. Then what the loop does with
    /// the signal it read, and with each after it, until it starts a callback,
    /// the read sleeps, or the process dies. Answers too the threads of other
    /// processes that a wake woke (see `wakeOwnThreads`), after `elsewhere`.
    ///
    /// Fails where the read would not be of the signal pipe: where the
    /// descriptor the loop was given no longer names the pipe's read end. If
    /// the guest closed it, the loop's read fails and the loop exits; if the
    /// guest put something else at its number, the loop reads that and takes
    /// its bytes for signal numbers. PawPrint models neither. A read already
    /// asleep holds the pipe's description, so it is finished from the pipe
    /// whatever the guest has done to the number since; only the read after it
    /// is refused.
    ///
    /// A read that sleeps leaves the dispatcher's task asleep in the kernel, as
    /// any thread's blocking read of a pipe does: the dispatcher holds the read
    /// end's description, and queues with any other reader of the pipe. It
    /// stays `Parked` rather than `BlockedInSyscall`, because it has no frame
    /// that made the call, and this finishes the read rather than
    /// `Scheduler.wakeFromSyscall`.
    let rec private wakeDispatcher
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (elsewhere : Caller<ThreadId> list)
        (state : IlMachineState)
        : SignalPoll * Caller<ThreadId> list
        =
        match idleDispatcher state with
        | None -> SignalPoll.Continues state, elsewhere
        | Some (dispatcher, pipe) ->

        let refuse (what : string) : 'a =
            failwith
                $"SignalDispatch.poll: System.Native's dispatcher reads descriptor %d{pipe.ReadEnd}, which it was given as its signal pipe's read end, and %s{what}; the real SignalHandlerLoop then closes the descriptor and its thread exits, which PawPrint does not model."

        let caller = state.Kernel.Caller dispatcher
        refuseOwedByOthers [ dispatcher ] state

        let read, state, elsewhere =
            try
                let system = state.Kernel.System

                match UnixThreadState.park (EmulatedKernel.taskOf dispatcher state.Kernel.Tasks) with
                // The loop reads the number it was given, so the read is of the
                // signal pipe only while that number names the pipe's read end.
                | None ->
                    match
                        FileDescriptorRegistry.tryFindWithId
                            pipe.ReadEnd
                            (UnixSystem.fileDescriptors state.Kernel.ProcessId system)
                    with
                    | Some (named, _) when named = pipe.ReadDescription ->
                        Some (UnixReadWrite.read caller pipe.ReadEnd UserBuffer.Mapped 1UL system), state, elsewhere
                    | Some (_, description) ->
                        failwith
                            $"SignalDispatch.poll: System.Native's dispatcher reads descriptor %d{pipe.ReadEnd}, which it was given as its signal pipe's read end, but which no longer names that pipe's read end: the guest has put %s{describeTarget description.Target} at that number. The real SignalHandlerLoop would read from that instead and take what it read for signal numbers, which PawPrint does not model."
                    | None -> refuse "which the guest has closed, so the read fails with EBADF"
                // Asleep in its read, which holds the description it was made
                // through: finished once the kernel wakes it.
                | Some {
                           Syscall = ParkedSyscall.PipeRead _
                           State = ParkState.Woken _
                       } -> Some (UnixReadWrite.finishRead caller system), state, elsewhere
                // Woken with every other sleeper, so that a reader that went to
                // sleep on the pipe first takes the byte first. Its own wake
                // condition is asked first, alone, because this runs between
                // every two instructions and that answer is almost always no.
                | Some {
                           Syscall = ParkedSyscall.PipeRead _
                           State = ParkState.Asleep
                       } ->
                    if
                        UnixWait.satisfied caller system
                        |> Option.exists (fun fired -> not (Set.isEmpty fired))
                    then
                        let woken, system = UnixWait.wake system

                        let state, others =
                            wakeOwnThreads woken (state.MapKernel (EmulatedKernel.withUnix system))

                        let elsewhere = elsewhere @ others

                        if woken |> List.exists (fun (woken, _) -> woken = caller) then
                            Some (UnixReadWrite.finishRead caller state.Kernel.System), state, elsewhere
                        else
                            None, state, elsewhere
                    else
                        None, state, elsewhere
                | Some other ->
                    failwith
                        $"SignalDispatch.poll: the dispatcher %O{dispatcher} is idle, but its task is asleep in %A{other.Syscall}; the loop makes no syscall but its read of the signal pipe (this is an interpreter bug)."
            with :? UnmodelledException as e ->
                KernelRefusal.fail
                    $"SignalDispatch.poll: System.Native's dispatcher reads descriptor %d{pipe.ReadEnd}, which it was given as its signal pipe's read end, and the kernel will not answer the read; the real SignalHandlerLoop then closes the descriptor and its thread exits, which PawPrint does not model"
                    e

        match read with
        | None -> SignalPoll.Continues state, elsewhere
        // Empty, with the write end open: the read sleeps, and so does the
        // dispatcher, until a poll finds the kernel has woken it.
        | Some (ReadOutcome.WouldBlock _, system) ->
            SignalPoll.Continues (state.MapKernel (EmulatedKernel.withUnix system)), elsewhere
        | Some (ReadOutcome.Interrupted, _) ->
            refuse "is interrupted by a signal, which PawPrint never directs at the dispatcher"
        | Some (ReadOutcome.Answered (ReadAnswer.Failed error), _) -> refuse $"fails with %O{error}"
        | Some (ReadOutcome.Answered (ReadAnswer.Drawn _), _) ->
            refuse "draws from the entropy pool, which a read of a pipe never does"
        | Some (ReadOutcome.Answered (ReadAnswer.Completed bytes), system) ->

        if bytes.Length <> 1 then
            refuse $"reads %d{bytes.Length} bytes rather than one"

        let state = state.MapKernel (EmulatedKernel.withUnix system)

        let numbering = SimulatedUnixPlatform.signalNumbering state.Kernel.UnixPlatform
        let signo = int bytes.[0]

        // The loop indexes its tables by `signalCode - 1` without checking it,
        // so a byte that is no signal number reached the pipe some other way
        // than the native handler: a guest writing to the pipe itself.
        if signo < 1 || signo > PosixSignalPal.signalMax numbering then
            refuse
                $"reads the byte %d{signo}, which is no signal number under the %O{numbering} numbering, and which the loop would use to index its tables out of bounds"

        // For SIGCHLD, SIGCONT and SIGWINCH the loop first calls the
        // console's terminal-invalidation callback, and for SIGCHLD the
        // process class's child-reaping callback, or reaps every child itself
        // if its saved disposition was SIG_IGN or it is process 1. PawPrint
        // implements neither `SystemNative_SetTerminalInvalidationHandler` nor
        // `SystemNative_RegisterForSigChld`, so neither callback is set, and
        // it models no child process for a `waitpid` to find.
        match Signal.ofRawSignoUnder numbering signo with
        | ValueSome signal when PosixSignalShim.isRegistered signal state.Kernel.PosixSignalShim ->
            SignalPoll.Continues (startCallback loggerFactory baseClassTypes dispatcher signal state), elsewhere
        | ValueSome _
        | ValueNone ->
            // Handled without guest code, and the loop goes straight back to
            // its read: so does this, rather than leaving the next byte for a
            // tick that may never come if every other thread is asleep.
            match handleNonCanceledOnDispatcher dispatcher signo state with
            | SignalPoll.Continues state -> wakeDispatcher loggerFactory baseClassTypes elsewhere state
            | killed -> killed, elsewhere

    /// System.Native's signal handling between two guest instructions: the
    /// native handler for whatever the kernel delivers to the leader now, and
    /// then the dispatcher, if it is idle and its pipe holds a signal. Polled
    /// once per tick by the driver's preamble (`MultiProgram.advance`), shortly
    /// before the scheduler picks its next thread, so a dispatcher it wakes can
    /// be picked on the same tick.
    ///
    /// The dispatcher's read is woken by `UnixWait.wake`, which wakes every
    /// sleeping thread on the machine that may go on: this program's are
    /// flipped back to `Runnable` here, and the threads of other processes it
    /// woke are answered, in the order the kernel woke them, for their own
    /// programs to flip.
    let pollMachine
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : SignalPoll * Caller<ThreadId> list
        =
        match deliverToLeader state with
        | SignalPoll.Continues state -> wakeDispatcher loggerFactory baseClassTypes [] state
        | killed -> killed, []

    /// `pollMachine` of a program alone on its machine, which no wake can find
    /// another process's thread on.
    ///
    /// Fails loudly if the wake woke a thread of another process.
    let poll
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : SignalPoll
        =
        match pollMachine loggerFactory baseClassTypes state with
        | polled, [] -> polled
        | _, elsewhere ->
            failwith
                $"SignalDispatch.poll: the dispatcher's wake woke %O{elsewhere}, threads of another process, which only that process's program can wake; poll a machine of several programs through `pollMachine`."

    /// Whether `poll` may do anything to `state` on the machine `system`, which holds its
    /// process: a return to user mode one of its threads owes (the leader's to make, any other's
    /// to refuse), or a dispatcher idle in its loop that has
    /// not yet made its read of the signal pipe, or whose read the kernel has woken. Reads the
    /// process from `system` rather than from `state`'s kernel, so that the driver can ask it of
    /// a program whose copy of the machine is stale and hand the machine over only to a program
    /// whose poll may act.
    ///
    /// A dispatcher asleep in its read is not counted, though a write to the pipe would wake
    /// it: the process's own shim made the pipe, and nothing passes a descriptor to another
    /// process, so only the process's own steps and polls write it, and the poll that follows
    /// each of those runs while the process is still the one checked out.
    let mayAct (system : UnixSystem<ThreadId, NativeSignalHandler>) (state : IlMachineState) : bool =
        UnixSystem.threads state.Kernel.ProcessId system
        |> Map.exists (fun thread _ -> UnixSignal.owesReturn (state.Kernel.Caller thread) system)
        || match idleDispatcher state with
           | None -> false
           | Some (dispatcher, _) ->
               match
                   UnixThreadState.park (
                       EmulatedKernel.taskOf dispatcher (UnixSystem.threads state.Kernel.ProcessId system)
                   )
               with
               | Some {
                          Syscall = ParkedSyscall.PipeRead _
                          State = ParkState.Asleep
                      } -> false
               | Some _
               | None -> true

    /// Called from `RunningProgram.stepDecided` when `ExecutionResult.Terminated`
    /// fires for the dispatcher's bottom frame (the callback `ret`urned past
    /// its own frame), with the callback's `int` result still on the
    /// dispatcher's evaluation stack. Resets the dispatcher to its idle shape
    /// (Parked + sentinel frame id + no live frames) so it reads the pipe
    /// again.
    ///
    /// A result of 0 is the callback reporting that nothing handled the
    /// signal (`OnPosixSignal` found no registration for it, which happens
    /// when the last one is disposed after the signal was written to the
    /// pipe): the loop then calls `SystemNative_HandleNonCanceledPosixSignal`
    /// itself, before it reads again.
    let reParkAfterHandler (dispatcher : ThreadId) (state : IlMachineState) : SignalPoll =
        let handled =
            match IlMachineState.peekEvalStack dispatcher state with
            | Some (EvalStackValue.Int32 (Int32Source.Verbatim result)) -> result <> 0
            | other ->
                failwith
                    $"SignalDispatch.reParkAfterHandler: expected the signal handler's Int32 result on dispatcher %O{dispatcher}'s evaluation stack, found %O{other}."

        let signo, shim = PosixSignalShim.endCallback state.Kernel.PosixSignalShim

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    PosixSignalShim = shim
                }
            )
            |> IlMachineState.reParkDispatcher dispatcher

        if handled then
            SignalPoll.Continues state
        else
            handleNonCanceledOnDispatcher dispatcher signo state
