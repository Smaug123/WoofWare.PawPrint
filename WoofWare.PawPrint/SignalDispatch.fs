namespace WoofWare.PawPrint

open System.Collections.Immutable
open WoofWare.PosixKernel

/// What the signal half of a scheduler tick did to the process.
[<RequireQualifiedAccess>]
type SignalPoll =
    /// The process carries on, in this state.
    | Continues of IlMachineState
    /// A signal killed the process: one System.Native re-raised at its
    /// default, or the SIGABRT of an abort a native handler called. The state
    /// is the machine as it stood then.
    | ProcessKilled of IlMachineState * signal : Signal * coreDumped : bool

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
///   * If the dispatcher is Parked, it is blocked reading the pipe: when the
///     pipe holds a byte, it reads one. For a signal with a registration it
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
/// The `SignalDelivery.Default*` cases are refused loudly: a default that
/// terminates or stops is applied when the signal is generated (see
/// `NativeLibc.kill` and `NativeLibc.raiseSignal`), so one reaches this poll
/// only by becoming receivable later, as a handler frame's mask is popped, and
/// no frame survives a poll.
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
    /// `TryConvertSignalCodeToPosixSignal` returns `false`). Both are read
    /// under the same numbering, so an entry spelled `Signal.Other 19` in a
    /// Darwin process is handed to the handler as `(19, PosixSignal.SIGCONT)`,
    /// exactly as the entry spelled `Signal.SIGCONT` is.
    let private buildArgs (numbering : SignalNumbering) (signal : Signal) : ImmutableArray<CliType> =
        let signo = Signal.toRawSignoUnder numbering signal
        let posixEnum = PosixSignalPal.toEnum numbering signal

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
                    (Signal.toRawSignoUnder numbering signal)
                    (Some SignalDisposition.Default)
                    (EmulatedKernel.unix state.Kernel)
            with
            | Ok (_, system) -> state.MapKernel (EmulatedKernel.withUnix system) |> SignalPoll.Continues
            | Error errno ->
                failwith
                    $"SignalDispatch.poll: the PAL's restore of %O{signal}'s default was refused (%O{errno}), but the PAL catches only signals sigaction accepts."
        | PalReplacedDisposition.Ignore ->
            // `abort` unblocks SIGABRT before raising it, which `frame`'s mask
            // holds if `frame` is SIGABRT's own; returning through the frame
            // first unblocks it the same way, and nothing else runs before the
            // process dies.
            let state =
                state.MapKernel (fun kernel ->
                    EmulatedKernel.withUnix
                        (UnixSignal.sigreturn kernel.Leader frame.Id (EmulatedKernel.unix kernel))
                        kernel
                )

            match EmulatedKernel.abort state.Kernel.Leader state.Kernel with
            | ProcessTermination.Signaled (killedBy, coreDumped) ->
                SignalPoll.ProcessKilled (state, killedBy, coreDumped)
            | ProcessTermination.Exited _ as other ->
                failwith $"SignalDispatch.poll: the PAL's abort for %O{signal} ended the process by %O{other}"

    /// System.Native's native handler for `frame`'s signal, run on the leader:
    /// write the signal's number into the shim's pipe, having first run the
    /// handler it replaced (see `PosixSignalShim.chainsToNativeHandler`).
    ///
    /// Fails where the real handler would do more than that: where the
    /// handler it replaced is not the PAL's hardware-fault handler, and where
    /// its `write` would not take the byte. The handler retries only `EINTR`
    /// and calls `abort()` on any other failure, and a write into a full pipe
    /// blocks the leader until the dispatcher makes room.
    let private runNativeHandler
        (frame : HandlerFrame<ThreadId, NativeSignalHandler>)
        (state : IlMachineState)
        : SignalPoll
        =
        let signal = frame.Entry.Signal
        let numbering = SimulatedUnixPlatform.signalNumbering state.Kernel.UnixPlatform
        let shim = state.Kernel.PosixSignalShim

        let chained =
            match PosixSignalShim.chainsToNativeHandler numbering signal shim with
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

        let system = EmulatedKernel.unix state.Kernel
        let bytes = ImmutableArray.Create (signalByte numbering signal)

        let refuse (what : string) : 'a =
            failwith
                $"SignalDispatch.poll: System.Native's handler for %O{signal} writes to descriptor %d{pipe.WriteEnd}, the write end of its signal pipe, and %s{what}; the real handler abort()s the process, or blocks until the dispatcher reads, and PawPrint models neither."

        // The shim writes to the number it was given, whatever the guest has
        // since put there. A byte written to a pipe PawPrint drains, one of the
        // standard output streams, would reach the guest's output without the
        // step effect that streams it, so anything but the write end of a pipe
        // the process made is refused rather than half-answered.
        match FileDescriptorRegistry.tryFindTarget pipe.WriteEnd system.Process.FileDescriptors with
        | None -> ()
        | Some (OpenFileTarget.Pipe (pipeId, PipeEnd.Write)) when
            (PipeState.drainedBy (UnixMachineState.pipe pipeId system.Machine)).IsNone
            ->
            ()
        | Some other ->
            failwith
                $"SignalDispatch.poll: System.Native's handler for %O{signal} writes to descriptor %d{pipe.WriteEnd}, which the guest has replaced with %O{other}; PawPrint models the handler writing only to a pipe."

        // The handler runs on the leader, so the leader makes the write.
        let leader = state.Kernel.Leader

        let describe (outcome : WriteOutcome<'Answer, ThreadId, NativeSignalHandler>) : string =
            match outcome with
            | WriteOutcome.Returns (answer, _) -> $"%O{answer}"
            | WriteOutcome.ReturnsRaising (answer, raised, _) -> $"%O{answer}, raising %O{raised.Signal}"
            | WriteOutcome.ProcessEnded ended -> $"the end of the process (%O{ended.Termination})"
            | WriteOutcome.WouldBlock _ -> "that the write sleeps, the pipe being full"
            | WriteOutcome.Restarts _ -> "a restart"

        match UnixReadWrite.admitWrite leader pipe.WriteEnd UserBuffer.Mapped 1UL system with
        | Error refusal -> refuse (WriteRefusal.describe refusal)
        | Ok (WriteOutcome.Returns (WriteAdmission.Transfer _, system)) ->

            match UnixReadWrite.write leader pipe.WriteEnd bytes system with
            | Error refusal -> refuse (WriteRefusal.describe refusal)
            | Ok (WriteOutcome.Returns (WriteAnswer.Completed 1L, system)) ->
                state.MapKernel (EmulatedKernel.withUnix system) |> SignalPoll.Continues
            | Ok outcome -> refuse $"the kernel answers %s{describe outcome}"
        | Ok outcome -> refuse $"the kernel answers %s{describe outcome} without taking the byte"

    /// The leader's return to user mode: whatever the kernel delivers to it now,
    /// each through its disposition. A handler frame's handler runs, innermost
    /// first, and each returns through `sigreturn`, after which the leader
    /// returns to user mode again and may take more. System.Native's handler
    /// writes its signal into the pipe, and the PAL's hardware-fault handler
    /// restores the default or aborts the process (see `runPalFaultHandler`);
    /// every other handler is refused.
    ///
    /// A leader asleep in a syscall is not in user mode, and is not asked. A
    /// signal it would take wakes it instead (`WakePrimitive.SignalDeliverable`),
    /// the syscall's finishing call answers `EINTR` or a restart, and the shim
    /// function that made it calls it again (see `NativeSystemNative`), leaving
    /// the leader in user mode for the next poll. A leader in one of PawPrint's
    /// own waits (a monitor, `Sleep`, a wait handle) is asked: CoreCLR's PAL
    /// carries those waits on across a signal handler.
    let private deliverToLeader (state : IlMachineState) : SignalPoll =
        let leader = state.Kernel.Leader

        match PosixSignalShim.signalThread state.Kernel.PosixSignalShim with
        | Some dispatcher when dispatcher = leader ->
            failwith
                $"SignalDispatch.poll: the dispatcher %O{dispatcher} is the process's leader, which the kernel delivers the process's signals to; the dispatcher runs handlers for the leader and is always a thread of its own."
        | _ -> ()

        // Nothing pending is nothing to deliver, and this runs between every
        // two instructions, so it answers without assembling the kernel's view.
        // No frame outlives a poll, so none is waiting for a sigreturn either.
        if List.isEmpty (SignalState.pending state.Kernel.Signals) then
            SignalPoll.Continues state
        elif (UnixTaskTable.parkedFor leader state.Kernel.Tasks).IsSome then
            SignalPoll.Continues state
        else

        let returnToUser
            (state : IlMachineState)
            : SignalDelivery<ThreadId, NativeSignalHandler> option * IlMachineState
            =
            let delivery, systemAfter =
                match UnixSignal.onReturnToUser leader (EmulatedKernel.unix state.Kernel) with
                | Ok answer -> answer
                | Error refusal ->
                    failwith $"SignalDispatch.poll: the kernel will not say what the leader takes: %O{refusal}"

            // Persist the walk's state whether or not it produced an action:
            // discarding a receivable ignored signal is a state change with no
            // delivery, and dropping it would replay the discard every tick.
            let state =
                if systemAfter.Process.Signals = state.Kernel.Signals then
                    state
                else
                    state.MapKernel (EmulatedKernel.withUnix systemAfter)

            delivery, state

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
                    EmulatedKernel.withUnix (UnixSignal.sigreturn leader frame.Id (EmulatedKernel.unix kernel)) kernel
                )

            state |> returnToUserThen (runFrames outer)

        and returnToUserThen (continuation : IlMachineState -> SignalPoll) (state : IlMachineState) : SignalPoll =
            match returnToUser state with
            | None, state -> continuation state
            | Some (SignalDelivery.RunHandlers frames), state ->
                match runFrames frames state with
                | SignalPoll.Continues state -> continuation state
                | killed -> killed
            | Some (SignalDelivery.DefaultTerminate (signal, _)), _
            | Some (SignalDelivery.DefaultStop signal), _
            | Some (SignalDelivery.DefaultContinue signal), _ ->
                // A pending signal at its default disposition, whose kernel
                // default is to terminate, stop or continue the process.
                // `SignalState.generate` applies a terminating or stopping
                // default at generation whenever some thread can receive the
                // signal, so it is pending here only if none could then, which
                // a mask held only by handler frames never arranges between
                // instructions. Reaching this is therefore a test driving the
                // queue by hand, and it is refused rather than half-modelled.
                failwith
                    $"SignalDispatch.poll: pending %O{signal} is at its default disposition, and its kernel default is not Ignore; applying a default disposition at delivery rather than at generation is not modelled."

        returnToUserThen SignalPoll.Continues state

    /// Start the managed callback for `signal` on the parked dispatcher: the
    /// shim's `g_posixSignalHandler(signo, posixSignal)`.
    ///
    /// Fails if no callback was installed by `SystemNative_SetPosixSignalHandler`:
    /// the shim asserts there is one, and the BCL installs it before it enables
    /// any signal.
    let private startCallback
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
                signo
                state
        with
        | NonCanceledPosixSignal.Continues state -> SignalPoll.Continues state
        | NonCanceledPosixSignal.ContinuesWithErrno (state, error) ->
            let numbering = SimulatedUnixPlatform.rawErrnoNumbering state.Kernel.UnixPlatform

            state.MapKernel (EmulatedKernel.withLastSystemError dispatcher (UnixError.toRawErrnoUnder numbering error))
            |> SignalPoll.Continues
        | NonCanceledPosixSignal.Terminated (state, signal, coreDumped) ->
            SignalPoll.ProcessKilled (state, signal, coreDumped)

    /// The dispatcher's blocking `read(pipeFd, &signalCode, 1)`, if it is
    /// Parked there and the pipe has a byte for it: what the loop then does
    /// with the signal, and with each after it, until it starts a callback,
    /// the pipe is empty, or the process dies.
    let rec private wakeDispatcher
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : SignalPoll
        =
        match
            PosixSignalShim.signalThread state.Kernel.PosixSignalShim,
            PosixSignalShim.signalPipe state.Kernel.PosixSignalShim
        with
        | None, _
        | _, None -> SignalPoll.Continues state
        | Some dispatcher, Some pipe ->

        let dispatcherStatus =
            match Map.tryFind dispatcher state.ThreadState with
            | Some ts -> ts.Status
            | None ->
                failwith
                    $"SignalDispatch.poll: dispatcher thread %O{dispatcher} recorded in PosixSignalShim but no ThreadState entry exists — the initialisation path should always allocate both."

        // Runnable: the dispatcher is running the callback for an earlier
        // signal, and reads the next one only once that returns, exactly as
        // the single-threaded `SignalHandlerLoop` does.
        if dispatcherStatus <> ThreadStatus.Parked then
            SignalPoll.Continues state
        else

        let refuse (what : string) : 'a =
            failwith
                $"SignalDispatch.poll: System.Native's dispatcher reads descriptor %d{pipe.ReadEnd}, the read end of its signal pipe, and %s{what}; the real SignalHandlerLoop then closes the descriptor and its thread exits, which PawPrint does not model."

        // The loop reads the number it was given, whatever the guest has since
        // put there; PawPrint models it reading only a pipe.
        match FileDescriptorRegistry.tryFindTarget pipe.ReadEnd state.Kernel.Process.FileDescriptors with
        // An empty pipe with its write end open, read through a blocking
        // description: the read sleeps, and so does the dispatcher. Answered
        // here because this runs between every two instructions of a process
        // that has initialised signal handling.
        | Some (OpenFileTarget.Pipe (pipeId, PipeEnd.Read)) when
            PipeBuffer.held (UnixMachineState.pipe pipeId state.Kernel.Machine).Buffer = 0
            && FileDescriptorRegistry.tryFind pipe.ReadEnd state.Kernel.Process.FileDescriptors
               |> Option.exists (fun description -> not description.NonBlocking)
            && FileDescriptorRegistry.tryFindTarget pipe.WriteEnd state.Kernel.Process.FileDescriptors = Some (
                OpenFileTarget.Pipe (pipeId, PipeEnd.Write)
            )
            ->
            SignalPoll.Continues state
        | Some (OpenFileTarget.File _ as other)
        | Some (OpenFileTarget.Directory _ as other)
        | Some (OpenFileTarget.SocketEventPort _ as other)
        | Some (OpenFileTarget.Socket _ as other)
        | Some (OpenFileTarget.Pipe (_, PipeEnd.Write) as other) -> refuse $"the guest has replaced it with %O{other}"
        | None
        | Some (OpenFileTarget.Pipe (_, PipeEnd.Read)) ->

        match UnixReadWrite.read dispatcher pipe.ReadEnd UserBuffer.Mapped 1UL (EmulatedKernel.unix state.Kernel) with
        // Empty, with the write end open: the read sleeps, and so does the
        // dispatcher. The loop is PawPrint's own thread rather than one the
        // kernel parks, so the park is not kept: this runs again between the
        // next two instructions.
        | Ok (ReadOutcome.WouldBlock _, _) -> SignalPoll.Continues state
        | Ok (ReadOutcome.Restarts, _) -> refuse "restarts, which only a read that slept can"
        | Error refusal -> refuse (ReadRefusal.describe refusal)
        | Ok (ReadOutcome.Answered (ReadAnswer.Failed error), _) -> refuse $"fails with %O{error}"
        | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), system) ->

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
        | ValueSome signal when PosixSignalShim.isRegistered numbering signal state.Kernel.PosixSignalShim ->
            SignalPoll.Continues (startCallback baseClassTypes dispatcher signal state)
        | ValueSome _
        | ValueNone ->
            // Handled without guest code, and the loop goes straight back to
            // its read: so does this, rather than leaving the next byte for a
            // tick that may never come if every other thread is asleep.
            match handleNonCanceledOnDispatcher dispatcher signo state with
            | SignalPoll.Continues state -> wakeDispatcher baseClassTypes state
            | killed -> killed

    /// System.Native's signal handling between two guest instructions: the
    /// native handler for whatever the kernel delivers to the leader now, and
    /// then the dispatcher, if it is idle and its pipe holds a signal. Polled
    /// once per tick by `Program.stepPrepared`, immediately before the
    /// scheduler picks its next thread, so a dispatcher it wakes can be picked
    /// on the same tick.
    let poll (baseClassTypes : BaseClassTypes<DumpedAssembly>) (state : IlMachineState) : SignalPoll =
        match deliverToLeader state with
        | SignalPoll.Continues state -> wakeDispatcher baseClassTypes state
        | killed -> killed

    /// Called from `Program.stepPrepared` when `ExecutionResult.Terminated`
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
