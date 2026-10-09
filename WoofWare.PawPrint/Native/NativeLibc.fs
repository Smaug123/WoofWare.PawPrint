namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// Why PawPrint will not send a signal to its own process, although the kernel
/// model has an answer: in each case a real CoreCLR process does something the
/// model cannot express, so the model's answer would be wrong.
[<RequireQualifiedAccess>]
type UnmodelledSelfSignal =
    /// Delivering the signal would run native code PawPrint does not model:
    /// `handler` is the signal's disposition, or the handler System.Native's
    /// own runs first (see `PosixSignalShim.chainsToNativeHandler`).
    | NativeHandler of Signal * handler : NativeSignalHandler
    /// A hardware-fault signal sent to the process by a thread other than the
    /// process's main thread, which is the thread that receives it. If the
    /// main thread is running managed code then, the runtime's fault handler
    /// can raise a managed exception there rather than restore the default;
    /// PawPrint does not model which code the main thread is running.
    | FaultSignalFromOtherThread of Signal
    /// A hardware-fault signal whose handler, run, would restore the default,
    /// on a platform where the same handler is how a later hardware fault in
    /// managed code (a null dereference, say) becomes a managed exception:
    /// once the default is back, such a fault kills the process instead, and
    /// PawPrint cannot tell which faults the JIT leaves to the hardware.
    | FaultHandlerNeededLater of Signal
    /// A signal whose kernel default is to continue a stopped process, with no
    /// handler registered. A real process, never stopped, carries on; the model
    /// has no stopped state for it to resume, and would leave it pending for a
    /// dispatcher that refuses it.
    | ContinueWithoutHandler of Signal
    /// A signal raised at the thread that made the call, which is not the main
    /// thread, and which stays pending there because the process catches it:
    /// `raise(3)` aims its signal at the caller, and Linux sends the SIGPIPE of
    /// a write into a pipe with no reader to the writer. PawPrint delivers
    /// signals to the main thread only.
    | PendingOnOtherThread of Signal

[<RequireQualifiedAccess>]
module UnmodelledSelfSignal =
    /// Why the model's answer would be wrong, for a diagnostic.
    let describe (refusal : UnmodelledSelfSignal) : string =
        match refusal with
        | UnmodelledSelfSignal.NativeHandler (signal, handler) ->
            let whose =
                match handler with
                | NativeSignalHandler.CoreClrPalFault _ -> "CoreCLR's PAL's hardware-fault handler"
                | NativeSignalHandler.CoreClrPalTrap -> "CoreCLR's PAL's SIGTRAP handler"
                | NativeSignalHandler.CoreClrPalActivation -> "CoreCLR's PAL's thread-activation handler"
                | NativeSignalHandler.GlibcSetXid -> "glibc's own SIGSETXID handler"
                | NativeSignalHandler.SystemNative -> "System.Native's handler"

            $"a real CoreCLR process runs %s{whose} for %O{signal}, which PawPrint does not model (usually the process survives the signal; on x86-64 Linux, SIGTRAP kills it with SIGILL instead)."
        | UnmodelledSelfSignal.FaultSignalFromOtherThread signal ->
            $"%O{signal} is sent by a thread other than the main thread, which receives it; a real CoreCLR process's fault handler raises a managed exception on the main thread if that thread is running managed code, and PawPrint does not model which code it is running."
        | UnmodelledSelfSignal.FaultHandlerNeededLater signal ->
            $"a real CoreCLR process's handler for %O{signal} would restore the default, and on this platform that handler is also how a later hardware fault in managed code becomes a managed exception; afterwards such a fault kills the process, and PawPrint cannot tell which faults the JIT leaves to the hardware."
        | UnmodelledSelfSignal.ContinueWithoutHandler signal ->
            $"%O{signal} with no handler registered continues a stopped process, and a running one carries on regardless; PawPrint has no stopped state, and would leave the signal pending for a dispatcher that refuses it."
        | UnmodelledSelfSignal.PendingOnOtherThread signal ->
            $"%O{signal} was raised at the thread that made the call, which is not the main thread, and the process catches it, so it is pending there; PawPrint delivers signals to the main thread only (SignalDispatch)."

/// The C library's signal entry points, `kill(2)` and `raise(3)`, which a
/// guest reaches only through a P/Invoke of its own naming the library
/// `libc`: the BCL calls neither directly. The runtime resolves that name to the platform's C library on
/// Linux and Darwin alike (`pal_dynamicload.c`, `LIBC_SO`). The file entry
/// points a CoreLib does call this way are `NativeLibcFile`'s.
[<RequireQualifiedAccess>]
module NativeLibc =

    /// Whether, on `platform`, CoreCLR's handler for the hardware-fault signal
    /// `signal` is also how a hardware fault in managed code becomes a managed
    /// exception, so that once it has restored the default, such a fault
    /// kills the process instead.
    let private faultHandlerNeededLater (platform : SimulatedUnixPlatform) (signal : Signal) : bool =
        // Measured 2026-10-01 by sending each signal to the process and then
        // dereferencing null in a non-inlined field read, and dividing by a
        // zero the JIT cannot see: .NET 10.0.7 on Darwin 27.0.0 arm64, and
        // .NET 10.0.12 on Linux 6.18.5 aarch64 and under Rosetta x86-64. On
        // Linux, after SIGSEGV the null dereference killed the process with
        // SIGSEGV where it had raised NullReferenceException, on both CPUs;
        // and on x86-64, after SIGFPE the division killed it with SIGFPE.
        // arm64's JIT checks a divisor itself, so its division still raised
        // DivideByZeroException. Darwin takes faults as Mach exceptions, and
        // raised both exceptions after every signal. SIGILL and SIGABRT
        // changed neither on any platform. Linux's SIGBUS was not measured
        // (nothing simple raises it), but its handler hands a bus error to the
        // same managed-exception path as a segmentation fault's
        // (pal/src/exception/signal.cpp, sigbus_handler and sigsegv_handler,
        // each calling common_signal_handler), so it is refused too.
        match SimulatedUnixPlatform.flavour platform, signal with
        | SimulatedUnixFlavour.Darwin, _ -> false
        | SimulatedUnixFlavour.Linux, Signal.SIGSEGV
        | SimulatedUnixFlavour.Linux, Signal.SIGBUS -> true
        | SimulatedUnixFlavour.Linux, Signal.SIGFPE ->
            match SimulatedUnixPlatform.architecture platform with
            | SimulatedUnixArchitecture.X64 -> true
            | SimulatedUnixArchitecture.Arm64 -> false
        | SimulatedUnixFlavour.Linux, _ -> false

    /// Whether PawPrint's kernel model can answer a signal that `sender` sends
    /// within its own process, on `platform`, given the system and
    /// System.Native's state before it is sent. `None` if it can. `receiver` is
    /// the thread that takes it: the process's main thread for a signal sent
    /// to the process, and the target of one sent to a thread.
    ///
    /// Asked only of a signal that is actually being sent: the null signal, an
    /// invalid number and another target are all answered or refused by the
    /// kernel model before this matters.
    let screenSelfSignal<'Task when 'Task : comparison>
        (platform : SimulatedUnixPlatform)
        (sender : 'Task)
        (receiver : 'Task)
        (shim : PosixSignalShim)
        (system : UnixSystem<'Task, NativeSignalHandler>)
        (signal : Signal)
        : UnmodelledSelfSignal option
        =
        let numbering = SimulatedUnixPlatform.signalNumbering platform

        // The PAL's fault handler, run for a signal a thread sent and takes
        // itself, interrupts its `kill` or `raise` in libc, so it finds no
        // managed code to raise an exception in, and restores what it replaced
        // (see `NativeSignalHandler.CoreClrPalFault`). Over an ignore it aborts
        // the process there and then, so no later fault meets the default.
        let faultHandler (replaced : PalReplacedDisposition) : UnmodelledSelfSignal option =
            if sender <> receiver then
                Some (UnmodelledSelfSignal.FaultSignalFromOtherThread signal)
            else
                match replaced with
                | PalReplacedDisposition.Ignore -> None
                | PalReplacedDisposition.Default ->
                    if faultHandlerNeededLater platform signal then
                        Some (UnmodelledSelfSignal.FaultHandlerNeededLater signal)
                    else
                        None

        // Asked of the kernel rather than through the C library, which will not
        // report Linux's 33, whose handler is glibc's own.
        match UnixSignal.sigactionSyscall (Signal.toRawSignoUnder numbering signal) None system with
        // Darwin will not report SIGKILL or SIGSTOP, which nothing can catch,
        // so no native code runs for either.
        | Error _ -> None
        | Ok (disposition, _) ->

        match disposition with
        | SignalDisposition.Catch {
                                      Handler = NativeSignalHandler.SystemNative
                                  } ->
            match PosixSignalShim.chainsToNativeHandler signal shim with
            | None -> None
            | Some (NativeSignalHandler.CoreClrPalFault replaced) -> faultHandler replaced
            | Some chained ->
                // Registering a handler does not take the runtime's own away
                // (pal_signal.c): `InstallSignalHandler` keeps the handler it
                // replaces, and the shim's handler calls it before anything
                // reaches managed code.
                Some (UnmodelledSelfSignal.NativeHandler (signal, chained))
        | SignalDisposition.Catch {
                                      Handler = NativeSignalHandler.CoreClrPalFault replaced
                                  } -> faultHandler replaced
        | SignalDisposition.Catch action -> Some (UnmodelledSelfSignal.NativeHandler (signal, action.Handler))
        | SignalDisposition.Default when Signal.defaultDispositionUnder numbering signal = DefaultDisposition.Continue ->
            Some (UnmodelledSelfSignal.ContinueWithoutHandler signal)
        | SignalDisposition.Default
        | SignalDisposition.Ignore -> None

    /// Whether PawPrint's kernel model can carry on after a syscall made by
    /// `sender` raised `raised`, given the system and System.Native's state
    /// before the call, and `after`, the system once the kernel generated it.
    /// `None` if it can. `leader` is the process's main thread.
    ///
    /// The signal is taken by the thread it is pending on, or by `leader` if
    /// it is pending on the process, and screened as `screenSelfSignal` screens
    /// a signal that thread takes; and one left pending on a thread other than
    /// `leader`, which nothing delivers, is refused.
    let screenRaisedSignal<'Task when 'Task : comparison>
        (platform : SimulatedUnixPlatform)
        (sender : 'Task)
        (leader : 'Task)
        (shim : PosixSignalShim)
        (before : UnixSystem<'Task, NativeSignalHandler>)
        (raised : PendingSignal<'Task>)
        (after : UnixSystem<'Task, NativeSignalHandler>)
        : UnmodelledSelfSignal option
        =
        let receiver =
            match raised.Target with
            | ValueSome target -> target
            | ValueNone -> leader

        match screenSelfSignal platform sender receiver shim before raised.Signal with
        | Some refusal -> Some refusal
        | None ->

        match raised.Target with
        | ValueSome target when
            target <> leader
            && List.contains raised (SignalState.pending (UnixSystem.signals after))
            ->
            Some (UnmodelledSelfSignal.PendingOnOtherThread raised.Signal)
        | ValueSome _
        | ValueNone -> None

    /// `kill(2)`, issued by the thread `ctx` is executing, pushing its `int`
    /// result: 0, or -1 with errno set.
    ///
    /// A signal whose kernel default ends the process ends the run here, with
    /// the call never returning. A target other than the calling process, and
    /// anything `screenSelfSignal` refuses, fail the run: the model has no
    /// answer to give.
    let kill (operation : string) (ctx : NativeCallContext) (pid : int) (signo : int) : NativeHandlerResult =
        let state = ctx.State
        let system = state.Kernel.System

        let returning (value : int) (state : IlMachineState) : NativeHandlerResult =
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 value)) ctx.Thread
            |> NativeHandlerResult.completed

        match UnixSignal.kill pid signo system with
        | Error refusal ->
            failwith
                $"%s{operation}: kill(%d{pid}, %d{signo}) from process %O{UnixSystem.processId system} is not modelled (%O{refusal}); only a signal to the calling process itself is."
        | Ok (Error errno) ->
            let numbering = SimulatedUnixPlatform.rawErrnoNumbering state.Kernel.UnixPlatform

            state.MapKernel (EmulatedKernel.withLastSystemError ctx.Thread (UnixError.toRawErrnoUnder numbering errno))
            |> returning -1
        | Ok (Ok outcome) ->

        let refusal =
            Signal.ofRawSignoUnder (SimulatedUnixPlatform.signalNumbering state.Kernel.UnixPlatform) signo
            |> ValueOption.bind (fun sent ->
                screenSelfSignal
                    state.Kernel.UnixPlatform
                    ctx.Thread
                    state.Kernel.Leader
                    state.Kernel.PosixSignalShim
                    system
                    sent
                |> ValueOption.ofOption
            )

        match refusal with
        | ValueSome refusal ->
            failwith
                $"%s{operation}: kill(%d{pid}, %d{signo}) is not modelled: %s{UnmodelledSelfSignal.describe refusal}"
        | ValueNone ->

        match outcome with
        | KillOutcome.ProcessContinues after -> state.MapKernel (EmulatedKernel.withUnix after) |> returning 0
        | KillOutcome.ProcessEnded ended ->
            // The process never returns from this call.
            match ended.Termination with
            | ProcessTermination.Signaled _ ->
                ExecutionResult.SignalTerminated (state, ended)
                |> NativeHandlerResult.ofExecutionResult
            | ProcessTermination.Exited _ ->
                failwith
                    $"%s{operation}: kill(%d{pid}, %d{signo}) ended the process with an exit status (%O{ended.Termination}), which only an exit can"
        | KillOutcome.ProcessStopped (signal, _) ->
            failwith
                $"%s{operation}: %O{signal} would stop the whole process, and PawPrint does not model a stopped process (nothing could continue it)."

    /// `raise(3)`, issued by the thread `ctx` is executing, pushing its `int`
    /// result: 0, or -1 with errno set.
    ///
    /// The signal is aimed at the calling thread, which takes it before the
    /// call returns. A signal whose kernel default ends the process ends the
    /// run here, with the call never returning. Anything `screenRaisedSignal`
    /// refuses fails the run: the model has no answer to give.
    let raiseSignal (operation : string) (ctx : NativeCallContext) (signo : int) : NativeHandlerResult =
        let state = ctx.State
        let system = state.Kernel.System

        let returning (value : int) (state : IlMachineState) : NativeHandlerResult =
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 value)) ctx.Thread
            |> NativeHandlerResult.completed

        match UnixSignal.pthreadKill ctx.Thread signo system with
        | Error refusal ->
            failwith
                $"%s{operation}: raise(%d{signo}) on %O{ctx.Thread} in process %O{UnixSystem.processId system} is not modelled (%O{refusal})."
        | Ok (Error errno) ->
            let numbering = SimulatedUnixPlatform.rawErrnoNumbering state.Kernel.UnixPlatform

            state.MapKernel (EmulatedKernel.withLastSystemError ctx.Thread (UnixError.toRawErrnoUnder numbering errno))
            |> returning -1
        | Ok (Ok outcome) ->

        let refusal =
            match
                Signal.ofRawSignoUnder (SimulatedUnixPlatform.signalNumbering state.Kernel.UnixPlatform) signo, outcome
            with
            | ValueSome sent, KillOutcome.ProcessContinues after ->
                screenRaisedSignal
                    state.Kernel.UnixPlatform
                    ctx.Thread
                    state.Kernel.Leader
                    state.Kernel.PosixSignalShim
                    system
                    {
                        Signal = sent
                        Target = ValueSome ctx.Thread
                    }
                    after
            | ValueSome sent, _ ->
                screenSelfSignal
                    state.Kernel.UnixPlatform
                    ctx.Thread
                    ctx.Thread
                    state.Kernel.PosixSignalShim
                    system
                    sent
            | ValueNone, _ -> None

        match refusal with
        | Some refusal ->
            failwith $"%s{operation}: raise(%d{signo}) is not modelled: %s{UnmodelledSelfSignal.describe refusal}"
        | None ->

        match outcome with
        | KillOutcome.ProcessContinues after -> state.MapKernel (EmulatedKernel.withUnix after) |> returning 0
        | KillOutcome.ProcessEnded ended ->
            // The process never returns from this call.
            match ended.Termination with
            | ProcessTermination.Signaled _ ->
                ExecutionResult.SignalTerminated (state, ended)
                |> NativeHandlerResult.ofExecutionResult
            | ProcessTermination.Exited _ ->
                failwith
                    $"%s{operation}: raise(%d{signo}) ended the process with an exit status (%O{ended.Termination}), which only an exit can"
        | KillOutcome.ProcessStopped (signal, _) ->
            failwith
                $"%s{operation}: %O{signal} would stop the whole process, and PawPrint does not model a stopped process (nothing could continue it)."

    /// What `gettid(2)` answers on `flavour` for a thread whose task's OS thread
    /// id is `id`: the id, as the `pid_t` it is on Linux. `None` on Darwin, whose
    /// C library exports no `gettid`, so a P/Invoke of it never binds: measured on
    /// Darwin 27.0.0 by `docs/plans/2026-08-23-posix-kernel-extraction/gettid-symbol.c`.
    let gettid (flavour : SimulatedUnixFlavour) (id : OsThreadId) : int32 option =
        match flavour with
        | SimulatedUnixFlavour.Darwin -> None
        | SimulatedUnixFlavour.Linux ->
            let id = OsThreadId.toUInt64 id

            // A Linux tid is below the greatest `pid_max` Linux takes, 2^22.
            if id > uint64 System.Int32.MaxValue then
                failwith $"libc gettid: the kernel minted the Linux thread ID %d{id}, which is not a pid_t"

            Some (int32 id)

    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        let entryPoint =
            match instruction.ExecutingMethod.TryNativeImport with
            | Some import when import.ModuleName = "libc" -> Some import.EntryPointName
            | _ -> None

        match
            entryPoint,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | Some "kill",
          [ ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32) ->
            // `int kill(pid_t pid, int sig)`: `pid_t` is a 32-bit int on both
            // flavours.
            let operation = "libc kill"
            let pid = NativeCall.int32Argument operation instruction.Arguments.[0]
            let signo = NativeCall.int32Argument operation instruction.Arguments.[1]
            kill operation ctx pid signo |> Some
        | Some "raise",
          [ ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32) ->
            // `int raise(int sig)`.
            let operation = "libc raise"
            let signo = NativeCall.int32Argument operation instruction.Arguments.[0]
            raiseSignal operation ctx signo |> Some
        | Some "gettid",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32) ->
            // `pid_t gettid(void)`, which glibc has exported since 2.30 and which
            // makes the syscall every time.
            let flavour = SimulatedUnixPlatform.flavour state.Kernel.UnixPlatform

            match gettid flavour (UnixTaskState.osThreadId (EmulatedKernel.taskOf ctx.Thread state.Kernel.Tasks)) with
            | Some tid ->
                state
                |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 tid)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | None ->
                failwith
                    $"libc gettid: the simulated platform is %O{flavour}, whose C library exports no gettid, so a real process's P/Invoke of it throws EntryPointNotFoundException; PawPrint does not raise that"
        | _ -> None
