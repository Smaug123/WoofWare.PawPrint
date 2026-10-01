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
    /// A signal whose kernel default is to continue a stopped process, with no
    /// handler registered. A real process, never stopped, carries on; the model
    /// has no stopped state for it to resume, and would leave it pending for a
    /// dispatcher that refuses it.
    | ContinueWithoutHandler of Signal

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
        | UnmodelledSelfSignal.ContinueWithoutHandler signal ->
            $"%O{signal} with no handler registered continues a stopped process, and a running one carries on regardless; PawPrint has no stopped state, and would leave the signal pending for a dispatcher that refuses it."

/// Entry points of the C library itself, which a guest reaches only through a
/// P/Invoke of its own naming the library `libc`: the BCL calls none of them
/// directly. The runtime resolves that name to the platform's C library on
/// Linux and Darwin alike (`pal_dynamicload.c`, `LIBC_SO`).
[<RequireQualifiedAccess>]
module NativeLibc =

    /// Whether PawPrint's kernel model can answer a signal sent to its own
    /// process, given the signal state and System.Native's state before it is
    /// sent. `None` if it can.
    ///
    /// Asked only of a signal that is actually being sent to the calling
    /// process: the null signal, an invalid number and another target are all
    /// answered or refused by `UnixSignal.kill` before this matters.
    let screenSelfSignal<'Task when 'Task : comparison>
        (shim : PosixSignalShim)
        (signals : SignalState<'Task, NativeSignalHandler>)
        (signal : Signal)
        : UnmodelledSelfSignal option
        =
        let numbering = SignalState.numbering signals
        let signal = Signal.canonicalUnder numbering signal

        match SignalState.disposition signal signals with
        | SignalDisposition.Catch {
                                      Handler = NativeSignalHandler.SystemNative
                                  } ->
            match PosixSignalShim.chainsToNativeHandler numbering signal shim with
            | Some (NativeSignalHandler.CoreClrPalFault _)
            | None -> None
            | Some chained ->
                // Registering a handler does not take the runtime's own away
                // (pal_signal.c): `InstallSignalHandler` keeps the handler it
                // replaces, and the shim's handler calls it before anything
                // reaches managed code.
                Some (UnmodelledSelfSignal.NativeHandler (signal, chained))
        | SignalDisposition.Catch {
                                      Handler = NativeSignalHandler.CoreClrPalFault _
                                  } -> None
        | SignalDisposition.Catch action -> Some (UnmodelledSelfSignal.NativeHandler (signal, action.Handler))
        | SignalDisposition.Default when Signal.defaultDispositionUnder numbering signal = DefaultDisposition.Continue ->
            Some (UnmodelledSelfSignal.ContinueWithoutHandler signal)
        | SignalDisposition.Default
        | SignalDisposition.Ignore -> None

    /// `kill(2)`, issued by the thread `ctx` is executing, pushing its `int`
    /// result: 0, or -1 with errno set.
    ///
    /// A signal whose kernel default ends the process ends the run here, with
    /// the call never returning. A target other than the calling process, and
    /// anything `screenSelfSignal` refuses, fail the run: the model has no
    /// answer to give.
    let kill (operation : string) (ctx : NativeCallContext) (pid : int) (signo : int) : NativeHandlerResult =
        let state = ctx.State
        let system = EmulatedKernel.unix state.Kernel

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
            Signal.ofRawSignoUnder (SignalState.numbering system.Process.Signals) signo
            |> ValueOption.bind (fun sent ->
                screenSelfSignal state.Kernel.PosixSignalShim system.Process.Signals sent
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
            | ProcessTermination.Signaled (signal, coreDumped) ->
                ExecutionResult.SignalTerminated (state, signal, coreDumped)
                |> NativeHandlerResult.ofExecutionResult
            | ProcessTermination.Exited _ ->
                failwith
                    $"%s{operation}: kill(%d{pid}, %d{signo}) ended the process with an exit status (%O{ended.Termination}), which only an exit can"
        | KillOutcome.ProcessStopped (signal, _) ->
            failwith
                $"%s{operation}: %O{signal} would stop the whole process, and PawPrint does not model a stopped process (nothing could continue it)."

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
        | _ -> None
