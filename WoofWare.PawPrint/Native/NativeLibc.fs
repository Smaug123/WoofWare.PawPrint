namespace WoofWare.PawPrint

open System.Collections.Immutable
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
    /// A signal a handler is registered for, already pending. The runtime's
    /// native handler takes each instance as it arrives and passes every one
    /// on to its dispatcher, so both would reach managed code; the model would
    /// merge the two into one pending instance.
    | WouldCoalesce of Signal
    /// Sending `sent` would discard `queued`, a pending signal a handler is
    /// registered for: generating a stop signal discards a pending SIGCONT,
    /// and SIGCONT pending stop signals. The runtime's native handler has
    /// already taken that instance and passed it on to its dispatcher, so a
    /// real process still runs its managed handler; the model would drop it.
    | WouldDiscardQueued of sent : Signal * queued : Signal

[<RequireQualifiedAccess>]
module UnmodelledSelfSignal =
    /// Why the model's answer would be wrong, for a diagnostic.
    let describe (refusal : UnmodelledSelfSignal) : string =
        match refusal with
        | UnmodelledSelfSignal.NativeHandler (signal, handler) ->
            let whose =
                match handler with
                | NativeSignalHandler.CoreClrPal -> "a handler of CoreCLR's PAL"
                | NativeSignalHandler.GlibcSetXid -> "glibc's own SIGSETXID handler"
                | NativeSignalHandler.SystemNative -> "System.Native's handler"

            $"a real CoreCLR process runs %s{whose} for %O{signal}, which PawPrint does not model (usually the process survives the signal; on x86-64 Linux, SIGTRAP kills it with SIGILL instead)."
        | UnmodelledSelfSignal.ContinueWithoutHandler signal ->
            $"%O{signal} with no handler registered continues a stopped process, and a running one carries on regardless; PawPrint has no stopped state, and would leave the signal pending for a dispatcher that refuses it."
        | UnmodelledSelfSignal.WouldCoalesce signal ->
            $"%O{signal} has a handler registered and is already pending. The runtime's native handler would pass both instances to its dispatcher, where PawPrint's pending set would merge them into one."
        | UnmodelledSelfSignal.WouldDiscardQueued (sent, queued) ->
            $"%O{sent} would discard the pending %O{queued}, which has a handler registered. The runtime's native handler has already passed that instance to its dispatcher, which still runs the managed handler for it; PawPrint's pending set does not hold the dispatcher's queue apart from the kernel's, and would drop it."

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
        | SignalDisposition.Catch NativeSignalHandler.SystemNative ->
            match PosixSignalShim.chainsToNativeHandler numbering signal shim with
            | Some chained ->
                // Registering a handler does not take the runtime's own away
                // (pal_signal.c): `InstallSignalHandler` keeps the handler it
                // replaces, and the shim's handler calls it before anything
                // reaches managed code.
                Some (UnmodelledSelfSignal.NativeHandler (signal, chained))
            | None ->
                if
                    not (Signal.isRealTimeUnder numbering signal)
                    && SignalState.pending signals
                       |> List.exists (fun pending -> pending.Signal = signal && pending.Target = ValueNone)
                then
                    Some (UnmodelledSelfSignal.WouldCoalesce signal)
                else
                    None
        | SignalDisposition.Catch handler -> Some (UnmodelledSelfSignal.NativeHandler (signal, handler))
        | SignalDisposition.Default when Signal.defaultDispositionUnder numbering signal = DefaultDisposition.Continue ->
            Some (UnmodelledSelfSignal.ContinueWithoutHandler signal)
        | SignalDisposition.Default
        | SignalDisposition.Ignore -> None

    /// Whether generating a signal, which took the signal state from `before`
    /// to `after`, discarded a pending instance of a signal System.Native's
    /// handler catches. `None` if it did not.
    ///
    /// Such an instance is one the dispatcher has not yet run the managed
    /// handler for, which on a real process the native handler has already
    /// taken; see `UnmodelledSelfSignal.WouldDiscardQueued`.
    let screenGeneration<'Task when 'Task : comparison>
        (sent : Signal)
        (before : SignalState<'Task, NativeSignalHandler>)
        (after : SignalState<'Task, NativeSignalHandler>)
        : UnmodelledSelfSignal option
        =
        let remaining = SignalState.pending after

        SignalState.pending before
        |> List.tryFind (fun entry ->
            SignalState.disposition entry.Signal before = SignalDisposition.Catch NativeSignalHandler.SystemNative
            && not (List.contains entry remaining)
        )
        |> Option.map (fun entry ->
            UnmodelledSelfSignal.WouldDiscardQueued (
                Signal.canonicalUnder (SignalState.numbering before) sent,
                entry.Signal
            )
        )

    /// `kill(2)`, issued by the thread `ctx` is executing, pushing its `int`
    /// result: 0, or -1 with errno set.
    ///
    /// A signal whose kernel default ends the process ends the run here, with
    /// the call never returning. A target other than the calling process, and
    /// anything `screenSelfSignal` or `screenGeneration` refuses, fail the
    /// run: the model has no answer to give.
    let kill (operation : string) (ctx : NativeCallContext) (pid : int) (signo : int) : NativeHandlerResult =
        let state = ctx.State
        let system = EmulatedKernel.unix state.Kernel

        let returning (value : int) (state : IlMachineState) : NativeHandlerResult =
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 value)) ctx.Thread
            |> NativeHandlerResult.completed

        let liveThreads =
            state.ThreadState
            |> Seq.choose (fun (KeyValue (thread, ts)) ->
                if ThreadStatus.canReceiveSignal ts.Status then
                    Some thread
                else
                    None
            )
            |> ImmutableArray.CreateRange

        match UnixSignal.kill liveThreads pid signo system with
        | Error refusal ->
            failwith
                $"%s{operation}: kill(%d{pid}, %d{signo}) from process %O{UnixSystem.processId system} is not modelled (%O{refusal}); only a signal to the calling process itself is."
        | Ok (Error errno) ->
            let numbering = SimulatedUnixPlatform.rawErrnoNumbering state.Kernel.UnixPlatform

            state.MapKernel (EmulatedKernel.withLastSystemError ctx.Thread (UnixError.toRawErrnoUnder numbering errno))
            |> returning -1
        | Ok (Ok (generation, after)) ->

        let sent =
            Signal.ofRawSignoUnder (SignalState.numbering system.Process.Signals) signo

        match
            sent
            |> ValueOption.bind (fun sent ->
                match screenSelfSignal state.Kernel.PosixSignalShim system.Process.Signals sent with
                | Some refusal -> ValueSome refusal
                | None ->
                    screenGeneration sent system.Process.Signals after.Process.Signals
                    |> ValueOption.ofOption
            )
        with
        | ValueSome refusal ->
            failwith
                $"%s{operation}: kill(%d{pid}, %d{signo}) is not modelled: %s{UnmodelledSelfSignal.describe refusal}"
        | ValueNone ->

        match generation with
        | SignalGeneration.ProcessContinues -> state.MapKernel (EmulatedKernel.withUnix after) |> returning 0
        | SignalGeneration.ProcessTerminated (signal, coreDumped) ->
            // The process never returns from this call.
            ExecutionResult.SignalTerminated (state.MapKernel (EmulatedKernel.withUnix after), signal, coreDumped)
            |> NativeHandlerResult.ofExecutionResult
        | SignalGeneration.ProcessStopped signal ->
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
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // `int kill(pid_t pid, int sig)`: `pid_t` is a 32-bit int on both
            // flavours.
            let operation = "libc kill"
            let pid = NativeCall.int32Argument operation instruction.Arguments.[0]
            let signo = NativeCall.int32Argument operation instruction.Arguments.[1]
            kill operation ctx pid signo |> Some
        | _ -> None
