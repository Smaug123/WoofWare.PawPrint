namespace WoofWare.PawPrint

open System.Collections.Immutable
open WoofWare.PosixKernel

/// Why PawPrint will not send a signal to its own process, although the kernel
/// model has an answer: in each case a real CoreCLR process does something the
/// model cannot express, so the model's answer would be wrong.
[<RequireQualifiedAccess>]
type UnmodelledSelfSignal =
    /// The runtime catches or ignores this signal from startup, so a real
    /// process is not terminated by it as the kernel's default says; the model
    /// starts every signal at that default. See `StartupSignalDispositions`.
    | StartupDisposition of Signal
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

[<RequireQualifiedAccess>]
module UnmodelledSelfSignal =
    /// Why the model's answer would be wrong, for a diagnostic.
    let describe (refusal : UnmodelledSelfSignal) : string =
        match refusal with
        | UnmodelledSelfSignal.StartupDisposition signal ->
            $"a real CoreCLR process catches or ignores %O{signal} from startup, so it is not terminated by the signal as the kernel's default says (usually it survives; on x86-64 Linux, SIGTRAP kills it with SIGILL instead). PawPrint's kernel model starts the signal at its default. Modelling the runtime's startup dispositions is not done yet."
        | UnmodelledSelfSignal.ContinueWithoutHandler signal ->
            $"%O{signal} with no handler registered continues a stopped process, and a running one carries on regardless; PawPrint has no stopped state, and would leave the signal pending for a dispatcher that refuses it."
        | UnmodelledSelfSignal.WouldCoalesce signal ->
            $"%O{signal} has a handler registered and is already pending. The runtime's native handler would pass both instances to its dispatcher, where PawPrint's pending set would merge them into one."

/// Entry points of the C library itself, which a guest reaches only through a
/// P/Invoke of its own naming the library `libc`: the BCL calls none of them
/// directly. The runtime resolves that name to the platform's C library on
/// Linux and Darwin alike (`pal_dynamicload.c`, `LIBC_SO`).
[<RequireQualifiedAccess>]
module NativeLibc =

    /// Whether PawPrint's kernel model can answer a signal sent to its own
    /// process, given the signal state before it is sent. `None` if it can.
    ///
    /// Asked only of a signal that is actually being sent to the calling
    /// process: the null signal, an invalid number and another target are all
    /// answered or refused by `UnixSignal.kill` before this matters.
    let screenSelfSignal<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (signals : SignalState<'Task, 'Handler>)
        (signal : Signal)
        : UnmodelledSelfSignal option
        =
        let numbering = SignalState.numbering signals
        let signal = Signal.canonicalUnder numbering signal
        let claimed = SignalState.isEnabled signal signals

        if StartupSignalDispositions.overridesTerminatingDefault numbering signal then
            // Whether or not a handler is registered. Registering one does not
            // restore the kernel's default (pal_signal.c): `InstallSignalHandler`
            // leaves an ignored signal ignored, and for a signal that already
            // had a handler, the shim calls that handler itself and
            // `HandleNonCanceledPosixSignal` then does nothing more.
            Some (UnmodelledSelfSignal.StartupDisposition signal)
        elif
            not claimed
            && Signal.defaultDispositionUnder numbering signal = DefaultDisposition.Continue
        then
            Some (UnmodelledSelfSignal.ContinueWithoutHandler signal)
        elif
            claimed
            && not (Signal.isRealTimeUnder numbering signal)
            && SignalState.pending signals
               |> List.exists (fun pending -> pending.Signal = signal && pending.Target = ValueNone)
        then
            Some (UnmodelledSelfSignal.WouldCoalesce signal)
        else
            None

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
            |> ValueOption.bind (screenSelfSignal system.Process.Signals >> ValueOption.ofOption)
        with
        | ValueSome refusal ->
            failwith
                $"%s{operation}: kill(%d{pid}, %d{signo}) is not modelled: %s{UnmodelledSelfSignal.describe refusal}"
        | ValueNone ->

        match generation with
        | SignalGeneration.ProcessContinues -> state.MapKernel (EmulatedKernel.withUnix after) |> returning 0
        | SignalGeneration.ProcessTerminated signal ->
            // The process never returns from this call.
            ExecutionResult.SignalTerminated (state.MapKernel (EmulatedKernel.withUnix after), signal)
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
