namespace WoofWare.PawPrint

open System.Collections.Immutable
open WoofWare.PosixKernel

/// Drives signal delivery onto the shim's dispatcher thread allocated
/// by `SystemNative_InitializeTerminalAndSignalHandling` (see
/// `PosixSignalShim`). Mirrors the real
/// CoreCLR `SignalHandlerLoop` pthread: a long-lived auxiliary thread that
/// the runtime owns and the guest never names, woken by the kernel when a
/// pending signal becomes deliverable, runs the installed managed handler,
/// and returns to its idle state.
///
/// PawPrint encodes that loop as two transitions over the dispatcher's
/// `ThreadStatus`:
///
///   * `trySpawnHandler` — Parked → Runnable. Polled between every guest IL
///     step from `Program.stepPrepared`. If a pending entry in
///     `SignalState.Pending` is deliverable now (signal enabled, target alive
///     and not blocking it, or no specific target but at least one such live
///     thread exists), and the dispatcher itself is currently Parked, we pop
///     the entry off the queue and install
///     a fresh bottom frame on the dispatcher that calls the registered
///     handler with `(int signo, int posixSignalEnumValue)`. The frame has no
///     `ReturnState`, so when the handler eventually `ret`urns, the bottom
///     frame's exit surfaces as `ExecutionResult.Terminated` — that's the
///     signal for `reParkAfterHandler` to fire.
///
///   * `reParkAfterHandler` — Runnable → Parked. Called from
///     `Program.stepPrepared` when it observes the dispatcher's bottom frame
///     returning past itself (via `ExecutionResult.Terminated`). Clears the
///     stale frames, resets the sentinel frame id, and flips the status back
///     to `Parked` so the next deliverable signal can wake it again.
///
/// The dispatcher is the *recipient* the runtime hands the signal to — never
/// itself a candidate recipient of the next signal: `nextDelivery` is
/// called with the live-thread set with the dispatcher removed, so a
/// process-directed signal whose mask is vacuously empty on the dispatcher
/// cannot pick the dispatcher as its receiver. The receiver chosen by
/// `nextDelivery` is intentionally discarded today; this module models the
/// "handler runs on the runtime-owned dispatcher thread" branch (which matches
/// CoreCLR's `SignalHandlerLoop`). When PawPrint grows the
/// `pthread_kill`-style branch where the receiver thread itself takes the
/// hit, the receiver id will be needed and this discard goes away.
///
/// The handler's `int` return value is real CoreCLR's "0 = not handled,
/// 1 = handled": on 0, `SignalHandlerLoop` goes on to call
/// `SystemNative_HandleNonCanceledPosixSignal` itself. PawPrint does not model
/// that step, so `reParkAfterHandler` refuses a 0. The
/// `SignalDelivery.Default*` cases are refused loudly: a default that
/// terminates or stops is applied when the signal is generated (see
/// `NativeLibc.kill`), so one reaches this poll only by becoming receivable
/// later, after an unblock, and nothing sets a signal mask yet.
[<RequireQualifiedAccess>]
module SignalDispatch =

    /// Pull the eligible-receiver thread ids out of state: every thread
    /// `ThreadStatus.canReceiveSignal` admits, other than the dispatcher.
    let private liveExcludingDispatcher (dispatcher : ThreadId) (state : IlMachineState) : ImmutableArray<ThreadId> =
        let builder = ImmutableArray.CreateBuilder<ThreadId> ()

        for KeyValue (tid, ts) in state.ThreadState do
            // The explicit `tid <> dispatcher` exclusion is redundant while
            // the dispatcher is `Parked` between invocations (which
            // `canReceiveSignal` already refuses), but enforces an invariant
            // that must survive refactoring: the dispatcher runs the handler
            // *for* a receiver and is never itself a candidate, even while it
            // is running one.
            if tid <> dispatcher && ThreadStatus.canReceiveSignal ts.Status then
                builder.Add tid

        builder.ToImmutable ()

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
                $"SignalDispatch.trySpawnHandler: registered handler %s{mi.Name} on type %s{MethodOwner.describe mi.Owner} declares %d{MethodInfo.arity mi} parameters; expected exactly 2 ((int signo, PosixSignal signal) -> int)."

        match mi.Signature.ReturnType with
        | MethodReturnType.Void ->
            failwith
                $"SignalDispatch.trySpawnHandler: registered handler %s{mi.Name} on type %s{MethodOwner.describe mi.Owner} returns void; expected Int32 (the 'should run default disposition?' flag)."
        | MethodReturnType.Returns ret ->
            match ret with
            | ConcretePrimitive concreteTypes PrimitiveType.Int32 -> ()
            | _ ->
                failwith
                    $"SignalDispatch.trySpawnHandler: registered handler %s{mi.Name} on type %s{MethodOwner.describe mi.Owner} returns a non-Int32 type; expected Int32 (the 'should run default disposition?' flag)."

    /// Polled once per tick by `Program.stepPrepared` immediately before the
    /// scheduler picks its next thread. If a pending signal is deliverable
    /// now and the dispatcher is currently Parked,
    /// pop the entry off the queue, build a `(signo, posixSignal-enum)`
    /// invocation frame for the handler, and flip the dispatcher
    /// Parked → Runnable so the scheduler picks it up on this tick. Otherwise
    /// returns the state unchanged: every guard path here is "no-op and let
    /// the next tick try again", matching the long-poll cadence of the real
    /// `SignalHandlerLoop`.
    ///
    /// Fails if the signal to deliver finds no callback installed by
    /// `SystemNative_SetPosixSignalHandler`: the shim asserts there is one,
    /// and the BCL installs it before it enables any signal.
    let trySpawnHandler (baseClassTypes : BaseClassTypes<DumpedAssembly>) (state : IlMachineState) : IlMachineState =
        match PosixSignalShim.signalThread state.Kernel.PosixSignalShim with
        | None ->
            // Signal handling has not been initialised; there is no
            // dispatcher to wake, so anything in `Pending` (there shouldn't
            // be, but a defensive caller might enqueue early) waits.
            state
        | Some dispatcher ->

        let dispatcherStatus =
            match Map.tryFind dispatcher state.ThreadState with
            | Some ts -> ts.Status
            | None ->
                failwith
                    $"SignalDispatch.trySpawnHandler: dispatcher thread %O{dispatcher} recorded in PosixSignalShim but no ThreadState entry exists — the initialisation path should always allocate both."

        match dispatcherStatus with
        | ThreadStatus.Parked -> ()
        | _ ->
            // Dispatcher is already running a previous handler invocation;
            // the next tick re-polls. Matches the single-threaded
            // `SignalHandlerLoop` body: only one signal runs at a time.
            ()

        if dispatcherStatus <> ThreadStatus.Parked then
            state
        else

        let liveThreads = liveExcludingDispatcher dispatcher state

        let delivery, signalsAfter =
            SignalState.nextDelivery liveThreads state.Kernel.Signals

        // Persist the scan's state whether or not it produced an action:
        // discarding a receivable ignored signal is a state change with no
        // delivery, and dropping it would replay the discard every tick.
        let state =
            if signalsAfter = state.Kernel.Signals then
                state
            else
                state.MapKernel (fun kernel ->
                    { kernel with
                        Process =
                            { kernel.Process with
                                Signals = signalsAfter
                            }
                    }
                )

        match delivery with
        | None ->
            // Nothing receivable now (queue empty, target dead/blocking, or —
            // for a process-directed signal — no eligible live thread).
            state
        | Some (SignalDelivery.DefaultTerminate signal)
        | Some (SignalDelivery.DefaultStop signal)
        | Some (SignalDelivery.DefaultContinue signal) ->
            // A pending signal with no handler enabled for it, whose kernel
            // default is to terminate, stop or continue the process.
            // `SignalState.generate` applies a terminating or stopping default
            // at generation whenever some thread can receive the signal, so it
            // is pending here only if none could then; and every thread
            // could, because nothing sets a signal mask yet. Reaching this is
            // therefore a test driving the queue by hand, or a mask landing
            // without this poll learning to apply defaults, and it is refused
            // rather than half-modelled.
            failwith
                $"SignalDispatch.trySpawnHandler: pending %O{signal} has no enabled handler and its kernel default is not Ignore; applying a default disposition at delivery rather than at generation is not modelled."
        | Some (SignalDelivery.RunHandler (entry, _receiver)) ->

        let handler =
            match PosixSignalShim.handler state.Kernel.PosixSignalShim with
            | Some handler -> handler
            | None ->
                // The shim's `SignalHandlerLoop` asserts `g_posixSignalHandler
                // != NULL` before calling through it, and a build without
                // asserts calls a null function pointer: there is no
                // behaviour here to model.
                failwith
                    $"SignalDispatch.trySpawnHandler: %O{entry.Signal} is enabled and due for delivery, but no handler has been installed with SystemNative_SetPosixSignalHandler; the real shim asserts one has."

        let mi = SignalHandler.methodInfo handler
        validateHandlerSignature state.ConcreteTypes mi

        let containingAssembly =
            state.LoadedAssembly mi.DeclaringAssemblyFullName
            |> Option.defaultWith (fun () ->
                failwith
                    $"SignalDispatch.trySpawnHandler: assembly %s{AssemblyDefinitionName.simpleName mi.DeclaringAssemblyFullName} for handler %s{mi.Name} is not loaded; the SetPosixSignalHandler QCall should have loaded it."
            )

        let args =
            buildArgs (SimulatedUnixPlatform.signalNumbering state.Kernel.UnixPlatform) entry.Signal

        // `MethodState.Empty` enforces an arity check against
        // `MethodInfo.arity mi` (plus 1 if non-static). The handler is
        // expected to be the static `OnPosixSignal`; if a test installs an
        // instance stand-in, that's a configuration error in the test, not
        // something this dispatch path should silently paper over.
        let newMethodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    baseClassTypes
                    state._LoadedAssemblies
                    containingAssembly
                    mi
                    mi.Generics
                    args
                    None
            with
            | Ok ms -> ms
            | Error _ ->
                failwith
                    $"SignalDispatch.trySpawnHandler: failed to build MethodState for handler %s{mi.Name} on type %s{MethodOwner.describe mi.Owner}."

        IlMachineState.startParkedDispatcher dispatcher newMethodState state

    /// Called from `Program.stepPrepared` when `ExecutionResult.Terminated`
    /// fires for the dispatcher's bottom frame (i.e. the handler `ret`urned
    /// past its own frame), with the handler's `int` result still on the
    /// dispatcher's evaluation stack. Resets the dispatcher to its idle shape
    /// (Parked + sentinel frame id + no live frames) so the next deliverable
    /// signal can wake it.
    ///
    /// Fails if the handler returned 0. `OnPosixSignal` does so when it finds
    /// no registration for the signal, which happens when the last one is
    /// disposed after the signal was dispatched; the real `SignalHandlerLoop`
    /// then applies the signal's default through
    /// `SystemNative_HandleNonCanceledPosixSignal`, which this dispatcher does
    /// not do.
    let reParkAfterHandler (dispatcher : ThreadId) (state : IlMachineState) : IlMachineState =
        match IlMachineState.peekEvalStack dispatcher state with
        | Some (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ->
            failwith
                "SignalDispatch.reParkAfterHandler: the signal handler returned 0, reporting that nothing handled the signal (OnPosixSignal found no registration for it). The real SignalHandlerLoop would then apply the signal's default through SystemNative_HandleNonCanceledPosixSignal; PawPrint's dispatcher does not model that step."
        | Some (EvalStackValue.Int32 (Int32Source.Verbatim _)) -> ()
        | other ->
            failwith
                $"SignalDispatch.reParkAfterHandler: expected the signal handler's Int32 result on dispatcher %O{dispatcher}'s evaluation stack, found %O{other}."

        IlMachineState.reParkDispatcher dispatcher state
