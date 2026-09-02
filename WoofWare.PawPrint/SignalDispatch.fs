namespace WoofWare.PawPrint

open System.Collections.Immutable
open WoofWare.PosixKernel

/// Drives signal delivery onto the kernel-owned dispatcher thread allocated
/// by `SystemNative_InitializeTerminalAndSignalHandling`. Mirrors the real
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
///     thread exists, and a handler has been installed), and the dispatcher
///     itself is currently Parked, we pop the entry off the queue and install
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
/// itself a candidate recipient of the next signal: `tryDeliverable` is
/// called with the live-thread set with the dispatcher removed, so a
/// process-directed signal whose mask is vacuously empty on the dispatcher
/// cannot pick the dispatcher as its receiver. The receiver chosen by
/// `tryDeliverable` is intentionally discarded today; this module models the
/// "handler runs on the kernel-owned dispatcher thread" branch (which matches
/// CoreCLR's `SignalHandlerLoop`). When PawPrint grows the
/// `pthread_kill`-style branch where the receiver thread itself takes the
/// hit, the receiver id will be needed and this discard goes away.
///
/// The handler's `int` return value (real CoreCLR's "0 = run default
/// disposition, 1 = consumed") is dropped on the floor; modelling default
/// dispositions is a later slice.
[<RequireQualifiedAccess>]
module SignalDispatch =

    /// Pull the eligible-receiver thread ids out of state. A thread is
    /// only eligible to *receive* a signal if there's a kernel-level thread
    /// behind it. In PawPrint terms three states correspond to "no OS
    /// thread":
    ///
    ///   * `NotStarted` — the managed `Thread` object exists but `Start`
    ///     hasn't been called, so the OS thread does not exist yet.
    ///   * `Parked` — PawPrint-internal auxiliary threads (the dispatcher
    ///     itself is the only inhabitant today); these never run guest IL
    ///     and have no OS-level identity for the kernel to deliver a signal
    ///     to.
    ///   * `Terminated` — the thread has exited; its final frames are
    ///     intentionally retained so other threads can observe state for
    ///     `Join`, but the OS thread is gone.
    let private liveExcludingDispatcher (dispatcher : ThreadId) (state : IlMachineState) : ImmutableArray<ThreadId> =
        let builder = ImmutableArray.CreateBuilder<ThreadId> ()

        for KeyValue (tid, ts) in state.ThreadState do
            // `NotStarted` and `Parked` are both classified as
            // `hasNoActiveFrame`, so the `not hasNoActiveFrame` arm covers
            // them and a new frameless `ThreadStatus` variant is
            // automatically excluded. `Terminated` retains its frames (so
            // `hasNoActiveFrame` returns `false` for it), hence the explicit
            // `<> Terminated` arm.
            //
            // The explicit `tid <> dispatcher` exclusion is redundant today
            // (the dispatcher is `Parked`, so `hasNoActiveFrame` already
            // drops it), but enforces an invariant that must survive
            // refactoring: the dispatcher runs the handler *for* a receiver
            // and is never itself a candidate, even if a future change gave
            // the dispatcher live frames between handler invocations.
            if
                tid <> dispatcher
                && ts.Status <> ThreadStatus.Terminated
                && not (ThreadStatus.hasNoActiveFrame ts.Status)
            then
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
    /// now, the dispatcher is currently Parked, and a handler is installed,
    /// pop the entry off the queue, build a `(signo, posixSignal-enum)`
    /// invocation frame for the handler, and flip the dispatcher
    /// Parked → Runnable so the scheduler picks it up on this tick. Otherwise
    /// returns the state unchanged: every guard path here is "no-op and let
    /// the next tick try again", matching the long-poll cadence of the real
    /// `SignalHandlerLoop`.
    let trySpawnHandler (baseClassTypes : BaseClassTypes<DumpedAssembly>) (state : IlMachineState) : IlMachineState =
        match SignalState.signalThread state.Kernel.Signals with
        | None ->
            // Signal handling has not been initialised; there is no
            // dispatcher to wake, so anything in `Pending` (there shouldn't
            // be, but a defensive caller might enqueue early) waits.
            state
        | Some dispatcher ->

        match SignalState.handler state.Kernel.Signals with
        | None ->
            // No managed handler installed yet. Real CoreCLR ignores
            // delivered signals while `g_posixSignalHandler == NULL` and
            // PawPrint mirrors that: pending entries stay queued so a
            // later `SetPosixSignalHandler` plus `enable` can drain them.
            state
        | Some handler ->

        let dispatcherStatus =
            match Map.tryFind dispatcher state.ThreadState with
            | Some ts -> ts.Status
            | None ->
                failwith
                    $"SignalDispatch.trySpawnHandler: dispatcher thread %O{dispatcher} recorded in SignalState but no ThreadState entry exists — the initialisation path should always allocate both."

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

        match SignalState.tryDeliverable liveThreads state.Kernel.Signals with
        | None ->
            // Nothing deliverable now (queue empty, signal disabled, target
            // dead/blocking, or — for a process-directed signal — no
            // eligible live thread).
            state
        | Some (entry, _receiver, signalsAfter) ->

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

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    Process =
                        { kernel.Process with
                            Signals = signalsAfter
                        }
                }
            )

        IlMachineState.startParkedDispatcher dispatcher newMethodState state

    /// Called from `Program.stepPrepared` when `ExecutionResult.Terminated`
    /// fires for the dispatcher's bottom frame (i.e. the handler `ret`urned
    /// past its own frame). Resets the dispatcher to its idle shape
    /// (Parked + sentinel frame id + no live frames) so the next deliverable
    /// signal can wake it.
    let reParkAfterHandler (dispatcher : ThreadId) (state : IlMachineState) : IlMachineState =
        IlMachineState.reParkDispatcher dispatcher state
