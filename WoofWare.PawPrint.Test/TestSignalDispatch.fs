namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Focused tests for the `SignalDispatch` module that drives wake/sleep
/// transitions on the kernel-owned signal-dispatch thread. The dispatcher is
/// the auxiliary thread allocated by
/// `SystemNative_InitializeTerminalAndSignalHandling` and parked permanently
/// until a deliverable signal arrives; these tests pin down each guard path
/// (`trySpawnHandler` is supposed to be a no-op) and the one positive
/// transition (Parked → Runnable with a fresh bottom frame on the handler),
/// plus the inverse `reParkAfterHandler` transition (Runnable + bottom-frame
/// `ret` → Parked).
///
/// The handler stand-in is a static, two-int-arg, int-returning method picked
/// out of the corelib. `SignalDispatch`'s signature gate is permissive on
/// parameter types so the test doesn't need to install the real
/// `PosixSignalRegistration.OnPosixSignal` (which would drag the whole
/// PosixSignal type and registration plumbing into the fixture); the gate
/// only cares about arity and `Int32` return type. We don't execute the
/// handler frame here — only assert that the state transition produced the
/// expected `ThreadState` shape.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSignalDispatch =

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        Corelib.getBaseTypes corelib

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll (LoadedAssemblies.ofAssemblies [ corelib ]) baseClassTypes AllConcreteTypes.Empty

    let private baseState () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    /// Find a static method on the given top-level corelib type by name and
    /// parameter count, then concretize it. The (name, arity) filter is
    /// sufficient to disambiguate `String.Compare(string, string)` from its
    /// many overloads, and similarly for `Math.Max(int, int)` if a future
    /// caller swaps it in. Fails loudly if zero or more than one match.
    let private concretizeStaticByArity
        (state : IlMachineState)
        (typeNamespace : string)
        (typeName : string)
        (methodName : string)
        (arity : int)
        : IlMachineState * MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let typeDef =
            corelib.TryGetTopLevelTypeDef typeNamespace typeName
            |> Option.defaultWith (fun () -> failwith $"%s{typeNamespace}.%s{typeName} not found in corelib")

        let rawMethod =
            typeDef.Methods
            |> List.filter (fun m -> m.Name = methodName && m.IsStatic && m.Parameters.Length = arity)
            |> function
                | [ method ] -> method
                | [] ->
                    failwith
                        $"static method %s{methodName} with arity %d{arity} not found on %s{typeNamespace}.%s{typeName}"
                | methods ->
                    failwith
                        $"static method %s{methodName} with arity %d{arity} on %s{typeNamespace}.%s{typeName} was ambiguous: %d{methods.Length} matches"

        let state, method, _declaringType =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                rawMethod
                None
                corelib.Name
                ImmutableArray.Empty
                state

        state, method

    /// A stable 2-arg static int-returning method to use as the handler
    /// stand-in. `String.Compare(string, string)` is one of the small set of
    /// such methods in corelib; its parameter types don't match the real
    /// handler signature `(int, PosixSignal)` but the dispatch validator is
    /// deliberately loose on parameter types.
    let private installCompareAsHandler (state : IlMachineState) : IlMachineState * SignalHandler =
        let state, method = concretizeStaticByArity state "System" "String" "Compare" 2
        let handler = SignalHandler.ofMethodInfo method
        state, handler

    /// Bring up the dispatcher and install a handler — the common preamble
    /// for the positive-transition tests.
    let private preparedState () : IlMachineState * ThreadId * SignalHandler =
        let state = baseState ()
        let state, dispatcher = IlMachineState.allocateParkedThread state

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    Signals = kernel.Signals |> SignalState.markInitialized dispatcher
                }
            )

        let state, handler = installCompareAsHandler state

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    Signals = kernel.Signals |> SignalState.setHandler handler
                }
            )

        state, dispatcher, handler

    let private stubThreadState (status : ThreadStatus) : ThreadState =
        {
            MethodStates = Map.empty
            YieldDebt = Set.empty
            NextFrameId = 0
            ActiveMethodState = FrameId -1
            Status = status
            IsBackground = false
            Name = None
            Cpu = CpuId 0
            // Inert here: a frameless stub cannot execute the `SystemNative_*OSThreadId`
            // P/Invoke that reads it. Do not reuse this literal for a stub standing in
            // for more than one thread -- guest OS thread ids must be distinct.
            OsThreadId = OsThreadId 1u
        }

    [<Test>]
    let ``trySpawnHandler is a no-op when signal handling is not initialised`` () : unit =
        // No dispatcher allocated, no handler installed, no pending signals:
        // every guard fires and the state's signal subsystem must be
        // unchanged. `ThreadState` has no structural equality (its embedded
        // `MethodState` carries reference-typed payloads), so we check the
        // observable bits explicitly: thread-id keyspace and signal state.
        let state = baseState ()
        let state' = SignalDispatch.trySpawnHandler baseClassTypes state

        state'.Kernel.Signals |> shouldEqual state.Kernel.Signals

        let keysBefore = state.ThreadState |> Map.toList |> List.map fst
        let keysAfter = state'.ThreadState |> Map.toList |> List.map fst
        keysAfter |> shouldEqual keysBefore

    [<Test>]
    let ``trySpawnHandler is a no-op when no handler is installed`` () : unit =
        // Dispatcher initialised but `SetPosixSignalHandler` never called.
        // The pending queue might still grow (a process-startup signal could
        // already be queued), but with no handler to dispatch to we mirror
        // real CoreCLR's "ignore until handler installed" behaviour.
        let state = baseState ()
        let state, dispatcher = IlMachineState.allocateParkedThread state

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    Signals =
                        kernel.Signals
                        |> SignalState.markInitialized dispatcher
                        |> SignalState.enable Signal.SIGINT
                        |> SignalState.enqueue
                            {
                                Signal = Signal.SIGINT
                                Target = ValueNone
                            }
                }
            )

        let state' = SignalDispatch.trySpawnHandler baseClassTypes state

        let dispatcherTs = state'.ThreadState |> Map.find dispatcher
        dispatcherTs.Status |> shouldEqual ThreadStatus.Parked
        dispatcherTs.MethodStates.Count |> shouldEqual 0
        // Pending entry is *not* consumed: a later `setHandler` should still
        // be able to drain it.
        state'.Kernel.Signals
        |> SignalState.pending
        |> shouldEqual
            [
                {
                    Signal = Signal.SIGINT
                    Target = ValueNone
                }
            ]

    [<Test>]
    let ``trySpawnHandler is a no-op when there is no pending signal`` () : unit =
        let state, dispatcher, _ = preparedState ()

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    Signals = kernel.Signals |> SignalState.enable Signal.SIGINT
                }
            )

        let state' = SignalDispatch.trySpawnHandler baseClassTypes state

        let dispatcherTs = state'.ThreadState |> Map.find dispatcher
        dispatcherTs.Status |> shouldEqual ThreadStatus.Parked
        dispatcherTs.MethodStates.Count |> shouldEqual 0

    [<Test>]
    let ``trySpawnHandler is a no-op when the only pending signal is disabled`` () : unit =
        // Pending entry exists but its signal hasn't been `enable`d. The
        // real native side enable bit gates dispatch; our model does the
        // same in `tryDeliverable`, so the entry must stay queued.
        let state, dispatcher, _ = preparedState ()

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    Signals =
                        kernel.Signals
                        |> SignalState.enqueue
                            {
                                Signal = Signal.SIGINT
                                Target = ValueNone
                            }
                }
            )

        let state' = SignalDispatch.trySpawnHandler baseClassTypes state

        let dispatcherTs = state'.ThreadState |> Map.find dispatcher
        dispatcherTs.Status |> shouldEqual ThreadStatus.Parked
        dispatcherTs.MethodStates.Count |> shouldEqual 0

        state'.Kernel.Signals
        |> SignalState.pending
        |> shouldEqual
            [
                {
                    Signal = Signal.SIGINT
                    Target = ValueNone
                }
            ]

    [<Test>]
    let ``trySpawnHandler is a no-op when the dispatcher is busy`` () : unit =
        // Dispatcher is Runnable (already mid-handler from a prior wake); the
        // next deliverable signal must wait. We don't consume the pending
        // entry on this tick — once the in-flight handler returns and re-parks
        // the dispatcher, a subsequent tick will pick it up.
        let state, dispatcher, _ = preparedState ()
        let runnableSibling = ThreadId 99

        let state =
            { state with
                ThreadState =
                    state.ThreadState
                    |> Map.add runnableSibling (stubThreadState ThreadStatus.Runnable)
                    |> Map.change
                        dispatcher
                        (Option.map (fun ts ->
                            { ts with
                                Status = ThreadStatus.Runnable
                            }
                        ))
            }

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    Signals =
                        kernel.Signals
                        |> SignalState.enable Signal.SIGINT
                        |> SignalState.enqueue
                            {
                                Signal = Signal.SIGINT
                                Target = ValueNone
                            }
                }
            )

        let state' = SignalDispatch.trySpawnHandler baseClassTypes state

        // Dispatcher still Runnable, no fresh frame inserted on top of
        // whatever it was running, queue intact.
        let dispatcherTs = state'.ThreadState |> Map.find dispatcher
        dispatcherTs.Status |> shouldEqual ThreadStatus.Runnable

        state'.Kernel.Signals
        |> SignalState.pending
        |> shouldEqual
            [
                {
                    Signal = Signal.SIGINT
                    Target = ValueNone
                }
            ]

    [<Test>]
    let ``trySpawnHandler dispatches a deliverable signal onto a Parked dispatcher`` () : unit =
        // The positive path: dispatcher Parked, handler installed, signal
        // enabled, at least one live non-dispatcher thread, pending entry
        // present. Expect Parked → Runnable, a bottom frame on the handler,
        // and the entry popped off the queue.
        let state, dispatcher, _ = preparedState ()
        let runnableSibling = ThreadId 99

        let state =
            { state with
                ThreadState =
                    state.ThreadState
                    |> Map.add runnableSibling (stubThreadState ThreadStatus.Runnable)
            }

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    Signals =
                        kernel.Signals
                        |> SignalState.enable Signal.SIGINT
                        |> SignalState.enqueue
                            {
                                Signal = Signal.SIGINT
                                Target = ValueNone
                            }
                }
            )

        let state' = SignalDispatch.trySpawnHandler baseClassTypes state

        let dispatcherTs = state'.ThreadState |> Map.find dispatcher
        dispatcherTs.Status |> shouldEqual ThreadStatus.Runnable
        // A handler frame has been installed; ActiveMethodState now points
        // at a live entry rather than the sentinel FrameId -1.
        dispatcherTs.MethodStates.ContainsKey dispatcherTs.ActiveMethodState
        |> shouldEqual true
        // Pending queue drained.
        state'.Kernel.Signals |> SignalState.pending |> shouldEqual []

    [<Test>]
    let ``trySpawnHandler passes the Linux signo and PosixSignal enum as int args`` () : unit =
        // The handler's bottom frame should have its Arguments populated with
        // `(linuxSigno, posixSignalEnum)`, both as `CliType.Numeric Int32`.
        // For SIGINT: signo = 2, enum value = -2.
        let state, dispatcher, _ = preparedState ()
        let runnableSibling = ThreadId 99

        let state =
            { state with
                ThreadState =
                    state.ThreadState
                    |> Map.add runnableSibling (stubThreadState ThreadStatus.Runnable)
            }

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    Signals =
                        kernel.Signals
                        |> SignalState.enable Signal.SIGINT
                        |> SignalState.enqueue
                            {
                                Signal = Signal.SIGINT
                                Target = ValueNone
                            }
                }
            )

        let state' = SignalDispatch.trySpawnHandler baseClassTypes state

        let dispatcherTs = state'.ThreadState |> Map.find dispatcher
        let frame = dispatcherTs.MethodStates |> Map.find dispatcherTs.ActiveMethodState

        frame.Arguments.Length |> shouldEqual 2
        frame.Arguments.[0] |> shouldEqual (CliType.Numeric (CliNumericType.Int32 2))
        frame.Arguments.[1] |> shouldEqual (CliType.Numeric (CliNumericType.Int32 -2))

    [<Test>]
    let ``trySpawnHandler does not pick the dispatcher itself as the signal receiver`` () : unit =
        // For a process-directed signal (Target = ValueNone), the dispatcher
        // has an empty `Blocked` map, so the naive eligibility check ("thread
        // is live and not blocking the signal") would mark it as a candidate
        // receiver. `SignalDispatch.trySpawnHandler` must exclude the
        // dispatcher from the live-threads set passed to `tryDeliverable`,
        // otherwise a process-directed signal with no other live threads
        // would dispatch its own handler to itself as the receiver. Set up a
        // world where the dispatcher is the *only* live thread and confirm
        // the entry is treated as non-deliverable (it stays queued).
        let state, dispatcher, _ = preparedState ()

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    Signals =
                        kernel.Signals
                        |> SignalState.enable Signal.SIGINT
                        |> SignalState.enqueue
                            {
                                Signal = Signal.SIGINT
                                Target = ValueNone
                            }
                }
            )

        let state' = SignalDispatch.trySpawnHandler baseClassTypes state

        let dispatcherTs = state'.ThreadState |> Map.find dispatcher
        dispatcherTs.Status |> shouldEqual ThreadStatus.Parked
        dispatcherTs.MethodStates.Count |> shouldEqual 0

        state'.Kernel.Signals
        |> SignalState.pending
        |> shouldEqual
            [
                {
                    Signal = Signal.SIGINT
                    Target = ValueNone
                }
            ]

    [<Test>]
    let ``trySpawnHandler rejects a handler with the wrong arity`` () : unit =
        // The validator must catch any handler that isn't (?, ?) -> int.
        // `String.IsNullOrEmpty(string) -> bool` is a static 1-arg method;
        // installing it should trip the arity check, not silently produce
        // a malformed handler frame.
        let state = baseState ()
        let state, dispatcher = IlMachineState.allocateParkedThread state

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    Signals = kernel.Signals |> SignalState.markInitialized dispatcher
                }
            )

        let state, method =
            concretizeStaticByArity state "System" "String" "IsNullOrEmpty" 1

        let handler = SignalHandler.ofMethodInfo method
        let runnableSibling = ThreadId 99

        let state =
            { state with
                ThreadState =
                    state.ThreadState
                    |> Map.add runnableSibling (stubThreadState ThreadStatus.Runnable)
            }

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    Signals =
                        kernel.Signals
                        |> SignalState.setHandler handler
                        |> SignalState.enable Signal.SIGINT
                        |> SignalState.enqueue
                            {
                                Signal = Signal.SIGINT
                                Target = ValueNone
                            }
                }
            )

        (fun () -> SignalDispatch.trySpawnHandler baseClassTypes state |> ignore)
        |> shouldFail<exn>

    [<Test>]
    let ``reParkAfterHandler restores the dispatcher to its idle shape`` () : unit =
        // Drive Parked → Runnable via `trySpawnHandler`, then assert the
        // inverse `reParkAfterHandler` transition removes the live frame,
        // restores the sentinel `FrameId -1`, and flips back to Parked.
        let state, dispatcher, _ = preparedState ()
        let runnableSibling = ThreadId 99

        let state =
            { state with
                ThreadState =
                    state.ThreadState
                    |> Map.add runnableSibling (stubThreadState ThreadStatus.Runnable)
            }

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    Signals =
                        kernel.Signals
                        |> SignalState.enable Signal.SIGINT
                        |> SignalState.enqueue
                            {
                                Signal = Signal.SIGINT
                                Target = ValueNone
                            }
                }
            )

        let state = SignalDispatch.trySpawnHandler baseClassTypes state
        let state = SignalDispatch.reParkAfterHandler dispatcher state

        let dispatcherTs = state.ThreadState |> Map.find dispatcher
        dispatcherTs.Status |> shouldEqual ThreadStatus.Parked
        dispatcherTs.MethodStates.Count |> shouldEqual 0
        dispatcherTs.ActiveMethodState |> shouldEqual (FrameId -1)
        dispatcherTs.NextFrameId |> shouldEqual 0

    [<Test>]
    let ``trySpawnHandler passes PosixSignalInvalid (0) for signals with no managed enum`` () : unit =
        // Real CoreCLR's `pal_signal.c` overwrites the `PosixSignal` callback
        // argument with `PosixSignalInvalid` (0) when the signo has no
        // negative `PosixSignal` enum value (SIGABRT, SIGUSR1, SIGUSR2,
        // SIGPIPE, and arbitrary `(PosixSignal)rawSigno` casts). The first
        // `signo` argument still carries the raw Linux signo. PawPrint must
        // match — passing the raw signo here instead of 0 would feed the
        // handler a `PosixSignal` value the real runtime never produces.
        let state, dispatcher, _ = preparedState ()
        let runnableSibling = ThreadId 99

        let state =
            { state with
                ThreadState =
                    state.ThreadState
                    |> Map.add runnableSibling (stubThreadState ThreadStatus.Runnable)
            }

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    Signals =
                        kernel.Signals
                        |> SignalState.enable Signal.SIGABRT
                        |> SignalState.enqueue
                            {
                                Signal = Signal.SIGABRT
                                Target = ValueNone
                            }
                }
            )

        let state' = SignalDispatch.trySpawnHandler baseClassTypes state

        let dispatcherTs = state'.ThreadState |> Map.find dispatcher
        let frame = dispatcherTs.MethodStates |> Map.find dispatcherTs.ActiveMethodState

        frame.Arguments.Length |> shouldEqual 2
        // signo: SIGABRT's Linux signo is 6.
        frame.Arguments.[0] |> shouldEqual (CliType.Numeric (CliNumericType.Int32 6))
        // posix enum: SIGABRT has no `PosixSignal` enum identity, so the
        // dispatcher must pass `PosixSignalInvalid` (0).
        frame.Arguments.[1] |> shouldEqual (CliType.Numeric (CliNumericType.Int32 0))

    [<Test>]
    let ``trySpawnHandler does not consider Terminated threads eligible receivers`` () : unit =
        // A `Terminated` thread is not signal-eligible: its OS-level thread
        // has exited, even though its final frames are intentionally
        // retained for `Join` observers. This means
        // `ThreadStatus.hasNoActiveFrame` returns `false` for a terminated
        // thread, and a naive "has a live frame" filter would mistakenly
        // include it. Set up a world where the only non-dispatcher thread
        // is `Terminated` and assert the pending signal is treated as
        // non-deliverable.
        let state, dispatcher, _ = preparedState ()
        let terminatedSibling = ThreadId 99

        let state =
            { state with
                ThreadState =
                    state.ThreadState
                    |> Map.add terminatedSibling (stubThreadState ThreadStatus.Terminated)
            }

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    Signals =
                        kernel.Signals
                        |> SignalState.enable Signal.SIGINT
                        |> SignalState.enqueue
                            {
                                Signal = Signal.SIGINT
                                Target = ValueNone
                            }
                }
            )

        let state' = SignalDispatch.trySpawnHandler baseClassTypes state

        let dispatcherTs = state'.ThreadState |> Map.find dispatcher
        dispatcherTs.Status |> shouldEqual ThreadStatus.Parked
        dispatcherTs.MethodStates.Count |> shouldEqual 0

        state'.Kernel.Signals
        |> SignalState.pending
        |> shouldEqual
            [
                {
                    Signal = Signal.SIGINT
                    Target = ValueNone
                }
            ]

    [<Test>]
    let ``trySpawnHandler does not consider NotStarted threads eligible receivers`` () : unit =
        // A managed `Thread` that has been constructed but never `Start`ed
        // has no kernel-level thread behind it: no OS thread exists to
        // receive the signal. PawPrint mirrors that by classifying
        // `NotStarted` threads as frameless via `ThreadStatus.hasNoActiveFrame`
        // and excluding them from the receiver candidate set. Set up a world
        // where the only non-dispatcher thread is `NotStarted` and assert the
        // pending signal is treated as non-deliverable (queue intact, frame
        // not spawned).
        let state, dispatcher, _ = preparedState ()
        let notStartedSibling = ThreadId 99

        let state =
            { state with
                ThreadState =
                    state.ThreadState
                    |> Map.add notStartedSibling (stubThreadState ThreadStatus.NotStarted)
            }

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    Signals =
                        kernel.Signals
                        |> SignalState.enable Signal.SIGINT
                        |> SignalState.enqueue
                            {
                                Signal = Signal.SIGINT
                                Target = ValueNone
                            }
                }
            )

        let state' = SignalDispatch.trySpawnHandler baseClassTypes state

        let dispatcherTs = state'.ThreadState |> Map.find dispatcher
        dispatcherTs.Status |> shouldEqual ThreadStatus.Parked
        dispatcherTs.MethodStates.Count |> shouldEqual 0

        state'.Kernel.Signals
        |> SignalState.pending
        |> shouldEqual
            [
                {
                    Signal = Signal.SIGINT
                    Target = ValueNone
                }
            ]
