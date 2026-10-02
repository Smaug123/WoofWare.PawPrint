namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// Focused tests for `SignalDispatch`, System.Native's signal handling
/// between guest instructions: its native handler, which writes each signal
/// the kernel delivers to the leader into the shim's signal pipe, and its
/// dispatcher, the auxiliary thread allocated by
/// `SystemNative_InitializeTerminalAndSignalHandling`, which is Parked in a
/// read of that pipe until a signal arrives. These pin down each guard path,
/// the pipe's contents, the dispatcher's wake (Parked → Runnable with a fresh
/// bottom frame on the handler) and its inverse `reParkAfterHandler`
/// (Runnable + bottom-frame `ret` → Parked), and what the loop does with a
/// signal nothing handles.
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
        BaseClassTypes.ofCorelib corelib

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll (LoadedAssemblies.ofAssemblies [ corelib ]) baseClassTypes AllConcreteTypes.Empty

    let private baseState () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let initialState = IlMachineState.initial loggerFactory ImmutableArray.Empty corelib

        { initialState with
            TypeSystem =
                { initialState.TypeSystem with
                    ConcreteTypes = concreteTypes
                }
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
            |> List.filter (fun m -> m.Name = methodName && m.IsStatic && (MethodInfo.arity m) = arity)
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
                corelib.DefinitionFullName
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

    /// A fresh machine on `platform`, with System.Native's signal handling
    /// initialised as the P/Invoke does it (a signal pipe and a parked
    /// dispatcher) and a handler installed: the common preamble for the tests
    /// that deliver a signal.
    let private preparedStateOn (platform : SimulatedUnixPlatform) : IlMachineState * ThreadId * SignalHandler =
        let state = baseState ()

        // The platform is fixed at construction, so the fresh state's machine,
        // untouched so far, is swapped for one of `platform`'s before anything
        // is made in it.
        let state =
            state.MapKernel (
                EmulatedKernel.mapMachine (fun _ ->
                    (EmulatedKernel.create platform StandardStreamsConfig.piped).Machine
                )
            )

        let state =
            match NativeSystemNative.initializeSignalHandling "test" state.Kernel.Leader state with
            | Ok state -> state
            | Error error -> failwith $"initialising signal handling failed with %O{error}"

        let dispatcher =
            PosixSignalShim.signalThread state.Kernel.PosixSignalShim
            |> Option.defaultWith (fun () -> failwith "initialisation recorded no dispatcher")

        let state, handler = installCompareAsHandler state

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    PosixSignalShim = kernel.PosixSignalShim |> PosixSignalShim.setHandler handler
                }
            )

        state, dispatcher, handler

    let private preparedState () : IlMachineState * ThreadId * SignalHandler =
        preparedStateOn SimulatedUnixPlatform.linuxX64

    let private stubThreadState (status : ThreadStatus) : ThreadState =
        {
            MethodStates = Map.empty
            YieldDebt = Set.empty
            NextFrameId = 0
            ActiveMethodState = FrameId -1
            Status = status
            IsBackground = false
            IsRaisingForeignException = false
            Name = None
        }

    let private numberingOf (state : IlMachineState) : SignalNumbering =
        SimulatedUnixPlatform.signalNumbering state.Kernel.UnixPlatform

    /// `SystemNative_EnablePosixSignalHandling` for `signal`: System.Native's
    /// handler installed, and the registration set.
    let private register (signal : Signal) (state : IlMachineState) : IlMachineState =
        state.MapKernel (fun kernel ->
            let signals, shim =
                PosixSignalShim.enable (numberingOf state) signal kernel.Signals kernel.PosixSignalShim

            { kernel with
                PosixSignalShim = shim
                Process =
                    { kernel.Process with
                        Signals = signals
                    }
            }
        )

    /// `SystemNative_DisablePosixSignalHandling` for `signal`.
    let private unregister (signal : Signal) (state : IlMachineState) : IlMachineState =
        state.MapKernel (fun kernel ->
            let signals, shim =
                PosixSignalShim.disable (numberingOf state) signal kernel.Signals kernel.PosixSignalShim

            { kernel with
                PosixSignalShim = shim
                Process =
                    { kernel.Process with
                        Signals = signals
                    }
            }
        )

    let private mapSignals
        (f : SignalState<ThreadId, NativeSignalHandler> -> SignalState<ThreadId, NativeSignalHandler>)
        (state : IlMachineState)
        : IlMachineState
        =
        state.MapKernel (fun kernel ->
            { kernel with
                Process =
                    { kernel.Process with
                        Signals = f kernel.Signals
                    }
            }
        )

    /// `signal`, generated for the whole process and pending there.
    let private sendToProcess (signal : Signal) (state : IlMachineState) : IlMachineState =
        mapSignals
            (SignalState.enqueue
                {
                    Signal = signal
                    Target = ValueNone
                })
            state

    let private withStatus (thread : ThreadId) (status : ThreadStatus) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (Option.map (fun ts ->
                        { ts with
                            Status = status
                        }
                    ))
        }

    let private withSibling (status : ThreadStatus) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState = state.ThreadState |> Map.add (ThreadId 99) (stubThreadState status)
        }

    let private poll (state : IlMachineState) : IlMachineState =
        match SignalDispatch.poll baseClassTypes state with
        | SignalPoll.Continues state -> state
        | SignalPoll.ProcessKilled (_, signal, _) -> failwith $"the poll killed the process with %O{signal}"

    let private pipeOf (state : IlMachineState) : SignalPipe =
        PosixSignalShim.signalPipe state.Kernel.PosixSignalShim
        |> Option.defaultWith (fun () -> failwith "signal handling is not initialised")

    /// The bytes in the signal pipe, oldest first, read from a copy of the
    /// kernel so that the state itself keeps them.
    let private pipeContents (state : IlMachineState) : byte list =
        let pipe = pipeOf state

        match
            UnixReadWrite.read
                state.Kernel.Leader
                pipe.ReadEnd
                UserBuffer.Mapped
                4096UL
                (EmulatedKernel.unix state.Kernel)
        with
        | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), _) -> List.ofSeq bytes
        | Ok (ReadOutcome.WouldBlock _, _) -> []
        | other -> failwith $"reading the signal pipe answered %O{other}"

    /// The arguments of the frame the dispatcher is running.
    let private callbackArguments (dispatcher : ThreadId) (state : IlMachineState) : CliType list =
        let ts = state.ThreadState |> Map.find dispatcher
        let frame = ts.MethodStates |> Map.find ts.ActiveMethodState
        List.ofSeq frame.Arguments

    let private int32Arg (value : int) : CliType =
        CliType.Numeric (CliNumericType.Int32 value)

    /// The callback's `ret`, returning `result`, and the loop's reaction.
    let private finishCallback (dispatcher : ThreadId) (result : int) (state : IlMachineState) : SignalPoll =
        state
        |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim result)) dispatcher
        |> SignalDispatch.reParkAfterHandler dispatcher

    [<Test>]
    let ``poll is a no-op when signal handling is not initialised`` () : unit =
        // No dispatcher allocated, no handler installed, no pending signals:
        // every guard fires and the state's signal subsystem must be
        // unchanged. `ThreadState` has no structural equality (its embedded
        // `MethodState` carries reference-typed payloads), so we check the
        // observable bits explicitly: thread-id keyspace and signal state.
        let state = baseState ()
        let state' = poll state

        state'.Kernel.Signals |> shouldEqual state.Kernel.Signals
        state'.Kernel.PosixSignalShim |> shouldEqual state.Kernel.PosixSignalShim

        let keysBefore = state.ThreadState |> Map.toList |> List.map fst
        let keysAfter = state'.ThreadState |> Map.toList |> List.map fst
        keysAfter |> shouldEqual keysBefore

    [<Test>]
    let ``initialisation makes a blocking pipe on the two lowest free descriptors, read end first`` () : unit =
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            let state, _, _ = preparedStateOn platform
            let pipe = pipeOf state

            // 0, 1 and 2 are the standard streams, so a fresh process's pipe is 3
            // and 4.
            pipe
            |> shouldEqual
                {
                    ReadEnd = 3
                    WriteEnd = 4
                }

            let descriptors = state.Kernel.Process.FileDescriptors

            let readEnd =
                FileDescriptorRegistry.tryFind pipe.ReadEnd descriptors
                |> Option.defaultWith (fun () -> failwith "no read end")

            let writeEnd =
                FileDescriptorRegistry.tryFind pipe.WriteEnd descriptors
                |> Option.defaultWith (fun () -> failwith "no write end")

            match readEnd.Target, writeEnd.Target with
            | OpenFileTarget.Pipe (readPipe, PipeEnd.Read), OpenFileTarget.Pipe (writePipe, PipeEnd.Write) ->
                readPipe |> shouldEqual writePipe
            | other -> failwith $"expected the two ends of one pipe, got %O{other}"

            readEnd.NonBlocking |> shouldEqual false
            writeEnd.NonBlocking |> shouldEqual false
            pipeContents state |> shouldEqual []

    [<Test>]
    let ``a second initialisation makes neither another pipe nor another dispatcher`` () : unit =
        let state, dispatcher, _ = preparedState ()
        let threads = state.ThreadState |> Map.keys |> Set.ofSeq

        let again =
            match NativeSystemNative.initializeSignalHandling "test" state.Kernel.Leader state with
            | Ok state -> state
            | Error error -> failwith $"%O{error}"

        again.Kernel |> shouldEqual state.Kernel
        again.ThreadState |> Map.keys |> Set.ofSeq |> shouldEqual threads

        PosixSignalShim.signalThread again.Kernel.PosixSignalShim
        |> shouldEqual (Some dispatcher)

    [<Test>]
    let ``the native handler writes a delivered signal's number into the pipe, whatever the dispatcher is doing``
        ()
        : unit
        =
        // The dispatcher is Runnable, mid-callback for an earlier signal. The
        // kernel still delivers to the leader, and System.Native's handler
        // still takes the signal off the kernel's pending set and into the pipe;
        // the dispatcher reads it once it is idle again.
        let state, dispatcher, _ = preparedState ()

        let state' =
            state
            |> withStatus dispatcher ThreadStatus.Runnable
            |> register Signal.SIGINT
            |> sendToProcess Signal.SIGINT
            |> poll

        state'.Kernel.Signals |> SignalState.pending |> shouldEqual []
        pipeContents state' |> shouldEqual [ 2uy ]

        (state'.ThreadState |> Map.find dispatcher).Status
        |> shouldEqual ThreadStatus.Runnable

    [<Test>]
    let ``poll is a no-op when there is no pending signal`` () : unit =
        let state, dispatcher, _ = preparedState ()
        let state' = state |> register Signal.SIGINT |> poll

        let dispatcherTs = state'.ThreadState |> Map.find dispatcher
        dispatcherTs.Status |> shouldEqual ThreadStatus.Parked
        dispatcherTs.MethodStates.Count |> shouldEqual 0
        pipeContents state' |> shouldEqual []

    [<Test>]
    let ``poll holds a pending signal every thread blocks`` () : unit =
        // The leader and the dispatcher, the process's only tasks, both block
        // SIGINT, so the entry stays queued and nothing reaches the pipe.
        // (SIGINT, because a Continue-default signal would surface whatever
        // the masks say.)
        let state, dispatcher, _ = preparedState ()

        let state' =
            state
            |> register Signal.SIGINT
            |> fun state ->
                state.MapKernel (
                    SignalFrames.enter state.Kernel.Leader (Set.singleton Signal.SIGINT)
                    >> SignalFrames.enter dispatcher (Set.singleton Signal.SIGINT)
                )
            |> sendToProcess Signal.SIGINT
            |> poll

        let dispatcherTs = state'.ThreadState |> Map.find dispatcher
        dispatcherTs.Status |> shouldEqual ThreadStatus.Parked
        dispatcherTs.MethodStates.Count |> shouldEqual 0
        pipeContents state' |> shouldEqual []

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
    let ``poll refuses a receivable signal whose kernel default it cannot apply yet`` () : unit =
        // A receivable pending signal nobody registered falls to its kernel
        // default — Terminate, for SIGINT. Generation applies that default
        // when a thread can receive the signal, so it can reach the poll only
        // by becoming receivable later; the poll must refuse it loudly rather
        // than leave it queued forever (the shape #1380 objected to) or
        // half-apply it.
        let state, _dispatcher, _ = preparedState ()

        let state =
            state |> withSibling ThreadStatus.Runnable |> sendToProcess Signal.SIGINT

        let exn = Assert.Throws (fun () -> poll state |> ignore<IlMachineState>)

        exn.Message |> shouldContainText "kernel default"

    [<Test>]
    let ``poll discards a receivable ignored signal and persists the discard`` () : unit =
        // SIGCHLD's kernel default is Ignore, and the test kernel simulates
        // Linux, so the non-registered entry survives generation and it is
        // the delivery scan that discards it. The scan produces no action —
        // nothing reaches the pipe, and the dispatcher stays Parked — but its
        // state change must be kept, or the next poll would discard the same
        // entry forever.
        let state, dispatcher, _ = preparedState ()

        let state =
            state |> withSibling ThreadStatus.Runnable |> sendToProcess Signal.SIGCHLD

        state.Kernel.Signals |> SignalState.pending |> List.length |> shouldEqual 1

        let state' = poll state

        let dispatcherTs = state'.ThreadState |> Map.find dispatcher
        dispatcherTs.Status |> shouldEqual ThreadStatus.Parked
        dispatcherTs.MethodStates.Count |> shouldEqual 0
        state'.Kernel.Signals |> SignalState.pending |> shouldEqual []
        pipeContents state' |> shouldEqual []

    [<Test>]
    let ``a busy dispatcher leaves the pipe unread`` () : unit =
        // Dispatcher is Runnable (already mid-callback from a prior wake); it
        // reads the next signal only once the callback returns, as the
        // single-threaded `SignalHandlerLoop` does.
        let state, dispatcher, _ = preparedState ()

        let state =
            state
            |> register Signal.SIGINT
            |> sendToProcess Signal.SIGINT
            |> withStatus dispatcher ThreadStatus.Runnable
            |> poll
            |> poll

        (state.ThreadState |> Map.find dispatcher).Status
        |> shouldEqual ThreadStatus.Runnable

        pipeContents state |> shouldEqual [ 2uy ]

    [<Test>]
    let ``a Parked dispatcher reads a signal from the pipe and runs the callback for it`` () : unit =
        // The positive path: dispatcher Parked, handler installed, signal
        // registered, pending entry present. The native handler writes it,
        // and in the same poll the dispatcher reads it back: Parked →
        // Runnable, a bottom frame on the handler, and the pipe empty again.
        let state, dispatcher, _ = preparedState ()

        let state' =
            state
            |> withSibling ThreadStatus.Runnable
            |> register Signal.SIGINT
            |> sendToProcess Signal.SIGINT
            |> poll

        let dispatcherTs = state'.ThreadState |> Map.find dispatcher
        dispatcherTs.Status |> shouldEqual ThreadStatus.Runnable
        // A handler frame has been installed; ActiveMethodState now points
        // at a live entry rather than the sentinel FrameId -1.
        dispatcherTs.MethodStates.ContainsKey dispatcherTs.ActiveMethodState
        |> shouldEqual true

        state'.Kernel.Signals |> SignalState.pending |> shouldEqual []
        pipeContents state' |> shouldEqual []

    [<Test>]
    let ``the callback is passed the signo and PosixSignal enum as int args`` () : unit =
        // For SIGINT: signo = 2, enum value = -2. The signo is read under the
        // state's platform, which is Linux here; for SIGINT it is 2 either way,
        // and the Darwin row below is what shows the platform is consulted.
        let state, dispatcher, _ = preparedState ()

        state
        |> register Signal.SIGINT
        |> sendToProcess Signal.SIGINT
        |> poll
        |> callbackArguments dispatcher
        |> shouldEqual [ int32Arg 2 ; int32Arg -2 ]

    [<Test>]
    let ``the pipe and the callback number the signal under the state's platform`` () : unit =
        // SIGCHLD is 17 on Linux and 20 on Darwin. A dispatcher that numbered
        // the signal under a fixed table would hand a Darwin guest's
        // `OnPosixSignal` a signo it never registered, and the BCL would find
        // no tokens for it and run nothing.
        let state, dispatcher, _ = preparedStateOn SimulatedUnixPlatform.macOsArm64

        let written =
            state
            |> withStatus dispatcher ThreadStatus.Runnable
            |> register Signal.SIGCHLD
            |> sendToProcess Signal.SIGCHLD
            |> poll

        pipeContents written |> shouldEqual [ 20uy ]

        state
        |> register Signal.SIGCHLD
        |> sendToProcess Signal.SIGCHLD
        |> poll
        |> callbackArguments dispatcher
        |> shouldEqual
            [
                int32Arg 20
                int32Arg (int System.Runtime.InteropServices.PosixSignal.SIGCHLD)
            ]

    [<Test>]
    let ``the callback is passed PosixSignalInvalid (0) for signals with no managed enum`` () : unit =
        // Real CoreCLR's `pal_signal.c` overwrites the `PosixSignal` callback
        // argument with `PosixSignalInvalid` (0) when the signo has no
        // negative `PosixSignal` enum value (SIGABRT, SIGUSR1, SIGUSR2,
        // SIGPIPE, and arbitrary `(PosixSignal)rawSigno` casts). The first
        // `signo` argument still carries the raw signo: SIGUSR2's, 12 under
        // Linux numbering.
        let state, dispatcher, _ = preparedState ()

        state
        |> register Signal.SIGUSR2
        |> sendToProcess Signal.SIGUSR2
        |> poll
        |> callbackArguments dispatcher
        |> shouldEqual [ int32Arg 12 ; int32Arg 0 ]

    /// Signals System.Native's handler can be registered for on a fresh
    /// PawPrint process under Linux numbering without chaining to a handler of
    /// the runtime's, and whose number is the same on Darwin.
    let private registrable : Signal list =
        [
            Signal.SIGHUP
            Signal.SIGINT
            Signal.SIGQUIT
            // SIGALRM, which has no case of its own.
            Signal.Other 14
            Signal.SIGTERM
            Signal.SIGWINCH
        ]

    [<Test>]
    let ``the dispatcher runs the callback for each signal in the order the native handler wrote it`` () : unit =
        // One signal per poll, while the dispatcher is busy: each reaches the
        // pipe in the order it was delivered, repeats included, and the
        // dispatcher then reads them in that order, one per callback. The
        // kernel's pending set would have merged the repeats and taken the
        // rest in its own order.
        let property (sent : Signal list) =
            let state, dispatcher, _ = preparedState ()

            // A first signal wakes the dispatcher onto its callback, which is
            // still running while the rest are sent.
            let state =
                (state, List.distinct (Signal.SIGHUP :: sent))
                ||> List.fold (fun state signal -> register signal state)
                |> sendToProcess Signal.SIGHUP
                |> poll

            (state.ThreadState |> Map.find dispatcher).Status
            |> shouldEqual ThreadStatus.Runnable

            let state =
                (state, sent)
                ||> List.fold (fun state signal -> state |> sendToProcess signal |> poll)

            let signo (signal : Signal) : int =
                Signal.toRawSignoUnder SignalNumbering.Linux signal

            pipeContents state |> shouldEqual (sent |> List.map (signo >> byte))

            // Each callback returns, and the dispatcher reads the next signal,
            // until the pipe is empty and it stays Parked.
            let rec drain (called : int list) (state : IlMachineState) : int list =
                let called =
                    match callbackArguments dispatcher state with
                    | CliType.Numeric (CliNumericType.Int32 signo) :: _ -> signo :: called
                    | other -> failwith $"unexpected callback arguments %O{other}"

                let state =
                    match finishCallback dispatcher 1 state with
                    | SignalPoll.Continues state -> poll state
                    | SignalPoll.ProcessKilled (_, signal, _) -> failwith $"killed by %O{signal}"

                match (state.ThreadState |> Map.find dispatcher).Status with
                | ThreadStatus.Parked -> List.rev called
                | _ -> drain called state

            drain [] state |> shouldEqual (signo Signal.SIGHUP :: List.map signo sent)

        let gen = Gen.elements registrable |> Gen.listOf |> Gen.map (List.truncate 12)

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 100, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``signals delivered at one return to user mode reach the pipe in the reverse of the order they were taken``
        ()
        : unit
        =
        // The kernel takes SIGINT (2) before SIGTERM (15) and gives each a
        // handler frame before any handler runs; the handlers run innermost
        // first, so System.Native's writes SIGTERM first (measured by
        // `signal-pick-order.c` and the signal fuzzer). No frame is left
        // afterwards.
        let state, dispatcher, _ = preparedState ()

        let state =
            state
            |> withStatus dispatcher ThreadStatus.Runnable
            |> register Signal.SIGINT
            |> register Signal.SIGTERM
            |> sendToProcess Signal.SIGTERM
            |> sendToProcess Signal.SIGINT
            |> poll

        pipeContents state |> shouldEqual [ 15uy ; 2uy ]
        SignalState.tasksWithFrames state.Kernel.Signals |> shouldEqual Set.empty
        EmulatedKernel.checkInvariants state.Kernel |> shouldEqual []

    [<Test>]
    let ``a signal the leader's handler blocks is written once that handler has returned`` () : unit =
        // Two instances of a real-time signal (Linux's 40): the first one's
        // handler blocks the signal, so the second waits for its sigreturn,
        // and is taken at the return to user mode that follows it, within the
        // same poll.
        let state, dispatcher, _ = preparedState ()
        let rt = Signal.Other 40

        let state =
            state
            |> withStatus dispatcher ThreadStatus.Runnable
            |> register rt
            |> sendToProcess rt
            |> sendToProcess rt

        state.Kernel.Signals |> SignalState.pending |> List.length |> shouldEqual 2

        let state = poll state
        pipeContents state |> shouldEqual [ 40uy ; 40uy ]
        state.Kernel.Signals |> SignalState.pending |> shouldEqual []
        SignalState.tasksWithFrames state.Kernel.Signals |> shouldEqual Set.empty

    [<Test>]
    let ``a signal read after its registration went is handled as not cancelled, by the dispatcher itself`` () : unit =
        // The native handler wrote SIGCHLD while it had a registration; the
        // registration is gone by the time the dispatcher reads it, so the
        // loop calls `SystemNative_HandleNonCanceledPosixSignal`, whose
        // SIGCHLD arm does nothing. No callback runs.
        let state, dispatcher, _ = preparedState ()

        let written =
            state
            |> withStatus dispatcher ThreadStatus.Runnable
            |> register Signal.SIGCHLD
            |> sendToProcess Signal.SIGCHLD
            |> poll
            |> unregister Signal.SIGCHLD
            |> withStatus dispatcher ThreadStatus.Parked

        pipeContents written |> shouldEqual [ 17uy ]

        let state' = poll written

        pipeContents state' |> shouldEqual []

        (state'.ThreadState |> Map.find dispatcher).Status
        |> shouldEqual ThreadStatus.Parked

    [<Test>]
    let ``the dispatcher reads on past a signal it handles without a callback, in the same poll`` () : unit =
        // SIGCHLD, whose registration has gone, and then SIGTERM, which is
        // registered, both in the pipe: the loop handles SIGCHLD itself and
        // goes straight back to its read, so one poll reaches SIGTERM's
        // callback. Leaving it for a later tick would report a deadlock when
        // every other thread is asleep.
        let state, dispatcher, _ = preparedState ()

        let written =
            state
            |> withStatus dispatcher ThreadStatus.Runnable
            |> register Signal.SIGCHLD
            |> register Signal.SIGTERM
            |> sendToProcess Signal.SIGCHLD
            |> poll
            |> sendToProcess Signal.SIGTERM
            |> poll
            |> unregister Signal.SIGCHLD
            |> withStatus dispatcher ThreadStatus.Parked

        pipeContents written |> shouldEqual [ 17uy ; 15uy ]

        let state' = poll written
        pipeContents state' |> shouldEqual []

        (state'.ThreadState |> Map.find dispatcher).Status
        |> shouldEqual ThreadStatus.Runnable

        callbackArguments dispatcher state' |> List.head |> shouldEqual (int32Arg 15)

    /// `state` with the descriptor `fd` replaced by a duplicate of standard
    /// output, as `dup2(1, fd)` would leave it.
    let private replacedByStdout (fd : int) (state : IlMachineState) : IlMachineState =
        state.MapKernel (fun kernel ->
            let system = EmulatedKernel.unix kernel

            let system =
                match UnixDescriptor.close fd system with
                | Ok (SyscallAnswer.Completed _, system) -> system
                | other -> failwith $"closing %d{fd} answered %O{other}"

            match UnixDescriptor.dup 1 system with
            | SyscallAnswer.Completed newFd, system when newFd = int64 fd -> EmulatedKernel.withUnix system kernel
            | other -> failwith $"duplicating stdout onto %d{fd} answered %O{other}"
        )

    [<Test>]
    let ``the native handler refuses a write end the guest has replaced with standard output`` () : unit =
        // The real handler writes its byte to whatever the descriptor now
        // names; on standard output that is guest-visible output, which
        // PawPrint would record without streaming it.
        let state, _dispatcher, _ = preparedState ()

        let exn =
            Assert.Throws (fun () ->
                state
                |> replacedByStdout (pipeOf state).WriteEnd
                |> register Signal.SIGINT
                |> sendToProcess Signal.SIGINT
                |> poll
                |> ignore<IlMachineState>
            )

        exn.Message |> shouldContainText "which the guest has replaced"

    [<Test>]
    let ``the dispatcher refuses an empty read end the guest has made non-blocking`` () : unit =
        // Its read then fails with EAGAIN rather than sleeping, and the real
        // SignalHandlerLoop closes the descriptor and exits.
        let state, _dispatcher, _ = preparedState ()

        let nonBlocking =
            state.MapKernel (fun kernel ->
                match UnixSocket.setNonBlocking (pipeOf state).ReadEnd true (EmulatedKernel.unix kernel) with
                | SetNonBlockingAnswer.Set, system -> EmulatedKernel.withUnix system kernel
                | other -> failwith $"setting O_NONBLOCK answered %O{other}"
            )

        let exn = Assert.Throws (fun () -> poll nonBlocking |> ignore<IlMachineState>)
        exn.Message |> shouldContainText "EAGAIN"

    [<Test>]
    let ``the dispatcher refuses a read end the guest has replaced`` () : unit =
        let state, _dispatcher, _ = preparedState ()

        let exn =
            Assert.Throws (fun () ->
                state
                |> replacedByStdout (pipeOf state).ReadEnd
                |> poll
                |> ignore<IlMachineState>
            )

        exn.Message |> shouldContainText "the guest has replaced it"

    [<Test>]
    let ``a terminating signal read after its registration went kills the process`` () : unit =
        // For SIGTERM the loop's `SystemNative_HandleNonCanceledPosixSignal`
        // restores the default it saved and re-raises the signal, which kills
        // the process.
        let state, dispatcher, _ = preparedState ()

        let written =
            state
            |> withStatus dispatcher ThreadStatus.Runnable
            |> register Signal.SIGTERM
            |> sendToProcess Signal.SIGTERM
            |> poll
            |> unregister Signal.SIGTERM
            |> withStatus dispatcher ThreadStatus.Parked

        match SignalDispatch.poll baseClassTypes written with
        | SignalPoll.ProcessKilled (_, signal, coreDumped) ->
            signal |> shouldEqual Signal.SIGTERM
            coreDumped |> shouldEqual false
        | SignalPoll.Continues _ -> failwith "expected SIGTERM to kill the process"

    [<Test>]
    let ``reParkAfterHandler restores the dispatcher to its idle shape`` () : unit =
        let state, dispatcher, _ = preparedState ()

        let state =
            state
            |> withSibling ThreadStatus.Runnable
            |> register Signal.SIGINT
            |> sendToProcess Signal.SIGINT
            |> poll

        // The handler's `ret` leaves its result on the dispatcher's stack: 1
        // is "handled".
        let state =
            match finishCallback dispatcher 1 state with
            | SignalPoll.Continues state -> state
            | SignalPoll.ProcessKilled _ -> failwith "a handled signal killed the process"

        let dispatcherTs = state.ThreadState |> Map.find dispatcher
        dispatcherTs.Status |> shouldEqual ThreadStatus.Parked
        dispatcherTs.MethodStates.Count |> shouldEqual 0
        dispatcherTs.ActiveMethodState |> shouldEqual (FrameId -1)
        dispatcherTs.NextFrameId |> shouldEqual 0

    [<Test>]
    let ``a callback reporting the signal unhandled gets the signal's default from the loop`` () : unit =
        // `OnPosixSignal` returns 0 when no registration for the signal is
        // left, and the real `SignalHandlerLoop` then calls
        // `SystemNative_HandleNonCanceledPosixSignal` itself: for SIGINT at
        // its saved default, that re-raises it and kills the process; for
        // SIGWINCH it does nothing, and the dispatcher goes back to the pipe.
        let state, dispatcher, _ = preparedState ()

        match
            state
            |> register Signal.SIGINT
            |> sendToProcess Signal.SIGINT
            |> poll
            |> finishCallback dispatcher 0
        with
        | SignalPoll.ProcessKilled (_, signal, _) -> signal |> shouldEqual Signal.SIGINT
        | SignalPoll.Continues _ -> failwith "expected SIGINT to kill the process"

        match
            state
            |> register Signal.SIGWINCH
            |> sendToProcess Signal.SIGWINCH
            |> poll
            |> finishCallback dispatcher 0
        with
        | SignalPoll.Continues state ->
            (state.ThreadState |> Map.find dispatcher).Status
            |> shouldEqual ThreadStatus.Parked
        | SignalPoll.ProcessKilled (_, signal, _) -> failwith $"SIGWINCH killed the process with %O{signal}"

    [<Test>]
    let ``the native handler refuses a pipe whose write end the guest closed`` () : unit =
        let state, _dispatcher, _ = preparedState ()

        let closed =
            match UnixDescriptor.close (pipeOf state).WriteEnd (EmulatedKernel.unix state.Kernel) with
            | Ok (SyscallAnswer.Completed _, system) -> state.MapKernel (EmulatedKernel.withUnix system)
            | other -> failwith $"closing the write end answered %O{other}"

        let exn =
            Assert.Throws (fun () ->
                closed
                |> register Signal.SIGINT
                |> sendToProcess Signal.SIGINT
                |> poll
                |> ignore<IlMachineState>
            )

        exn.Message |> shouldContainText "abort()"

    [<Test>]
    let ``the native handler refuses a full pipe, which blocks the leader`` () : unit =
        let state, dispatcher, _ = preparedState ()
        let pipe = pipeOf state

        // Fill the pipe to capacity through its write end.
        let rec fill (system : UnixSystem<ThreadId, NativeSignalHandler>) =
            match UnixReadWrite.admitWrite system.Leader pipe.WriteEnd UserBuffer.Mapped 4096UL system with
            | Ok (WriteOutcome.Returns (WriteAdmission.Transfer count, system)) ->
                match
                    UnixReadWrite.write
                        system.Leader
                        pipe.WriteEnd
                        (ImmutableArray.CreateRange (Array.create count 1uy))
                        system
                with
                | Ok (WriteOutcome.Returns (WriteAnswer.Completed _, system)) -> fill system
                | other -> failwith $"filling the pipe answered %O{other}"
            // Full: the write would sleep, and the pipe is kept as it was.
            | Ok (WriteOutcome.WouldBlock _) -> system
            | other -> failwith $"filling the pipe was admitted as %O{other}"

        let full =
            state.MapKernel (fun kernel -> EmulatedKernel.withUnix (fill (EmulatedKernel.unix kernel)) kernel)
            |> withStatus dispatcher ThreadStatus.Runnable

        let exn =
            Assert.Throws (fun () ->
                full
                |> register Signal.SIGINT
                |> sendToProcess Signal.SIGINT
                |> poll
                |> ignore<IlMachineState>
            )

        exn.Message |> shouldContainText "blocks until the dispatcher reads"

    [<Test>]
    let ``the dispatcher refuses a byte in its pipe that is no signal number`` () : unit =
        // Only a guest writing to the pipe itself can put one there; the loop
        // would index its tables out of bounds with it.
        let state, _dispatcher, _ = preparedState ()
        let pipe = pipeOf state

        let written =
            state.MapKernel (fun kernel ->
                let system = EmulatedKernel.unix kernel

                match UnixReadWrite.write system.Leader pipe.WriteEnd (ImmutableArray.Create 0uy) system with
                | Ok (WriteOutcome.Returns (WriteAnswer.Completed 1L, system)) -> EmulatedKernel.withUnix system kernel
                | other -> failwith $"writing to the pipe answered %O{other}"
            )

        let exn = Assert.Throws (fun () -> poll written |> ignore<IlMachineState>)
        exn.Message |> shouldContainText "no signal number"

    [<Test>]
    let ``a caught signal before signal handling is initialised is refused`` () : unit =
        // Only a hand-rolled `SystemNative_EnablePosixSignalHandling` installs
        // System.Native's handler before initialisation; the handler would then
        // write to descriptor -1 and abort().
        let state = baseState () |> withSibling ThreadStatus.Runnable

        let state =
            state
            |> mapSignals (
                SignalState.setDisposition
                    Signal.SIGINT
                    (SignalDisposition.Catch (SignalCatch.ofHandler NativeSignalHandler.SystemNative))
            )
            |> sendToProcess Signal.SIGINT

        let exn = Assert.Throws (fun () -> poll state |> ignore<IlMachineState>)
        exn.Message |> shouldContainText "never initialised"

    [<Test>]
    let ``poll never gives a signal to the dispatcher in place of the leader`` () : unit =
        // The kernel gives a signal sent to the process to its leader. With
        // the leader blocking SIGINT and the dispatcher not, a real kernel
        // would pick another thread, which PawPrint does not model: the poll
        // must refuse rather than run the handler as though the dispatcher
        // had received it.
        let state, _dispatcher, _ = preparedState ()

        let state =
            state
            |> register Signal.SIGINT
            |> fun state -> state.MapKernel (SignalFrames.enter state.Kernel.Leader (Set.singleton Signal.SIGINT))
            |> sendToProcess Signal.SIGINT

        let exn = Assert.Throws (fun () -> poll state |> ignore<IlMachineState>)
        exn.Message |> shouldContainText "LeaderBlocks"

    [<Test>]
    let ``poll refuses a dispatcher that is the leader`` () : unit =
        // The dispatcher runs handlers for the leader, so it cannot be the
        // leader; a state that says it is has lost track of which thread is
        // which.
        let state, _dispatcher, handler = preparedState ()
        let leader = state.Kernel.Leader

        let state =
            { state with
                ThreadState = state.ThreadState |> Map.add leader (stubThreadState ThreadStatus.Parked)
            }

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    PosixSignalShim =
                        PosixSignalShim.initial
                        |> PosixSignalShim.markInitialized
                            leader
                            {
                                ReadEnd = 3
                                WriteEnd = 4
                            }
                        |> PosixSignalShim.setHandler handler
                }
            )

        let exn = Assert.Throws (fun () -> poll state |> ignore<IlMachineState>)
        exn.Message |> shouldContainText "is the process's leader"

    [<Test>]
    let ``poll rejects a handler with the wrong arity`` () : unit =
        // The validator must catch any handler that isn't (?, ?) -> int.
        // `String.IsNullOrEmpty(string) -> bool` is a static 1-arg method;
        // installing it should trip the arity check, not silently produce
        // a malformed handler frame.
        let state, _dispatcher, _ = preparedState ()

        let state, method =
            concretizeStaticByArity state "System" "String" "IsNullOrEmpty" 1

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    PosixSignalShim =
                        kernel.PosixSignalShim
                        |> PosixSignalShim.setHandler (SignalHandler.ofMethodInfo method)
                }
            )
            |> register Signal.SIGINT
            |> sendToProcess Signal.SIGINT

        (fun () -> poll state |> ignore) |> shouldFail<exn>

    [<Test>]
    let ``the dispatcher refuses a registered signal while no handler is installed`` () : unit =
        // `SetPosixSignalHandler` never called. The real shim's
        // `SignalHandlerLoop` asserts `g_posixSignalHandler != NULL` before
        // calling through it, so there is no behaviour to model: a release
        // build would call a null function pointer.
        let state = baseState ()

        let state =
            match NativeSystemNative.initializeSignalHandling "test" state.Kernel.Leader state with
            | Ok state -> state
            | Error error -> failwith $"%O{error}"

        let state =
            state
            |> withSibling ThreadStatus.Runnable
            |> register Signal.SIGINT
            |> sendToProcess Signal.SIGINT

        let exn = Assert.Throws (fun () -> poll state |> ignore<IlMachineState>)
        exn.Message |> shouldContainText "no handler"

    [<Test>]
    let ``poll delivers to the leader whatever state the other threads are in`` () : unit =
        // Which thread takes a signal sent to the process is the kernel's
        // answer, and it is the leader: not the lowest-numbered thread that
        // happens to be running, so no other thread's status enters into it.
        for sibling in
            [
                ThreadStatus.Terminated
                ThreadStatus.NotStarted (CpuId 0)
                ThreadStatus.Runnable
            ] do
            let state, dispatcher, _ = preparedState ()

            let state' =
                state
                |> withSibling sibling
                |> register Signal.SIGINT
                |> sendToProcess Signal.SIGINT
                |> poll

            (state'.ThreadState |> Map.find dispatcher).Status
            |> shouldEqual ThreadStatus.Runnable

            state'.Kernel.Signals |> SignalState.pending |> shouldEqual []

    [<Test>]
    let ``poll runs the runtime's fault handler for a sent SIGILL, which restores the default`` () : unit =
        // SIGILL is caught by CoreCLR's PAL from startup, over the default it
        // saves; sent the signal, the handler puts that default back and
        // returns.
        let state, _dispatcher, _ = preparedState ()
        let sigill = Signal.Other 4

        let state' = state |> sendToProcess sigill |> poll

        SignalState.disposition sigill state'.Kernel.Signals
        |> shouldEqual SignalDisposition.Default

        state'.Kernel.Signals |> SignalState.pending |> shouldEqual []
        pipeContents state' |> shouldEqual []

    [<Test>]
    let ``with SIGILL registered, System.Native's handler runs the runtime's first, whose default replaces it``
        ()
        : unit
        =
        // Registering SIGILL installs System.Native's handler over the PAL's,
        // which the shim's handler calls first: the PAL's restore leaves the
        // default installed over System.Native's own, and System.Native still
        // hands the signal to the callback. SIGILL has no PosixSignal member.
        let state, dispatcher, _ = preparedState ()
        let sigill = Signal.Other 4

        let state' = state |> register sigill |> sendToProcess sigill |> poll

        SignalState.disposition sigill state'.Kernel.Signals
        |> shouldEqual SignalDisposition.Default

        callbackArguments dispatcher state' |> shouldEqual [ int32Arg 4 ; int32Arg 0 ]

    [<Test>]
    let ``poll aborts the process for a SIGILL whose fault handler replaced an ignore`` () : unit =
        // A launcher that left SIGILL ignored: the PAL saved the ignore, and
        // its handler, sent the signal, calls `PROCAbort`.
        let state, _dispatcher, _ = preparedState ()
        let sigill = Signal.Other 4

        let state =
            state
            |> mapSignals (
                SignalState.setDisposition
                    sigill
                    (SignalDisposition.Catch (
                        SignalCatch.ofHandler (NativeSignalHandler.CoreClrPalFault PalReplacedDisposition.Ignore)
                    ))
            )
            |> sendToProcess sigill

        match SignalDispatch.poll baseClassTypes state with
        | SignalPoll.ProcessKilled (_, signal, _) -> signal |> shouldEqual Signal.SIGABRT
        | SignalPoll.Continues _ -> failwith "expected the poll to abort the process"

    [<Test>]
    let ``poll refuses a signal caught by any other handler installed before Main`` () : unit =
        // The PAL's thread-activation handler: SIGRTMIN, 34, on Linux.
        let state, _dispatcher, _ = preparedState ()
        let state = state |> sendToProcess (Signal.Other 34)

        let exn = Assert.Throws (fun () -> poll state |> ignore<IlMachineState>)
        exn.Message |> shouldContainText "installed before Main"
