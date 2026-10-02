namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `NativeLibc.screenSelfSignal`: which signals sent by a process to itself
/// PawPrint refuses to answer, because the kernel model's answer would differ
/// from a real CoreCLR process's; and `StartupSignalDispositions`, the table
/// those answers start from.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNativeLibc =

    let private everyNumbering : SignalNumbering list =
        [ SignalNumbering.Linux ; SignalNumbering.Darwin ]

    /// The main thread, which receives every signal sent to the process.
    let private leader : int = 0

    let private platformOf (numbering : SignalNumbering) : SimulatedUnixPlatform =
        match numbering with
        | SignalNumbering.Linux -> SimulatedUnixPlatform.linuxX64
        | SignalNumbering.Darwin -> SimulatedUnixPlatform.macOsArm64

    /// A process whose launcher left `inheritedIgnores` ignored, as it stands
    /// at Main.
    let private launched
        (numbering : SignalNumbering)
        (inheritedIgnores : Set<Signal>)
        : UnixSystem<int, NativeSignalHandler>
        =
        UnixSystem.initial (platformOf numbering) UnixSystem.pipedStandardStreams leader (CpuId 0)
        |> StartupSignalDispositions.install "test" numbering inheritedIgnores

    let private initial (numbering : SignalNumbering) : UnixSystem<int, NativeSignalHandler> =
        launched numbering Set.empty

    let private numberingOf (system : UnixSystem<int, NativeSignalHandler>) : SignalNumbering =
        SimulatedUnixPlatform.signalNumbering system.Machine.UnixPlatform

    /// `system` with `entry` pending, as though it had been generated while
    /// every task that could take it blocked it.
    let private enqueue
        (entry : PendingSignal<int>)
        (system : UnixSystem<int, NativeSignalHandler>)
        : UnixSystem<int, NativeSignalHandler>
        =
        { system with
            Process =
                { system.Process with
                    Signals = SignalState.enqueue entry system.Process.Signals
                }
        }

    /// `system` once `entry` has been generated among the tasks `leader` and
    /// `worker`, failing the test unless the process carries on.
    let private generateAmong
        (worker : int)
        (entry : PendingSignal<int>)
        (system : UnixSystem<int, NativeSignalHandler>)
        : UnixSystem<int, NativeSignalHandler>
        =
        match
            SignalState.generate
                CoreDumps.Suppressed
                leader
                (Set.ofList [ leader ; worker ])
                entry
                system.Process.Signals
        with
        | Ok (SignalGeneration.ProcessContinues signals) ->
            { system with
                Process =
                    { system.Process with
                        Signals = signals
                    }
            }
        | other -> failwith $"generating %A{entry}: %A{other}"

    let private signal (numbering : SignalNumbering) (signo : int) : Signal =
        match Signal.ofRawSignoUnder numbering signo with
        | ValueSome signal -> signal
        | ValueNone -> failwith $"%d{signo} is not a signal under %O{numbering}"

    let private processDirected (signal : Signal) : PendingSignal<int> =
        {
            Signal = signal
            Target = ValueNone
        }

    /// `InstallSignalHandler`'s effect on the two halves.
    let private register
        (numbering : SignalNumbering)
        (sent : Signal)
        (system : UnixSystem<int, NativeSignalHandler>, shim : PosixSignalShim)
        : UnixSystem<int, NativeSignalHandler> * PosixSignalShim
        =
        match PosixSignalShim.installHandler numbering sent system shim with
        | Ok installed -> installed
        | Error errno -> failwith $"installing System.Native's handler for %O{sent} failed with %O{errno}"

    /// `screenSelfSignal` for a signal `sender` sends, on `platform`.
    let private screenOn
        (platform : SimulatedUnixPlatform)
        (sender : int)
        (system : UnixSystem<int, NativeSignalHandler>, shim : PosixSignalShim)
        (sent : Signal)
        : UnmodelledSelfSignal option
        =
        NativeLibc.screenSelfSignal platform sender leader shim system sent

    let private everyPlatform : SimulatedUnixPlatform list =
        [
            SimulatedUnixPlatform.linuxX64
            SimulatedUnixPlatform.linuxArm64
            SimulatedUnixPlatform.macOsArm64
        ]

    /// `screenOn` for a signal the main thread sends, on a platform whose
    /// signals are numbered as `signals`' are.
    let private screen
        (system : UnixSystem<int, NativeSignalHandler>, shim : PosixSignalShim)
        (sent : Signal)
        : UnmodelledSelfSignal option
        =
        screenOn (platformOf (numberingOf system)) leader (system, shim) sent

    let private fresh (numbering : SignalNumbering) : UnixSystem<int, NativeSignalHandler> * PosixSignalShim =
        initial numbering, PosixSignalShim.initial

    let private faultHandler : NativeSignalHandler =
        NativeSignalHandler.CoreClrPalFault PalReplacedDisposition.Default

    /// Both columns of the table, as measured (see the comments in
    /// `StartupSignalDispositions`); `TestStartupSignalDispositions` checks
    /// the host's column against the real runtime. Which of the runtime's
    /// handlers is the fault handler is measured too: a process survives only
    /// the first of the signals it catches.
    let private measured (numbering : SignalNumbering) : Map<int, SignalDisposition<NativeSignalHandler>> =
        let catch (handler : NativeSignalHandler) =
            SignalDisposition.Catch (SignalCatch.ofHandler handler)

        match numbering with
        | SignalNumbering.Linux ->
            Map.ofList
                [
                    for signo in [ 4 ; 6 ; 7 ; 8 ; 11 ] do
                        signo, catch faultHandler
                    5, catch NativeSignalHandler.CoreClrPalTrap
                    34, catch NativeSignalHandler.CoreClrPalActivation
                    33, catch NativeSignalHandler.GlibcSetXid
                    13, SignalDisposition.Ignore
                ]
        | SignalNumbering.Darwin ->
            Map.ofList
                [
                    for signo in [ 4 ; 6 ; 8 ; 10 ; 11 ] do
                        signo, catch faultHandler
                    30, catch NativeSignalHandler.CoreClrPalActivation
                    13, SignalDisposition.Ignore
                ]

    [<Test>]
    let ``the startup dispositions are the measured ones`` () : unit =
        for numbering in everyNumbering do
            // Which handler catches each signal is measured; the handlers' own
            // masks and flags are the PAL's source, pinned below.
            let actual =
                KernelSignals.dispositions (initial numbering)
                |> Map.toSeq
                |> Seq.map (fun (signal, disposition) ->
                    let handlerOnly =
                        match disposition with
                        | SignalDisposition.Catch action ->
                            SignalDisposition.Catch (SignalCatch.ofHandler action.Handler)
                        | other -> other

                    Signal.toRawSignoUnder numbering signal, handlerOnly
                )
                |> Map.ofSeq

            (numbering, actual) |> shouldEqual (numbering, measured numbering)

    [<Test>]
    let ``the runtime's and glibc's handlers restart system calls, and the PAL's SIGSEGV masks its activation signal``
        ()
        : unit
        =
        // pal/src/exception/signal.cpp `handle_signal`: SA_RESTART, an empty
        // sa_mask, and on Linux SA_ONSTACK for SIGSEGV, whose mask then holds
        // the activation signal (34). glibc's SIGSETXID handler's flags were
        // read back as SA_SIGINFO | SA_RESTART | SA_RESTORER.
        for numbering in everyNumbering do
            for KeyValue (signal, disposition) in KernelSignals.dispositions (initial numbering) do
                match disposition with
                | SignalDisposition.Catch action ->
                    action.Restart |> shouldEqual true
                    action.NoDefer |> shouldEqual false
                    action.ResetHand |> shouldEqual false

                    let expectedMask =
                        match numbering, Signal.toRawSignoUnder numbering signal with
                        | SignalNumbering.Linux, 11 -> Set.singleton (Signal.Other 34)
                        | _ -> Set.empty

                    action.Mask |> shouldEqual expectedMask
                | SignalDisposition.Ignore
                | SignalDisposition.Default -> ()

    [<Test>]
    let ``inherited ignores stay ignored except where the runtime installs its own handler`` () : unit =
        // Measured by starting the startup probes with every catchable signal
        // ignored (see the comment in StartupSignalDispositions): the runtime's
        // handlers replaced exactly the ignores of the signals it catches.
        for numbering in everyNumbering do
            let ignorable =
                [ 1 .. Signal.highestSignoUnder numbering ]
                |> List.map (signal numbering)
                |> List.filter (fun s -> not (Signal.isUncatchableUnder numbering s) && s <> Signal.SIGTERM)

            let state : UnixSystem<int, NativeSignalHandler> =
                launched numbering (Set.ofList ignorable)

            for s in ignorable do
                // The fault handler saves the ignore it replaced.
                let expected =
                    match Map.tryFind (Signal.toRawSignoUnder numbering s) (measured numbering) with
                    | Some (SignalDisposition.Catch action) when action.Handler = faultHandler ->
                        SignalDisposition.Catch (
                            SignalCatch.ofHandler (NativeSignalHandler.CoreClrPalFault PalReplacedDisposition.Ignore)
                        )
                    | Some (SignalDisposition.Catch action) -> SignalDisposition.Catch action
                    | Some _
                    | None -> SignalDisposition.Ignore

                let actual =
                    match KernelSignals.disposition s state with
                    | SignalDisposition.Catch action -> SignalDisposition.Catch (SignalCatch.ofHandler action.Handler)
                    | other -> other

                (numbering, s, actual) |> shouldEqual (numbering, s, expected)

    [<Test>]
    let ``inherited ignores refuse what a launcher cannot ignore, and SIGTERM`` () : unit =
        for numbering in everyNumbering do
            let refused =
                [
                    yield Signal.Other 9
                    yield Signal.SIGTERM
                    match numbering with
                    | SignalNumbering.Linux ->
                        yield Signal.Other 19
                        yield Signal.Other 32
                        yield Signal.Other 33
                    | SignalNumbering.Darwin ->
                        yield Signal.Other 17
                        yield Signal.Other 32
                ]

            for s in refused do
                StartupSignalDispositions.refusal numbering (Set.ofList [ Signal.SIGHUP ; s ])
                |> Option.isSome
                |> shouldEqual true

                Assert.Throws (fun () ->
                    launched numbering (Set.singleton s)
                    |> ignore<UnixSystem<int, NativeSignalHandler>>
                )
                |> ignore<exn>

            StartupSignalDispositions.refusal numbering (Set.ofList [ Signal.SIGHUP ; Signal.SIGUSR2 ])
            |> shouldEqual None

    /// The fault signals the main thread may send itself on `platform`:
    /// those whose handler, once it has restored the default, is not needed
    /// for a later hardware fault (see the comment in `NativeLibc`).
    let private answeredFaultSignos (platform : SimulatedUnixPlatform) : Set<int> =
        match SimulatedUnixPlatform.flavour platform, SimulatedUnixPlatform.architecture platform with
        | SimulatedUnixFlavour.Linux, SimulatedUnixArchitecture.X64 -> Set.ofList [ 4 ; 6 ]
        | SimulatedUnixFlavour.Linux, SimulatedUnixArchitecture.Arm64 -> Set.ofList [ 4 ; 6 ; 8 ]
        | SimulatedUnixFlavour.Darwin, _ -> Set.ofList [ 4 ; 6 ; 8 ; 10 ; 11 ]

    let private faultSignos (numbering : SignalNumbering) : int list =
        measured numbering
        |> Map.toList
        |> List.choose (fun (signo, disposition) ->
            if disposition = SignalDisposition.Catch (SignalCatch.ofHandler faultHandler) then
                Some signo
            else
                None
        )

    [<Test>]
    let ``the main thread's fault signal is answered, registered or not, unless its handler is needed later``
        ()
        : unit
        =
        for platform in everyPlatform do
            let numbering = SimulatedUnixPlatform.signalNumbering platform

            for signo in faultSignos numbering do
                let expected =
                    if Set.contains signo (answeredFaultSignos platform) then
                        None
                    else
                        Some (UnmodelledSelfSignal.FaultHandlerNeededLater (signal numbering signo))

                (platform, signo, screenOn platform leader (fresh numbering) (Signal.Other signo))
                |> shouldEqual (platform, signo, expected)

                // System.Native's handler runs the fault handler first, so the
                // same answer holds with a registration.
                let registered = register numbering (Signal.Other signo) (fresh numbering)

                PosixSignalShim.chainsToNativeHandler numbering (signal numbering signo) (snd registered)
                |> shouldEqual (Some faultHandler)

                (platform, signo, screenOn platform leader registered (Signal.Other signo))
                |> shouldEqual (platform, signo, expected)

    [<Test>]
    let ``a fault signal another thread sends is refused, registered or not`` () : unit =
        for platform in everyPlatform do
            let numbering = SimulatedUnixPlatform.signalNumbering platform

            for signo in faultSignos numbering do
                let expected =
                    Some (UnmodelledSelfSignal.FaultSignalFromOtherThread (signal numbering signo))

                screenOn platform 1 (fresh numbering) (Signal.Other signo)
                |> shouldEqual expected

                screenOn platform 1 (register numbering (Signal.Other signo) (fresh numbering)) (Signal.Other signo)
                |> shouldEqual expected

    [<Test>]
    let ``over an inherited ignore, the main thread's fault signal is answered on every platform`` () : unit =
        // The handler aborts the process at once, so no later fault meets the
        // default it would otherwise have restored.
        for platform in everyPlatform do
            let numbering = SimulatedUnixPlatform.signalNumbering platform
            let signos = faultSignos numbering

            let ignored : UnixSystem<int, NativeSignalHandler> =
                launched numbering (signos |> List.map (signal numbering) |> Set.ofList)

            for signo in signos do
                (platform, signo, screenOn platform leader (ignored, PosixSignalShim.initial) (Signal.Other signo))
                |> shouldEqual (platform, signo, None)

    [<Test>]
    let ``a signal any other native handler catches from startup is refused, whatever the state`` () : unit =
        for numbering in everyNumbering do
            for KeyValue (signo, disposition) in measured numbering do
                match disposition with
                | SignalDisposition.Catch {
                                              Handler = NativeSignalHandler.CoreClrPalFault _
                                          } -> ()
                | SignalDisposition.Catch {
                                              Handler = handler
                                          } ->
                    let sent = signal numbering signo

                    screen (fresh numbering) (Signal.Other signo)
                    |> shouldEqual (Some (UnmodelledSelfSignal.NativeHandler (sent, handler)))

                    // Registered and pending: System.Native's handler runs
                    // the runtime's first. (Linux's 33 is glibc's, which no
                    // handler can be registered for.)
                    if not (Signal.isUncatchableUnder numbering sent) then
                        let signals, shim = register numbering (Signal.Other signo) (fresh numbering)

                        match KernelSignals.disposition sent signals with
                        | SignalDisposition.Catch action ->
                            action.Handler |> shouldEqual NativeSignalHandler.SystemNative
                            // Over the runtime's handler, System.Native keeps its
                            // mask and flags, and adds SA_RESTART.
                            action.Restart |> shouldEqual true
                        | other -> failwith $"expected System.Native's handler, got %A{other}"

                        let registered = enqueue (processDirected sent) signals, shim

                        screen registered (Signal.Other signo)
                        |> shouldEqual (Some (UnmodelledSelfSignal.NativeHandler (sent, handler)))
                | SignalDisposition.Ignore
                | SignalDisposition.Default -> ()

    [<Test>]
    let ``SIGPIPE is answered, registered or not, and stays ignored`` () : unit =
        for numbering in everyNumbering do
            screen (fresh numbering) Signal.SIGPIPE |> shouldEqual None

            let signals, shim = register numbering Signal.SIGPIPE (fresh numbering)

            KernelSignals.disposition Signal.SIGPIPE signals
            |> shouldEqual SignalDisposition.Ignore

            screen (signals, shim) Signal.SIGPIPE |> shouldEqual None

            // Unregistering restores the ignore the shim saved.
            let restored, _, refused =
                PosixSignalShim.restoreHandler numbering Signal.SIGPIPE signals shim

            refused |> shouldEqual None

            restored
            |> KernelSignals.disposition Signal.SIGPIPE
            |> shouldEqual SignalDisposition.Ignore

    /// A write into a pipe with no reader raises SIGPIPE at the writer on
    /// Linux and at the process on Darwin. What stays pending on a thread other
    /// than the main thread is never delivered, so it is refused; a signal
    /// discarded as it was raised, or pending on the process or the main
    /// thread, is not.
    [<Test>]
    let ``a raised signal left pending on a thread other than the main thread is refused`` () : unit =
        let worker = 1

        let dispositions : SignalDisposition<NativeSignalHandler> list =
            [
                SignalDisposition.Ignore
                SignalDisposition.Catch (SignalCatch.ofHandler NativeSignalHandler.SystemNative)
            ]

        for platform in everyPlatform do
            let numbering = SimulatedUnixPlatform.signalNumbering platform

            for disposition in dispositions do
                for sender in [ leader ; worker ] do
                    let before =
                        initial numbering |> KernelSignals.setDisposition Signal.SIGPIPE disposition

                    let raised : PendingSignal<int> =
                        {
                            Signal = Signal.SIGPIPE
                            Target =
                                match SimulatedUnixPlatform.flavour platform with
                                | SimulatedUnixFlavour.Linux -> ValueSome sender
                                | SimulatedUnixFlavour.Darwin -> ValueNone
                        }

                    let after = generateAmong worker raised before

                    let expected =
                        match disposition, raised.Target with
                        | SignalDisposition.Catch _, ValueSome target when target <> leader ->
                            Some (UnmodelledSelfSignal.PendingOnOtherThread Signal.SIGPIPE)
                        | _ -> None

                    NativeLibc.screenRaisedSignal platform sender leader PosixSignalShim.initial before raised after
                    |> shouldEqual expected

    /// `raise(3)` aims the signal at the thread that raises it, which takes it
    /// inside its own call, as the main thread takes one it sends the process
    /// with `kill(2)`. A hardware-fault signal the main thread raises is
    /// answered as its own `kill` is; one another thread raises is refused for
    /// being left pending there, not for being taken by a thread that did not
    /// send it.
    [<Test>]
    let ``a hardware-fault signal a thread raises is screened as taken by that thread`` () : unit =
        let worker = 1

        for platform in everyPlatform do
            let numbering = SimulatedUnixPlatform.signalNumbering platform
            let sigill = signal numbering 4
            let before = initial numbering

            for raiser in [ leader ; worker ] do
                let raised : PendingSignal<int> =
                    {
                        Signal = sigill
                        Target = ValueSome raiser
                    }

                let after = generateAmong worker raised before

                let expected =
                    if raiser = leader then
                        None
                    else
                        Some (UnmodelledSelfSignal.PendingOnOtherThread sigill)

                (platform,
                 raiser,
                 NativeLibc.screenRaisedSignal platform raiser leader PosixSignalShim.initial before raised after)
                |> shouldEqual (platform, raiser, expected)

    [<Test>]
    let ``System.Native installs its handler once until it restores it, whatever the disposition does meanwhile``
        ()
        : unit
        =
        // The runtime's fault handler, run first by System.Native's, restores
        // the default over System.Native's handler; installing again then does
        // nothing (`g_handlerIsInstalled`), until a restore puts the runtime's
        // handler back and forgets the installation.
        for numbering in everyNumbering do
            let sigill = signal numbering 4
            let signals, shim = register numbering sigill (fresh numbering)

            let restoredByRuntime =
                KernelSignals.setDisposition sigill SignalDisposition.Default signals

            let again, shimAgain = register numbering sigill (restoredByRuntime, shim)

            KernelSignals.disposition sigill again |> shouldEqual SignalDisposition.Default
            shimAgain |> shouldEqual shim

            let restored, shimRestored, refused =
                PosixSignalShim.restoreHandler numbering sigill restoredByRuntime shim

            refused |> shouldEqual None

            match KernelSignals.disposition sigill restored with
            | SignalDisposition.Catch action -> action.Handler |> shouldEqual faultHandler
            | other -> failwith $"expected the runtime's handler back, got %A{other}"

            match KernelSignals.disposition sigill (fst (register numbering sigill (restored, shimRestored))) with
            | SignalDisposition.Catch action -> action.Handler |> shouldEqual NativeSignalHandler.SystemNative
            | other -> failwith $"expected System.Native's handler installed afresh, got %A{other}"

    [<Test>]
    let ``every other signal is answered from a fresh state, but SIGCONT`` () : unit =
        for numbering in everyNumbering do
            for signo in 1 .. Signal.highestSignoUnder numbering do
                match Map.tryFind signo (measured numbering) with
                | Some (SignalDisposition.Catch _) -> ()
                | _ ->
                    let expected =
                        if signal numbering signo = Signal.SIGCONT then
                            Some (UnmodelledSelfSignal.ContinueWithoutHandler Signal.SIGCONT)
                        else
                            None

                    (numbering, signo, screen (fresh numbering) (signal numbering signo))
                    |> shouldEqual (numbering, signo, expected)

    [<Test>]
    let ``SIGCONT is answered once a handler is registered for it, or once it is ignored`` () : unit =
        for numbering in everyNumbering do
            screen (register numbering Signal.SIGCONT (fresh numbering)) Signal.SIGCONT
            |> shouldEqual None

            let ignored : UnixSystem<int, NativeSignalHandler> =
                launched numbering (Set.singleton Signal.SIGCONT)

            screen (ignored, PosixSignalShim.initial) Signal.SIGCONT |> shouldEqual None

    [<Test>]
    let ``a registered signal is answered whatever is already pending`` () : unit =
        // The kernel's pending set holds only what the kernel has not
        // delivered: once System.Native's handler has taken a signal it is a
        // byte in the shim's pipe, which no later signal merges with,
        // discards or overtakes. So nothing pending, registered or not, the
        // same signal or another, process- or thread-directed, is a reason to
        // refuse sending a registered signal.
        let candidates =
            [
                Signal.SIGHUP
                Signal.SIGINT
                Signal.SIGQUIT
                Signal.SIGTERM
                Signal.SIGWINCH
                Signal.SIGTSTP
                Signal.SIGCONT
                Signal.SIGUSR2
            ]

        let property (numbering : SignalNumbering) (registeredMask : bool list) (pending : (int * bool) list) =
            let registered =
                List.zip candidates (List.truncate candidates.Length (registeredMask @ List.replicate 8 false))
                |> List.filter snd
                |> List.map fst

            let signals, shim =
                (fresh numbering, registered)
                ||> List.fold (fun state signal -> register numbering signal state)

            let signals =
                (signals, pending)
                ||> List.fold (fun signals (index, aimedAtThread) ->
                    let signal =
                        candidates.[((index % candidates.Length) + candidates.Length) % candidates.Length]

                    enqueue
                        {
                            Signal = signal
                            Target = if aimedAtThread then ValueSome 3 else ValueNone
                        }
                        signals
                )

            for sent in registered do
                screen (signals, shim) sent |> shouldEqual None

                screen (signals, shim) (Signal.Other (Signal.toRawSignoUnder numbering sent))
                |> shouldEqual None

        let gen =
            gen {
                let! numbering = Gen.elements everyNumbering
                let! registeredMask = Gen.listOfLength 8 (Gen.elements [ true ; false ])
                let! pending = Gen.listOf (Gen.zip (Gen.choose (0, 7)) (Gen.elements [ true ; false ]))
                return numbering, registeredMask, List.truncate 10 pending
            }

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 200,
            Prop.forAll (Arb.fromGen gen) (fun (numbering, mask, pending) -> property numbering mask pending)
        )

        // The same signal, pending while registered, spelt either way.
        for numbering in everyNumbering do
            let signals, shim = register numbering Signal.SIGTERM (fresh numbering)
            let pending = enqueue (processDirected Signal.SIGTERM) signals, shim
            screen pending Signal.SIGTERM |> shouldEqual None
            screen pending (Signal.Other 15) |> shouldEqual None

    [<Test>]
    let ``inherited ignores are set as the process is created`` () : unit =
        EmulatedKernel.createInheritingSignalIgnores
            "test"
            (Set.singleton Signal.SIGHUP)
            SimulatedUnixPlatform.linuxX64
            StandardStreamsConfig.piped
        |> EmulatedKernel.unix
        |> KernelSignals.disposition Signal.SIGHUP
        |> shouldEqual SignalDisposition.Ignore

        let exn =
            Assert.Throws (fun () ->
                EmulatedKernel.createInheritingSignalIgnores
                    "the context"
                    (Set.singleton Signal.SIGTERM)
                    SimulatedUnixPlatform.linuxX64
                    StandardStreamsConfig.piped
                |> ignore<EmulatedKernel>
            )

        exn.Message |> shouldContainText "the context"

    [<Test>]
    let ``gettid answers the task's id on Linux, and binds to nothing on Darwin`` () : unit =
        // The id a Linux kernel mints is below `pid_max`, so every one fits the `pid_t`
        // gettid returns; Darwin's C library has no gettid at all.
        let linuxId (pid : int32) : OsThreadId =
            let kernel =
                KernelConfig.toKernel
                    { KernelConfig.Default with
                        ProcessId = ProcessId.parseOrFail "test" pid
                    }

            UnixTaskTable.osThreadIdOf kernel.Leader kernel.Tasks

        let darwinId (id : uint64) : OsThreadId =
            let kernel =
                KernelConfig.toKernel
                    { KernelConfig.Default with
                        UnixPlatform = SimulatedUnixPlatform.macOsArm64
                        LeaderThreadId = Some id
                    }

            UnixTaskTable.osThreadIdOf kernel.Leader kernel.Tasks

        let linux (pid : int32) : bool =
            NativeLibc.gettid SimulatedUnixFlavour.Linux (linuxId pid) = Some pid

        let darwin (id : uint64) : bool =
            NativeLibc.gettid SimulatedUnixFlavour.Darwin (darwinId id) = None

        for pid in [ 1 ; 4242 ; UnixSystem.defaultPidMax - 1 ] do
            linux pid |> shouldEqual true

        for id in [ 1UL ; 4242UL ; 0x1_0000_0000UL ] do
            darwin id |> shouldEqual true

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 200,
            Prop.forAll (Arb.fromGen (Gen.choose (1, UnixSystem.defaultPidMax - 1))) linux
        )
