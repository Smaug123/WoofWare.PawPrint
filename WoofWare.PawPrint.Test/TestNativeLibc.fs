namespace WoofWare.PawPrint.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `NativeLibc.screenSelfSignal`: which signals sent by a process to itself
/// PawPrint refuses to answer, because the kernel model's answer would differ
/// from a real CoreCLR process's.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNativeLibc =

    let private everyNumbering : SignalNumbering list =
        [ SignalNumbering.Linux ; SignalNumbering.Darwin ]

    let private initial (numbering : SignalNumbering) : SignalState<int, string> = SignalState.initial numbering

    let private signal (numbering : SignalNumbering) (signo : int) : Signal =
        match Signal.ofRawSignoUnder numbering signo with
        | ValueSome signal -> signal
        | ValueNone -> failwith $"%d{signo} is not a signal under %O{numbering}"

    let private processDirected (signal : Signal) : PendingSignal<int> =
        {
            Signal = signal
            Target = ValueNone
        }

    /// Both columns of `StartupSignalDispositions`, as measured (see the
    /// comment there); `TestStartupSignalDispositions` checks the host's
    /// column against the real runtime.
    let private survivingSignos (numbering : SignalNumbering) : Set<int> =
        match numbering with
        | SignalNumbering.Linux -> Set.ofList [ 4 ; 5 ; 6 ; 7 ; 8 ; 11 ; 13 ; 33 ; 34 ]
        | SignalNumbering.Darwin -> Set.ofList [ 4 ; 6 ; 8 ; 10 ; 11 ; 13 ; 30 ]

    [<Test>]
    let ``the startup dispositions are the measured ones`` () : unit =
        for numbering in everyNumbering do
            let actual =
                [ 1 .. Signal.highestSignoUnder numbering ]
                |> List.filter (fun signo ->
                    StartupSignalDispositions.overridesTerminatingDefault numbering (signal numbering signo)
                )
                |> Set.ofList

            (numbering, actual) |> shouldEqual (numbering, survivingSignos numbering)

    [<Test>]
    let ``a signal whose default the runtime overrides from startup is refused, whatever the state`` () : unit =
        for numbering in everyNumbering do
            for signo in survivingSignos numbering do
                let sent = signal numbering signo

                let states =
                    [
                        yield initial numbering
                        // Its number spelt as Other, and a handler registered
                        // and pending: the startup disposition still decides.
                        // (Linux's 33 is glibc's, which no handler can be
                        // registered for.)
                        if not (Signal.isUncatchableUnder numbering sent) then
                            yield
                                initial numbering
                                |> SignalState.enable (Signal.Other signo)
                                |> SignalState.enqueue (processDirected sent)
                    ]

                for state in states do
                    NativeLibc.screenSelfSignal state (Signal.Other signo)
                    |> shouldEqual (Some (UnmodelledSelfSignal.StartupDisposition sent))

    [<Test>]
    let ``every other signal is answered from a fresh state, but SIGCONT`` () : unit =
        for numbering in everyNumbering do
            for signo in 1 .. Signal.highestSignoUnder numbering do
                if not (Set.contains signo (survivingSignos numbering)) then
                    let expected =
                        if signal numbering signo = Signal.SIGCONT then
                            Some (UnmodelledSelfSignal.ContinueWithoutHandler Signal.SIGCONT)
                        else
                            None

                    (numbering, signo, NativeLibc.screenSelfSignal (initial numbering) (signal numbering signo))
                    |> shouldEqual (numbering, signo, expected)

    [<Test>]
    let ``SIGCONT is answered once a handler is registered for it`` () : unit =
        for numbering in everyNumbering do
            let registered = initial numbering |> SignalState.enable Signal.SIGCONT
            NativeLibc.screenSelfSignal registered Signal.SIGCONT |> shouldEqual None

    [<Test>]
    let ``a registered signal already pending is refused`` () : unit =
        for numbering in everyNumbering do
            let registered = initial numbering |> SignalState.enable Signal.SIGTERM

            NativeLibc.screenSelfSignal registered Signal.SIGTERM |> shouldEqual None

            let pending = registered |> SignalState.enqueue (processDirected Signal.SIGTERM)

            NativeLibc.screenSelfSignal pending Signal.SIGTERM
            |> shouldEqual (Some (UnmodelledSelfSignal.WouldCoalesce Signal.SIGTERM))

            // Spelt by number, it is still the same signal.
            NativeLibc.screenSelfSignal pending (Signal.Other 15)
            |> shouldEqual (Some (UnmodelledSelfSignal.WouldCoalesce Signal.SIGTERM))

            // A different pending signal is no reason.
            let otherPending =
                registered
                |> SignalState.enable Signal.SIGHUP
                |> SignalState.enqueue (processDirected Signal.SIGHUP)

            NativeLibc.screenSelfSignal otherPending Signal.SIGTERM |> shouldEqual None

    [<Test>]
    let ``a pending instance aimed at one thread is no reason to refuse`` () : unit =
        // It sits in the thread's own pending set, which a signal sent to the
        // process does not merge with.
        let pending =
            initial SignalNumbering.Linux
            |> SignalState.enable Signal.SIGTERM
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGTERM
                    Target = ValueSome 3
                }

        NativeLibc.screenSelfSignal pending Signal.SIGTERM |> shouldEqual None

    [<Test>]
    let ``a pending signal with no handler is no reason to refuse`` () : unit =
        // Pending because every thread blocks it: that is the kernel's own
        // pending set, which does merge a second instance.
        let pending =
            initial SignalNumbering.Linux
            |> SignalState.block 0 Signal.SIGTERM
            |> SignalState.enqueue (processDirected Signal.SIGTERM)

        NativeLibc.screenSelfSignal pending Signal.SIGTERM |> shouldEqual None

    [<Test>]
    let ``a pending real-time signal is no reason to refuse`` () : unit =
        // Real-time signals queue rather than merge, in the kernel and in the
        // model alike.
        let realTime = Signal.Other 40

        let pending =
            initial SignalNumbering.Linux
            |> SignalState.enable realTime
            |> SignalState.enqueue (processDirected realTime)

        NativeLibc.screenSelfSignal pending realTime |> shouldEqual None
