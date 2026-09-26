namespace WoofWare.PawPrint.Test

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

    let private initial (numbering : SignalNumbering) : SignalState<int, NativeSignalHandler> =
        StartupSignalDispositions.initial numbering Set.empty

    let private signal (numbering : SignalNumbering) (signo : int) : Signal =
        match Signal.ofRawSignoUnder numbering signo with
        | ValueSome signal -> signal
        | ValueNone -> failwith $"%d{signo} is not a signal under %O{numbering}"

    let private processDirected (signal : Signal) : PendingSignal<int> =
        {
            Signal = signal
            Target = ValueNone
        }

    /// `SystemNative_EnablePosixSignalHandling`'s effect on the two halves.
    let private register
        (numbering : SignalNumbering)
        (sent : Signal)
        (signals : SignalState<int, NativeSignalHandler>, shim : PosixSignalShim)
        : SignalState<int, NativeSignalHandler> * PosixSignalShim
        =
        PosixSignalShim.installHandler numbering sent signals shim

    let private screen
        (signals : SignalState<int, NativeSignalHandler>, shim : PosixSignalShim)
        (sent : Signal)
        : UnmodelledSelfSignal option
        =
        NativeLibc.screenSelfSignal shim signals sent

    let private fresh (numbering : SignalNumbering) : SignalState<int, NativeSignalHandler> * PosixSignalShim =
        initial numbering, PosixSignalShim.initial

    let private caughtByRuntime : NativeSignalHandler = NativeSignalHandler.CoreClrPal

    /// Both columns of the table, as measured (see the comments in
    /// `StartupSignalDispositions`); `TestStartupSignalDispositions` checks
    /// the host's column against the real runtime.
    let private measured (numbering : SignalNumbering) : Map<int, SignalDisposition<NativeSignalHandler>> =
        match numbering with
        | SignalNumbering.Linux ->
            Map.ofList
                [
                    for signo in [ 4 ; 5 ; 6 ; 7 ; 8 ; 11 ; 34 ] do
                        signo, SignalDisposition.Catch caughtByRuntime
                    33, SignalDisposition.Catch NativeSignalHandler.GlibcSetXid
                    13, SignalDisposition.Ignore
                ]
        | SignalNumbering.Darwin ->
            Map.ofList
                [
                    for signo in [ 4 ; 6 ; 8 ; 10 ; 11 ; 30 ] do
                        signo, SignalDisposition.Catch caughtByRuntime
                    13, SignalDisposition.Ignore
                ]

    [<Test>]
    let ``the startup dispositions are the measured ones`` () : unit =
        for numbering in everyNumbering do
            let actual =
                SignalState.dispositions (initial numbering)
                |> Map.toSeq
                |> Seq.map (fun (signal, disposition) -> Signal.toRawSignoUnder numbering signal, disposition)
                |> Map.ofSeq

            (numbering, actual) |> shouldEqual (numbering, measured numbering)

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

            let state : SignalState<int, NativeSignalHandler> =
                StartupSignalDispositions.initial numbering (Set.ofList ignorable)

            for s in ignorable do
                let expected =
                    match Map.tryFind (Signal.toRawSignoUnder numbering s) (measured numbering) with
                    | Some (SignalDisposition.Catch handler) -> SignalDisposition.Catch handler
                    | Some _
                    | None -> SignalDisposition.Ignore

                (numbering, s, SignalState.disposition s state)
                |> shouldEqual (numbering, s, expected)

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
                    StartupSignalDispositions.initial numbering (Set.singleton s)
                    |> ignore<SignalState<int, NativeSignalHandler>>
                )
                |> ignore<exn>

            StartupSignalDispositions.refusal numbering (Set.ofList [ Signal.SIGHUP ; Signal.SIGUSR2 ])
            |> shouldEqual None

    [<Test>]
    let ``a signal a native handler catches from startup is refused, whatever the state`` () : unit =
        for numbering in everyNumbering do
            for KeyValue (signo, disposition) in measured numbering do
                match disposition with
                | SignalDisposition.Catch handler ->
                    let sent = signal numbering signo

                    screen (fresh numbering) (Signal.Other signo)
                    |> shouldEqual (Some (UnmodelledSelfSignal.NativeHandler (sent, handler)))

                    // Registered and pending: System.Native's handler runs
                    // the runtime's first. (Linux's 33 is glibc's, which no
                    // handler can be registered for.)
                    if not (Signal.isUncatchableUnder numbering sent) then
                        let signals, shim = register numbering (Signal.Other signo) (fresh numbering)

                        SignalState.disposition sent signals
                        |> shouldEqual (SignalDisposition.Catch NativeSignalHandler.SystemNative)

                        let registered = SignalState.enqueue (processDirected sent) signals, shim

                        screen registered (Signal.Other signo)
                        |> shouldEqual (Some (UnmodelledSelfSignal.NativeHandler (sent, handler)))
                | SignalDisposition.Ignore
                | SignalDisposition.Default -> ()

    [<Test>]
    let ``SIGPIPE is answered, registered or not, and stays ignored`` () : unit =
        for numbering in everyNumbering do
            screen (fresh numbering) Signal.SIGPIPE |> shouldEqual None

            let signals, shim = register numbering Signal.SIGPIPE (fresh numbering)

            SignalState.disposition Signal.SIGPIPE signals
            |> shouldEqual SignalDisposition.Ignore

            screen (signals, shim) Signal.SIGPIPE |> shouldEqual None

            // Unregistering restores the ignore the shim saved.
            PosixSignalShim.restoreHandler numbering Signal.SIGPIPE signals shim
            |> SignalState.disposition Signal.SIGPIPE
            |> shouldEqual SignalDisposition.Ignore

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

            let ignored : SignalState<int, NativeSignalHandler> =
                StartupSignalDispositions.initial numbering (Set.singleton Signal.SIGCONT)

            screen (ignored, PosixSignalShim.initial) Signal.SIGCONT |> shouldEqual None

    [<Test>]
    let ``a registered signal already pending is refused`` () : unit =
        for numbering in everyNumbering do
            let signals, shim = register numbering Signal.SIGTERM (fresh numbering)

            screen (signals, shim) Signal.SIGTERM |> shouldEqual None

            let pending = SignalState.enqueue (processDirected Signal.SIGTERM) signals, shim

            screen pending Signal.SIGTERM
            |> shouldEqual (Some (UnmodelledSelfSignal.WouldCoalesce Signal.SIGTERM))

            // Spelt by number, it is still the same signal.
            screen pending (Signal.Other 15)
            |> shouldEqual (Some (UnmodelledSelfSignal.WouldCoalesce Signal.SIGTERM))

            // A different pending signal is no reason.
            let otherSignals, otherShim = register numbering Signal.SIGHUP (signals, shim)

            screen (SignalState.enqueue (processDirected Signal.SIGHUP) otherSignals, otherShim) Signal.SIGTERM
            |> shouldEqual None

    [<Test>]
    let ``a pending instance aimed at one thread is no reason to refuse`` () : unit =
        // It sits in the thread's own pending set, which a signal sent to the
        // process does not merge with.
        let signals, shim =
            register SignalNumbering.Linux Signal.SIGTERM (fresh SignalNumbering.Linux)

        let pending =
            signals
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGTERM
                    Target = ValueSome 3
                }

        screen (pending, shim) Signal.SIGTERM |> shouldEqual None

    [<Test>]
    let ``a pending signal with no handler is no reason to refuse`` () : unit =
        // Pending because every thread blocks it: that is the kernel's own
        // pending set, which does merge a second instance.
        let pending =
            initial SignalNumbering.Linux
            |> SignalState.block 0 Signal.SIGTERM
            |> SignalState.enqueue (processDirected Signal.SIGTERM)

        screen (pending, PosixSignalShim.initial) Signal.SIGTERM |> shouldEqual None

    [<Test>]
    let ``a pending real-time signal is no reason to refuse`` () : unit =
        // Real-time signals queue rather than merge, in the kernel and in the
        // model alike.
        let realTime = Signal.Other 40

        let signals, shim =
            register SignalNumbering.Linux realTime (fresh SignalNumbering.Linux)

        screen (SignalState.enqueue (processDirected realTime) signals, shim) realTime
        |> shouldEqual None

    [<Test>]
    let ``inherited ignores are set only on a fresh process`` () : unit =
        let kernel = EmulatedKernel.initial

        kernel
        |> EmulatedKernel.withInheritedSignalIgnores "test" (Set.singleton Signal.SIGHUP)
        |> fun k -> SignalState.disposition Signal.SIGHUP k.Signals
        |> shouldEqual SignalDisposition.Ignore

        let touched =
            EmulatedKernel.mapProcess
                (fun proc ->
                    { proc with
                        Signals = SignalState.setDisposition Signal.SIGUSR2 SignalDisposition.Ignore proc.Signals
                    }
                )
                kernel

        Assert.Throws (fun () ->
            EmulatedKernel.withInheritedSignalIgnores "test" (Set.singleton Signal.SIGHUP) touched
            |> ignore<EmulatedKernel>
        )
        |> ignore<exn>
