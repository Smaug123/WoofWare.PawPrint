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
                        signo, SignalDisposition.Catch (SignalCatch.ofHandler caughtByRuntime)
                    33, SignalDisposition.Catch (SignalCatch.ofHandler NativeSignalHandler.GlibcSetXid)
                    13, SignalDisposition.Ignore
                ]
        | SignalNumbering.Darwin ->
            Map.ofList
                [
                    for signo in [ 4 ; 6 ; 8 ; 10 ; 11 ; 30 ] do
                        signo, SignalDisposition.Catch (SignalCatch.ofHandler caughtByRuntime)
                    13, SignalDisposition.Ignore
                ]

    [<Test>]
    let ``the startup dispositions are the measured ones`` () : unit =
        for numbering in everyNumbering do
            // Which handler catches each signal is measured; the handlers' own
            // masks and flags are the PAL's source, pinned below.
            let actual =
                SignalState.dispositions (initial numbering)
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

    /// The signals of `measured`'s runtime-caught ones whose handler restores
    /// the default when sent the signal, as measured (see the comments in
    /// `StartupSignalDispositions`).
    let private measuredRestoring (numbering : SignalNumbering) : Set<int> =
        match numbering with
        | SignalNumbering.Linux -> Set.ofList [ 4 ; 6 ; 7 ; 8 ; 11 ]
        | SignalNumbering.Darwin -> Set.ofList [ 4 ; 6 ; 8 ; 10 ; 11 ]

    [<Test>]
    let ``the startup handlers that restore the default when sent are the measured ones`` () : unit =
        for numbering in everyNumbering do
            let actual =
                [ 1 .. Signal.highestSignoUnder numbering ]
                |> List.filter (fun signo ->
                    StartupSignalDispositions.restoresDefaultWhenSent numbering (signal numbering signo)
                )
                |> Set.ofList

            (numbering, actual) |> shouldEqual (numbering, measuredRestoring numbering)

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
            for KeyValue (signal, disposition) in SignalState.dispositions (initial numbering) do
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

            let state : SignalState<int, NativeSignalHandler> =
                StartupSignalDispositions.initial numbering (Set.ofList ignorable)

            for s in ignorable do
                let expected =
                    match Map.tryFind (Signal.toRawSignoUnder numbering s) (measured numbering) with
                    | Some (SignalDisposition.Catch handler) -> SignalDisposition.Catch handler
                    | Some _
                    | None -> SignalDisposition.Ignore

                let actual =
                    match SignalState.disposition s state with
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

                        match SignalState.disposition sent signals with
                        | SignalDisposition.Catch action ->
                            action.Handler |> shouldEqual NativeSignalHandler.SystemNative
                            // Over the runtime's handler, System.Native keeps its
                            // mask and flags, and adds SA_RESTART.
                            action.Restart |> shouldEqual true
                        | other -> failwith $"expected System.Native's handler, got %A{other}"

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

                    SignalState.enqueue
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
            let pending = SignalState.enqueue (processDirected Signal.SIGTERM) signals, shim
            screen pending Signal.SIGTERM |> shouldEqual None
            screen pending (Signal.Other 15) |> shouldEqual None

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
