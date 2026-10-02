namespace WoofWare.PosixKernel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The rows of `docs/plans/2026-08-23-posix-kernel-extraction/signal-disposition-table.c`,
/// measured 2026-09-26 on Linux 6.18.5 (aarch64, glibc 2.41) and Darwin 27.0.0,
/// replayed through `SignalState`. Each trial there ran in a fresh child with
/// the signal blocked; each is replayed here on a fresh state with the signal
/// blocked on the only thread.
///
/// The expected answers are the probe's output, written as the sets of
/// signal numbers that departed from each row's majority, rather than derived
/// from `Signal`'s classifiers, so that a wrong classifier cannot make its
/// own oracle agree with it. The same Darwin rows on 25.6.0 (the signals
/// research's `ignore_pending.c`, handlers at generation only) agree.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSignalDispositions =

    type private Task = | Task of int

    type private Handler =
        | H
        | H2

    /// A disposition as the probe names it.
    type private Disposition =
        | Dfl
        | Ign
        | Handler of Handler

    let private toDisposition (d : Disposition) : SignalDisposition<Handler> =
        match d with
        | Disposition.Dfl -> SignalDisposition.Default
        | Disposition.Ign -> SignalDisposition.Ignore
        | Disposition.Handler h -> SignalDisposition.Catch (SignalCatch.ofHandler h)

    let private t0 : Task = Task 0

    let private tasks : Set<Task> = Set.singleton t0

    /// `SignalState.generate` in a process whose only task is `t0`.
    let private generateIn
        (entry : PendingSignal<Task>)
        (s : SignalState<Task, Handler>)
        : SignalGeneration<Task, Handler>
        =
        match SignalState.generate CoreDumps.Suppressed t0 tasks entry s with
        | Ok generation -> generation
        | Error refusal -> failwith $"generate refused: %O{refusal}"

    /// What `t0` takes next.
    let private leaderDelivery
        (s : SignalState<Task, Handler>)
        : SignalDelivery<Task, Handler> option * SignalState<Task, Handler>
        =
        match SignalState.onReturnToUser CoreDumps.Suppressed t0 tasks t0 s with
        | Ok answer -> answer
        | Error refusal -> failwith $"onReturnToUser refused: %O{refusal}"

    /// `t0` inside a handler that blocks `signal`, delivered through a carrier
    /// the sweep is not looking at.
    let private block (signal : Signal) (s : SignalState<Task, Handler>) : SignalState<Task, Handler> =
        let carrier =
            if signal = HandlerFrames.carrier then
                Signal.SIGVTALRM
            else
                HandlerFrames.carrier

        HandlerFrames.enterVia carrier H t0 tasks t0 (Set.singleton signal) s

    /// `t0` returns from its handler for `signal`, and the frame of every
    /// handler it entered after.
    let private unblock (s : SignalState<Task, Handler>) : SignalState<Task, Handler> = HandlerFrames.leave t0 s

    /// The signals a process that `generation` did not kill has.
    let private stateAfter (generation : SignalGeneration<Task, Handler>) : SignalState<Task, Handler> =
        match generation with
        | SignalGeneration.ProcessContinues state
        | SignalGeneration.ProcessStopped (_, state) -> state
        | SignalGeneration.ProcessTerminated _ -> failwith $"expected the process to survive, got %A{generation}"

    let private everyNumbering : SignalNumbering list =
        [ SignalNumbering.Linux ; SignalNumbering.Darwin ]

    /// The signals the probe swept: every one sigaction accepts through glibc,
    /// which on Linux leaves out 32 and 33.
    let private sweptSignos (numbering : SignalNumbering) : int list =
        let standard =
            [ 1..31 ]
            |> List.filter (fun signo ->
                match numbering, signo with
                | _, 9
                | SignalNumbering.Linux, 19
                | SignalNumbering.Darwin, 17 -> false
                | _ -> true
            )

        match numbering with
        | SignalNumbering.Linux -> standard @ [ 34..64 ]
        | SignalNumbering.Darwin -> standard

    let private signalOf (numbering : SignalNumbering) (signo : int) : Signal =
        match Signal.ofRawSignoUnder numbering signo with
        | ValueSome signal -> signal
        | ValueNone -> failwith $"%O{numbering}: %d{signo} is not a signal"

    /// Signals whose default is to discard them: Linux CHLD URG WINCH; Darwin
    /// URG CHLD IO WINCH INFO.
    let private defaultIgnored (numbering : SignalNumbering) : Set<int> =
        match numbering with
        | SignalNumbering.Linux -> Set.ofList [ 17 ; 23 ; 28 ]
        | SignalNumbering.Darwin -> Set.ofList [ 16 ; 20 ; 23 ; 28 ; 29 ]

    let private sigcont (numbering : SignalNumbering) : int =
        match numbering with
        | SignalNumbering.Linux -> 18
        | SignalNumbering.Darwin -> 19

    /// Part "tr", the `pending_at_gen` column: the signos *not* pending straight
    /// after generation under `from`.
    let private notPendingAtGeneration (numbering : SignalNumbering) (from : Disposition) : Set<int> =
        match numbering, from with
        | SignalNumbering.Linux, _ -> Set.empty
        | SignalNumbering.Darwin, Disposition.Ign ->
            sweptSignos numbering |> List.filter (fun s -> s <> 19) |> Set.ofList
        | SignalNumbering.Darwin, Disposition.Dfl -> defaultIgnored numbering
        | SignalNumbering.Darwin, Disposition.Handler _ -> Set.empty

    /// Part "tr", the `after` column: the signos a change to `toward`
    /// discarded, having been pending.
    let private discardedBy (numbering : SignalNumbering) (toward : Disposition) : Set<int> =
        match toward with
        | Disposition.Ign -> sweptSignos numbering |> Set.ofList
        | Disposition.Dfl -> Set.add (sigcont numbering) (defaultIgnored numbering)
        | Disposition.Handler _ -> Set.empty

    let private everyDisposition : Disposition list =
        [ Disposition.Dfl ; Disposition.Ign ; Disposition.Handler H ]

    [<Test>]
    let ``changing the disposition of a blocked pending signal matches every measured row`` () : unit =
        let mutable rows = 0

        for numbering in everyNumbering do
            for signo in sweptSignos numbering do
                let signal = signalOf numbering signo

                for target in [ ValueNone ; ValueSome t0 ] do
                    for from in everyDisposition do
                        for toward in everyDisposition @ [ Disposition.Handler H2 ] do
                            let blocked : SignalState<Task, Handler> =
                                SignalState.initial numbering Set.empty
                                |> block signal
                                |> SignalState.setDisposition signal (toDisposition from)

                            let generated =
                                match
                                    generateIn
                                        {
                                            Signal = signal
                                            Target = target
                                        }
                                        blocked
                                with
                                | SignalGeneration.ProcessContinues generated -> generated
                                | other -> failwith $"expected the process to carry on, got %A{other}"

                            let isPending (s : SignalState<Task, Handler>) : bool =
                                SignalState.pending s |> List.exists (fun e -> e.Signal = signal)

                            let atGeneration = isPending generated
                            let changed = SignalState.setDisposition signal (toDisposition toward) generated
                            let after = isPending changed

                            let delivered =
                                changed
                                |> SignalState.setDisposition signal (SignalDisposition.Catch (SignalCatch.ofHandler H))
                                |> unblock
                                |> leaderDelivery
                                |> fst

                            let expectedAtGeneration =
                                not (Set.contains signo (notPendingAtGeneration numbering from))

                            let expectedAfter =
                                expectedAtGeneration && not (Set.contains signo (discardedBy numbering toward))

                            let row = $"%O{numbering} %d{signo} %O{target} %A{from}->%A{toward}"

                            (row, atGeneration, after)
                            |> shouldEqual (row, expectedAtGeneration, expectedAfter)

                            match delivered with
                            | Some (SignalDelivery.RunHandlers [ frame ]) ->
                                (row, true) |> shouldEqual (row, expectedAfter)
                                frame.Entry.Signal |> shouldEqual signal
                                frame.Action.Handler |> shouldEqual H
                            | None -> (row, false) |> shouldEqual (row, expectedAfter)
                            | Some other -> failwith $"%s{row}: delivered %A{other}"

                            rows <- rows + 1

        // Linux: 60 signals, Darwin: 29; each in 2 directions x 3 x 4.
        rows |> shouldEqual ((60 + 29) * 2 * 3 * 4)

    /// Part "fl": the measured exceptions to "generating a stop signal
    /// discards a pending SIGCONT, and generating SIGCONT discards pending
    /// stop signals". Darwin's ignored stop signal discards nothing; nothing
    /// else departed.
    let private flushes
        (numbering : SignalNumbering)
        (first : int)
        (second : int)
        (secondDisposition : Disposition)
        : bool
        =
        let cont = sigcont numbering

        let stops =
            match numbering with
            | SignalNumbering.Linux -> Set.ofList [ 20 ; 21 ; 22 ]
            | SignalNumbering.Darwin -> Set.ofList [ 18 ; 21 ; 22 ]

        let opposite =
            (first = cont && Set.contains second stops)
            || (Set.contains first stops && second = cont)

        let darwinIgnoredStop =
            numbering = SignalNumbering.Darwin
            && Set.contains second stops
            && secondDisposition = Disposition.Ign

        opposite && not darwinIgnoredStop

    [<Test>]
    let ``generating a stop signal or SIGCONT discards the other kind as measured`` () : unit =
        let mutable rows = 0

        for numbering in everyNumbering do
            let usr1 =
                match numbering with
                | SignalNumbering.Linux -> 10
                | SignalNumbering.Darwin -> 30

            let swept =
                match numbering with
                | SignalNumbering.Linux -> [ 20 ; 21 ; 22 ; 18 ; usr1 ]
                | SignalNumbering.Darwin -> [ 18 ; 21 ; 22 ; 19 ; usr1 ]

            for first in swept do
                for second in swept do
                    if first <> second then
                        for firstDisposition in everyDisposition do
                            for secondDisposition in everyDisposition do
                                for firstTarget in [ ValueNone ; ValueSome t0 ] do
                                    for secondTarget in [ ValueNone ; ValueSome t0 ] do
                                        let firstSignal = signalOf numbering first
                                        let secondSignal = signalOf numbering second

                                        let start : SignalState<Task, Handler> =
                                            SignalState.initial numbering Set.empty
                                            |> block firstSignal
                                            |> block secondSignal
                                            |> SignalState.setDisposition firstSignal (toDisposition firstDisposition)
                                            |> SignalState.setDisposition secondSignal (toDisposition secondDisposition)

                                        let generate (signal : Signal) target (s : SignalState<Task, Handler>) =
                                            generateIn
                                                {
                                                    Signal = signal
                                                    Target = target
                                                }
                                                s
                                            |> stateAfter

                                        let isPending (signal : Signal) (s : SignalState<Task, Handler>) : bool =
                                            SignalState.pending s |> List.exists (fun e -> e.Signal = signal)

                                        let afterFirst = start |> generate firstSignal firstTarget
                                        let afterSecond = afterFirst |> generate secondSignal secondTarget

                                        let firstAtGeneration =
                                            not (Set.contains first (notPendingAtGeneration numbering firstDisposition))

                                        let secondAtGeneration =
                                            not (
                                                Set.contains second (notPendingAtGeneration numbering secondDisposition)
                                            )

                                        let expectedFirst =
                                            firstAtGeneration && not (flushes numbering first second secondDisposition)

                                        let row =
                                            $"%O{numbering} %d{first} %A{firstDisposition} %O{firstTarget} then %d{second} %A{secondDisposition} %O{secondTarget}"

                                        (row,
                                         isPending firstSignal afterFirst,
                                         isPending firstSignal afterSecond,
                                         isPending secondSignal afterSecond)
                                        |> shouldEqual (row, firstAtGeneration, expectedFirst, secondAtGeneration)

                                        rows <- rows + 1

        rows |> shouldEqual (2 * 20 * 9 * 4)

    [<Test>]
    let ``an unblocked stop signal or SIGCONT discards the other kind as measured`` () : unit =
        // Part "fu": the first signal caught and blocked, the second
        // unblocked. Only Darwin's ignored stop signal leaves SIGCONT pending.
        for numbering in everyNumbering do
            let cont = sigcont numbering

            let stops =
                match numbering with
                | SignalNumbering.Linux -> [ 20 ; 21 ; 22 ]
                | SignalNumbering.Darwin -> [ 18 ; 21 ; 22 ]

            for stop in stops do
                for first, second in [ stop, cont ; cont, stop ] do
                    for secondDisposition in everyDisposition do
                        // An unblocked default stop signal would stop the process.
                        if not (secondDisposition = Disposition.Dfl && second <> cont) then
                            let firstSignal = signalOf numbering first
                            let secondSignal = signalOf numbering second

                            let s : SignalState<Task, Handler> =
                                SignalState.initial numbering Set.empty
                                |> block firstSignal
                                |> SignalState.setDisposition
                                    firstSignal
                                    (SignalDisposition.Catch (SignalCatch.ofHandler H))
                                |> SignalState.setDisposition secondSignal (toDisposition secondDisposition)
                                |> SignalState.enqueue
                                    {
                                        Signal = firstSignal
                                        Target = ValueNone
                                    }

                            let s =
                                generateIn
                                    {
                                        Signal = secondSignal
                                        Target = ValueNone
                                    }
                                    s
                                |> stateAfter

                            let expected =
                                numbering = SignalNumbering.Darwin
                                && second <> cont
                                && secondDisposition = Disposition.Ign

                            let row = $"%O{numbering} %d{first} then %d{second} %A{secondDisposition}"

                            (row, SignalState.pending s |> List.exists (fun e -> e.Signal = firstSignal))
                            |> shouldEqual (row, expected)
