namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// Property-based and unit tests for the deterministic `SignalState` data
/// model, exercised in isolation from the dispatcher.
///
/// The property test runs a random sequence of operations through both the
/// production module and a structurally-different reference oracle, then
/// asserts agreement on every observable accessor after each step. The
/// oracle uses index-based scanning over an array; the production module
/// uses a recursive accumulator-threaded walk. A regression in either side
/// surfaces as a divergence the property catches.
///
/// Everything runs under both numberings, because the state's contract is
/// stated *under a numbering*: `Other 17` is `SIGCHLD` to a Linux process
/// and `SIGSTOP` to a Darwin one, and several tests below assert exactly
/// that divergence.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSignalState =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private everyNumbering : SignalNumbering list =
        [ SignalNumbering.Linux ; SignalNumbering.Darwin ]

    /// Stand-in for whatever a client uses to identify a task. Deliberately a
    /// nominal type of this test's own rather than `int`: the point of these
    /// tests is that `SignalState` is generic in its task identity, and an
    /// `int` could satisfy the signature through some numeric path without the
    /// parameter being genuinely opaque.
    type TestTask = | TestTask of int

    /// Stand-in for a client's signal-handler identity, which `SignalState` is
    /// generic in and constrains only to equality.
    type TestHandler = | TestHandler of string

    /// `initial` at this test's instantiation. Named because the
    /// generic `initial` constrains neither parameter, so every bare use would
    /// infer `obj` for the handler -- which `FS3559` rejects, correctly: an
    /// `obj` handler would make the equality constraint vacuous and the tests
    /// below would stop saying anything about it.
    let private initial (numbering : SignalNumbering) : SignalState<TestTask, TestHandler> =
        SignalState.initial numbering Set.empty

    /// Most of the operations' behaviour does not depend on the numbering at
    /// all; those tests run on this instance and rely on the property test to
    /// cover the other numbering.
    let private empty : SignalState<TestTask, TestHandler> =
        initial SignalNumbering.Linux

    /// The signals a process that `generation` left running has.
    let private continuesWith
        (generation : SignalGeneration<TestTask, TestHandler>)
        : SignalState<TestTask, TestHandler>
        =
        match generation with
        | SignalGeneration.ProcessContinues state -> state
        | SignalGeneration.ProcessTerminated _
        | SignalGeneration.ProcessStopped _ -> failwith $"expected the process to carry on, got %A{generation}"

    let private t0 : TestTask = TestTask 0
    let private t1 : TestTask = TestTask 1
    let private t2 : TestTask = TestTask 2

    let private allThreads : TestTask list = [ t0 ; t1 ; t2 ]

    let private namedSignals : Signal list =
        [
            Signal.SIGHUP
            Signal.SIGINT
            Signal.SIGQUIT
            Signal.SIGTERM
            Signal.SIGCHLD
            Signal.SIGCONT
            Signal.SIGWINCH
            Signal.SIGTSTP
            Signal.SIGTTIN
            Signal.SIGTTOU
            Signal.SIGPIPE
            Signal.SIGUSR1
            Signal.SIGUSR2
            Signal.SIGABRT
            Signal.SIGURG
        ]

    /// SIGKILL's number and SIGSTOP's under this numbering, plus (on Linux)
    /// the two glibc reserves for itself: what `block` must silently drop.
    let private unblockableSpellings (numbering : SignalNumbering) : Signal list =
        match numbering with
        | SignalNumbering.Linux -> [ Signal.Other 9 ; Signal.Other 19 ; Signal.Other 32 ; Signal.Other 33 ]
        | SignalNumbering.Darwin -> [ Signal.Other 9 ; Signal.Other 17 ]

    /// Signals that exist under the numbering but name no case: blockable and
    /// catchable, so they flow through every operation like a named one.
    let private unnamedSignals (numbering : SignalNumbering) : Signal list =
        match numbering with
        // 5 is SIGTRAP on both; 40 is a real-time signal, which Darwin does
        // not have.
        | SignalNumbering.Linux -> [ Signal.Other 5 ; Signal.Other 40 ]
        | SignalNumbering.Darwin -> [ Signal.Other 5 ]

    /// Every spelling the state can legally be handed under this numbering:
    /// the named cases, each named case respelt as `Other` carrying its
    /// number, the unnamed signals, and the unblockable ones.
    let private allSignals (numbering : SignalNumbering) : Signal list =
        let otherSpellings =
            namedSignals
            |> List.map (fun signal -> Signal.Other (Signal.toRawSignoUnder numbering signal))

        namedSignals
        @ otherSpellings
        @ unnamedSignals numbering
        @ unblockableSpellings numbering

    /// The subset of `allSignals` that `enable` accepts: everything
    /// `sigaction` would install a handler for.
    let private enableableSignals (numbering : SignalNumbering) : Signal list =
        allSignals numbering
        |> List.filter (fun signal -> not (Signal.isUncatchableUnder numbering signal))

    /// What the leader, `t0`, takes next, in a process whose tasks are `tasks`.
    /// Fails the test on a refusal.
    let private leaderDelivery
        (tasks : TestTask list)
        (s : SignalState<TestTask, TestHandler>)
        : SignalDelivery<TestTask, TestHandler> option * SignalState<TestTask, TestHandler>
        =
        match SignalState.nextDelivery CoreDumps.Suppressed t0 (Set.ofList tasks) t0 s with
        | Ok answer -> answer
        | Error refusal -> failwith $"nextDelivery refused: %O{refusal}"

    /// `SignalState.generate` in a process whose tasks are `tasks`, led by `t0`.
    /// Fails the test on a refusal.
    let private generateAmong
        (coreDumps : CoreDumps)
        (tasks : TestTask list)
        (entry : PendingSignal<TestTask>)
        (s : SignalState<TestTask, TestHandler>)
        : SignalGeneration<TestTask, TestHandler>
        =
        match SignalState.generate coreDumps t0 (Set.ofList tasks) entry s with
        | Ok generation -> generation
        | Error refusal -> failwith $"generate refused: %O{refusal}"

    let private handler : TestHandler = TestHandler "h"

    let private catch : SignalDisposition<TestHandler> = SignalDisposition.Catch handler

    /// Install `handler` for `signal`, as `sigaction` with a handler does.
    let private enable (signal : Signal) (s : SignalState<TestTask, TestHandler>) : SignalState<TestTask, TestHandler> =
        SignalState.setDisposition signal catch s

    /// Restore `signal`'s default, as `sigaction` with `SIG_DFL` does.
    let private restoreDefault
        (signal : Signal)
        (s : SignalState<TestTask, TestHandler>)
        : SignalState<TestTask, TestHandler>
        =
        SignalState.setDisposition signal SignalDisposition.Default s

    // ------------------------- Unit tests ------------------------- //

    [<Test>]
    let ``initial has every signal at its default, nothing blocked, nothing pending`` () : unit =
        for numbering in everyNumbering do
            let s = initial numbering
            SignalState.numbering s |> shouldEqual numbering
            SignalState.disposition Signal.SIGINT s |> shouldEqual SignalDisposition.Default
            SignalState.isBlocked t0 Signal.SIGINT s |> shouldEqual false
            SignalState.blockedFor t0 s |> shouldEqual Set.empty
            SignalState.pending s |> Seq.toList |> shouldEqual []
            SignalState.dispositions s |> shouldEqual Map.empty

    [<Test>]
    let ``initial ignores exactly the inherited ignores, in their canonical spelling`` () : unit =
        for numbering in everyNumbering do
            let spelt = Signal.Other (Signal.toRawSignoUnder numbering Signal.SIGHUP)

            let s : SignalState<TestTask, TestHandler> =
                SignalState.initial numbering (Set.ofList [ spelt ; Signal.SIGUSR2 ])

            SignalState.dispositions s
            |> shouldEqual (
                Map.ofList
                    [
                        Signal.SIGHUP, SignalDisposition.Ignore
                        Signal.SIGUSR2, SignalDisposition.Ignore
                    ]
            )

            SignalState.disposition Signal.SIGHUP s |> shouldEqual SignalDisposition.Ignore
            SignalState.disposition Signal.SIGINT s |> shouldEqual SignalDisposition.Default

    [<Test>]
    let ``initial refuses an inherited ignore of SIGKILL or SIGSTOP`` () : unit =
        for numbering in everyNumbering do
            let stop =
                match numbering with
                | SignalNumbering.Linux -> 19
                | SignalNumbering.Darwin -> 17

            for raw in [ 9 ; stop ] do
                Assert.Throws (fun () ->
                    SignalState.initial numbering (Set.singleton (Signal.Other raw))
                    |> ignore<SignalState<TestTask, TestHandler>>
                )
                |> ignore<exn>

    [<Test>]
    let ``setDisposition installs a handler`` () : unit =
        let s = empty |> enable Signal.SIGINT
        SignalState.disposition Signal.SIGINT s |> shouldEqual catch
        SignalState.disposition Signal.SIGHUP s |> shouldEqual SignalDisposition.Default
        SignalState.dispositions s |> shouldEqual (Map.ofList [ Signal.SIGINT, catch ])

    [<Test>]
    let ``setDisposition is idempotent`` () : unit =
        let once = empty |> enable Signal.SIGINT
        let twice = once |> enable Signal.SIGINT
        twice |> shouldEqual once

    [<Test>]
    let ``a second handler replaces the first`` () : unit =
        let other = SignalDisposition.Catch (TestHandler "other")

        let s =
            empty |> enable Signal.SIGINT |> SignalState.setDisposition Signal.SIGINT other

        SignalState.disposition Signal.SIGINT s |> shouldEqual other

    [<Test>]
    let ``restoring the default of a signal at its default is a no-op`` () : unit =
        let s = empty |> restoreDefault Signal.SIGINT
        s |> shouldEqual empty

    [<Test>]
    let ``a handler then the default collapses to the initial state`` () : unit =
        // Mirrors the unblock/empty-mask collapse: installing a handler and
        // then restoring the default must be structurally identical to never
        // having installed one. Without this, state dedup in the debugger
        // would distinguish two semantically-equivalent states.
        let caughtThenRestored =
            empty |> enable Signal.SIGINT |> restoreDefault Signal.SIGINT

        caughtThenRestored |> shouldEqual empty

    [<Test>]
    let ``restoring a terminating default leaves a pending entry queued, and the default then applies`` () : unit =
        // Removing the handler while a signal is pending doesn't drop the
        // signal — it remains queued, and delivery applies whatever the
        // disposition is at that moment: SIGINT's kernel default is to
        // terminate the process.
        let entry =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let s =
            empty
            |> enable Signal.SIGINT
            |> SignalState.enqueue entry
            |> restoreDefault Signal.SIGINT

        SignalState.pending s |> Seq.toList |> shouldEqual [ entry ]

        let delivery, s' = leaderDelivery [ t0 ] s

        delivery
        |> shouldEqual (Some (SignalDelivery.DefaultTerminate (Signal.SIGINT, false)))

        SignalState.pending s' |> shouldEqual []

    [<Test>]
    let ``enable after enqueue makes a queued signal deliverable`` () : unit =
        let entry =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let s = empty |> SignalState.enqueue entry |> enable Signal.SIGINT

        match leaderDelivery [ t0 ] s with
        | Some (SignalDelivery.RunHandler (e, _)), s' ->
            e |> shouldEqual entry
            SignalState.pending s' |> Seq.toList |> shouldEqual []
        | other, _ -> failwith $"expected RunHandler once signal was enabled, got %A{other}"

    [<Test>]
    let ``block then isBlocked`` () : unit =
        let s = empty |> SignalState.block t0 Signal.SIGINT
        SignalState.isBlocked t0 Signal.SIGINT s |> shouldEqual true
        SignalState.isBlocked t0 Signal.SIGHUP s |> shouldEqual false
        SignalState.isBlocked t1 Signal.SIGINT s |> shouldEqual false

    [<Test>]
    let ``block is idempotent`` () : unit =
        let once = empty |> SignalState.block t0 Signal.SIGINT
        let twice = once |> SignalState.block t0 Signal.SIGINT
        twice |> shouldEqual once

    [<Test>]
    let ``unblock removes a blocked signal`` () : unit =
        let s =
            empty
            |> SignalState.block t0 Signal.SIGINT
            |> SignalState.unblock t0 Signal.SIGINT

        SignalState.isBlocked t0 Signal.SIGINT s |> shouldEqual false
        SignalState.blockedFor t0 s |> shouldEqual Set.empty

    [<Test>]
    let ``unblock collapses empty mask back to the initial state`` () : unit =
        // A state that had a signal blocked and then unblocked must
        // be structurally identical to a state that never blocked it. Without
        // collapsing the empty mask, equality would distinguish two
        // semantically-equivalent states and the property-test oracle would
        // diverge from the implementation after every full unblock.
        let blockedThenUnblocked =
            empty
            |> SignalState.block t0 Signal.SIGINT
            |> SignalState.unblock t0 Signal.SIGINT

        blockedThenUnblocked |> shouldEqual empty

    [<Test>]
    let ``unblock of an unblocked signal is a no-op`` () : unit =
        let s = empty |> SignalState.unblock t0 Signal.SIGINT
        s |> shouldEqual empty

    // ------------------- Canonical identity ------------------- //

    [<Test>]
    let ``a named signal and its Other spelling are one signal to every operation`` () : unit =
        for numbering in everyNumbering do
            for signal in namedSignals do
                let spelt = Signal.Other (Signal.toRawSignoUnder numbering signal)

                // Block via the raw spelling, observe via the name — and the
                // state is structurally identical to one built via the name.
                let blocked = initial numbering |> SignalState.block t0 spelt
                SignalState.isBlocked t0 signal blocked |> shouldEqual true
                SignalState.blockedFor t0 blocked |> shouldEqual (Set.singleton signal)
                blocked |> shouldEqual (initial numbering |> SignalState.block t0 signal)

                // And the other way round: block the name, query the spelling.
                SignalState.isBlocked t0 spelt blocked |> shouldEqual true

                // Unblocking via the other spelling collapses back to initial.
                blocked |> SignalState.unblock t0 signal |> shouldEqual (initial numbering)

                if not (Signal.isUncatchableUnder numbering spelt) then
                    let caught = initial numbering |> enable spelt
                    SignalState.disposition signal caught |> shouldEqual catch
                    SignalState.dispositions caught |> shouldEqual (Map.ofList [ signal, catch ])
                    caught |> restoreDefault signal |> shouldEqual (initial numbering)

    [<Test>]
    let ``enqueue stores the canonical spelling`` () : unit =
        for numbering in everyNumbering do
            for signal in namedSignals do
                let spelt = Signal.Other (Signal.toRawSignoUnder numbering signal)

                // Caught first (every named case is catchable), so the
                // ignore-default rows are not discarded at generation and
                // every row exercises the storage path.
                let start = initial numbering |> enable signal

                let viaSpelling =
                    start
                    |> SignalState.enqueue
                        {
                            Signal = spelt
                            Target = ValueSome t1
                        }

                let viaName =
                    start
                    |> SignalState.enqueue
                        {
                            Signal = signal
                            Target = ValueSome t1
                        }

                viaSpelling |> shouldEqual viaName

                SignalState.pending viaSpelling
                |> List.map (fun entry -> entry.Signal)
                |> shouldEqual [ signal ]

    [<Test>]
    let ``the same Other payload is a different signal under each numbering`` () : unit =
        // 17 is SIGCHLD to a Linux process: blockable, catchable, and one
        // signal with the named case.
        let linux = initial SignalNumbering.Linux |> SignalState.block t0 (Signal.Other 17)

        SignalState.blockedFor t0 linux |> shouldEqual (Set.singleton Signal.SIGCHLD)

        // The same number is SIGSTOP to a Darwin process: the block is
        // silently dropped, exactly as sigprocmask drops it.
        let darwin =
            initial SignalNumbering.Darwin |> SignalState.block t0 (Signal.Other 17)

        darwin |> shouldEqual (initial SignalNumbering.Darwin)

        // And 19 the other way round: SIGSTOP to Linux, SIGCONT to Darwin.
        initial SignalNumbering.Linux
        |> SignalState.block t0 (Signal.Other 19)
        |> shouldEqual (initial SignalNumbering.Linux)

        initial SignalNumbering.Darwin
        |> SignalState.block t0 (Signal.Other 19)
        |> SignalState.blockedFor t0
        |> shouldEqual (Set.singleton Signal.SIGCONT)

    // ------------------- Unblockable and uncatchable signals ------------------- //

    [<Test>]
    let ``block silently drops every signal the mask calls refuse to hold`` () : unit =
        for numbering in everyNumbering do
            for signal in unblockableSpellings numbering do
                let s = initial numbering |> SignalState.block t0 signal
                // sigprocmask's own shape: success, but the mask is unchanged
                // — structurally the initial state, not merely equivalent.
                s |> shouldEqual (initial numbering)
                SignalState.isBlocked t0 signal s |> shouldEqual false

    [<Test>]
    let ``a block-everything sweep holds every signal but the unblockable ones`` () : unit =
        // The mask a thread ends up with after trying to block everything is
        // everything *blockable* — which is exactly what a real thread's mask
        // reads back as after the same sweep.
        for numbering in everyNumbering do
            let s =
                (initial numbering, allSignals numbering)
                ||> List.fold (fun s signal -> SignalState.block t0 signal s)

            let expected =
                allSignals numbering
                |> List.filter (fun signal -> not (Signal.isUnblockableUnder numbering signal))
                |> List.map (Signal.canonicalUnder numbering)
                |> Set.ofList

            SignalState.blockedFor t0 s |> shouldEqual expected

            for signal in unblockableSpellings numbering do
                SignalState.isBlocked t0 signal s |> shouldEqual false

    /// SIGKILL's number and SIGSTOP's under this numbering: the signals for
    /// which the kernel refuses any disposition but the default.
    let private kernelUncatchable (numbering : SignalNumbering) : Signal list =
        match numbering with
        | SignalNumbering.Linux -> [ Signal.Other 9 ; Signal.Other 19 ]
        | SignalNumbering.Darwin -> [ Signal.Other 9 ; Signal.Other 17 ]

    [<Test>]
    let ``setDisposition refuses every disposition for SIGKILL and SIGSTOP`` () : unit =
        for numbering in everyNumbering do
            for signal in kernelUncatchable numbering do
                for disposition in [ SignalDisposition.Default ; SignalDisposition.Ignore ; catch ] do
                    Assert.Throws (fun () ->
                        initial numbering
                        |> SignalState.setDisposition signal disposition
                        |> ignore<SignalState<_, _>>
                    )
                    |> ignore<exn>

    [<Test>]
    let ``setDisposition accepts glibc's reserved 32 and 33, which only glibc's sigaction refuses`` () : unit =
        // The kernel holds a handler for 33 in every process glibc's
        // setxid machinery has run in; it is glibc's own sigaction wrapper
        // that refuses the pair, and a client modelling that wrapper screens
        // with Signal.isUncatchableUnder first.
        for raw in [ 32 ; 33 ] do
            Signal.isUncatchableUnder SignalNumbering.Linux (Signal.Other raw)
            |> shouldEqual true

            let s = initial SignalNumbering.Linux |> enable (Signal.Other raw)
            SignalState.disposition (Signal.Other raw) s |> shouldEqual catch

    [<Test>]
    let ``every operation refuses a number that is not a signal under the numbering`` () : unit =
        let notASignal (numbering : SignalNumbering) : int list =
            match numbering with
            | SignalNumbering.Linux -> [ 0 ; -1 ; 65 ; 99 ]
            // 40 is a real-time signal on Linux and nothing at all on Darwin.
            | SignalNumbering.Darwin -> [ 0 ; -1 ; 32 ; 40 ; 99 ]

        for numbering in everyNumbering do
            for raw in notASignal numbering do
                let signal = Signal.Other raw
                let s = initial numbering

                Assert.Throws (fun () -> enable signal s |> ignore<SignalState<_, _>>)
                |> ignore<exn>

                Assert.Throws (fun () -> restoreDefault signal s |> ignore<SignalState<_, _>>)
                |> ignore<exn>

                Assert.Throws (fun () -> SignalState.disposition signal s |> ignore<SignalDisposition<_>>)
                |> ignore<exn>

                Assert.Throws (fun () ->
                    SignalState.initial numbering (Set.singleton signal)
                    |> ignore<SignalState<TestTask, TestHandler>>
                )
                |> ignore<exn>

                Assert.Throws (fun () -> SignalState.block t0 signal s |> ignore<SignalState<_, _>>)
                |> ignore<exn>

                Assert.Throws (fun () -> SignalState.unblock t0 signal s |> ignore<SignalState<_, _>>)
                |> ignore<exn>

                Assert.Throws (fun () -> SignalState.isBlocked t0 signal s |> ignore<bool>)
                |> ignore<exn>

                Assert.Throws (fun () ->
                    SignalState.enqueue
                        {
                            Signal = signal
                            Target = ValueNone
                        }
                        s
                    |> ignore<SignalState<_, _>>
                )
                |> ignore<exn>

    // ------------------- Queue and delivery ------------------- //

    [<Test>]
    let ``enqueue keeps each set in the order it is taken in, whatever order it was generated in`` () : unit =
        // No kernel delivers in generation order (`TestSignalPickOrder`), so two
        // generation orders of the same signals are one state.
        let a =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let b =
            {
                Signal = Signal.SIGHUP
                Target = ValueNone
            }

        let s = empty |> SignalState.enqueue a |> SignalState.enqueue b

        SignalState.pending s |> Seq.toList |> shouldEqual [ b ; a ]
        s |> shouldEqual (empty |> SignalState.enqueue b |> SignalState.enqueue a)

    [<Test>]
    let ``enqueue coalesces a standard signal already pending in the same set`` () : unit =
        // A kernel holds at most one pending instance of a standard signal
        // per pending set: measured, three process-directed SIGUSR1 while
        // blocked deliver once, on both platforms. The discarded duplicate
        // leaves the state structurally identical, not merely equivalent.
        for target in [ ValueNone ; ValueSome t1 ] do
            let e =
                {
                    Signal = Signal.SIGINT
                    Target = target
                }

            let once = empty |> SignalState.enqueue e
            let twice = once |> SignalState.enqueue e

            twice |> shouldEqual once
            SignalState.pending twice |> shouldEqual [ e ]

    [<Test>]
    let ``enqueue coalesces across spellings of one signal`` () : unit =
        // The coalescing key is the canonical signal: SIGUSR1 pending and a
        // second generation spelt as its raw number are one signal. (SIGUSR1
        // rather than an ignore-default signal, so the entries survive
        // generation under both numberings; its number also diverges, which
        // keeps the spelling non-trivial.)
        for numbering in everyNumbering do
            let spelt = Signal.Other (Signal.toRawSignoUnder numbering Signal.SIGUSR1)

            let s =
                initial numbering
                |> SignalState.enqueue
                    {
                        Signal = Signal.SIGUSR1
                        Target = ValueNone
                    }
                |> SignalState.enqueue
                    {
                        Signal = spelt
                        Target = ValueNone
                    }

            SignalState.pending s
            |> shouldEqual
                [
                    {
                        Signal = Signal.SIGUSR1
                        Target = ValueNone
                    }
                ]

    [<Test>]
    let ``enqueue keeps separate pending sets separate`` () : unit =
        // Measured: a process-directed plus a thread-directed instance of one
        // standard signal deliver twice (Linux, and a two-thread Darwin
        // process), so ValueNone and each ValueSome are distinct sets — as
        // are two different threads' own sets.
        let processDirected =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let atT0 =
            {
                Signal = Signal.SIGINT
                Target = ValueSome t0
            }

        let atT1 =
            {
                Signal = Signal.SIGINT
                Target = ValueSome t1
            }

        let s =
            empty
            |> SignalState.enqueue processDirected
            |> SignalState.enqueue atT0
            |> SignalState.enqueue atT1
            // And a redundant round: each is already pending in its own set.
            |> SignalState.enqueue processDirected
            |> SignalState.enqueue atT0

        SignalState.pending s |> shouldEqual [ processDirected ; atT0 ; atT1 ]

    [<Test>]
    let ``a real-time signal queues without coalescing under Linux numbering`` () : unit =
        // Measured on Linux 6.18.5: three generations of signo 36 while
        // blocked deliver three times, via sigqueue and via kill alike.
        let rt =
            {
                Signal = Signal.Other 36
                Target = ValueNone
            }

        let s =
            initial SignalNumbering.Linux
            |> SignalState.enqueue rt
            |> SignalState.enqueue rt
            |> SignalState.enqueue rt

        SignalState.pending s |> shouldEqual [ rt ; rt ; rt ]

        // And each queued instance delivers separately.
        let s = s |> enable (Signal.Other 36)

        let s =
            match leaderDelivery [ t0 ] s with
            | Some (SignalDelivery.RunHandler (e, _)), s' ->
                e |> shouldEqual rt
                s'
            | other, _ -> failwith $"expected the first real-time instance to deliver, got %A{other}"

        SignalState.pending s |> shouldEqual [ rt ; rt ]

    [<Test>]
    let ``structural equality survives a non-empty pending queue`` () : unit =
        // `ImmutableQueue<T>` compares by reference, so storing `Pending` in
        // one would make two independently-built states with identical
        // contents compare unequal once the queue was non-empty.
        // `EmulatedKernel` (which embeds `SignalState`) is compared
        // structurally for deterministic state dedup; this test pins
        // down that the contract holds across every operation that
        // touches the queue.
        let entryA =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let entryB =
            {
                Signal = Signal.SIGHUP
                Target = ValueSome t1
            }

        let buildA () =
            empty
            |> enable Signal.SIGINT
            |> SignalState.block t0 Signal.SIGTERM
            |> SignalState.enqueue entryA
            |> SignalState.enqueue entryB

        let buildB () =
            empty
            |> enable Signal.SIGINT
            |> SignalState.block t0 Signal.SIGTERM
            |> SignalState.enqueue entryA
            |> SignalState.enqueue entryB

        let a = buildA ()
        let b = buildB ()
        a |> shouldEqual b
        hash a |> shouldEqual (hash b)

        // The state after delivery must also compare equal to an
        // independently-rebuilt equivalent — exercises the path where
        // nextDelivery rebuilds the pending list from a skipped/tail
        // split.
        let drainedFromA =
            match leaderDelivery [ t0 ; t1 ] a with
            | Some (SignalDelivery.RunHandler _), s' -> s'
            | other, _ -> failwith $"expected a handler delivery from buildA, got %A{other}"

        let drainedFromB =
            match leaderDelivery [ t0 ; t1 ] b with
            | Some (SignalDelivery.RunHandler _), s' -> s'
            | other, _ -> failwith $"expected a handler delivery from buildB, got %A{other}"

        drainedFromA |> shouldEqual drainedFromB
        hash drainedFromA |> shouldEqual (hash drainedFromB)

    [<Test>]
    let ``nextDelivery surfaces the kernel default for a pending signal nobody enabled`` () : unit =
        // The scenario #1380 called out: a pending, non-enabled SIGTERM must
        // not sit queued forever — a kernel applies SIG_DFL and terminates.
        for signal in [ Signal.SIGTERM ; Signal.SIGINT ; Signal.SIGHUP ] do
            let s =
                empty
                |> SignalState.enqueue
                    {
                        Signal = signal
                        Target = ValueNone
                    }

            let delivery, s' = leaderDelivery [ t0 ] s

            delivery |> shouldEqual (Some (SignalDelivery.DefaultTerminate (signal, false)))
            SignalState.pending s' |> shouldEqual []

    [<Test>]
    let ``nextDelivery surfaces Stop and Continue defaults`` () : unit =
        let deliveryFor (signal : Signal) : SignalDelivery<TestTask, TestHandler> option =
            empty
            |> SignalState.enqueue
                {
                    Signal = signal
                    Target = ValueNone
                }
            |> leaderDelivery [ t0 ]
            |> fst

        deliveryFor Signal.SIGTSTP
        |> shouldEqual (Some (SignalDelivery.DefaultStop Signal.SIGTSTP))

        deliveryFor Signal.SIGCONT
        |> shouldEqual (Some (SignalDelivery.DefaultContinue Signal.SIGCONT))

    [<Test>]
    let ``a Continue default bypasses masks`` () : unit =
        // Resumption happens at generation on a real kernel, whatever any
        // mask says: measured on Linux 6.18.5 and Darwin 25.6.0 (two runs
        // each), a child that blocks SIGCONT and stops itself is resumed by
        // SIGCONT anyway — the mask defers only handler delivery. So the
        // event must surface even when every task blocks SIGCONT.
        let s =
            empty
            |> SignalState.block t0 Signal.SIGCONT
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGCONT
                    Target = ValueNone
                }

        let delivery, s' = leaderDelivery [ t0 ] s

        delivery |> shouldEqual (Some (SignalDelivery.DefaultContinue Signal.SIGCONT))
        SignalState.pending s' |> shouldEqual []

    [<Test>]
    let ``generating an unclaimed fatal signal terminates the process at once`` () : unit =
        let entry =
            {
                Signal = Signal.SIGTERM
                Target = ValueNone
            }

        generateAmong CoreDumps.Suppressed [ t0 ] entry empty
        |> shouldEqual (SignalGeneration.ProcessTerminated (Signal.SIGTERM, false))

        // SIGKILL cannot be blocked, so a mask naming it changes nothing.
        let masked = empty |> SignalState.block t0 (Signal.Other 9)

        generateAmong
            CoreDumps.Suppressed
            [ t0 ]
            {
                Signal = (Signal.Other 9)
                Target = ValueNone
            }
            masked
        |> shouldEqual (SignalGeneration.ProcessTerminated (Signal.Other 9, false))

    [<Test>]
    let ``generating an unclaimed stop signal stops the process at once`` () : unit =
        generateAmong
            CoreDumps.Suppressed
            [ t0 ]
            {
                Signal = (Signal.Other 19)
                Target = ValueNone
            }
            empty
        |> shouldEqual (SignalGeneration.ProcessStopped (Signal.Other 19, empty))

    [<Test>]
    let ``a fatal signal nobody can receive yet is queued, not fatal`` () : unit =
        let entry =
            {
                Signal = Signal.SIGTERM
                Target = ValueNone
            }

        // Every task blocks it.
        let blocked =
            empty
            |> SignalState.block t0 Signal.SIGTERM
            |> SignalState.block t1 Signal.SIGTERM

        let s' =
            generateAmong CoreDumps.Suppressed [ t0 ; t1 ] entry blocked |> continuesWith

        SignalState.pending s' |> shouldEqual [ entry ]

        // A thread-directed one whose target blocks it waits even though
        // another task would not block it.
        let directed =
            { entry with
                Target = ValueSome t0
            }

        let s' =
            generateAmong CoreDumps.Suppressed [ t0 ; t1 ] directed blocked |> continuesWith

        SignalState.pending s' |> shouldEqual [ directed ]

    [<Test>]
    let ``a signal a handler claims is queued for it`` () : unit =
        let entry =
            {
                Signal = Signal.SIGTERM
                Target = ValueNone
            }

        let claimed = empty |> enable Signal.SIGTERM

        let s' = generateAmong CoreDumps.Suppressed [ t0 ] entry claimed |> continuesWith

        SignalState.pending s' |> shouldEqual [ entry ]

    [<Test>]
    let ``an ignored signal some thread could receive is discarded at generation`` () : unit =
        for numbering in everyNumbering do
            let entry =
                {
                    Signal = Signal.SIGCHLD
                    Target = ValueNone
                }

            let s = initial numbering

            generateAmong CoreDumps.Suppressed [ t0 ] entry s
            |> shouldEqual (SignalGeneration.ProcessContinues s)

            // Nothing is left for a later handler to claim.
            let claimedLater =
                generateAmong CoreDumps.Suppressed [ t0 ] entry s
                |> continuesWith
                |> enable Signal.SIGCHLD

            leaderDelivery [ t0 ] claimedLater |> fst |> shouldEqual None

    [<Test>]
    let ``an ignored signal every thread blocks is left pending at generation under Linux numbering`` () : unit =
        let entry =
            {
                Signal = Signal.SIGCHLD
                Target = ValueNone
            }

        let blocked = empty |> SignalState.block t0 Signal.SIGCHLD

        let s' = generateAmong CoreDumps.Suppressed [ t0 ] entry blocked |> continuesWith

        SignalState.pending s' |> shouldEqual [ entry ]

    [<Test>]
    let ``a default action still requires a receiver`` () : unit =
        // A pending terminate-default signal blocked by every live thread
        // stays pending, exactly as a handler delivery would.
        let s =
            empty
            |> SignalState.block t0 Signal.SIGTERM
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGTERM
                    Target = ValueNone
                }

        let delivery, s' = leaderDelivery [ t0 ] s

        delivery |> shouldEqual None
        s' |> shouldEqual s

    [<Test>]
    let ``a receivable ignored signal is discarded with no action`` () : unit =
        // Linux keeps an ignored signal pending only while nothing could
        // receive it; the delivery scan is what discards it. The scan must
        // report the state change even though there is no action.
        let s =
            empty
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGCHLD
                    Target = ValueNone
                }

        SignalState.pending s |> List.length |> shouldEqual 1

        let delivery, s' = leaderDelivery [ t0 ] s

        delivery |> shouldEqual None
        SignalState.pending s' |> shouldEqual []
        s' |> shouldEqual empty

    [<Test>]
    let ``an ignored signal blocked everywhere stays pending under Linux numbering`` () : unit =
        // The measured Linux rule: blocked-and-ignored stays pending, and a
        // handler enabled before the unblock receives it — or, still ignored
        // when a receiver appears, it is discarded.
        let entry =
            {
                Signal = Signal.SIGCHLD
                Target = ValueNone
            }

        let held = empty |> SignalState.block t0 Signal.SIGCHLD |> SignalState.enqueue entry

        let delivery, afterScan = leaderDelivery [ t0 ] held

        delivery |> shouldEqual None
        SignalState.pending afterScan |> shouldEqual [ entry ]

        // Handler claimed before the unblock: delivered.
        let claimed =
            afterScan |> enable Signal.SIGCHLD |> SignalState.unblock t0 Signal.SIGCHLD

        match leaderDelivery [ t0 ] claimed with
        | Some (SignalDelivery.RunHandler (e, _)), s' ->
            e |> shouldEqual entry
            SignalState.pending s' |> shouldEqual []
        | other, _ -> failwith $"expected the held SIGCHLD to deliver, got %A{other}"

        // Still ignored at the unblock: discarded.
        let discarded = afterScan |> SignalState.unblock t0 Signal.SIGCHLD

        let delivery, s' = leaderDelivery [ t0 ] discarded

        delivery |> shouldEqual None
        SignalState.pending s' |> shouldEqual []

    [<Test>]
    let ``Darwin numbering discards an ignored signal at generation even when blocked`` () : unit =
        // The measured Darwin rule: the block does not preserve it, so the
        // handler-before-unblock rescue that works on Linux has nothing to
        // rescue.
        let blocked = initial SignalNumbering.Darwin |> SignalState.block t0 Signal.SIGCHLD

        let s =
            blocked
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGCHLD
                    Target = ValueNone
                }

        s |> shouldEqual blocked
        SignalState.pending s |> shouldEqual []

    [<Test>]
    let ``a discarded ignored entry does not stop the scan`` () : unit =
        // An ignored entry taken first (SIGHUP is 1) is dropped and the walk
        // carries on to deliver the caught entry behind it, all in one call.
        let ignored =
            {
                Signal = Signal.SIGHUP
                Target = ValueNone
            }

        let handled =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let s =
            empty
            |> SignalState.setDisposition Signal.SIGHUP SignalDisposition.Ignore
            |> enable Signal.SIGINT
            |> SignalState.enqueue ignored
            |> SignalState.enqueue handled

        SignalState.pending s |> shouldEqual [ ignored ; handled ]

        match leaderDelivery [ t0 ] s with
        | Some (SignalDelivery.RunHandler (e, _)), s' ->
            e |> shouldEqual handled
            SignalState.pending s' |> shouldEqual []
        | other, _ -> failwith $"expected the enabled entry to deliver past the discard, got %A{other}"

    [<Test>]
    let ``nextDelivery returns nothing for an empty queue`` () : unit =
        let delivery, s' = leaderDelivery [ t0 ] empty

        delivery |> shouldEqual None
        s' |> shouldEqual empty

    [<Test>]
    let ``nextDelivery gives a signal pending on the process to the leader alone`` () : unit =
        let entry =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let s = empty |> enable Signal.SIGINT |> SignalState.enqueue entry
        let tasks = Set.ofList [ t2 ; t0 ; t1 ]

        for task in [ t1 ; t2 ] do
            SignalState.nextDelivery CoreDumps.Suppressed t0 tasks task s
            |> shouldEqual (Ok (None, s))

        SignalState.nextDelivery CoreDumps.Suppressed t0 tasks t0 s
        |> shouldEqual (Ok (Some (SignalDelivery.RunHandler (entry, handler)), empty |> enable Signal.SIGINT))

    [<Test>]
    let ``nextDelivery refuses every task while a caught signal pending on the process could reach only a non-leader``
        ()
        : unit
        =
        let s =
            empty
            |> enable Signal.SIGINT
            |> SignalState.block t0 Signal.SIGINT
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueNone
                }

        for task in [ t0 ; t1 ; t2 ] do
            SignalState.nextDelivery CoreDumps.Suppressed t0 (Set.ofList [ t0 ; t1 ; t2 ]) task s
            |> shouldEqual (Error (SignalReceiverRefusal.LeaderBlocks Signal.SIGINT))

    [<Test>]
    let ``generate refuses a caught signal for the process that only a non-leader could receive`` () : unit =
        let s =
            empty
            |> enable Signal.SIGINT
            |> SignalState.block t0 Signal.SIGINT
            |> SignalState.block t1 Signal.SIGINT

        let entry =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        SignalState.generate CoreDumps.Suppressed t0 (Set.ofList [ t0 ; t1 ; t2 ]) entry s
        |> shouldEqual (Error (SignalReceiverRefusal.LeaderBlocks Signal.SIGINT))

        // Once every task blocks it, it is simply pending.
        SignalState.generate CoreDumps.Suppressed t0 (Set.ofList [ t0 ; t1 ]) entry s
        |> Result.map continuesWith
        |> Result.map SignalState.pending
        |> shouldEqual (Ok [ entry ])

    [<Test>]
    let ``a default SIGCONT pending on the process is no reason to refuse, whoever blocks it`` () : unit =
        // Resuming the process needs no receiver, so it is answered even while
        // only a non-leader could have received it.
        let entry =
            {
                Signal = Signal.SIGCONT
                Target = ValueNone
            }

        let s = empty |> SignalState.block t0 Signal.SIGCONT |> SignalState.enqueue entry

        SignalState.nextDelivery CoreDumps.Suppressed t0 (Set.ofList [ t0 ; t1 ]) t0 s
        |> shouldEqual (
            Ok (Some (SignalDelivery.DefaultContinue Signal.SIGCONT), empty |> SignalState.block t0 Signal.SIGCONT)
        )

    [<Test>]
    let ``nextDelivery holds a signal every task blocks`` () : unit =
        let s =
            empty
            |> enable Signal.SIGINT
            |> SignalState.block t0 Signal.SIGINT
            |> SignalState.block t1 Signal.SIGINT
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueNone
                }

        let delivery, s' = leaderDelivery [ t0 ; t1 ] s

        delivery |> shouldEqual None
        s' |> shouldEqual s

    [<Test>]
    let ``nextDelivery does not redirect a targeted signal to another thread`` () : unit =
        // pthread_kill is pinned: even though t1 is unblocked, a signal
        // targeted at t0 must stay queued, not get delivered to t1.
        let s =
            empty
            |> enable Signal.SIGINT
            |> SignalState.block t0 Signal.SIGINT
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueSome t0
                }

        for task in [ t0 ; t1 ] do
            SignalState.nextDelivery CoreDumps.Suppressed t0 (Set.ofList [ t0 ; t1 ]) task s
            |> shouldEqual (Ok (None, s))

    [<Test>]
    let ``a signal pending on something that is not a task fails loudly`` () : unit =
        let s =
            empty
            |> enable Signal.SIGINT
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueSome t2
                }

        let exn = Assert.Throws<exn> (fun () -> leaderDelivery [ t0 ; t1 ] s |> ignore)

        exn.Message |> shouldContainText "not one of the process's tasks"

    [<Test>]
    let ``asking with a leader or task that is not a task fails loudly`` () : unit =
        let tasks = Set.ofList [ t0 ; t1 ]

        (Assert.Throws<exn> (fun () -> SignalState.nextDelivery CoreDumps.Suppressed t2 tasks t0 empty |> ignore))
            .Message
        |> shouldContainText "leader"

        (Assert.Throws<exn> (fun () -> SignalState.nextDelivery CoreDumps.Suppressed t0 tasks t2 empty |> ignore))
            .Message
        |> shouldContainText "task asked"

        (Assert.Throws<exn> (fun () ->
            SignalState.generate
                CoreDumps.Suppressed
                t2
                tasks
                {
                    Signal = Signal.SIGINT
                    Target = ValueNone
                }
                empty
            |> ignore
        ))
            .Message
        |> shouldContainText "leader"

    [<Test>]
    let ``nextDelivery walks past what the task blocks and what is not its own, leaving both`` () : unit =
        // SIGHUP (1) is pending on t1 alone, SIGINT (2) on the process, and
        // SIGQUIT (3) on the process but blocked everywhere: the leader skips the
        // first as not its own and the last as blocked, and takes SIGINT.
        let atT1 =
            {
                Signal = Signal.SIGHUP
                Target = ValueSome t1
            }

        let deliverable =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let held =
            {
                Signal = Signal.SIGQUIT
                Target = ValueNone
            }

        let s =
            empty
            |> enable Signal.SIGHUP
            |> enable Signal.SIGINT
            |> enable Signal.SIGQUIT
            |> SignalState.block t0 Signal.SIGQUIT
            |> SignalState.block t1 Signal.SIGQUIT
            |> SignalState.enqueue held
            |> SignalState.enqueue atT1
            |> SignalState.enqueue deliverable

        match leaderDelivery [ t0 ; t1 ] s with
        | Some (SignalDelivery.RunHandler (e, _)), s' ->
            e |> shouldEqual deliverable
            SignalState.pending s' |> Seq.toList |> shouldEqual [ held ; atT1 ]
        | other, _ -> failwith $"expected a handler delivery, got %A{other}"

    // ----------------------- Property tests ----------------------- //

    /// Operation language for the random property test. Each constructor
    /// maps to exactly one public method on the API, except `Spawn`, which is
    /// a task joining the process: it has no entries yet, so only the task set
    /// the operations are given changes.
    type private Op =
        | SetDisposition of signal : Signal * disposition : SignalDisposition<TestHandler>
        | Block of thread : TestTask * signal : Signal
        | Unblock of thread : TestTask * signal : Signal
        | Enqueue of entry : PendingSignal<TestTask>
        | Generate of coreDumps : CoreDumps * entry : PendingSignal<TestTask>
        | Deliver of coreDumps : CoreDumps * task : TestTask
        | Spawn of task : TestTask
        | Exit of task : TestTask

    /// The leader of every process the property test runs. It never exits.
    let private referenceLeader : TestTask = t0

    /// Every task the property test's processes can have.
    let private taskPool : TestTask list = [ t0 ; t1 ; t2 ; TestTask 3 ]

    /// Reference implementation: simple lists / sets / maps, completely
    /// independent of the production module's internal representation. Every
    /// signal is stored canonically, via `Signal.canonicalUnder` — whose two
    /// columns `TestSignal` pins independently — so a production module that
    /// forgot to canonicalise diverges from it on the first `Other` spelling.
    ///
    /// It stores every disposition it is given, `Default` included: the
    /// production module must store none, and `assertEquivalent` checks that
    /// it answers every read the same way *and* holds no stored default.
    ///
    /// It keeps pending signals in the order they were generated, and sorts
    /// them only when asked which comes first, where the production module
    /// keeps them sorted as they arrive.
    type private ReferenceState =
        {
            Dispositions : Map<Signal, SignalDisposition<TestHandler>>
            Blocked : Map<TestTask, Set<Signal>>
            Pending : PendingSignal<TestTask> list
        }

    let private referenceEmpty : ReferenceState =
        {
            Dispositions = Map.empty
            Blocked = Map.empty
            Pending = []
        }

    let private referenceDisposition (r : ReferenceState) (signal : Signal) : SignalDisposition<TestHandler> =
        Map.tryFind signal r.Dispositions
        |> Option.defaultValue SignalDisposition.Default

    let private referenceBlocks (r : ReferenceState) (task : TestTask) (signal : Signal) : bool =
        match Map.tryFind task r.Blocked with
        | None -> false
        | Some set -> Set.contains signal set

    /// The measured table, stated per disposition rather than through the
    /// production module's helpers: which dispositions ignore a signal as it
    /// is generated.
    let private referenceIgnoredAtGeneration
        (numbering : SignalNumbering)
        (r : ReferenceState)
        (signal : Signal)
        : bool
        =
        match referenceDisposition r signal, Signal.defaultDispositionUnder numbering signal with
        | SignalDisposition.Ignore, _ -> true
        | SignalDisposition.Default, DefaultDisposition.Ignore -> true
        | _, _ -> false

    /// Which dispositions discard a signal's pending instances as they are
    /// set: the generation-time ones, plus a default SIGCONT.
    let private referenceDiscardsWhenSet
        (numbering : SignalNumbering)
        (disposition : SignalDisposition<TestHandler>)
        (signal : Signal)
        : bool
        =
        match disposition, Signal.defaultDispositionUnder numbering signal with
        | SignalDisposition.Ignore, _ -> true
        | SignalDisposition.Default, DefaultDisposition.Ignore -> true
        | SignalDisposition.Default, DefaultDisposition.Continue -> true
        | _, _ -> false

    /// Whether `signal` is at its default and its default continues the process.
    let private referenceContinuesAtDefault
        (numbering : SignalNumbering)
        (r : ReferenceState)
        (signal : Signal)
        : bool
        =
        referenceDisposition r signal = SignalDisposition.Default
        && Signal.defaultDispositionUnder numbering signal = DefaultDisposition.Continue

    [<RequireQualifiedAccess>]
    type private ReferenceReceiver =
        | Task of TestTask
        | BeyondLeader
        | Nobody

    /// Who would take `e` now: its target, if not blocking; for the process's
    /// own, the leader if not blocking, and otherwise "some other task", which
    /// the model does not name.
    let private referenceReceiver
        (tasks : Set<TestTask>)
        (r : ReferenceState)
        (e : PendingSignal<TestTask>)
        : ReferenceReceiver
        =
        match e.Target with
        | ValueSome task ->
            if referenceBlocks r task e.Signal then
                ReferenceReceiver.Nobody
            else
                ReferenceReceiver.Task task
        | ValueNone ->
            if not (referenceBlocks r referenceLeader e.Signal) then
                ReferenceReceiver.Task referenceLeader
            elif
                tasks
                |> Set.toList
                |> List.exists (fun task -> not (referenceBlocks r task e.Signal))
            then
                ReferenceReceiver.BeyondLeader
            else
                ReferenceReceiver.Nobody

    /// The order of the measured pick rules, written out from the probe's rows
    /// rather than taken from the module: Linux takes ILL, TRAP, BUS, FPE, SEGV
    /// and SYS (4, 5, 7, 8, 11, 31) first, then the lowest number; Darwin the
    /// lowest number.
    let private referencePickKey (numbering : SignalNumbering) (signal : Signal) : int * int =
        let signo = Signal.toRawSignoUnder numbering signal

        match numbering with
        | SignalNumbering.Linux when List.contains signo [ 4 ; 5 ; 7 ; 8 ; 11 ; 31 ] -> 0, signo
        | SignalNumbering.Linux -> 1, signo
        | SignalNumbering.Darwin -> 0, signo

    /// What `task` could take, in the order it takes them: a naive stable sort
    /// of the entries in generation order. Linux takes its own set before the
    /// process's; Darwin takes them as one, its own first where they tie.
    let private referenceCandidates
        (numbering : SignalNumbering)
        (task : TestTask)
        (r : ReferenceState)
        : PendingSignal<TestTask> list
        =
        let own = r.Pending |> List.filter (fun e -> e.Target = ValueSome task)

        let shared =
            if task = referenceLeader then
                r.Pending |> List.filter (fun e -> e.Target = ValueNone)
            else
                []

        let byKey (entries : PendingSignal<TestTask> list) : PendingSignal<TestTask> list =
            entries |> List.sortBy (fun e -> referencePickKey numbering e.Signal)

        match numbering with
        | SignalNumbering.Linux -> byKey own @ byKey shared
        | SignalNumbering.Darwin -> byKey (own @ shared)

    /// Every pending entry grouped by set, the process's first, each set in the
    /// order it is taken in: what `SignalState.pending` promises.
    let private referencePendingView (numbering : SignalNumbering) (r : ReferenceState) : PendingSignal<TestTask> list =
        r.Pending
        |> List.sortBy (fun e -> e.Target, referencePickKey numbering e.Signal)

    /// `pending` without its first entry equal to `e`: the earliest generated.
    let rec private referenceRemoveFirst
        (e : PendingSignal<TestTask>)
        (pending : PendingSignal<TestTask> list)
        : PendingSignal<TestTask> list
        =
        match pending with
        | [] -> failwith $"reference: %A{e} is not pending"
        | head :: tail when head = e -> tail
        | head :: tail -> head :: referenceRemoveFirst e tail

    /// The first step of generating `signal` (canonical): `None` if Darwin
    /// drops it as ignored before anything else (SIGCONT excepted), and
    /// otherwise the state after the opposite kind's pending instances are
    /// flushed.
    let private referenceBeginGeneration
        (numbering : SignalNumbering)
        (signal : Signal)
        (r : ReferenceState)
        : ReferenceState option
        =
        let droppedFirst =
            numbering = SignalNumbering.Darwin
            && signal <> Signal.SIGCONT
            && referenceIgnoredAtGeneration numbering r signal

        if droppedFirst then
            None
        else
            let isStop (x : Signal) : bool =
                Signal.defaultDispositionUnder numbering x = DefaultDisposition.Stop

            let flushes (pendingSignal : Signal) : bool =
                (signal = Signal.SIGCONT && isStop pendingSignal)
                || (isStop signal && pendingSignal = Signal.SIGCONT)

            Some
                { r with
                    Pending = r.Pending |> List.filter (fun p -> not (flushes p.Signal))
                }

    /// Coalescing of standard signals, on an entry already admitted.
    let private referenceAdmit
        (numbering : SignalNumbering)
        (entry : PendingSignal<TestTask>)
        (r : ReferenceState)
        : ReferenceState
        =
        let alreadyPendingInSet =
            r.Pending
            |> List.exists (fun p -> p.Signal = entry.Signal && p.Target = entry.Target)

        if alreadyPendingInSet && not (Signal.isRealTimeUnder numbering entry.Signal) then
            r
        else
            { r with
                Pending = r.Pending @ [ entry ]
            }

    let private referenceEnqueue
        (numbering : SignalNumbering)
        (entry : PendingSignal<TestTask>)
        (r : ReferenceState)
        : ReferenceState
        =
        match referenceBeginGeneration numbering entry.Signal r with
        | None -> r
        | Some r -> referenceAdmit numbering entry r

    let private referenceCore (numbering : SignalNumbering) (coreDumps : CoreDumps) (signal : Signal) : bool =
        coreDumps = CoreDumps.Written && Signal.dumpsCoreUnder numbering signal

    /// The reference's `SignalGeneration`: what generating a signal does, with
    /// the state the process carries on with where it carries on.
    [<RequireQualifiedAccess>]
    type private ReferenceGeneration =
        | Continues of ReferenceState
        | Terminated of Signal * coreDumped : bool
        | Stopped of Signal * ReferenceState
        | Refused of Signal

    /// What generating `entry` (canonical) does at once, and the state after.
    /// A default-disposition signal some task could receive takes its default
    /// here: terminate, stop, or be discarded if the default ignores it; an
    /// ignored one some task could receive is discarded. A caught one for the
    /// process that only a non-leader could receive is refused.
    let private referenceGenerate
        (numbering : SignalNumbering)
        (coreDumps : CoreDumps)
        (tasks : Set<TestTask>)
        (entry : PendingSignal<TestTask>)
        (r : ReferenceState)
        : ReferenceGeneration
        =
        match referenceBeginGeneration numbering entry.Signal r with
        | None -> ReferenceGeneration.Continues r
        | Some r ->
            let receiver = referenceReceiver tasks r entry

            match
                receiver, referenceDisposition r entry.Signal, Signal.defaultDispositionUnder numbering entry.Signal
            with
            | ReferenceReceiver.Nobody, _, _ -> ReferenceGeneration.Continues (referenceAdmit numbering entry r)
            | ReferenceReceiver.BeyondLeader, SignalDisposition.Catch _, _ -> ReferenceGeneration.Refused entry.Signal
            | _, SignalDisposition.Catch _, _ -> ReferenceGeneration.Continues (referenceAdmit numbering entry r)
            | _, SignalDisposition.Ignore, _ -> ReferenceGeneration.Continues r
            | _, SignalDisposition.Default, DefaultDisposition.Terminate ->
                ReferenceGeneration.Terminated (entry.Signal, referenceCore numbering coreDumps entry.Signal)
            | _, SignalDisposition.Default, DefaultDisposition.Stop -> ReferenceGeneration.Stopped (entry.Signal, r)
            | _, SignalDisposition.Default, DefaultDisposition.Ignore -> ReferenceGeneration.Continues r
            | _, SignalDisposition.Default, DefaultDisposition.Continue ->
                ReferenceGeneration.Continues (referenceAdmit numbering entry r)

    /// Index-based walk over the candidates as an array: a distinct algorithm
    /// from the production module's recursive walk, so a regression in either
    /// side surfaces as a divergence.
    let private referenceNextDelivery
        (numbering : SignalNumbering)
        (coreDumps : CoreDumps)
        (tasks : Set<TestTask>)
        (task : TestTask)
        (r : ReferenceState)
        : Result<SignalDelivery<TestTask, TestHandler> option * ReferenceState, SignalReceiverRefusal>
        =
        let refused =
            referencePendingView numbering r
            |> List.tryFind (fun e ->
                referenceReceiver tasks r e = ReferenceReceiver.BeyondLeader
                && not (referenceContinuesAtDefault numbering r e.Signal)
            )

        match refused with
        | Some e -> Error (SignalReceiverRefusal.LeaderBlocks e.Signal)
        | None ->

        let candidates = referenceCandidates numbering task r |> List.toArray
        let mutable pending = r.Pending
        let mutable result : SignalDelivery<TestTask, TestHandler> option = None
        let mutable i = 0

        while result.IsNone && i < candidates.Length do
            let entry = candidates.[i]

            if referenceContinuesAtDefault numbering r entry.Signal then
                pending <- referenceRemoveFirst entry pending
                result <- Some (SignalDelivery.DefaultContinue entry.Signal)
            elif not (referenceBlocks r task entry.Signal) then
                match referenceDisposition r entry.Signal with
                | SignalDisposition.Catch h ->
                    pending <- referenceRemoveFirst entry pending
                    result <- Some (SignalDelivery.RunHandler (entry, h))
                | SignalDisposition.Ignore -> pending <- referenceRemoveFirst entry pending
                | SignalDisposition.Default ->
                    match Signal.defaultDispositionUnder numbering entry.Signal with
                    | DefaultDisposition.Ignore -> pending <- referenceRemoveFirst entry pending
                    | DefaultDisposition.Terminate ->
                        pending <- referenceRemoveFirst entry pending

                        result <-
                            Some (
                                SignalDelivery.DefaultTerminate (
                                    entry.Signal,
                                    referenceCore numbering coreDumps entry.Signal
                                )
                            )
                    | DefaultDisposition.Stop ->
                        pending <- referenceRemoveFirst entry pending
                        result <- Some (SignalDelivery.DefaultStop entry.Signal)
                    | DefaultDisposition.Continue -> failwith "unreachable: handled above"

            i <- i + 1

        Ok (
            result,
            { r with
                Pending = pending
            }
        )

    /// Advance both implementations by one op, asserting agreement on
    /// `nextDelivery`'s full returned action (since the next step's
    /// observable state alone cannot always distinguish a divergence in
    /// which entry was consumed). Answers the task set after the op too.
    let private stepBoth
        (numbering : SignalNumbering)
        (tasks : Set<TestTask>)
        (op : Op)
        (s : SignalState<TestTask, TestHandler>)
        (r : ReferenceState)
        : SignalState<TestTask, TestHandler> * ReferenceState * Set<TestTask>
        =
        let canonical (signal : Signal) : Signal = Signal.canonicalUnder numbering signal

        match op with
        | Op.SetDisposition (sig0, disposition) ->
            let signal = canonical sig0

            let pending =
                if referenceDiscardsWhenSet numbering disposition signal then
                    r.Pending |> List.filter (fun p -> p.Signal <> signal)
                else
                    r.Pending

            SignalState.setDisposition sig0 disposition s,
            { r with
                Dispositions = Map.add signal disposition r.Dispositions
                Pending = pending
            },
            tasks
        | Op.Block (tid, sig0) ->
            let reference =
                if Signal.isUnblockableUnder numbering sig0 then
                    r
                else
                    let existing : Set<Signal> =
                        match Map.tryFind tid r.Blocked with
                        | None -> Set.empty
                        | Some set -> set

                    { r with
                        Blocked = Map.add tid (Set.add (canonical sig0) existing) r.Blocked
                    }

            SignalState.block tid sig0 s, reference, tasks
        | Op.Unblock (tid, sig0) ->
            let r' : ReferenceState =
                match Map.tryFind tid r.Blocked with
                | None -> r
                | Some set ->
                    if not (Set.contains (canonical sig0) set) then
                        r
                    else
                        let set' = Set.remove (canonical sig0) set

                        let blocked =
                            if Set.isEmpty set' then
                                Map.remove tid r.Blocked
                            else
                                Map.add tid set' r.Blocked

                        { r with
                            Blocked = blocked
                        }

            SignalState.unblock tid sig0 s, r', tasks
        | Op.Enqueue e ->
            let entry =
                { e with
                    Signal = canonical e.Signal
                }

            SignalState.enqueue e s, referenceEnqueue numbering entry r, tasks
        | Op.Generate (coreDumps, e) ->
            let actual = SignalState.generate coreDumps referenceLeader tasks e s

            let expected =
                referenceGenerate
                    numbering
                    coreDumps
                    tasks
                    { e with
                        Signal = canonical e.Signal
                    }
                    r

            // A terminated process has no state to carry on with, and a refused
            // generation did not happen, so in both the run goes on from the
            // state the signal was generated in.
            match actual, expected with
            | Ok (SignalGeneration.ProcessContinues s'), ReferenceGeneration.Continues r' -> s', r', tasks
            | Ok (SignalGeneration.ProcessStopped (a, s')), ReferenceGeneration.Stopped (b, r') when a = b ->
                s', r', tasks
            | Ok (SignalGeneration.ProcessTerminated (a, aCore)), ReferenceGeneration.Terminated (b, bCore) when
                a = b && aCore = bCore
                ->
                s, r, tasks
            | Error (SignalReceiverRefusal.LeaderBlocks a), ReferenceGeneration.Refused b when a = b -> s, r, tasks
            | _ -> failwith $"generate disagreed: actual=%A{actual}, reference=%A{expected}"
        | Op.Deliver (coreDumps, task) ->
            let actual = SignalState.nextDelivery coreDumps referenceLeader tasks task s
            let expected = referenceNextDelivery numbering coreDumps tasks task r

            match actual, expected with
            | Ok (actualDelivery, s'), Ok (expectedDelivery, r') ->
                if actualDelivery <> expectedDelivery then
                    failwith $"nextDelivery disagreed: actual=%A{actualDelivery}, reference=%A{expectedDelivery}"

                s', r', tasks
            | Error a, Error b when a = b -> s, r, tasks
            | _ -> failwith $"nextDelivery disagreed: actual=%A{actual}, reference=%A{expected}"
        | Op.Spawn task -> s, r, Set.add task tasks
        | Op.Exit task ->
            SignalState.forgetTask task s,
            { r with
                Blocked = Map.remove task r.Blocked
                Pending = r.Pending |> List.filter (fun e -> e.Target <> ValueSome task)
            },
            Set.remove task tasks

    /// Compare every observable accessor; the accessors are the contract.
    /// Queries run over every legal spelling, so a production module that
    /// canonicalised its stores but not its reads diverges here.
    let private assertEquivalent
        (numbering : SignalNumbering)
        (tasks : Set<TestTask>)
        (s : SignalState<TestTask, TestHandler>)
        (r : ReferenceState)
        : unit
        =
        // Equal as maps, so a stored `Default` on the production side is a
        // failure even though every read below would agree with it.
        SignalState.dispositions s
        |> shouldEqual (r.Dispositions |> Map.filter (fun _ d -> d <> SignalDisposition.Default))

        SignalState.pending s
        |> Seq.toList
        |> shouldEqual (referencePendingView numbering r)

        for task in tasks do
            SignalState.pendingFor referenceLeader task s
            |> shouldEqual (referenceCandidates numbering task r)

        for sig0 in allSignals numbering do
            SignalState.disposition sig0 s
            |> shouldEqual (referenceDisposition r (Signal.canonicalUnder numbering sig0))

        for tid in taskPool do
            for sig0 in allSignals numbering do
                let actualBlocked = SignalState.isBlocked tid sig0 s

                let expectedBlocked = referenceBlocks r tid (Signal.canonicalUnder numbering sig0)

                if actualBlocked <> expectedBlocked then
                    failwith
                        $"isBlocked %O{tid} %O{sig0} disagreed: actual=%b{actualBlocked}, reference=%b{expectedBlocked}"

        for tid in taskPool do
            let actualMask = SignalState.blockedFor tid s

            let expectedMask =
                match Map.tryFind tid r.Blocked with
                | None -> Set.empty
                | Some set -> set

            actualMask |> shouldEqual expectedMask

    /// Every signal `setDisposition` accepts: everything but SIGKILL and
    /// SIGSTOP, glibc's reserved pair included.
    let private settableSignals (numbering : SignalNumbering) : Signal list =
        allSignals numbering
        |> List.filter (fun signal -> not (List.contains signal (kernelUncatchable numbering)))

    /// The signals the stop/continue flush acts between, over-weighted in the
    /// walk: a flush needs one pending and the other generated after it.
    let private jobControl (numbering : SignalNumbering) : Signal list =
        [
            Signal.SIGTSTP
            Signal.SIGTTIN
            Signal.SIGTTOU
            Signal.SIGCONT
            Signal.Other (Signal.toRawSignoUnder numbering Signal.SIGCONT)
        ]

    let private randomOp
        (numbering : SignalNumbering)
        (tasks : Set<TestTask>)
        (r : ReferenceState)
        (rng : System.Random)
        : Op
        =
        let pick (xs : 'a list) : 'a = xs.[rng.Next xs.Length]
        let current = Set.toList tasks

        let pickDisposition () : SignalDisposition<TestHandler> =
            match rng.Next 4 with
            | 0 -> SignalDisposition.Default
            | 1 -> SignalDisposition.Ignore
            | 2 -> catch
            | _ -> SignalDisposition.Catch (TestHandler "other")

        let pickCoreDumps () : CoreDumps =
            if rng.Next 2 = 0 then
                CoreDumps.Suppressed
            else
                CoreDumps.Written

        let pickGenerated () : Signal =
            // The real-time signal is over-weighted: observing
            // queue-not-coalesce needs the *same* signal generated twice
            // before a delivery consumes it. So are the job-control signals,
            // for the flush between them, and a few signals Linux takes early
            // or late, so that the two pick rules part company often, and
            // signals the leader blocks, which another task may not.
            let leaderMask =
                Map.tryFind referenceLeader r.Blocked
                |> Option.map Set.toList
                |> Option.defaultValue []

            match rng.Next 10 with
            | 0 when numbering = SignalNumbering.Linux -> Signal.Other 40
            | 1
            | 2 -> pick (jobControl numbering)
            | 3 -> pick [ Signal.Other 4 ; Signal.Other 11 ; Signal.SIGHUP ; Signal.SIGTERM ]
            | 4 when not leaderMask.IsEmpty -> pick leaderMask
            | _ -> pick (allSignals numbering)

        let pickTarget () : TestTask voption =
            if rng.Next 2 = 0 then
                ValueNone
            else
                ValueSome (pick current)

        let kind = rng.Next 100

        if kind < 25 then
            // Only what the kernel accepts: `setDisposition` fails loud on
            // SIGKILL and SIGSTOP, and the refusal has its own unit test.
            Op.SetDisposition (pick (settableSignals numbering), pickDisposition ())
        elif kind < 36 then
            Op.Block (pick current, pick (allSignals numbering))
        elif kind < 47 then
            Op.Unblock (pick current, pick (allSignals numbering))
        elif kind < 60 then
            Op.Enqueue
                {
                    Signal = pickGenerated ()
                    Target = pickTarget ()
                }
        elif kind < 70 then
            Op.Generate (
                pickCoreDumps (),
                {
                    Signal = pickGenerated ()
                    Target = pickTarget ()
                }
            )
        elif kind < 94 then
            // The leader is asked half the time: it is the only task that takes
            // the process's own signals.
            let task = if rng.Next 2 = 0 then referenceLeader else pick current

            Op.Deliver (pickCoreDumps (), task)
        else
            match taskPool |> List.filter (fun task -> not (Set.contains task tasks)) with
            | absent when not absent.IsEmpty && rng.Next 2 = 0 -> Op.Spawn (pick absent)
            | _ ->
                match current |> List.filter (fun task -> task <> referenceLeader) with
                | [] -> Op.Spawn (pick (taskPool |> List.filter (fun task -> not (Set.contains task tasks))))
                | others -> Op.Exit (pick others)

    let private checkAgainstOracle (numbering : SignalNumbering) : unit =
        let mutable observedHandlerDeliveries = 0
        let mutable observedNonLeaderDeliveries = 0
        let mutable observedDefaultTerminates = 0
        let mutable observedCoreDumps = 0
        let mutable observedDefaultStopsAndContinues = 0
        let mutable observedIgnoredDiscards = 0
        let mutable observedActionAfterSkip = 0
        let mutable observedDrainOfEmpty = 0
        let mutable observedDrainNoneNonEmpty = 0
        let mutable observedNonCanonicalSpellings = 0
        let mutable observedUnblockableBlocks = 0
        let mutable observedGenerationDrops = 0
        let mutable observedCoalescedEnqueues = 0
        let mutable observedQueuedRealTimeDuplicates = 0
        let mutable observedGeneratedTerminations = 0
        let mutable observedGeneratedStops = 0
        let mutable observedGeneratedQueued = 0
        let mutable observedGeneratedIgnoredDiscards = 0
        let mutable observedGenerationRefusals = 0
        let mutable observedDeliveryRefusals = 0
        let mutable observedDiscardsWhenSet = 0
        let mutable observedFlushes = 0
        let mutable observedDefaultsStored = 0
        let mutable observedOwnAndSharedCandidates = 0
        let mutable observedOutOfGenerationOrder = 0
        let mutable observedExits = 0

        let property (NonNegativeInt seed : NonNegativeInt) : unit =
            let rng = System.Random seed
            let steps = rng.Next (10, 80)

            let mutable s = initial numbering
            let mutable r = referenceEmpty
            let mutable tasks = Set.ofList [ t0 ; t1 ; t2 ]
            assertEquivalent numbering tasks s r

            for _ in 1..steps do
                let op = randomOp numbering tasks r rng

                // Distribution telemetry collected before the step so we can
                // see what shape the random walk drove the model into.
                match op with
                | Op.Deliver (coreDumps, task) ->
                    let candidates = referenceCandidates numbering task r

                    if
                        candidates |> List.exists (fun e -> e.Target.IsSome)
                        && candidates |> List.exists (fun e -> e.Target.IsNone)
                    then
                        observedOwnAndSharedCandidates <- observedOwnAndSharedCandidates + 1

                    if candidates <> (r.Pending |> List.filter (fun e -> List.contains e candidates)) then
                        observedOutOfGenerationOrder <- observedOutOfGenerationOrder + 1

                    match referenceNextDelivery numbering coreDumps tasks task r with
                    | Error _ -> observedDeliveryRefusals <- observedDeliveryRefusals + 1
                    | Ok (expected, r') ->

                    match expected with
                    | Some (SignalDelivery.RunHandler _) ->
                        observedHandlerDeliveries <- observedHandlerDeliveries + 1

                        if task <> referenceLeader then
                            observedNonLeaderDeliveries <- observedNonLeaderDeliveries + 1
                    | Some (SignalDelivery.DefaultTerminate (_, cored)) ->
                        observedDefaultTerminates <- observedDefaultTerminates + 1

                        if cored then
                            observedCoreDumps <- observedCoreDumps + 1
                    | Some (SignalDelivery.DefaultStop _)
                    | Some (SignalDelivery.DefaultContinue _) ->
                        observedDefaultStopsAndContinues <- observedDefaultStopsAndContinues + 1
                    | None when r.Pending.IsEmpty -> observedDrainOfEmpty <- observedDrainOfEmpty + 1
                    | None -> observedDrainNoneNonEmpty <- observedDrainNoneNonEmpty + 1

                    // Entries the walk removed beyond the one the action
                    // consumed are ignored-signal discards.
                    let consumed =
                        match expected with
                        | Some _ -> 1
                        | None -> 0

                    observedIgnoredDiscards <-
                        observedIgnoredDiscards
                        + (List.length r.Pending - List.length r'.Pending - consumed)

                    // An action fired past a candidate the task blocks.
                    match expected, candidates with
                    | Some _, head :: _ when List.contains head r'.Pending ->
                        observedActionAfterSkip <- observedActionAfterSkip + 1
                    | _ -> ()
                | Op.Generate (coreDumps, e) ->
                    let entry =
                        { e with
                            Signal = Signal.canonicalUnder numbering e.Signal
                        }

                    match referenceGenerate numbering coreDumps tasks entry r with
                    | ReferenceGeneration.Terminated (_, cored) ->
                        observedGeneratedTerminations <- observedGeneratedTerminations + 1

                        if cored then
                            observedCoreDumps <- observedCoreDumps + 1
                    | ReferenceGeneration.Stopped _ -> observedGeneratedStops <- observedGeneratedStops + 1
                    | ReferenceGeneration.Refused _ -> observedGenerationRefusals <- observedGenerationRefusals + 1
                    | ReferenceGeneration.Continues r' ->
                        observedGeneratedQueued <- observedGeneratedQueued + 1

                        if r' = r && referenceIgnoredAtGeneration numbering r entry.Signal then
                            observedGeneratedIgnoredDiscards <- observedGeneratedIgnoredDiscards + 1
                | Op.SetDisposition (signal, disposition) ->
                    if Signal.canonicalUnder numbering signal <> signal then
                        observedNonCanonicalSpellings <- observedNonCanonicalSpellings + 1

                    let canonicalSignal = Signal.canonicalUnder numbering signal

                    if disposition = SignalDisposition.Default then
                        observedDefaultsStored <- observedDefaultsStored + 1

                    if
                        referenceDiscardsWhenSet numbering disposition canonicalSignal
                        && r.Pending |> List.exists (fun p -> p.Signal = canonicalSignal)
                    then
                        observedDiscardsWhenSet <- observedDiscardsWhenSet + 1
                | Op.Block (_, signal)
                | Op.Unblock (_, signal)
                | Op.Enqueue {
                                 Signal = signal
                             } ->
                    if Signal.canonicalUnder numbering signal <> signal then
                        observedNonCanonicalSpellings <- observedNonCanonicalSpellings + 1
                | Op.Exit _ -> observedExits <- observedExits + 1
                | Op.Spawn _ -> ()

                match op with
                | Op.Block (_, signal) when Signal.isUnblockableUnder numbering signal ->
                    observedUnblockableBlocks <- observedUnblockableBlocks + 1
                | Op.Enqueue e
                | Op.Generate (_, e) ->
                    let canonicalSignal = Signal.canonicalUnder numbering e.Signal

                    match referenceBeginGeneration numbering canonicalSignal r with
                    | None -> observedGenerationDrops <- observedGenerationDrops + 1
                    | Some flushed ->
                        if List.length flushed.Pending < List.length r.Pending then
                            observedFlushes <- observedFlushes + 1

                        match op with
                        | Op.Enqueue _ ->
                            let alreadyPendingInSet =
                                flushed.Pending
                                |> List.exists (fun p -> p.Signal = canonicalSignal && p.Target = e.Target)

                            if alreadyPendingInSet then
                                if Signal.isRealTimeUnder numbering e.Signal then
                                    observedQueuedRealTimeDuplicates <- observedQueuedRealTimeDuplicates + 1
                                else
                                    observedCoalescedEnqueues <- observedCoalescedEnqueues + 1
                        | _ -> ()
                | _ -> ()

                let s', r', tasks' = stepBoth numbering tasks op s r
                s <- s'
                r <- r'
                tasks <- tasks'
                assertEquivalent numbering tasks s r

        Check.One (propertyConfig, property)


        // Distribution checks: the random walk must hit each of these
        // paths frequently enough that a regression would actually surface.
        // The thresholds are conservative — expected counts
        // are in the hundreds, so requiring a few dozen guards against
        // pathological non-coverage without becoming flaky on the lower
        // tail of the seed distribution.
        observedHandlerDeliveries |> shouldBeGreaterThan 30
        observedNonLeaderDeliveries |> shouldBeGreaterThan 10
        observedDefaultTerminates |> shouldBeGreaterThan 50
        observedCoreDumps |> shouldBeGreaterThan 20
        observedDefaultStopsAndContinues |> shouldBeGreaterThan 20
        observedActionAfterSkip |> shouldBeGreaterThan 20
        observedDrainOfEmpty |> shouldBeGreaterThan 20
        observedDrainNoneNonEmpty |> shouldBeGreaterThan 20
        observedNonCanonicalSpellings |> shouldBeGreaterThan 100
        observedUnblockableBlocks |> shouldBeGreaterThan 20
        observedCoalescedEnqueues |> shouldBeGreaterThan 20
        observedGeneratedTerminations |> shouldBeGreaterThan 20
        observedGeneratedStops |> shouldBeGreaterThan 5
        observedGeneratedQueued |> shouldBeGreaterThan 20
        observedGeneratedIgnoredDiscards |> shouldBeGreaterThan 20
        observedGenerationRefusals |> shouldBeGreaterThan 10
        observedDeliveryRefusals |> shouldBeGreaterThan 20
        observedDiscardsWhenSet |> shouldBeGreaterThan 20
        observedFlushes |> shouldBeGreaterThan 20
        observedDefaultsStored |> shouldBeGreaterThan 100
        observedOwnAndSharedCandidates |> shouldBeGreaterThan 20
        observedOutOfGenerationOrder |> shouldBeGreaterThan 50
        observedExits |> shouldBeGreaterThan 20

        // The generation-versus-delivery halves of the ignore rule are
        // flavour-divergent, so their counters are too: only Darwin drops at
        // generation, and only Linux lets an ignored signal reach the
        // delivery walk's discard. (A Darwin discard is still reachable by
        // catching, enqueueing and then ignoring, but the walk is not
        // guaranteed to line those up, so its floor stays at zero.)
        match numbering with
        | SignalNumbering.Linux ->
            observedIgnoredDiscards |> shouldBeGreaterThan 20
            observedGenerationDrops |> shouldEqual 0
        | SignalNumbering.Darwin -> observedGenerationDrops |> shouldBeGreaterThan 20

        // Only Linux numbering has real-time signals in the pool (`Other 40`),
        // so only there can the walk exercise the queue-not-coalesce arm.
        match numbering with
        | SignalNumbering.Linux -> observedQueuedRealTimeDuplicates |> shouldBeGreaterThan 5
        | SignalNumbering.Darwin -> observedQueuedRealTimeDuplicates |> shouldEqual 0

    [<Test>]
    let ``random op sequences agree with the reference oracle on every observable, under Linux numbering`` () : unit =
        checkAgainstOracle SignalNumbering.Linux

    [<Test>]
    let ``random op sequences agree with the reference oracle on every observable, under Darwin numbering`` () : unit =
        checkAgainstOracle SignalNumbering.Darwin
