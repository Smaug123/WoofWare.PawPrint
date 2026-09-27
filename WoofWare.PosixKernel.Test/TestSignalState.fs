namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
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

    let private liveThreads (threads : TestTask list) : ImmutableArray<TestTask> = threads |> ImmutableArray.CreateRange

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

        let delivery, s' =
            SignalState.nextDelivery CoreDumps.Suppressed (liveThreads [ t0 ]) s

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

        match SignalState.nextDelivery CoreDumps.Suppressed (liveThreads [ t0 ]) s with
        | Some (SignalDelivery.RunHandler (e, tid, _)), s' ->
            e |> shouldEqual entry
            tid |> shouldEqual t0
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
    let ``enqueue appends to the back of the pending queue`` () : unit =
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

        SignalState.pending s |> Seq.toList |> shouldEqual [ a ; b ]

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
            match SignalState.nextDelivery CoreDumps.Suppressed (liveThreads [ t0 ]) s with
            | Some (SignalDelivery.RunHandler (e, _, _)), s' ->
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
            match SignalState.nextDelivery CoreDumps.Suppressed (liveThreads [ t0 ; t1 ]) a with
            | Some (SignalDelivery.RunHandler _), s' -> s'
            | other, _ -> failwith $"expected a handler delivery from buildA, got %A{other}"

        let drainedFromB =
            match SignalState.nextDelivery CoreDumps.Suppressed (liveThreads [ t0 ; t1 ]) b with
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

            let delivery, s' =
                SignalState.nextDelivery CoreDumps.Suppressed (liveThreads [ t0 ]) s

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
            |> SignalState.nextDelivery CoreDumps.Suppressed (liveThreads [ t0 ])
            |> fst

        deliveryFor Signal.SIGTSTP
        |> shouldEqual (Some (SignalDelivery.DefaultStop Signal.SIGTSTP))

        deliveryFor Signal.SIGCONT
        |> shouldEqual (Some (SignalDelivery.DefaultContinue Signal.SIGCONT))

    [<Test>]
    let ``a Continue default bypasses masks and receivers`` () : unit =
        // Resumption happens at generation on a real kernel, whatever any
        // mask says: measured on Linux 6.18.5 and Darwin 25.6.0 (two runs
        // each), a child that blocks SIGCONT and stops itself is resumed by
        // SIGCONT anyway — the mask defers only handler delivery. So the
        // event must surface even when every live thread blocks SIGCONT —
        // and even with no live threads at all, since resuming a process
        // needs no receiver thread.
        let s =
            empty
            |> SignalState.block t0 Signal.SIGCONT
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGCONT
                    Target = ValueNone
                }

        let delivery, s' =
            SignalState.nextDelivery CoreDumps.Suppressed (liveThreads [ t0 ]) s

        delivery |> shouldEqual (Some (SignalDelivery.DefaultContinue Signal.SIGCONT))
        SignalState.pending s' |> shouldEqual []

        let s =
            empty
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGCONT
                    Target = ValueNone
                }

        let delivery, s' = SignalState.nextDelivery CoreDumps.Suppressed (liveThreads []) s
        delivery |> shouldEqual (Some (SignalDelivery.DefaultContinue Signal.SIGCONT))
        SignalState.pending s' |> shouldEqual []

    [<Test>]
    let ``generating an unclaimed fatal signal terminates the process at once`` () : unit =
        let entry =
            {
                Signal = Signal.SIGTERM
                Target = ValueNone
            }

        let generation, s' =
            SignalState.generate CoreDumps.Suppressed (liveThreads [ t0 ]) entry empty

        generation
        |> shouldEqual (SignalGeneration.ProcessTerminated (Signal.SIGTERM, false))
        // Nothing is left pending for a delivery that the termination replaced.
        s' |> shouldEqual empty

        // SIGKILL cannot be blocked, so a mask naming it changes nothing.
        let masked = empty |> SignalState.block t0 (Signal.Other 9)

        SignalState.generate
            CoreDumps.Suppressed
            (liveThreads [ t0 ])
            {
                Signal = (Signal.Other 9)
                Target = ValueNone
            }
            masked
        |> fst
        |> shouldEqual (SignalGeneration.ProcessTerminated (Signal.Other 9, false))

    [<Test>]
    let ``generating an unclaimed stop signal stops the process at once`` () : unit =
        SignalState.generate
            CoreDumps.Suppressed
            (liveThreads [ t0 ])
            {
                Signal = (Signal.Other 19)
                Target = ValueNone
            }
            empty
        |> shouldEqual (SignalGeneration.ProcessStopped (Signal.Other 19), empty)

    [<Test>]
    let ``a fatal signal nobody can receive yet is queued, not fatal`` () : unit =
        let entry =
            {
                Signal = Signal.SIGTERM
                Target = ValueNone
            }

        // Every live thread blocks it...
        let blocked = empty |> SignalState.block t0 Signal.SIGTERM

        let generation, s' =
            SignalState.generate CoreDumps.Suppressed (liveThreads [ t0 ]) entry blocked

        generation |> shouldEqual SignalGeneration.ProcessContinues
        SignalState.pending s' |> shouldEqual [ entry ]

        // ...or there is no live thread at all.
        let generation, s' =
            SignalState.generate CoreDumps.Suppressed (liveThreads []) entry empty

        generation |> shouldEqual SignalGeneration.ProcessContinues
        SignalState.pending s' |> shouldEqual [ entry ]

        // A thread-directed one whose target blocks it waits even though
        // another live thread would not block it.
        let directed =
            { entry with
                Target = ValueSome t0
            }

        let generation, s' =
            SignalState.generate CoreDumps.Suppressed (liveThreads [ t0 ; t1 ]) directed blocked

        generation |> shouldEqual SignalGeneration.ProcessContinues
        SignalState.pending s' |> shouldEqual [ directed ]

    [<Test>]
    let ``a signal a handler claims is queued for it`` () : unit =
        let entry =
            {
                Signal = Signal.SIGTERM
                Target = ValueNone
            }

        let claimed = empty |> enable Signal.SIGTERM

        let generation, s' =
            SignalState.generate CoreDumps.Suppressed (liveThreads [ t0 ]) entry claimed

        generation |> shouldEqual SignalGeneration.ProcessContinues
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

            SignalState.generate CoreDumps.Suppressed (liveThreads [ t0 ]) entry s
            |> shouldEqual (SignalGeneration.ProcessContinues, s)

            // Nothing is left for a later handler to claim.
            let claimedLater =
                SignalState.generate CoreDumps.Suppressed (liveThreads [ t0 ]) entry s
                |> snd
                |> enable Signal.SIGCHLD

            SignalState.nextDelivery CoreDumps.Suppressed (liveThreads [ t0 ]) claimedLater
            |> fst
            |> shouldEqual None

    [<Test>]
    let ``an ignored signal every thread blocks is left pending at generation under Linux numbering`` () : unit =
        let entry =
            {
                Signal = Signal.SIGCHLD
                Target = ValueNone
            }

        let blocked = empty |> SignalState.block t0 Signal.SIGCHLD

        let generation, s' =
            SignalState.generate CoreDumps.Suppressed (liveThreads [ t0 ]) entry blocked

        generation |> shouldEqual SignalGeneration.ProcessContinues
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

        let delivery, s' =
            SignalState.nextDelivery CoreDumps.Suppressed (liveThreads [ t0 ]) s

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

        let delivery, s' =
            SignalState.nextDelivery CoreDumps.Suppressed (liveThreads [ t0 ]) s

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

        let delivery, afterScan =
            SignalState.nextDelivery CoreDumps.Suppressed (liveThreads [ t0 ]) held

        delivery |> shouldEqual None
        SignalState.pending afterScan |> shouldEqual [ entry ]

        // Handler claimed before the unblock: delivered.
        let claimed =
            afterScan |> enable Signal.SIGCHLD |> SignalState.unblock t0 Signal.SIGCHLD

        match SignalState.nextDelivery CoreDumps.Suppressed (liveThreads [ t0 ]) claimed with
        | Some (SignalDelivery.RunHandler (e, tid, _)), s' ->
            e |> shouldEqual entry
            tid |> shouldEqual t0
            SignalState.pending s' |> shouldEqual []
        | other, _ -> failwith $"expected the held SIGCHLD to deliver, got %A{other}"

        // Still ignored at the unblock: discarded.
        let discarded = afterScan |> SignalState.unblock t0 Signal.SIGCHLD

        let delivery, s' =
            SignalState.nextDelivery CoreDumps.Suppressed (liveThreads [ t0 ]) discarded

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
        // FIFO: an ignored entry at the head is dropped and the scan carries
        // on to deliver the enabled entry behind it, all in one call.
        let ignored =
            {
                Signal = Signal.SIGCHLD
                Target = ValueNone
            }

        let handled =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let s =
            empty
            |> enable Signal.SIGINT
            |> SignalState.enqueue ignored
            |> SignalState.enqueue handled

        match SignalState.nextDelivery CoreDumps.Suppressed (liveThreads [ t0 ]) s with
        | Some (SignalDelivery.RunHandler (e, _, _)), s' ->
            e |> shouldEqual handled
            SignalState.pending s' |> shouldEqual []
        | other, _ -> failwith $"expected the enabled entry to deliver past the discard, got %A{other}"

    [<Test>]
    let ``nextDelivery returns nothing for an empty queue`` () : unit =
        let delivery, s' =
            SignalState.nextDelivery CoreDumps.Suppressed (liveThreads [ t0 ]) empty

        delivery |> shouldEqual None
        s' |> shouldEqual empty

    [<Test>]
    let ``nextDelivery holds everything when there are no live threads`` () : unit =
        let s =
            empty
            |> enable Signal.SIGINT
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueNone
                }

        let delivery, s' = SignalState.nextDelivery CoreDumps.Suppressed (liveThreads []) s
        delivery |> shouldEqual None
        s' |> shouldEqual s

    [<Test>]
    let ``nextDelivery picks the lowest live thread for a process-directed signal`` () : unit =
        let entry =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let s = empty |> enable Signal.SIGINT |> SignalState.enqueue entry

        // Live-thread order is deliberately scrambled to confirm the
        // implementation sorts internally rather than trusting input order.
        match SignalState.nextDelivery CoreDumps.Suppressed (liveThreads [ t2 ; t0 ; t1 ]) s with
        | Some (SignalDelivery.RunHandler (e, tid, _)), s' ->
            e |> shouldEqual entry
            tid |> shouldEqual t0
            SignalState.pending s' |> Seq.toList |> shouldEqual []
        | other, _ -> failwith $"expected a handler delivery, got %A{other}"

    [<Test>]
    let ``nextDelivery skips the lowest thread if it is blocking the signal`` () : unit =
        let s =
            empty
            |> enable Signal.SIGINT
            |> SignalState.block t0 Signal.SIGINT
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueNone
                }

        match SignalState.nextDelivery CoreDumps.Suppressed (liveThreads [ t0 ; t1 ; t2 ]) s with
        | Some (SignalDelivery.RunHandler (_, tid, _)), _ -> tid |> shouldEqual t1
        | other, _ -> failwith $"expected a handler delivery, got %A{other}"

    [<Test>]
    let ``nextDelivery holds a signal every live thread blocks`` () : unit =
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

        let delivery, s' =
            SignalState.nextDelivery CoreDumps.Suppressed (liveThreads [ t0 ; t1 ]) s

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

        let delivery, s' =
            SignalState.nextDelivery CoreDumps.Suppressed (liveThreads [ t0 ; t1 ]) s

        delivery |> shouldEqual None
        s' |> shouldEqual s

    [<Test>]
    let ``nextDelivery holds a signal targeted at a dead thread`` () : unit =
        let s =
            empty
            |> enable Signal.SIGINT
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueSome t2
                }

        let delivery, s' =
            SignalState.nextDelivery CoreDumps.Suppressed (liveThreads [ t0 ; t1 ]) s

        delivery |> shouldEqual None
        s' |> shouldEqual s

    [<Test>]
    let ``nextDelivery preserves the FIFO order of skipped entries`` () : unit =
        // Three entries: (1) enabled but targeted at a thread blocking it,
        // (2) enabled and targeted at a dead thread, (3) process-directed
        // and deliverable to t1. The returned state must contain entries
        // (1) and (2) in their original order; only entry (3) is removed.
        let head =
            {
                Signal = Signal.SIGINT
                Target = ValueSome t0
            }

        let middle =
            {
                Signal = Signal.SIGINT
                Target = ValueSome t2
            }

        let tail =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let s =
            empty
            |> enable Signal.SIGINT
            |> SignalState.block t0 Signal.SIGINT
            |> SignalState.enqueue head
            |> SignalState.enqueue middle
            |> SignalState.enqueue tail

        match SignalState.nextDelivery CoreDumps.Suppressed (liveThreads [ t0 ; t1 ]) s with
        | Some (SignalDelivery.RunHandler (e, tid, _)), s' ->
            e |> shouldEqual tail
            tid |> shouldEqual t1
            SignalState.pending s' |> Seq.toList |> shouldEqual [ head ; middle ]
        | other, _ -> failwith $"expected a handler delivery, got %A{other}"

    // ----------------------- Property tests ----------------------- //

    /// Operation language for the random property test. Each constructor
    /// maps to exactly one public method on the API.
    type private Op =
        | SetDisposition of signal : Signal * disposition : SignalDisposition<TestHandler>
        | Block of thread : TestTask * signal : Signal
        | Unblock of thread : TestTask * signal : Signal
        | Enqueue of entry : PendingSignal<TestTask>
        | Generate of coreDumps : CoreDumps * live : TestTask list * entry : PendingSignal<TestTask>
        | Deliver of coreDumps : CoreDumps * live : TestTask list

    /// Reference implementation: simple lists / sets / maps, completely
    /// independent of the production module's internal representation. Every
    /// signal is stored canonically, via `Signal.canonicalUnder` — whose two
    /// columns `TestSignal` pins independently — so a production module that
    /// forgot to canonicalise diverges from it on the first `Other` spelling.
    ///
    /// It stores every disposition it is given, `Default` included: the
    /// production module must store none, and `assertEquivalent` checks that
    /// it answers every read the same way *and* holds no stored default.
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

    /// The thread that would take `e` now, if any: the target if it is live and
    /// not blocking, or for a process-directed signal the lowest-numbered live
    /// thread not blocking it.
    let private referenceReceiver
        (live : TestTask list)
        (r : ReferenceState)
        (e : PendingSignal<TestTask>)
        : TestTask option
        =
        let liveSet : Set<TestTask> = Set.ofList live

        let sortedLive : TestTask list =
            live |> List.sortBy (fun (TestTask.TestTask i) -> i)

        let isBlocked (tid : TestTask) (s : Signal) : bool =
            match Map.tryFind tid r.Blocked with
            | None -> false
            | Some set -> Set.contains s set

        match e.Target with
        | ValueSome tid ->
            if Set.contains tid liveSet && not (isBlocked tid e.Signal) then
                Some tid
            else
                None
        | ValueNone -> sortedLive |> List.tryFind (fun tid -> not (isBlocked tid e.Signal))

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

    /// What generating `entry` (canonical) does at once, and the state after.
    /// A default-disposition signal some thread could receive takes its
    /// default here: terminate, stop, or be discarded if the default ignores
    /// it; an ignored one some thread could receive is discarded.
    let private referenceGenerate
        (numbering : SignalNumbering)
        (coreDumps : CoreDumps)
        (live : TestTask list)
        (entry : PendingSignal<TestTask>)
        (r : ReferenceState)
        : SignalGeneration * ReferenceState
        =
        match referenceBeginGeneration numbering entry.Signal r with
        | None -> SignalGeneration.ProcessContinues, r
        | Some r ->
            let receivable = (referenceReceiver live r entry).IsSome

            match
                receivable, referenceDisposition r entry.Signal, Signal.defaultDispositionUnder numbering entry.Signal
            with
            | true, SignalDisposition.Ignore, _ -> SignalGeneration.ProcessContinues, r
            | true, SignalDisposition.Default, DefaultDisposition.Terminate ->
                SignalGeneration.ProcessTerminated (entry.Signal, referenceCore numbering coreDumps entry.Signal), r
            | true, SignalDisposition.Default, DefaultDisposition.Stop ->
                SignalGeneration.ProcessStopped entry.Signal, r
            | true, SignalDisposition.Default, DefaultDisposition.Ignore -> SignalGeneration.ProcessContinues, r
            | _, _, _ -> SignalGeneration.ProcessContinues, referenceAdmit numbering entry r

    /// Index-based scan over an array with a removal mask: distinct algorithm
    /// from the production module's recursive accumulator walk, so a
    /// regression in either side surfaces as a divergence.
    let private referenceNextDelivery
        (numbering : SignalNumbering)
        (coreDumps : CoreDumps)
        (live : TestTask list)
        (r : ReferenceState)
        : SignalDelivery<TestTask, TestHandler> option * ReferenceState
        =
        let pickReceiver (e : PendingSignal<TestTask>) : TestTask option = referenceReceiver live r e

        let entries : PendingSignal<TestTask>[] = r.Pending |> List.toArray
        let removed : bool[] = Array.zeroCreate entries.Length
        let mutable result : SignalDelivery<TestTask, TestHandler> option = None
        let mutable i : int = 0

        while result.IsNone && i < entries.Length do
            let entry = entries.[i]

            match referenceDisposition r entry.Signal with
            | SignalDisposition.Catch h ->
                match pickReceiver entry with
                | Some receiver ->
                    removed.[i] <- true
                    result <- Some (SignalDelivery.RunHandler (entry, receiver, h))
                | None -> ()
            | SignalDisposition.Ignore ->
                if (pickReceiver entry).IsSome then
                    removed.[i] <- true
            | SignalDisposition.Default ->
                match Signal.defaultDispositionUnder numbering entry.Signal with
                | DefaultDisposition.Continue ->
                    // Resumption bypasses masks and receivers entirely.
                    removed.[i] <- true
                    result <- Some (SignalDelivery.DefaultContinue entry.Signal)
                | DefaultDisposition.Ignore ->
                    if (pickReceiver entry).IsSome then
                        removed.[i] <- true
                | DefaultDisposition.Terminate ->
                    if (pickReceiver entry).IsSome then
                        removed.[i] <- true

                        result <-
                            Some (
                                SignalDelivery.DefaultTerminate (
                                    entry.Signal,
                                    referenceCore numbering coreDumps entry.Signal
                                )
                            )
                | DefaultDisposition.Stop ->
                    if (pickReceiver entry).IsSome then
                        removed.[i] <- true
                        result <- Some (SignalDelivery.DefaultStop entry.Signal)

            i <- i + 1

        let pending : PendingSignal<TestTask> list =
            [
                for j in 0 .. entries.Length - 1 do
                    if not removed.[j] then
                        yield entries.[j]
            ]

        result,
        { r with
            Pending = pending
        }

    /// Advance both implementations by one op, asserting agreement on
    /// `nextDelivery`'s full returned action (since the next step's
    /// observable state alone cannot always distinguish a divergence in
    /// which entry was consumed).
    let private stepBoth
        (numbering : SignalNumbering)
        (op : Op)
        (s : SignalState<TestTask, TestHandler>)
        (r : ReferenceState)
        : SignalState<TestTask, TestHandler> * ReferenceState
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
            }
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

            SignalState.block tid sig0 s, reference
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

            SignalState.unblock tid sig0 s, r'
        | Op.Enqueue e ->
            let entry =
                { e with
                    Signal = canonical e.Signal
                }

            SignalState.enqueue e s, referenceEnqueue numbering entry r
        | Op.Generate (coreDumps, live, e) ->
            let actual, s' = SignalState.generate coreDumps (liveThreads live) e s

            let expected, r' =
                referenceGenerate
                    numbering
                    coreDumps
                    live
                    { e with
                        Signal = canonical e.Signal
                    }
                    r

            if actual <> expected then
                failwith $"generate disagreed: actual=%A{actual}, reference=%A{expected}"

            s', r'
        | Op.Deliver (coreDumps, live) ->
            let actualDelivery, s' = SignalState.nextDelivery coreDumps (liveThreads live) s
            let expectedDelivery, r' = referenceNextDelivery numbering coreDumps live r

            if actualDelivery <> expectedDelivery then
                failwith $"nextDelivery disagreed: actual=%A{actualDelivery}, reference=%A{expectedDelivery}"

            s', r'

    /// Compare every observable accessor; the accessors are the contract.
    /// Queries run over every legal spelling, so a production module that
    /// canonicalised its stores but not its reads diverges here.
    let private assertEquivalent
        (numbering : SignalNumbering)
        (s : SignalState<TestTask, TestHandler>)
        (r : ReferenceState)
        : unit
        =
        // Equal as maps, so a stored `Default` on the production side is a
        // failure even though every read below would agree with it.
        SignalState.dispositions s
        |> shouldEqual (r.Dispositions |> Map.filter (fun _ d -> d <> SignalDisposition.Default))

        SignalState.pending s |> Seq.toList |> shouldEqual r.Pending

        for sig0 in allSignals numbering do
            SignalState.disposition sig0 s
            |> shouldEqual (referenceDisposition r (Signal.canonicalUnder numbering sig0))

        for tid in allThreads do
            for sig0 in allSignals numbering do
                let actualBlocked = SignalState.isBlocked tid sig0 s

                let expectedBlocked =
                    match Map.tryFind tid r.Blocked with
                    | None -> false
                    | Some set -> Set.contains (Signal.canonicalUnder numbering sig0) set

                if actualBlocked <> expectedBlocked then
                    failwith
                        $"isBlocked %O{tid} %O{sig0} disagreed: actual=%b{actualBlocked}, reference=%b{expectedBlocked}"

        for tid in allThreads do
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

    let private randomOp (numbering : SignalNumbering) (rng : System.Random) : Op =
        let pick (xs : 'a list) : 'a = xs.[rng.Next xs.Length]

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
            // for the flush between them.
            match rng.Next 8 with
            | 0 when numbering = SignalNumbering.Linux -> Signal.Other 40
            | 1
            | 2 -> pick (jobControl numbering)
            | _ -> pick (allSignals numbering)

        let pickTarget () : TestTask voption =
            if rng.Next 2 = 0 then
                ValueNone
            else
                ValueSome (pick allThreads)

        let kind = 10 + rng.Next 90

        if kind < 35 then
            // Only what the kernel accepts: `setDisposition` fails loud on
            // SIGKILL and SIGSTOP, and the refusal has its own unit test.
            Op.SetDisposition (pick (settableSignals numbering), pickDisposition ())
        elif kind < 48 then
            Op.Block (pick allThreads, pick (allSignals numbering))
        elif kind < 57 then
            Op.Unblock (pick allThreads, pick (allSignals numbering))
        elif kind < 72 then
            Op.Enqueue
                {
                    Signal = pickGenerated ()
                    Target = pickTarget ()
                }
        elif kind < 82 then
            let nThreads = rng.Next (allThreads.Length + 1)
            let live = allThreads |> List.sortBy (fun _ -> rng.Next ()) |> List.take nThreads

            Op.Generate (
                pickCoreDumps (),
                live,
                {
                    Signal = pickGenerated ()
                    Target = pickTarget ()
                }
            )
        else
            // Live-thread set varies independently of pending entries so
            // the dispatcher sees a moving target.
            let nThreads = rng.Next (allThreads.Length + 1)

            let threads = allThreads |> List.sortBy (fun _ -> rng.Next ()) |> List.take nThreads

            Op.Deliver (pickCoreDumps (), threads)

    let private checkAgainstOracle (numbering : SignalNumbering) : unit =
        let mutable observedHandlerDeliveries = 0
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
        let mutable observedDiscardsWhenSet = 0
        let mutable observedFlushes = 0
        let mutable observedDefaultsStored = 0

        let property (NonNegativeInt seed : NonNegativeInt) : unit =
            let rng = System.Random seed
            let steps = rng.Next (10, 80)

            let mutable s = initial numbering
            let mutable r = referenceEmpty
            assertEquivalent numbering s r

            for _ in 1..steps do
                let op = randomOp numbering rng

                // Distribution telemetry collected before the step so we can
                // see what shape the random walk drove the model into.
                match op with
                | Op.Deliver (coreDumps, live) ->
                    let expected, r' = referenceNextDelivery numbering coreDumps live r

                    match expected with
                    | Some (SignalDelivery.RunHandler _) -> observedHandlerDeliveries <- observedHandlerDeliveries + 1
                    | Some (SignalDelivery.DefaultTerminate (_, cored)) ->
                        observedDefaultTerminates <- observedDefaultTerminates + 1

                        if cored then
                            observedCoreDumps <- observedCoreDumps + 1
                    | Some (SignalDelivery.DefaultStop _)
                    | Some (SignalDelivery.DefaultContinue _) ->
                        observedDefaultStopsAndContinues <- observedDefaultStopsAndContinues + 1
                    | None when r.Pending.IsEmpty -> observedDrainOfEmpty <- observedDrainOfEmpty + 1
                    | None -> observedDrainNoneNonEmpty <- observedDrainNoneNonEmpty + 1

                    // Entries the scan removed beyond the one the action
                    // consumed are ignored-signal discards.
                    let consumed =
                        match expected with
                        | Some _ -> 1
                        | None -> 0

                    observedIgnoredDiscards <-
                        observedIgnoredDiscards
                        + (List.length r.Pending - List.length r'.Pending - consumed)

                    // An action fired past a held entry at the head of the
                    // queue: the FIFO-skip path.
                    match expected, r.Pending with
                    | Some _, head :: _ when List.contains head r'.Pending ->
                        observedActionAfterSkip <- observedActionAfterSkip + 1
                    | _ -> ()
                | Op.Generate (coreDumps, live, e) ->
                    let entry =
                        { e with
                            Signal = Signal.canonicalUnder numbering e.Signal
                        }

                    match referenceGenerate numbering coreDumps live entry r with
                    | SignalGeneration.ProcessTerminated (_, cored), _ ->
                        observedGeneratedTerminations <- observedGeneratedTerminations + 1

                        if cored then
                            observedCoreDumps <- observedCoreDumps + 1
                    | SignalGeneration.ProcessStopped _, _ -> observedGeneratedStops <- observedGeneratedStops + 1
                    | SignalGeneration.ProcessContinues, r' ->
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

                match op with
                | Op.Block (_, signal) when Signal.isUnblockableUnder numbering signal ->
                    observedUnblockableBlocks <- observedUnblockableBlocks + 1
                | Op.Enqueue e
                | Op.Generate (_, _, e) ->
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

                let s', r' = stepBoth numbering op s r
                s <- s'
                r <- r'
                assertEquivalent numbering s r

        Check.One (propertyConfig, property)

        // Distribution checks: the random walk must hit each of these
        // paths frequently enough that a regression would actually surface.
        // The thresholds are conservative — expected counts
        // are in the hundreds, so requiring a few dozen guards against
        // pathological non-coverage without becoming flaky on the lower
        // tail of the seed distribution.
        observedHandlerDeliveries |> shouldBeGreaterThan 30
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
        observedDiscardsWhenSet |> shouldBeGreaterThan 20
        observedFlushes |> shouldBeGreaterThan 20
        observedDefaultsStored |> shouldBeGreaterThan 100

        // The generation-versus-delivery halves of the ignore rule are
        // flavour-divergent, so their counters are too: only Darwin drops at
        // generation, and only Linux lets an ignored signal reach the
        // delivery scan's discard. (A Darwin discard is still reachable by
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
