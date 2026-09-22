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

    /// Stand-in for a client's signal-handler identity. `SignalState` requires
    /// only equality of it, and this is the evidence that it requires no more:
    /// PawPrint instantiates this parameter with a wrapped CLR `MethodInfo`,
    /// which this test cannot see and must not need to.
    type TestHandler = | TestHandler of string

    /// `initial` at this test's instantiation. Named because the
    /// generic `initial` constrains neither parameter, so every bare use would
    /// infer `obj` for the handler -- which `FS3559` rejects, correctly: an
    /// `obj` handler would make the equality constraint vacuous and the tests
    /// below would stop saying anything about it.
    let private initial (numbering : SignalNumbering) : SignalState<TestTask, TestHandler> =
        SignalState.initial numbering

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

    // ------------------------- Unit tests ------------------------- //

    [<Test>]
    let ``initial has nothing enabled, nothing blocked, nothing pending`` () : unit =
        for numbering in everyNumbering do
            let s = initial numbering
            SignalState.numbering s |> shouldEqual numbering
            SignalState.isInitialized s |> shouldEqual false
            SignalState.isEnabled Signal.SIGINT s |> shouldEqual false
            SignalState.isBlocked t0 Signal.SIGINT s |> shouldEqual false
            SignalState.blockedFor t0 s |> shouldEqual Set.empty
            SignalState.pending s |> Seq.toList |> shouldEqual []
            SignalState.enabled s |> shouldEqual Set.empty

    [<Test>]
    let ``markInitialized is idempotent and structurally stable`` () : unit =
        let dispatcher = TestTask 42
        let other = TestTask 99
        let once = empty |> SignalState.markInitialized dispatcher
        let twice = once |> SignalState.markInitialized other
        SignalState.isInitialized once |> shouldEqual true
        SignalState.signalThread once |> shouldEqual (Some dispatcher)
        // A second mark must not mutate the state's identity; downstream code
        // that compares states for equality (e.g. dedup hashing in the
        // debugger) relies on this. The dispatcher recorded on first init must
        // also survive — a re-init must not orphan the previously-allocated
        // signal thread by overwriting its id.
        twice |> shouldEqual once
        SignalState.signalThread twice |> shouldEqual (Some dispatcher)

    [<Test>]
    let ``enable flips a signal's bit on`` () : unit =
        let s = empty |> SignalState.enable Signal.SIGINT
        SignalState.isEnabled Signal.SIGINT s |> shouldEqual true
        SignalState.isEnabled Signal.SIGHUP s |> shouldEqual false
        SignalState.enabled s |> shouldEqual (Set.singleton Signal.SIGINT)

    [<Test>]
    let ``enable is idempotent`` () : unit =
        let once = empty |> SignalState.enable Signal.SIGINT
        let twice = once |> SignalState.enable Signal.SIGINT
        twice |> shouldEqual once

    [<Test>]
    let ``disable clears a previously enabled signal`` () : unit =
        let s =
            empty |> SignalState.enable Signal.SIGINT |> SignalState.disable Signal.SIGINT

        SignalState.isEnabled Signal.SIGINT s |> shouldEqual false

    [<Test>]
    let ``disable of an unenabled signal is a no-op`` () : unit =
        let s = empty |> SignalState.disable Signal.SIGINT
        s |> shouldEqual empty

    [<Test>]
    let ``enable then disable collapses to the initial state`` () : unit =
        // Mirrors the unblock/empty-mask collapse: an enable followed by a
        // matching disable must be structurally identical to never having
        // enabled. Without this, state dedup in the debugger would
        // distinguish two semantically-equivalent states.
        let enabledThenDisabled =
            empty |> SignalState.enable Signal.SIGINT |> SignalState.disable Signal.SIGINT

        enabledThenDisabled |> shouldEqual empty

    [<Test>]
    let ``disable leaves a pending entry queued, and its kernel default then applies`` () : unit =
        // Removing the disposition while a signal is pending doesn't drop
        // the signal — it remains queued, and delivery applies whatever the
        // disposition is at that moment: SIGINT's kernel default is to
        // terminate the process.
        let entry =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let s =
            empty
            |> SignalState.enable Signal.SIGINT
            |> SignalState.enqueue entry
            |> SignalState.disable Signal.SIGINT

        SignalState.pending s |> Seq.toList |> shouldEqual [ entry ]

        let delivery, s' = SignalState.nextDelivery (liveThreads [ t0 ]) s
        delivery |> shouldEqual (Some (SignalDelivery.DefaultTerminate Signal.SIGINT))
        SignalState.pending s' |> shouldEqual []

    [<Test>]
    let ``enable after enqueue makes a queued signal deliverable`` () : unit =
        let entry =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let s =
            empty
            |> SignalState.setHandler (TestHandler "h")
            |> SignalState.enqueue entry
            |> SignalState.enable Signal.SIGINT

        match SignalState.nextDelivery (liveThreads [ t0 ]) s with
        | Some (SignalDelivery.RunHandler (e, tid, h)), s' ->
            e |> shouldEqual entry
            tid |> shouldEqual t0
            h |> shouldEqual (TestHandler "h")
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
                    let enabled = initial numbering |> SignalState.enable spelt
                    SignalState.isEnabled signal enabled |> shouldEqual true
                    SignalState.enabled enabled |> shouldEqual (Set.singleton signal)
                    enabled |> SignalState.disable signal |> shouldEqual (initial numbering)

    [<Test>]
    let ``enqueue stores the canonical spelling`` () : unit =
        for numbering in everyNumbering do
            for signal in namedSignals do
                let spelt = Signal.Other (Signal.toRawSignoUnder numbering signal)

                // Enabled first (every named case is catchable), so the
                // ignore-default rows are not discarded at generation and
                // every row exercises the storage path.
                let start = initial numbering |> SignalState.enable signal

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

    [<Test>]
    let ``enable refuses a signal sigaction cannot install a handler for`` () : unit =
        for numbering in everyNumbering do
            for signal in unblockableSpellings numbering do
                // The unblockable spellings are exactly the uncatchable ones
                // today (SIGKILL, SIGSTOP, glibc's 32/33), so they double as
                // the enable-refusal cases.
                Signal.isUncatchableUnder numbering signal |> shouldEqual true

                Assert.Throws (fun () -> initial numbering |> SignalState.enable signal |> ignore<SignalState<_, _>>)
                |> ignore<exn>

    [<Test>]
    let ``disable of an uncatchable signal is the ordinary not-enabled no-op`` () : unit =
        for numbering in everyNumbering do
            for signal in unblockableSpellings numbering do
                initial numbering
                |> SignalState.disable signal
                |> shouldEqual (initial numbering)

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

                Assert.Throws (fun () -> SignalState.enable signal s |> ignore<SignalState<_, _>>)
                |> ignore<exn>

                Assert.Throws (fun () -> SignalState.disable signal s |> ignore<SignalState<_, _>>)
                |> ignore<exn>

                Assert.Throws (fun () -> SignalState.isEnabled signal s |> ignore<bool>)
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
        let s =
            s
            |> SignalState.enable (Signal.Other 36)
            |> SignalState.setHandler (TestHandler "h")

        let s =
            match SignalState.nextDelivery (liveThreads [ t0 ]) s with
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
            |> SignalState.setHandler (TestHandler "h")
            |> SignalState.enable Signal.SIGINT
            |> SignalState.block t0 Signal.SIGTERM
            |> SignalState.enqueue entryA
            |> SignalState.enqueue entryB

        let buildB () =
            empty
            |> SignalState.setHandler (TestHandler "h")
            |> SignalState.enable Signal.SIGINT
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
            match SignalState.nextDelivery (liveThreads [ t0 ; t1 ]) a with
            | Some (SignalDelivery.RunHandler _), s' -> s'
            | other, _ -> failwith $"expected a handler delivery from buildA, got %A{other}"

        let drainedFromB =
            match SignalState.nextDelivery (liveThreads [ t0 ; t1 ]) b with
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

            let delivery, s' = SignalState.nextDelivery (liveThreads [ t0 ]) s
            delivery |> shouldEqual (Some (SignalDelivery.DefaultTerminate signal))
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
            |> SignalState.nextDelivery (liveThreads [ t0 ])
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

        let delivery, s' = SignalState.nextDelivery (liveThreads [ t0 ]) s
        delivery |> shouldEqual (Some (SignalDelivery.DefaultContinue Signal.SIGCONT))
        SignalState.pending s' |> shouldEqual []

        let s =
            empty
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGCONT
                    Target = ValueNone
                }

        let delivery, s' = SignalState.nextDelivery (liveThreads []) s
        delivery |> shouldEqual (Some (SignalDelivery.DefaultContinue Signal.SIGCONT))
        SignalState.pending s' |> shouldEqual []

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

        let delivery, s' = SignalState.nextDelivery (liveThreads [ t0 ]) s
        delivery |> shouldEqual None
        s' |> shouldEqual s

    [<Test>]
    let ``an enabled pending signal with no handler installed stays queued`` () : unit =
        // The real shim ignores deliveries while g_posixSignalHandler is
        // NULL; entries wait for a handler rather than falling through to
        // the kernel default — the disposition is "handled", just not yet
        // claimable.
        let s =
            empty
            |> SignalState.enable Signal.SIGINT
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueNone
                }

        let delivery, s' = SignalState.nextDelivery (liveThreads [ t0 ]) s
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

        let delivery, s' = SignalState.nextDelivery (liveThreads [ t0 ]) s
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

        let delivery, afterScan = SignalState.nextDelivery (liveThreads [ t0 ]) held
        delivery |> shouldEqual None
        SignalState.pending afterScan |> shouldEqual [ entry ]

        // Handler claimed before the unblock: delivered.
        let claimed =
            afterScan
            |> SignalState.enable Signal.SIGCHLD
            |> SignalState.setHandler (TestHandler "h")
            |> SignalState.unblock t0 Signal.SIGCHLD

        match SignalState.nextDelivery (liveThreads [ t0 ]) claimed with
        | Some (SignalDelivery.RunHandler (e, tid, _)), s' ->
            e |> shouldEqual entry
            tid |> shouldEqual t0
            SignalState.pending s' |> shouldEqual []
        | other, _ -> failwith $"expected the held SIGCHLD to deliver, got %A{other}"

        // Still ignored at the unblock: discarded.
        let discarded = afterScan |> SignalState.unblock t0 Signal.SIGCHLD
        let delivery, s' = SignalState.nextDelivery (liveThreads [ t0 ]) discarded
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
            |> SignalState.enable Signal.SIGINT
            |> SignalState.setHandler (TestHandler "h")
            |> SignalState.enqueue ignored
            |> SignalState.enqueue handled

        match SignalState.nextDelivery (liveThreads [ t0 ]) s with
        | Some (SignalDelivery.RunHandler (e, _, _)), s' ->
            e |> shouldEqual handled
            SignalState.pending s' |> shouldEqual []
        | other, _ -> failwith $"expected the enabled entry to deliver past the discard, got %A{other}"

    [<Test>]
    let ``nextDelivery returns nothing for an empty queue`` () : unit =
        let delivery, s' = SignalState.nextDelivery (liveThreads [ t0 ]) empty
        delivery |> shouldEqual None
        s' |> shouldEqual empty

    [<Test>]
    let ``nextDelivery holds everything when there are no live threads`` () : unit =
        let s =
            empty
            |> SignalState.enable Signal.SIGINT
            |> SignalState.setHandler (TestHandler "h")
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueNone
                }

        let delivery, s' = SignalState.nextDelivery (liveThreads []) s
        delivery |> shouldEqual None
        s' |> shouldEqual s

    [<Test>]
    let ``nextDelivery picks the lowest live thread for a process-directed signal`` () : unit =
        let entry =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let s =
            empty
            |> SignalState.enable Signal.SIGINT
            |> SignalState.setHandler (TestHandler "h")
            |> SignalState.enqueue entry

        // Live-thread order is deliberately scrambled to confirm the
        // implementation sorts internally rather than trusting input order.
        match SignalState.nextDelivery (liveThreads [ t2 ; t0 ; t1 ]) s with
        | Some (SignalDelivery.RunHandler (e, tid, _)), s' ->
            e |> shouldEqual entry
            tid |> shouldEqual t0
            SignalState.pending s' |> Seq.toList |> shouldEqual []
        | other, _ -> failwith $"expected a handler delivery, got %A{other}"

    [<Test>]
    let ``nextDelivery skips the lowest thread if it is blocking the signal`` () : unit =
        let s =
            empty
            |> SignalState.enable Signal.SIGINT
            |> SignalState.setHandler (TestHandler "h")
            |> SignalState.block t0 Signal.SIGINT
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueNone
                }

        match SignalState.nextDelivery (liveThreads [ t0 ; t1 ; t2 ]) s with
        | Some (SignalDelivery.RunHandler (_, tid, _)), _ -> tid |> shouldEqual t1
        | other, _ -> failwith $"expected a handler delivery, got %A{other}"

    [<Test>]
    let ``nextDelivery holds a signal every live thread blocks`` () : unit =
        let s =
            empty
            |> SignalState.enable Signal.SIGINT
            |> SignalState.setHandler (TestHandler "h")
            |> SignalState.block t0 Signal.SIGINT
            |> SignalState.block t1 Signal.SIGINT
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueNone
                }

        let delivery, s' = SignalState.nextDelivery (liveThreads [ t0 ; t1 ]) s
        delivery |> shouldEqual None
        s' |> shouldEqual s

    [<Test>]
    let ``nextDelivery does not redirect a targeted signal to another thread`` () : unit =
        // pthread_kill is pinned: even though t1 is unblocked, a signal
        // targeted at t0 must stay queued, not get delivered to t1.
        let s =
            empty
            |> SignalState.enable Signal.SIGINT
            |> SignalState.setHandler (TestHandler "h")
            |> SignalState.block t0 Signal.SIGINT
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueSome t0
                }

        let delivery, s' = SignalState.nextDelivery (liveThreads [ t0 ; t1 ]) s
        delivery |> shouldEqual None
        s' |> shouldEqual s

    [<Test>]
    let ``nextDelivery holds a signal targeted at a dead thread`` () : unit =
        let s =
            empty
            |> SignalState.enable Signal.SIGINT
            |> SignalState.setHandler (TestHandler "h")
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueSome t2
                }

        let delivery, s' = SignalState.nextDelivery (liveThreads [ t0 ; t1 ]) s
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
            |> SignalState.enable Signal.SIGINT
            |> SignalState.setHandler (TestHandler "h")
            |> SignalState.block t0 Signal.SIGINT
            |> SignalState.enqueue head
            |> SignalState.enqueue middle
            |> SignalState.enqueue tail

        match SignalState.nextDelivery (liveThreads [ t0 ; t1 ]) s with
        | Some (SignalDelivery.RunHandler (e, tid, _)), s' ->
            e |> shouldEqual tail
            tid |> shouldEqual t1
            SignalState.pending s' |> Seq.toList |> shouldEqual [ head ; middle ]
        | other, _ -> failwith $"expected a handler delivery, got %A{other}"

    // ----------------------- Property tests ----------------------- //

    /// Operation language for the random property test. Each constructor
    /// maps to exactly one public method on the API.
    type private Op =
        | MarkInitialized
        | InstallHandler of handler : TestHandler
        | Enable of signal : Signal
        | Disable of signal : Signal
        | Block of thread : TestTask * signal : Signal
        | Unblock of thread : TestTask * signal : Signal
        | Enqueue of entry : PendingSignal<TestTask>
        | Deliver of live : TestTask list

    /// Reference implementation: simple lists / sets / maps, completely
    /// independent of the production module's internal representation. Every
    /// signal is stored canonically, via `Signal.canonicalUnder` — whose two
    /// columns `TestSignal` pins independently — so a production module that
    /// forgot to canonicalise diverges from it on the first `Other` spelling.
    type private ReferenceState =
        {
            Initialized : bool
            Handler : TestHandler option
            Enabled : Set<Signal>
            Blocked : Map<TestTask, Set<Signal>>
            Pending : PendingSignal<TestTask> list
        }

    let private referenceEmpty : ReferenceState =
        {
            Initialized = false
            Handler = None
            Enabled = Set.empty
            Blocked = Map.empty
            Pending = []
        }

    /// Index-based scan over an array with a removal mask: distinct algorithm
    /// from the production module's recursive accumulator walk, so a
    /// regression in either side surfaces as a divergence.
    let private referenceNextDelivery
        (numbering : SignalNumbering)
        (live : TestTask list)
        (r : ReferenceState)
        : SignalDelivery<TestTask, TestHandler> option * ReferenceState
        =
        let liveSet : Set<TestTask> = Set.ofList live

        let sortedLive : TestTask list =
            live |> List.sortBy (fun (TestTask.TestTask i) -> i)

        let isBlocked (tid : TestTask) (s : Signal) : bool =
            match Map.tryFind tid r.Blocked with
            | None -> false
            | Some set -> Set.contains s set

        let pickReceiver (e : PendingSignal<TestTask>) : TestTask option =
            match e.Target with
            | ValueSome tid ->
                if Set.contains tid liveSet && not (isBlocked tid e.Signal) then
                    Some tid
                else
                    None
            | ValueNone -> sortedLive |> List.tryFind (fun tid -> not (isBlocked tid e.Signal))

        let entries : PendingSignal<TestTask>[] = r.Pending |> List.toArray
        let removed : bool[] = Array.zeroCreate entries.Length
        let mutable result : SignalDelivery<TestTask, TestHandler> option = None
        let mutable i : int = 0

        while result.IsNone && i < entries.Length do
            let entry = entries.[i]

            if Set.contains entry.Signal r.Enabled then
                match pickReceiver entry, r.Handler with
                | Some receiver, Some handler ->
                    removed.[i] <- true
                    result <- Some (SignalDelivery.RunHandler (entry, receiver, handler))
                | _, _ -> ()
            else
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
                        result <- Some (SignalDelivery.DefaultTerminate entry.Signal)
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

    /// Fixed `TestTask` standing in for the signal-dispatcher thread in
    /// property runs. The property test does not model thread allocation,
    /// so any stable id will do — the oracle compares against `Initialized`
    /// (a `bool`) rather than the dispatcher id, and a second
    /// `MarkInitialized` op in a run must not change the recorded id
    /// (see `SignalState.markInitialized`'s idempotency contract). Using a
    /// constant guarantees both branches stay aligned across the random
    /// sequence.
    let private propertyDispatcher : TestTask = TestTask 0

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
        | Op.MarkInitialized ->
            SignalState.markInitialized propertyDispatcher s,
            { r with
                Initialized = true
            }
        | Op.InstallHandler handler ->
            SignalState.setHandler handler s,
            { r with
                Handler = Some handler
            }
        | Op.Enable sig0 ->
            SignalState.enable sig0 s,
            { r with
                Enabled = Set.add (canonical sig0) r.Enabled
            }
        | Op.Disable sig0 ->
            SignalState.disable sig0 s,
            { r with
                Enabled = Set.remove (canonical sig0) r.Enabled
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

            let ignoredNow =
                not (Set.contains entry.Signal r.Enabled)
                && Signal.defaultDispositionUnder numbering entry.Signal = DefaultDisposition.Ignore

            let alreadyPendingInSet =
                r.Pending
                |> List.exists (fun p -> p.Signal = entry.Signal && p.Target = entry.Target)

            let reference =
                if ignoredNow && not (Signal.blockedIgnoredSignalStaysPendingUnder numbering) then
                    r
                elif alreadyPendingInSet && not (Signal.isRealTimeUnder numbering entry.Signal) then
                    r
                else
                    { r with
                        Pending = r.Pending @ [ entry ]
                    }

            SignalState.enqueue e s, reference
        | Op.Deliver live ->
            let actualDelivery, s' = SignalState.nextDelivery (liveThreads live) s
            let expectedDelivery, r' = referenceNextDelivery numbering live r

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
        SignalState.isInitialized s |> shouldEqual r.Initialized
        SignalState.handler s |> shouldEqual r.Handler
        SignalState.enabled s |> shouldEqual r.Enabled
        SignalState.pending s |> Seq.toList |> shouldEqual r.Pending

        for sig0 in allSignals numbering do
            SignalState.isEnabled sig0 s
            |> shouldEqual (Set.contains (Signal.canonicalUnder numbering sig0) r.Enabled)

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

    let private randomOp (numbering : SignalNumbering) (rng : System.Random) : Op =
        let pick (xs : 'a list) : 'a = xs.[rng.Next xs.Length]
        let kind = rng.Next 100

        if kind < 5 then
            Op.MarkInitialized
        elif kind < 10 then
            // Two identities so last-writer-wins stays exercised.
            Op.InstallHandler (pick [ TestHandler "h1" ; TestHandler "h2" ])
        elif kind < 28 then
            // Only what sigaction would accept: `enable` fails loud on the
            // rest, and the refusal has its own unit test.
            Op.Enable (pick (enableableSignals numbering))
        elif kind < 35 then
            Op.Disable (pick (allSignals numbering))
        elif kind < 48 then
            Op.Block (pick allThreads, pick (allSignals numbering))
        elif kind < 57 then
            Op.Unblock (pick allThreads, pick (allSignals numbering))
        elif kind < 80 then
            let target =
                if rng.Next 2 = 0 then
                    ValueNone
                else
                    ValueSome (pick allThreads)

            // The real-time signal is over-weighted: observing
            // queue-not-coalesce needs the *same* signal generated twice
            // before a delivery consumes it, which a uniform pick over ~35
            // signals rarely produces now that default actions drain the
            // queue.
            let signal =
                match numbering with
                | SignalNumbering.Linux when rng.Next 6 = 0 -> Signal.Other 40
                | _ -> pick (allSignals numbering)

            Op.Enqueue
                {
                    Signal = signal
                    Target = target
                }
        else
            // Live-thread set varies independently of pending entries so
            // the dispatcher sees a moving target.
            let nThreads = rng.Next (allThreads.Length + 1)

            let threads = allThreads |> List.sortBy (fun _ -> rng.Next ()) |> List.take nThreads

            Op.Deliver threads

    let private checkAgainstOracle (numbering : SignalNumbering) : unit =
        let mutable observedHandlerDeliveries = 0
        let mutable observedDefaultTerminates = 0
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
                | Op.Deliver live ->
                    let expected, r' = referenceNextDelivery numbering live r

                    match expected with
                    | Some (SignalDelivery.RunHandler _) -> observedHandlerDeliveries <- observedHandlerDeliveries + 1
                    | Some (SignalDelivery.DefaultTerminate _) ->
                        observedDefaultTerminates <- observedDefaultTerminates + 1
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
                | Op.Enable signal
                | Op.Disable signal
                | Op.Block (_, signal)
                | Op.Unblock (_, signal)
                | Op.Enqueue {
                                 Signal = signal
                             } ->
                    if Signal.canonicalUnder numbering signal <> signal then
                        observedNonCanonicalSpellings <- observedNonCanonicalSpellings + 1
                | Op.MarkInitialized
                | Op.InstallHandler _ -> ()

                match op with
                | Op.Block (_, signal) when Signal.isUnblockableUnder numbering signal ->
                    observedUnblockableBlocks <- observedUnblockableBlocks + 1
                | Op.Enqueue e ->
                    let canonicalSignal = Signal.canonicalUnder numbering e.Signal

                    let ignoredNow =
                        not (Set.contains canonicalSignal r.Enabled)
                        && Signal.defaultDispositionUnder numbering canonicalSignal = DefaultDisposition.Ignore

                    if ignoredNow && not (Signal.blockedIgnoredSignalStaysPendingUnder numbering) then
                        observedGenerationDrops <- observedGenerationDrops + 1
                    else
                        let alreadyPendingInSet =
                            r.Pending
                            |> List.exists (fun p -> p.Signal = canonicalSignal && p.Target = e.Target)

                        if alreadyPendingInSet then
                            if Signal.isRealTimeUnder numbering e.Signal then
                                observedQueuedRealTimeDuplicates <- observedQueuedRealTimeDuplicates + 1
                            else
                                observedCoalescedEnqueues <- observedCoalescedEnqueues + 1
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
        observedDefaultStopsAndContinues |> shouldBeGreaterThan 20
        observedActionAfterSkip |> shouldBeGreaterThan 20
        observedDrainOfEmpty |> shouldBeGreaterThan 20
        observedDrainNoneNonEmpty |> shouldBeGreaterThan 20
        observedNonCanonicalSpellings |> shouldBeGreaterThan 100
        observedUnblockableBlocks |> shouldBeGreaterThan 20
        observedCoalescedEnqueues |> shouldBeGreaterThan 20

        // The generation-versus-delivery halves of the ignore rule are
        // flavour-divergent, so their counters are too: only Darwin drops at
        // generation, and only Linux lets an ignored signal reach the
        // delivery scan's discard. (A Darwin discard is still reachable by
        // enabling, enqueueing and then disabling, but the walk is not
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

    /// The handler slot, which nothing else in this file exercises.
    ///
    /// `SignalState` is generic in the handler's type and constrains it only to
    /// `equality`. That is a claim about the library, not about PawPrint: the
    /// one production instantiation wraps a CLR `MethodInfo`, so if the slot
    /// were secretly relying on anything of that type's, these tests -- which
    /// instantiate it with a string wrapper -- could not compile, let alone
    /// pass.
    [<Test>]
    let ``the handler slot needs only equality`` () =
        SignalState.handler empty |> shouldEqual None

        let installed = empty |> SignalState.setHandler (TestHandler "first")
        SignalState.handler installed |> shouldEqual (Some (TestHandler "first"))

        // Last writer wins, mirroring the native side's unconditional store
        // into `g_posixSignalHandler`.
        let replaced = installed |> SignalState.setHandler (TestHandler "second")
        SignalState.handler replaced |> shouldEqual (Some (TestHandler "second"))

        // Re-installing an equal handler is a no-op on the whole state, not
        // merely on the slot: this is what lets a caller re-register without
        // perturbing a state that is compared for equality to decide whether a
        // step changed anything.
        let reinstalled = replaced |> SignalState.setHandler (TestHandler "second")
        reinstalled |> shouldEqual replaced

    /// Installing a handler must not disturb anything else, which is the half
    /// of the previous test that a slot implemented as "replace the whole
    /// record" would still pass. Here the state is non-trivial first.
    [<Test>]
    let ``installing a handler preserves the rest of the state`` () =
        let before =
            empty
            |> SignalState.enable Signal.SIGINT
            |> SignalState.block t0 Signal.SIGTERM
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueNone
                }

        let after = before |> SignalState.setHandler (TestHandler "h")

        SignalState.enabled after |> shouldEqual (SignalState.enabled before)

        SignalState.blockedFor t0 after
        |> shouldEqual (SignalState.blockedFor t0 before)

        SignalState.pending after |> shouldEqual (SignalState.pending before)
