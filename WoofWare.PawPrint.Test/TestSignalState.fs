namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Property-based and unit tests for the deterministic `SignalState` data
/// model. `SignalState` has no full dispatcher in the simulator yet — these
/// tests pin down its behaviour in isolation so the downstream slices
/// (dispatch, harness injection, native handler arms) can build on a
/// known-good shape.
///
/// The property test runs a random sequence of operations through both the
/// production module and a structurally-different reference oracle, then
/// asserts agreement on every observable accessor after each step. The
/// oracle uses index-based scanning over an array; the production module
/// uses a recursive accumulator-threaded walk. A regression in either side
/// surfaces as a divergence the property catches.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSignalState =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private t0 : ThreadId = ThreadId 0
    let private t1 : ThreadId = ThreadId 1
    let private t2 : ThreadId = ThreadId 2

    let private allThreads : ThreadId list = [ t0 ; t1 ; t2 ]

    let private allSignals : Signal list =
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
            Signal.Other 99
        ]

    let private liveThreads (threads : ThreadId list) : ImmutableArray<ThreadId> = threads |> ImmutableArray.CreateRange

    // ------------------------- Unit tests ------------------------- //

    [<Test>]
    let ``empty has nothing enabled, nothing blocked, nothing pending`` () : unit =
        let s = SignalState.empty
        SignalState.isInitialized s |> shouldEqual false
        SignalState.isEnabled Signal.SIGINT s |> shouldEqual false
        SignalState.isBlocked t0 Signal.SIGINT s |> shouldEqual false
        SignalState.blockedFor t0 s |> shouldEqual Set.empty
        SignalState.pending s |> Seq.toList |> shouldEqual []
        SignalState.enabled s |> shouldEqual Set.empty

    [<Test>]
    let ``markInitialized is idempotent and structurally stable`` () : unit =
        let once = SignalState.empty |> SignalState.markInitialized
        let twice = once |> SignalState.markInitialized
        SignalState.isInitialized once |> shouldEqual true
        // A second mark must not mutate the state's identity; downstream code
        // that compares states for equality (e.g. dedup hashing in the
        // debugger) relies on this.
        twice |> shouldEqual once

    [<Test>]
    let ``enable flips a signal's bit on`` () : unit =
        let s = SignalState.empty |> SignalState.enable Signal.SIGINT
        SignalState.isEnabled Signal.SIGINT s |> shouldEqual true
        SignalState.isEnabled Signal.SIGHUP s |> shouldEqual false
        SignalState.enabled s |> shouldEqual (Set.singleton Signal.SIGINT)

    [<Test>]
    let ``enable is idempotent`` () : unit =
        let once = SignalState.empty |> SignalState.enable Signal.SIGINT
        let twice = once |> SignalState.enable Signal.SIGINT
        twice |> shouldEqual once

    [<Test>]
    let ``disable clears a previously enabled signal`` () : unit =
        let s =
            SignalState.empty
            |> SignalState.enable Signal.SIGINT
            |> SignalState.disable Signal.SIGINT

        SignalState.isEnabled Signal.SIGINT s |> shouldEqual false

    [<Test>]
    let ``disable of an unenabled signal is a no-op`` () : unit =
        let s = SignalState.empty |> SignalState.disable Signal.SIGINT
        s |> shouldEqual SignalState.empty

    [<Test>]
    let ``enable then disable collapses to the empty state`` () : unit =
        // Mirrors the unblock/empty-mask collapse: an enable followed by a
        // matching disable must be structurally identical to never having
        // enabled. Without this, state dedup in the debugger would
        // distinguish two semantically-equivalent states.
        let enabledThenDisabled =
            SignalState.empty
            |> SignalState.enable Signal.SIGINT
            |> SignalState.disable Signal.SIGINT

        enabledThenDisabled |> shouldEqual SignalState.empty

    [<Test>]
    let ``disable leaves pending entries queued, just non-deliverable`` () : unit =
        // POSIX semantics: removing the disposition while a signal is
        // pending doesn't drop the signal — it remains queued and becomes
        // deliverable again if the signal is re-enabled.
        let entry =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let s =
            SignalState.empty
            |> SignalState.enable Signal.SIGINT
            |> SignalState.enqueue entry
            |> SignalState.disable Signal.SIGINT

        SignalState.pending s |> Seq.toList |> shouldEqual [ entry ]
        SignalState.tryDeliverable (liveThreads [ t0 ]) s |> shouldEqual None

    [<Test>]
    let ``enable after enqueue makes a queued signal deliverable`` () : unit =
        let entry =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let s =
            SignalState.empty
            |> SignalState.enqueue entry
            |> SignalState.enable Signal.SIGINT

        match SignalState.tryDeliverable (liveThreads [ t0 ]) s with
        | Some (e, tid, s') ->
            e |> shouldEqual entry
            tid |> shouldEqual t0
            SignalState.pending s' |> Seq.toList |> shouldEqual []
        | None -> failwith "expected deliverable entry once signal was enabled"

    [<Test>]
    let ``block then isBlocked`` () : unit =
        let s = SignalState.empty |> SignalState.block t0 Signal.SIGINT
        SignalState.isBlocked t0 Signal.SIGINT s |> shouldEqual true
        SignalState.isBlocked t0 Signal.SIGHUP s |> shouldEqual false
        SignalState.isBlocked t1 Signal.SIGINT s |> shouldEqual false

    [<Test>]
    let ``block is idempotent`` () : unit =
        let once = SignalState.empty |> SignalState.block t0 Signal.SIGINT
        let twice = once |> SignalState.block t0 Signal.SIGINT
        twice |> shouldEqual once

    [<Test>]
    let ``unblock removes a blocked signal`` () : unit =
        let s =
            SignalState.empty
            |> SignalState.block t0 Signal.SIGINT
            |> SignalState.unblock t0 Signal.SIGINT

        SignalState.isBlocked t0 Signal.SIGINT s |> shouldEqual false
        SignalState.blockedFor t0 s |> shouldEqual Set.empty

    [<Test>]
    let ``unblock collapses empty mask back to the empty state`` () : unit =
        // Critical: a state that had a signal blocked and then unblocked must
        // be structurally identical to a state that never blocked it. Without
        // collapsing the empty mask, equality would distinguish two
        // semantically-equivalent states and the property-test oracle would
        // diverge from the implementation after every full unblock.
        let blockedThenUnblocked =
            SignalState.empty
            |> SignalState.block t0 Signal.SIGINT
            |> SignalState.unblock t0 Signal.SIGINT

        blockedThenUnblocked |> shouldEqual SignalState.empty

    [<Test>]
    let ``unblock of an unblocked signal is a no-op`` () : unit =
        let s = SignalState.empty |> SignalState.unblock t0 Signal.SIGINT
        s |> shouldEqual SignalState.empty

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

        let s = SignalState.empty |> SignalState.enqueue a |> SignalState.enqueue b

        SignalState.pending s |> Seq.toList |> shouldEqual [ a ; b ]

    [<Test>]
    let ``enqueue does not coalesce duplicates`` () : unit =
        let e =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let s = SignalState.empty |> SignalState.enqueue e |> SignalState.enqueue e

        SignalState.pending s |> Seq.toList |> shouldEqual [ e ; e ]

    [<Test>]
    let ``structural equality survives a non-empty pending queue`` () : unit =
        // Regression guard for a previous representation that stored
        // `Pending` as `ImmutableQueue<T>`: that container uses reference
        // equality, so two independently-built states with identical
        // contents would compare unequal once the queue was non-empty.
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
            SignalState.empty
            |> SignalState.enable Signal.SIGINT
            |> SignalState.block t0 Signal.SIGTERM
            |> SignalState.enqueue entryA
            |> SignalState.enqueue entryB

        let buildB () =
            SignalState.empty
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
        // tryDeliverable rebuilds the pending list from a skipped/tail
        // split.
        let drainedFromA =
            match SignalState.tryDeliverable (liveThreads [ t0 ; t1 ]) a with
            | Some (_, _, s') -> s'
            | None -> failwith "expected deliverable entry from buildA"

        let drainedFromB =
            match SignalState.tryDeliverable (liveThreads [ t0 ; t1 ]) b with
            | Some (_, _, s') -> s'
            | None -> failwith "expected deliverable entry from buildB"

        drainedFromA |> shouldEqual drainedFromB
        hash drainedFromA |> shouldEqual (hash drainedFromB)

    [<Test>]
    let ``tryDeliverable returns None when nothing is pending`` () : unit =
        SignalState.tryDeliverable (liveThreads [ t0 ]) SignalState.empty
        |> shouldEqual None

    [<Test>]
    let ``tryDeliverable returns None when the signal is not enabled`` () : unit =
        let s =
            SignalState.empty
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueNone
                }

        SignalState.tryDeliverable (liveThreads [ t0 ]) s |> shouldEqual None

    [<Test>]
    let ``tryDeliverable returns None when there are no live threads`` () : unit =
        let s =
            SignalState.empty
            |> SignalState.enable Signal.SIGINT
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueNone
                }

        SignalState.tryDeliverable (liveThreads []) s |> shouldEqual None

    [<Test>]
    let ``tryDeliverable picks the lowest live thread for a process-directed signal`` () : unit =
        let entry =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let s =
            SignalState.empty
            |> SignalState.enable Signal.SIGINT
            |> SignalState.enqueue entry

        // Live-thread order is deliberately scrambled to confirm the
        // implementation sorts internally rather than trusting input order.
        match SignalState.tryDeliverable (liveThreads [ t2 ; t0 ; t1 ]) s with
        | Some (e, tid, s') ->
            e |> shouldEqual entry
            tid |> shouldEqual t0
            SignalState.pending s' |> Seq.toList |> shouldEqual []
        | None -> failwith "expected deliverable entry"

    [<Test>]
    let ``tryDeliverable skips the lowest thread if it is blocking the signal`` () : unit =
        let s =
            SignalState.empty
            |> SignalState.enable Signal.SIGINT
            |> SignalState.block t0 Signal.SIGINT
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueNone
                }

        match SignalState.tryDeliverable (liveThreads [ t0 ; t1 ; t2 ]) s with
        | Some (_, tid, _) -> tid |> shouldEqual t1
        | None -> failwith "expected deliverable entry"

    [<Test>]
    let ``tryDeliverable returns None when every live thread blocks the signal`` () : unit =
        let s =
            SignalState.empty
            |> SignalState.enable Signal.SIGINT
            |> SignalState.block t0 Signal.SIGINT
            |> SignalState.block t1 Signal.SIGINT
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueNone
                }

        SignalState.tryDeliverable (liveThreads [ t0 ; t1 ]) s |> shouldEqual None

    [<Test>]
    let ``tryDeliverable for a targeted signal does not redirect to another thread`` () : unit =
        // pthread_kill is pinned: even though t1 is unblocked, a signal
        // targeted at t0 must stay queued, not get delivered to t1.
        let s =
            SignalState.empty
            |> SignalState.enable Signal.SIGINT
            |> SignalState.block t0 Signal.SIGINT
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueSome t0
                }

        SignalState.tryDeliverable (liveThreads [ t0 ; t1 ]) s |> shouldEqual None

    [<Test>]
    let ``tryDeliverable for a targeted dead thread is held`` () : unit =
        let s =
            SignalState.empty
            |> SignalState.enable Signal.SIGINT
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueSome t2
                }

        SignalState.tryDeliverable (liveThreads [ t0 ; t1 ]) s |> shouldEqual None

    [<Test>]
    let ``tryDeliverable preserves the FIFO order of skipped entries`` () : unit =
        // Three entries: (1) signal not enabled, (2) targeted at a thread
        // blocking it, (3) process-directed and deliverable to t1. The
        // returned state must contain entries (1) and (2) in their original
        // order; only entry (3) is removed.
        let head =
            {
                Signal = Signal.SIGHUP
                Target = ValueNone
            }

        let middle =
            {
                Signal = Signal.SIGINT
                Target = ValueSome t0
            }

        let tail =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let s =
            SignalState.empty
            |> SignalState.enable Signal.SIGINT
            |> SignalState.block t0 Signal.SIGINT
            |> SignalState.enqueue head
            |> SignalState.enqueue middle
            |> SignalState.enqueue tail

        match SignalState.tryDeliverable (liveThreads [ t0 ; t1 ]) s with
        | Some (e, tid, s') ->
            e |> shouldEqual tail
            tid |> shouldEqual t1
            SignalState.pending s' |> Seq.toList |> shouldEqual [ head ; middle ]
        | None -> failwith "expected deliverable entry"

    // ----------------------- Property tests ----------------------- //

    /// Operation language for the random property test. Each constructor
    /// maps to exactly one public method on the API.
    type private Op =
        | MarkInitialized
        | Enable of signal : Signal
        | Disable of signal : Signal
        | Block of thread : ThreadId * signal : Signal
        | Unblock of thread : ThreadId * signal : Signal
        | Enqueue of entry : PendingSignal
        | DrainOne of live : ThreadId list

    /// Reference implementation: simple lists / sets / maps, completely
    /// independent of the production module's internal representation.
    type private ReferenceState =
        {
            Initialized : bool
            Enabled : Set<Signal>
            Blocked : Map<ThreadId, Set<Signal>>
            Pending : PendingSignal list
        }

    let private referenceEmpty : ReferenceState =
        {
            Initialized = false
            Enabled = Set.empty
            Blocked = Map.empty
            Pending = []
        }

    /// Index-based two-pass scan: distinct algorithm from the production
    /// module's recursive accumulator walk, so a regression in either side
    /// surfaces as a divergence.
    let private referenceTryDeliverable
        (live : ThreadId list)
        (r : ReferenceState)
        : (PendingSignal * ThreadId * ReferenceState) option
        =
        let liveSet : Set<ThreadId> = Set.ofList live

        let sortedLive : ThreadId list =
            live |> List.sortBy (fun (ThreadId.ThreadId i) -> i)

        let isBlocked (tid : ThreadId) (s : Signal) : bool =
            match Map.tryFind tid r.Blocked with
            | None -> false
            | Some set -> Set.contains s set

        let pickReceiver (e : PendingSignal) : ThreadId option =
            match e.Target with
            | ValueSome tid ->
                if Set.contains tid liveSet && not (isBlocked tid e.Signal) then
                    Some tid
                else
                    None
            | ValueNone -> sortedLive |> List.tryFind (fun tid -> not (isBlocked tid e.Signal))

        let entries : PendingSignal[] = r.Pending |> List.toArray
        let mutable foundIdx : int = -1
        let mutable foundReceiver : ThreadId option = None
        let mutable i : int = 0

        while foundIdx < 0 && i < entries.Length do
            let entry = entries.[i]

            if Set.contains entry.Signal r.Enabled then
                match pickReceiver entry with
                | Some tid ->
                    foundIdx <- i
                    foundReceiver <- Some tid
                | None -> ()

            i <- i + 1

        if foundIdx < 0 then
            None
        else
            let remaining : PendingSignal list =
                Array.append
                    (Array.sub entries 0 foundIdx)
                    (Array.sub entries (foundIdx + 1) (entries.Length - foundIdx - 1))
                |> Array.toList

            Some (
                entries.[foundIdx],
                foundReceiver.Value,
                { r with
                    Pending = remaining
                }
            )

    /// Advance both implementations by one op, asserting agreement on
    /// `tryDeliverable`'s full return tuple (since the next step's
    /// observable state alone cannot always distinguish a divergence in
    /// which entry was dequeued).
    let private stepBoth (op : Op) (s : SignalState) (r : ReferenceState) : SignalState * ReferenceState =
        match op with
        | Op.MarkInitialized ->
            SignalState.markInitialized s,
            { r with
                Initialized = true
            }
        | Op.Enable sig0 ->
            SignalState.enable sig0 s,
            { r with
                Enabled = Set.add sig0 r.Enabled
            }
        | Op.Disable sig0 ->
            SignalState.disable sig0 s,
            { r with
                Enabled = Set.remove sig0 r.Enabled
            }
        | Op.Block (tid, sig0) ->
            let existing : Set<Signal> =
                match Map.tryFind tid r.Blocked with
                | None -> Set.empty
                | Some set -> set

            SignalState.block tid sig0 s,
            { r with
                Blocked = Map.add tid (Set.add sig0 existing) r.Blocked
            }
        | Op.Unblock (tid, sig0) ->
            let r' : ReferenceState =
                match Map.tryFind tid r.Blocked with
                | None -> r
                | Some set ->
                    if not (Set.contains sig0 set) then
                        r
                    else
                        let set' = Set.remove sig0 set

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
            SignalState.enqueue e s,
            { r with
                Pending = r.Pending @ [ e ]
            }
        | Op.DrainOne live ->
            let actual = SignalState.tryDeliverable (liveThreads live) s
            let expected = referenceTryDeliverable live r

            match actual, expected with
            | None, None -> s, r
            | Some (e1, tid1, s'), Some (e2, tid2, r') ->
                e1 |> shouldEqual e2
                tid1 |> shouldEqual tid2
                s', r'
            | a, b -> failwith $"tryDeliverable disagreed: actual=%A{a}, reference=%A{b}"

    /// Compare every observable accessor; the accessors are the contract.
    let private assertEquivalent (s : SignalState) (r : ReferenceState) : unit =
        SignalState.isInitialized s |> shouldEqual r.Initialized
        SignalState.enabled s |> shouldEqual r.Enabled
        SignalState.pending s |> Seq.toList |> shouldEqual r.Pending

        for sig0 in allSignals do
            SignalState.isEnabled sig0 s |> shouldEqual (Set.contains sig0 r.Enabled)

        for tid in allThreads do
            for sig0 in allSignals do
                let actualBlocked = SignalState.isBlocked tid sig0 s

                let expectedBlocked =
                    match Map.tryFind tid r.Blocked with
                    | None -> false
                    | Some set -> Set.contains sig0 set

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

    let private randomOp (rng : System.Random) : Op =
        let pick (xs : 'a list) : 'a = xs.[rng.Next xs.Length]
        let kind = rng.Next 100

        if kind < 5 then
            Op.MarkInitialized
        elif kind < 25 then
            Op.Enable (pick allSignals)
        elif kind < 32 then
            Op.Disable (pick allSignals)
        elif kind < 47 then
            Op.Block (pick allThreads, pick allSignals)
        elif kind < 57 then
            Op.Unblock (pick allThreads, pick allSignals)
        elif kind < 80 then
            let target =
                if rng.Next 2 = 0 then
                    ValueNone
                else
                    ValueSome (pick allThreads)

            Op.Enqueue
                {
                    Signal = pick allSignals
                    Target = target
                }
        else
            // Live-thread set varies independently of pending entries so
            // the dispatcher sees a moving target.
            let nThreads = rng.Next (allThreads.Length + 1)

            let threads = allThreads |> List.sortBy (fun _ -> rng.Next ()) |> List.take nThreads

            Op.DrainOne threads

    [<Test>]
    let ``random op sequences agree with the reference oracle on every observable`` () : unit =
        let mutable observedDeliveries = 0
        let mutable observedSkipThenDeliver = 0
        let mutable observedDrainOfEmpty = 0
        let mutable observedDrainNoneNonEmpty = 0

        let property (NonNegativeInt seed : NonNegativeInt) : unit =
            let rng = System.Random seed
            let steps = rng.Next (10, 80)

            let mutable s = SignalState.empty
            let mutable r = referenceEmpty
            assertEquivalent s r

            for _ in 1..steps do
                let op = randomOp rng

                // Distribution telemetry collected before the step so we can
                // see what shape the random walk drove the model into.
                match op with
                | Op.DrainOne live ->
                    match referenceTryDeliverable live r with
                    | Some (entry, _, _) ->
                        observedDeliveries <- observedDeliveries + 1

                        match r.Pending with
                        | head :: _ when head <> entry -> observedSkipThenDeliver <- observedSkipThenDeliver + 1
                        | _ -> ()
                    | None when r.Pending.IsEmpty -> observedDrainOfEmpty <- observedDrainOfEmpty + 1
                    | None -> observedDrainNoneNonEmpty <- observedDrainNoneNonEmpty + 1
                | _ -> ()

                let s', r' = stepBoth op s r
                s <- s'
                r <- r'
                assertEquivalent s r

        Check.One (propertyConfig, property)

        // Distribution checks: the random walk must hit each load-bearing
        // path frequently enough that a regression would actually surface.
        // The thresholds are deliberately conservative — expected counts
        // are in the hundreds, so requiring a few dozen guards against
        // pathological non-coverage without becoming flaky on the lower
        // tail of the seed distribution.
        observedDeliveries |> shouldBeGreaterThan 100
        observedSkipThenDeliver |> shouldBeGreaterThan 20
        observedDrainOfEmpty |> shouldBeGreaterThan 20
        observedDrainNoneNonEmpty |> shouldBeGreaterThan 20
