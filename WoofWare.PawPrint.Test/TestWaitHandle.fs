namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Property-based tests for the deterministic `WaitHandle` state machine
/// that backs the `CreateSemaphoreExW` / `ReleaseSemaphore` / `CloseHandle`
/// / `WaitHandle_WaitOneCore` QCalls. Mirrors `TestLowLevelMonitor.fs`:
/// the state machine is exercised in isolation through a stub
/// `IlMachineState`, since the wait-handle module only reads and writes
/// `Status`, the `WaitHandles` registry, and `NextWaitHandleId`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestWaitHandle =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 200

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private baseState () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()
        IlMachineState.initial loggerFactory ImmutableArray.Empty corelib

    /// Frame-less thread stub; wait-handle transitions only read/write
    /// `Status`, so any code path that tried to dereference a frame would
    /// crash on the sentinel `ActiveMethodState`, which is the correct
    /// response if the wait-handle module ever started reaching for frames.
    let private stubThreadState (status : ThreadStatus) : ThreadState =
        {
            MethodStates = Map.empty
            NextFrameId = 0
            ActiveMethodState = FrameId -1
            Status = status
        }

    let private withThreads (threads : ThreadId list) (state : IlMachineState) : IlMachineState =
        let threadMap =
            threads
            |> List.map (fun tid -> tid, stubThreadState ThreadStatus.Runnable)
            |> Map.ofList

        { state with
            ThreadState = threadMap
        }

    let private statusOf (thread : ThreadId) (state : IlMachineState) : ThreadStatus = state.ThreadState.[thread].Status

    let private semaphoreOf (id : WaitHandleId) (state : IlMachineState) : SemaphoreState =
        match Map.find id state.Kernel.WaitHandles with
        | WaitHandleState.Semaphore s -> s

    let private acquired (outcome : WaitHandle.WaitOutcome) : IlMachineState =
        match outcome with
        | WaitHandle.WaitOutcome.Acquired state -> state
        | WaitHandle.WaitOutcome.Blocked _ -> failwith "expected Acquired but got Blocked"

    let private blocked (outcome : WaitHandle.WaitOutcome) : IlMachineState =
        match outcome with
        | WaitHandle.WaitOutcome.Blocked state -> state
        | WaitHandle.WaitOutcome.Acquired _ -> failwith "expected Blocked but got Acquired"

    let private t0 = ThreadId 0
    let private t1 = ThreadId 1
    let private t2 = ThreadId 2
    let private t3 = ThreadId 3
    let private t4 = ThreadId 4

    // -------------------------------------------------------------------
    // createSemaphore — round-trip and input validation
    // -------------------------------------------------------------------

    [<Test>]
    let ``createSemaphore mints distinct ids`` () : unit =
        let state = baseState ()
        let id1, state = WaitHandle.createSemaphore 0 1 state
        let id2, state = WaitHandle.createSemaphore 0 1 state
        let id3, _ = WaitHandle.createSemaphore 0 1 state

        id1 |> shouldNotEqual id2
        id1 |> shouldNotEqual id3
        id2 |> shouldNotEqual id3

    [<Test>]
    let ``minted wait-handle ids are never zero (BCL OOM guard never fires)`` () : unit =
        let state = baseState ()
        let id, _ = WaitHandle.createSemaphore 0 1 state
        let (WaitHandleId i) = id
        // The handle must be non-zero so the BCL's `if (handle ==
        // IntPtr.Zero) throw new ...` check stays quiet for successful
        // creates.
        i |> shouldNotEqual 0

    [<Test>]
    let ``newly minted semaphore reflects the requested (count, maximum) with an empty queue`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createSemaphore 3 5 state

        let s = semaphoreOf id state
        s.Count |> shouldEqual 3
        s.Maximum |> shouldEqual 5
        s.WaitQueue |> shouldEqual []

    [<Test>]
    let ``createSemaphore rejects maximum < 1`` () : unit =
        let state = baseState ()

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.createSemaphore 0 0 state |> ignore)

        exn.Message |> shouldContainText "maximumCount"

    [<Test>]
    let ``createSemaphore rejects negative initial`` () : unit =
        let state = baseState ()

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.createSemaphore -1 5 state |> ignore)

        exn.Message |> shouldContainText "initialCount"

    [<Test>]
    let ``createSemaphore rejects initial > maximum`` () : unit =
        let state = baseState ()

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.createSemaphore 6 5 state |> ignore)

        exn.Message |> shouldContainText "exceeds maximumCount"

    [<Test>]
    let ``Property: createSemaphore round-trips the requested (initial, maximum)`` () : unit =
        let property (NonNegativeInt initial) (PositiveInt maximum) : bool =
            if initial > maximum then
                true
            else
                let state = baseState ()
                let id, state = WaitHandle.createSemaphore initial maximum state

                let s = semaphoreOf id state
                s.Count = initial && s.Maximum = maximum && s.WaitQueue = []

        Check.One (config, property)

    // -------------------------------------------------------------------
    // waitOne — fast path / slow path
    // -------------------------------------------------------------------

    [<Test>]
    let ``waitOne on a signalled semaphore decrements count and stays Runnable`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createSemaphore 2 5 state

        let state = WaitHandle.waitOne t0 id state |> acquired

        let s = semaphoreOf id state
        s.Count |> shouldEqual 1
        s.WaitQueue |> shouldEqual []
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable

    [<Test>]
    let ``waitOne on a zero-count semaphore parks the caller at the FIFO tail`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let id, state = WaitHandle.createSemaphore 0 5 state

        let state = WaitHandle.waitOne t0 id state |> blocked
        let state = WaitHandle.waitOne t1 id state |> blocked

        let s = semaphoreOf id state
        s.Count |> shouldEqual 0
        s.WaitQueue |> shouldEqual [ t0 ; t1 ]
        statusOf t0 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle id)
        statusOf t1 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle id)

    [<Test>]
    let ``waitOne fast path drives count to zero in sequence`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ]
        let id, state = WaitHandle.createSemaphore 2 5 state
        let state = WaitHandle.waitOne t0 id state |> acquired
        let state = WaitHandle.waitOne t1 id state |> acquired
        // Count is now 0; t2 must block.
        let state = WaitHandle.waitOne t2 id state |> blocked

        let s = semaphoreOf id state
        s.Count |> shouldEqual 0
        s.WaitQueue |> shouldEqual [ t2 ]
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable
        statusOf t1 state |> shouldEqual ThreadStatus.Runnable
        statusOf t2 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle id)

    // -------------------------------------------------------------------
    // releaseSemaphore — direct handoff, overflow, FIFO
    // -------------------------------------------------------------------

    [<Test>]
    let ``releaseSemaphore on an idle semaphore increments count and reports previous`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createSemaphore 1 5 state

        let result, state = WaitHandle.releaseSemaphore id 2 state

        result |> shouldEqual (Ok 1)
        (semaphoreOf id state).Count |> shouldEqual 3

    [<Test>]
    let ``releaseSemaphore returns previous count of zero from a fresh semaphore`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createSemaphore 0 5 state

        let result, state = WaitHandle.releaseSemaphore id 1 state

        result |> shouldEqual (Ok 0)
        (semaphoreOf id state).Count |> shouldEqual 1

    [<Test>]
    let ``releaseSemaphore wakes a single FIFO-head waiter via direct handoff`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let id, state = WaitHandle.createSemaphore 0 5 state
        let state = WaitHandle.waitOne t0 id state |> blocked
        let state = WaitHandle.waitOne t1 id state |> blocked

        let result, state = WaitHandle.releaseSemaphore id 1 state

        result |> shouldEqual (Ok 0)
        let s = semaphoreOf id state
        // Direct handoff: the freshly-added unit was consumed by t0;
        // count stays at 0. t1 remains parked behind.
        s.Count |> shouldEqual 0
        s.WaitQueue |> shouldEqual [ t1 ]
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable
        statusOf t1 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle id)

    [<Test>]
    let ``releaseSemaphore N wakes min(N, K) FIFO-head waiters and leaves N-K units accumulated`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ; t3 ]

        let id, state = WaitHandle.createSemaphore 0 10 state
        let state = WaitHandle.waitOne t0 id state |> blocked
        let state = WaitHandle.waitOne t1 id state |> blocked
        let state = WaitHandle.waitOne t2 id state |> blocked
        // Release 5 with 3 waiters: wakes all three; (5 - 3) = 2 units
        // accumulate in Count.
        let result, state = WaitHandle.releaseSemaphore id 5 state

        result |> shouldEqual (Ok 0)
        let s = semaphoreOf id state
        s.Count |> shouldEqual 2
        s.WaitQueue |> shouldEqual []
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable
        statusOf t1 state |> shouldEqual ThreadStatus.Runnable
        statusOf t2 state |> shouldEqual ThreadStatus.Runnable

    [<Test>]
    let ``releaseSemaphore N wakes only N of K waiters when K > N`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ; t3 ; t4 ]

        let id, state = WaitHandle.createSemaphore 0 10 state
        let state = WaitHandle.waitOne t0 id state |> blocked
        let state = WaitHandle.waitOne t1 id state |> blocked
        let state = WaitHandle.waitOne t2 id state |> blocked
        let state = WaitHandle.waitOne t3 id state |> blocked
        let state = WaitHandle.waitOne t4 id state |> blocked

        let result, state = WaitHandle.releaseSemaphore id 2 state

        result |> shouldEqual (Ok 0)
        let s = semaphoreOf id state
        // Direct handoff: both new units consumed by the FIFO-head pair;
        // Count remains 0. t2..t4 stay parked in FIFO order.
        s.Count |> shouldEqual 0
        s.WaitQueue |> shouldEqual [ t2 ; t3 ; t4 ]
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable
        statusOf t1 state |> shouldEqual ThreadStatus.Runnable
        statusOf t2 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle id)
        statusOf t3 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle id)
        statusOf t4 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle id)

    [<Test>]
    let ``releaseSemaphore rejects overflow and leaves state unchanged`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createSemaphore 4 5 state
        let before = semaphoreOf id state

        let result, state' = WaitHandle.releaseSemaphore id 2 state

        match result with
        | Error (WaitHandle.ReleaseFailure.WouldExceedMaximum (attempted, maximum)) ->
            attempted |> shouldEqual 6L
            maximum |> shouldEqual 5
        | Ok _ -> failwith "expected overflow but got success"

        // State must be untouched: a failed release is observably a no-op
        // on the semaphore side, mirroring `ERROR_TOO_MANY_POSTS` /
        // `SemaphoreFullException`.
        semaphoreOf id state' |> shouldEqual before

    [<Test>]
    let ``releaseSemaphore rejects overflow without int32 wraparound near Int32.MaxValue`` () : unit =
        // Regression for an int32 overflow in the maximum check: at
        // `previousCount = Int32.MaxValue`, computing `previousCount +
        // releaseCount` wraps to a negative value that's < Maximum, so
        // the naive check would silently accept the release and store a
        // negative `Count`. The actual check must compare without
        // computing the sum.
        let state = baseState ()

        let id, state =
            WaitHandle.createSemaphore System.Int32.MaxValue System.Int32.MaxValue state

        let before = semaphoreOf id state

        let result, state' = WaitHandle.releaseSemaphore id 1 state

        match result with
        | Error (WaitHandle.ReleaseFailure.WouldExceedMaximum (attempted, maximum)) ->
            // The error must carry the true (non-wrapped) total.
            attempted |> shouldEqual (int64 System.Int32.MaxValue + 1L)
            maximum |> shouldEqual System.Int32.MaxValue
        | Ok _ -> failwith "expected overflow but got success"

        semaphoreOf id state' |> shouldEqual before

    [<Test>]
    let ``releaseSemaphore rejects non-positive releaseCount`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createSemaphore 0 5 state

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.releaseSemaphore id 0 state |> ignore)

        exn.Message |> shouldContainText "releaseCount"

    [<Test>]
    let ``Property: balanced release-then-waitOne pairs return semaphore to its initial count`` () : unit =
        let property (PositiveInt n) : bool =
            let n = min 16 n
            let threads = [ 0 .. n - 1 ] |> List.map ThreadId
            let state = baseState () |> withThreads threads
            // Start signalled at n so every waitOne is fast-path; releases
            // bring count back up to n.
            let id, state = WaitHandle.createSemaphore n n state

            let state =
                threads
                |> List.fold
                    (fun s tid ->
                        let s = WaitHandle.waitOne tid id s |> acquired
                        let _, s = WaitHandle.releaseSemaphore id 1 s
                        s
                    )
                    state

            let s = semaphoreOf id state

            s.Count = n
            && s.WaitQueue = []
            && threads |> List.forall (fun t -> statusOf t state = ThreadStatus.Runnable)

        Check.One (config, property)

    /// FIFO fairness oracle for releaseSemaphore. Block K threads on a
    /// zero-count semaphore in registration order, then issue K single-unit
    /// releases. Each release must wake the FIFO head of the wait queue
    /// (not a later waiter), so the observed wake order equals the
    /// registration order. Moving the wait queue to LIFO or arbitrary
    /// ordering would break LowLevelLifoSemaphore fairness higher up.
    [<Test>]
    let ``Property: releaseSemaphore wakes parked waiters in FIFO registration order`` () : unit =
        let property (PositiveInt k) : bool =
            let k = min 16 k
            let threads = [ 0 .. k - 1 ] |> List.map ThreadId
            let state = baseState () |> withThreads threads
            let id, state = WaitHandle.createSemaphore 0 k state

            let state =
                threads |> List.fold (fun s tid -> WaitHandle.waitOne tid id s |> blocked) state

            // Snapshot the queue before any releases.
            let expectedWakeOrder = (semaphoreOf id state).WaitQueue

            let mutable state = state
            let mutable observed = []

            for _ in 1..k do
                // Look at which thread is currently at the head before
                // release; we'll verify it transitioned to Runnable below.
                let head = List.head (semaphoreOf id state).WaitQueue
                let result, s = WaitHandle.releaseSemaphore id 1 state
                state <- s

                match result with
                | Ok _ -> ()
                | Error _ -> failwith "unexpected overflow in a fresh K-zero semaphore"

                if statusOf head state <> ThreadStatus.Runnable then
                    failwith $"release did not wake the FIFO head %O{head}"

                observed <- observed @ [ head ]

            let s = semaphoreOf id state

            observed = expectedWakeOrder
            && s.Count = 0
            && s.WaitQueue = []
            && threads |> List.forall (fun t -> statusOf t state = ThreadStatus.Runnable)

        Check.One (config, property)

    // -------------------------------------------------------------------
    // close — invariants
    // -------------------------------------------------------------------

    [<Test>]
    let ``close removes a quiescent semaphore from the registry`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createSemaphore 0 1 state

        let state = WaitHandle.close id state

        Map.containsKey id state.Kernel.WaitHandles |> shouldEqual false

    [<Test>]
    let ``close on a signalled (non-zero count) semaphore is permitted`` () : unit =
        // The Win32 contract only forbids closing with parked waiters;
        // a semaphore that simply has spare capacity (no blocked threads)
        // is fine to dispose. The BCL's `Dispose` does exactly this.
        let state = baseState ()
        let id, state = WaitHandle.createSemaphore 3 5 state

        let state = WaitHandle.close id state

        Map.containsKey id state.Kernel.WaitHandles |> shouldEqual false

    [<Test>]
    let ``close fails loud with parked waiters`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createSemaphore 0 5 state
        let state = WaitHandle.waitOne t0 id state |> blocked

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.close id state |> ignore)

        exn.Message |> shouldContainText "parked"

    [<Test>]
    let ``close on an unknown handle fails loud (use-after-free)`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createSemaphore 0 1 state
        let state = WaitHandle.close id state

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.close id state |> ignore)

        exn.Message |> shouldContainText "not registered"

    [<Test>]
    let ``waitOne on a closed handle fails loud (use-after-free)`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createSemaphore 1 5 state
        let state = WaitHandle.close id state

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.waitOne t0 id state |> ignore)

        exn.Message |> shouldContainText "not registered"

    [<Test>]
    let ``releaseSemaphore on a closed handle fails loud (use-after-free)`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createSemaphore 1 5 state
        let state = WaitHandle.close id state

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.releaseSemaphore id 1 state |> ignore)

        exn.Message |> shouldContainText "not registered"

    // -------------------------------------------------------------------
    // Count invariant oracle: an arbitrary script of Wait / Release on a
    // single semaphore must keep `Count ∈ [0, Maximum]` and preserve the
    // conservation law
    //     Count - |WaitQueue| = initial + Σreleases - Σwaits
    // where Σreleases counts only successful releases (overflows reject)
    // and Σwaits counts every `waitOne` call applied to the semaphore (both
    // fast-path acquires that decrement Count and slow-path parks that
    // enqueue). The sign of `|WaitQueue|` is negative because a parked
    // thread is "owed" a unit: a subsequent release will hand a freshly
    // issued unit to it directly, never accumulating into Count.
    // -------------------------------------------------------------------

    [<RequireQualifiedAccess>]
    type private Op =
        | Wait of ThreadId
        | Release of int

    /// Apply one script step. Returns the updated state plus a pair of
    /// (delta-from-waits, delta-from-releases) used by the oracle. A
    /// release that overflows contributes 0 (its state is unchanged); a
    /// wait by a thread that's already blocked is a guest bug and we just
    /// skip the script step (the oracle works on what was actually applied).
    let private applyOp
        (id : WaitHandleId)
        (op : Op)
        (state : IlMachineState)
        (waited : int)
        (released : int)
        : IlMachineState * int * int
        =
        match op with
        | Op.Wait tid ->
            // A thread that's already blocked cannot wait again; in
            // PawPrint that would re-park it which is a guest bug. Skip.
            match statusOf tid state with
            | ThreadStatus.BlockedOnWaitHandle _ -> state, waited, released
            | _ ->
                let outcome = WaitHandle.waitOne tid id state

                match outcome with
                | WaitHandle.WaitOutcome.Acquired s -> s, waited + 1, released
                | WaitHandle.WaitOutcome.Blocked s -> s, waited + 1, released
        | Op.Release n ->
            let result, state' = WaitHandle.releaseSemaphore id n state

            match result with
            | Ok _ -> state', waited, released + n
            | Error _ -> state, waited, released

    [<Test>]
    let ``Property: count invariant + conservation hold across an arbitrary Wait/Release script`` () : unit =
        // Generators kept small so the script stays scrutable but covers
        // the interesting interleavings (fast-path acquires, parking, and
        // releases that wake K of N waiters).
        let property (NonNegativeInt initialRaw) (PositiveInt maximumRaw) (PositiveInt threadCountRaw) : bool =
            let maximum = 1 + (maximumRaw % 10)
            let initial = min initialRaw maximum
            let threadCount = 1 + (threadCountRaw % 6)
            let threads = [ 0 .. threadCount - 1 ] |> List.map ThreadId
            let state = baseState () |> withThreads threads
            let id, state = WaitHandle.createSemaphore initial maximum state

            // Drive a fixed-length pseudo-random script using a
            // deterministic seed so any failure shrinks reproducibly.
            let scriptLen = 40
            let mutable rng = System.Random (initial * 31 + maximum * 17 + threadCount)
            let mutable state = state
            let mutable totalWaited = 0
            let mutable totalReleased = 0
            let mutable invariantOk = true

            for _ in 1..scriptLen do
                let op =
                    if rng.Next 2 = 0 then
                        Op.Wait (List.item (rng.Next threadCount) threads)
                    else
                        Op.Release (1 + rng.Next maximum)

                let s, w, r = applyOp id op state totalWaited totalReleased
                state <- s
                totalWaited <- w
                totalReleased <- r

                let sem = semaphoreOf id state

                if sem.Count < 0 || sem.Count > sem.Maximum then
                    invariantOk <- false

            let sem = semaphoreOf id state
            // Conservation: count - parked = initial + releases - waits.
            // A parked thread is owed a unit, hence the negative sign.
            let parked = List.length sem.WaitQueue
            let conservation = sem.Count - parked = initial + totalReleased - totalWaited

            invariantOk && conservation && sem.Maximum = maximum

        Check.One (config, property)
