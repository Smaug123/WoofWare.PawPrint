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
            IsBackground = false
            Name = None
            Cpu = CpuId 0
        }

    let private withThreads (threads : ThreadId list) (state : IlMachineState) : IlMachineState =
        let threadMap =
            threads
            |> List.map (fun tid -> tid, stubThreadState ThreadStatus.Runnable)
            |> Map.ofList

        { state with
            ThreadState = threadMap
        }

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        Corelib.getBaseTypes corelib

    /// Threads with a *real* frame, so their evaluation stack exists.
    ///
    /// The frameless `stubThreadState` above is deliberate for the
    /// single-handle tests: those transitions only touch `Status` and the
    /// handle registry, so a frame dereference would be a bug and the sentinel
    /// `FrameId -1` catches it. Multi-handle waits genuinely need the stack —
    /// a wait-any's return value is not known until the wake, so the waker has
    /// to rewrite the slot pushed at park time — and these tests assert on
    /// exactly that value.
    let private withFramedThreads (threads : ThreadId list) (state : IlMachineState) : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        // Any concrete method with a body will do; nothing reads its
        // instructions, only its frame's evaluation stack.
        let objectToString =
            baseClassTypes.Object.Methods
            |> List.find (fun method -> method.Name = "ToString" && method.Parameters.IsEmpty)

        let state, signature =
            TypeMethodSignature.map
                state
                (fun state ty ->
                    IlMachineState.concretizeType
                        loggerFactory
                        baseClassTypes
                        state
                        corelib.Name
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        ty
                )
                objectToString.Signature

        let method =
            objectToString
            |> MethodInfo.mapTypeGenerics (fun _ -> failwith "System.Object::ToString is not type-generic")
            |> MethodInfo.mapMethodGenerics (fun _ _ -> failwith "System.Object::ToString is not method-generic")
            |> MethodInfo.setMethodVars (MethodBody.Il (MethodInstructions.onlyRet ())) signature

        let threadMap =
            threads
            |> List.map (fun tid ->
                let methodState =
                    match
                        MethodState.Empty
                            state.ConcreteTypes
                            baseClassTypes
                            state._LoadedAssemblies
                            corelib
                            method
                            ImmutableArray.Empty
                            (ImmutableArray.Create (CliType.ObjectRef None))
                            None
                    with
                    | Ok methodState -> methodState
                    | Error missing -> failwith $"Unexpected missing assembly references creating frame: %O{missing}"

                tid, ThreadState.New (CpuId 0) methodState
            )
            |> Map.ofList

        { state with
            ThreadState = threadMap
        }

    /// The single value on `thread`'s evaluation stack — the wait result the
    /// guest will observe when the scheduler next picks it up.
    let private waitResultOf (thread : ThreadId) (state : IlMachineState) : int =
        let value, _ = IlMachineState.popEvalStack thread state

        match value with
        | EvalStackValue.Int32 result -> result
        | other -> failwith $"expected an Int32 wait result on thread %O{thread}'s eval stack, got %O{other}"

    let private evalStackDepth (thread : ThreadId) (state : IlMachineState) : int =
        let threadState = state.ThreadState.[thread]
        threadState.MethodStates.[threadState.ActiveMethodState].EvaluationStack.Values.Length

    let private statusOf (thread : ThreadId) (state : IlMachineState) : ThreadStatus = state.ThreadState.[thread].Status

    let private semaphoreOf (id : WaitHandleId) (state : IlMachineState) : SemaphoreState =
        match Map.find id state.Kernel.WaitHandles with
        | WaitHandleState.Semaphore s -> s
        | WaitHandleState.Mutex _ -> failwith "expected a Semaphore handle but got a Mutex"
        | WaitHandleState.Event _ -> failwith "expected a Semaphore handle but got an Event"

    let private mutexOf (id : WaitHandleId) (state : IlMachineState) : MutexState =
        match Map.find id state.Kernel.WaitHandles with
        | WaitHandleState.Mutex m -> m
        | WaitHandleState.Semaphore _ -> failwith "expected a Mutex handle but got a Semaphore"
        | WaitHandleState.Event _ -> failwith "expected a Mutex handle but got an Event"

    let private eventOf (id : WaitHandleId) (state : IlMachineState) : EventState =
        match Map.find id state.Kernel.WaitHandles with
        | WaitHandleState.Event e -> e
        | WaitHandleState.Semaphore _ -> failwith "expected an Event handle but got a Semaphore"
        | WaitHandleState.Mutex _ -> failwith "expected an Event handle but got a Mutex"

    let private acquired (outcome : WaitHandle.WaitOutcome) : IlMachineState =
        match outcome with
        | WaitHandle.WaitOutcome.Acquired state -> state
        | WaitHandle.WaitOutcome.AcquiredAbandoned _ -> failwith "expected Acquired but got AcquiredAbandoned"
        | WaitHandle.WaitOutcome.Blocked _ -> failwith "expected Acquired but got Blocked"

    let private acquiredAbandoned (outcome : WaitHandle.WaitOutcome) : IlMachineState =
        match outcome with
        | WaitHandle.WaitOutcome.AcquiredAbandoned state -> state
        | WaitHandle.WaitOutcome.Acquired _ -> failwith "expected AcquiredAbandoned but got Acquired"
        | WaitHandle.WaitOutcome.Blocked _ -> failwith "expected AcquiredAbandoned but got Blocked"

    let private blocked (outcome : WaitHandle.WaitOutcome) : IlMachineState =
        match outcome with
        | WaitHandle.WaitOutcome.Blocked state -> state
        | WaitHandle.WaitOutcome.Acquired _ -> failwith "expected Blocked but got Acquired"
        | WaitHandle.WaitOutcome.AcquiredAbandoned _ -> failwith "expected Blocked but got AcquiredAbandoned"

    let private tryAcquired (outcome : WaitHandle.TryWaitOutcome) : IlMachineState =
        match outcome with
        | WaitHandle.TryWaitOutcome.Acquired state -> state
        | WaitHandle.TryWaitOutcome.AcquiredAbandoned _ -> failwith "expected Acquired but got AcquiredAbandoned"
        | WaitHandle.TryWaitOutcome.TimedOut _ -> failwith "expected Acquired but got TimedOut"

    let private tryAcquiredAbandoned (outcome : WaitHandle.TryWaitOutcome) : IlMachineState =
        match outcome with
        | WaitHandle.TryWaitOutcome.AcquiredAbandoned state -> state
        | WaitHandle.TryWaitOutcome.Acquired _ -> failwith "expected AcquiredAbandoned but got Acquired"
        | WaitHandle.TryWaitOutcome.TimedOut _ -> failwith "expected AcquiredAbandoned but got TimedOut"

    let private timedOut (outcome : WaitHandle.TryWaitOutcome) : IlMachineState =
        match outcome with
        | WaitHandle.TryWaitOutcome.TimedOut state -> state
        | WaitHandle.TryWaitOutcome.Acquired _ -> failwith "expected TimedOut but got Acquired"
        | WaitHandle.TryWaitOutcome.AcquiredAbandoned _ -> failwith "expected TimedOut but got AcquiredAbandoned"

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

        let state = WaitHandle.waitOne t0 id None state |> acquired

        let s = semaphoreOf id state
        s.Count |> shouldEqual 1
        s.WaitQueue |> shouldEqual []
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable

    [<Test>]
    let ``waitOne on a zero-count semaphore parks the caller at the FIFO tail`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let id, state = WaitHandle.createSemaphore 0 5 state

        let state = WaitHandle.waitOne t0 id None state |> blocked
        let state = WaitHandle.waitOne t1 id None state |> blocked

        let s = semaphoreOf id state
        s.Count |> shouldEqual 0
        s.WaitQueue |> shouldEqual [ t0 ; t1 ]
        statusOf t0 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))
        statusOf t1 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))

    [<Test>]
    let ``waitOne fast path drives count to zero in sequence`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ]
        let id, state = WaitHandle.createSemaphore 2 5 state
        let state = WaitHandle.waitOne t0 id None state |> acquired
        let state = WaitHandle.waitOne t1 id None state |> acquired
        // Count is now 0; t2 must block.
        let state = WaitHandle.waitOne t2 id None state |> blocked

        let s = semaphoreOf id state
        s.Count |> shouldEqual 0
        s.WaitQueue |> shouldEqual [ t2 ]
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable
        statusOf t1 state |> shouldEqual ThreadStatus.Runnable
        statusOf t2 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))

    // -------------------------------------------------------------------
    // tryWaitOne — non-blocking probe used by zero-timeout WaitOne(0)
    // -------------------------------------------------------------------

    [<Test>]
    let ``tryWaitOne on a signalled semaphore decrements count and reports Acquired`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createSemaphore 2 5 state

        let state = WaitHandle.tryWaitOne t0 id state |> tryAcquired

        let s = semaphoreOf id state
        s.Count |> shouldEqual 1
        s.WaitQueue |> shouldEqual []

    [<Test>]
    let ``tryWaitOne on a zero-count semaphore reports TimedOut without parking`` () : unit =
        // The deterministic non-blocking probe the BCL drives through a
        // zero-timeout WaitOne(0). CoreCLR's semantics: the caller does
        // not enter the wait queue and the handle is left untouched —
        // we'd be observably violating the contract if we parked the
        // thread for what should be an immediate-return path.
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createSemaphore 0 5 state

        let state = WaitHandle.tryWaitOne t0 id state |> timedOut

        let s = semaphoreOf id state
        s.Count |> shouldEqual 0
        s.WaitQueue |> shouldEqual []
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable

    // -------------------------------------------------------------------
    // waitOnePrioritized — LIFO insertion at head of wait queue
    // -------------------------------------------------------------------

    [<Test>]
    let ``waitOnePrioritized on a signalled semaphore decrements count and stays Runnable`` () : unit =
        // Fast path is identical to waitOne: priority only matters when
        // the wait actually blocks.
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createSemaphore 2 5 state

        let state = WaitHandle.waitOnePrioritized t0 id None state |> acquired

        let s = semaphoreOf id state
        s.Count |> shouldEqual 1
        s.WaitQueue |> shouldEqual []
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable

    [<Test>]
    let ``waitOnePrioritized parks each caller at the HEAD of the wait queue`` () : unit =
        // PAL_WaitForSingleObjectPrioritized contract: prioritized waiters
        // are registered at the BEGINNING of the wait queue (LIFO release
        // policy). Verifies the queue shape after three back-to-back
        // prioritized waits.
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ]
        let id, state = WaitHandle.createSemaphore 0 5 state

        let state = WaitHandle.waitOnePrioritized t0 id None state |> blocked
        let state = WaitHandle.waitOnePrioritized t1 id None state |> blocked
        let state = WaitHandle.waitOnePrioritized t2 id None state |> blocked

        let s = semaphoreOf id state
        s.Count |> shouldEqual 0
        // Latest-arrived prioritized waiter is at head; oldest is at tail.
        s.WaitQueue |> shouldEqual [ t2 ; t1 ; t0 ]
        statusOf t0 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))
        statusOf t1 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))
        statusOf t2 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))

    [<Test>]
    let ``releaseSemaphore wakes prioritized waiters in LIFO order`` () : unit =
        // The LowLevelLifoSemaphore contract that PortableThreadPool
        // relies on: a later-arrived prioritized waiter is woken before
        // an earlier-arrived one.
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ]
        let id, state = WaitHandle.createSemaphore 0 5 state

        let state = WaitHandle.waitOnePrioritized t0 id None state |> blocked
        let state = WaitHandle.waitOnePrioritized t1 id None state |> blocked
        let state = WaitHandle.waitOnePrioritized t2 id None state |> blocked

        let _, state = WaitHandle.releaseSemaphore id 1 state

        statusOf t2 state |> shouldEqual ThreadStatus.Runnable
        statusOf t1 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))
        statusOf t0 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))

        let _, state = WaitHandle.releaseSemaphore id 1 state

        statusOf t1 state |> shouldEqual ThreadStatus.Runnable
        statusOf t0 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))

        let _, state = WaitHandle.releaseSemaphore id 1 state

        statusOf t0 state |> shouldEqual ThreadStatus.Runnable

    [<Test>]
    let ``prioritized waiter wakes before any earlier-arrived non-prioritized waiter`` () : unit =
        // PAL contract: a prioritized waiter goes to the HEAD of the queue
        // even when non-prioritized waiters are already enqueued behind it.
        // Verifies the cross-class ordering (prioritized strictly precedes
        // earlier-arrived non-prioritized in wake order).
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ]
        let id, state = WaitHandle.createSemaphore 0 5 state

        let state = WaitHandle.waitOne t0 id None state |> blocked
        let state = WaitHandle.waitOne t1 id None state |> blocked
        let state = WaitHandle.waitOnePrioritized t2 id None state |> blocked

        let s = semaphoreOf id state
        // t2 is at the head; t0 and t1 preserve their FIFO ordering behind.
        s.WaitQueue |> shouldEqual [ t2 ; t0 ; t1 ]

        let _, state = WaitHandle.releaseSemaphore id 1 state

        statusOf t2 state |> shouldEqual ThreadStatus.Runnable
        statusOf t0 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))
        statusOf t1 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))

    [<Test>]
    let ``Property: releaseSemaphore wakes prioritized waiters in LIFO registration order`` () : unit =
        let property (PositiveInt k) : bool =
            let k = min 16 k
            let threads = [ 0 .. k - 1 ] |> List.map ThreadId
            let state = baseState () |> withThreads threads
            let id, state = WaitHandle.createSemaphore 0 k state

            let state =
                threads
                |> List.fold (fun s tid -> WaitHandle.waitOnePrioritized tid id None s |> blocked) state

            // LIFO over prioritized waiters: latest registration wakes first.
            let expectedWakeOrder = List.rev threads

            let mutable state = state
            let mutable observed = []

            for _ in 1..k do
                let head = List.head (semaphoreOf id state).WaitQueue
                let _, s = WaitHandle.releaseSemaphore id 1 state
                state <- s
                observed <- observed @ [ head ]

            let s = semaphoreOf id state

            observed = expectedWakeOrder
            && s.Count = 0
            && s.WaitQueue = []
            && threads |> List.forall (fun t -> statusOf t state = ThreadStatus.Runnable)

        Check.One (config, property)

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
        let state = WaitHandle.waitOne t0 id None state |> blocked
        let state = WaitHandle.waitOne t1 id None state |> blocked

        let result, state = WaitHandle.releaseSemaphore id 1 state

        result |> shouldEqual (Ok 0)
        let s = semaphoreOf id state
        // Direct handoff: the freshly-added unit was consumed by t0;
        // count stays at 0. t1 remains parked behind.
        s.Count |> shouldEqual 0
        s.WaitQueue |> shouldEqual [ t1 ]
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable
        statusOf t1 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))

    [<Test>]
    let ``releaseSemaphore N wakes min(N, K) FIFO-head waiters and leaves N-K units accumulated`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ; t3 ]

        let id, state = WaitHandle.createSemaphore 0 10 state
        let state = WaitHandle.waitOne t0 id None state |> blocked
        let state = WaitHandle.waitOne t1 id None state |> blocked
        let state = WaitHandle.waitOne t2 id None state |> blocked
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
        let state = WaitHandle.waitOne t0 id None state |> blocked
        let state = WaitHandle.waitOne t1 id None state |> blocked
        let state = WaitHandle.waitOne t2 id None state |> blocked
        let state = WaitHandle.waitOne t3 id None state |> blocked
        let state = WaitHandle.waitOne t4 id None state |> blocked

        let result, state = WaitHandle.releaseSemaphore id 2 state

        result |> shouldEqual (Ok 0)
        let s = semaphoreOf id state
        // Direct handoff: both new units consumed by the FIFO-head pair;
        // Count remains 0. t2..t4 stay parked in FIFO order.
        s.Count |> shouldEqual 0
        s.WaitQueue |> shouldEqual [ t2 ; t3 ; t4 ]
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable
        statusOf t1 state |> shouldEqual ThreadStatus.Runnable
        statusOf t2 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))
        statusOf t3 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))
        statusOf t4 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))

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
                        let s = WaitHandle.waitOne tid id None s |> acquired
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
                threads
                |> List.fold (fun s tid -> WaitHandle.waitOne tid id None s |> blocked) state

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
        let state = WaitHandle.waitOne t0 id None state |> blocked

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
            Assert.Throws<System.Exception> (fun () -> WaitHandle.waitOne t0 id None state |> ignore)

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
                let outcome = WaitHandle.waitOne tid id None state

                match outcome with
                | WaitHandle.WaitOutcome.Acquired s -> s, waited + 1, released
                | WaitHandle.WaitOutcome.Blocked s -> s, waited + 1, released
                | WaitHandle.WaitOutcome.AcquiredAbandoned _ ->
                    // The semaphore variant never produces AcquiredAbandoned;
                    // that's mutex-only. Surface as a script-invariant break.
                    failwith "Semaphore waitOne unexpectedly produced AcquiredAbandoned"
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

    // -------------------------------------------------------------------
    // Mutex — create, re-entrancy, FIFO direct handoff, abandoned flag
    // -------------------------------------------------------------------

    [<Test>]
    let ``createMutex without initialOwner starts free, not abandoned, with empty queue`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createMutex false t0 state

        let m = mutexOf id state
        m.Ownership |> shouldEqual (MutexOwnership.Free false)
        m.WaitQueue |> shouldEqual []

    [<Test>]
    let ``createMutex with initialOwner installs the creator with recursion count 1`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createMutex true t0 state

        let m = mutexOf id state
        m.Ownership |> shouldEqual (MutexOwnership.Held (t0, 1))
        m.WaitQueue |> shouldEqual []

    [<Test>]
    let ``minted mutex ids are never zero (BCL OOM guard never fires)`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, _ = WaitHandle.createMutex false t0 state
        let (WaitHandleId i) = id
        i |> shouldNotEqual 0

    [<Test>]
    let ``mutex ids interleave with semaphore ids on a shared counter`` () : unit =
        // Single monotonic ID source; minting mutex / semaphore handles
        // alternately produces distinct ids in registration order.
        let state = baseState () |> withThreads [ t0 ]
        let mutexId1, state = WaitHandle.createMutex false t0 state
        let semId1, state = WaitHandle.createSemaphore 0 1 state
        let mutexId2, state = WaitHandle.createMutex false t0 state
        let semId2, _ = WaitHandle.createSemaphore 0 1 state

        mutexId1 |> shouldNotEqual semId1
        semId1 |> shouldNotEqual mutexId2
        mutexId2 |> shouldNotEqual semId2
        mutexId1 |> shouldNotEqual mutexId2

    [<Test>]
    let ``waitOne on a free mutex takes ownership with recursion count 1`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createMutex false t0 state

        let state = WaitHandle.waitOne t0 id None state |> acquired

        let m = mutexOf id state
        m.Ownership |> shouldEqual (MutexOwnership.Held (t0, 1))
        m.WaitQueue |> shouldEqual []
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable

    [<Test>]
    let ``waitOne by owner is re-entrant and bumps recursion count`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createMutex false t0 state

        let state = WaitHandle.waitOne t0 id None state |> acquired
        let state = WaitHandle.waitOne t0 id None state |> acquired
        let state = WaitHandle.waitOne t0 id None state |> acquired

        let m = mutexOf id state
        m.Ownership |> shouldEqual (MutexOwnership.Held (t0, 3))
        m.WaitQueue |> shouldEqual []

    [<Test>]
    let ``waitOne by a non-owner parks at the FIFO tail`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ]
        let id, state = WaitHandle.createMutex true t0 state

        let state = WaitHandle.waitOne t1 id None state |> blocked
        let state = WaitHandle.waitOne t2 id None state |> blocked

        let m = mutexOf id state
        m.Ownership |> shouldEqual (MutexOwnership.Held (t0, 1))
        m.WaitQueue |> shouldEqual [ t1 ; t2 ]
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable
        statusOf t1 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))
        statusOf t2 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))

    [<Test>]
    let ``releaseMutex by owner without waiters marks the mutex free`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createMutex true t0 state

        let result, state = WaitHandle.releaseMutex t0 id state

        result |> shouldEqual (Ok ())
        let m = mutexOf id state
        m.Ownership |> shouldEqual (MutexOwnership.Free false)
        m.WaitQueue |> shouldEqual []

    [<Test>]
    let ``releaseMutex unwinds recursion before releasing`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createMutex true t0 state
        let state = WaitHandle.waitOne t0 id None state |> acquired
        let state = WaitHandle.waitOne t0 id None state |> acquired

        let _, state = WaitHandle.releaseMutex t0 id state
        (mutexOf id state).Ownership |> shouldEqual (MutexOwnership.Held (t0, 2))

        let _, state = WaitHandle.releaseMutex t0 id state
        (mutexOf id state).Ownership |> shouldEqual (MutexOwnership.Held (t0, 1))

        let _, state = WaitHandle.releaseMutex t0 id state
        (mutexOf id state).Ownership |> shouldEqual (MutexOwnership.Free false)

    [<Test>]
    let ``releaseMutex with waiters hands ownership directly to the FIFO head`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ]
        let id, state = WaitHandle.createMutex true t0 state
        let state = WaitHandle.waitOne t1 id None state |> blocked
        let state = WaitHandle.waitOne t2 id None state |> blocked

        let result, state = WaitHandle.releaseMutex t0 id state

        result |> shouldEqual (Ok ())
        let m = mutexOf id state
        // Direct handoff: t1 wakes already owning the mutex with
        // recursion count 1; t2 stays parked behind.
        m.Ownership |> shouldEqual (MutexOwnership.Held (t1, 1))
        m.WaitQueue |> shouldEqual [ t2 ]
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable
        statusOf t1 state |> shouldEqual ThreadStatus.Runnable
        statusOf t2 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))

    [<Test>]
    let ``releaseMutex by a non-owner returns NotOwner and leaves state unchanged`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let id, state = WaitHandle.createMutex true t0 state
        let before = mutexOf id state

        let result, state' = WaitHandle.releaseMutex t1 id state

        result |> shouldEqual (Error WaitHandle.ReleaseMutexFailure.NotOwner)
        mutexOf id state' |> shouldEqual before

    [<Test>]
    let ``releaseMutex on a free mutex returns NotOwner and leaves state unchanged`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createMutex false t0 state
        let before = mutexOf id state

        let result, state' = WaitHandle.releaseMutex t0 id state

        result |> shouldEqual (Error WaitHandle.ReleaseMutexFailure.NotOwner)
        mutexOf id state' |> shouldEqual before

    [<Test>]
    let ``Property: matched waitOne/releaseMutex pairs by owner leave mutex Free false`` () : unit =
        let property (PositiveInt depthRaw) : bool =
            let depth = 1 + (depthRaw % 16)
            let state = baseState () |> withThreads [ t0 ]
            // Start free; the first WaitOne takes ownership; further
            // WaitOnes bump the recursion count; matched releases unwind.
            let id, state = WaitHandle.createMutex false t0 state

            let state =
                [ 1..depth ]
                |> List.fold (fun s _ -> WaitHandle.waitOne t0 id None s |> acquired) state

            (mutexOf id state).Ownership = MutexOwnership.Held (t0, depth)
            && let state =
                [ 1..depth ]
                |> List.fold
                    (fun s _ ->
                        let _, s = WaitHandle.releaseMutex t0 id s
                        s
                    )
                    state in

               (mutexOf id state).Ownership = MutexOwnership.Free false
               && (mutexOf id state).WaitQueue = []

        Check.One (config, property)

    // -------------------------------------------------------------------
    // tryWaitOne on mutex — kind-aware non-blocking probe
    // -------------------------------------------------------------------

    [<Test>]
    let ``tryWaitOne on a free mutex acquires it`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createMutex false t0 state

        let state = WaitHandle.tryWaitOne t0 id state |> tryAcquired

        (mutexOf id state).Ownership |> shouldEqual (MutexOwnership.Held (t0, 1))

    [<Test>]
    let ``tryWaitOne by owner of a held mutex bumps recursion count`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createMutex true t0 state

        let state = WaitHandle.tryWaitOne t0 id state |> tryAcquired

        (mutexOf id state).Ownership |> shouldEqual (MutexOwnership.Held (t0, 2))

    [<Test>]
    let ``tryWaitOne on a mutex held by another thread reports TimedOut without parking`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let id, state = WaitHandle.createMutex true t0 state

        let state = WaitHandle.tryWaitOne t1 id state |> timedOut

        // No enqueue, no status flip — the entire point of the zero-
        // timeout probe.
        let m = mutexOf id state
        m.Ownership |> shouldEqual (MutexOwnership.Held (t0, 1))
        m.WaitQueue |> shouldEqual []
        statusOf t1 state |> shouldEqual ThreadStatus.Runnable

    // -------------------------------------------------------------------
    // Abandoned flag — synthesised via direct state manipulation
    // -------------------------------------------------------------------
    // Full abandoned propagation (driving the flag via owner-thread
    // termination) is structural and deferred — Scheduler.onThreadTerminated
    // currently fails loud on owned mutexes. The state-machine behaviour
    // on a Free(wasAbandoned=true) mutex is still required to be correct
    // for when that gap closes; we test it here by synthesising the state
    // directly. -------------------------------------------------------------------

    let private installAbandonedMutex (id : WaitHandleId) (state : IlMachineState) : IlMachineState =
        state.MapKernel (fun kernel ->
            let mutex =
                {
                    Ownership = MutexOwnership.Free true
                    WaitQueue = []
                }

            { kernel with
                WaitHandles = Map.add id (WaitHandleState.Mutex mutex) kernel.WaitHandles
            }
        )

    [<Test>]
    let ``waitOne on an abandoned-flagged free mutex produces AcquiredAbandoned and clears the flag`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createMutex false t0 state
        let state = installAbandonedMutex id state

        let state = WaitHandle.waitOne t0 id None state |> acquiredAbandoned

        let m = mutexOf id state
        m.Ownership |> shouldEqual (MutexOwnership.Held (t0, 1))

    [<Test>]
    let ``tryWaitOne on an abandoned-flagged free mutex produces AcquiredAbandoned and clears the flag`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createMutex false t0 state
        let state = installAbandonedMutex id state

        let state = WaitHandle.tryWaitOne t0 id state |> tryAcquiredAbandoned

        let m = mutexOf id state
        m.Ownership |> shouldEqual (MutexOwnership.Held (t0, 1))

    [<Test>]
    let ``releaseMutex on a mutex acquired-abandoned leaves the flag cleared`` () : unit =
        // Once the abandoned flag is consumed by the acquiring WaitOne,
        // a subsequent release produces a plain Free(false), not a
        // Free(true) — the flag is sticky to a single wake, not to the
        // mutex permanently.
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createMutex false t0 state
        let state = installAbandonedMutex id state
        let state = WaitHandle.waitOne t0 id None state |> acquiredAbandoned

        let _, state = WaitHandle.releaseMutex t0 id state

        (mutexOf id state).Ownership |> shouldEqual (MutexOwnership.Free false)

    // -------------------------------------------------------------------
    // close — mutex variants
    // -------------------------------------------------------------------

    [<Test>]
    let ``close removes a quiescent free mutex from the registry`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createMutex false t0 state

        let state = WaitHandle.close id state

        Map.containsKey id state.Kernel.WaitHandles |> shouldEqual false

    [<Test>]
    let ``close fails loud on a still-held mutex`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createMutex true t0 state

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.close id state |> ignore)

        exn.Message |> shouldContainText "still held"

    [<Test>]
    let ``close fails loud on a mutex with parked waiters`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let id, state = WaitHandle.createMutex true t0 state
        let state = WaitHandle.waitOne t1 id None state |> blocked

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.close id state |> ignore)

        exn.Message |> shouldContainText "parked"

    // -------------------------------------------------------------------
    // Cross-kind failures — wrong-kind handle to a kind-specific operation
    // -------------------------------------------------------------------

    [<Test>]
    let ``releaseSemaphore on a mutex id fails loud`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createMutex false t0 state

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.releaseSemaphore id 1 state |> ignore)

        exn.Message |> shouldContainText "Mutex"

    [<Test>]
    let ``waitOnePrioritized on a mutex id fails loud`` () : unit =
        // The LowLevelLifoSemaphore park primitive is only ever called
        // against semaphores; routing a mutex into the prioritized
        // entry point is a guest bug.
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createMutex false t0 state

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.waitOnePrioritized t0 id None state |> ignore)

        exn.Message |> shouldContainText "Mutex"

    [<Test>]
    let ``releaseMutex on a semaphore id fails loud`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createSemaphore 1 5 state

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.releaseMutex t0 id state |> ignore)

        exn.Message |> shouldContainText "Semaphore"

    // -------------------------------------------------------------------
    // Use-after-free
    // -------------------------------------------------------------------

    [<Test>]
    let ``waitOne on a closed mutex fails loud (use-after-free)`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createMutex false t0 state
        let state = WaitHandle.close id state

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.waitOne t0 id None state |> ignore)

        exn.Message |> shouldContainText "not registered"

    [<Test>]
    let ``releaseMutex on a closed mutex fails loud (use-after-free)`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createMutex true t0 state
        // Release before close so the close itself succeeds.
        let _, state = WaitHandle.releaseMutex t0 id state
        let state = WaitHandle.close id state

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.releaseMutex t0 id state |> ignore)

        exn.Message |> shouldContainText "not registered"

    // -------------------------------------------------------------------
    // Event — create, set, reset, wait, close
    // -------------------------------------------------------------------

    [<Test>]
    let ``createEvent Manual unsignalled starts with empty queue and Signaled=false`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createEvent false EventResetMode.Manual state
        let e = eventOf id state
        e.Mode |> shouldEqual EventResetMode.Manual
        e.Signaled |> shouldEqual false
        e.WaitQueue |> shouldEqual []

    [<Test>]
    let ``createEvent Manual signalled starts Signaled=true with empty queue`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createEvent true EventResetMode.Manual state
        let e = eventOf id state
        e.Mode |> shouldEqual EventResetMode.Manual
        e.Signaled |> shouldEqual true
        e.WaitQueue |> shouldEqual []

    [<Test>]
    let ``createEvent Auto unsignalled starts with empty queue and Signaled=false`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createEvent false EventResetMode.Auto state
        let e = eventOf id state
        e.Mode |> shouldEqual EventResetMode.Auto
        e.Signaled |> shouldEqual false
        e.WaitQueue |> shouldEqual []

    [<Test>]
    let ``createEvent Auto signalled starts Signaled=true with empty queue`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createEvent true EventResetMode.Auto state
        let e = eventOf id state
        e.Mode |> shouldEqual EventResetMode.Auto
        e.Signaled |> shouldEqual true
        e.WaitQueue |> shouldEqual []

    [<Test>]
    let ``createEvent mints distinct ids`` () : unit =
        let state = baseState ()
        let id1, state = WaitHandle.createEvent false EventResetMode.Manual state
        let id2, state = WaitHandle.createEvent true EventResetMode.Auto state
        let id3, _ = WaitHandle.createEvent false EventResetMode.Manual state

        id1 |> shouldNotEqual id2
        id1 |> shouldNotEqual id3
        id2 |> shouldNotEqual id3

    [<Test>]
    let ``minted event ids are never zero (BCL OOM guard never fires)`` () : unit =
        let state = baseState ()
        let id, _ = WaitHandle.createEvent false EventResetMode.Manual state
        let (WaitHandleId i) = id
        i |> shouldNotEqual 0

    [<Test>]
    let ``event ids interleave with semaphore + mutex ids on the shared counter`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let semId, state = WaitHandle.createSemaphore 0 1 state
        let evId1, state = WaitHandle.createEvent false EventResetMode.Manual state
        let mutexId, state = WaitHandle.createMutex false t0 state
        let evId2, _ = WaitHandle.createEvent true EventResetMode.Auto state

        semId |> shouldNotEqual evId1
        evId1 |> shouldNotEqual mutexId
        mutexId |> shouldNotEqual evId2
        evId1 |> shouldNotEqual evId2

    // ---- waitOne ----

    [<Test>]
    let ``waitOne on a signalled Manual event acquires without clearing the signal`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createEvent true EventResetMode.Manual state

        let state = WaitHandle.waitOne t0 id None state |> acquired

        let e = eventOf id state
        e.Signaled |> shouldEqual true
        e.WaitQueue |> shouldEqual []
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable

    [<Test>]
    let ``waitOne on a signalled Auto event acquires and clears the signal`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createEvent true EventResetMode.Auto state

        let state = WaitHandle.waitOne t0 id None state |> acquired

        let e = eventOf id state
        e.Signaled |> shouldEqual false
        e.WaitQueue |> shouldEqual []
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable

    [<Test>]
    let ``waitOne on an unsignalled Manual event parks the caller at the FIFO tail`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ]
        let id, state = WaitHandle.createEvent false EventResetMode.Manual state

        let state = WaitHandle.waitOne t0 id None state |> blocked
        let state = WaitHandle.waitOne t1 id None state |> blocked
        let state = WaitHandle.waitOne t2 id None state |> blocked

        let e = eventOf id state
        e.Signaled |> shouldEqual false
        e.WaitQueue |> shouldEqual [ t0 ; t1 ; t2 ]
        statusOf t0 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))
        statusOf t1 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))
        statusOf t2 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))

    [<Test>]
    let ``waitOne on an unsignalled Auto event parks the caller at the FIFO tail`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ]
        let id, state = WaitHandle.createEvent false EventResetMode.Auto state

        let state = WaitHandle.waitOne t0 id None state |> blocked
        let state = WaitHandle.waitOne t1 id None state |> blocked

        let e = eventOf id state
        e.Signaled |> shouldEqual false
        e.WaitQueue |> shouldEqual [ t0 ; t1 ]

    // ---- setEvent ----

    [<Test>]
    let ``setEvent on a Manual event with no waiters latches Signaled=true`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createEvent false EventResetMode.Manual state

        let state = WaitHandle.setEvent id state

        let e = eventOf id state
        e.Signaled |> shouldEqual true
        e.WaitQueue |> shouldEqual []

    [<Test>]
    let ``setEvent on a Manual event wakes every parked waiter and latches Signaled=true`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ]
        let id, state = WaitHandle.createEvent false EventResetMode.Manual state
        let state = WaitHandle.waitOne t0 id None state |> blocked
        let state = WaitHandle.waitOne t1 id None state |> blocked
        let state = WaitHandle.waitOne t2 id None state |> blocked

        let state = WaitHandle.setEvent id state

        let e = eventOf id state
        e.Signaled |> shouldEqual true
        e.WaitQueue |> shouldEqual []
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable
        statusOf t1 state |> shouldEqual ThreadStatus.Runnable
        statusOf t2 state |> shouldEqual ThreadStatus.Runnable

    [<Test>]
    let ``setEvent on an Auto event with no waiters latches Signaled=true`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createEvent false EventResetMode.Auto state

        let state = WaitHandle.setEvent id state

        let e = eventOf id state
        e.Signaled |> shouldEqual true
        e.WaitQueue |> shouldEqual []

    [<Test>]
    let ``setEvent on an Auto event with waiters wakes only the FIFO head, Signaled stays false`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ]
        let id, state = WaitHandle.createEvent false EventResetMode.Auto state
        let state = WaitHandle.waitOne t0 id None state |> blocked
        let state = WaitHandle.waitOne t1 id None state |> blocked
        let state = WaitHandle.waitOne t2 id None state |> blocked

        let state = WaitHandle.setEvent id state

        let e = eventOf id state
        e.Signaled |> shouldEqual false
        e.WaitQueue |> shouldEqual [ t1 ; t2 ]
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable
        statusOf t1 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))
        statusOf t2 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))

    [<Test>]
    let ``repeated setEvent on Auto with a queue drains FIFO one at a time`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ; t3 ]
        let id, state = WaitHandle.createEvent false EventResetMode.Auto state
        let state = WaitHandle.waitOne t0 id None state |> blocked
        let state = WaitHandle.waitOne t1 id None state |> blocked
        let state = WaitHandle.waitOne t2 id None state |> blocked
        let state = WaitHandle.waitOne t3 id None state |> blocked

        let state = WaitHandle.setEvent id state
        (eventOf id state).WaitQueue |> shouldEqual [ t1 ; t2 ; t3 ]
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable

        let state = WaitHandle.setEvent id state
        (eventOf id state).WaitQueue |> shouldEqual [ t2 ; t3 ]
        statusOf t1 state |> shouldEqual ThreadStatus.Runnable

        let state = WaitHandle.setEvent id state
        (eventOf id state).WaitQueue |> shouldEqual [ t3 ]
        statusOf t2 state |> shouldEqual ThreadStatus.Runnable

        let state = WaitHandle.setEvent id state
        (eventOf id state).WaitQueue |> shouldEqual []
        statusOf t3 state |> shouldEqual ThreadStatus.Runnable
        // Queue drained; the latched signal should now be false (the
        // last setEvent woke t3 directly rather than latching).
        (eventOf id state).Signaled |> shouldEqual false

    [<Test>]
    let ``setEvent on an already-signalled Manual event is idempotent`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createEvent true EventResetMode.Manual state
        let before = eventOf id state

        let state = WaitHandle.setEvent id state

        eventOf id state |> shouldEqual before

    [<Test>]
    let ``setEvent on an already-signalled Auto event is idempotent`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createEvent true EventResetMode.Auto state
        let before = eventOf id state

        let state = WaitHandle.setEvent id state

        eventOf id state |> shouldEqual before

    [<Test>]
    let ``Auto event: setEvent then waitOne acquires and clears the signal`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createEvent false EventResetMode.Auto state
        let state = WaitHandle.setEvent id state
        (eventOf id state).Signaled |> shouldEqual true

        let state = WaitHandle.waitOne t0 id None state |> acquired

        let e = eventOf id state
        e.Signaled |> shouldEqual false
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable

    // ---- resetEvent ----

    [<Test>]
    let ``resetEvent clears Signaled on a signalled Manual event`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createEvent true EventResetMode.Manual state

        let state = WaitHandle.resetEvent id state

        (eventOf id state).Signaled |> shouldEqual false

    [<Test>]
    let ``resetEvent clears Signaled on a signalled Auto event`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createEvent true EventResetMode.Auto state

        let state = WaitHandle.resetEvent id state

        (eventOf id state).Signaled |> shouldEqual false

    [<Test>]
    let ``resetEvent on an already-unsignalled event is idempotent`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createEvent false EventResetMode.Manual state
        let before = eventOf id state

        let state = WaitHandle.resetEvent id state

        eventOf id state |> shouldEqual before

    [<Test>]
    let ``resetEvent on an unsignalled Manual event with parked waiters does not touch the queue`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let id, state = WaitHandle.createEvent false EventResetMode.Manual state
        let state = WaitHandle.waitOne t0 id None state |> blocked
        let state = WaitHandle.waitOne t1 id None state |> blocked

        let state = WaitHandle.resetEvent id state

        let e = eventOf id state
        e.Signaled |> shouldEqual false
        e.WaitQueue |> shouldEqual [ t0 ; t1 ]
        statusOf t0 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))
        statusOf t1 state |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None))

    // ---- tryWaitOne ----

    [<Test>]
    let ``tryWaitOne on a signalled Manual event acquires without clearing the signal`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createEvent true EventResetMode.Manual state

        let state = WaitHandle.tryWaitOne t0 id state |> tryAcquired

        (eventOf id state).Signaled |> shouldEqual true

    [<Test>]
    let ``tryWaitOne on a signalled Auto event acquires and clears the signal`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createEvent true EventResetMode.Auto state

        let state = WaitHandle.tryWaitOne t0 id state |> tryAcquired

        (eventOf id state).Signaled |> shouldEqual false

    [<Test>]
    let ``tryWaitOne on an unsignalled event reports TimedOut without parking`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createEvent false EventResetMode.Auto state

        let state = WaitHandle.tryWaitOne t0 id state |> timedOut

        let e = eventOf id state
        e.WaitQueue |> shouldEqual []
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable

    // ---- close ----

    [<Test>]
    let ``close removes a quiescent unsignalled event from the registry`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createEvent false EventResetMode.Manual state

        let state = WaitHandle.close id state

        Map.containsKey id state.Kernel.WaitHandles |> shouldEqual false

    [<Test>]
    let ``close removes a quiescent signalled event from the registry`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createEvent true EventResetMode.Auto state

        let state = WaitHandle.close id state

        Map.containsKey id state.Kernel.WaitHandles |> shouldEqual false

    [<Test>]
    let ``close fails loud on an event with parked waiters`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createEvent false EventResetMode.Manual state
        let state = WaitHandle.waitOne t0 id None state |> blocked

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.close id state |> ignore)

        exn.Message |> shouldContainText "parked"

    // ---- cross-kind safety ----

    [<Test>]
    let ``setEvent on a semaphore id fails loud`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createSemaphore 1 5 state

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.setEvent id state |> ignore)

        exn.Message |> shouldContainText "Semaphore"

    [<Test>]
    let ``setEvent on a mutex id fails loud`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createMutex false t0 state

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.setEvent id state |> ignore)

        exn.Message |> shouldContainText "Mutex"

    [<Test>]
    let ``resetEvent on a semaphore id fails loud`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createSemaphore 1 5 state

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.resetEvent id state |> ignore)

        exn.Message |> shouldContainText "Semaphore"

    [<Test>]
    let ``resetEvent on a mutex id fails loud`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createMutex false t0 state

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.resetEvent id state |> ignore)

        exn.Message |> shouldContainText "Mutex"

    [<Test>]
    let ``releaseSemaphore on an event id fails loud`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createEvent false EventResetMode.Manual state

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.releaseSemaphore id 1 state |> ignore)

        exn.Message |> shouldContainText "Event"

    [<Test>]
    let ``releaseMutex on an event id fails loud`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createEvent false EventResetMode.Manual state

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.releaseMutex t0 id state |> ignore)

        exn.Message |> shouldContainText "Event"

    [<Test>]
    let ``waitOnePrioritized on an event id fails loud`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createEvent false EventResetMode.Manual state

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.waitOnePrioritized t0 id None state |> ignore)

        exn.Message |> shouldContainText "Event"

    // ---- use-after-free ----

    [<Test>]
    let ``setEvent on a closed event fails loud (use-after-free)`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createEvent false EventResetMode.Manual state
        let state = WaitHandle.close id state

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.setEvent id state |> ignore)

        exn.Message |> shouldContainText "not registered"

    [<Test>]
    let ``resetEvent on a closed event fails loud (use-after-free)`` () : unit =
        let state = baseState ()
        let id, state = WaitHandle.createEvent false EventResetMode.Manual state
        let state = WaitHandle.close id state

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.resetEvent id state |> ignore)

        exn.Message |> shouldContainText "not registered"

    [<Test>]
    let ``waitOne on a closed event fails loud (use-after-free)`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createEvent false EventResetMode.Manual state
        let state = WaitHandle.close id state

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.waitOne t0 id None state |> ignore)

        exn.Message |> shouldContainText "not registered"

    [<Test>]
    let ``tryWaitOne on a closed event fails loud (use-after-free)`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = WaitHandle.createEvent false EventResetMode.Manual state
        let state = WaitHandle.close id state

        let exn =
            Assert.Throws<System.Exception> (fun () -> WaitHandle.tryWaitOne t0 id state |> ignore)

        exn.Message |> shouldContainText "not registered"

    // ---- property: invariant `Signaled ⇒ WaitQueue = []` ----

    [<RequireQualifiedAccess>]
    type private EventOp =
        | Wait of ThreadId
        | Set
        | Reset

    let private applyEventOp (id : WaitHandleId) (op : EventOp) (state : IlMachineState) : IlMachineState =
        match op with
        | EventOp.Wait tid ->
            // A thread that is already blocked cannot wait again; in
            // PawPrint that would re-park it which is a guest bug. Skip.
            match statusOf tid state with
            | ThreadStatus.BlockedOnWaitHandle _ -> state
            | _ ->
                match WaitHandle.waitOne tid id None state with
                | WaitHandle.WaitOutcome.Acquired s
                | WaitHandle.WaitOutcome.Blocked s -> s
                | WaitHandle.WaitOutcome.AcquiredAbandoned _ ->
                    failwith "Event waitOne unexpectedly produced AcquiredAbandoned"
        | EventOp.Set -> WaitHandle.setEvent id state
        | EventOp.Reset -> WaitHandle.resetEvent id state

    let private runEventScript
        (mode : EventResetMode)
        (initialSignal : bool)
        (threadCount : int)
        (script : EventOp list)
        : EventState
        =
        let threads = [ 0 .. threadCount - 1 ] |> List.map ThreadId
        let state = baseState () |> withThreads threads
        let id, state = WaitHandle.createEvent initialSignal mode state

        let final = script |> List.fold (fun s op -> applyEventOp id op s) state

        eventOf id final

    [<Test>]
    let ``Property: Manual event invariant Signaled implies empty WaitQueue holds across any script`` () : unit =
        let property (PositiveInt threadCountRaw) (NonNegativeInt seedRaw) : bool =
            let threadCount = 1 + (threadCountRaw % 6)
            let rng = System.Random seedRaw
            let scriptLen = 30

            let threads = [ 0 .. threadCount - 1 ] |> List.map ThreadId

            let script =
                [
                    for _ in 1..scriptLen do
                        let r = rng.Next 3

                        if r = 0 then
                            yield EventOp.Wait (List.item (rng.Next threadCount) threads)
                        elif r = 1 then
                            yield EventOp.Set
                        else
                            yield EventOp.Reset
                ]

            let e = runEventScript EventResetMode.Manual false threadCount script
            // Signaled ⇒ WaitQueue = []
            not e.Signaled || e.WaitQueue = []

        Check.One (config, property)

    [<Test>]
    let ``Property: Auto event invariant Signaled implies empty WaitQueue holds across any script`` () : unit =
        let property (PositiveInt threadCountRaw) (NonNegativeInt seedRaw) : bool =
            let threadCount = 1 + (threadCountRaw % 6)
            let rng = System.Random seedRaw
            let scriptLen = 30

            let threads = [ 0 .. threadCount - 1 ] |> List.map ThreadId

            let script =
                [
                    for _ in 1..scriptLen do
                        let r = rng.Next 3

                        if r = 0 then
                            yield EventOp.Wait (List.item (rng.Next threadCount) threads)
                        elif r = 1 then
                            yield EventOp.Set
                        else
                            yield EventOp.Reset
                ]

            let e = runEventScript EventResetMode.Auto false threadCount script
            not e.Signaled || e.WaitQueue = []

        Check.One (config, property)

    // -------------------------------------------------------------------
    // waitMultiple / tryWaitMultiple — the WaitAny / WaitAll state machine
    // -------------------------------------------------------------------

    let private multiAcquired (outcome : WaitHandle.MultiWaitOutcome) : int * bool * IlMachineState =
        match outcome with
        | WaitHandle.MultiWaitOutcome.Acquired (index, abandoned, state) -> index, abandoned, state
        | WaitHandle.MultiWaitOutcome.Blocked _ -> failwith "expected Acquired but got Blocked"
        | WaitHandle.MultiWaitOutcome.Failed _ -> failwith "expected Acquired but got Failed"

    let private multiBlocked (outcome : WaitHandle.MultiWaitOutcome) : IlMachineState =
        match outcome with
        | WaitHandle.MultiWaitOutcome.Blocked state -> state
        | WaitHandle.MultiWaitOutcome.Acquired _ -> failwith "expected Blocked but got Acquired"
        | WaitHandle.MultiWaitOutcome.Failed _ -> failwith "expected Blocked but got Failed"

    /// Every handle in the state, paired with the threads currently queued on
    /// it. The oracle below is stated over this projection.
    let private queues (state : IlMachineState) : (WaitHandleId * ThreadId list) list =
        state.Kernel.WaitHandles
        |> Map.toList
        |> List.map (fun (id, handle) ->
            let queue =
                match handle with
                | WaitHandleState.Semaphore s -> s.WaitQueue
                | WaitHandleState.Mutex m -> m.WaitQueue
                | WaitHandleState.Event e -> e.WaitQueue

            id, queue
        )

    let private isAcquirableBy (thread : ThreadId) (id : WaitHandleId) (state : IlMachineState) : bool =
        match Map.find id state.Kernel.WaitHandles with
        | WaitHandleState.Semaphore s -> s.Count > 0
        | WaitHandleState.Mutex m ->
            match m.Ownership with
            | MutexOwnership.Free _ -> true
            | MutexOwnership.Held (owner, _) -> owner = thread
        | WaitHandleState.Event e -> e.Signaled

    /// The weakened queue invariant that multi-handle wait introduces, stated
    /// as an oracle rather than prose.
    ///
    /// The strong invariant PawPrint used to hold — a signalled handle has an
    /// empty queue — is false once a wait-all waiter can be parked on a handle
    /// it cannot yet use. What survives is: every thread still queued on a
    /// handle that it could otherwise take is a wait-all waiter that is
    /// verifiably unacquirable on at least one of its other handles. Anything
    /// else queued behind an available resource is a lost wakeup.
    let private noLostWakeups (state : IlMachineState) : bool =
        queues state
        |> List.forall (fun (id, queue) ->
            queue
            |> List.forall (fun thread ->
                if not (isAcquirableBy thread id state) then
                    // Nothing to give: staying parked is correct.
                    true
                else
                    match state.ThreadState.[thread].Status with
                    | ThreadStatus.BlockedOnWaitHandles (handles, true, _) ->
                        handles
                        |> List.exists (fun other -> other <> id && not (isAcquirableBy thread other state))
                    | _ -> false
            )
        )

    /// No thread is ever both `Runnable` and sitting in some handle's queue.
    let private noRunnableThreadIsQueued (state : IlMachineState) : bool =
        queues state
        |> List.forall (fun (_, queue) ->
            queue
            |> List.forall (fun thread ->
                match state.ThreadState.[thread].Status with
                | ThreadStatus.BlockedOnWaitHandle _
                | ThreadStatus.BlockedOnWaitHandles _ -> true
                | _ -> false
            )
        )

    [<Test>]
    let ``waitMultiple wait-any reports the smallest signalled index`` () : unit =
        let state = baseState () |> withFramedThreads [ t0 ]
        let a, state = WaitHandle.createEvent false EventResetMode.Manual state
        let b, state = WaitHandle.createEvent true EventResetMode.Manual state
        let c, state = WaitHandle.createEvent true EventResetMode.Manual state

        let index, abandoned, state =
            WaitHandle.waitMultiple t0 [ a ; b ; c ] false None state |> multiAcquired

        index |> shouldEqual 1
        abandoned |> shouldEqual false
        // The fast path stays Runnable and touches no queue.
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable
        queues state |> List.forall (fun (_, q) -> q = []) |> shouldEqual true

    [<Test>]
    let ``waitMultiple wait-any consumes only the handle that satisfied it`` () : unit =
        let state = baseState () |> withFramedThreads [ t0 ]
        let a, state = WaitHandle.createSemaphore 0 4 state
        let b, state = WaitHandle.createSemaphore 3 4 state
        let c, state = WaitHandle.createSemaphore 2 4 state

        let index, _, state =
            WaitHandle.waitMultiple t0 [ a ; b ; c ] false None state |> multiAcquired

        index |> shouldEqual 1
        (semaphoreOf a state).Count |> shouldEqual 0
        (semaphoreOf b state).Count |> shouldEqual 2
        // Untouched: the scan stopped at the first acquirable handle.
        (semaphoreOf c state).Count |> shouldEqual 2

    [<Test>]
    let ``waitMultiple wait-any resolves a duplicated handle to its first index`` () : unit =
        let state = baseState () |> withFramedThreads [ t0 ]
        let a, state = WaitHandle.createEvent false EventResetMode.Manual state
        let b, state = WaitHandle.createEvent true EventResetMode.Manual state

        let index, _, _ =
            WaitHandle.waitMultiple t0 [ a ; b ; b ] false None state |> multiAcquired

        index |> shouldEqual 1

    [<Test>]
    let ``waitMultiple wait-all is atomic: a partial match consumes nothing`` () : unit =
        let state = baseState () |> withFramedThreads [ t0 ]
        let a, state = WaitHandle.createSemaphore 1 1 state
        let b, state = WaitHandle.createSemaphore 1 1 state
        let c, state = WaitHandle.createSemaphore 0 1 state

        let blockedState =
            WaitHandle.waitMultiple t0 [ a ; b ; c ] true None state |> multiBlocked

        // Not one unit was taken, even though two of the three were available.
        (semaphoreOf a blockedState).Count |> shouldEqual 1
        (semaphoreOf b blockedState).Count |> shouldEqual 1
        (semaphoreOf c blockedState).Count |> shouldEqual 0

        statusOf t0 blockedState
        |> shouldEqual (ThreadStatus.BlockedOnWaitHandles ([ a ; b ; c ], true, None))

        // Parked on every named handle, so any of them can wake it.
        queues blockedState
        |> List.forall (fun (_, q) -> q = [ t0 ])
        |> shouldEqual true

    [<Test>]
    let ``waitMultiple wait-all consumes every handle when all are available`` () : unit =
        let state = baseState () |> withFramedThreads [ t0 ]
        let a, state = WaitHandle.createSemaphore 1 1 state
        let b, state = WaitHandle.createSemaphore 2 2 state
        let c, state = WaitHandle.createEvent true EventResetMode.Auto state

        let _, abandoned, state =
            WaitHandle.waitMultiple t0 [ a ; b ; c ] true None state |> multiAcquired

        abandoned |> shouldEqual false
        (semaphoreOf a state).Count |> shouldEqual 0
        (semaphoreOf b state).Count |> shouldEqual 1
        // Auto events are consumed by acquiring.
        (eventOf c state).Signaled |> shouldEqual false

    [<Test>]
    let ``waitMultiple wait-all rejects duplicate handles`` () : unit =
        let state = baseState () |> withFramedThreads [ t0 ]
        let a, state = WaitHandle.createEvent true EventResetMode.Manual state

        match WaitHandle.waitMultiple t0 [ a ; a ] true None state with
        | WaitHandle.MultiWaitOutcome.Failed failedState ->
            // A rejected wait must not have touched anything.
            statusOf t0 failedState |> shouldEqual ThreadStatus.Runnable
            (eventOf a failedState).Signaled |> shouldEqual true
        | other -> failwith $"expected Failed for a duplicated wait-all handle, got %O{other}"

        // The same array is legal for a wait-any.
        match WaitHandle.waitMultiple t0 [ a ; a ] false None state with
        | WaitHandle.MultiWaitOutcome.Acquired (index, _, _) -> index |> shouldEqual 0
        | other -> failwith $"expected a duplicated wait-any to be Acquired, got %O{other}"

    [<Test>]
    let ``tryWaitMultiple never enqueues`` () : unit =
        let state = baseState () |> withFramedThreads [ t0 ]
        let a, state = WaitHandle.createSemaphore 0 1 state
        let b, state = WaitHandle.createEvent false EventResetMode.Manual state

        for waitAll in [ true ; false ] do
            match WaitHandle.tryWaitMultiple t0 [ a ; b ] waitAll state with
            | WaitHandle.MultiTryWaitOutcome.TimedOut timedOutState ->
                statusOf t0 timedOutState |> shouldEqual ThreadStatus.Runnable
                queues timedOutState |> List.forall (fun (_, q) -> q = []) |> shouldEqual true
            | other -> failwith $"expected TimedOut (waitAll = %b{waitAll}), got %O{other}"

    [<Test>]
    let ``a signal wake rewrites the parked wait-any result to its index`` () : unit =
        let state = baseState () |> withFramedThreads [ t0 ]
        let a, state = WaitHandle.createEvent false EventResetMode.Manual state
        let b, state = WaitHandle.createEvent false EventResetMode.Manual state

        let state = WaitHandle.waitMultiple t0 [ a ; b ] false None state |> multiBlocked

        // The interpreter's park-time push: optimistic, and wrong for index 1.
        let state =
            IlMachineState.pushToEvalStack' (EvalStackValue.Int32 WaitHandle.waitObjectZero) t0 state

        let depthAtPark = evalStackDepth t0 state

        let state = WaitHandle.setEvent b state

        statusOf t0 state |> shouldEqual ThreadStatus.Runnable
        waitResultOf t0 state |> shouldEqual (WaitHandle.waitObjectZero + 1)
        // The rewrite must not change how deep the stack is.
        evalStackDepth t0 state |> shouldEqual depthAtPark
        // Dequeued from both handles, not just the one that signalled.
        queues state |> List.forall (fun (_, q) -> q = []) |> shouldEqual true

    [<Test>]
    let ``a signal wake reports a bare success for a parked wait-all`` () : unit =
        let state = baseState () |> withFramedThreads [ t0 ]
        let a, state = WaitHandle.createEvent false EventResetMode.Manual state
        let b, state = WaitHandle.createEvent false EventResetMode.Manual state

        let state = WaitHandle.waitMultiple t0 [ a ; b ] true None state |> multiBlocked

        let state =
            IlMachineState.pushToEvalStack' (EvalStackValue.Int32 WaitHandle.waitObjectZero) t0 state

        // The first signal cannot satisfy the wait-all, so the waiter stays put
        // — and the event stays signalled with a non-empty queue, which is the
        // weakened invariant.
        let state = WaitHandle.setEvent a state

        statusOf t0 state
        |> shouldEqual (ThreadStatus.BlockedOnWaitHandles ([ a ; b ], true, None))

        (eventOf a state).Signaled |> shouldEqual true
        (eventOf a state).WaitQueue |> shouldEqual [ t0 ]
        noLostWakeups state |> shouldEqual true

        let state = WaitHandle.setEvent b state
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable
        // Wait-all reports no index: the OS cannot say which handle it was.
        waitResultOf t0 state |> shouldEqual WaitHandle.waitObjectZero
        queues state |> List.forall (fun (_, q) -> q = []) |> shouldEqual true

    [<Test>]
    let ``an unsatisfiable wait-all waiter is skipped, not blocking the queue`` () : unit =
        // t0 waits on {a, b} but b is empty, so releasing `a` cannot satisfy
        // it. t1 waits on `a` alone, behind t0 in the queue. The release must
        // hand the unit to t1 rather than stall behind t0 — which is what the
        // PAL does, and what stops one wait-all from wedging a shared handle.
        let state = baseState () |> withFramedThreads [ t0 ; t1 ]
        let a, state = WaitHandle.createSemaphore 0 4 state
        let b, state = WaitHandle.createSemaphore 0 4 state

        let state = WaitHandle.waitMultiple t0 [ a ; b ] true None state |> multiBlocked

        let state =
            IlMachineState.pushToEvalStack' (EvalStackValue.Int32 WaitHandle.waitObjectZero) t0 state

        let state = WaitHandle.waitOne t1 a None state |> blocked
        (semaphoreOf a state).WaitQueue |> shouldEqual [ t0 ; t1 ]

        let outcome, state = WaitHandle.releaseSemaphore a 1 state
        outcome |> shouldEqual (Ok 0)

        statusOf t1 state |> shouldEqual ThreadStatus.Runnable

        statusOf t0 state
        |> shouldEqual (ThreadStatus.BlockedOnWaitHandles ([ a ; b ], true, None))

        (semaphoreOf a state).Count |> shouldEqual 0
        (semaphoreOf a state).WaitQueue |> shouldEqual [ t0 ]
        noLostWakeups state |> shouldEqual true

    [<Test>]
    let ``a fresh wait acquires ahead of a parked unsatisfiable wait-all waiter`` () : unit =
        // The documented consequence of skip-don't-block: the fast paths do
        // not consult WaitQueue, so a newly-arriving single waiter takes the
        // unit that the parked wait-all waiter cannot yet use. This matches
        // Win32; pinning it stops a well-meaning "fix" from adding a queue
        // check to the fast path.
        let state = baseState () |> withFramedThreads [ t0 ; t1 ]
        let a, state = WaitHandle.createSemaphore 0 4 state
        let b, state = WaitHandle.createSemaphore 0 4 state

        let state = WaitHandle.waitMultiple t0 [ a ; b ] true None state |> multiBlocked

        let state =
            IlMachineState.pushToEvalStack' (EvalStackValue.Int32 WaitHandle.waitObjectZero) t0 state

        // Nobody is woken: t0 is the only waiter and it is unsatisfiable, so
        // the unit stays in Count.
        let _, state = WaitHandle.releaseSemaphore a 1 state
        (semaphoreOf a state).Count |> shouldEqual 1
        (semaphoreOf a state).WaitQueue |> shouldEqual [ t0 ]

        // t1 arrives afterwards and takes it on the fast path.
        let state = WaitHandle.waitOne t1 a None state |> acquired
        statusOf t1 state |> shouldEqual ThreadStatus.Runnable
        (semaphoreOf a state).Count |> shouldEqual 0

        statusOf t0 state
        |> shouldEqual (ThreadStatus.BlockedOnWaitHandles ([ a ; b ], true, None))

        noLostWakeups state |> shouldEqual true

    [<Test>]
    let ``a satisfiable wait-all waiter is granted atomically by the last release`` () : unit =
        let state = baseState () |> withFramedThreads [ t0 ]
        let a, state = WaitHandle.createSemaphore 0 4 state
        let b, state = WaitHandle.createSemaphore 0 4 state

        let state = WaitHandle.waitMultiple t0 [ a ; b ] true None state |> multiBlocked

        let state =
            IlMachineState.pushToEvalStack' (EvalStackValue.Int32 WaitHandle.waitObjectZero) t0 state

        let _, state = WaitHandle.releaseSemaphore a 1 state

        statusOf t0 state
        |> shouldEqual (ThreadStatus.BlockedOnWaitHandles ([ a ; b ], true, None))

        let _, state = WaitHandle.releaseSemaphore b 1 state

        statusOf t0 state |> shouldEqual ThreadStatus.Runnable
        // Both units consumed by the single grant.
        (semaphoreOf a state).Count |> shouldEqual 0
        (semaphoreOf b state).Count |> shouldEqual 0
        waitResultOf t0 state |> shouldEqual WaitHandle.waitObjectZero
        queues state |> List.forall (fun (_, q) -> q = []) |> shouldEqual true

    [<Test>]
    let ``fireMultipleTimeout dequeues from every handle and reports WAIT_TIMEOUT`` () : unit =
        let state = baseState () |> withFramedThreads [ t0 ]
        let a, state = WaitHandle.createSemaphore 0 1 state
        let b, state = WaitHandle.createEvent false EventResetMode.Manual state

        let state =
            WaitHandle.waitMultiple t0 [ a ; b ] false (Some 100L) state |> multiBlocked

        let state =
            IlMachineState.pushToEvalStack' (EvalStackValue.Int32 WaitHandle.waitObjectZero) t0 state

        let depthAtPark = evalStackDepth t0 state
        let state = WaitHandle.fireMultipleTimeout t0 state

        statusOf t0 state |> shouldEqual ThreadStatus.Runnable
        waitResultOf t0 state |> shouldEqual WaitHandle.waitTimeout
        evalStackDepth t0 state |> shouldEqual depthAtPark
        queues state |> List.forall (fun (_, q) -> q = []) |> shouldEqual true

    /// One step of the randomised multi-primitive script below.
    [<RequireQualifiedAccess>]
    type private MultiOp =
        | WaitAny of thread : int * handles : int list
        | WaitAll of thread : int * handles : int list
        | WaitOne of thread : int * handle : int
        | Release of handle : int
        | SetEvent of handle : int
        | ResetEvent of handle : int

    [<Test>]
    let ``Property: no lost wakeups across randomised mixed single and multi waits`` () : unit =
        // The generalisation of the single-semaphore conservation oracle to a
        // mixed population: several handles of two kinds, threads issuing
        // single-handle waits, wait-anys and wait-alls, interleaved with
        // releases and signals.
        //
        // The oracle is the weakened queue invariant itself. After every step,
        // any thread still parked on a handle it could take must be a wait-all
        // waiter demonstrably blocked on one of its other handles. A release
        // that failed to walk past an unsatisfiable wait-all, a wake that
        // forgot to dequeue from the waiter's other queues, or a grant that
        // left a woken thread queued would each break it.
        let property (PositiveInt threadCountRaw) (NonNegativeInt seedRaw) : bool =
            let threadCount = 1 + (threadCountRaw % 4)
            let rng = System.Random seedRaw
            let threads = [ 0 .. threadCount - 1 ] |> List.map ThreadId

            let state = baseState () |> withFramedThreads threads
            let semA, state = WaitHandle.createSemaphore 0 8 state
            let semB, state = WaitHandle.createSemaphore 0 8 state
            let evtA, state = WaitHandle.createEvent false EventResetMode.Auto state
            let evtB, state = WaitHandle.createEvent false EventResetMode.Manual state
            let handles = [| semA ; semB ; evtA ; evtB |]

            let script =
                [
                    for _ in 1..40 do
                        let thread = rng.Next threadCount

                        match rng.Next 6 with
                        | 0 ->
                            // Wait-any over a random non-empty subset.
                            let count = 1 + rng.Next 3
                            let chosen = [ for _ in 1..count -> rng.Next handles.Length ]
                            yield MultiOp.WaitAny (thread, chosen)
                        | 1 ->
                            // Wait-all needs distinct handles to be legal.
                            let count = 1 + rng.Next 3

                            let chosen =
                                [ 0 .. handles.Length - 1 ]
                                |> List.sortBy (fun _ -> rng.Next ())
                                |> List.truncate count

                            yield MultiOp.WaitAll (thread, chosen)
                        | 2 -> yield MultiOp.WaitOne (thread, rng.Next handles.Length)
                        | 3 -> yield MultiOp.Release (rng.Next 2)
                        | 4 -> yield MultiOp.SetEvent (2 + rng.Next 2)
                        | _ -> yield MultiOp.ResetEvent (2 + rng.Next 2)
                ]

            let isRunnable (thread : ThreadId) (state : IlMachineState) : bool =
                match state.ThreadState.[thread].Status with
                | ThreadStatus.Runnable -> true
                | _ -> false

            let parkPush (tid : ThreadId) (state : IlMachineState) : IlMachineState =
                IlMachineState.pushToEvalStack' (EvalStackValue.Int32 WaitHandle.waitObjectZero) tid state

            let step (state : IlMachineState) (op : MultiOp) : IlMachineState =
                // Only a Runnable thread can issue a wait; a parked one is not
                // executing IL. Skipping keeps the script well-formed rather
                // than modelling an impossible interleaving.
                let afterOp =
                    match op with
                    | MultiOp.WaitAny (thread, chosen) ->
                        let tid = List.item thread threads

                        if not (isRunnable tid state) then
                            state
                        else

                        let chosenHandles = chosen |> List.map (fun i -> handles.[i])

                        match WaitHandle.waitMultiple tid chosenHandles false None state with
                        | WaitHandle.MultiWaitOutcome.Acquired (_, _, state) -> state
                        | WaitHandle.MultiWaitOutcome.Blocked state -> parkPush tid state
                        | WaitHandle.MultiWaitOutcome.Failed state -> state
                    | MultiOp.WaitAll (thread, chosen) ->
                        let tid = List.item thread threads

                        if not (isRunnable tid state) then
                            state
                        else

                        let chosenHandles = chosen |> List.map (fun i -> handles.[i])

                        match WaitHandle.waitMultiple tid chosenHandles true None state with
                        | WaitHandle.MultiWaitOutcome.Acquired (_, _, state) -> state
                        | WaitHandle.MultiWaitOutcome.Blocked state -> parkPush tid state
                        | WaitHandle.MultiWaitOutcome.Failed state -> state
                    | MultiOp.WaitOne (thread, handle) ->
                        let tid = List.item thread threads

                        if not (isRunnable tid state) then
                            state
                        else

                        match WaitHandle.waitOne tid handles.[handle] None state with
                        | WaitHandle.WaitOutcome.Acquired state
                        | WaitHandle.WaitOutcome.AcquiredAbandoned state -> state
                        | WaitHandle.WaitOutcome.Blocked state -> parkPush tid state
                    | MultiOp.Release handle ->
                        // A release that would breach the maximum is refused
                        // and leaves the state alone, which is fine here.
                        let _, state = WaitHandle.releaseSemaphore handles.[handle] 1 state
                        state
                    | MultiOp.SetEvent handle -> WaitHandle.setEvent handles.[handle] state
                    | MultiOp.ResetEvent handle -> WaitHandle.resetEvent handles.[handle] state

                if not (noLostWakeups afterOp) then
                    failwith $"lost wakeup after %O{op}: %A{queues afterOp}"

                if not (noRunnableThreadIsQueued afterOp) then
                    failwith $"a Runnable thread is still queued after %O{op}: %A{queues afterOp}"

                afterOp

            script |> List.fold step state |> ignore
            true

        Check.One (config, property)
