namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Pins `ThreadState.classInitWaitChainReaches`, the "any thread waiting for this thread"
/// half of ECMA-335 II.10.5.3.3 step 2.2.1: a thread about to park `BlockedOnClassInit` on a
/// holder must first follow the holder's own `BlockedOnClassInit` chain, and proceed instead
/// of parking if that chain leads back to itself.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestClassInitWaitChain =

    /// Frame-less stub: the walk reads only `Status`.
    let private stubThreadState (status : ThreadStatus) : ThreadState =
        {
            MethodStates = Map.empty
            YieldDebt = Set.empty
            NextFrameId = 0
            ActiveMethodState = FrameId -1
            Status = status
            IsBackground = false
            IsRaisingForeignException = false
            Name = None
        }

    let private threads (statuses : (ThreadId * ThreadStatus) list) : Map<ThreadId, ThreadState> =
        statuses
        |> List.map (fun (tid, status) -> tid, stubThreadState status)
        |> Map.ofList

    let private t0 = ThreadId 0
    let private t1 = ThreadId 1
    let private t2 = ThreadId 2
    let private t3 = ThreadId 3

    [<Test>]
    let ``a holder that is running free does not reach the asker`` () : unit =
        let map = threads [ t0, ThreadStatus.Runnable ; t1, ThreadStatus.Runnable ]
        ThreadState.classInitWaitChainReaches t0 t1 map |> shouldEqual false

    [<Test>]
    let ``a holder parked directly on the asker reaches it`` () : unit =
        let map =
            threads [ t0, ThreadStatus.Runnable ; t1, ThreadStatus.BlockedOnClassInit t0 ]

        ThreadState.classInitWaitChainReaches t0 t1 map |> shouldEqual true

    [<Test>]
    let ``a chain through a third thread reaches the asker`` () : unit =
        let map =
            threads
                [
                    t0, ThreadStatus.Runnable
                    t1, ThreadStatus.BlockedOnClassInit t2
                    t2, ThreadStatus.BlockedOnClassInit t0
                ]

        ThreadState.classInitWaitChainReaches t0 t1 map |> shouldEqual true

    [<Test>]
    let ``a chain that ends on a thread running free does not reach the asker`` () : unit =
        let map =
            threads
                [
                    t0, ThreadStatus.Runnable
                    t1, ThreadStatus.BlockedOnClassInit t2
                    t2, ThreadStatus.BlockedOnClassInit t3
                    t3, ThreadStatus.Runnable
                ]

        ThreadState.classInitWaitChainReaches t0 t1 map |> shouldEqual false

    [<Test>]
    let ``only BlockedOnClassInit links are followed`` () : unit =
        // The holder is waiting for the asker, but on a join rather than on class
        // initialisation. CoreCLR's `DeadlockAwareLock` sees only its own lock kind, so the
        // asker parks; the join is not a class-initialisation wait.
        let map =
            threads [ t0, ThreadStatus.Runnable ; t1, ThreadStatus.BlockedOnJoin (t0, None) ]

        ThreadState.classInitWaitChainReaches t0 t1 map |> shouldEqual false

    [<Test>]
    let ``the asker's own status is not consulted`` () : unit =
        // Whatever the asker is recorded as, the question is about the holder's chain.
        let map =
            threads [ t0, ThreadStatus.BlockedOnClassInit t1 ; t1, ThreadStatus.Runnable ]

        ThreadState.classInitWaitChainReaches t0 t1 map |> shouldEqual false

    [<Test>]
    let ``a cycle that excludes the asker is a logic error`` () : unit =
        let map =
            threads
                [
                    t0, ThreadStatus.Runnable
                    t1, ThreadStatus.BlockedOnClassInit t2
                    t2, ThreadStatus.BlockedOnClassInit t1
                ]

        let exn =
            Assert.Throws<System.Exception> (fun () -> ThreadState.classInitWaitChainReaches t0 t1 map |> ignore)

        exn.Message |> shouldContainText "cycle that does not include"

    [<Test>]
    let ``a chain that leaves the thread table is a logic error`` () : unit =
        let map =
            threads [ t0, ThreadStatus.Runnable ; t1, ThreadStatus.BlockedOnClassInit t2 ]

        let exn =
            Assert.Throws<System.Exception> (fun () -> ThreadState.classInitWaitChainReaches t0 t1 map |> ignore)

        exn.Message |> shouldContainText "has no thread state"

    /// A status that ends a chain, drawn from the variants that do so.
    let private freeStatusGen : Gen<ThreadStatus> =
        Gen.elements
            [
                ThreadStatus.Runnable
                ThreadStatus.Terminated
                ThreadStatus.NotStarted
                ThreadStatus.BlockedOnSleep None
                ThreadStatus.BlockedOnJoin (ThreadId 0, None)
                ThreadStatus.BlockedInSyscall
            ]

    /// A thread table in which every `BlockedOnClassInit` link other than the asker's own
    /// points at a lower-numbered thread, so the links form a forest and every chain
    /// terminates. The asker's own link may point anywhere: the walk must never follow it.
    /// Returns the asker, the holder, and the table.
    let private forestGen : Gen<ThreadId * ThreadId * Map<ThreadId, ThreadState>> =
        gen {
            let! count = Gen.choose (1, 8)
            let! asker = Gen.choose (0, count - 1)
            let! holder = Gen.choose (0, count - 1)

            let statusGen (i : int) : Gen<ThreadStatus> =
                if i = asker then
                    Gen.oneof
                        [
                            freeStatusGen
                            Gen.choose (0, count - 1)
                            |> Gen.map (fun j -> ThreadStatus.BlockedOnClassInit (ThreadId j))
                        ]
                elif i = 0 then
                    freeStatusGen
                else
                    Gen.oneof
                        [
                            freeStatusGen
                            Gen.choose (0, i - 1)
                            |> Gen.map (fun j -> ThreadStatus.BlockedOnClassInit (ThreadId j))
                        ]

            let! statuses = List.init count statusGen |> Gen.sequenceToList

            let table = statuses |> List.mapi (fun i s -> ThreadId i, s) |> threads

            return ThreadId asker, ThreadId holder, table
        }

    /// Reference answer: the set of threads reachable from `holder` by repeatedly following
    /// `BlockedOnClassInit` links, stopping at `asker`, contains `asker`. Computed by
    /// enumerating the whole reachable set rather than by the implementation's early-exit
    /// walk, so the two can disagree.
    let private reachableFrom (asker : ThreadId) (holder : ThreadId) (table : Map<ThreadId, ThreadState>) : bool =
        let rec go (frontier : ThreadId list) (seen : Set<ThreadId>) : Set<ThreadId> =
            match frontier with
            | [] -> seen
            | current :: rest ->
                if seen.Contains current || current = asker then
                    go rest (seen.Add current)
                else
                    match table.[current].Status with
                    | ThreadStatus.BlockedOnClassInit next -> go (next :: rest) (seen.Add current)
                    | _ -> go rest (seen.Add current)

        (go [ holder ] Set.empty).Contains asker

    [<Test>]
    let ``on a forest of waits the walk agrees with the reachable set`` () : unit =
        let property ((asker, holder, table) : ThreadId * ThreadId * Map<ThreadId, ThreadState>) : unit =
            ThreadState.classInitWaitChainReaches asker holder table
            |> shouldEqual (reachableFrom asker holder table)

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 500, Prop.forAll (Arb.fromGen forestGen) property)

    [<Test>]
    let ``the asker as its own holder reaches itself`` () : unit =
        // Callers handle same-thread re-entry before asking, but the answer must still be
        // the truthful one if they do ask.
        let map = threads [ t0, ThreadStatus.Runnable ]
        ThreadState.classInitWaitChainReaches t0 t0 map |> shouldEqual true
