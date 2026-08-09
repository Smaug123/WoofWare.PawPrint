namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Pins the scheduler contract for `WhatWeDid.VoluntaryYield`: the yielder made
/// forward progress, so any thread parked `BlockedOnClassInit` on the yielder
/// must be woken to re-check its blocker. This must be identical to
/// `WhatWeDid.Executed`'s treatment in `Scheduler.onStepOutcome`. A future
/// refactor that tightens VoluntaryYield's policy must not silently drop the
/// wake — at minimum because today's `Yielded` arm in `AbstractMachine.fs`
/// follows a successful cctor with a VoluntaryYield from a managed sub-call
/// path, and dropping the wake would deadlock any thread parked behind the
/// yielder's class init.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSchedulerVoluntaryYield =

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private baseState () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()
        IlMachineState.initial loggerFactory ImmutableArray.Empty corelib

    /// Frame-less stub thread state: `Scheduler.onStepOutcome` only touches `Status`,
    /// so we never need a real method frame. The sentinel FrameId would crash loudly
    /// if any code path started dereferencing it, which is the right response if the
    /// scheduler ever starts touching frames.
    let private stubThreadState (status : ThreadStatus) : ThreadState =
        {
            MethodStates = Map.empty
            YieldDebt = Set.empty
            NextFrameId = 0
            ActiveMethodState = FrameId -1
            Status = status
            IsBackground = false
            Name = None
            Cpu = CpuId 0
            // Inert here: a frameless stub cannot execute the `SystemNative_*OSThreadId`
            // P/Invoke that reads it. Do not reuse this literal for a stub standing in
            // for more than one thread -- guest OS thread ids must be distinct.
            OsThreadId = OsThreadId 1u
        }

    let private withThreads (threads : (ThreadId * ThreadStatus) list) (state : IlMachineState) : IlMachineState =
        let threadMap =
            threads
            |> List.map (fun (tid, status) -> tid, stubThreadState status)
            |> Map.ofList

        { state with
            ThreadState = threadMap
        }

    let private statusOf (thread : ThreadId) (state : IlMachineState) : ThreadStatus = state.ThreadState.[thread].Status

    let private t0 = ThreadId 0
    let private t1 = ThreadId 1
    let private t2 = ThreadId 2

    [<Test>]
    let ``VoluntaryYield wakes threads BlockedOnClassInit on the yielder`` () : unit =
        let state =
            baseState ()
            |> withThreads
                [
                    t0, ThreadStatus.Runnable
                    t1, ThreadStatus.BlockedOnClassInit t0
                    t2, ThreadStatus.BlockedOnClassInit t0
                ]

        let after = Scheduler.onStepOutcome t0 (WhatWeDid.VoluntaryYield false) state

        // Yielder's own status is untouched — VoluntaryYield is a hint, not a self-block.
        statusOf t0 after |> shouldEqual ThreadStatus.Runnable
        // Both threads parked behind t0's class init are woken; identical to Executed.
        statusOf t1 after |> shouldEqual ThreadStatus.Runnable
        statusOf t2 after |> shouldEqual ThreadStatus.Runnable

    [<Test>]
    let ``VoluntaryYield does not wake threads BlockedOnClassInit on someone else`` () : unit =
        let state =
            baseState ()
            |> withThreads
                [
                    t0, ThreadStatus.Runnable
                    t1, ThreadStatus.Runnable
                    // t2 is blocked on t1's class init, not t0's. The yielder is t0, so this
                    // must remain blocked — the wake-on-yield policy is keyed to the yielder.
                    t2, ThreadStatus.BlockedOnClassInit t1
                ]

        let after = Scheduler.onStepOutcome t0 (WhatWeDid.VoluntaryYield false) state

        statusOf t0 after |> shouldEqual ThreadStatus.Runnable
        statusOf t1 after |> shouldEqual ThreadStatus.Runnable
        statusOf t2 after |> shouldEqual (ThreadStatus.BlockedOnClassInit t1)

    [<Test>]
    let ``VoluntaryYield wakes match Executed exactly, and only the wakes`` () : unit =
        // VoluntaryYield used to be *wholly* identical to Executed; it no longer is, because
        // a yield now also charges the yielder a `YieldDebt`. What must stay identical is the
        // class-init wake behaviour: yielding is still forward progress, so the same threads
        // wake either way. Pinning both halves separately is the point — the wake logic must
        // not drift, and the debt must be the *only* difference, so a future change that
        // (say) parked the yielder would fail here rather than passing a statuses-only check.
        //
        // Snapshot all three threads' statuses to keep the wake assertion total rather than
        // spot-checking.
        let state =
            baseState ()
            |> withThreads
                [
                    t0, ThreadStatus.Runnable
                    t1, ThreadStatus.BlockedOnClassInit t0
                    t2, ThreadStatus.BlockedOnClassInit t1
                ]

        let afterYield = Scheduler.onStepOutcome t0 (WhatWeDid.VoluntaryYield false) state
        let afterExecuted = Scheduler.onStepOutcome t0 WhatWeDid.Executed state

        let statuses (s : IlMachineState) =
            [ t0 ; t1 ; t2 ] |> List.map (fun tid -> statusOf tid s)

        statuses afterYield |> shouldEqual (statuses afterExecuted)

        // The debt names t1: it was woken by this very step, so it is part of the run queue
        // the yielder goes to the back of. t2 is still blocked behind t1 and so is not owed a
        // turn. Executed charges nothing.
        let debtOf (tid : ThreadId) (s : IlMachineState) : Set<ThreadId> = s.ThreadState.[tid].YieldDebt

        debtOf t0 afterYield |> shouldEqual (Set.ofList [ t1 ])
        debtOf t0 afterExecuted |> shouldEqual Set.empty

        // Nobody else is charged anything, either way.
        for tid in [ t1 ; t2 ] do
            debtOf tid afterYield |> shouldEqual Set.empty
            debtOf tid afterExecuted |> shouldEqual Set.empty

    [<Test>]
    let ``onWorkerSpawned treats VoluntaryYield init outcome as Runnable`` () : unit =
        // Cross-check the spawn path: `onWorkerSpawned` is fed `WhatWeDid` from the
        // worker's class-init pump and must keep the worker Runnable on VoluntaryYield
        // (the worker is free to run; the hint doesn't park it). Today's
        // `ensureTypeInitialised` doesn't actually produce VoluntaryYield, but the
        // match in `onWorkerSpawned` lists the variant explicitly to document the
        // intended treatment; pin that here so the documentation doesn't drift.
        let state =
            baseState ()
            |> withThreads [ t0, ThreadStatus.Runnable ; t1, ThreadStatus.Runnable ]

        let after = Scheduler.onWorkerSpawned t1 (WhatWeDid.VoluntaryYield false) state

        statusOf t1 after |> shouldEqual ThreadStatus.Runnable
