namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// Focused tests for the kernel-owned signal-dispatch thread that PawPrint
/// spawns on the first call to
/// `SystemNative_InitializeTerminalAndSignalHandling`. The dispatcher mirrors
/// real CoreCLR's `SignalHandlerLoop` pthread: it exists permanently from
/// init time, the scheduler never picks it while it is Parked, and it has no
/// managed `Thread` heap mirror. These tests pin down that structural shape.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSignalDispatcherThread =

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private baseState () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()
        IlMachineState.initial loggerFactory ImmutableArray.Empty corelib

    /// Frameless `ThreadState` used to populate the scheduler-test states
    /// below with `Runnable` threads we don't actually want to step. Mirrors
    /// the pattern in `TestSyncBlockMonitor`.
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

    [<Test>]
    let ``empty SignalState has no signal thread`` () : unit =
        let empty : SignalState<ThreadId, SignalHandler> = SignalState.empty

        empty |> SignalState.signalThread |> shouldEqual None

    [<Test>]
    let ``markInitialized records the dispatcher ThreadId`` () : unit =
        let dispatcher = ThreadId 7
        let empty : SignalState<ThreadId, SignalHandler> = SignalState.empty

        empty
        |> SignalState.markInitialized dispatcher
        |> SignalState.signalThread
        |> shouldEqual (Some dispatcher)

    [<Test>]
    let ``allocateParkedThread mints a Parked, frameless thread`` () : unit =
        let state, thread = baseState () |> IlMachineState.allocateParkedThread

        let ts =
            state.ThreadState
            |> Map.tryFind thread
            |> Option.defaultWith (fun () -> failwith "expected parked thread to be present in ThreadState")

        ts.Status |> shouldEqual ThreadStatus.Parked
        // A frameless thread carries an empty MethodStates map and a sentinel
        // ActiveMethodState that is not live in the map. Any code path that
        // dereferences the active frame on a Parked thread must crash loudly
        // rather than silently producing garbage. `MethodState` does not
        // satisfy structural equality (its embedded `MethodInfo` carries
        // reference-equality payloads), so we assert emptiness via `Count`.
        ts.MethodStates.Count |> shouldEqual 0
        ts.MethodStates.ContainsKey ts.ActiveMethodState |> shouldEqual false

    [<Test>]
    let ``allocateParkedThread does not register a managed Thread object`` () : unit =
        // The dispatcher is kernel-owned, not constructed via the managed
        // `new Thread(...)` path. There is no `Thread` heap object for guest
        // code to observe, so `ManagedThreadObjects` must not be touched.
        let initial = baseState ()
        let state, thread = initial |> IlMachineState.allocateParkedThread

        state.ManagedThreadObjects |> Map.containsKey thread |> shouldEqual false

        // The pre-existing managed-thread mapping is preserved bit-for-bit
        // (allocation should only add to ThreadState, not touch the managed
        // mirror).
        state.ManagedThreadObjects |> shouldEqual initial.ManagedThreadObjects

    [<Test>]
    let ``allocateParkedThread mints distinct ids on successive calls`` () : unit =
        let state0 = baseState ()
        let state1, t1 = state0 |> IlMachineState.allocateParkedThread
        let state2, t2 = state1 |> IlMachineState.allocateParkedThread

        t1 |> shouldNotEqual t2
        state2.ThreadState |> Map.containsKey t1 |> shouldEqual true
        state2.ThreadState |> Map.containsKey t2 |> shouldEqual true

    [<Test>]
    let ``scheduler skips Parked threads in favour of Runnable ones`` () : unit =
        // Put a Runnable thread alongside a Parked one; the scheduler must
        // ignore the Parked slot and return the Runnable id.
        let runnable = ThreadId 0
        let parked = ThreadId 1

        let state =
            { baseState () with
                ThreadState =
                    Map.ofList
                        [
                            runnable, stubThreadState ThreadStatus.Runnable
                            parked, stubThreadState ThreadStatus.Parked
                        ]
            }

        Scheduler.chooseNext (ThreadId -1) state |> snd |> shouldEqual (Some runnable)

    [<Test>]
    let ``scheduler returns None when only Parked threads exist`` () : unit =
        // A program whose only live threads are Parked is making no
        // progress; the scheduler must return None and let the driver decide
        // (deadlock vs. NormalExit on entry-thread termination). Pin down the
        // None outcome so a future scheduler change doesn't quietly start
        // picking Parked.
        let parkedA = ThreadId 0
        let parkedB = ThreadId 1

        let state =
            { baseState () with
                ThreadState =
                    Map.ofList
                        [
                            parkedA, stubThreadState ThreadStatus.Parked
                            parkedB, stubThreadState ThreadStatus.Parked
                        ]
            }

        Scheduler.chooseNext (ThreadId -1) state |> snd |> shouldEqual None
