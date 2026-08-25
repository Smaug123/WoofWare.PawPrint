namespace WoofWare.PosixKernel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The task table, exercised directly rather than through a client.
///
/// It is generic in the task name for the same reason `SignalState` is: naming a
/// scheduling entity is the client's business. These rows use `int` as the name,
/// which is the point — nothing here knows what a `ThreadId` is.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixTaskTable =

    let private empty : Map<int, UnixTaskState> = Map.empty

    let private withTask (name : int) (cpu : int) (tasks : Map<int, UnixTaskState>) : Map<int, UnixTaskState> =
        UnixTaskTable.register name (CpuId cpu) (OsThreadId (uint32 name + 1u)) tasks

    [<Test>]
    let ``a registered task is readable`` () : unit =
        let tasks = empty |> withTask 7 3

        UnixTaskTable.cpuOf 7 tasks |> shouldEqual (CpuId 3)
        UnixTaskTable.osThreadIdOf 7 tasks |> shouldEqual (OsThreadId 8u)
        UnixTaskTable.parkedSocketWaitFor 7 tasks |> shouldEqual None

    [<Test>]
    let ``a name that was never registered is refused loudly`` () : unit =
        let exn =
            Assert.Throws<exn> (fun () -> UnixTaskTable.get 7 empty |> ignore<UnixTaskState>)

        exn.Message |> shouldContainText "names no task"

    [<Test>]
    let ``registering one name twice is refused`` () : unit =
        // Re-registration would discard the first registration's processor and OS
        // thread id, which is how two tasks end up sharing an id.
        let tasks = empty |> withTask 7 3

        let exn =
            Assert.Throws<exn> (fun () -> withTask 7 5 tasks |> ignore<Map<int, UnixTaskState>>)

        exn.Message |> shouldContainText "already names a task"

    [<Test>]
    let ``parking and releasing leave the rest of the task alone`` () : unit =
        // On a task that is *not* on processor 0, so that "left alone" and "reset
        // to zero" are distinguishable.
        let tasks = empty |> withTask 7 3

        let wait : ParkedSocketWait =
            {
                Port = OpenFileDescriptionId 5L
                MaxEvents = 8
            }

        let parked = UnixTaskTable.withParkedSocketWait 7 (Some wait) tasks
        UnixTaskTable.parkedSocketWaitFor 7 parked |> shouldEqual (Some wait)
        UnixTaskTable.cpuOf 7 parked |> shouldEqual (CpuId 3)
        UnixTaskTable.osThreadIdOf 7 parked |> shouldEqual (OsThreadId 8u)

        let released = UnixTaskTable.withParkedSocketWait 7 None parked
        UnixTaskTable.parkedSocketWaitFor 7 released |> shouldEqual None
        UnixTaskTable.cpuOf 7 released |> shouldEqual (CpuId 3)

    [<Test>]
    let ``reconcile is silent when the table matches`` () : unit =
        let tasks = empty |> withTask 1 0 |> withTask 2 1

        UnixTaskTable.reconcile (Set.ofList [ 1 ; 2 ]) tasks |> shouldEqual ([], [])

    [<Test>]
    let ``reconcile reports a live task the table has no entry for`` () : unit =
        let tasks = empty |> withTask 1 0

        UnixTaskTable.reconcile (Set.ofList [ 1 ; 2 ]) tasks |> shouldEqual ([ 2 ], [])

    [<Test>]
    let ``reconcile reports an entry no live task claims`` () : unit =
        let tasks = empty |> withTask 1 0 |> withTask 2 1

        UnixTaskTable.reconcile (Set.ofList [ 1 ]) tasks |> shouldEqual ([], [ 2 ])

    [<Test>]
    let ``reconcile reports both directions at once`` () : unit =
        // The row that separates "reports both" from "reports whichever it
        // happens to check first".
        let tasks = empty |> withTask 1 0 |> withTask 3 1

        UnixTaskTable.reconcile (Set.ofList [ 1 ; 2 ]) tasks
        |> shouldEqual ([ 2 ], [ 3 ])
