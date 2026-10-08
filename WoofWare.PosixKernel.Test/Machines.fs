namespace WoofWare.PosixKernel.Test

open FsUnitTyped
open WoofWare.PosixKernel

/// Several processes on one `SimulatedMachine`, for a test that drives each
/// through its own view.
[<RequireQualifiedAccess>]
module internal Machines =

    /// A launch of a process on `platform` with the three piped standard
    /// streams and one task, 0, on processor 0.
    let launchOn (platform : SimulatedUnixPlatform) : ProcessLaunch<int> =
        Launched.launch platform UnixSystem.pipedStandardStreams 0 (CpuId 0)

    /// A machine of `platform` with `count` processes, the first booted and
    /// the rest launched, each with tasks 0 to `tasks - 1`, and their process
    /// IDs in that order.
    let withTasks
        (platform : SimulatedUnixPlatform)
        (count : int)
        (tasks : int)
        : ProcessId list * SimulatedMachine<int, string>
        =
        let first =
            UnixSystem.initial<int, string> platform
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

        let pids, machine =
            (([ UnixSystem.processId first ], SimulatedMachine.ofSystem first), [ 2..count ])
            ||> List.fold (fun (pids, machine) _ ->
                match SimulatedMachine.launch (launchOn platform) machine with
                | Ok (pid, machine) -> pids @ [ pid ], machine
                | Error refusal -> failwith $"launch: %s{ProcessCreationRefusal.describe refusal}"
            )

        let machine =
            (machine, pids)
            ||> List.fold (fun machine pid ->
                match
                    SimulatedMachine.inView
                        pid
                        (fun view -> (), ([ 1 .. tasks - 1 ], view) ||> List.foldBack Tasks.ensure)
                        machine
                with
                | Some ((), machine) -> machine
                | None -> failwith $"no process %O{pid}"
            )

        pids, machine

    /// `withTasks` with tasks 0 to 4 in each process, as `KeventWorld`'s
    /// helpers expect.
    let ofCount (platform : SimulatedUnixPlatform) (count : int) : ProcessId list * SimulatedMachine<int, string> =
        withTasks platform count 5

    /// The view of `pid`, which must be on `machine`.
    let viewOf (pid : ProcessId) (machine : SimulatedMachine<int, string>) : UnixSystem<int, string> =
        match SimulatedMachine.focus pid machine with
        | Some view -> view
        | None -> failwith $"no process %O{pid}"

    /// `f` in the view of `pid`, written back.
    let inProcess
        (pid : ProcessId)
        (f : UnixSystem<int, string> -> 'a * UnixSystem<int, string>)
        (machine : SimulatedMachine<int, string>)
        : 'a * SimulatedMachine<int, string>
        =
        match SimulatedMachine.inView pid f machine with
        | Some answered -> answered
        | None -> failwith $"no process %O{pid}"

    /// `f` in the view of `pid`, written back, for an `f` that answers nothing.
    let doIn
        (pid : ProcessId)
        (f : UnixSystem<int, string> -> UnixSystem<int, string>)
        (machine : SimulatedMachine<int, string>)
        : SimulatedMachine<int, string>
        =
        inProcess pid (fun view -> (), f view) machine |> snd

    /// Every invariant of `machine` holds: the machine's own, each view's, each
    /// process's descriptor table's as one view of it sees it, and the
    /// filesystem's.
    let assertClean (machine : SimulatedMachine<int, string>) : unit =
        SimulatedMachine.checkInvariants machine |> shouldEqual []

        for pid in SimulatedMachine.processIds machine do
            let view = viewOf pid machine
            UnixSystem.checkInvariants view |> shouldEqual []

            FileDescriptorRegistry.checkInvariants (UnixSystem.fileDescriptors view)
            |> shouldEqual []

            VirtualFileSystem.checkInvariants (ObjectLifetime.pinnedInodes view) view.Machine.FileSystem
            |> shouldEqual []

    let platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]
