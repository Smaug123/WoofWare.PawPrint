namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// Several processes on one `SimulatedMachine`, each making its syscalls in its
/// own view: the kernel chooses each later process's ID, the processes share
/// the machine's ports, filesystem and pipes and nothing of each other's own
/// state, and every invariant holds after every call.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMultiProcess =

    let private context : string = "TestMultiProcess"

    let private launchOn (platform : SimulatedUnixPlatform) : ProcessLaunch<int> =
        Launched.launch platform UnixSystem.pipedStandardStreams 0 (CpuId 0)

    /// A machine of `platform` with `count` processes, the first booted and
    /// the rest launched, and their process IDs in that order.
    let private machineOf
        (platform : SimulatedUnixPlatform)
        (count : int)
        : ProcessId list * SimulatedMachine<int, string>
        =
        let first =
            UnixSystem.initial<int, string> platform
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

        let machine = SimulatedMachine.ofSystem first

        ((([ UnixSystem.processId first ], machine)), [ 2..count ])
        ||> List.fold (fun (pids, machine) _ ->
            match SimulatedMachine.launch (launchOn platform) machine with
            | Ok (pid, machine) -> pids @ [ pid ], machine
            | Error refusal -> failwith $"launch: %s{ProcessCreationRefusal.describe refusal}"
        )

    let private viewOf (pid : ProcessId) (machine : SimulatedMachine<int, string>) : UnixSystem<int, string> =
        match SimulatedMachine.focus pid machine with
        | Some view -> view
        | None -> failwith $"no process %O{pid}"

    /// `f` in the view of `pid`, written back.
    let private inProcess
        (pid : ProcessId)
        (f : UnixSystem<int, string> -> 'a * UnixSystem<int, string>)
        (machine : SimulatedMachine<int, string>)
        : 'a * SimulatedMachine<int, string>
        =
        match SimulatedMachine.inView pid f machine with
        | Some answered -> answered
        | None -> failwith $"no process %O{pid}"

    let private fdsOf (view : UnixSystem<int, string>) : Set<int> =
        FileDescriptorRegistry.fds (UnixSystemState.fileDescriptors view)
        |> Map.keys
        |> Set.ofSeq

    let private assertClean (machine : SimulatedMachine<int, string>) : unit =
        SimulatedMachine.checkInvariants machine |> shouldEqual []

        for pid in SimulatedMachine.processIds machine do
            let view = viewOf pid machine
            // A view of a machine with other processes checks only what one
            // process can check truthfully, which must hold too.
            UnixSystem.checkInvariants view |> shouldEqual []

            VirtualFileSystem.checkInvariants (ObjectLifetime.pinnedInodes view) view.Machine.FileSystem
            |> shouldEqual []

    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    // ------------------------------------------------------------- process IDs

    [<Test>]
    let ``Linux mints a later process's ID from the thread ID counter, and its leader's thread ID is it`` () : unit =
        let platform = SimulatedUnixPlatform.linuxX64

        let first =
            UnixSystem.initial<int, string> platform
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

        // A thread of the first process takes the next thread ID, so the
        // second process gets the one after.
        let worker, first =
            match UnixTaskLifecycle.spawn 0 1 (CpuId 0) first with
            | Ok spawned -> spawned
            | Error error -> failwith $"spawn: %O{error}"

        OsThreadId.toUInt64 worker |> shouldEqual 4243UL

        let pid, machine =
            match SimulatedMachine.launch (launchOn platform) (SimulatedMachine.ofSystem first) with
            | Ok launched -> launched
            | Error refusal -> failwith $"launch: %s{ProcessCreationRefusal.describe refusal}"

        ProcessId.toInt32 pid |> shouldEqual 4244

        let view = viewOf pid machine
        UnixSystem.processId view |> shouldEqual pid

        UnixTaskTable.osThreadIdOf 0 view.Tasks
        |> OsThreadId.toUInt64
        |> shouldEqual 4244UL

        SimulatedMachine.processIds machine
        |> shouldEqual (Set.ofList [ UnixSystem.processId first ; pid ])

        assertClean machine

    [<Test>]
    let ``Darwin mints a later process's ID from a counter of its own, and its leader's thread ID from the thread counter``
        ()
        : unit
        =
        let platform = SimulatedUnixPlatform.macOsArm64

        let first =
            UnixSystem.initial<int, string> platform
            |> Launched.processId (ProcessId.parseOrFail context 500)
            |> Launched.leaderThreadId 9000UL
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

        let pids, machine =
            ((([], SimulatedMachine.ofSystem first)), [ 1..2 ])
            ||> List.fold (fun (pids, machine) _ ->
                match SimulatedMachine.launch (launchOn platform) machine with
                | Ok (pid, machine) -> pids @ [ pid ], machine
                | Error refusal -> failwith $"launch: %s{ProcessCreationRefusal.describe refusal}"
            )

        pids |> List.map ProcessId.toInt32 |> shouldEqual [ 501 ; 502 ]

        pids
        |> List.map (fun pid -> UnixTaskTable.osThreadIdOf 0 (viewOf pid machine).Tasks |> OsThreadId.toUInt64)
        |> shouldEqual [ 9001UL ; 9002UL ]

        assertClean machine

    [<Test>]
    let ``Darwin refuses a process ID at PID_MAX, and stops at it rather than wrapping`` () : unit =
        let platform = SimulatedUnixPlatform.macOsArm64

        UnixSystem.initial<int, string> platform
        |> UnixBootImage.withProcessId (ProcessId.parseOrFail context ProcessIdTable.darwinPidMax)
        |> Result.map ignore
        |> shouldEqual (
            Error (
                ProcessIdRefusal.NotBelowDarwinPidMax (
                    ProcessId.parseOrFail context ProcessIdTable.darwinPidMax,
                    ProcessIdTable.darwinPidMax
                )
            )
        )

        let first =
            UnixSystem.initial<int, string> platform
            |> Launched.processId (ProcessId.parseOrFail context (ProcessIdTable.darwinPidMax - 1))
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

        SimulatedMachine.launch (launchOn platform) (SimulatedMachine.ofSystem first)
        |> Result.map ignore
        |> shouldEqual (Error (ProcessCreationRefusal.DarwinPidMaxReached ProcessIdTable.darwinPidMax))

    [<Test>]
    let ``Linux refuses a launch once every process ID it would hand out is a live thread's`` () : unit =
        let platform = SimulatedUnixPlatform.linuxX64

        // pid_max 301 leaves 300 alone free once the first process is 299.
        let first =
            UnixSystem.initial<int, string> platform
            |> Launched.processId (ProcessId.parseOrFail context 299)
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixSystem.writePidMaxSysctl context 301

        let pid, machine =
            match SimulatedMachine.launch (launchOn platform) (SimulatedMachine.ofSystem first) with
            | Ok launched -> launched
            | Error refusal -> failwith $"launch: %s{ProcessCreationRefusal.describe refusal}"

        ProcessId.toInt32 pid |> shouldEqual 300

        SimulatedMachine.launch (launchOn platform) machine
        |> Result.map ignore
        |> shouldEqual (Error ProcessCreationRefusal.NoFreeProcessId)

    [<Test>]
    let ``a launch is refused for another platform, and for a directory the filesystem does not hold`` () : unit =
        let _, machine = machineOf SimulatedUnixPlatform.linuxX64 1

        SimulatedMachine.launch (launchOn SimulatedUnixPlatform.linuxArm64) machine
        |> Result.map ignore
        |> shouldEqual (
            Error (
                ProcessCreationRefusal.Launch (
                    LaunchRefusal.NotOfPlatform (SimulatedUnixPlatform.linuxArm64, SimulatedUnixPlatform.linuxX64)
                )
            )
        )

        let nowhere = AbsoluteUnixPath.parseOrFail context "/nowhere"

        SimulatedMachine.launch
            (launchOn SimulatedUnixPlatform.linuxX64
             |> ProcessLaunch.withCurrentDirectory nowhere)
            machine
        |> Result.map ignore
        |> shouldEqual (
            Error (
                ProcessCreationRefusal.Launch (
                    LaunchRefusal.CurrentDirectory (nowhere, CurrentDirectoryFault.DoesNotResolve UnixError.ENOENT)
                )
            )
        )

    // ---------------------------------------------------------- shared machine

    let private bindAt (port : uint16) (view : UnixSystem<int, string>) : BindAnswer * UnixSystem<int, string> =
        let fd, view = KeventWorld.stream true view

        match
            CopyIn.bind
                fd
                UserBuffer.Mapped
                16u
                (CopyIn.inet (UnixSystem.platform view) (KeventWorld.loopback port))
                view
        with
        | Ok answered -> answered
        | Error refusal -> failwith $"bind: %s{BindRefusal.describe refusal}"

    [<Test>]
    let ``a port one process has bound is EADDRINUSE to another`` () : unit =
        for platform in platforms do
            let pids, machine = machineOf platform 2
            let a, b = pids.[0], pids.[1]

            let answer, machine = inProcess a (bindAt 8080us) machine

            match answer with
            | BindAnswer.Bound _ -> ()
            | other -> failwith $"the first bind answered %A{other}"

            let answer, machine = inProcess b (bindAt 8080us) machine
            answer |> shouldEqual (BindAnswer.Failed UnixError.EADDRINUSE)

            // ...and a different port is free to it.
            let answer, machine = inProcess b (bindAt 8081us) machine

            match answer with
            | BindAnswer.Bound _ -> ()
            | other -> failwith $"the second process's bind of a free port answered %A{other}"

            assertClean machine

    [<Test>]
    let ``each process's descriptors are numbered on their own, and each writes to its own output`` () : unit =
        for platform in platforms do
            let pids, machine = machineOf platform 2

            let opened (view : UnixSystem<int, string>) =
                match
                    OpenFlagWords.openPath
                        {
                            Access = FileAccessMode.ReadWrite
                            Create = true
                            Exclusive = false
                            Truncate = false
                            NoFollow = false
                            CloseOnExec = false
                            Synchronous = false
                            DataSynchronous = false
                            Directory = false
                        }
                        (PathArg.ofText "/shared")
                        0o644
                        view
                with
                | Ok (SyscallAnswer.Completed fd, view) -> int fd, view
                | other -> failwith $"open: %A{other}"

            let fds, machine =
                (([], machine), pids)
                ||> List.fold (fun (fds, machine) pid ->
                    let fd, machine = inProcess pid opened machine
                    fds @ [ fd ], machine
                )

            // The three launched descriptors are each process's own, so each
            // first open is 3.
            fds |> shouldEqual [ 3 ; 3 ]

            let write (text : string) (view : UnixSystem<int, string>) =
                match
                    UnixReadWrite.write 0 1 (ImmutableArray.CreateRange (System.Text.Encoding.ASCII.GetBytes text)) view
                with
                | Ok (WriteOutcome.Returns (_, view)) -> (), view
                | other -> failwith $"write: %A{other}"

            let (), machine = inProcess pids.[0] (write "first") machine
            let (), machine = inProcess pids.[1] (write "second") machine
            let (), machine = inProcess pids.[0] (write "again") machine

            UnixSystem.delivered (viewOf pids.[0] machine)
            |> DeliveryLog.toList
            |> List.map (fun delivery ->
                delivery.Endpoint, System.Text.Encoding.ASCII.GetString (delivery.Bytes.AsSpan ())
            )
            |> shouldEqual
                [
                    ExternalEndpoint (pids.[0], 1), "first"
                    ExternalEndpoint (pids.[1], 1), "second"
                    ExternalEndpoint (pids.[0], 1), "again"
                ]

            assertClean machine

    let private mkdir (path : string) (view : UnixSystem<int, string>) : SyscallAnswer * UnixSystem<int, string> =
        Answered.mkdir (PathArg.ofText path) 0o755 view

    let private chdir (path : string) (view : UnixSystem<int, string>) : SyscallAnswer * UnixSystem<int, string> =
        Answered.chdir (PathArg.ofText path) view

    let private rmdir (path : string) (view : UnixSystem<int, string>) : SyscallAnswer * UnixSystem<int, string> =
        match UnixNamespace.rmdir (PathArg.ofText path) view with
        | Ok answered -> answered
        | Error refusal -> failwith $"rmdir: %s{RemovalRefusal.describe refusal}"

    [<Test>]
    let ``a directory one process stands in outlives its removal by another, until it steps out`` () : unit =
        for platform in platforms do
            let pids, machine = machineOf platform 2
            let a, b = pids.[0], pids.[1]

            let _, machine = inProcess a (mkdir "/d") machine
            let _, machine = inProcess a (chdir "/d") machine
            let standing = (viewOf a machine).Process.CurrentDirectoryInode

            let answer, machine = inProcess b (rmdir "/d") machine
            answer |> shouldEqual (SyscallAnswer.Completed 0L)

            // No name reaches it, and it is still there: a stands in it.
            VirtualFileSystem.tryGet standing (viewOf b machine).Machine.FileSystem
            |> Option.isSome
            |> shouldEqual true

            assertClean machine

            // Stepping out lets it go.
            let _, machine = inProcess a (chdir "/") machine

            VirtualFileSystem.tryGet standing (viewOf b machine).Machine.FileSystem
            |> shouldEqual None

            assertClean machine

    [<Test>]
    let ``a view is refused once another process's view has been written back`` () : unit =
        let pids, machine = machineOf SimulatedUnixPlatform.linuxX64 2
        let first = viewOf pids.[0] machine
        let second = viewOf pids.[1] machine

        let _, second = KeventWorld.dup 1 second
        let machine' = SimulatedMachine.unfocus second machine

        let error =
            Assert.Throws<exn> (fun () -> SimulatedMachine.unfocus first machine' |> ignore)

        error.Message |> shouldContainText "has since changed"

        // ...and the written-back process's change is the only one.
        (viewOf pids.[1] machine').Process |> shouldEqual second.Process
        machine'.Processes.[pids.[0]] |> shouldEqual machine.Processes.[pids.[0]]

    [<Test>]
    let ``a view checks only what it can see truthfully, and the machine checks the rest`` () : unit =
        let pids, machine = machineOf SimulatedUnixPlatform.linuxX64 2
        let view = viewOf pids.[0] machine

        // Clean, though the machine records the other process's thread and
        // process IDs, its holds and its current directory.
        UnixSystem.checkInvariants view |> shouldEqual []

        // A process ID recorded as live that no process has: the view cannot
        // tell it from the other process's, and the machine can.
        let ghost = ProcessId.parseOrFail context 77

        let forged =
            { machine with
                Machine =
                    { machine.Machine with
                        ProcessIds = ProcessIdTable.add ghost machine.Machine.ProcessIds
                    }
            }

        UnixSystem.checkInvariants (viewOf pids.[0] forged) |> shouldEqual []

        SimulatedMachine.checkInvariants forged
        |> shouldEqual
            [
                SimulatedMachineDefect.Machine (
                    UnixSystemDefect.LiveProcessIdsMismatch (Set.singleton ghost, Set.empty)
                )
            ]

    [<Test>]
    let ``the machine reports a current directory hold no process stands in, and a missing one`` () : unit =
        let pids, machine = machineOf SimulatedUnixPlatform.linuxX64 2
        let root = (viewOf pids.[0] machine).Process.CurrentDirectoryInode

        machine.Machine.CurrentDirectories |> shouldEqual (Map.ofList [ root, 2 ])

        let short =
            { machine with
                Machine = UnixMachineState.releaseCurrentDirectory root machine.Machine
            }

        SimulatedMachine.checkInvariants short
        |> shouldEqual
            [
                SimulatedMachineDefect.Machine (UnixSystemDefect.CurrentDirectoryHoldMismatch (root, 1, 2))
            ]
