namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// A `SimulatedMachine` holding one process: the view `focus` takes is the
/// system it was made from, `unfocus` writes a view back and refuses one that is
/// not of the machine as it stands, and `checkInvariants` reports the clauses
/// that relate the machine's processes to each other.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSimulatedMachine =

    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    let private world (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        UnixSystem.initial<int, string> platform
        |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

    let private sorted (defects : 'a list) : 'a list = List.sortBy (sprintf "%A") defects

    /// `view` as a system no machine has focused: equal to the system a view
    /// of the same state would be, apart from where it was taken from.
    let private unfocused (view : UnixSystem<int, string>) : UnixSystem<int, string> =
        { view with
            Origin = FocusOrigin.NotFocused
        }

    /// One step of a walk through the process's own syscalls: each index is
    /// read modulo whatever it picks from.
    [<RequireQualifiedAccess>]
    type private Op =
        | Listener of port : int
        | Client of port : int
        | Accept of fd : int
        | Pipe
        | Dup of fd : int
        | Close of fd : int

    let private opGen : Gen<Op> =
        let small = Gen.choose (0, 63)

        Gen.frequency
            [
                2, Gen.map Op.Listener small
                3, Gen.map Op.Client small
                2, Gen.map Op.Accept small
                2, Gen.constant Op.Pipe
                2, Gen.map Op.Dup small
                3, Gen.map Op.Close small
            ]

    let private ports : uint16 list = [ 8080us ; 8081us ]

    let private openFds (system : UnixSystem<int, string>) : int list =
        FileDescriptorRegistry.fds (UnixSystemState.fileDescriptors system)
        |> Map.keys
        |> List.ofSeq

    let private apply (system : UnixSystem<int, string>) (op : Op) : UnixSystem<int, string> =
        let pick (i : int) (from : 'a list) : 'a option =
            if from.IsEmpty then None else Some from.[i % from.Length]

        match op with
        | Op.Listener port ->
            let fd, system = KeventWorld.stream true system
            let port = ports.[port % ports.Length]

            match
                CopyIn.bind
                    fd
                    UserBuffer.Mapped
                    16u
                    (CopyIn.inet (UnixSystem.platform system) (KeventWorld.loopback port))
                    system
            with
            | Ok (BindAnswer.Bound _, system) -> KeventWorld.listen fd system
            | Ok (BindAnswer.Failed _, system) -> system
            | other -> failwith $"bind: %A{other}"
        | Op.Client port ->
            let fd, system = KeventWorld.stream true system
            let port = ports.[port % ports.Length]

            match
                CopyIn.connect
                    fd
                    UserBuffer.Mapped
                    16u
                    (CopyIn.inet (UnixSystem.platform system) (KeventWorld.loopback port))
                    system
            with
            | Ok (_, system) -> system
            // A full accept queue is outside what the kernel models; the socket
            // stays, unconnected.
            | Error _ -> system
        | Op.Accept fd ->
            match pick fd (openFds system) with
            | None -> system
            | Some fd ->
                match UnixConnection.accept 0 fd UserBuffer.Mapped 16u system with
                | Ok (_, system) -> system
                // Outside what the kernel models, so the step changes nothing.
                | Error _ -> system
        | Op.Pipe ->
            match UnixPipe.pipe2 0 UserBuffer.Mapped system with
            | Ok (_, system) -> system
            | Error refusal -> failwith $"pipe: %A{refusal}"
        | Op.Dup fd ->
            match pick fd (openFds system) with
            | None -> system
            | Some fd -> KeventWorld.dup fd system |> snd
        | Op.Close fd ->
            match pick fd (openFds system) with
            | None -> system
            | Some fd ->
                match UnixDescriptor.close fd system with
                | Ok (_, system) -> system
                // Outside what the kernel models, so the step changes nothing.
                | Error _ -> system

    [<Test>]
    let ``a machine made from a system focuses back to that system, unfocuses it, and checks clean`` () : unit =
        // Runs whose process ended holding a connection, a pipe it made, and a
        // duplicated descriptor: the walk must reach each.
        let mutable connected = 0
        let mutable piped = 0
        let mutable duplicated = 0

        let property (platform : SimulatedUnixPlatform) (ops : Op list) : unit =
            let system = List.fold apply (world platform) ops

            if not system.Machine.Connections.IsEmpty then
                connected <- connected + 1

            if system.Machine.Pipes.Count > 3 then
                piped <- piped + 1

            if
                OpenFileTable.descriptions system.Machine.OpenFiles
                |> Map.exists (fun id _ -> OpenFileTable.descriptorCount id system.Machine.OpenFiles > Some 1)
            then
                duplicated <- duplicated + 1

            let pid = UnixSystem.processId system
            let machine = SimulatedMachine.ofSystem system

            SimulatedMachine.processIds machine |> shouldEqual (Set.singleton pid)

            SimulatedMachine.focus pid machine
            |> Option.map unfocused
            |> shouldEqual (Some system)

            SimulatedMachine.checkInvariants machine |> shouldEqual []

            let written = SimulatedMachine.unfocus system machine

            SimulatedMachine.focus pid written
            |> Option.map unfocused
            |> shouldEqual (Some system)

            // A focused view written back unchanged leaves the machine as it was.
            let view = SimulatedMachine.focus pid machine |> Option.get
            SimulatedMachine.unfocus view machine |> shouldEqual machine

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 200,
            Prop.forAll
                (Arb.fromGen (Gen.zip (Gen.elements platforms) (Gen.listOf opGen)))
                (fun (platform, ops) -> property platform ops)
        )

        connected |> shouldBeGreaterThan 20
        piped |> shouldBeGreaterThan 20
        duplicated |> shouldBeGreaterThan 20

    [<Test>]
    let ``step makes the call in the process's view and writes the view back`` () : unit =
        for platform in platforms do
            let system = world platform
            let pid = UnixSystem.processId system
            let machine = SimulatedMachine.ofSystem system

            let expected =
                match UnixSystem.step 0 (Syscall.Dup 1) system with
                | Ok (outcome, view) -> outcome, view
                | Error refusal -> failwith $"dup: %A{refusal}"

            match SimulatedMachine.step pid 0 (Syscall.Dup 1) machine with
            | Some (Ok outcome, after) ->
                outcome |> shouldEqual (fst expected)

                SimulatedMachine.focus pid after
                |> Option.map unfocused
                |> shouldEqual (Some (snd expected))
            | other -> failwith $"step: %A{other}"

            let other = ProcessId.parseOrFail "test" 7
            SimulatedMachine.step other 0 (Syscall.Dup 1) machine |> shouldEqual None

    [<Test>]
    let ``a refused step leaves the machine itself, so a view focused before it can still be written back`` () : unit =
        // Linux refuses clonefile, which is Darwin's.
        let system = world SimulatedUnixPlatform.linuxX64
        let pid = UnixSystem.processId system
        let machine = SimulatedMachine.ofSystem system
        let pending = SimulatedMachine.focus pid machine |> Option.get

        let call = Syscall.CloneFile (PathArg.ofText "/a", PathArg.ofText "/b", 0)

        match SimulatedMachine.step pid 0 call machine with
        | Some (Error _, after) ->
            obj.ReferenceEquals (after, machine) |> shouldEqual true
            SimulatedMachine.unfocus pending after |> shouldEqual machine
        | other -> failwith $"expected a refusal, got %A{other}"

    [<Test>]
    let ``unfocus refuses a view once another has been written back`` () : unit =
        for platform in platforms do
            let system = world platform
            let pid = UnixSystem.processId system
            let machine = SimulatedMachine.ofSystem system

            let first = SimulatedMachine.focus pid machine |> Option.get
            let second = SimulatedMachine.focus pid machine |> Option.get

            let _, first = KeventWorld.dup 1 first
            let machine = SimulatedMachine.unfocus first machine

            let error =
                Assert.Throws<exn> (fun () -> SimulatedMachine.unfocus second machine |> ignore)

            error.Message
            |> shouldContainText "was not focused from the machine as it stands"

    [<Test>]
    let ``unfocus refuses a second view of one process once the first is written back`` () : unit =
        // The machine is unchanged by the first write-back (umask touches the
        // process alone), so only the process's own state tells the views
        // apart.
        for platform in platforms do
            let system = world platform
            let pid = UnixSystem.processId system
            let machine = SimulatedMachine.ofSystem system

            let first = SimulatedMachine.focus pid machine |> Option.get
            let second = SimulatedMachine.focus pid machine |> Option.get

            let _, first = UnixSystem.umask 0o077 first
            let machine = SimulatedMachine.unfocus first machine

            obj.ReferenceEquals (machine.Machine, second.Machine) |> shouldEqual true

            let error =
                Assert.Throws<exn> (fun () -> SimulatedMachine.unfocus second machine |> ignore)

            error.Message
            |> shouldContainText "was not focused from the machine as it stands"

    [<Test>]
    let ``unfocus refuses a view focused from another history of the machine`` () : unit =
        // Two histories branched from one machine: in each, one write-back.
        // A view from one is not a view of the other, whatever the two have
        // done since.
        for platform in platforms do
            let system = world platform
            let pid = UnixSystem.processId system
            let root = SimulatedMachine.ofSystem system

            let branch (fd : int) =
                let view = SimulatedMachine.focus pid root |> Option.get
                let _, view = KeventWorld.dup fd view
                SimulatedMachine.unfocus view root

            let left = branch 1
            let right = branch 2

            let fromLeft = SimulatedMachine.focus pid left |> Option.get

            let error =
                Assert.Throws<exn> (fun () -> SimulatedMachine.unfocus fromLeft right |> ignore)

            error.Message
            |> shouldContainText "was not focused from the machine as it stands"

    [<Test>]
    let ``a machine made of a view accepts that view unchanged`` () : unit =
        for platform in platforms do
            let system = world platform
            let pid = UnixSystem.processId system

            let view =
                SimulatedMachine.focus pid (SimulatedMachine.ofSystem system) |> Option.get

            let _, view = UnixSystem.umask 0o077 view

            let remade = SimulatedMachine.ofSystem view
            SimulatedMachine.unfocus view remade |> shouldEqual remade

    [<Test>]
    let ``unfocus refuses a view of a process the machine does not hold`` () : unit =
        for platform in platforms do
            let machine = SimulatedMachine.ofSystem (world platform)

            let stranger =
                UnixSystem.initial<int, string> platform
                |> Launched.processId (ProcessId.parseOrFail "test" 77)
                |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

            let error =
                Assert.Throws<exn> (fun () -> SimulatedMachine.unfocus stranger machine |> ignore)

            error.Message |> shouldContainText "no process on the machine has ID 77"

    [<Test>]
    let ``ofSystem refuses a view of a machine holding other processes`` () : unit =
        for platform in platforms do
            let system = world platform

            let crowded =
                { system with
                    Machine =
                        { system.Machine with
                            ProcessIds = ProcessIdTable.add (ProcessId.parseOrFail "test" 77) system.Machine.ProcessIds
                        }
                }

            let error =
                Assert.Throws<exn> (fun () -> SimulatedMachine.ofSystem crowded |> ignore)

            error.Message |> shouldContainText "holding other processes besides"

    [<Test>]
    let ``a process held under a second ID as well is reported by every clause it breaks`` () : unit =
        for platform in platforms do
            let system = world platform
            let pid = UnixSystem.processId system
            let other = ProcessId.parseOrFail "test" 77
            let machine = SimulatedMachine.ofSystem system

            let cloned =
                { machine with
                    Processes = Map.add other machine.Processes.[pid] machine.Processes
                }

            let leaderTid = UnixTaskState.osThreadId (UnixTaskTable.get 0 system.Tasks)

            // The launched pipes' three descriptions are each named once by the
            // machine's record and twice by the two tables.
            let counts =
                OpenFileTable.descriptions system.Machine.OpenFiles
                |> Map.keys
                |> Seq.map (fun id ->
                    SimulatedMachineDefect.OpenFiles (FileDescriptorRegistryDefect.DescriptorCountMismatch (id, 1, 2))
                )
                |> List.ofSeq

            counts.Length |> shouldEqual 3

            SimulatedMachine.checkInvariants cloned
            |> sorted
            |> shouldEqual (
                sorted (
                    [
                        SimulatedMachineDefect.Machine (UnixSystemDefect.DuplicateOsThreadId (leaderTid, [ 0 ; 0 ]))
                        SimulatedMachineDefect.Machine (
                            UnixSystemDefect.CurrentDirectoryHoldMismatch (system.Process.CurrentDirectoryInode, 1, 2)
                        )
                        SimulatedMachineDefect.SlotUnderAnotherProcessId (other, pid)
                        SimulatedMachineDefect.DuplicateProcessId pid
                    ]
                    @ counts
                )
            )

    [<Test>]
    let ``a kqueue whose owner is no process on the machine is reported, and its registrations are no table's``
        ()
        : unit
        =
        let listener, system = KeventWorld.listenerAt 8080us KeventWorld.darwin
        let kqueue, system = KeventWorld.kqueue system
        let id = KeventWorld.idOf kqueue system
        let pid = UnixSystem.processId system
        let gone = ProcessId.parseOrFail "test" 9

        // Registered through a descriptor that is not open in any table.
        let state (owner : ProcessId) =
            {
                Owner = owner
                Drained = false
                Registrations =
                    Map.ofList
                        [
                            (listener + 40, KqueueFilter.Read),
                            {
                                Clear = true
                                Receipt = false
                                UserData = 0UL
                                RegisteredAt = 0L
                                Socket = SocketId 0L
                            }
                        ]
                Active = []
            }

        let withOwner (owner : ProcessId) =
            UnixSystemState.mapOpenFiles (OpenFileTable.setKqueueState id (state owner)) system
            |> fun system ->
                { system with
                    Machine =
                        { system.Machine with
                            NextEventRegistrationOrdinal = 1L
                        }
                }
            |> SimulatedMachine.ofSystem
            |> SimulatedMachine.checkInvariants

        withOwner pid
        |> shouldEqual
            [
                SimulatedMachineDefect.DescriptorTable (
                    pid,
                    FileDescriptorRegistryDefect.KqueueRegistrationThroughClosedDescriptor (
                        id,
                        listener + 40,
                        KqueueFilter.Read
                    )
                )
            ]

        withOwner gone
        |> sorted
        |> shouldEqual (
            sorted
                [
                    SimulatedMachineDefect.View (pid, UnixSystemDefect.KqueueOfAnotherProcess (kqueue, id, gone))
                    SimulatedMachineDefect.Machine (UnixSystemDefect.KqueueOwnerNotLive (id, gone))
                ]
        )
