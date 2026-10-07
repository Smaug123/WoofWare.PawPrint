namespace WoofWare.PosixKernel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The machine's walks over its open file descriptions, exercised directly
/// rather than through a syscall.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixMachineState =

    /// A registration watching `EPOLLIN` alone, edge-triggered, as the kernel
    /// stores it: with `EPOLLERR` and `EPOLLHUP` added.
    let private readInterest : EpollRegistration =
        {
            Events =
                EpollEvents.In
                ||| EpollEvents.EdgeTriggered
                ||| EpollEvents.Err
                ||| EpollEvents.Hup
            Data = 0UL
            RegisteredAt = 0L
        }

    /// A freshly booted machine whose open file descriptions are `registry`'s.
    let private machineWith (registry : FileDescriptorRegistry) : UnixMachineState =
        let system : UnixSystem<int, string> =
            UnixSystem.initial SimulatedUnixPlatform.linuxX64
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

        (UnixSystemState.withFileDescriptors registry system).Machine

    [<Test>]
    let ``every reference a description holds keeps its inode alive`` () : unit =
        // One of each kind that can hold one, and one of each that cannot, so a
        // rule that answered "every description" or "no description" fails.
        let fileInode = InodeNumber 7L
        let directoryInode = InodeNumber 9L

        let _fd, withFile =
            FileDescriptorRegistry.openFile fileInode FileAccessMode.ReadOnly LaunchedStreams.registry

        let _directoryFd, withDirectory =
            FileDescriptorRegistry.openDirectory directoryInode withFile

        let _sock, withSocket =
            FileDescriptorRegistry.createSocket (SocketId 1L) withDirectory

        let _epollFd, registry = FileDescriptorRegistry.createEpoll withSocket

        let machine = machineWith registry

        // ...besides the process's current directory, which the machine holds
        // too.
        UnixMachineState.heldInodes machine
        |> shouldEqual (
            Set.ofList [ fileInode ; directoryInode ]
            |> Set.union (machine.CurrentDirectories |> Map.keys |> Set.ofSeq)
        )

        machine.CurrentDirectories.Count |> shouldEqual 1

    [<Test>]
    let ``a socket is named by exactly the descriptions that name it`` () : unit =
        let watched = SocketId 1L
        let other = SocketId 2L

        let _watchedFd, registry =
            FileDescriptorRegistry.createSocket watched LaunchedStreams.registry

        let _otherFd, registry = FileDescriptorRegistry.createSocket other registry
        let _queueFd, registry = FileDescriptorRegistry.createEpoll registry

        let machine = machineWith registry

        UnixMachineState.descriptionsNamingSocket watched machine
        |> Set.count
        |> shouldEqual 1

        UnixMachineState.descriptionsNamingSocket (SocketId 3L) machine
        |> shouldEqual Set.empty

    [<Test>]
    let ``a state-change wake queues every registration of the socket`` () : unit =
        let watched = SocketId 1L

        let watchedFd, registry =
            FileDescriptorRegistry.createSocket watched LaunchedStreams.registry

        let queueFd, registry = FileDescriptorRegistry.createEpoll registry

        let idOf (fd : int) : OpenFileDescriptionId =
            match FileDescriptorRegistry.tryFindId fd registry with
            | Some id -> id
            | None -> failwith $"fd %d{fd} is not live"

        let registry =
            registry
            |> FileDescriptorRegistry.mapOpenFiles (
                OpenFileTable.addEpollRegistration (idOf queueFd) (watchedFd, idOf watchedFd) readInterest
            )

        let ready (openFiles : OpenFileTable) : (int * OpenFileDescriptionId) list =
            OpenFileTable.descriptions openFiles
            |> Map.toSeq
            |> Seq.collect (fun (_, description) ->
                match description.Target with
                | OpenFileTarget.Epoll queueState -> queueState.Ready
                | _ -> []
            )
            |> List.ofSeq

        let machine = machineWith registry

        ready machine.OpenFiles |> shouldEqual []

        let signalled (wake : SocketWake) (socketId : SocketId) : OpenFileTable =
            OpenFileTable.signalEpollInstances
                (UnixMachineState.descriptionsNamingSocket socketId machine)
                (SocketWake.epollKey wake)
                machine.OpenFiles

        for wake in [ SocketWake.ConnectResolved ; SocketWake.RefusalReset ; SocketWake.PeerFin ] do
            ready (signalled wake watched) |> List.length |> shouldEqual 1

            // A socket nothing watches wakes nothing.
            ready (signalled wake (SocketId 2L)) |> shouldEqual []
