namespace WoofWare.PosixKernel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// What one process on a `SimulatedMachine` does to another through the
/// machine they share: a connection or a close in one waking a call asleep in
/// another, a kqueue or a Darwin `poll` in one seeing a socket event another
/// caused, and one process's end closing everything it held.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCrossProcess =

    // ------------------------------------------------------- one view's checks

    [<Test>]
    let ``one process's descriptor table checks only what one table can tell, on a machine holding others`` () : unit =
        for platform in Machines.platforms do
            let pids, machine = Machines.ofCount platform 2
            let a, b = pids.[0], pids.[1]

            // The other process holds descriptors of its own, which this table
            // does not name; and on Darwin a kqueue registering a socket
            // through a descriptor number this table has not opened.
            let machine =
                match SimulatedUnixPlatform.flavour platform with
                | SimulatedUnixFlavour.Linux ->
                    machine |> Machines.doIn b (fun view -> KeventWorld.stream true view |> snd)
                | SimulatedUnixFlavour.Darwin ->
                    machine
                    |> Machines.doIn
                        b
                        (fun view ->
                            let socket, view = KeventWorld.stream true view
                            let kq, view = KeventWorld.kqueue view
                            KeventWorld.register kq socket -1s 0x1us 0UL view
                        )

            Machines.assertClean machine

            let registry = UnixSystem.fileDescriptors (Machines.viewOf a machine)
            FileDescriptorRegistry.checkInvariants registry |> shouldEqual []

            // A description this table alone names more often than its count
            // records is caught from the one view...
            let stdout = FileDescriptorRegistry.tryFindId 1 registry |> Option.get

            FileDescriptorRegistry.Unchecked.setDescriptorCount stdout 0 registry
            |> FileDescriptorRegistry.checkInvariants
            |> shouldEqual [ FileDescriptorRegistryDefect.DescriptorCountMismatch (stdout, 0, 1) ]

            // ...while one counted more often than this table names it may be
            // named in another process's table, so only the machine can tell.
            FileDescriptorRegistry.Unchecked.setDescriptorCount stdout 2 registry
            |> FileDescriptorRegistry.checkInvariants
            |> shouldEqual []

    [<Test>]
    let ``a descriptor table on a machine holding its process alone checks every count exactly`` () : unit =
        for platform in Machines.platforms do
            let pids, machine = Machines.ofCount platform 1
            let registry = UnixSystem.fileDescriptors (Machines.viewOf pids.[0] machine)
            let stdout = FileDescriptorRegistry.tryFindId 1 registry |> Option.get

            FileDescriptorRegistry.Unchecked.setDescriptorCount stdout 2 registry
            |> FileDescriptorRegistry.checkInvariants
            |> shouldEqual [ FileDescriptorRegistryDefect.DescriptorCountMismatch (stdout, 2, 1) ]
