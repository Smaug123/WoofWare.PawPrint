namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The bound every descriptor this kernel hands out lies below
/// (`SimulatedUnixPlatform.descriptorBound`), the soft `RLIMIT_NOFILE` it
/// assumes the process has at least: the calls `TestFcntlMeasured`'s replays do
/// not reach, and the invariant and the launch table that state it.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDescriptorBound =

    let private runs : FcntlWorld.Run list = FcntlWorld.runs

    /// `run`'s system with `f` open at 3, and every descriptor from 4 below
    /// `last + 1` a dup of it.
    let private filledTo (last : int) (run : FcntlWorld.Run) : UnixSystem<int, string> =
        let system = FcntlWorld.system run

        let file, system =
            FcntlWorld.openWith (FcntlWorld.opening FileAccessMode.ReadOnly) "f" system

        file |> shouldEqual 3

        ([ 4..last ], system)
        ||> List.foldBack (fun target system ->
            match UnixDescriptor.dup2 3 target system with
            | Ok (SyscallAnswer.Completed _, system) -> system
            | other -> failwith $"filling the table at %d{target}: %A{other}"
        )

    let private refusalAt (bound : int) (descriptor : int) : DescriptorLimitRefusal =
        {
            Descriptor = descriptor
            Bound = bound
        }

    /// The bound is each flavour's default soft limit at process start, which
    /// `rlimit-nofile.c` measured.
    [<Test>]
    let ``the bound is the flavour's default soft limit`` () : unit =
        SimulatedUnixPlatform.descriptorBound SimulatedUnixPlatform.linuxX64
        |> shouldEqual 1024

        SimulatedUnixPlatform.descriptorBound SimulatedUnixPlatform.linuxArm64
        |> shouldEqual 1024

        SimulatedUnixPlatform.descriptorBound SimulatedUnixPlatform.macOsArm64
        |> shouldEqual 256

    /// F_DUPFD from an argument below the bound, with every descriptor from it
    /// to the bound taken: the lowest it could use is the bound, so it is
    /// refused, naming the bound; with one free below it, it takes that one.
    [<Test>]
    let ``F_DUPFD below the bound with no room before it is refused`` () : unit =
        for run in runs do
            let bound = SimulatedUnixPlatform.descriptorBound run.Platform
            let full = filledTo (bound - 1) run

            for command in [ FcntlWorld.DupFd ; FcntlWorld.dupFdCloexec run.Platform ] do
                for minimum in [ 0 ; 4 ; bound - 1 ] do
                    match UnixDescriptor.fcntl 3 command minimum full with
                    | Error (FcntlRefusal.DescriptorLimit refusal) -> refusal |> shouldEqual (refusalAt bound bound)
                    | other -> failwith $"%s{run.Name}: F_DUPFD from %d{minimum} on a full table: %A{other}"

                let oneLeft = filledTo (bound - 2) run

                match UnixDescriptor.fcntl 3 command (bound - 1) oneLeft with
                | Ok (SyscallAnswer.Completed fd, after) ->
                    fd |> shouldEqual (int64 (bound - 1))
                    UnixSystem.checkInvariants after |> shouldEqual []
                | other -> failwith $"%s{run.Name}: F_DUPFD into the last slot: %A{other}"

    /// dup3 of a good source onto a target at or above the bound is refused;
    /// its screens ahead of that are answered (`TestFcntlMeasured` has the
    /// rows).
    [<Test>]
    let ``dup2 and dup3 onto a target at or above the bound are refused`` () : unit =
        for run in runs do
            let bound = SimulatedUnixPlatform.descriptorBound run.Platform
            let system = filledTo 3 run

            for target in [ bound ; bound + 1 ; Int32.MaxValue ] do
                match UnixDescriptor.dup2 3 target system with
                | Error (Dup2Refusal.DescriptorLimit refusal) -> refusal |> shouldEqual (refusalAt bound target)
                | other -> failwith $"%s{run.Name}: dup2 onto %d{target}: %A{other}"

                match SimulatedUnixPlatform.flavour run.Platform with
                | SimulatedUnixFlavour.Darwin -> ()
                | SimulatedUnixFlavour.Linux ->
                    match UnixDescriptor.dup3 3 target OpenFlagNumbering.LinuxCloseOnExec system with
                    | Error (Dup3Refusal.DescriptorLimit refusal) -> refusal |> shouldEqual (refusalAt bound target)
                    | other -> failwith $"%s{run.Name}: dup3 onto %d{target}: %A{other}"

    /// A pipe needs two descriptors below the bound: with two left it takes
    /// them, and with one it is refused, naming the bound as the write end's
    /// number.
    [<Test>]
    let ``pipe2 needs two descriptors below the bound`` () : unit =
        for run in runs do
            let bound = SimulatedUnixPlatform.descriptorBound run.Platform

            match UnixPipe.pipe2 0 UserBuffer.Mapped (filledTo (bound - 3) run) with
            | Ok (Pipe2Answer.Created (r, w), after) ->
                (r, w) |> shouldEqual (bound - 2, bound - 1)
                UnixSystem.checkInvariants after |> shouldEqual []
            | other -> failwith $"%s{run.Name}: pipe2 with two left: %A{other}"

            match UnixPipe.pipe2 0 UserBuffer.Mapped (filledTo (bound - 2) run) with
            | Error (Pipe2Refusal.DescriptorLimit refusal) -> refusal |> shouldEqual (refusalAt bound bound)
            | other -> failwith $"%s{run.Name}: pipe2 with one left: %A{other}"

    /// An accept parked on an empty listener, finished once a connection is
    /// queued but the table has filled meanwhile, is refused rather than handed
    /// a descriptor at the bound.
    [<Test>]
    let ``a parked accept finishing on a full table is refused`` () : unit =
        for run in runs do
            let bound = SimulatedUnixPlatform.descriptorBound run.Platform
            let system = FcntlWorld.system run

            let file, system =
                FcntlWorld.openWith (FcntlWorld.opening FileAccessMode.ReadOnly) "f" system

            let listening, system = FcntlWorld.listener 5000us false system

            let system =
                match UnixConnection.accept 1 listening UserBuffer.Mapped 16u system with
                | Ok (AcceptOutcome.WouldBlock _, system) -> system
                | other -> failwith $"accept on an empty listener: %A{other}"

            let client, system =
                NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

            let system = FcntlWorld.connect client 5000us system

            let system =
                ([ client + 1 .. bound - 1 ], system)
                ||> List.foldBack (fun target system ->
                    match UnixDescriptor.dup2 file target system with
                    | Ok (SyscallAnswer.Completed _, system) -> system
                    | other -> failwith $"filling the table at %d{target}: %A{other}"
                )

            UnixWait.wakes (Set.singleton 1) system |> List.map fst |> shouldEqual [ 1 ]

            match UnixConnection.finishAccept 1 system with
            | Error (AcceptRefusal.DescriptorLimit refusal) -> refusal |> shouldEqual (refusalAt bound bound)
            | other -> failwith $"%s{run.Name}: finishing the accept on a full table: %A{other}"

    /// `UnixConnection.acceptConnection`, which skips `accept`'s screens, fails
    /// loudly on a full table rather than handing out a descriptor at the
    /// bound.
    [<Test>]
    let ``acceptConnection on a full table fails`` () : unit =
        for run in runs do
            let bound = SimulatedUnixPlatform.descriptorBound run.Platform
            let system = FcntlWorld.system run

            let file, system =
                FcntlWorld.openWith (FcntlWorld.opening FileAccessMode.ReadOnly) "f" system

            let listening, system = FcntlWorld.listener 5000us false system

            let client, system =
                NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

            let system = FcntlWorld.connect client 5000us system

            let listener =
                match FileDescriptorRegistry.tryFindTarget listening (UnixSystemState.fileDescriptors system) with
                | Some (OpenFileTarget.Socket socketId) -> socketId
                | other -> failwith $"%s{run.Name}: descriptor %d{listening} names %A{other}, not a socket"

            let full =
                ([ client + 1 .. bound - 1 ], system)
                ||> List.foldBack (fun target system ->
                    match UnixDescriptor.dup2 file target system with
                    | Ok (SyscallAnswer.Completed _, system) -> system
                    | other -> failwith $"filling the table at %d{target}: %A{other}"
                )

            let e =
                Assert.Throws<exn> (fun () -> UnixConnection.acceptConnection listener full |> ignore<_>)

            e.Message |> shouldContainText $"at or above %d{bound}"

            let oneLeft =
                match UnixDescriptor.close (bound - 1) full with
                | Ok (SyscallAnswer.Completed _, system) -> system
                | other -> failwith $"%s{run.Name}: closing %d{bound - 1}: %A{other}"

            let fd, _, after = UnixConnection.acceptConnection listener oneLeft
            fd |> shouldEqual (bound - 1)
            UnixSystem.checkInvariants after |> shouldEqual []

    /// `UnixSystem.step`'s `Dup` carries the refusal.
    [<Test>]
    let ``step refuses a dup at the bound`` () : unit =
        for run in runs do
            let bound = SimulatedUnixPlatform.descriptorBound run.Platform

            match UnixSystem.step 0 (Syscall.Dup 3) (filledTo (bound - 1) run) with
            | Error (SyscallRefusal.Dup refusal) -> refusal |> shouldEqual (refusalAt bound bound)
            | other -> failwith $"%s{run.Name}: step Dup on a full table: %A{other}"

    /// A descriptor at the bound is one no call hands out, which the system's
    /// invariants name.
    [<Test>]
    let ``a descriptor at the bound breaks the invariants`` () : unit =
        for run in runs do
            let bound = SimulatedUnixPlatform.descriptorBound run.Platform
            let system = filledTo 3 run

            let registry =
                FileDescriptorRegistry.Unchecked.ofParts
                    (FileDescriptorRegistry.fds (UnixSystemState.fileDescriptors system)
                     |> Map.add bound (Map.find 3 (FileDescriptorRegistry.fds (UnixSystemState.fileDescriptors system))))
                    (OpenFileTable.descriptions system.Machine.OpenFiles)
                    (OpenFileDescriptionId 1000L)

            let broken = UnixSystemState.withFileDescriptors registry system

            UnixSystem.checkInvariants broken
            |> List.filter (fun defect ->
                match defect with
                | UnixSystemDefect.DescriptorAtOrAboveBound _ -> true
                | _ -> false
            )
            |> shouldEqual [ UnixSystemDefect.DescriptorAtOrAboveBound (bound, bound) ]

    /// A launch table naming a descriptor at the bound is refused.
    [<Test>]
    let ``a launch table at the bound is refused`` () : unit =
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            let bound = SimulatedUnixPlatform.descriptorBound platform

            let launch = Map.ofList [ bound - 1, LaunchDescriptor.Drained ]

            UnixSystem.initial<int, string> platform
            |> Launched.boot launch 0 (CpuId 0)
            |> UnixSystem.checkInvariants
            |> shouldEqual []

            ProcessLaunch.create platform (Map.ofList [ bound, LaunchDescriptor.Drained ]) 0 (CpuId 0)
            |> Result.map ignore
            |> shouldEqual (Error (LaunchTableRefusal.AtOrAboveBound (bound, bound)))

            ProcessLaunch.create platform (Map.ofList [ -1, LaunchDescriptor.Drained ]) 0 (CpuId 0)
            |> Result.map ignore
            |> shouldEqual (Error (LaunchTableRefusal.NegativeDescriptor -1))
