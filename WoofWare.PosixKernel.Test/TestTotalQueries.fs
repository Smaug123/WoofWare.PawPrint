namespace WoofWare.PosixKernel.Test

open System
open System.Reflection
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open Microsoft.FSharp.Reflection
open NUnit.Framework
open WoofWare.PosixKernel

/// The public functions that take a bare identity of a kernel object answer
/// for every value of it. A client can build any `InodeNumber` or `SocketId`,
/// and holds one read from a state that may since have let the object go, so
/// a function that took one and failed for an absent object would fail on
/// input the client is entitled to pass.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTotalQueries =

    /// Every public function of every module the library exports that takes
    /// a parameter of type `identity`, as `Module.function`.
    let private takers (identity : Type) : string list =
        typeof<VirtualFileSystem>.Assembly.GetExportedTypes ()
        |> Array.toList
        |> List.filter FSharpType.IsModule
        |> List.collect (fun (m : Type) ->
            m.GetMethods (BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly)
            |> Array.toList
            |> List.filter (fun (f : MethodInfo) ->
                f.GetParameters ()
                |> Array.exists (fun (p : ParameterInfo) -> p.ParameterType = identity)
            )
            |> List.map (fun (f : MethodInfo) -> $"%s{m.Name}.%s{f.Name}")
        )
        |> List.sort

    /// A bare inode can name nothing, a non-directory, or the root of Darwin's
    /// devfs, which this library does not model; so a function that took one
    /// and needed it to be a live directory would fail on input a client can
    /// build. Each of these readers answers `None`, zero or `false` for an
    /// inode the filesystem does not hold. A new public function that takes a
    /// bare inode belongs here only once it does the same; otherwise it should
    /// reach its inode through a path or a descriptor.
    [<Test>]
    let ``the public functions that take a bare inode are the readers that answer for any inode`` () : unit =
        takers typeof<InodeNumber>
        |> shouldEqual (
            List.sort
                [
                    "VirtualFileSystemModule.bindingCount"
                    "VirtualFileSystemModule.entryCount"
                    "VirtualFileSystemModule.isOrphanedDirectory"
                    "VirtualFileSystemModule.mountedRootOf"
                    "VirtualFileSystemModule.mountOf"
                    "VirtualFileSystemModule.pathOfDirectory"
                    "VirtualFileSystemModule.subdirectoryCount"
                    "VirtualFileSystemModule.tryGet"
                    "VirtualFileSystemModule.tryGetContent"
                ]
        )

    [<Test>]
    let ``every public reader of a bare inode answers for an inode the filesystem does not hold`` () : unit =
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            let vfs =
                UnixSystem.initial<int, string> platform
                |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)
                |> UnixSystem.fileSystem

            let held = VirtualFileSystem.inodes vfs

            let property (raw : int64) : unit =
                let inode = InodeNumber raw

                if not (Map.containsKey inode held) then
                    VirtualFileSystem.bindingCount inode vfs |> shouldEqual 0
                    VirtualFileSystem.entryCount inode vfs |> shouldEqual 0
                    VirtualFileSystem.isOrphanedDirectory inode vfs |> shouldEqual false
                    VirtualFileSystem.mountedRootOf inode vfs |> shouldEqual None
                    VirtualFileSystem.mountOf inode vfs |> shouldEqual None
                    VirtualFileSystem.pathOfDirectory inode vfs |> shouldEqual None
                    VirtualFileSystem.subdirectoryCount inode vfs |> shouldEqual 0
                    VirtualFileSystem.tryGet inode vfs |> shouldEqual None
                    VirtualFileSystem.tryGetContent inode vfs |> shouldEqual None

            // Near the inodes the filesystem does hold, where an off-by-one
            // would land, as well as anywhere at all.
            let near =
                let top = held |> Map.keys |> Seq.map (fun (InodeNumber i) -> i) |> Seq.max
                Gen.choose64 (-2L, top + 2L)

            Check.One (Config.QuickThrowOnFailure, Prop.forAll (Arb.fromGen near) property)

            Check.One (
                Config.QuickThrowOnFailure,
                Prop.forAll (Arb.fromGen (ArbMap.defaults |> ArbMap.generate<int64>)) property
            )

    /// A socket goes when its last open file description does, so a
    /// `SocketId` read from a descriptor names nothing once that descriptor
    /// has closed, whether or not the client could forge one.
    [<Test>]
    let ``the public function that takes a bare socket identity answers for any`` () : unit =
        takers typeof<SocketId> |> shouldEqual [ "UnixSystem.socket" ]

    /// What a socket identity names: the socket a live descriptor names by it,
    /// with the shape it was created with, and nothing for any other identity,
    /// including the identities of sockets since closed.
    [<Test>]
    let ``UnixSystem.socket answers exactly the sockets a descriptor names`` () : unit =
        let gen =
            gen {
                let! requests = Gen.listOf (Gen.elements NewSocket.requests)
                let! closes = Gen.listOfLength (List.length requests) (ArbMap.defaults |> ArbMap.generate<bool>)
                let! forged = (ArbMap.defaults |> ArbMap.generate<int64>)
                let! platform = Gen.elements [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]
                return platform, List.zip requests closes, forged
            }

        let property
            (
                platform : SimulatedUnixPlatform,
                sockets : ((SocketDomain * SocketKind * SocketProtocol) * bool) list,
                forged : int64
            )
            : unit
            =
            let initial : UnixSystem<int, string> =
                UnixSystem.initial platform
                |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

            let socketIdOf (fd : int) (system : UnixSystem<int, string>) : SocketId =
                match UnixSystem.descriptorTarget fd system with
                | Some (OpenFileTarget.Socket socketId) -> socketId
                | other -> failwith $"descriptor %d{fd} names %A{other} rather than a socket"

            let live, closed, system =
                ((Map.empty, Set.empty, initial), sockets)
                ||> List.fold (fun (live, closed, system) ((domain, kind, protocol) as request, close) ->
                    match NewSocket.tryCreate domain kind protocol system with
                    | None -> live, closed, system
                    | Some (fd, system) ->
                        let socketId = socketIdOf fd system

                        if close then
                            match UnixDescriptor.close fd system with
                            | Ok (SyscallAnswer.Completed _, system) -> live, Set.add socketId closed, system
                            | other -> failwith $"closing socket descriptor %d{fd}: %A{other}"
                        else
                            Map.add socketId request live, closed, system
                )

            for KeyValue (socketId, (domain, kind, _)) in live do
                match UnixSystem.socket socketId system with
                | Some socket -> (socket.Domain, socket.Kind) |> shouldEqual (domain, kind)
                | None -> failwith $"socket %O{socketId} has a descriptor open on it, but the machine does not hold it"

            for socketId in closed do
                UnixSystem.socket socketId system |> shouldEqual None

            let forged = SocketId forged

            if not (Map.containsKey forged live) then
                UnixSystem.socket forged system |> shouldEqual None

        Check.One (Config.QuickThrowOnFailure, Prop.forAll (Arb.fromGen gen) property)

    /// A client names a task by its own identity, and the process holds a task
    /// by that name only from its creation to its exit; so the library has no
    /// public question about one task by its name, and a client looks the
    /// name up in `UnixSystem.tasks` and reads the state it finds.
    [<Test>]
    let ``UnixTaskTable asks nothing about a single task by its name`` () : unit =
        let table =
            typeof<VirtualFileSystem>.Assembly.GetType ("WoofWare.PosixKernel.UnixTaskTable", true)

        table.GetMethods (BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly)
        |> Array.map (fun (f : MethodInfo) -> f.Name)
        |> shouldEqual [| "reconcile" |]

    [<Test>]
    let ``a task is in UnixSystem.tasks from its creation to its exit, with the processor and ID it was given``
        ()
        : unit
        =
        let property (cpu : NonNegativeInt) (unknown : int) : unit =
            // A machine with the processor the child is created on.
            let system : UnixSystem<int, string> =
                UnixSystem.initial SimulatedUnixPlatform.linuxX64
                |> UnixBootImage.withProcessorCount (cpu.Get + 1)
                |> Configured.expectOk ProcessorCountRefusal.describe
                |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

            let child = 1

            if unknown <> 0 && unknown <> child then
                Map.tryFind unknown (UnixSystem.tasks system) |> shouldEqual None

            Map.tryFind child (UnixSystem.tasks system) |> shouldEqual None

            let tid, system =
                match UnixTaskLifecycle.spawn 0 child (CpuId cpu.Get) system with
                | Ok (SpawnAnswer.Spawned tid, system) -> tid, system
                | other -> failwith $"spawning task %d{child}: %A{other}"

            match Map.tryFind child (UnixSystem.tasks system) with
            | Some task ->
                UnixTaskState.cpu task |> shouldEqual (CpuId cpu.Get)
                UnixTaskState.osThreadId task |> shouldEqual tid
                UnixTaskState.parkedIn task |> shouldEqual None
            | None -> failwith $"task %d{child} was spawned, but the process holds no task by that name"

            let system =
                match UnixTaskLifecycle.exitThread child 0 system with
                | Ok (TaskOutcome.Continues system) -> system
                | other -> failwith $"task %d{child}'s exit: %A{other}"

            Map.tryFind child (UnixSystem.tasks system) |> shouldEqual None

        Check.One (Config.QuickThrowOnFailure, property)
