namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// The socket table and the descriptor table have to agree, and neither module
/// can check that alone: `FileDescriptorRegistry` compiles before the kernel
/// that holds the sockets. These are the claims about the pair.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEmulatedKernelSockets =

    // The soundness walk's coverage counters are sums over the whole run, so
    // their spread relative to the mean shrinks as the case count grows. Over 40
    // sampled runs the rarest of them ranged 18-47 at 500 cases — a threshold
    // anywhere in that band is a coin toss — and 111-169 at 2000. Every
    // threshold below is set at about half the minimum of 40 runs at this count.
    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 2000

    /// The soundness walk's only input is a seed for its own `System.Random`, so
    /// one draw is one walk and the seed's *magnitude* means nothing — nothing
    /// is lost by having no shrinker, because a smaller seed is a different
    /// walk rather than a simpler one. Drawing the seed here rather than
    /// through `NonNegativeInt` is what makes the case count honest: FsCheck
    /// sizes an integer generator, so at the default end size `NonNegativeInt`
    /// yields values in [0, 100], and 500 cases were really ~85 distinct walks
    /// resampled with replacement.
    let private genWalkSeed : Gen<int> = Gen.choose (0, System.Int32.MaxValue)

    /// `SocketEventPort.drain` against a kernel, with the claim its two readers
    /// exist to satisfy checked on every call: the predicate a parked waiter is
    /// polled against and the drain its woken handler performs read the same
    /// annotated walk, so a drain reports something exactly when the predicate
    /// said it would.
    let private deliverSocketEvents
        (portId : OpenFileDescriptionId)
        (maxCount : int)
        (kernel : EmulatedKernel)
        : (uint64 * ReadinessLevel) list * EmulatedKernel
        =
        let system = EmulatedKernel.unix kernel
        let predicted = SocketEventPort.hasDeliverableEvent portId system
        let delivered, system = SocketEventPort.drain portId maxCount system

        if List.isEmpty delivered = predicted then
            failwith
                $"SocketEventPort.hasDeliverableEvent answered %b{predicted} of port %O{portId}, but draining it reported %d{List.length delivered} events. The two read the same annotated walk, so they cannot disagree."

        delivered, EmulatedKernel.withUnix system kernel

    let private hasDeliverableSocketEvents (portId : OpenFileDescriptionId) (kernel : EmulatedKernel) : bool =
        SocketEventPort.hasDeliverableEvent portId (EmulatedKernel.unix kernel)

    let private epollReadinessOfDescription
        (targetId : OpenFileDescriptionId)
        (kernel : EmulatedKernel)
        : ReadinessLevel
        =
        SocketEventPort.epollReadinessOfDescription targetId (EmulatedKernel.unix kernel)

    /// A kernel whose socket table and descriptor table are built by hand, so
    /// that `checkInvariants` has something unsound to reject. Every operation
    /// the kernel offers maintains the invariant, which is exactly why the
    /// defects need forging.
    let private forge
        (descriptions : (int * OpenFileDescriptionId * OpenFileTarget) list)
        (sockets : (int64 * SocketDescription) list)
        (nextSocketId : int64)
        : EmulatedKernel
        =
        let registry =
            FileDescriptorRegistry.Unchecked.ofParts
                (descriptions |> List.map (fun (fd, id, _) -> fd, id) |> Map.ofList)
                (descriptions
                 |> List.map (fun (_, id, target) ->
                     id,
                     {
                         Target = target
                         AccessMode = FileAccessMode.ReadWrite
                         NonBlocking = false
                         Flock = None
                     }
                 )
                 |> Map.ofList)
                (OpenFileDescriptionId (int64 descriptions.Length + 100L))

        { EmulatedKernel.initial with
            Machine =
                { EmulatedKernel.initial.Machine with
                    Sockets = sockets |> List.map (fun (id, socket) -> SocketId id, socket) |> Map.ofList
                    NextSocketId = SocketId nextSocketId
                }
            Process =
                { EmulatedKernel.initial.Process with
                    FileDescriptors = registry
                }
        }

    let private someSocket : SocketDescription =
        {
            Domain = SocketDomain.InterNetwork
            Kind = SocketKind.Stream
            Protocol = SocketProtocol.Tcp
            Binding = None
            Phase = SocketPhase.Idle
            ReuseAddress = false
        }

    /// The triple a socket is created with, asserted per field because nothing
    /// else in the runtime reads it back yet: a transposition here would
    /// otherwise survive until `SystemNative_GetSocketType` reported it.
    [<Test>]
    let ``a fresh socket carries its triple into the socket table`` () : unit =
        let fd, kernel =
            EmulatedKernel.createSocket
                SocketDomain.InterNetworkV6
                SocketKind.Datagram
                SocketProtocol.Udp
                EmulatedKernel.initial

        match FileDescriptorRegistry.tryFind fd kernel.FileDescriptors with
        | None -> failwith "the socket descriptor is not live"
        | Some description ->

        match description.Target with
        | OpenFileTarget.Socket socketId ->
            let socket = UnixMachineState.socket socketId kernel.Machine
            socket.Domain |> shouldEqual SocketDomain.InterNetworkV6
            socket.Kind |> shouldEqual SocketKind.Datagram
            socket.Protocol |> shouldEqual SocketProtocol.Udp
        | other -> failwith $"expected a socket target, got %O{other}"

        EmulatedKernel.checkInvariants kernel |> shouldEqual []

    /// Two sockets are two identities and two table entries — the fact
    /// `OpenFileObject.Socket` turns into two `flock` slots.
    [<Test>]
    let ``two sockets get distinct identities`` () : unit =
        let _, kernel =
            EmulatedKernel.createSocket
                SocketDomain.InterNetwork
                SocketKind.Stream
                SocketProtocol.Tcp
                EmulatedKernel.initial

        let _, kernel =
            EmulatedKernel.createSocket SocketDomain.Unix SocketKind.Datagram SocketProtocol.Unspecified kernel

        kernel.Sockets |> Map.count |> shouldEqual 2
        kernel.NextSocketId |> shouldEqual (SocketId 2L)
        EmulatedKernel.checkInvariants kernel |> shouldEqual []

    /// Closing the last descriptor onto a socket destroys the socket too. Before
    /// the socket table existed this was automatic — the socket *was* the
    /// description — so it is the clause most at risk of being forgotten.
    [<Test>]
    let ``closing the last descriptor destroys the socket`` () : unit =
        let fd, kernel =
            EmulatedKernel.createSocket
                SocketDomain.InterNetwork
                SocketKind.Stream
                SocketProtocol.Tcp
                EmulatedKernel.initial

        kernel.Sockets |> Map.count |> shouldEqual 1

        match KernelSyscall.close fd kernel with
        | Error e -> failwith $"expected close to succeed, got %O{e}"
        | Ok kernel ->

        kernel.Sockets |> shouldEqual Map.empty
        // Not rewound: identities are never reused, so the next socket does not
        // take the dead one's name.
        kernel.NextSocketId |> shouldEqual (SocketId 1L)
        EmulatedKernel.checkInvariants kernel |> shouldEqual []

    /// ...and closing one of two descriptors onto a socket destroys neither the
    /// description nor the socket. This is the half that a `close` keying off
    /// "the descriptor named a socket" rather than off "a description died"
    /// would get wrong, and it is reachable from a guest: `dup(2)` of a socket
    /// descriptor is an ordinary thing to do.
    [<Test>]
    let ``closing a dup leaves the socket alive`` () : unit =
        let fd, kernel =
            EmulatedKernel.createSocket
                SocketDomain.InterNetwork
                SocketKind.Stream
                SocketProtocol.Tcp
                EmulatedKernel.initial

        let duped, kernel =
            match FileDescriptorRegistry.dup fd kernel.FileDescriptors with
            | Ok (duped, registry) ->
                duped,
                { kernel with
                    Process =
                        { kernel.Process with
                            FileDescriptors = registry
                        }
                }
            | Error e -> failwith $"expected dup to succeed, got %O{e}"

        match KernelSyscall.close duped kernel with
        | Error e -> failwith $"expected close to succeed, got %O{e}"
        | Ok kernel ->

        kernel.Sockets |> Map.count |> shouldEqual 1
        EmulatedKernel.checkInvariants kernel |> shouldEqual []

        // And the surviving descriptor still resolves to it.
        match FileDescriptorRegistry.tryFind fd kernel.FileDescriptors with
        | Some description ->
            match description.Target with
            | OpenFileTarget.Socket socketId ->
                UnixMachineState.socket socketId kernel.Machine |> shouldEqual someSocket
            | other -> failwith $"expected a socket target, got %O{other}"
        | None -> failwith "the original descriptor should still be live"

    /// Closing a descriptor that names something other than a socket must leave
    /// the socket table alone. Without this, a `close` that cleared the table
    /// on every destroyed description would pass every test above.
    [<Test>]
    let ``closing a non-socket descriptor leaves the socket table alone`` () : unit =
        let _, kernel =
            EmulatedKernel.createSocket
                SocketDomain.InterNetwork
                SocketKind.Stream
                SocketProtocol.Tcp
                EmulatedKernel.initial

        let port, registry =
            FileDescriptorRegistry.createSocketEventPort kernel.FileDescriptors

        let kernel =
            { kernel with
                Process =
                    { kernel.Process with
                        FileDescriptors = registry
                    }
            }

        match KernelSyscall.close port kernel with
        | Error e -> failwith $"expected close to succeed, got %O{e}"
        | Ok kernel ->

        kernel.Sockets |> Map.count |> shouldEqual 1
        EmulatedKernel.checkInvariants kernel |> shouldEqual []

    /// A description naming a socket the table does not hold. `UnixMachineState.socket`
    /// is total against this, so without the check it would surface as an
    /// interpreter crash at some unrelated call site instead.
    [<Test>]
    let ``checkInvariants rejects a description naming no socket`` () : unit =
        forge [ 0, OpenFileDescriptionId 7L, OpenFileTarget.Socket (SocketId 5L) ] [] 6L
        |> EmulatedKernel.checkInvariants
        |> shouldEqual
            [
                EmulatedKernelDefect.System (UnixSystemDefect.DanglingSocket (OpenFileDescriptionId 7L, SocketId 5L))
            ]

    /// A socket no description names. Today that means a close forgot to clean
    /// up; `SystemNative_Accept` is what will make it legal.
    [<Test>]
    let ``checkInvariants rejects a socket no description names`` () : unit =
        forge [] [ 5L, someSocket ] 6L
        |> EmulatedKernel.checkInvariants
        |> shouldEqual
            [
                EmulatedKernelDefect.System (UnixSystemDefect.UnreferencedSocket (SocketId 5L))
            ]

    /// A socket identity at or above the cursor would be minted again by the next
    /// `socket(2)`, giving two sockets one identity — and hence, through
    /// `OpenFileObject`, one `flock` contention key. `NextIdNotFresh`'s sibling.
    [<TestCase(5L)>]
    [<TestCase(0L)>]
    let ``checkInvariants rejects a NextSocketId at or below a live socket`` (next : int64) : unit =
        forge [ 0, OpenFileDescriptionId 7L, OpenFileTarget.Socket (SocketId 5L) ] [ 5L, someSocket ] next
        |> EmulatedKernel.checkInvariants
        |> shouldEqual
            [
                EmulatedKernelDefect.System (UnixSystemDefect.NextSocketIdNotFresh (SocketId next, SocketId 5L))
            ]

    [<Test>]
    let ``checkInvariants accepts a NextSocketId above every live socket`` () : unit =
        forge [ 0, OpenFileDescriptionId 7L, OpenFileTarget.Socket (SocketId 5L) ] [ 5L, someSocket ] 6L
        |> EmulatedKernel.checkInvariants
        |> shouldEqual []

    /// `UnixMachineState.socket` is total against the invariant rather than
    /// optional, so the one state that could defeat it must fail loudly.
    [<Test>]
    let ``resolving an unknown socket identity fails loudly`` () : unit =
        let e =
            Assert.Throws<System.Exception> (fun () ->
                UnixMachineState.socket (SocketId 99L) EmulatedKernel.initial.Machine |> ignore
            )

        e.Message |> shouldContainText "names no socket in this kernel's socket table"

    /// A handful of files to open and unlink, so the property below works on a
    /// filesystem rather than on fabricated inode numbers. Names in one flat
    /// directory: this property is about descriptor and inode *lifetime*, and a
    /// deeper tree would add walking without adding a lifetime.
    /// Five files at the root, and a chain of nested directories beside them.
    ///
    /// The chain is what lets a removal *orphan* something: a directory held
    /// open after its last name has gone keeps its whole ancestor chain alive,
    /// and freeing it must collect that chain. A flat seed can never produce
    /// the state, so the `DanglingParent` clause would go untouched.
    let private lifetimeSeed : Map<FileName, SeedEntry> =
        let name (n : string) = FileName.parseOrFail "test seed" n

        let files =
            [ "a" ; "b" ; "c" ; "d" ; "e" ]
            |> List.map (fun n ->
                name n, SeedEntry.file (System.Text.Encoding.UTF8.GetBytes n |> ImmutableArray.CreateRange)
            )

        let chain =
            // Three deep, so a removal can orphan a directory whose own parent
            // is later orphaned in turn.
            SeedEntry.directory (
                Map.ofList
                    [
                        name "mid", SeedEntry.directory (Map.ofList [ name "leaf", SeedEntry.directory Map.empty ])
                    ]
            )

        Map.ofList ((name "top", chain) :: files)

    /// The allocating and closing operations interleaved at random must leave
    /// *all three* tables sound. This is what connects the hand-forged defects
    /// above to the code paths that maintain them: a `close` that forgot the
    /// socket table shows up here as `UnreferencedSocket`, a `createSocket` that
    /// failed to advance the counter as `NextSocketIdNotFresh`, and a `close`
    /// that reaped an inode a surviving descriptor still names as
    /// `DanglingOpenInode`.
    ///
    /// The filesystem is seeded and every `openFile` names an inode that really
    /// exists, which is what a real `open(2)` guarantees. Interleaving `unlink`
    /// with `dup` and `close` is the point: whether an inode may be freed
    /// depends on both tables at once, and no single-threaded sequence of
    /// hand-written steps covers the orders in which the last name and the last
    /// descriptor can go.
    [<Test>]
    let ``a random mix of allocations and closes keeps both tables sound`` () : unit =
        let mutable observedSockets = 0
        let mutable observedHeldOrphanDirectories = 0
        let mutable observedSocketCloses = 0
        let mutable observedDups = 0
        let mutable observedUnlinks = 0
        let mutable observedReaps = 0

        let property (seed : int) : unit =
            let rng = System.Random (seed)
            let steps = rng.Next (1, 30)

            let mutable kernel =
                EmulatedKernel.initial
                |> EmulatedKernel.withFileSystemAndCurrentDirectory
                    SimulatedUnixPlatform.linuxX64
                    (UnixTimestamp.createOrFail "test" 1_700_000_000L 0)
                    lifetimeSeed
                    AbsoluteUnixPath.root

            for _ in 1..steps do
                let live =
                    FileDescriptorRegistry.fds kernel.FileDescriptors |> Map.toList |> List.map fst

                let namesSocket (fd : int) : bool =
                    match FileDescriptorRegistry.tryFind fd kernel.FileDescriptors with
                    | Some description ->
                        match description.Target with
                        | OpenFileTarget.Socket _ -> true
                        | _ -> false
                    | None -> false

                // Biased towards allocation, so that several sockets are live at
                // once and a close has something to get wrong.
                match (if List.isEmpty live then 9 else rng.Next 10) with
                | 0
                | 1 ->
                    let chosen = live.[rng.Next live.Length]
                    let wasSocket = namesSocket chosen

                    match KernelSyscall.close chosen kernel with
                    | Ok kernel' ->
                        kernel <- kernel'

                        if wasSocket then
                            observedSocketCloses <- observedSocketCloses + 1
                    | Error e -> failwith $"unexpected close error: %O{e}"
                | 2
                | 3 ->
                    match FileDescriptorRegistry.dup live.[rng.Next live.Length] kernel.FileDescriptors with
                    | Ok (_, registry) ->
                        kernel <-
                            { kernel with
                                Process =
                                    { kernel.Process with
                                        FileDescriptors = registry
                                    }
                            }

                        observedDups <- observedDups + 1
                    | Error e -> failwith $"unexpected dup error: %O{e}"
                | 4
                | 5 ->
                    // An inode that really exists, chosen from whatever the
                    // filesystem still holds: a descriptor onto an inode the
                    // filesystem does not contain is a state `open(2)` cannot
                    // produce, and `DanglingOpenInode` says so.
                    let candidates =
                        VirtualFileSystem.inodes kernel.FileSystem
                        |> Map.toList
                        |> List.map fst
                        |> List.filter (fun inode -> inode <> VirtualFileSystem.root kernel.FileSystem)

                    if not (List.isEmpty candidates) then
                        let _, registry =
                            FileDescriptorRegistry.openFile
                                candidates.[rng.Next candidates.Length]
                                FileAccessMode.ReadOnly
                                kernel.FileDescriptors

                        kernel <-
                            { kernel with
                                Process =
                                    { kernel.Process with
                                        FileDescriptors = registry
                                    }
                            }
                | 7 ->
                    // Remove a name at random, from *any* directory the graph
                    // still contains, and reap if that was the last reference.
                    // Interleaved with the closes above, so the two orders —
                    // last name first, last descriptor first — both occur.
                    //
                    // Removing a name at *depth* is what produces an orphaned
                    // directory, whose recorded parent must then outlive being
                    // unbound itself. A flat corpus reaches neither.
                    //
                    // Only names `unlink(2)` or `rmdir(2)` could remove are
                    // candidates: anything that is not a directory, and a
                    // directory that is empty. Unbinding a *populated* one is a
                    // state no syscall PawPrint models can produce — it would
                    // orphan a whole subtree at once — and asserting soundness
                    // in it would be asserting a rule nobody has decided.
                    let removable =
                        let filesystem = kernel.FileSystem

                        let isRemovable (target : InodeNumber) : bool =
                            match VirtualFileSystem.tryGetContent target filesystem with
                            | Some (InodeContent.Directory content) -> Map.isEmpty content.Entries
                            | Some (InodeContent.RegularFile _)
                            | Some (InodeContent.Symlink _) -> true
                            | None -> false

                        VirtualFileSystem.inodes filesystem
                        |> Map.toList
                        |> List.collect (fun (holder, node) ->
                            match node.Content with
                            | InodeContent.Directory content ->
                                content.Entries
                                |> Map.toList
                                |> List.filter (fun (_, target) -> isRemovable target)
                                |> List.map (fun (entry, _) -> holder, entry)
                            | InodeContent.RegularFile _
                            | InodeContent.Symlink _ -> []
                        )

                    if not (List.isEmpty removable) then
                        let holder, chosen = removable.[rng.Next removable.Length]

                        match
                            VirtualFileSystem.unbind
                                UnbindTargetEffect.LostALink
                                holder
                                chosen
                                (UnixMachineState.fileTimestamp kernel.Machine)
                                kernel.FileSystem
                        with
                        | Error e -> failwith $"unexpected unbind error: %O{e}"
                        | Ok (inode, filesystem) ->
                            let before = VirtualFileSystem.inodes filesystem |> Map.count

                            kernel <-
                                { kernel with
                                    Machine =
                                        { kernel.Machine with
                                            FileSystem = filesystem
                                        }
                                }
                                |> EmulatedKernel.mapUnix (UnixSystem.forgetIfUnheld inode)

                            observedUnlinks <- observedUnlinks + 1

                            if (VirtualFileSystem.inodes kernel.FileSystem |> Map.count) < before then
                                observedReaps <- observedReaps + 1

                            // The inode survived the loss of its last name, and
                            // it is a directory — so something is holding an
                            // orphan whose ".." must not dangle.
                            match VirtualFileSystem.tryGetContent inode kernel.FileSystem with
                            | Some (InodeContent.Directory _) ->
                                observedHeldOrphanDirectories <- observedHeldOrphanDirectories + 1
                            | Some (InodeContent.RegularFile _)
                            | Some (InodeContent.Symlink _)
                            | None -> ()
                | 6 ->
                    let _, registry =
                        FileDescriptorRegistry.createSocketEventPort kernel.FileDescriptors

                    kernel <-
                        { kernel with
                            Process =
                                { kernel.Process with
                                    FileDescriptors = registry
                                }
                        }
                | _ ->
                    // A different triple each time, so that a `createSocket`
                    // which keyed identity off the triple rather than off a
                    // counter would not be saved by them all being equal.
                    let domain =
                        match rng.Next 3 with
                        | 0 -> SocketDomain.InterNetwork
                        | 1 -> SocketDomain.InterNetworkV6
                        | _ -> SocketDomain.Unix

                    let kind =
                        if rng.Next 2 = 0 then
                            SocketKind.Stream
                        else
                            SocketKind.Datagram

                    let _, kernel' =
                        EmulatedKernel.createSocket domain kind SocketProtocol.Unspecified kernel

                    kernel <- kernel'
                    observedSockets <- observedSockets + 1

                EmulatedKernel.checkInvariants kernel |> shouldEqual []
                FileDescriptorRegistry.checkInvariants kernel.FileDescriptors |> shouldEqual []

                VirtualFileSystem.checkInvariants
                    (UnixSystem.pinnedInodes (EmulatedKernel.unix kernel))
                    kernel.FileSystem
                |> shouldEqual []

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genWalkSeed) property)

        // Without these the run could be sound while never having exercised the
        // operations the clauses are about — a close that never fell on a socket
        // would leave the `UnreferencedSocket` clause untouched throughout.
        observedSockets |> shouldBeGreaterThan 2500
        observedSocketCloses |> shouldBeGreaterThan 500
        observedDups |> shouldBeGreaterThan 2500
        observedUnlinks |> shouldBeGreaterThan 1400

        // ...and a reap really happened, so the `DanglingOpenInode` and
        // `UnreachableFromRoot` clauses had something to be wrong about. An
        // unlink whose inode is still held reaps nothing, and a run of only
        // those would leave both clauses untouched.
        observedReaps |> shouldBeGreaterThan 1100

        // ...and specifically that a *directory* outlived its last name. Without
        // this the run could be sound while never producing an orphaned
        // directory at all, which is the only state in which `pinnedInodes`'s
        // climb up `DirectoryContent.Parent` does anything — a flat corpus
        // reaches neither, and the `DanglingParent` clause would go untouched
        // throughout.
        //
        // `forgetIfUnheld`'s *cascade* is deliberately not guarded here.
        // Reaching it needs one four-step interleaving in a fixed order — open a
        // directory, unbind it, unbind its parent while the pin still holds,
        // then close — which an instrumented count of reaps of two inodes at
        // once found not once in 80,000 walks at the case count above (nor in
        // 500 walks of up to 60 steps, twice this one's length). Raising the
        // case count chases a probability that is a product of four independent
        // choices, so it does not concentrate the way the counters above do; the
        // cascade is pinned deterministically by
        // `TestEmulatedKernelInodeLifetime`'s `an orphan held by a descriptor
        // keeps its ancestors alive` and `the cascade stops at the root`.
        observedHeldOrphanDirectories |> shouldBeGreaterThan 50

    // --- socketReadinessLevel ---

    /// A listening stream socket presents nothing while its queue is empty
    /// and `EPOLLIN` once something is queued (measured, `masks.c` rows 3-5).
    [<Test>]
    let ``a listening stream socket's readiness is its queue`` () : unit =
        let listening (queue : ConnectionId list) =
            forge
                [ 3, OpenFileDescriptionId 10L, OpenFileTarget.Socket (SocketId 0L) ]
                [
                    0L,
                    { someSocket with
                        Phase =
                            SocketPhase.Listening
                                {
                                    Backlog = 8
                                    Queue = queue
                                }
                    }
                ]
                1L

        UnixMachineState.socketReadinessLevel (SocketId 0L) (listening []).Machine
        |> shouldEqual ReadinessLevel.none

        let queued =
            let kernel = listening [ ConnectionId 0L ]

            { kernel with
                Machine =
                    { kernel.Machine with
                        Connections =
                            Map.ofList
                                [
                                    ConnectionId 0L,
                                    {
                                        ClientAddress = InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress 5000us
                                        ServerAddress = InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress 6000us
                                    }
                                ]
                        NextConnectionId = ConnectionId 1L
                    }
            }

        UnixMachineState.socketReadinessLevel (SocketId 0L) queued.Machine
        |> shouldEqual
            { ReadinessLevel.none with
                In = true
            }

    /// An idle stream socket is `EPOLLOUT|EPOLLHUP` (measured, `masks.c`
    /// rows 1-2, bound or not).
    [<Test>]
    let ``a non-listening stream socket presents OUT and HUP`` () : unit =
        let kernel =
            forge [ 3, OpenFileDescriptionId 10L, OpenFileTarget.Socket (SocketId 0L) ] [ 0L, someSocket ] 1L

        UnixMachineState.socketReadinessLevel (SocketId 0L) kernel.Machine
        |> shouldEqual
            { ReadinessLevel.none with
                Out = true
                Hup = true
            }

    /// A datagram socket is `EPOLLOUT` alone — no HUP, unlike the idle
    /// stream case (measured, `masks.c` rows 13-14).
    [<Test>]
    let ``a datagram socket presents OUT without HUP`` () : unit =
        let kernel =
            forge
                [ 3, OpenFileDescriptionId 10L, OpenFileTarget.Socket (SocketId 0L) ]
                [
                    0L,
                    { someSocket with
                        Kind = SocketKind.Datagram
                        Protocol = SocketProtocol.Udp
                    }
                ]
                1L

        UnixMachineState.socketReadinessLevel (SocketId 0L) kernel.Machine
        |> shouldEqual
            { ReadinessLevel.none with
                Out = true
            }

    /// A pending refusal presents everything (measured, `masks.c` row 10:
    /// 0x201d), and the interest filter keeps ERR and HUP whatever the mask
    /// asks for (rows 16-17).
    [<Test>]
    let ``a pending refusal presents every condition and survives a narrowed interest`` () : unit =
        let kernel =
            forge
                [ 3, OpenFileDescriptionId 10L, OpenFileTarget.Socket (SocketId 0L) ]
                [
                    0L,
                    { someSocket with
                        Phase = SocketPhase.RefusedPendingDelivery
                    }
                ]
                1L

        let level = UnixMachineState.socketReadinessLevel (SocketId 0L) kernel.Machine

        level
        |> shouldEqual
            {
                In = true
                Out = true
                RdHup = true
                Hup = true
                Err = true
            }

        level
        |> ReadinessLevel.reportedUnder (SocketEventsPal.toInterest "test" 0)
        |> shouldEqual
            { ReadinessLevel.none with
                Hup = true
                Err = true
            }

        level
        |> ReadinessLevel.reportedUnder (SocketEventsPal.toInterest "test" 0x01)
        |> shouldEqual
            { ReadinessLevel.none with
                In = true
                Hup = true
                Err = true
            }

    /// The standard streams' levels are constants of the launch shape
    /// (measured, `pipes.c`): stdin is a read end whose writer is closed —
    /// `EPOLLHUP` — and the output ends are writable. The ids are
    /// `initial`'s standard streams.
    [<Test>]
    let ``the standard streams present their pipe-end levels`` () : unit =
        epollReadinessOfDescription (OpenFileDescriptionId 0L) EmulatedKernel.initial
        |> shouldEqual
            { ReadinessLevel.none with
                Hup = true
            }

        for id in 1L .. 2L do
            epollReadinessOfDescription (OpenFileDescriptionId id) EmulatedKernel.initial
            |> shouldEqual
                { ReadinessLevel.none with
                    Out = true
                }

    /// Registrations reference live descriptions (`close` sweeps), so a
    /// dangling id must be reported as the interpreter bug it is rather than
    /// answered either way.
    [<Test>]
    let ``a dangling readiness target crashes rather than answering`` () : unit =
        let exc =
            Assert.Throws<System.Exception> (fun () ->
                epollReadinessOfDescription (OpenFileDescriptionId 99L) EmulatedKernel.initial
                |> ignore
            )

        exc.Message |> shouldContainText "names no live open file description"

    // --- the connect(2) state machine ---

    let private loopback (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port

    let private inetFamily : int option =
        Some SimulatedUnixPlatform.internetAddressFamily

    /// A listening stream socket at loopback:`port` with the given backlog and
    /// `clients` idle stream sockets, each with a descriptor: fds 3, 4, 5, ...
    /// onto sockets 0 (the listener), 1, 2, ...
    let private listenerAndClients (backlog : int) (port : uint16) (clients : int) : EmulatedKernel =
        forge
            [
                for i in 0..clients ->
                    3 + i, OpenFileDescriptionId (int64 (10 + i)), OpenFileTarget.Socket (SocketId (int64 i))
            ]
            [
                yield
                    0L,
                    { someSocket with
                        Binding =
                            Some
                                {
                                    Endpoint = loopback port
                                    LockedAddress = None
                                }
                        Phase =
                            SocketPhase.Listening
                                {
                                    Backlog = backlog
                                    Queue = []
                                }
                    }
                for i in 1..clients -> int64 i, someSocket
            ]
            (int64 clients + 1L)

    let private connect
        (client : SocketId)
        (nonBlocking : bool)
        (dest : InternetEndpoint)
        (kernel : EmulatedKernel)
        : ConnectOutcome * EmulatedKernel
        =
        EmulatedKernel.connectSocket client nonBlocking 16 inetFamily (Some dest) kernel

    /// The write-back a guest cannot inspect: the queue's content, the
    /// connection's two addresses, and the client's implicit binding. A
    /// handler recording zeroes for any of them would survive every guest row.
    [<Test>]
    let ``a blocking connect establishes: queue content, addresses, binding, invariants`` () : unit =
        let kernel = listenerAndClients 8 5000us 1
        let outcome, kernel = connect (SocketId 1L) false (loopback 5000us) kernel
        outcome |> shouldEqual ConnectOutcome.Completed

        let client = UnixMachineState.socket (SocketId 1L) kernel.Machine

        let connectionId =
            match client.Phase with
            | SocketPhase.Established connectionId -> connectionId
            | other -> failwith $"expected Established, got %A{other}"

        match (UnixMachineState.socket (SocketId 0L) kernel.Machine).Phase with
        | SocketPhase.Listening listenState -> listenState.Queue |> shouldEqual [ connectionId ]
        | other -> failwith $"expected Listening, got %A{other}"

        let tcpConnection = UnixMachineState.connection connectionId kernel.Machine
        tcpConnection.ServerAddress |> shouldEqual (loopback 5000us)

        // The implicit bind: loopback, a nonzero port from the ephemeral
        // range, and the connection records exactly that endpoint.
        match client.Binding with
        | None -> failwith "expected the connect to bind the client implicitly"
        | Some binding ->
            binding.Endpoint.Address |> shouldEqual InternetEndpoint.LoopbackAddress
            let low, high = kernel.EphemeralPortRange

            (binding.Endpoint.Port >= low && binding.Endpoint.Port <= high)
            |> shouldEqual true

            tcpConnection.ClientAddress |> shouldEqual binding.Endpoint

        EmulatedKernel.checkInvariants kernel |> shouldEqual []

    [<Test>]
    let ``two connects queue oldest first`` () : unit =
        let kernel = listenerAndClients 8 5000us 2
        let _, kernel = connect (SocketId 1L) false (loopback 5000us) kernel
        let _, kernel = connect (SocketId 2L) false (loopback 5000us) kernel

        let connectionOf (client : int64) =
            match (UnixMachineState.socket (SocketId client) kernel.Machine).Phase with
            | SocketPhase.Established connectionId -> connectionId
            | other -> failwith $"expected Established, got %A{other}"

        match (UnixMachineState.socket (SocketId 0L) kernel.Machine).Phase with
        | SocketPhase.Listening listenState -> listenState.Queue |> shouldEqual [ connectionOf 1L ; connectionOf 2L ]
        | other -> failwith $"expected Listening, got %A{other}"

    /// Linux's completion report is deferred to the first retry, and the
    /// retry's answer carries the *same* connection — an identity no guest can
    /// compare.
    [<Test>]
    let ``a non-blocking Linux establishment pends the completion report`` () : unit =
        let kernel = listenerAndClients 8 5000us 1
        let outcome, kernel = connect (SocketId 1L) true (loopback 5000us) kernel

        outcome |> shouldEqual (ConnectOutcome.Failed UnixError.EINPROGRESS)

        let connectionId =
            match (UnixMachineState.socket (SocketId 1L) kernel.Machine).Phase with
            | SocketPhase.EstablishedPendingReport connectionId -> connectionId
            | other -> failwith $"expected EstablishedPendingReport, got %A{other}"

        let outcome, kernel = connect (SocketId 1L) true (loopback 5000us) kernel
        outcome |> shouldEqual ConnectOutcome.Completed

        match (UnixMachineState.socket (SocketId 1L) kernel.Machine).Phase with
        | SocketPhase.Established reported -> reported |> shouldEqual connectionId
        | other -> failwith $"expected Established, got %A{other}"

        let outcome, _ = connect (SocketId 1L) true (loopback 5000us) kernel
        outcome |> shouldEqual (ConnectOutcome.Failed UnixError.EISCONN)

    [<Test>]
    let ``a non-blocking Darwin establishment is Established at once`` () : unit =
        let kernel =
            let baseKernel = listenerAndClients 8 5000us 1

            { baseKernel with
                Machine =
                    { baseKernel.Machine with
                        UnixPlatform = SimulatedUnixPlatform.macOsArm64
                    }
            }

        let outcome, kernel = connect (SocketId 1L) true (loopback 5000us) kernel

        outcome |> shouldEqual (ConnectOutcome.Failed UnixError.EINPROGRESS)

        match (UnixMachineState.socket (SocketId 1L) kernel.Machine).Phase with
        | SocketPhase.Established _ -> ()
        | other -> failwith $"expected Established, got %A{other}"

        let outcome, _ = connect (SocketId 1L) true (loopback 5000us) kernel
        outcome |> shouldEqual (ConnectOutcome.Failed UnixError.EISCONN)

    [<Test>]
    let ``a refusal delivery resets a Linux socket and kills a Darwin one`` () : unit =
        // No listener anywhere: loopback:9999 refuses.
        let fresh () =
            forge [ 3, OpenFileDescriptionId 10L, OpenFileTarget.Socket (SocketId 0L) ] [ 0L, someSocket ] 1L

        // Linux: EINPROGRESS, delivery, then a fresh attempt.
        let outcome, kernel = connect (SocketId 0L) true (loopback 9999us) (fresh ())

        outcome |> shouldEqual (ConnectOutcome.Failed UnixError.EINPROGRESS)

        (UnixMachineState.socket (SocketId 0L) kernel.Machine).Phase
        |> shouldEqual SocketPhase.RefusedPendingDelivery

        let outcome, kernel = connect (SocketId 0L) true (loopback 9999us) kernel

        outcome |> shouldEqual (ConnectOutcome.Failed UnixError.ECONNREFUSED)

        (UnixMachineState.socket (SocketId 0L) kernel.Machine).Phase
        |> shouldEqual SocketPhase.Idle

        let outcome, _ = connect (SocketId 0L) true (loopback 9999us) kernel

        outcome |> shouldEqual (ConnectOutcome.Failed UnixError.EINPROGRESS)

        // Darwin: the delivery latches the socket dead.
        let darwin =
            let baseKernel = fresh ()

            { baseKernel with
                Machine =
                    { baseKernel.Machine with
                        UnixPlatform = SimulatedUnixPlatform.macOsArm64
                    }
            }

        let _, kernel = connect (SocketId 0L) true (loopback 9999us) darwin
        let outcome, kernel = connect (SocketId 0L) true (loopback 9999us) kernel

        outcome |> shouldEqual (ConnectOutcome.Failed UnixError.ECONNREFUSED)

        (UnixMachineState.socket (SocketId 0L) kernel.Machine).Phase
        |> shouldEqual SocketPhase.Dead

        let outcome, _ = connect (SocketId 0L) true (loopback 9999us) kernel
        outcome |> shouldEqual (ConnectOutcome.Failed UnixError.EINVAL)

    [<Test>]
    let ``an explicitly bound client keeps its binding through connect`` () : unit =
        let kernel = listenerAndClients 8 5000us 1

        let kernel =
            { kernel with
                Machine =
                    { kernel.Machine with
                        Sockets =
                            Map.add
                                (SocketId 1L)
                                { someSocket with
                                    Binding =
                                        Some
                                            {
                                                Endpoint = loopback 4444us
                                                LockedAddress = None
                                            }
                                }
                                kernel.Sockets
                    }
            }

        let _, kernel = connect (SocketId 1L) false (loopback 5000us) kernel

        let connectionId =
            match (UnixMachineState.socket (SocketId 1L) kernel.Machine).Phase with
            | SocketPhase.Established connectionId -> connectionId
            | other -> failwith $"expected Established, got %A{other}"

        (UnixMachineState.connection connectionId kernel.Machine).ClientAddress
        |> shouldEqual (loopback 4444us)

    /// Both error directions of the capacity boundary matter: recording where
    /// a kernel would pend is as wrong as refusing where it would record. The
    /// guests can only see the under side, so the exact boundary is pinned
    /// here.
    [<Test>]
    let ``the accept queue admits backlog plus one on Linux and refuses the next`` () : unit =
        let kernel = listenerAndClients 1 5000us 3
        let _, kernel = connect (SocketId 1L) false (loopback 5000us) kernel
        let _, kernel = connect (SocketId 2L) false (loopback 5000us) kernel

        let e =
            Assert.Throws<System.Exception> (fun () -> connect (SocketId 3L) false (loopback 5000us) kernel |> ignore)

        e.Message |> shouldContainText "its measured capacity"

    [<Test>]
    let ``the accept queue admits exactly backlog on Darwin`` () : unit =
        let kernel =
            let baseKernel = listenerAndClients 1 5000us 2

            { baseKernel with
                Machine =
                    { baseKernel.Machine with
                        UnixPlatform = SimulatedUnixPlatform.macOsArm64
                    }
            }

        let _, kernel = connect (SocketId 1L) false (loopback 5000us) kernel

        let e =
            Assert.Throws<System.Exception> (fun () -> connect (SocketId 2L) false (loopback 5000us) kernel |> ignore)

        e.Message |> shouldContainText "its measured capacity"

    /// The producer this slice exists for: a connect onto a registered
    /// listener queues the accept-queue-push edge, so the port has something
    /// to deliver.
    [<Test>]
    let ``connect onto a registered listener makes the registration pending and deliverable`` () : unit =
        let registration =
            {
                Interest = SocketEventsPal.toInterest "test" 0x3
                Data = 0xBEEFUL
                RegisteredAt = 0L
            }

        let kernel =
            forge
                [
                    3, OpenFileDescriptionId 10L, OpenFileTarget.Socket (SocketId 0L)
                    4, OpenFileDescriptionId 11L, OpenFileTarget.Socket (SocketId 1L)
                    9,
                    OpenFileDescriptionId 50L,
                    OpenFileTarget.SocketEventPort
                        {
                            Registrations = Map.ofList [ (3, OpenFileDescriptionId 10L), registration ]
                            Ready = []
                        }
                ]
                [
                    0L,
                    { someSocket with
                        Binding =
                            Some
                                {
                                    Endpoint = loopback 5000us
                                    LockedAddress = None
                                }
                        Phase =
                            SocketPhase.Listening
                                {
                                    Backlog = 8
                                    Queue = []
                                }
                    }
                    1L, someSocket
                ]
                2L

        let kernel =
            { kernel with
                Machine =
                    { kernel.Machine with
                        NextSocketEventRegistrationOrdinal = 1L
                    }
            }

        hasDeliverableSocketEvents (OpenFileDescriptionId 50L) kernel
        |> shouldEqual false

        let outcome, kernel = connect (SocketId 1L) false (loopback 5000us) kernel

        outcome |> shouldEqual ConnectOutcome.Completed

        hasDeliverableSocketEvents (OpenFileDescriptionId 50L) kernel
        |> shouldEqual true

        let delivered, kernel = deliverSocketEvents (OpenFileDescriptionId 50L) 8 kernel

        delivered
        |> shouldEqual
            [
                0xBEEFUL,
                { ReadinessLevel.none with
                    In = true
                }
            ]

        // Consumed: nothing further until the next edge.
        hasDeliverableSocketEvents (OpenFileDescriptionId 50L) kernel
        |> shouldEqual false

    [<Test>]
    let ``accept dequeues the head, materialises the server end, and inherits SO_REUSEADDR`` () : unit =
        let kernel = listenerAndClients 8 5000us 2

        let kernel =
            { kernel with
                Machine =
                    { kernel.Machine with
                        Sockets =
                            kernel.Sockets
                            |> Map.add
                                (SocketId 0L)
                                { UnixMachineState.socket (SocketId 0L) kernel.Machine with
                                    ReuseAddress = true
                                }
                    }
            }

        let _, kernel = connect (SocketId 1L) false (loopback 5000us) kernel
        let _, kernel = connect (SocketId 2L) false (loopback 5000us) kernel

        let connectionOf (client : int64) =
            match (UnixMachineState.socket (SocketId client) kernel.Machine).Phase with
            | SocketPhase.Established connectionId -> connectionId
            | other -> failwith $"expected Established, got %A{other}"

        let fd, tcpConnection, kernel = EmulatedKernel.acceptConnection (SocketId 0L) kernel
        let firstClient = UnixMachineState.connection (connectionOf 1L) kernel.Machine
        tcpConnection |> shouldEqual firstClient

        let acceptedId =
            match FileDescriptorRegistry.tryFind fd kernel.FileDescriptors with
            | Some description ->
                match description.Target with
                | OpenFileTarget.Socket socketId -> socketId
                | other -> failwith $"expected a socket target, got %O{other}"
            | None -> failwith "the accepted descriptor is not live"

        let accepted = UnixMachineState.socket acceptedId kernel.Machine
        accepted.Phase |> shouldEqual (SocketPhase.Established (connectionOf 1L))

        accepted.Binding
        |> shouldEqual (
            Some
                {
                    Endpoint = loopback 5000us
                    LockedAddress = None
                }
        )

        accepted.ReuseAddress |> shouldEqual true

        match (UnixMachineState.socket (SocketId 0L) kernel.Machine).Phase with
        | SocketPhase.Listening listenState -> listenState.Queue |> shouldEqual [ connectionOf 2L ]
        | other -> failwith $"expected Listening, got %A{other}"

        EmulatedKernel.checkInvariants kernel |> shouldEqual []

        let _, _, kernel = EmulatedKernel.acceptConnection (SocketId 0L) kernel

        let e =
            Assert.Throws<System.Exception> (fun () -> EmulatedKernel.acceptConnection (SocketId 0L) kernel |> ignore)

        e.Message |> shouldContainText "the accept queue is empty"

    /// The sweep: a connection lives exactly while a socket phase or an accept
    /// queue references it.
    [<Test>]
    let ``closing the last reference sweeps the connection`` () : unit =
        let kernel = listenerAndClients 8 5000us 1
        let _, kernel = connect (SocketId 1L) false (loopback 5000us) kernel
        let acceptedFd, _, kernel = EmulatedKernel.acceptConnection (SocketId 0L) kernel

        let connectionId =
            match (UnixMachineState.socket (SocketId 1L) kernel.Machine).Phase with
            | SocketPhase.Established connectionId -> connectionId
            | other -> failwith $"expected Established, got %A{other}"

        // The client dies; the accepted socket still references the
        // connection, so it survives.
        let kernel =
            match KernelSyscall.close 4 kernel with
            | Ok kernel -> kernel
            | Error e -> failwith $"expected close to succeed, got %O{e}"

        Map.containsKey connectionId kernel.Connections |> shouldEqual true
        EmulatedKernel.checkInvariants kernel |> shouldEqual []

        // The accepted socket dies too; nothing references the connection.
        let kernel =
            match KernelSyscall.close acceptedFd kernel with
            | Ok kernel -> kernel
            | Error e -> failwith $"expected close to succeed, got %O{e}"

        Map.containsKey connectionId kernel.Connections |> shouldEqual false
        EmulatedKernel.checkInvariants kernel |> shouldEqual []

    /// A queued connection outlives its client (measured: accept still returns
    /// it), and dies with the listener once nothing else references it.
    [<Test>]
    let ``a queued connection survives its client and dies with its listener`` () : unit =
        let kernel = listenerAndClients 8 5000us 1
        let _, kernel = connect (SocketId 1L) false (loopback 5000us) kernel

        let connectionId =
            match (UnixMachineState.socket (SocketId 1L) kernel.Machine).Phase with
            | SocketPhase.Established connectionId -> connectionId
            | other -> failwith $"expected Established, got %A{other}"

        let kernel =
            match KernelSyscall.close 4 kernel with
            | Ok kernel -> kernel
            | Error e -> failwith $"expected close to succeed, got %O{e}"

        // Still queued, so still alive — this is what lets a later accept
        // return it.
        Map.containsKey connectionId kernel.Connections |> shouldEqual true
        EmulatedKernel.checkInvariants kernel |> shouldEqual []

        let kernel =
            match KernelSyscall.close 3 kernel with
            | Ok kernel -> kernel
            | Error e -> failwith $"expected close to succeed, got %O{e}"

        Map.containsKey connectionId kernel.Connections |> shouldEqual false
        EmulatedKernel.checkInvariants kernel |> shouldEqual []

    /// A datagram connect stores the peer it was given — a value with no other
    /// observer until a transfer syscall reads the filter — and the Linux
    /// dissolve clears it while keeping the binding.
    [<Test>]
    let ``a datagram connect records the peer and the Linux dissolve clears it`` () : unit =
        let kernel =
            forge
                [ 3, OpenFileDescriptionId 10L, OpenFileTarget.Socket (SocketId 0L) ]
                [
                    0L,
                    { someSocket with
                        Kind = SocketKind.Datagram
                        Protocol = SocketProtocol.Udp
                    }
                ]
                1L

        let outcome, kernel = connect (SocketId 0L) false (loopback 9999us) kernel
        outcome |> shouldEqual ConnectOutcome.Completed

        (UnixMachineState.socket (SocketId 0L) kernel.Machine).Phase
        |> shouldEqual (SocketPhase.DatagramPeer (loopback 9999us))

        // Re-connect re-targets.
        let _, kernel = connect (SocketId 0L) false (loopback 1234us) kernel

        (UnixMachineState.socket (SocketId 0L) kernel.Machine).Phase
        |> shouldEqual (SocketPhase.DatagramPeer (loopback 1234us))

        // AF_UNSPEC dissolves on Linux, and — measured, unlike TCP's reset —
        // drops the implicit binding entirely, port included.
        let outcome, kernel =
            EmulatedKernel.connectSocket (SocketId 0L) false 16 (Some 0) None kernel

        outcome |> shouldEqual ConnectOutcome.Completed
        let socket = UnixMachineState.socket (SocketId 0L) kernel.Machine
        socket.Phase |> shouldEqual SocketPhase.Idle
        socket.Binding |> shouldEqual None

        // A re-connect binds afresh.
        let outcome, kernel = connect (SocketId 0L) false (loopback 1234us) kernel
        outcome |> shouldEqual ConnectOutcome.Completed

        match (UnixMachineState.socket (SocketId 0L) kernel.Machine).Binding with
        | Some binding -> (binding.Endpoint.Port > 0us) |> shouldEqual true
        | None -> failwith "expected the re-connect to bind the socket"

        // Darwin refuses the dissolve instead.
        let darwin =
            { kernel with
                Machine =
                    { kernel.Machine with
                        UnixPlatform = SimulatedUnixPlatform.macOsArm64
                    }
            }

        let outcome, _ =
            EmulatedKernel.connectSocket (SocketId 0L) false 16 (Some 0) None darwin

        outcome |> shouldEqual (ConnectOutcome.Failed UnixError.EAFNOSUPPORT)

    // --- the connection-table invariants ---

    [<Test>]
    let ``checkInvariants rejects a phase naming a dead connection`` () : unit =
        let kernel =
            forge
                [ 3, OpenFileDescriptionId 10L, OpenFileTarget.Socket (SocketId 0L) ]
                [
                    0L,
                    { someSocket with
                        Phase = SocketPhase.Established (ConnectionId 5L)
                    }
                ]
                1L

        EmulatedKernel.checkInvariants kernel
        |> shouldEqual
            [
                EmulatedKernelDefect.System (UnixSystemDefect.DanglingConnection (SocketId 0L, ConnectionId 5L))
            ]

    [<Test>]
    let ``checkInvariants rejects a queue naming a dead connection`` () : unit =
        let kernel =
            forge
                [ 3, OpenFileDescriptionId 10L, OpenFileTarget.Socket (SocketId 0L) ]
                [
                    0L,
                    { someSocket with
                        Phase =
                            SocketPhase.Listening
                                {
                                    Backlog = 8
                                    Queue = [ ConnectionId 5L ]
                                }
                    }
                ]
                1L

        EmulatedKernel.checkInvariants kernel
        |> shouldEqual
            [
                EmulatedKernelDefect.System (UnixSystemDefect.DanglingQueuedConnection (SocketId 0L, ConnectionId 5L))
            ]

    [<Test>]
    let ``checkInvariants rejects an orphan connection and a stale counter`` () : unit =
        let kernel =
            forge [ 3, OpenFileDescriptionId 10L, OpenFileTarget.Socket (SocketId 0L) ] [ 0L, someSocket ] 1L

        let kernel =
            { kernel with
                Machine =
                    { kernel.Machine with
                        Connections =
                            Map.ofList
                                [
                                    ConnectionId 2L,
                                    {
                                        ClientAddress = loopback 1us
                                        ServerAddress = loopback 2us
                                    }
                                ]
                        NextConnectionId = ConnectionId 1L
                    }
            }

        EmulatedKernel.checkInvariants kernel
        |> shouldEqual
            [
                EmulatedKernelDefect.System (UnixSystemDefect.OrphanConnection (ConnectionId 2L))
                EmulatedKernelDefect.System (
                    UnixSystemDefect.NextConnectionIdNotFresh (ConnectionId 1L, ConnectionId 2L)
                )
            ]

    [<Test>]
    let ``checkInvariants rejects a phase a socket's kind cannot enter`` () : unit =
        let kernel =
            forge
                [ 3, OpenFileDescriptionId 10L, OpenFileTarget.Socket (SocketId 0L) ]
                [
                    0L,
                    { someSocket with
                        Kind = SocketKind.Datagram
                        Phase = SocketPhase.RefusedPendingDelivery
                    }
                ]
                1L

        EmulatedKernel.checkInvariants kernel
        |> shouldEqual
            [
                EmulatedKernelDefect.System (
                    UnixSystemDefect.SocketPhaseKindMismatch (
                        SocketId 0L,
                        SocketKind.Datagram,
                        SocketPhase.RefusedPendingDelivery
                    )
                )
            ]

    [<Test>]
    let ``checkInvariants rejects a connection queued twice`` () : unit =
        let kernel =
            forge
                [ 3, OpenFileDescriptionId 10L, OpenFileTarget.Socket (SocketId 0L) ]
                [
                    0L,
                    { someSocket with
                        Phase =
                            SocketPhase.Listening
                                {
                                    Backlog = 8
                                    Queue = [ ConnectionId 0L ; ConnectionId 0L ]
                                }
                    }
                ]
                1L

        let kernel =
            { kernel with
                Machine =
                    { kernel.Machine with
                        Connections =
                            Map.ofList
                                [
                                    ConnectionId 0L,
                                    {
                                        ClientAddress = loopback 1us
                                        ServerAddress = loopback 2us
                                    }
                                ]
                        NextConnectionId = ConnectionId 1L
                    }
            }

        EmulatedKernel.checkInvariants kernel
        |> shouldEqual
            [
                EmulatedKernelDefect.System (UnixSystemDefect.DuplicateQueuedConnection (ConnectionId 0L))
            ]

    /// A wildcard-bound listener receives a loopback-destined connect — the
    /// shape `listen(2)`'s implicit bind creates, which no guest reaches yet
    /// (they all bind loopback explicitly).
    [<Test>]
    let ``a wildcard-bound listener receives a loopback connect`` () : unit =
        let kernel = listenerAndClients 8 5000us 1

        let kernel =
            { kernel with
                Machine =
                    { kernel.Machine with
                        Sockets =
                            Map.add
                                (SocketId 0L)
                                { UnixMachineState.socket (SocketId 0L) kernel.Machine with
                                    Binding =
                                        Some
                                            {
                                                Endpoint =
                                                    InternetEndpoint.ofParts InternetEndpoint.WildcardAddress 5000us
                                                LockedAddress = None
                                            }
                                }
                                kernel.Sockets
                    }
            }

        let outcome, kernel = connect (SocketId 1L) false (loopback 5000us) kernel
        outcome |> shouldEqual ConnectOutcome.Completed

        let connectionId =
            match (UnixMachineState.socket (SocketId 1L) kernel.Machine).Phase with
            | SocketPhase.Established connectionId -> connectionId
            | other -> failwith $"expected Established, got %A{other}"

        // The connection's server address is the *destination*, not the
        // wildcard the listener bound: the accepted socket's getsockname
        // reports loopback.
        (UnixMachineState.connection connectionId kernel.Machine).ServerAddress
        |> shouldEqual (loopback 5000us)

    /// The somaxconn clamp, at a forged sysctl of 3 so the boundary is in
    /// reach — the shape it was measured in. Both error directions matter:
    /// admitting where a kernel would pend is as wrong as refusing where it
    /// would admit.
    [<Test>]
    let ``a Linux backlog clamps to somaxconn before the plus-one`` () : unit =
        for backlog in [ System.Int32.MaxValue ; -1 ] do
            let kernel =
                let baseKernel = listenerAndClients backlog 5000us 5

                { baseKernel with
                    Machine =
                        { baseKernel.Machine with
                            SoMaxConn = 3
                        }
                }

            // Capacity is somaxconn + 1 = 4: four connects land, the fifth
            // refuses.
            let kernel =
                (kernel, [ 1L .. 4L ])
                ||> List.fold (fun kernel client ->
                    let outcome, kernel = connect (SocketId client) false (loopback 5000us) kernel
                    outcome |> shouldEqual ConnectOutcome.Completed
                    kernel
                )

            let e =
                Assert.Throws<System.Exception> (fun () ->
                    connect (SocketId 5L) false (loopback 5000us) kernel |> ignore
                )

            e.Message |> shouldContainText "its measured capacity"

    [<Test>]
    let ``a Darwin backlog clamps to somaxconn with no plus-one`` () : unit =
        for backlog in [ System.Int32.MaxValue ; 0 ; -1 ] do
            let kernel =
                let baseKernel = listenerAndClients backlog 5000us 4

                { baseKernel with
                    Machine =
                        { baseKernel.Machine with
                            UnixPlatform = SimulatedUnixPlatform.macOsArm64
                            SoMaxConn = 3
                        }
                }

            // Capacity is exactly somaxconn = 3.
            let kernel =
                (kernel, [ 1L .. 3L ])
                ||> List.fold (fun kernel client ->
                    let outcome, kernel = connect (SocketId client) false (loopback 5000us) kernel
                    outcome |> shouldEqual ConnectOutcome.Completed
                    kernel
                )

            let e =
                Assert.Throws<System.Exception> (fun () ->
                    connect (SocketId 4L) false (loopback 5000us) kernel |> ignore
                )

            e.Message |> shouldContainText "its measured capacity"

    /// A wildcard-bound client resolves to a concrete loopback source at
    /// connect — in the *binding* (getsockname reports it) and in the
    /// connection's record of the peer.
    [<Test>]
    let ``a wildcard-bound client resolves to loopback at connect, keeping its port`` () : unit =
        let kernel = listenerAndClients 8 5000us 1

        let kernel =
            { kernel with
                Machine =
                    { kernel.Machine with
                        Sockets =
                            Map.add
                                (SocketId 1L)
                                { someSocket with
                                    Binding =
                                        Some
                                            {
                                                Endpoint =
                                                    InternetEndpoint.ofParts InternetEndpoint.WildcardAddress 4444us
                                                LockedAddress = None
                                            }
                                }
                                kernel.Sockets
                    }
            }

        let outcome, kernel = connect (SocketId 1L) false (loopback 5000us) kernel
        outcome |> shouldEqual ConnectOutcome.Completed

        let client = UnixMachineState.socket (SocketId 1L) kernel.Machine

        client.Binding
        |> shouldEqual (
            Some
                {
                    Endpoint = loopback 4444us
                    LockedAddress = None
                }
        )

        let connectionId =
            match client.Phase with
            | SocketPhase.Established connectionId -> connectionId
            | other -> failwith $"expected Established, got %A{other}"

        (UnixMachineState.connection connectionId kernel.Machine).ClientAddress
        |> shouldEqual (loopback 4444us)

    /// The largest configurable sysctl must not wrap the Linux `+ 1`: a
    /// backlog clamped to Int32.MaxValue still admits the first connection.
    [<Test>]
    let ``a maximal somaxconn does not overflow the Linux capacity`` () : unit =
        let kernel =
            let baseKernel = listenerAndClients System.Int32.MaxValue 5000us 1

            { baseKernel with
                Machine =
                    { baseKernel.Machine with
                        SoMaxConn = System.Int32.MaxValue
                    }
            }

        let outcome, kernel = connect (SocketId 1L) false (loopback 5000us) kernel
        outcome |> shouldEqual ConnectOutcome.Completed
        EmulatedKernel.checkInvariants kernel |> shouldEqual []

    /// The refusal's effect on the local binding, across all three bind
    /// provenances (implicit; bind(2) to loopback; bind(2) to the wildcard) —
    /// measured: the pending attempt resolves the source to loopback on both
    /// flavours, and the delivery then reverts it to whatever bind(2) locked
    /// on Linux (the wildcard when nothing was) while Darwin keeps it.
    [<Test>]
    let ``a refusal delivery reverts the source to what bind was given on Linux and keeps it on Darwin`` () : unit =
        let provenances =
            [
                // (pre-connect binding, Linux post-delivery address)
                None, InternetEndpoint.WildcardAddress
                Some
                    {
                        Endpoint = loopback 4444us
                        LockedAddress = Some InternetEndpoint.LoopbackAddress
                    },
                InternetEndpoint.LoopbackAddress
                Some
                    {
                        Endpoint = InternetEndpoint.ofParts InternetEndpoint.WildcardAddress 4444us
                        LockedAddress = Some InternetEndpoint.WildcardAddress
                    },
                InternetEndpoint.WildcardAddress
            ]

        for preBinding, linuxAddress in provenances do
            for darwin in [ false ; true ] do
                let kernel =
                    forge [ 3, OpenFileDescriptionId 10L, OpenFileTarget.Socket (SocketId 0L) ] [ 0L, someSocket ] 1L

                let kernel =
                    { kernel with
                        Machine =
                            { kernel.Machine with
                                UnixPlatform =
                                    if darwin then
                                        SimulatedUnixPlatform.macOsArm64
                                    else
                                        kernel.UnixPlatform
                                Sockets =
                                    Map.add
                                        (SocketId 0L)
                                        { someSocket with
                                            Binding = preBinding
                                        }
                                        kernel.Sockets
                            }
                    }

                let outcome, kernel = connect (SocketId 0L) true (loopback 9999us) kernel

                outcome |> shouldEqual (ConnectOutcome.Failed UnixError.EINPROGRESS)

                // While the refusal pends, the source reads as resolved
                // loopback with a nonzero port on both flavours.
                let pending =
                    match (UnixMachineState.socket (SocketId 0L) kernel.Machine).Binding with
                    | Some binding -> binding
                    | None -> failwith "expected the pending attempt to have bound the socket"

                pending.Endpoint.Address |> shouldEqual InternetEndpoint.LoopbackAddress
                (pending.Endpoint.Port > 0us) |> shouldEqual true

                let outcome, kernel = connect (SocketId 0L) true (loopback 9999us) kernel

                outcome |> shouldEqual (ConnectOutcome.Failed UnixError.ECONNREFUSED)

                let delivered =
                    match (UnixMachineState.socket (SocketId 0L) kernel.Machine).Binding with
                    | Some binding -> binding
                    | None -> failwith "expected the delivery to keep the socket bound"

                delivered.Endpoint.Port |> shouldEqual pending.Endpoint.Port

                delivered.Endpoint.Address
                |> shouldEqual (
                    if darwin then
                        InternetEndpoint.LoopbackAddress
                    else
                        linuxAddress
                )

    /// Darwin drops a SYN only for a port held by a *bound but unconnected*
    /// socket. A port held by established ends (after their listener closed)
    /// or by a refused socket answers RST like a closed port — measured on
    /// both kernels — so those connects take the ordinary refusal path.
    [<Test>]
    let ``Darwin drops a SYN only toward a bound idle socket`` () : unit =
        let kernelWith (phase : SocketPhase) (connections : Map<ConnectionId, TcpConnection>) =
            let kernel =
                forge
                    [
                        3, OpenFileDescriptionId 10L, OpenFileTarget.Socket (SocketId 0L)
                        4, OpenFileDescriptionId 11L, OpenFileTarget.Socket (SocketId 1L)
                    ]
                    [
                        0L,
                        { someSocket with
                            Binding =
                                Some
                                    {
                                        Endpoint = loopback 5000us
                                        LockedAddress = Some InternetEndpoint.LoopbackAddress
                                    }
                            Phase = phase
                        }
                        1L, someSocket
                    ]
                    2L

            { kernel with
                Machine =
                    { kernel.Machine with
                        UnixPlatform = SimulatedUnixPlatform.macOsArm64
                        Connections = connections
                        NextConnectionId = ConnectionId 1L
                    }
            }

        // Bound and idle: the measured dropped-SYN refusal.
        let e =
            Assert.Throws<System.Exception> (fun () ->
                connect (SocketId 1L) false (loopback 5000us) (kernelWith SocketPhase.Idle Map.empty)
                |> ignore
            )

        e.Message |> shouldContainText "rather than answering RST"

        // Established at the port (its listener long closed): RST, so the
        // connect is refused like any closed port — and this socket latches
        // dead, as a refused Darwin socket does.
        let established =
            kernelWith
                (SocketPhase.Established (ConnectionId 0L))
                (Map.ofList
                    [
                        ConnectionId 0L,
                        {
                            ClientAddress = loopback 4000us
                            ServerAddress = loopback 5000us
                        }
                    ])

        let outcome, kernel = connect (SocketId 1L) false (loopback 5000us) established

        outcome |> shouldEqual (ConnectOutcome.Failed UnixError.ECONNREFUSED)

        (UnixMachineState.socket (SocketId 1L) kernel.Machine).Phase
        |> shouldEqual SocketPhase.Dead

        // A refused socket's port likewise answers RST.
        let outcome, _ =
            connect (SocketId 1L) false (loopback 5000us) (kernelWith SocketPhase.RefusedPendingDelivery Map.empty)

        outcome |> shouldEqual (ConnectOutcome.Failed UnixError.ECONNREFUSED)

    /// The two four-tuple corners a REUSEADDR-bound client can engineer are
    /// refused by name: a resolved source equal to the destination even with
    /// a listener present, and a duplicate (source, destination) pair. Both
    /// real answers are unmeasured, and both refusals fire only on the exact
    /// input — the ordinary two-client shape stays fine.
    [<Test>]
    let ``a self-tuple and a duplicate four-tuple both refuse by name`` () : unit =
        // Self-tuple: the client is (legally, under REUSEADDR) bound to the
        // very endpoint the wildcard listener covers.
        let kernel = listenerAndClients 8 5000us 1

        let kernel =
            { kernel with
                Machine =
                    { kernel.Machine with
                        Sockets =
                            kernel.Sockets
                            |> Map.add
                                (SocketId 0L)
                                { UnixMachineState.socket (SocketId 0L) kernel.Machine with
                                    Binding =
                                        Some
                                            {
                                                Endpoint =
                                                    InternetEndpoint.ofParts InternetEndpoint.WildcardAddress 5000us
                                                LockedAddress = Some InternetEndpoint.WildcardAddress
                                            }
                                }
                            |> Map.add
                                (SocketId 1L)
                                { someSocket with
                                    Binding =
                                        Some
                                            {
                                                Endpoint = loopback 5000us
                                                LockedAddress = Some InternetEndpoint.LoopbackAddress
                                            }
                                }
                    }
            }

        let e =
            Assert.Throws<System.Exception> (fun () -> connect (SocketId 1L) false (loopback 5000us) kernel |> ignore)

        e.Message |> shouldContainText "equals the destination, with a listener present"

        // Duplicate tuple: two clients bound to one source endpoint, both
        // connecting to the listener.
        let kernel = listenerAndClients 8 5000us 2

        let boundAt (socketId : int64) (kernel : EmulatedKernel) =
            { kernel with
                Machine =
                    { kernel.Machine with
                        Sockets =
                            Map.add
                                (SocketId socketId)
                                { someSocket with
                                    Binding =
                                        Some
                                            {
                                                Endpoint = loopback 4444us
                                                LockedAddress = Some InternetEndpoint.LoopbackAddress
                                            }
                                }
                                kernel.Sockets
                    }
            }

        let kernel = kernel |> boundAt 1L |> boundAt 2L
        let outcome, kernel = connect (SocketId 1L) false (loopback 5000us) kernel
        outcome |> shouldEqual ConnectOutcome.Completed

        let e =
            Assert.Throws<System.Exception> (fun () -> connect (SocketId 2L) false (loopback 5000us) kernel |> ignore)

        e.Message |> shouldContainText "refuses a duplicate four-tuple"

    /// A connection outliving its closed client keeps its four-tuple
    /// occupied, and the ephemeral allocator must skip that port for the
    /// same destination — a real kernel's connect-time selection does — or
    /// an innocent fresh client aborts on the duplicate-tuple refusal.
    [<Test>]
    let ``the implicit bind skips a port whose tuple a live connection occupies`` () : unit =
        let kernel =
            let baseKernel = listenerAndClients 8 5000us 2

            { baseKernel with
                Machine =
                    { baseKernel.Machine with
                        EphemeralPortRange = 40000us, 40001us
                        NextEphemeralPort = 40000us
                    }
            }

        let _, kernel = connect (SocketId 1L) false (loopback 5000us) kernel

        let firstPort =
            match (UnixMachineState.socket (SocketId 1L) kernel.Machine).Binding with
            | Some binding -> binding.Endpoint.Port
            | None -> failwith "expected the connect to bind the client"

        firstPort |> shouldEqual 40000us

        // The client dies; its connection stays queued, so the tuple lives.
        let kernel =
            match KernelSyscall.close 4 kernel with
            | Ok kernel -> kernel
            | Error e -> failwith $"expected close to succeed, got %O{e}"

        // Rewind the cursor onto the occupied port, as a wrapped sweep would
        // land: the hazard is precisely the allocator re-offering it.
        let kernel =
            { kernel with
                Machine =
                    { kernel.Machine with
                        NextEphemeralPort = 40000us
                    }
            }

        // The next client must take the other port rather than colliding.
        let outcome, kernel = connect (SocketId 2L) false (loopback 5000us) kernel
        outcome |> shouldEqual ConnectOutcome.Completed

        match (UnixMachineState.socket (SocketId 2L) kernel.Machine).Binding with
        | Some binding -> binding.Endpoint.Port |> shouldEqual 40001us
        | None -> failwith "expected the connect to bind the client"

    /// The duplicate-tuple refusal sees both orientations: a connection's
    /// endpoint pair occupies the tuple from either end.
    [<Test>]
    let ``a reverse-orientation duplicate four-tuple also refuses`` () : unit =
        let kernel = listenerAndClients 8 5000us 1

        let kernel =
            { kernel with
                Machine =
                    { kernel.Machine with
                        Sockets =
                            Map.add
                                (SocketId 1L)
                                { someSocket with
                                    Binding =
                                        Some
                                            {
                                                Endpoint = loopback 4444us
                                                LockedAddress = Some InternetEndpoint.LoopbackAddress
                                            }
                                }
                                kernel.Sockets
                        Connections =
                            Map.ofList
                                [
                                    ConnectionId 0L,
                                    {
                                        // The existing connection ran the other way:
                                        // from the listener's endpoint to the
                                        // client's.
                                        ClientAddress = loopback 5000us
                                        ServerAddress = loopback 4444us
                                    }
                                ]
                        NextConnectionId = ConnectionId 1L
                    }
            }

        let e =
            Assert.Throws<System.Exception> (fun () -> connect (SocketId 1L) false (loopback 5000us) kernel |> ignore)

        e.Message |> shouldContainText "refuses a duplicate four-tuple"
