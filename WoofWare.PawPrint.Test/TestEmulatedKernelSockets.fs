namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The socket table and the descriptor table have to agree, and neither module
/// can check that alone: `FileDescriptorRegistry` compiles before the kernel
/// that holds the sockets. These are the claims about the pair.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEmulatedKernelSockets =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

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
            FileDescriptors = registry
            Sockets = sockets |> List.map (fun (id, socket) -> SocketId id, socket) |> Map.ofList
            NextSocketId = SocketId nextSocketId
        }

    let private someSocket : SocketDescription =
        {
            Domain = SocketDomain.InterNetwork
            Kind = SocketKind.Stream
            Protocol = SocketProtocol.Tcp
            Binding = None
            IsListening = false
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
            let socket = EmulatedKernel.socket socketId kernel
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

        match EmulatedKernel.closeFd fd kernel with
        | Error e -> failwith $"expected close to succeed, got %O{e}"
        | Ok kernel ->

        kernel.Sockets |> shouldEqual Map.empty
        // Not rewound: identities are never reused, so the next socket does not
        // take the dead one's name.
        kernel.NextSocketId |> shouldEqual (SocketId 1L)
        EmulatedKernel.checkInvariants kernel |> shouldEqual []

    /// ...and closing one of two descriptors onto a socket destroys neither the
    /// description nor the socket. This is the half that a `closeFd` keying off
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
                    FileDescriptors = registry
                }
            | Error e -> failwith $"expected dup to succeed, got %O{e}"

        match EmulatedKernel.closeFd duped kernel with
        | Error e -> failwith $"expected close to succeed, got %O{e}"
        | Ok kernel ->

        kernel.Sockets |> Map.count |> shouldEqual 1
        EmulatedKernel.checkInvariants kernel |> shouldEqual []

        // And the surviving descriptor still resolves to it.
        match FileDescriptorRegistry.tryFind fd kernel.FileDescriptors with
        | Some description ->
            match description.Target with
            | OpenFileTarget.Socket socketId -> EmulatedKernel.socket socketId kernel |> shouldEqual someSocket
            | other -> failwith $"expected a socket target, got %O{other}"
        | None -> failwith "the original descriptor should still be live"

    /// Closing a descriptor that names something other than a socket must leave
    /// the socket table alone. Without this, a `closeFd` that cleared the table
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
                FileDescriptors = registry
            }

        match EmulatedKernel.closeFd port kernel with
        | Error e -> failwith $"expected close to succeed, got %O{e}"
        | Ok kernel ->

        kernel.Sockets |> Map.count |> shouldEqual 1
        EmulatedKernel.checkInvariants kernel |> shouldEqual []

    /// A description naming a socket the table does not hold. `EmulatedKernel.socket`
    /// is total against this, so without the check it would surface as an
    /// interpreter crash at some unrelated call site instead.
    [<Test>]
    let ``checkInvariants rejects a description naming no socket`` () : unit =
        forge [ 0, OpenFileDescriptionId 7L, OpenFileTarget.Socket (SocketId 5L) ] [] 6L
        |> EmulatedKernel.checkInvariants
        |> shouldEqual [ EmulatedKernelDefect.DanglingSocket (OpenFileDescriptionId 7L, SocketId 5L) ]

    /// A socket no description names. Today that means a close forgot to clean
    /// up; `SystemNative_Accept` is what will make it legal.
    [<Test>]
    let ``checkInvariants rejects a socket no description names`` () : unit =
        forge [] [ 5L, someSocket ] 6L
        |> EmulatedKernel.checkInvariants
        |> shouldEqual [ EmulatedKernelDefect.UnreferencedSocket (SocketId 5L) ]

    /// A socket identity at or above the cursor would be minted again by the next
    /// `socket(2)`, giving two sockets one identity — and hence, through
    /// `OpenFileObject`, one `flock` contention key. `NextIdNotFresh`'s sibling.
    [<TestCase(5L)>]
    [<TestCase(0L)>]
    let ``checkInvariants rejects a NextSocketId at or below a live socket`` (next : int64) : unit =
        forge [ 0, OpenFileDescriptionId 7L, OpenFileTarget.Socket (SocketId 5L) ] [ 5L, someSocket ] next
        |> EmulatedKernel.checkInvariants
        |> shouldEqual [ EmulatedKernelDefect.NextSocketIdNotFresh (SocketId next, SocketId 5L) ]

    [<Test>]
    let ``checkInvariants accepts a NextSocketId above every live socket`` () : unit =
        forge [ 0, OpenFileDescriptionId 7L, OpenFileTarget.Socket (SocketId 5L) ] [ 5L, someSocket ] 6L
        |> EmulatedKernel.checkInvariants
        |> shouldEqual []

    /// `EmulatedKernel.socket` is total against the invariant rather than
    /// optional, so the one state that could defeat it must fail loudly.
    [<Test>]
    let ``resolving an unknown socket identity fails loudly`` () : unit =
        let e =
            Assert.Throws<System.Exception> (fun () ->
                EmulatedKernel.socket (SocketId 99L) EmulatedKernel.initial |> ignore
            )

        e.Message |> shouldContainText "names no socket in this kernel's socket table"

    /// A handful of files to open and unlink, so the property below works on a
    /// filesystem rather than on fabricated inode numbers. Names in one flat
    /// directory: this property is about descriptor and inode *lifetime*, and a
    /// deeper tree would add walking without adding a lifetime.
    let private lifetimeSeed : Map<FileName, SeedEntry> =
        [ "a" ; "b" ; "c" ; "d" ; "e" ]
        |> List.map (fun n ->
            FileName.parseOrFail "test seed" n,
            SeedEntry.file (System.Text.Encoding.UTF8.GetBytes n |> ImmutableArray.CreateRange)
        )
        |> Map.ofList

    /// The allocating and closing operations interleaved at random must leave
    /// *all three* tables sound. This is what connects the hand-forged defects
    /// above to the code paths that maintain them: a `closeFd` that forgot the
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
        let mutable observedSocketCloses = 0
        let mutable observedDups = 0
        let mutable observedUnlinks = 0
        let mutable observedReaps = 0

        let property (NonNegativeInt seed : NonNegativeInt) : unit =
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

                    match EmulatedKernel.closeFd chosen kernel with
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
                                FileDescriptors = registry
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
                                FileDescriptors = registry
                            }
                | 7 ->
                    // Remove a name at random, and reap if that was the last
                    // reference. Interleaved with the closes above, so the two
                    // orders — last name first, last descriptor first — both
                    // occur.
                    let root = VirtualFileSystem.root kernel.FileSystem

                    let names =
                        match VirtualFileSystem.tryGetContent root kernel.FileSystem with
                        | Some (InodeContent.Directory content) -> content.Entries |> Map.toList |> List.map fst
                        | Some (InodeContent.RegularFile _)
                        | Some (InodeContent.Symlink _)
                        | None -> []

                    if not (List.isEmpty names) then
                        let chosen = names.[rng.Next names.Length]

                        match
                            VirtualFileSystem.unbind root chosen (EmulatedKernel.fileTimestamp kernel) kernel.FileSystem
                        with
                        | Error e -> failwith $"unexpected unbind error: %O{e}"
                        | Ok (inode, filesystem) ->
                            let before = VirtualFileSystem.inodes filesystem |> Map.count

                            kernel <-
                                { kernel with
                                    FileSystem = filesystem
                                }
                                |> EmulatedKernel.forgetIfUnheld inode

                            observedUnlinks <- observedUnlinks + 1

                            if VirtualFileSystem.inodes kernel.FileSystem |> Map.count < before then
                                observedReaps <- observedReaps + 1
                | 6 ->
                    let _, registry =
                        FileDescriptorRegistry.createSocketEventPort kernel.FileDescriptors

                    kernel <-
                        { kernel with
                            FileDescriptors = registry
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

                VirtualFileSystem.checkInvariants (EmulatedKernel.pinnedInodes kernel) kernel.FileSystem
                |> shouldEqual []

        Check.One (propertyConfig, property)

        // Without these the run could be sound while never having exercised the
        // operations the clauses are about — a close that never fell on a socket
        // would leave the `UnreferencedSocket` clause untouched throughout.
        observedSockets |> shouldBeGreaterThan 500
        observedSocketCloses |> shouldBeGreaterThan 50
        observedDups |> shouldBeGreaterThan 100
        observedUnlinks |> shouldBeGreaterThan 100

        // ...and a reap really happened, so the `DanglingOpenInode` and
        // `UnreachableFromRoot` clauses had something to be wrong about. An
        // unlink whose inode is still held reaps nothing, and a run of only
        // those would leave both clauses untouched.
        observedReaps |> shouldBeGreaterThan 50

    // --- socketEventRegistrationCouldFire ---

    /// The one target whose events are ruled out: a listening stream socket.
    /// Its readiness needs a backlog entry, and nothing can produce one until
    /// `SystemNative_Connect` lands.
    [<Test>]
    let ``a listening stream socket's registration cannot fire`` () : unit =
        let kernel =
            forge
                [ 3, OpenFileDescriptionId 10L, OpenFileTarget.Socket (SocketId 0L) ]
                [
                    0L,
                    { someSocket with
                        IsListening = true
                    }
                ]
                1L

        EmulatedKernel.socketEventRegistrationCouldFire (OpenFileDescriptionId 10L) kernel
        |> shouldEqual false

    /// A non-listening stream socket is `EPOLLOUT|EPOLLHUP` the moment a real
    /// kernel adds it, so its registration could fire.
    [<Test>]
    let ``a non-listening stream socket's registration could fire`` () : unit =
        let kernel =
            forge [ 3, OpenFileDescriptionId 10L, OpenFileTarget.Socket (SocketId 0L) ] [ 0L, someSocket ] 1L

        EmulatedKernel.socketEventRegistrationCouldFire (OpenFileDescriptionId 10L) kernel
        |> shouldEqual true

    /// A datagram socket is writable immediately, listening being no part of
    /// its life at all.
    [<Test>]
    let ``a datagram socket's registration could fire`` () : unit =
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

        EmulatedKernel.socketEventRegistrationCouldFire (OpenFileDescriptionId 10L) kernel
        |> shouldEqual true

    /// A pipe end's readiness depends on peer state PawPrint does not model,
    /// so it is never ruled out. The ids are `initial`'s standard streams.
    [<Test>]
    let ``a standard stream's registration could fire`` () : unit =
        for id in 0L .. 2L do
            EmulatedKernel.socketEventRegistrationCouldFire (OpenFileDescriptionId id) EmulatedKernel.initial
            |> shouldEqual true

    /// Registrations reference live descriptions (`close` sweeps), so a
    /// dangling id must be reported as the interpreter bug it is rather than
    /// answered either way.
    [<Test>]
    let ``a dangling registration target crashes rather than answering`` () : unit =
        let exc =
            Assert.Throws<System.Exception> (fun () ->
                EmulatedKernel.socketEventRegistrationCouldFire (OpenFileDescriptionId 99L) EmulatedKernel.initial
                |> ignore
            )

        exc.Message |> shouldContainText "names no live open file description"
