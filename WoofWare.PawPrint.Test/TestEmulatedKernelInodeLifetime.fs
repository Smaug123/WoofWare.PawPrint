namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// When an inode stops existing.
///
/// A real kernel frees one once its last name *and* its last descriptor have
/// gone, and neither half is a fact about the filesystem alone. The rules live
/// in `EmulatedKernel.pinnedInodes` and `EmulatedKernel.forgetIfUnheld`, which
/// is the one place that can see both tables.
///
/// None of this is guest-observable — freeing memory is not something a process
/// can watch — so these rows and `sourcesImpure/UnlinkReapSeeded.cs`'s terminal
/// assertion are the only checks on it.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEmulatedKernelInodeLifetime =

    let private name (s : string) : FileName = FileName.parseOrFail "test" s

    let private absolute (s : string) : AbsoluteUnixPath = AbsoluteUnixPath.parseOrFail "test" s

    let private createdAt : UnixTimestamp =
        UnixTimestamp.createOrFail "test" 1_700_000_000L 0

    let private later : UnixTimestamp =
        UnixTimestamp.createOrFail "test" 1_700_005_000L 0

    let private bytes (s : string) : ImmutableArray<byte> =
        System.Text.Encoding.UTF8.GetBytes s |> ImmutableArray.CreateRange

    /// `/outer/inner/` holding `a` and `b`, with the current directory at
    /// `/outer/inner` — so that the cwd is a directory the seed really contains
    /// rather than the root, which every other rule would keep alive anyway.
    let private seed : Map<FileName, SeedEntry> =
        Map.ofList
            [
                name "outer",
                SeedEntry.directory (
                    Map.ofList
                        [
                            name "inner",
                            SeedEntry.directory (
                                Map.ofList
                                    [
                                        name "a", SeedEntry.file (bytes "aaa")
                                        name "b", SeedEntry.file (bytes "bbb")
                                    ]
                            )
                        ]
                )
            ]

    let private kernel () : EmulatedKernel =
        EmulatedKernel.initial
        |> EmulatedKernel.withFileSystemAndCurrentDirectory
            SimulatedUnixPlatform.linuxX64
            createdAt
            seed
            (absolute "/outer/inner")

    let private inodeOf (kernel : EmulatedKernel) (path : string) : InodeNumber =
        match
            VirtualFileSystem.resolveExisting
                (SimulatedUnixPlatform.pathLimits kernel.UnixPlatform)
                CallerPrivilege.Privileged
                (VirtualFileSystem.root kernel.FileSystem)
                SymlinkPolicy.Follow
                (UnixPath.parseOrFail "test" path)
                kernel.FileSystem
        with
        | Ok inode -> inode
        | Error error -> failwith $"could not resolve %s{path} in the test seed: %O{error}"

    /// Open `inode` read-only, answering the descriptor and the kernel holding it.
    let private opened (inode : InodeNumber) (kernel : EmulatedKernel) : int * EmulatedKernel =
        let fd, registry =
            FileDescriptorRegistry.openFile inode FileAccessMode.ReadOnly kernel.FileDescriptors

        fd,
        { kernel with
            FileDescriptors = registry
        }

    /// Remove `name` from the directory `path` names, answering the inode that
    /// name bound and the kernel with the name gone — the state `unlink` leaves
    /// before anything decides whether to free the inode.
    let private unbound (path : string) (entry : string) (kernel : EmulatedKernel) : InodeNumber * EmulatedKernel =
        match VirtualFileSystem.unbind (inodeOf kernel path) (name entry) later kernel.FileSystem with
        | Error error -> failwith $"could not unbind %s{entry} from %s{path}: %O{error}"
        | Ok (inode, filesystem) ->
            inode,
            { kernel with
                FileSystem = filesystem
            }

    let private closed (fd : int) (kernel : EmulatedKernel) : EmulatedKernel =
        match EmulatedKernel.closeFd fd kernel with
        | Ok kernel -> kernel
        | Error error -> failwith $"could not close fd %d{fd}: %O{error}"

    let private contains (inode : InodeNumber) (kernel : EmulatedKernel) : bool =
        (VirtualFileSystem.tryGet inode kernel.FileSystem).IsSome

    // ------------------------------------------------------------ pinnedInodes

    [<Test>]
    let ``pinnedInodes names every open file and the current directory`` () : unit =
        let kernel = kernel ()
        let a = inodeOf kernel "/outer/inner/a"
        let b = inodeOf kernel "/outer/inner/b"

        // Before anything is opened, the current directory is the only
        // reference. A `pinnedInodes` that enumerated only the descriptor table
        // would answer the empty set here — and then reap the directory the
        // process is standing in, the moment `rmdir` can orphan one.
        EmulatedKernel.pinnedInodes kernel
        |> shouldEqual (Set.singleton kernel.CurrentDirectoryInode)

        let _, withA = opened a kernel

        EmulatedKernel.pinnedInodes withA
        |> shouldEqual (Set.ofList [ kernel.CurrentDirectoryInode ; a ])

        // ...and not merely "some file is open": `b` is not held.
        EmulatedKernel.pinnedInodes withA |> Set.contains b |> shouldEqual false

    [<Test>]
    let ``a standard stream or a socket pins no inode`` () : unit =
        // The three descriptors every process inherits are `OpenFileTarget.StandardStream`,
        // which names no inode at all; a `choose` matching them would pin
        // whatever `InodeNumber` it invented for them.
        let kernel = kernel ()

        EmulatedKernel.pinnedInodes kernel |> Set.count |> shouldEqual 1

        FileDescriptorRegistry.descriptions kernel.FileDescriptors
        |> Map.isEmpty
        |> shouldEqual false

    // --------------------------------------------------------- forgetIfUnheld

    [<Test>]
    let ``forgetIfUnheld frees an inode nothing names and nothing holds`` () : unit =
        let kernel = kernel ()
        let a, unbound = unbound "/outer/inner" "a" kernel

        contains a unbound |> shouldEqual true

        let reaped = EmulatedKernel.forgetIfUnheld a unbound

        contains a reaped |> shouldEqual false

        VirtualFileSystem.checkInvariants (EmulatedKernel.pinnedInodes reaped) reaped.FileSystem
        |> shouldEqual []

        EmulatedKernel.checkInvariants reaped |> shouldEqual []

    [<Test>]
    let ``forgetIfUnheld leaves an inode that still has a name`` () : unit =
        let kernel = kernel ()
        let a = inodeOf kernel "/outer/inner/a"

        EmulatedKernel.forgetIfUnheld a kernel |> contains a |> shouldEqual true

    [<Test>]
    let ``forgetIfUnheld leaves an inode a descriptor holds`` () : unit =
        let kernel = kernel ()
        let a, unbound = unbound "/outer/inner" "a" kernel
        let _, held = opened a unbound

        let attempted = EmulatedKernel.forgetIfUnheld a held

        contains a attempted |> shouldEqual true

        // Legitimately unreachable, and only because of the pin.
        VirtualFileSystem.checkInvariants (EmulatedKernel.pinnedInodes attempted) attempted.FileSystem
        |> shouldEqual []

        VirtualFileSystem.checkInvariants Set.empty attempted.FileSystem
        |> shouldEqual [ VirtualFileSystemDefect.UnreachableFromRoot a ]

    [<Test>]
    let ``forgetIfUnheld never frees the root`` () : unit =
        // Nothing holds an entry naming the root, so its binding count is zero
        // by construction and a rule that consulted only the count would free
        // the filesystem out from under every path. `open("/")` and then
        // `close` is an ordinary thing for a guest to do, so this is reachable.
        let kernel = kernel ()
        let root = VirtualFileSystem.root kernel.FileSystem

        EmulatedKernel.forgetIfUnheld root kernel |> contains root |> shouldEqual true

        let fd, withRoot = opened root kernel
        let afterClose = closed fd withRoot

        contains root afterClose |> shouldEqual true
        EmulatedKernel.checkInvariants afterClose |> shouldEqual []

    [<Test>]
    let ``forgetIfUnheld leaves an inode that is already gone`` () : unit =
        // Total, so that a caller need not know whether some earlier step
        // already reaped it — which is exactly the position `closeFd` is in.
        let kernel = kernel ()
        let a, unbound = unbound "/outer/inner" "a" kernel
        let once = EmulatedKernel.forgetIfUnheld a unbound

        EmulatedKernel.forgetIfUnheld a once |> shouldEqual once

    // ---------------------------------------------------------------- closeFd

    [<Test>]
    let ``closing the last descriptor reaps an unbound inode`` () : unit =
        let kernel = kernel ()
        let a = inodeOf kernel "/outer/inner/a"
        let fd, held = opened a kernel
        let _, unbound = unbound "/outer/inner" "a" held

        // The name has gone and the descriptor has not, so the inode stays.
        contains a unbound |> shouldEqual true

        let afterClose = closed fd unbound

        contains a afterClose |> shouldEqual false
        EmulatedKernel.checkInvariants afterClose |> shouldEqual []

        VirtualFileSystem.checkInvariants (EmulatedKernel.pinnedInodes afterClose) afterClose.FileSystem
        |> shouldEqual []

    [<Test>]
    let ``closing one of two descriptors onto one description reaps nothing`` () : unit =
        // `dup(2)` makes a second descriptor onto the *same* description, and
        // `FileDescriptorRegistry.close` destroys the description only when the
        // last of them goes. A reaping rule keyed on the descriptor rather than
        // on the description would free the inode here, while a live fd still
        // named it.
        let kernel = kernel ()
        let a = inodeOf kernel "/outer/inner/a"
        let fd, held = opened a kernel

        let duplicate, held =
            match FileDescriptorRegistry.dup fd held.FileDescriptors with
            | Ok (duplicate, registry) ->
                duplicate,
                { held with
                    FileDescriptors = registry
                }
            | Error error -> failwith $"could not dup fd %d{fd}: %O{error}"

        let _, unbound = unbound "/outer/inner" "a" held

        let afterFirst = closed fd unbound
        contains a afterFirst |> shouldEqual true
        EmulatedKernel.checkInvariants afterFirst |> shouldEqual []

        let afterSecond = closed duplicate afterFirst
        contains a afterSecond |> shouldEqual false

    [<Test>]
    let ``closing a descriptor on a still-named inode reaps nothing`` () : unit =
        let kernel = kernel ()
        let a = inodeOf kernel "/outer/inner/a"
        let fd, held = opened a kernel

        closed fd held |> contains a |> shouldEqual true

    // ------------------------------------------------------------- invariants

    [<Test>]
    let ``checkInvariants rejects a descriptor naming an inode the filesystem has forgotten`` () : unit =
        // The mirror image of `UnreachableFromRoot`: between them they bracket
        // the reaping rule, so a `forget` that fires too late is caught there
        // and one that fires too early is caught here.
        let kernel = kernel ()
        let a, unbound = unbound "/outer/inner" "a" kernel
        let _, held = opened a unbound

        // Forged: `forget` is happy to do this, since nothing *names* the
        // inode; only the kernel can see that something holds it.
        let broken =
            { held with
                FileSystem = VirtualFileSystem.forget a held.FileSystem
            }

        let description =
            FileDescriptorRegistry.descriptions broken.FileDescriptors
            |> Map.toList
            |> List.choose (fun (id, description) ->
                match description.Target with
                | OpenFileTarget.File (inode, _) when inode = a -> Some id
                | _ -> None
            )
            |> List.exactlyOne

        EmulatedKernel.checkInvariants broken
        |> shouldEqual [ EmulatedKernelDefect.DanglingOpenInode (description, a) ]
