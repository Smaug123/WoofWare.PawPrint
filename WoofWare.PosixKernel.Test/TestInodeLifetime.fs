namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// When an inode stops existing.
///
/// A real kernel frees one once its last name *and* its last descriptor have
/// gone, and neither half is a fact about the filesystem alone. The rules live
/// in `UnixDescriptor.pinnedInodes` and `UnixDescriptor.forgetIfUnheld`, which
/// is the one place that can see both tables.
///
/// None of this is guest-observable — freeing memory is not something a process
/// can watch — so these rows and `sourcesImpure/UnlinkReapSeeded.cs`'s terminal
/// assertion are the only checks on it.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestInodeLifetime =

    let private name (s : string) : DirectoryEntryName = DirectoryEntryName.parseOrFail "test" s

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
    let private seed : Map<DirectoryEntryName, SeedEntry> =
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

    /// A system on the tree above, standing at `dir`.
    let private standingAt (dir : string) : UnixSystem<int, string> =
        match
            UnixSystem.initial<int, string> SimulatedUnixPlatform.linuxX64
            |> UnixSystem.withFileSystemAndCurrentDirectory SimulatedUnixPlatform.linuxX64 createdAt seed (absolute dir)
        with
        | Ok system -> system
        | Error fault -> failwith $"the fixture's own seed did not boot at %s{dir}: %O{fault}."

    let private kernel () : UnixSystem<int, string> = standingAt "/outer/inner"

    /// The same tree, but standing at the root — so that `/outer/inner` can be
    /// orphaned with a descriptor as its only holder, which is the shape the
    /// cascade rows need.
    let private kernelAtRoot () : UnixSystem<int, string> = standingAt "/"

    let private inodeOf (kernel : UnixSystem<int, string>) (path : string) : InodeNumber =
        match
            VirtualFileSystem.resolveExisting
                (SimulatedUnixPlatform.pathLimits kernel.Machine.UnixPlatform)
                CallerPrivilege.Privileged
                (VirtualFileSystem.root kernel.Machine.FileSystem)
                SymlinkPolicy.Follow
                (UnixPath.parseOrFail "test" path)
                kernel.Machine.FileSystem
        with
        | Ok inode -> inode
        | Error error -> failwith $"could not resolve %s{path} in the test seed: %O{error}"

    /// Open `inode` read-only, answering the descriptor and the kernel holding it.
    let private opened (inode : InodeNumber) (kernel : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let fd, registry =
            FileDescriptorRegistry.openFile inode FileAccessMode.ReadOnly kernel.Process.FileDescriptors

        fd,
        { kernel with
            Process =
                { kernel.Process with
                    FileDescriptors = registry
                }
        }

    /// Remove `name` from the directory `path` names, answering the inode that
    /// name bound and the kernel with the name gone — the state `unlink` leaves
    /// before anything decides whether to free the inode.
    let private unbound
        (path : string)
        (entry : string)
        (kernel : UnixSystem<int, string>)
        : InodeNumber * UnixSystem<int, string>
        =
        match
            VirtualFileSystem.unbind
                UnbindTargetEffect.LostALink
                (inodeOf kernel path)
                (name entry)
                later
                kernel.Machine.FileSystem
        with
        | Error error -> failwith $"could not unbind %s{entry} from %s{path}: %O{error}"
        | Ok (inode, filesystem) ->
            inode,
            { kernel with
                Machine =
                    { kernel.Machine with
                        FileSystem = filesystem
                    }
            }

    let private closed (fd : int) (kernel : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixDescriptor.close fd kernel with
        | Error refusal -> failwith $"close of fd %d{fd} refused: %s{CloseRefusal.describe refusal}"
        | Ok (SyscallAnswer.Failed error, _) -> failwith $"could not close fd %d{fd}: %O{error}"
        | Ok (SyscallAnswer.Completed _, system) -> system

    let private contains (inode : InodeNumber) (kernel : UnixSystem<int, string>) : bool =
        (VirtualFileSystem.tryGet inode kernel.Machine.FileSystem).IsSome

    // -------------------------------------------------------------- heldInodes

    [<Test>]
    let ``heldInodes names every open file and the current directory`` () : unit =
        let kernel = kernel ()
        let a = inodeOf kernel "/outer/inner/a"
        let b = inodeOf kernel "/outer/inner/b"

        // Before anything is opened, the current directory is the only
        // reference. A `heldInodes` that enumerated only the descriptor table
        // would answer the empty set here — and then reap the directory the
        // process is standing in, the moment `rmdir` can orphan one.
        UnixProcessState.heldInodes kernel.Process
        |> shouldEqual (Set.singleton kernel.Process.CurrentDirectoryInode)

        let _, withA = opened a kernel

        UnixProcessState.heldInodes withA.Process
        |> shouldEqual (Set.ofList [ kernel.Process.CurrentDirectoryInode ; a ])

        // ...and not merely "some file is open": `b` is not held.
        UnixProcessState.heldInodes withA.Process |> Set.contains b |> shouldEqual false

    [<Test>]
    let ``a standard stream or a socket holds no inode`` () : unit =
        // The three descriptors every process inherits are `OpenFileTarget.StandardStream`,
        // which names no inode at all; a `choose` matching them would hold
        // whatever `InodeNumber` it invented for them.
        let kernel = kernel ()

        UnixProcessState.heldInodes kernel.Process |> Set.count |> shouldEqual 1

        FileDescriptorRegistry.descriptions kernel.Process.FileDescriptors
        |> Map.isEmpty
        |> shouldEqual false

    // ------------------------------------------------------------ pinnedInodes

    [<Test>]
    let ``pinnedInodes climbs from every held inode to the root`` () : unit =
        // Measured on both flavours: a held orphan keeps its "..", and its
        // parent's "..", all the way up. So the ancestors of anything held must
        // not be freed, and `checkInvariants` must excuse them.
        let kernel = kernel ()
        let root = VirtualFileSystem.root kernel.Machine.FileSystem
        let outer = inodeOf kernel "/outer"
        let inner = inodeOf kernel "/outer/inner"

        UnixProcessState.heldInodes kernel.Process |> shouldEqual (Set.singleton inner)

        UnixDescriptor.pinnedInodes kernel
        |> shouldEqual (Set.ofList [ inner ; outer ; root ])

    [<Test>]
    let ``pinning an open file climbs from the file's directory, not the file`` () : unit =
        // A file records no parent, so holding one pins the file and whatever
        // the *other* references reach — never the directory that happens to
        // name it. Nothing needs it to: while a name binds the file, the
        // directory holding that name is reachable from the root anyway.
        let kernel = kernel ()
        let a = inodeOf kernel "/outer/inner/a"
        let _, withA = opened a kernel

        let root = VirtualFileSystem.root kernel.Machine.FileSystem
        let outer = inodeOf kernel "/outer"
        let inner = inodeOf kernel "/outer/inner"

        UnixDescriptor.pinnedInodes withA
        |> shouldEqual (Set.ofList [ a ; inner ; outer ; root ])

    // --------------------------------------------------------- forgetIfUnheld

    [<Test>]
    let ``forgetIfUnheld frees an inode nothing names and nothing holds`` () : unit =
        let kernel = kernel ()
        let a, unbound = unbound "/outer/inner" "a" kernel

        contains a unbound |> shouldEqual true

        let reaped = UnixDescriptor.forgetIfUnheld a unbound

        contains a reaped |> shouldEqual false

        VirtualFileSystem.checkInvariants (UnixDescriptor.pinnedInodes reaped) reaped.Machine.FileSystem
        |> shouldEqual []

        UnixSystem.checkInvariants reaped |> shouldEqual []

    [<Test>]
    let ``forgetIfUnheld leaves an inode that still has a name`` () : unit =
        let kernel = kernel ()
        let a = inodeOf kernel "/outer/inner/a"

        UnixDescriptor.forgetIfUnheld a kernel |> contains a |> shouldEqual true

    [<Test>]
    let ``forgetIfUnheld leaves an inode a descriptor holds`` () : unit =
        let kernel = kernel ()
        let a, unbound = unbound "/outer/inner" "a" kernel
        let _, held = opened a unbound

        let attempted = UnixDescriptor.forgetIfUnheld a held

        contains a attempted |> shouldEqual true

        // Legitimately unreachable, and only because of the pin.
        VirtualFileSystem.checkInvariants (UnixDescriptor.pinnedInodes attempted) attempted.Machine.FileSystem
        |> shouldEqual []

        VirtualFileSystem.checkInvariants Set.empty attempted.Machine.FileSystem
        |> shouldEqual [ VirtualFileSystemDefect.UnreachableFromRoot a ]

    [<Test>]
    let ``forgetIfUnheld never frees the root`` () : unit =
        // Nothing holds an entry naming the root, so its binding count is zero
        // by construction and a rule that consulted only the count would free
        // the filesystem out from under every path. `open("/")` and then
        // `close` is an ordinary thing for a guest to do, so this is reachable.
        let kernel = kernel ()
        let root = VirtualFileSystem.root kernel.Machine.FileSystem

        UnixDescriptor.forgetIfUnheld root kernel |> contains root |> shouldEqual true

        let fd, withRoot = opened root kernel
        let afterClose = closed fd withRoot

        contains root afterClose |> shouldEqual true
        UnixSystem.checkInvariants afterClose |> shouldEqual []

    [<Test>]
    let ``forgetIfUnheld leaves an inode that is already gone`` () : unit =
        // Total, so that a caller need not know whether some earlier step
        // already reaped it — which is exactly the position `close` is in.
        let kernel = kernel ()
        let a, unbound = unbound "/outer/inner" "a" kernel
        let once = UnixDescriptor.forgetIfUnheld a unbound

        UnixDescriptor.forgetIfUnheld a once |> shouldEqual once

    // ------------------------------------------------------------------ close

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
        UnixSystem.checkInvariants afterClose |> shouldEqual []

        VirtualFileSystem.checkInvariants (UnixDescriptor.pinnedInodes afterClose) afterClose.Machine.FileSystem
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
            match FileDescriptorRegistry.dup fd held.Process.FileDescriptors with
            | Ok (duplicate, registry) ->
                duplicate,
                { held with
                    Process =
                        { held.Process with
                            FileDescriptors = registry
                        }
                }
            | Error error -> failwith $"could not dup fd %d{fd}: %O{error}"

        let _, unbound = unbound "/outer/inner" "a" held

        let afterFirst = closed fd unbound
        contains a afterFirst |> shouldEqual true
        UnixSystem.checkInvariants afterFirst |> shouldEqual []

        let afterSecond = closed duplicate afterFirst
        contains a afterSecond |> shouldEqual false

    [<Test>]
    let ``closing a descriptor on a still-named inode reaps nothing`` () : unit =
        let kernel = kernel ()
        let a = inodeOf kernel "/outer/inner/a"
        let fd, held = opened a kernel

        closed fd held |> contains a |> shouldEqual true

    // ---------------------------------------------------- orphaned directories

    /// Empty `/outer/inner` out, then unbind it from `/outer`, answering the
    /// three inodes and the kernel holding the orphan through `fd`.
    let private orphanedInner () : int * InodeNumber * InodeNumber * UnixSystem<int, string> =
        let kernel = kernelAtRoot ()
        let inner = inodeOf kernel "/outer/inner"
        let outer = inodeOf kernel "/outer"
        let fd, kernel = opened inner kernel

        let kernel =
            [ "a" ; "b" ]
            |> List.fold
                (fun kernel entry ->
                    let inode, kernel = unbound "/outer/inner" entry kernel
                    UnixDescriptor.forgetIfUnheld inode kernel
                )
                kernel

        let _, kernel = unbound "/outer" "inner" kernel
        fd, inner, outer, UnixDescriptor.forgetIfUnheld inner kernel

    [<Test>]
    let ``an orphan held by a descriptor keeps its ancestors alive`` () : unit =
        let fd, inner, outer, kernel = orphanedInner ()

        // The descriptor holds `inner` directly...
        contains inner kernel |> shouldEqual true

        UnixProcessState.heldInodes kernel.Process
        |> Set.contains inner
        |> shouldEqual true

        // ...and `inner`'s recorded parent is what "`.." from the orphan
        // resolves to, so it must survive being unbound as well. Measured on
        // both flavours: after `rmdir(b)` and `rmdir(a)`, `stat("..")` from
        // inside the orphan still answers `a`'s inode.
        let _, kernel = unbound "/" "outer" kernel
        let kernel = UnixDescriptor.forgetIfUnheld outer kernel

        contains outer kernel |> shouldEqual true

        UnixProcessState.heldInodes kernel.Process
        |> Set.contains outer
        |> shouldEqual false

        UnixDescriptor.pinnedInodes kernel |> Set.contains outer |> shouldEqual true

        VirtualFileSystem.checkInvariants (UnixDescriptor.pinnedInodes kernel) kernel.Machine.FileSystem
        |> shouldEqual []

        UnixSystem.checkInvariants kernel |> shouldEqual []

        // Both are legitimately unreachable, and only because of the pin.
        VirtualFileSystem.checkInvariants Set.empty kernel.Machine.FileSystem
        |> List.sort
        |> shouldEqual (
            [
                VirtualFileSystemDefect.UnreachableFromRoot inner
                VirtualFileSystemDefect.UnreachableFromRoot outer
            ]
            |> List.sort
        )

        // Closing the one descriptor drops the last reference to the whole
        // chain, so both go at once.
        let afterClose = closed fd kernel

        contains inner afterClose |> shouldEqual false
        contains outer afterClose |> shouldEqual false

        VirtualFileSystem.checkInvariants (UnixDescriptor.pinnedInodes afterClose) afterClose.Machine.FileSystem
        |> shouldEqual []

        UnixSystem.checkInvariants afterClose |> shouldEqual []

    [<Test>]
    let ``the cascade stops at a parent something still names`` () : unit =
        // The ordinary case, and the one the guard is for: `/outer` is still
        // bound in the root, so freeing `/outer/inner` must not touch it.
        let kernel = kernelAtRoot ()
        let outer = inodeOf kernel "/outer"

        let kernel =
            [ "a" ; "b" ]
            |> List.fold
                (fun kernel entry ->
                    let inode, kernel = unbound "/outer/inner" entry kernel
                    UnixDescriptor.forgetIfUnheld inode kernel
                )
                kernel

        let inner, kernel = unbound "/outer" "inner" kernel
        let reaped = UnixDescriptor.forgetIfUnheld inner kernel

        contains inner reaped |> shouldEqual false
        contains outer reaped |> shouldEqual true

        VirtualFileSystem.checkInvariants (UnixDescriptor.pinnedInodes reaped) reaped.Machine.FileSystem
        |> shouldEqual []

        UnixSystem.checkInvariants reaped |> shouldEqual []

    [<Test>]
    let ``the cascade stops at the root`` () : unit =
        // Every chain ends at the root, whose binding count is zero by
        // construction — so a cascade that only checked "is anything naming
        // this" would free the filesystem out from under every path.
        let fd, _, outer, kernel = orphanedInner ()
        let root = VirtualFileSystem.root kernel.Machine.FileSystem

        let _, kernel = unbound "/" "outer" kernel

        let afterClose = closed fd (UnixDescriptor.forgetIfUnheld outer kernel)

        contains root afterClose |> shouldEqual true
        VirtualFileSystem.root afterClose.Machine.FileSystem |> shouldEqual root
        UnixSystem.checkInvariants afterClose |> shouldEqual []

    // ------------------------------------------------------------- invariants

    // ------------------------------------------------- open directory streams

    /// A stream over `/outer/inner`, and the inode it is over.
    let private streaming
        (kernel : UnixSystem<int, string>)
        : DirectoryStreamId * InodeNumber * UnixSystem<int, string>
        =
        let inner = inodeOf kernel "/outer/inner"

        match UnixNamespace.opendir (UnixPath.parseOrFail "test" "/outer/inner") kernel with
        | OpenDirAnswer.Opened id, system -> id, inner, system
        | other -> failwith $"could not open the directory: %O{other}"

    /// `closedir`'s bookkeeping half. Written out because the library has no
    /// `closedir` to call: `opendir` mints a stream and nothing here removes
    /// one, so every client is currently doing this for itself.
    let private forgetStream (id : DirectoryStreamId) (kernel : UnixSystem<int, string>) : UnixSystem<int, string> =
        { kernel with
            Process =
                { kernel.Process with
                    DirectoryStreams = Map.remove id kernel.Process.DirectoryStreams
                }
        }

    [<Test>]
    let ``an open stream holds its directory even with its descriptor gone`` () : unit =
        // The descriptor already holds it, so this adds nothing while the stream
        // is intact. It is here for the guest that closes the stream's own
        // descriptor out from under it — undefined behaviour on a real libc, but
        // a guessable fd number away here. Without it the next `readdir` would
        // reach a reaped inode and take the interpreter down.
        let kernel = kernelAtRoot ()
        let id, inner, kernel = streaming kernel

        let kernel = closed kernel.Process.DirectoryStreams.[id].Fd kernel

        UnixProcessState.heldInodes kernel.Process
        |> Set.contains inner
        |> shouldEqual true

        // ...and it really was the stream that held it: forget the stream and
        // the inode is unheld.
        UnixProcessState.heldInodes (forgetStream id kernel).Process
        |> Set.contains inner
        |> shouldEqual false

    [<Test>]
    let ``a stream keeps an rmdir'd directory alive, and closing it reaps`` () : unit =
        let kernel = kernelAtRoot ()
        let id, inner, kernel = streaming kernel

        // `rmdir /outer/inner`, which succeeds against a real kernel because the
        // stream is not a name.
        let removed, kernel = unbound "/outer" "inner" kernel
        removed |> shouldEqual inner

        let kernel = UnixDescriptor.forgetIfUnheld removed kernel
        contains inner kernel |> shouldEqual true

        VirtualFileSystem.isOrphanedDirectory inner kernel.Machine.FileSystem
        |> shouldEqual true

        // And the orphan answers end-of-stream at once, dots included: probed on
        // both kernels, `opendir` then `rmdir` then `readdir` gives NULL.
        VirtualFileSystem.nextDirectoryEntry inner DirectoryCursor.Start kernel.Machine.FileSystem
        |> shouldEqual None

        // `closedir`: forget the stream, then close the descriptor under it.
        // That order is what makes the reap happen — `heldInodes` counts the
        // stream among the things holding the inode.
        let fd = kernel.Process.DirectoryStreams.[id].Fd
        let kernel = forgetStream id kernel |> closed fd

        contains inner kernel |> shouldEqual false

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
                Machine =
                    { held.Machine with
                        FileSystem = VirtualFileSystem.forget a held.Machine.FileSystem
                    }
            }

        let description =
            FileDescriptorRegistry.descriptions broken.Process.FileDescriptors
            |> Map.toList
            |> List.choose (fun (id, description) ->
                match description.Target with
                | OpenFileTarget.File (inode, _) when inode = a -> Some id
                | _ -> None
            )
            |> List.exactlyOne

        UnixSystem.checkInvariants broken
        |> shouldEqual [ UnixSystemDefect.DanglingOpenInode (description, a) ]
