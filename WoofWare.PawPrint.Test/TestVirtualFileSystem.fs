namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestVirtualFileSystem =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 300

    let private name (s : string) : FileName = FileName.parseOrFail "test" s

    let private path (s : string) : UnixPath = UnixPath.parseOrFail "test" s

    let private target (s : string) : SymlinkTarget = SymlinkTarget.parseOrFail "test" s

    let private ok (result : Result<'a, UnixError>) : 'a =
        match result with
        | Ok value -> value
        | Error error -> failwith $"expected success, got %O{error}"

    let private noBytes : ImmutableArray<byte> = ImmutableArray<byte>.Empty

    /// Build a filesystem from a script of operations, failing loudly if any
    /// step is rejected. Keeps the tests below readable.
    let private build (steps : (VirtualFileSystem -> VirtualFileSystem) list) : VirtualFileSystem =
        steps |> List.fold (fun vfs step -> step vfs) VirtualFileSystem.empty

    let private mkdir (parent : InodeNumber) (n : string) (vfs : VirtualFileSystem) : VirtualFileSystem =
        VirtualFileSystem.createDirectory parent (name n) vfs |> ok |> snd

    let private mkfile (parent : InodeNumber) (n : string) (vfs : VirtualFileSystem) : VirtualFileSystem =
        VirtualFileSystem.createFile parent (name n) noBytes vfs |> ok |> snd

    let private mklink (parent : InodeNumber) (n : string) (t : string) (vfs : VirtualFileSystem) : VirtualFileSystem =
        VirtualFileSystem.createSymlink parent (name n) (target t) vfs |> ok |> snd

    let private rootOf (vfs : VirtualFileSystem) : InodeNumber = VirtualFileSystem.root vfs

    // ------------------------------------------------------------- the basics

    [<Test>]
    let ``empty is a sound filesystem containing only the root`` () : unit =
        VirtualFileSystem.checkInvariants VirtualFileSystem.empty |> shouldEqual []

        VirtualFileSystem.inodes VirtualFileSystem.empty |> Map.count |> shouldEqual 1

        // The root's parent is itself, so "/.." is "/".
        VirtualFileSystem.resolve
            (rootOf VirtualFileSystem.empty)
            SymlinkPolicy.Follow
            (path "/..")
            VirtualFileSystem.empty
        |> shouldEqual (Ok (ResolvedTarget.Directory (rootOf VirtualFileSystem.empty, FinalNavigation.Parent)))

    [<Test>]
    let ``the empty path is ENOENT, not the directory we started from`` () : unit =
        // The trap this guards: a walk over zero components would silently mean
        // "the start directory", which is a successful answer to a call every
        // Unix rejects.
        VirtualFileSystem.resolve
            (rootOf VirtualFileSystem.empty)
            SymlinkPolicy.Follow
            UnixPath.empty
            VirtualFileSystem.empty
        |> shouldEqual (Error UnixError.ENOENT)

    [<Test>]
    let ``a relative path starting from a non-directory is ENOTDIR`` () : unit =
        let vfs = build [ mkfile (rootOf VirtualFileSystem.empty) "f" ]

        let file =
            VirtualFileSystem.resolveExisting (rootOf vfs) SymlinkPolicy.Follow (path "/f") vfs
            |> ok

        VirtualFileSystem.resolve file SymlinkPolicy.Follow (path "a") vfs
        |> shouldEqual (Error UnixError.ENOTDIR)

    [<Test>]
    let ``a path cannot continue through a regular file`` () : unit =
        let vfs = build [ mkfile (rootOf VirtualFileSystem.empty) "f" ]

        VirtualFileSystem.resolve (rootOf vfs) SymlinkPolicy.Follow (path "/f/x") vfs
        |> shouldEqual (Error UnixError.ENOTDIR)

    [<Test>]
    let ``a free name in the final position is not an error`` () : unit =
        // The whole point of stopping short of the final lookup: mkdir and
        // open(O_CREAT) need this state, and only stat turns it into ENOENT.
        let vfs = VirtualFileSystem.empty

        VirtualFileSystem.resolve (rootOf vfs) SymlinkPolicy.Follow (path "/nx") vfs
        |> shouldEqual (Ok (ResolvedTarget.Entry (rootOf vfs, name "nx", None)))

        VirtualFileSystem.resolveExisting (rootOf vfs) SymlinkPolicy.Follow (path "/nx") vfs
        |> shouldEqual (Error UnixError.ENOENT)

        // ...but a free name part-way along is ENOENT even so.
        VirtualFileSystem.resolve (rootOf vfs) SymlinkPolicy.Follow (path "/nx/y") vfs
        |> shouldEqual (Error UnixError.ENOENT)

    // --------------------------------------------------- the trailing separator

    [<Test>]
    let ``a trailing separator is not desugared into a dot component`` () : unit =
        // Probed on macOS: mkdir("d/") succeeds while mkdir("nx/.") is ENOENT,
        // and rmdir("d/") succeeds while rmdir("d/.") is EINVAL. Desugaring
        // would collapse the Entry that mkdir("nx/") needs into a Directory,
        // and would make a free name report ENOENT.
        let vfs = VirtualFileSystem.empty

        let resolution =
            VirtualFileSystem.resolveFull (rootOf vfs) SymlinkPolicy.Follow (path "/nx/") vfs
            |> ok

        resolution.Target
        |> shouldEqual (ResolvedTarget.Entry (rootOf vfs, name "nx", None))

        resolution.TrailingSeparatorDemanded |> shouldEqual true

        // Whereas the genuinely-dotted path has no final name at all, which is
        // what makes rmdir able to tell the two apart and report EINVAL.
        let withDot =
            VirtualFileSystem.resolveFull (rootOf vfs) SymlinkPolicy.Follow (path "/nx/.") vfs

        withDot |> shouldEqual (Error UnixError.ENOENT)

    [<Test>]
    let ``a trailing separator on an existing non-directory is ENOTDIR`` () : unit =
        // The part of the trailing-separator rule every platform agrees on.
        let vfs = build [ mkfile (rootOf VirtualFileSystem.empty) "f" ]

        VirtualFileSystem.resolve (rootOf vfs) SymlinkPolicy.Follow (path "/f/") vfs
        |> shouldEqual (Error UnixError.ENOTDIR)

        // Without the separator the same path is perfectly fine.
        VirtualFileSystem.resolve (rootOf vfs) SymlinkPolicy.Follow (path "/f") vfs
        |> shouldEqual (
            Ok (
                ResolvedTarget.Entry (
                    rootOf vfs,
                    name "f",
                    Some (
                        VirtualFileSystem.resolveExisting (rootOf vfs) SymlinkPolicy.Follow (path "/f") vfs
                        |> ok
                    )
                )
            )
        )

    [<Test>]
    let ``a trailing separator follows a final symlink even under NoFollowFinal`` () : unit =
        // POSIX resolves "p/" as "p/.", and both platforms agree for lookups:
        // probed, lstat("ld/") stats the directory the link names.
        let vfs =
            build
                [
                    mkdir (rootOf VirtualFileSystem.empty) "d"
                    mklink (rootOf VirtualFileSystem.empty) "ld" "d"
                ]

        let directory =
            VirtualFileSystem.resolveExisting (rootOf vfs) SymlinkPolicy.Follow (path "/d") vfs
            |> ok

        let withSlash =
            VirtualFileSystem.resolveFull (rootOf vfs) SymlinkPolicy.NoFollowFinal (path "/ld/") vfs
            |> ok

        withSlash.Target
        |> shouldEqual (ResolvedTarget.Entry (rootOf vfs, name "d", Some directory))

        // ...and the fact is reported, because this is precisely the
        // combination on which the platforms diverge destructively for
        // mutating callers.
        withSlash.FinalSymlinkFollowed |> shouldEqual true
        withSlash.TrailingSeparatorDemanded |> shouldEqual true

        // Without the separator, NoFollowFinal stops at the link itself.
        let link =
            VirtualFileSystem.resolveExisting (rootOf vfs) SymlinkPolicy.NoFollowFinal (path "/ld") vfs
            |> ok

        match VirtualFileSystem.tryGet link vfs with
        | Some (InodeContent.Symlink _) -> ()
        | other -> failwith $"expected the symlink itself, got %A{other}"

    [<Test>]
    let ``a symlink target's own trailing separator takes effect only when final`` () : unit =
        let vfs =
            build
                [
                    mkfile (rootOf VirtualFileSystem.empty) "f"
                    mklink (rootOf VirtualFileSystem.empty) "lf" "f/"
                ]

        // "lf" expands to "f/", whose trailing separator now demands that f be
        // a directory. It is not.
        VirtualFileSystem.resolve (rootOf vfs) SymlinkPolicy.Follow (path "/lf") vfs
        |> shouldEqual (Error UnixError.ENOTDIR)

    // ------------------------------------------------------------- symlinks

    [<Test>]
    let ``a dangling final symlink under Follow is a free name, not an error`` () : unit =
        // open("/link", O_CREAT) where link -> /nx must create nx, so the walk
        // has to hand back the *target's* parent and name.
        let vfs = build [ mklink (rootOf VirtualFileSystem.empty) "dang" "nx" ]

        VirtualFileSystem.resolve (rootOf vfs) SymlinkPolicy.Follow (path "/dang") vfs
        |> shouldEqual (Ok (ResolvedTarget.Entry (rootOf vfs, name "nx", None)))

        // But a dangling link whose target's *parent* is missing is ENOENT,
        // because that failure happens part-way along.
        let vfs = build [ mklink (rootOf VirtualFileSystem.empty) "deep" "nx1/nx2" ]

        VirtualFileSystem.resolve (rootOf vfs) SymlinkPolicy.Follow (path "/deep") vfs
        |> shouldEqual (Error UnixError.ENOENT)

    [<Test>]
    let ``a rooted symlink target restarts at the root`` () : unit =
        let vfs =
            build
                [
                    mkdir (rootOf VirtualFileSystem.empty) "a"
                    fun vfs ->
                        let a =
                            VirtualFileSystem.resolveExisting (rootOf vfs) SymlinkPolicy.Follow (path "/a") vfs
                            |> ok

                        vfs |> mkfile a "f" |> mklink a "up" "/f2"
                    mkfile (rootOf VirtualFileSystem.empty) "f2"
                ]

        let f2 =
            VirtualFileSystem.resolveExisting (rootOf vfs) SymlinkPolicy.Follow (path "/f2") vfs
            |> ok

        VirtualFileSystem.resolveExisting (rootOf vfs) SymlinkPolicy.Follow (path "/a/up") vfs
        |> shouldEqual (Ok f2)

    /// A chain of `length` symlinks ending at a regular file, so that resolving
    /// the head performs exactly `length` traversals.
    let private symlinkChain (length : int) : VirtualFileSystem =
        let steps =
            [
                for i in 1..length do
                    let next = if i = length then "target" else $"s%d{i + 1}"
                    yield fun vfs -> mklink (rootOf vfs) $"s%d{i}" next vfs
                yield fun vfs -> mkfile (rootOf vfs) "target" vfs
            ]

        build steps

    [<Test>]
    let ``a symlink chain every platform allows resolves`` () : unit =
        let vfs = symlinkChain VirtualFileSystem.symlinksEveryPlatformAllows

        VirtualFileSystem.resolveExisting (rootOf vfs) SymlinkPolicy.Follow (path "/s1") vfs
        |> Result.isOk
        |> shouldEqual true

    [<Test>]
    let ``a symlink chain no platform allows is ELOOP`` () : unit =
        let vfs = symlinkChain VirtualFileSystem.symlinksNoPlatformAllows

        VirtualFileSystem.resolveExisting (rootOf vfs) SymlinkPolicy.Follow (path "/s1") vfs
        |> shouldEqual (Error UnixError.ELOOP)

    [<Test>]
    let ``a symlink chain only some platforms allow crashes rather than choosing`` () : unit =
        // Linux permits 40 traversals and macOS 32, so anything in between has
        // two different right answers and PawPrint refuses to pick.
        for length in
            [
                VirtualFileSystem.symlinksEveryPlatformAllows + 1
                VirtualFileSystem.symlinksNoPlatformAllows - 1
            ] do
            let vfs = symlinkChain length

            let exn =
                Assert.Throws<Exception> (fun () ->
                    VirtualFileSystem.resolveExisting (rootOf vfs) SymlinkPolicy.Follow (path "/s1") vfs
                    |> ignore<Result<InodeNumber, UnixError>>
                )

            exn.Message |> shouldContainText "MAXSYMLINKS"

    [<Test>]
    let ``the divergence crash fires for a failing walk too, not only a succeeding one`` () : unit =
        // The hole worth guarding: a 35-link chain ending at a missing name
        // gives ENOENT on Linux and ELOOP on macOS. Once the 33rd traversal has
        // happened, ELOOP is the only outcome any platform could still agree
        // on, so ENOENT would be a silent divergence.
        let length = VirtualFileSystem.symlinksEveryPlatformAllows + 3

        let steps =
            [
                for i in 1..length do
                    let next = if i = length then "nowhere" else $"s%d{i + 1}"
                    yield fun vfs -> mklink (rootOf vfs) $"s%d{i}" next vfs
            ]

        let vfs = build steps

        let exn =
            Assert.Throws<Exception> (fun () ->
                VirtualFileSystem.resolveExisting (rootOf vfs) SymlinkPolicy.Follow (path "/s1") vfs
                |> ignore<Result<InodeNumber, UnixError>>
            )

        exn.Message |> shouldContainText "MAXSYMLINKS"

    [<Test>]
    let ``a self-extending symlink terminates rather than growing forever`` () : unit =
        // The case that defeats cycle detection: "l" -> "l/x" never repeats a
        // (directory, remaining) state, it just grows the path. Only the
        // traversal count stops it, which is why there is no seen-state set.
        let vfs = build [ mklink (rootOf VirtualFileSystem.empty) "l" "l/x" ]

        VirtualFileSystem.resolveExisting (rootOf vfs) SymlinkPolicy.Follow (path "/l") vfs
        |> shouldEqual (Error UnixError.ELOOP)

    [<Test>]
    let ``a symlink cycle is ELOOP rather than a crash`` () : unit =
        // Both platforms report ELOOP for a cycle, so this must be answered
        // rather than referred back as a divergence — the count reaches the
        // no-platform-allows bound before it reaches the divergent band's top.
        let vfs =
            build
                [
                    mklink (rootOf VirtualFileSystem.empty) "a" "b"
                    mklink (rootOf VirtualFileSystem.empty) "b" "a"
                ]

        VirtualFileSystem.resolveExisting (rootOf vfs) SymlinkPolicy.Follow (path "/a") vfs
        |> shouldEqual (Error UnixError.ELOOP)

    [<Test>]
    let ``a symlink target is stored verbatim`` () : unit =
        // readlink(2) returns the stored bytes unchanged and lstat reports
        // their length as st_size, but UnixPath.parse collapses "//". Storing
        // a parsed path would make FileInfo.LinkTarget disagree with every Unix.
        let raw = "a//b/"
        let vfs = build [ mklink (rootOf VirtualFileSystem.empty) "l" raw ]

        let link =
            VirtualFileSystem.resolveExisting (rootOf vfs) SymlinkPolicy.NoFollowFinal (path "/l") vfs
            |> ok

        match VirtualFileSystem.tryGet link vfs with
        | Some (InodeContent.Symlink stored) ->
            SymlinkTarget.toString stored |> shouldEqual raw
            SymlinkTarget.toUtf8 stored |> Seq.length |> shouldEqual raw.Length
            // ...while the *traversal* view is the normalised path.
            SymlinkTarget.toUnixPath stored |> UnixPath.toString |> shouldEqual "a/b/"
        | other -> failwith $"expected a symlink, got %A{other}"

    [<Test>]
    let ``an empty symlink target is unrepresentable`` () : unit =
        // Linux rejects symlink("") with ENOENT; macOS accepts it. Refusing to
        // model the value at all keeps the divergence at the syscall boundary
        // and out of the seed manifest.
        SymlinkTarget.parse "" |> shouldEqual (Error SymlinkTargetError.Empty)
        SymlinkTarget.parse null |> shouldEqual (Error SymlinkTargetError.Empty)

    [<Test>]
    let ``a symlink expansion's own final navigation is reported`` () : unit =
        // Probed on macOS: with l1 -> "." and l2 -> "d/..", rmdir("l1/") gives
        // EINVAL while rmdir("l2/") gives ENOTEMPTY. The two paths are the same
        // shape, so a caller reading the final component off its own UnixPath
        // could not tell them apart — the walk has to say which navigation it
        // actually ended on.
        let vfs =
            build
                [
                    mkdir (rootOf VirtualFileSystem.empty) "d"
                    mklink (rootOf VirtualFileSystem.empty) "l1" "."
                    mklink (rootOf VirtualFileSystem.empty) "l2" "d/.."
                    mklink (rootOf VirtualFileSystem.empty) "l3" "/"
                ]

        let reachedBy (candidate : string) =
            match VirtualFileSystem.resolve (rootOf vfs) SymlinkPolicy.NoFollowFinal (path candidate) vfs with
            | Ok (ResolvedTarget.Directory (_, reachedBy)) -> reachedBy
            | other -> failwith $"expected a navigation-final directory, got %A{other}"

        reachedBy "/l1/" |> shouldEqual FinalNavigation.Current
        reachedBy "/l2/" |> shouldEqual FinalNavigation.Parent
        // A target of "/" has no components at all, so the effective path is
        // the root rather than whatever navigation preceded the link. Reached
        // via ".." so that the reset is observable: with the link at the start
        // of the path, the navigation would already be Root and a missing reset
        // would look correct.
        reachedBy "/d/../l3/" |> shouldEqual FinalNavigation.Root
        reachedBy "/l3/" |> shouldEqual FinalNavigation.Root

        // ...and the unexpanded forms agree with the expanded ones.
        reachedBy "/." |> shouldEqual FinalNavigation.Current
        reachedBy "/d/.." |> shouldEqual FinalNavigation.Parent
        reachedBy "/" |> shouldEqual FinalNavigation.Root

    // ------------------------------------------------------------- builders

    [<Test>]
    let ``builders report the errnos their syscalls do`` () : unit =
        let vfs =
            build
                [
                    mkdir (rootOf VirtualFileSystem.empty) "d"
                    mkfile (rootOf VirtualFileSystem.empty) "f"
                ]

        let root = rootOf vfs

        let file =
            VirtualFileSystem.resolveExisting root SymlinkPolicy.Follow (path "/f") vfs
            |> ok

        let directory =
            VirtualFileSystem.resolveExisting root SymlinkPolicy.Follow (path "/d") vfs
            |> ok

        VirtualFileSystem.createDirectory root (name "d") vfs
        |> shouldEqual (Error UnixError.EEXIST)

        VirtualFileSystem.createFile file (name "x") noBytes vfs
        |> shouldEqual (Error UnixError.ENOTDIR)

        VirtualFileSystem.createFile (InodeNumber 9999L) (name "x") noBytes vfs
        |> shouldEqual (Error UnixError.ENOENT)

        // link(2) refuses to hard-link a directory: it would make the graph a
        // non-tree and leave Parent naming only one container.
        VirtualFileSystem.hardLink root (name "d2") directory vfs
        |> shouldEqual (Error UnixError.EPERM)

        // ...but hard-linking a file is fine, and both names reach one inode.
        let linked = VirtualFileSystem.hardLink root (name "f2") file vfs |> ok
        VirtualFileSystem.checkInvariants linked |> shouldEqual []

        VirtualFileSystem.resolveExisting root SymlinkPolicy.Follow (path "/f2") linked
        |> shouldEqual (Ok file)

    [<Test>]
    let ``a rejected builder leaves the filesystem sound`` () : unit =
        let vfs = build [ mkdir (rootOf VirtualFileSystem.empty) "d" ]

        match VirtualFileSystem.createDirectory (rootOf vfs) (name "d") vfs with
        | Ok _ -> failwith "expected EEXIST"
        | Error _ ->
            // The burnt inode number is unobservable, since numbers are never
            // reused; what matters is that the original is untouched and sound.
            VirtualFileSystem.checkInvariants vfs |> shouldEqual []

    [<Test>]
    let ``inode numbers are never reused`` () : unit =
        let vfs = build [ mkfile (rootOf VirtualFileSystem.empty) "a" ]
        let before = VirtualFileSystem.nextInode vfs

        // A rejected creation still consumes a number.
        VirtualFileSystem.createFile (InodeNumber 9999L) (name "x") noBytes vfs
        |> Result.isError
        |> shouldEqual true

        let after =
            build
                [
                    mkfile (rootOf VirtualFileSystem.empty) "a"
                    mkfile (rootOf VirtualFileSystem.empty) "b"
                ]

        VirtualFileSystem.nextInode after |> shouldBeGreaterThan before

    // ------------------------------------------------------- pathOfDirectory

    [<Test>]
    let ``pathOfDirectory round-trips through resolve`` () : unit =
        let vfs =
            build
                [
                    mkdir (rootOf VirtualFileSystem.empty) "a"
                    fun vfs ->
                        let a =
                            VirtualFileSystem.resolveExisting (rootOf vfs) SymlinkPolicy.Follow (path "/a") vfs
                            |> ok

                        vfs |> mkdir a "b" |> mkfile a "f"
                ]

        for inode, content in Map.toList (VirtualFileSystem.inodes vfs) do
            match content with
            | InodeContent.Directory _ ->
                match VirtualFileSystem.pathOfDirectory inode vfs with
                | None -> failwith $"no path for directory %O{inode} in a sound filesystem"
                | Some absolute ->
                    VirtualFileSystem.resolveExisting
                        (rootOf vfs)
                        SymlinkPolicy.Follow
                        (UnixPath.ofAbsolute absolute)
                        vfs
                    |> shouldEqual (Ok inode)
            | InodeContent.RegularFile _
            | InodeContent.Symlink _ ->
                // Not a directory, so deliberately unanswerable: a hard-linked
                // file has no single path.
                VirtualFileSystem.pathOfDirectory inode vfs |> shouldEqual None

    [<Test>]
    let ``pathOfDirectory of the root is the root`` () : unit =
        VirtualFileSystem.pathOfDirectory (rootOf VirtualFileSystem.empty) VirtualFileSystem.empty
        |> shouldEqual (Some AbsoluteUnixPath.root)

    // ------------------------------------------------------------- invariants

    /// A directory holding one entry, for assembling defective graphs.
    let private dir (parent : InodeNumber) (entries : (string * InodeNumber) list) : InodeContent =
        InodeContent.Directory
            {
                Entries = entries |> List.map (fun (n, i) -> name n, i) |> Map.ofList
                Parent = parent
            }

    let private one = InodeNumber 1L
    let private two = InodeNumber 2L
    let private three = InodeNumber 3L

    /// Assert that `vfs` has exactly the defects `expected`, so that a test
    /// which accidentally builds a *differently* broken graph fails rather than
    /// passing on the wrong defect.
    let private shouldHaveDefects (expected : VirtualFileSystemDefect list) (vfs : VirtualFileSystem) : unit =
        VirtualFileSystem.checkInvariants vfs |> shouldEqual expected

    [<Test>]
    let ``RootMissing`` () : unit =
        VirtualFileSystem.Unchecked.ofParts Map.empty one two
        |> shouldHaveDefects [ VirtualFileSystemDefect.RootMissing one ]

    [<Test>]
    let ``RootIsNotDirectory`` () : unit =
        VirtualFileSystem.Unchecked.ofParts (Map.ofList [ one, InodeContent.RegularFile noBytes ]) one two
        |> shouldHaveDefects [ VirtualFileSystemDefect.RootIsNotDirectory one ]

    [<Test>]
    let ``RootParentIsNotSelf`` () : unit =
        VirtualFileSystem.Unchecked.ofParts (Map.ofList [ one, dir two [] ]) one two
        |> shouldHaveDefects [ VirtualFileSystemDefect.RootParentIsNotSelf (one, two) ]

    [<Test>]
    let ``RootHasIncomingLink`` () : unit =
        // The gap a bare "multiply linked" check misses: a single entry
        // pointing at the root is not multiply-linked, but it makes the graph
        // cyclic while every individual link count stays plausible.
        VirtualFileSystem.Unchecked.ofParts (Map.ofList [ one, dir one [ "loop", one ] ]) one two
        |> shouldHaveDefects [ VirtualFileSystemDefect.RootHasIncomingLink [ one, name "loop" ] ]

    [<Test>]
    let ``DanglingEntry`` () : unit =
        VirtualFileSystem.Unchecked.ofParts (Map.ofList [ one, dir one [ "gone", two ] ]) one three
        |> shouldHaveDefects [ VirtualFileSystemDefect.DanglingEntry (one, name "gone", two) ]

    [<Test>]
    let ``DanglingParent`` () : unit =
        VirtualFileSystem.Unchecked.ofParts
            (Map.ofList [ one, dir one [ "d", two ] ; two, dir three [] ])
            one
            (InodeNumber 4L)
        |> shouldHaveDefects
            [
                VirtualFileSystemDefect.DanglingParent (two, three)
                VirtualFileSystemDefect.ParentMismatch (two, three, one)
            ]

    [<Test>]
    let ``ParentIsNotDirectory`` () : unit =
        VirtualFileSystem.Unchecked.ofParts
            (Map.ofList
                [
                    one, dir one [ "f", two ; "d", three ]
                    two, InodeContent.RegularFile noBytes
                    three, dir two []
                ])
            one
            (InodeNumber 4L)
        |> shouldHaveDefects
            [
                VirtualFileSystemDefect.ParentIsNotDirectory (three, two)
                VirtualFileSystemDefect.ParentMismatch (three, two, one)
            ]

    [<Test>]
    let ``ParentMismatch`` () : unit =
        VirtualFileSystem.Unchecked.ofParts
            (Map.ofList [ one, dir one [ "a", two ; "b", three ] ; two, dir one [] ; three, dir two [] ])
            one
            (InodeNumber 4L)
        |> shouldHaveDefects [ VirtualFileSystemDefect.ParentMismatch (three, two, one) ]

    [<Test>]
    let ``DirectoryMultiplyLinked`` () : unit =
        VirtualFileSystem.Unchecked.ofParts
            (Map.ofList [ one, dir one [ "a", two ; "b", two ] ; two, dir one [] ])
            one
            three
        |> shouldHaveDefects
            [
                VirtualFileSystemDefect.DirectoryMultiplyLinked (two, [ one, name "a" ; one, name "b" ])
            ]

    [<Test>]
    let ``UnreachableFromRoot`` () : unit =
        VirtualFileSystem.Unchecked.ofParts
            (Map.ofList [ one, dir one [] ; two, InodeContent.RegularFile noBytes ])
            one
            three
        |> shouldHaveDefects [ VirtualFileSystemDefect.UnreachableFromRoot two ]

    [<Test>]
    let ``an internally consistent orphaned subtree is still unreachable`` () : unit =
        // Two directories that hold each other: every link count is 1, every
        // recorded parent agrees with reality, and nothing reaches them. This
        // is the cycle that the link-count rules alone cannot see, and the
        // reason reachability is computed through entries rather than parents.
        VirtualFileSystem.Unchecked.ofParts
            (Map.ofList
                [
                    one, dir one []
                    two, dir three [ "b", three ]
                    three, dir two [ "a", two ]
                ])
            one
            (InodeNumber 4L)
        |> shouldHaveDefects
            [
                VirtualFileSystemDefect.UnreachableFromRoot two
                VirtualFileSystemDefect.UnreachableFromRoot three
            ]

    [<Test>]
    let ``NextInodeNotFresh`` () : unit =
        VirtualFileSystem.Unchecked.ofParts (Map.ofList [ one, dir one [] ]) one one
        |> shouldHaveDefects [ VirtualFileSystemDefect.NextInodeNotFresh (one, one) ]

    [<Test>]
    let ``assertInvariants names the context and the defect`` () : unit =
        let broken = VirtualFileSystem.Unchecked.ofParts Map.empty one two

        let exn =
            Assert.Throws<Exception> (fun () ->
                VirtualFileSystem.assertInvariants "seed manifest" broken
                |> ignore<VirtualFileSystem>
            )

        exn.Message |> shouldContainText "seed manifest"
        exn.Message |> shouldContainText "RootMissing"

    [<Test>]
    let ``a forged default name or target is rejected at the boundary`` () : unit =
        // `private` on a struct union case stops construction but not
        // `Unchecked.defaultof`, and C# `default` reaches the same value. Left
        // unchecked, both produce a graph checkInvariants calls sound: an entry
        // no parsed path could ever name, or a symlink that crashes only later
        // when some unrelated resolution happens to traverse it.
        let vfs = VirtualFileSystem.empty

        let forgedName =
            Assert.Throws<Exception> (fun () ->
                VirtualFileSystem.createFile (rootOf vfs) Unchecked.defaultof<FileName> noBytes vfs
                |> ignore<Result<InodeNumber * VirtualFileSystem, UnixError>>
            )

        forgedName.Message |> shouldContainText "Unchecked.defaultof"

        let forgedTarget =
            Assert.Throws<Exception> (fun () ->
                VirtualFileSystem.createSymlink (rootOf vfs) (name "l") Unchecked.defaultof<SymlinkTarget> vfs
                |> ignore<Result<InodeNumber * VirtualFileSystem, UnixError>>
            )

        forgedTarget.Message |> shouldContainText "Unchecked.defaultof"

        // Every builder binds through one place, so the name check covers them
        // all rather than only the one probed above.
        for builder in
            [
                (fun n -> VirtualFileSystem.createDirectory (rootOf vfs) n vfs |> Result.map snd)
                (fun n -> VirtualFileSystem.createFile (rootOf vfs) n noBytes vfs |> Result.map snd)
                (fun n ->
                    VirtualFileSystem.createSymlink (rootOf vfs) n (target "x") vfs
                    |> Result.map snd
                )
            ] do
            Assert.Throws<Exception> (fun () ->
                builder Unchecked.defaultof<FileName>
                |> ignore<Result<VirtualFileSystem, UnixError>>
            )
            |> ignore<Exception>

    // ------------------------------------------------------------- properties

    /// A script of builder operations, each naming a directory by index into
    /// the directories that exist when it runs, so that generated filesystems
    /// are always plausible shapes rather than mostly-rejected noise.
    type private Step =
        | MakeDirectory of parent : int * name : string
        | MakeFile of parent : int * name : string
        | MakeSymlink of parent : int * name : string * target : string
        | MakeHardLink of parent : int * name : string * targetFile : int

    let private stepGen : Gen<Step> =
        let nameGen = Gen.elements [ "a" ; "b" ; "c" ; "d" ; ".hidden" ; "..x" ; "..." ]

        let targetGen =
            Gen.elements [ "a" ; "a/b" ; "/a" ; "/a/b" ; ".." ; "." ; "a/" ; "../a" ; "l" ; "l/x" ; "/" ]

        Gen.oneof
            [
                Gen.map2 (fun p n -> Step.MakeDirectory (p, n)) (Gen.choose (0, 5)) nameGen
                Gen.map2 (fun p n -> Step.MakeFile (p, n)) (Gen.choose (0, 5)) nameGen
                Gen.map3 (fun p n t -> Step.MakeSymlink (p, n, t)) (Gen.choose (0, 5)) nameGen targetGen
                Gen.map3 (fun p n t -> Step.MakeHardLink (p, n, t)) (Gen.choose (0, 5)) nameGen (Gen.choose (0, 5))
            ]

    let private applyStep (step : Step) (vfs : VirtualFileSystem) : VirtualFileSystem =
        let inodesOfKind (predicate : InodeContent -> bool) =
            VirtualFileSystem.inodes vfs
            |> Map.toList
            |> List.filter (fun (_, content) -> predicate content)
            |> List.map fst

        let directories =
            inodesOfKind (fun content ->
                match content with
                | InodeContent.Directory _ -> true
                | _ -> false
            )

        let files =
            inodesOfKind (fun content ->
                match content with
                | InodeContent.RegularFile _ -> true
                | _ -> false
            )

        let pick (xs : InodeNumber list) (i : int) = xs.[i % List.length xs]

        let outcome =
            match step with
            | Step.MakeDirectory (p, n) ->
                VirtualFileSystem.createDirectory (pick directories p) (name n) vfs
                |> Result.map snd
            | Step.MakeFile (p, n) ->
                VirtualFileSystem.createFile (pick directories p) (name n) noBytes vfs
                |> Result.map snd
            | Step.MakeSymlink (p, n, t) ->
                VirtualFileSystem.createSymlink (pick directories p) (name n) (target t) vfs
                |> Result.map snd
            | Step.MakeHardLink (p, n, t) ->
                if List.isEmpty files then
                    Ok vfs
                else
                    VirtualFileSystem.hardLink (pick directories p) (name n) (pick files t) vfs

        // A rejected step (EEXIST, mostly) leaves the filesystem alone, which is
        // itself part of what the property asserts.
        match outcome with
        | Ok updated -> updated
        | Error _ -> vfs

    let private filesystemGen : Gen<VirtualFileSystem> =
        Gen.listOf stepGen
        |> Gen.map (List.fold (fun vfs step -> applyStep step vfs) VirtualFileSystem.empty)

    [<Test>]
    let ``any sequence of builder operations leaves a sound filesystem`` () : unit =
        let property (vfs : VirtualFileSystem) : unit =
            VirtualFileSystem.checkInvariants vfs |> shouldEqual []

        Check.One (config, Prop.forAll (Arb.fromGen filesystemGen) property)

    [<Test>]
    let ``every directory's path resolves back to it`` () : unit =
        // The corollary of tree-ness: on a sound filesystem the Parent chain
        // always reaches the root, so pathOfDirectory is total on directories.
        let property (vfs : VirtualFileSystem) : unit =
            for inode, content in Map.toList (VirtualFileSystem.inodes vfs) do
                match content with
                | InodeContent.Directory _ ->
                    match VirtualFileSystem.pathOfDirectory inode vfs with
                    | None -> failwith $"no path for directory %O{inode} in a sound filesystem"
                    | Some absolute ->
                        // resolveExisting rather than resolve: only a path with
                        // no final name ("/", ".", "..") yields
                        // ResolvedTarget.Directory, and pathOfDirectory names
                        // every directory but the root.
                        VirtualFileSystem.resolveExisting
                            (rootOf vfs)
                            SymlinkPolicy.Follow
                            (UnixPath.ofAbsolute absolute)
                            vfs
                        |> shouldEqual (Ok inode)
                | _ -> ()

        Check.One (config, Prop.forAll (Arb.fromGen filesystemGen) property)

    [<Test>]
    let ``resolution never throws on a sound filesystem, whatever the path`` () : unit =
        // Except for the deliberate divergence crash, which the generated
        // targets cannot reach: they build chains far shorter than 33.
        let pathGen =
            Gen.elements
                [
                    "/"
                    ""
                    "."
                    ".."
                    "/.."
                    "/../.."
                    "a"
                    "/a"
                    "/a/"
                    "/a/b"
                    "/a/./b"
                    "/a/../b"
                    "//a//b//"
                    "/l"
                    "/l/"
                    "/l/x"
                    "/.hidden"
                    "/..."
                    "/a/b/c/d/e"
                ]

        let property (vfs : VirtualFileSystem, candidate : string) : unit =
            for policy in [ SymlinkPolicy.Follow ; SymlinkPolicy.NoFollowFinal ] do
                VirtualFileSystem.resolveFull (rootOf vfs) policy (path candidate) vfs
                |> ignore<Result<Resolution, UnixError>>

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.zip filesystemGen pathGen)) property)

    [<Test>]
    let ``resolveExisting agrees with resolve on what exists`` () : unit =
        let pathGen = Gen.elements [ "/" ; "/a" ; "/a/b" ; "/l" ; "/nx" ; "/a/nx" ; "/.." ]

        let property (vfs : VirtualFileSystem, candidate : string) : unit =
            let full =
                VirtualFileSystem.resolve (rootOf vfs) SymlinkPolicy.Follow (path candidate) vfs

            let existing =
                VirtualFileSystem.resolveExisting (rootOf vfs) SymlinkPolicy.Follow (path candidate) vfs

            match full, existing with
            | Ok (ResolvedTarget.Directory (a, _)), Ok b -> b |> shouldEqual a
            | Ok (ResolvedTarget.Entry (_, _, Some a)), Ok b -> b |> shouldEqual a
            | Ok (ResolvedTarget.Entry (_, _, None)), Error error -> error |> shouldEqual UnixError.ENOENT
            | Error a, Error b -> b |> shouldEqual a
            | a, b -> failwith $"resolve gave %A{a} but resolveExisting gave %A{b}"

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.zip filesystemGen pathGen)) property)
