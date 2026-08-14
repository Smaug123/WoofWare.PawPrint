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

    /// The limits every test here resolves under unless it is specifically about
    /// the limits themselves. Obtained from a platform rather than constructed,
    /// so no test can accidentally pin behaviour under a `MAXSYMLINKS` no real
    /// kernel has; Linux because that is what `KernelConfig` defaults to.
    let private limits : PathLimits =
        SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.linuxX64

    let private ok (result : Result<'a, UnixError>) : 'a =
        match result with
        | Ok value -> value
        | Error error -> failwith $"expected success, got %O{error}"

    let private noBytes : ImmutableArray<byte> = ImmutableArray<byte>.Empty

    let private filePerms : PermissionBits = PermissionBits.defaultForRegularFile

    let private dirPerms : PermissionBits = PermissionBits.defaultForDirectory

    /// The moment the tests build their filesystems at, where the test is not
    /// *about* time. A distinctive non-epoch value with a nanosecond part, so
    /// that a metadata assertion cannot pass against a zero someone forgot to
    /// set: `Unchecked.defaultof<UnixTimestamp>` is the epoch, so building at
    /// the epoch would make "the times were recorded" indistinguishable from
    /// "the times are default".
    let private buildTime : UnixTimestamp =
        UnixTimestamp.createOrFail "test" 1_700_000_000L 123_456_789

    let private emptyFs : VirtualFileSystem = VirtualFileSystem.empty buildTime

    /// Build a filesystem from a script of operations, failing loudly if any
    /// step is rejected. Keeps the tests below readable.
    let private build (steps : (VirtualFileSystem -> VirtualFileSystem) list) : VirtualFileSystem =
        steps |> List.fold (fun vfs step -> step vfs) emptyFs

    let private mkdir (parent : InodeNumber) (n : string) (vfs : VirtualFileSystem) : VirtualFileSystem =
        VirtualFileSystem.createDirectory parent (name n) dirPerms buildTime vfs
        |> ok
        |> snd

    let private mkfile (parent : InodeNumber) (n : string) (vfs : VirtualFileSystem) : VirtualFileSystem =
        VirtualFileSystem.createFile parent (name n) filePerms buildTime noBytes vfs
        |> ok
        |> snd

    let private mklink (parent : InodeNumber) (n : string) (t : string) (vfs : VirtualFileSystem) : VirtualFileSystem =
        VirtualFileSystem.createSymlink parent (name n) buildTime (target t) vfs
        |> ok
        |> snd

    /// An inode for the `Unchecked.ofParts` tests, whose subject is the shape of
    /// the graph rather than any metadata.
    let private inodeOf (content : InodeContent) : Inode =
        {
            Content = content
            Times = InodeTimes.createdAt buildTime
        }

    let private regularFileInode : Inode =
        inodeOf (InodeContent.RegularFile (noBytes, filePerms))

    let private rootOf (vfs : VirtualFileSystem) : InodeNumber = VirtualFileSystem.root vfs

    // ------------------------------------------------------------- the basics

    [<Test>]
    let ``empty is a sound filesystem containing only the root`` () : unit =
        VirtualFileSystem.checkInvariants emptyFs |> shouldEqual []

        VirtualFileSystem.inodes emptyFs |> Map.count |> shouldEqual 1

        // The root's parent is itself, so "/.." is "/".
        VirtualFileSystem.resolve limits (rootOf emptyFs) SymlinkPolicy.Follow (path "/..") emptyFs
        |> shouldEqual (Ok (ResolvedTarget.Directory (rootOf emptyFs, FinalNavigation.Parent)))

    [<Test>]
    let ``the empty path is ENOENT, not the directory we started from`` () : unit =
        // The trap this guards: a walk over zero components would silently mean
        // "the start directory", which is a successful answer to a call every
        // Unix rejects.
        VirtualFileSystem.resolve limits (rootOf emptyFs) SymlinkPolicy.Follow UnixPath.empty emptyFs
        |> shouldEqual (Error UnixError.ENOENT)

    [<Test>]
    let ``a relative path starting from a non-directory is ENOTDIR`` () : unit =
        let vfs = build [ mkfile (rootOf emptyFs) "f" ]

        let file =
            VirtualFileSystem.resolveExisting limits (rootOf vfs) SymlinkPolicy.Follow (path "/f") vfs
            |> ok

        VirtualFileSystem.resolve limits file SymlinkPolicy.Follow (path "a") vfs
        |> shouldEqual (Error UnixError.ENOTDIR)

    [<Test>]
    let ``a path cannot continue through a regular file`` () : unit =
        let vfs = build [ mkfile (rootOf emptyFs) "f" ]

        VirtualFileSystem.resolve limits (rootOf vfs) SymlinkPolicy.Follow (path "/f/x") vfs
        |> shouldEqual (Error UnixError.ENOTDIR)

    [<Test>]
    let ``a free name in the final position is not an error`` () : unit =
        // The whole point of stopping short of the final lookup: mkdir and
        // open(O_CREAT) need this state, and only stat turns it into ENOENT.
        let vfs = emptyFs

        VirtualFileSystem.resolve limits (rootOf vfs) SymlinkPolicy.Follow (path "/nx") vfs
        |> shouldEqual (Ok (ResolvedTarget.Entry (rootOf vfs, name "nx", None)))

        VirtualFileSystem.resolveExisting limits (rootOf vfs) SymlinkPolicy.Follow (path "/nx") vfs
        |> shouldEqual (Error UnixError.ENOENT)

        // ...but a free name part-way along is ENOENT even so.
        VirtualFileSystem.resolve limits (rootOf vfs) SymlinkPolicy.Follow (path "/nx/y") vfs
        |> shouldEqual (Error UnixError.ENOENT)

    // --------------------------------------------------- the trailing separator

    [<Test>]
    let ``a trailing separator is not desugared into a dot component`` () : unit =
        // Probed on macOS: mkdir("d/") succeeds while mkdir("nx/.") is ENOENT,
        // and rmdir("d/") succeeds while rmdir("d/.") is EINVAL. Desugaring
        // would collapse the Entry that mkdir("nx/") needs into a Directory,
        // and would make a free name report ENOENT.
        let vfs = emptyFs

        let resolution =
            VirtualFileSystem.resolveFull limits (rootOf vfs) SymlinkPolicy.Follow (path "/nx/") vfs
            |> ok

        resolution.Target
        |> shouldEqual (ResolvedTarget.Entry (rootOf vfs, name "nx", None))

        resolution.TrailingSeparatorDemanded |> shouldEqual true

        // Whereas the genuinely-dotted path has no final name at all, which is
        // what makes rmdir able to tell the two apart and report EINVAL.
        let withDot =
            VirtualFileSystem.resolveFull limits (rootOf vfs) SymlinkPolicy.Follow (path "/nx/.") vfs

        withDot |> shouldEqual (Error UnixError.ENOENT)

    [<Test>]
    let ``a trailing separator on an existing non-directory is ENOTDIR`` () : unit =
        // The part of the trailing-separator rule every platform agrees on.
        let vfs = build [ mkfile (rootOf emptyFs) "f" ]

        VirtualFileSystem.resolve limits (rootOf vfs) SymlinkPolicy.Follow (path "/f/") vfs
        |> shouldEqual (Error UnixError.ENOTDIR)

        // Without the separator the same path is perfectly fine.
        VirtualFileSystem.resolve limits (rootOf vfs) SymlinkPolicy.Follow (path "/f") vfs
        |> shouldEqual (
            Ok (
                ResolvedTarget.Entry (
                    rootOf vfs,
                    name "f",
                    Some (
                        VirtualFileSystem.resolveExisting limits (rootOf vfs) SymlinkPolicy.Follow (path "/f") vfs
                        |> ok
                    )
                )
            )
        )

    [<Test>]
    let ``a trailing separator follows a final symlink even under NoFollowFinal`` () : unit =
        // POSIX resolves "p/" as "p/.", and both platforms agree for lookups:
        // probed, lstat("ld/") stats the directory the link names.
        let vfs = build [ mkdir (rootOf emptyFs) "d" ; mklink (rootOf emptyFs) "ld" "d" ]

        let directory =
            VirtualFileSystem.resolveExisting limits (rootOf vfs) SymlinkPolicy.Follow (path "/d") vfs
            |> ok

        let withSlash =
            VirtualFileSystem.resolveFull limits (rootOf vfs) SymlinkPolicy.NoFollowFinal (path "/ld/") vfs
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
            VirtualFileSystem.resolveExisting limits (rootOf vfs) SymlinkPolicy.NoFollowFinal (path "/ld") vfs
            |> ok

        match VirtualFileSystem.tryGetContent link vfs with
        | Some (InodeContent.Symlink _) -> ()
        | other -> failwith $"expected the symlink itself, got %A{other}"

    [<Test>]
    let ``a symlink target's own trailing separator takes effect only when final`` () : unit =
        let vfs = build [ mkfile (rootOf emptyFs) "f" ; mklink (rootOf emptyFs) "lf" "f/" ]

        // "lf" expands to "f/", whose trailing separator now demands that f be
        // a directory. It is not.
        VirtualFileSystem.resolve limits (rootOf vfs) SymlinkPolicy.Follow (path "/lf") vfs
        |> shouldEqual (Error UnixError.ENOTDIR)

    // ------------------------------------------------------------- symlinks

    [<Test>]
    let ``a dangling final symlink under Follow is a free name, not an error`` () : unit =
        // open("/link", O_CREAT) where link -> /nx must create nx, so the walk
        // has to hand back the *target's* parent and name.
        let vfs = build [ mklink (rootOf emptyFs) "dang" "nx" ]

        VirtualFileSystem.resolve limits (rootOf vfs) SymlinkPolicy.Follow (path "/dang") vfs
        |> shouldEqual (Ok (ResolvedTarget.Entry (rootOf vfs, name "nx", None)))

        // But a dangling link whose target's *parent* is missing is ENOENT,
        // because that failure happens part-way along.
        let vfs = build [ mklink (rootOf emptyFs) "deep" "nx1/nx2" ]

        VirtualFileSystem.resolve limits (rootOf vfs) SymlinkPolicy.Follow (path "/deep") vfs
        |> shouldEqual (Error UnixError.ENOENT)

    [<Test>]
    let ``a rooted symlink target restarts at the root`` () : unit =
        let vfs =
            build
                [
                    mkdir (rootOf emptyFs) "a"
                    fun vfs ->
                        let a =
                            VirtualFileSystem.resolveExisting limits (rootOf vfs) SymlinkPolicy.Follow (path "/a") vfs
                            |> ok

                        vfs |> mkfile a "f" |> mklink a "up" "/f2"
                    mkfile (rootOf emptyFs) "f2"
                ]

        let f2 =
            VirtualFileSystem.resolveExisting limits (rootOf vfs) SymlinkPolicy.Follow (path "/f2") vfs
            |> ok

        VirtualFileSystem.resolveExisting limits (rootOf vfs) SymlinkPolicy.Follow (path "/a/up") vfs
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

    /// Both flavours, so neither column of the table rests on the other. These
    /// come from the platform rather than being written out here, because a
    /// literal would let this test agree with a wrong `pathLimits`.
    let private everyFlavour : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    [<Test>]
    let ``forged path limits are refused rather than silently obeyed`` () : unit =
        // `PathLimits.create` rejects a zero limit, but the type is a struct, so
        // `Unchecked.defaultof` carries one past the constructor. Left
        // unchecked, that limit does not crash: it makes the first symlink on
        // *any* path report ELOOP, which is a plausible-looking answer from a
        // kernel that cannot exist.
        let forged = Unchecked.defaultof<PathLimits>
        let vfs = build [ mkfile (rootOf emptyFs) "f" ; mklink (rootOf emptyFs) "l" "f" ]

        let exn =
            Assert.Throws<Exception> (fun () ->
                VirtualFileSystem.resolveExisting forged (rootOf vfs) SymlinkPolicy.Follow (path "/l") vfs
                |> ignore<Result<InodeNumber, UnixError>>
            )

        exn.Message |> shouldContainText "no Unix does"

        // ...and it is refused even where no symlink is involved, so that the
        // guard cannot be satisfied by a check that only runs at a traversal.
        Assert.Throws<Exception> (fun () ->
            VirtualFileSystem.resolveExisting forged (rootOf vfs) SymlinkPolicy.Follow (path "/f") vfs
            |> ignore<Result<InodeNumber, UnixError>>
        )
        |> ignore<Exception>

    [<Test>]
    let ``create refuses a limit no Unix has`` () : unit =
        Assert.Throws<Exception> (fun () -> PathLimits.create 0 |> ignore<PathLimits>)
        |> ignore<Exception>

    [<Test>]
    let ``a symlink chain exactly at a platform's limit resolves`` () : unit =
        for platform in everyFlavour do
            let limits = SimulatedUnixPlatform.pathLimits platform
            let vfs = symlinkChain (PathLimits.maxSymlinkTraversals limits)

            VirtualFileSystem.resolveExisting limits (rootOf vfs) SymlinkPolicy.Follow (path "/s1") vfs
            |> Result.isOk
            |> shouldEqual true

    [<Test>]
    let ``a symlink chain one past a platform's limit is ELOOP`` () : unit =
        for platform in everyFlavour do
            let limits = SimulatedUnixPlatform.pathLimits platform
            let vfs = symlinkChain (PathLimits.maxSymlinkTraversals limits + 1)

            VirtualFileSystem.resolveExisting limits (rootOf vfs) SymlinkPolicy.Follow (path "/s1") vfs
            |> shouldEqual (Error UnixError.ELOOP)

    [<Test>]
    let ``the band the two platforms disagree about is answered, each its own way`` () : unit =
        // The whole point of threading the limit: a chain of 33 to 40 links is
        // exactly where Linux and macOS differ, and this used to abort the
        // interpreter rather than choose. Now each platform gets its own answer.
        let darwin = SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.macOsArm64
        let linux = SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.linuxX64

        let inBetween = PathLimits.maxSymlinkTraversals darwin + 1
        inBetween |> shouldBeSmallerThan (PathLimits.maxSymlinkTraversals linux + 1)

        let vfs = symlinkChain inBetween

        VirtualFileSystem.resolveExisting darwin (rootOf vfs) SymlinkPolicy.Follow (path "/s1") vfs
        |> shouldEqual (Error UnixError.ELOOP)

        VirtualFileSystem.resolveExisting linux (rootOf vfs) SymlinkPolicy.Follow (path "/s1") vfs
        |> Result.isOk
        |> shouldEqual true

    [<Test>]
    let ``a chain past the limit is ELOOP even where the walk would have failed anyway`` () : unit =
        // A chain in the disputed band ending at a *missing* name, so the walk
        // has two reasons to fail and their order is what is pinned: under macOS
        // limits the 33rd traversal fails before the missing name is ever looked
        // up (ELOOP), while under Linux limits the walk reaches it (ENOENT).
        //
        // This is the case the deleted `failwith` was guarding, and it is worth
        // its own test because the limit is easy to apply only on the path where
        // the walk *succeeds*. It does not distinguish enforcement mid-walk from
        // enforcement after the fact: the count is monotone, so both answer
        // ELOOP here. What it does distinguish is a limit that only bounds
        // successful resolutions.
        let darwin = SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.macOsArm64
        let linux = SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.linuxX64
        let length = PathLimits.maxSymlinkTraversals darwin + 3

        let steps =
            [
                for i in 1..length do
                    let next = if i = length then "nowhere" else $"s%d{i + 1}"
                    yield fun vfs -> mklink (rootOf vfs) $"s%d{i}" next vfs
            ]

        let vfs = build steps

        VirtualFileSystem.resolveExisting darwin (rootOf vfs) SymlinkPolicy.Follow (path "/s1") vfs
        |> shouldEqual (Error UnixError.ELOOP)

        VirtualFileSystem.resolveExisting linux (rootOf vfs) SymlinkPolicy.Follow (path "/s1") vfs
        |> shouldEqual (Error UnixError.ENOENT)

    [<Test>]
    let ``a self-extending symlink terminates rather than growing forever`` () : unit =
        // The case that defeats cycle detection: "l" -> "l/x" never repeats a
        // (directory, remaining) state, it just grows the path. Only the
        // traversal count stops it, which is why there is no seen-state set.
        let vfs = build [ mklink (rootOf emptyFs) "l" "l/x" ]

        VirtualFileSystem.resolveExisting limits (rootOf vfs) SymlinkPolicy.Follow (path "/l") vfs
        |> shouldEqual (Error UnixError.ELOOP)

    [<Test>]
    let ``a symlink cycle is ELOOP rather than a crash`` () : unit =
        // Both platforms report ELOOP for a cycle, so this must be answered
        // rather than referred back as a divergence — the count reaches the
        // no-platform-allows bound before it reaches the divergent band's top.
        let vfs =
            build [ mklink (rootOf emptyFs) "a" "b" ; mklink (rootOf emptyFs) "b" "a" ]

        VirtualFileSystem.resolveExisting limits (rootOf vfs) SymlinkPolicy.Follow (path "/a") vfs
        |> shouldEqual (Error UnixError.ELOOP)

    [<Test>]
    let ``a symlink target is stored verbatim`` () : unit =
        // readlink(2) returns the stored bytes unchanged and lstat reports
        // their length as st_size, but UnixPath.parse collapses "//". Storing
        // a parsed path would make FileInfo.LinkTarget disagree with every Unix.
        let raw = "a//b/"
        let vfs = build [ mklink (rootOf emptyFs) "l" raw ]

        let link =
            VirtualFileSystem.resolveExisting limits (rootOf vfs) SymlinkPolicy.NoFollowFinal (path "/l") vfs
            |> ok

        match VirtualFileSystem.tryGetContent link vfs with
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
                    mkdir (rootOf emptyFs) "d"
                    mklink (rootOf emptyFs) "l1" "."
                    mklink (rootOf emptyFs) "l2" "d/.."
                    mklink (rootOf emptyFs) "l3" "/"
                ]

        let reachedBy (candidate : string) =
            match VirtualFileSystem.resolve limits (rootOf vfs) SymlinkPolicy.NoFollowFinal (path candidate) vfs with
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
        let vfs = build [ mkdir (rootOf emptyFs) "d" ; mkfile (rootOf emptyFs) "f" ]

        let root = rootOf vfs

        let file =
            VirtualFileSystem.resolveExisting limits root SymlinkPolicy.Follow (path "/f") vfs
            |> ok

        let directory =
            VirtualFileSystem.resolveExisting limits root SymlinkPolicy.Follow (path "/d") vfs
            |> ok

        VirtualFileSystem.createDirectory root (name "d") dirPerms buildTime vfs
        |> shouldEqual (Error UnixError.EEXIST)

        VirtualFileSystem.createFile file (name "x") filePerms buildTime noBytes vfs
        |> shouldEqual (Error UnixError.ENOTDIR)

        VirtualFileSystem.createFile (InodeNumber 9999L) (name "x") filePerms buildTime noBytes vfs
        |> shouldEqual (Error UnixError.ENOENT)

        // link(2) refuses to hard-link a directory: it would make the graph a
        // non-tree and leave Parent naming only one container.
        VirtualFileSystem.hardLink root (name "d2") directory buildTime vfs
        |> shouldEqual (Error UnixError.EPERM)

        // ...but hard-linking a file is fine, and both names reach one inode.
        let linked = VirtualFileSystem.hardLink root (name "f2") file buildTime vfs |> ok
        VirtualFileSystem.checkInvariants linked |> shouldEqual []

        VirtualFileSystem.resolveExisting limits root SymlinkPolicy.Follow (path "/f2") linked
        |> shouldEqual (Ok file)

    [<Test>]
    let ``creating into the about-to-be-allocated inode is ENOENT, not self-parenthood`` () : unit =
        // The parent is checked *before* the child is allocated, so that a
        // parent naming exactly the next inode number cannot be satisfied by
        // the allocation itself. Otherwise createDirectory would install the
        // new directory at that number, find it, and bind it as its own child —
        // returning Ok for a filesystem that is unreachable from the root.
        let vfs = build [ mkdir (rootOf emptyFs) "d" ]
        let absent = VirtualFileSystem.nextInode vfs

        VirtualFileSystem.createDirectory absent (name "x") dirPerms buildTime vfs
        |> shouldEqual (Error UnixError.ENOENT)

        VirtualFileSystem.createFile absent (name "x") filePerms buildTime noBytes vfs
        |> shouldEqual (Error UnixError.ENOENT)

        VirtualFileSystem.createSymlink absent (name "x") buildTime (target "y") vfs
        |> shouldEqual (Error UnixError.ENOENT)

    [<Test>]
    let ``a rejected builder leaves the filesystem sound`` () : unit =
        let vfs = build [ mkdir (rootOf emptyFs) "d" ]

        match VirtualFileSystem.createDirectory (rootOf vfs) (name "d") dirPerms buildTime vfs with
        | Ok _ -> failwith "expected EEXIST"
        | Error _ ->
            // The burnt inode number is unobservable, since numbers are never
            // reused; what matters is that the original is untouched and sound.
            VirtualFileSystem.checkInvariants vfs |> shouldEqual []

    [<Test>]
    let ``inode numbers are never reused`` () : unit =
        let vfs = build [ mkfile (rootOf emptyFs) "a" ]
        let before = VirtualFileSystem.nextInode vfs

        // A rejected creation still consumes a number.
        VirtualFileSystem.createFile (InodeNumber 9999L) (name "x") filePerms buildTime noBytes vfs
        |> Result.isError
        |> shouldEqual true

        let after = build [ mkfile (rootOf emptyFs) "a" ; mkfile (rootOf emptyFs) "b" ]

        VirtualFileSystem.nextInode after |> shouldBeGreaterThan before

    // ------------------------------------------------------- pathOfDirectory

    [<Test>]
    let ``pathOfDirectory round-trips through resolve`` () : unit =
        let vfs =
            build
                [
                    mkdir (rootOf emptyFs) "a"
                    fun vfs ->
                        let a =
                            VirtualFileSystem.resolveExisting limits (rootOf vfs) SymlinkPolicy.Follow (path "/a") vfs
                            |> ok

                        vfs |> mkdir a "b" |> mkfile a "f"
                ]

        for inode, entry in Map.toList (VirtualFileSystem.inodes vfs) do
            match entry.Content with
            | InodeContent.Directory _ ->
                match VirtualFileSystem.pathOfDirectory inode vfs with
                | None -> failwith $"no path for directory %O{inode} in a sound filesystem"
                | Some absolute ->
                    VirtualFileSystem.resolveExisting
                        limits
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
        VirtualFileSystem.pathOfDirectory (rootOf emptyFs) emptyFs
        |> shouldEqual (Some AbsoluteUnixPath.root)

    // ------------------------------------------------------------- invariants

    /// A directory holding one entry, for assembling defective graphs.
    let private dir (parent : InodeNumber) (entries : (string * InodeNumber) list) : Inode =
        inodeOf (
            InodeContent.Directory
                {
                    Entries = entries |> List.map (fun (n, i) -> name n, i) |> Map.ofList
                    Parent = parent
                    Permissions = dirPerms
                }
        )

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
        VirtualFileSystem.Unchecked.ofParts (Map.ofList [ one, regularFileInode ]) one two
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
                    two, regularFileInode
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
        VirtualFileSystem.Unchecked.ofParts (Map.ofList [ one, dir one [] ; two, regularFileInode ]) one three
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
        let vfs = emptyFs

        let forgedName =
            Assert.Throws<Exception> (fun () ->
                VirtualFileSystem.createFile (rootOf vfs) Unchecked.defaultof<FileName> filePerms buildTime noBytes vfs
                |> ignore<Result<InodeNumber * VirtualFileSystem, UnixError>>
            )

        forgedName.Message |> shouldContainText "Unchecked.defaultof"

        let forgedTarget =
            Assert.Throws<Exception> (fun () ->
                VirtualFileSystem.createSymlink
                    (rootOf vfs)
                    (name "l")
                    buildTime
                    Unchecked.defaultof<SymlinkTarget>
                    vfs
                |> ignore<Result<InodeNumber * VirtualFileSystem, UnixError>>
            )

        forgedTarget.Message |> shouldContainText "Unchecked.defaultof"

        // ImmutableArray is a struct too, and its default wraps a null array —
        // which is not an empty file but an uninitialised one.
        let forgedContents =
            Assert.Throws<Exception> (fun () ->
                VirtualFileSystem.createFile
                    (rootOf vfs)
                    (name "f")
                    filePerms
                    buildTime
                    Unchecked.defaultof<ImmutableArray<byte>>
                    vfs
                |> ignore<Result<InodeNumber * VirtualFileSystem, UnixError>>
            )

        forgedContents.Message |> shouldContainText "ImmutableArray<byte>.Empty"

        // ...and the genuinely empty file is still fine.
        VirtualFileSystem.createFile (rootOf vfs) (name "f") filePerms buildTime ImmutableArray<byte>.Empty vfs
        |> Result.isOk
        |> shouldEqual true

        // Every builder binds through one place, so the name check covers them
        // all rather than only the one probed above.
        for builder in
            [
                (fun n ->
                    VirtualFileSystem.createDirectory (rootOf vfs) n dirPerms buildTime vfs
                    |> Result.map snd
                )
                (fun n ->
                    VirtualFileSystem.createFile (rootOf vfs) n filePerms buildTime noBytes vfs
                    |> Result.map snd
                )
                (fun n ->
                    VirtualFileSystem.createSymlink (rootOf vfs) n buildTime (target "x") vfs
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
                Gen.map2 (fun p n -> Step.MakeDirectory (p, n)) (Gen.choose (0, 9)) nameGen
                Gen.map2 (fun p n -> Step.MakeFile (p, n)) (Gen.choose (0, 9)) nameGen
                Gen.map3 (fun p n t -> Step.MakeSymlink (p, n, t)) (Gen.choose (0, 9)) nameGen targetGen
                Gen.map3 (fun p n t -> Step.MakeHardLink (p, n, t)) (Gen.choose (0, 9)) nameGen (Gen.choose (0, 9))
            ]

    /// A distinct moment for each step, so that a timestamp copied from the
    /// wrong inode — or never moved at all — is visible rather than
    /// indistinguishable from the right one.
    let private tickOf (index : int) : UnixTimestamp =
        UnixTimestamp.createOrFail
            "test"
            (UnixTimestamp.seconds buildTime + int64 index)
            (UnixTimestamp.nanoseconds buildTime)

    let private applyStep (now : UnixTimestamp) (step : Step) (vfs : VirtualFileSystem) : VirtualFileSystem =
        let inodesOfKind (predicate : InodeContent -> bool) =
            VirtualFileSystem.inodes vfs
            |> Map.toList
            |> List.filter (fun (_, entry) -> predicate entry.Content)
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

        // Deliberately able to name an inode that does not exist, and in
        // particular the one about to be allocated. Restricting the generator
        // to existing directories is why this property missed a builder that
        // returned Ok for a self-parented, unreachable directory: the failure
        // needed a parent equal to `nextInode`, which the generator could not
        // produce. A rejected step leaves the filesystem alone, so widening the
        // alphabet costs nothing.
        let pick (xs : InodeNumber list) (i : int) =
            if i < List.length xs then xs.[i]
            elif i % 2 = 0 then VirtualFileSystem.nextInode vfs
            else InodeNumber (int64 (1000 + i))

        let outcome =
            match step with
            | Step.MakeDirectory (p, n) ->
                VirtualFileSystem.createDirectory (pick directories p) (name n) dirPerms now vfs
                |> Result.map snd
            | Step.MakeFile (p, n) ->
                VirtualFileSystem.createFile (pick directories p) (name n) filePerms now noBytes vfs
                |> Result.map snd
            | Step.MakeSymlink (p, n, t) ->
                VirtualFileSystem.createSymlink (pick directories p) (name n) now (target t) vfs
                |> Result.map snd
            | Step.MakeHardLink (p, n, t) ->
                if List.isEmpty files then
                    Ok vfs
                else
                    VirtualFileSystem.hardLink (pick directories p) (name n) (pick files t) now vfs

        // A rejected step (EEXIST, mostly) leaves the filesystem alone, which is
        // itself part of what the property asserts.
        match outcome with
        | Ok updated -> updated
        | Error _ -> vfs

    let private filesystemGen : Gen<VirtualFileSystem> =
        Gen.listOf stepGen
        |> Gen.map (fun steps ->
            steps
            |> List.mapi (fun index step -> tickOf (index + 1), step)
            |> List.fold (fun vfs (now, step) -> applyStep now step vfs) emptyFs
        )

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
            for inode, entry in Map.toList (VirtualFileSystem.inodes vfs) do
                match entry.Content with
                | InodeContent.Directory _ ->
                    match VirtualFileSystem.pathOfDirectory inode vfs with
                    | None -> failwith $"no path for directory %O{inode} in a sound filesystem"
                    | Some absolute ->
                        // resolveExisting rather than resolve: only a path with
                        // no final name ("/", ".", "..") yields
                        // ResolvedTarget.Directory, and pathOfDirectory names
                        // every directory but the root.
                        VirtualFileSystem.resolveExisting
                            limits
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
                VirtualFileSystem.resolveFull limits (rootOf vfs) policy (path candidate) vfs
                |> ignore<Result<Resolution, UnixError>>

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.zip filesystemGen pathGen)) property)

    [<Test>]
    let ``resolveExisting agrees with resolve on what exists`` () : unit =
        let pathGen = Gen.elements [ "/" ; "/a" ; "/a/b" ; "/l" ; "/nx" ; "/a/nx" ; "/.." ]

        let property (vfs : VirtualFileSystem, candidate : string) : unit =
            let full =
                VirtualFileSystem.resolve limits (rootOf vfs) SymlinkPolicy.Follow (path candidate) vfs

            let existing =
                VirtualFileSystem.resolveExisting limits (rootOf vfs) SymlinkPolicy.Follow (path candidate) vfs

            match full, existing with
            | Ok (ResolvedTarget.Directory (a, _)), Ok b -> b |> shouldEqual a
            | Ok (ResolvedTarget.Entry (_, _, Some a)), Ok b -> b |> shouldEqual a
            | Ok (ResolvedTarget.Entry (_, _, None)), Error error -> error |> shouldEqual UnixError.ENOENT
            | Error a, Error b -> b |> shouldEqual a
            | a, b -> failwith $"resolve gave %A{a} but resolveExisting gave %A{b}"

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.zip filesystemGen pathGen)) property)

    // --------------------------------------------------------------- metadata

    let private timesOf (inode : InodeNumber) (vfs : VirtualFileSystem) : InodeTimes =
        match VirtualFileSystem.tryGet inode vfs with
        | Some entry -> entry.Times
        | None -> failwith $"no such inode %O{inode}"

    [<Test>]
    let ``permission bits are exactly chmod's domain`` () : unit =
        PermissionBits.parse 0o7777 |> Option.isSome |> shouldEqual true
        PermissionBits.parse 0 |> Option.isSome |> shouldEqual true
        PermissionBits.parse -1 |> shouldEqual None

        // The one that matters: a caller handing over a whole `st_mode` is
        // making exactly the type/permission conflation this type exists to
        // prevent, and is refused rather than silently masked down to 0o644.
        PermissionBits.parse 0o100644 |> shouldEqual None
        PermissionBits.parse 0o10000 |> shouldEqual None

    [<Test>]
    let ``the default permissions are the umask-022 derivation, not invented constants`` () : unit =
        // 0o666 and 0o777 are what `open(2)` and `mkdir(2)` are actually passed;
        // 022 is the umask that produces the familiar 644/755. Asserting the
        // arithmetic as well as the answer keeps the doc comment honest about
        // where the numbers come from.
        PermissionBits.toInt PermissionBits.defaultForRegularFile
        |> shouldEqual (0o666 &&& ~~~0o022)

        PermissionBits.toInt PermissionBits.defaultForDirectory
        |> shouldEqual (0o777 &&& ~~~0o022)

        PermissionBits.toInt PermissionBits.defaultForRegularFile |> shouldEqual 0o644
        PermissionBits.toInt PermissionBits.defaultForDirectory |> shouldEqual 0o755

    [<Test>]
    let ``a timestamp is a timespec, not a nanosecond count`` () : unit =
        UnixTimestamp.create 0L 999_999_999 |> Option.isSome |> shouldEqual true
        UnixTimestamp.create 0L 1_000_000_000 |> shouldEqual None
        UnixTimestamp.create 0L -1 |> shouldEqual None

        // Pre-1970 is an ordinary mtime — tar archives are full of them — so
        // negative seconds are representable even though negative nanoseconds
        // are not.
        UnixTimestamp.create -1L 0 |> Option.isSome |> shouldEqual true

        // Seconds are a genuine int64, not the ±292 years a single nanosecond
        // count would have bought. `File.SetLastWriteTime` can be handed a
        // DateTime well outside that, and a filesystem does not clamp.
        UnixTimestamp.seconds (UnixTimestamp.createOrFail "test" 300_000_000_000L 0)
        |> shouldEqual 300_000_000_000L

        // Ordering is lexicographic on (seconds, nanoseconds), which the
        // invariants below rely on.
        UnixTimestamp.createOrFail "test" 1L 0 < UnixTimestamp.createOrFail "test" 1L 1
        |> shouldEqual true

        UnixTimestamp.createOrFail "test" 1L 999_999_999 < UnixTimestamp.createOrFail "test" 2L 0
        |> shouldEqual true

        // There is no `assertValid` counterpart to FileName's, because there is
        // nothing to catch: the forged value is a legal timestamp.
        Unchecked.defaultof<UnixTimestamp> |> shouldEqual UnixTimestamp.epoch

    [<Test>]
    let ``a pre-epoch timestamp renders as the instant it is`` () : unit =
        // A timespec is seconds *plus* nanoseconds, so a negative instant is not
        // the pair with a minus glued on the front: (-1, 5e8) is half a second
        // *before* the epoch, not one and a half. Printing the fields adjacently
        // would name a different moment — in a diagnostic, which is exactly
        // where someone would trust it.
        let render (seconds : int64) (nanoseconds : int) : string =
            sprintf "%O" (UnixTimestamp.createOrFail "test" seconds nanoseconds)

        render -1L 500_000_000 |> shouldEqual "-0.500000000"
        render -2L 500_000_000 |> shouldEqual "-1.500000000"
        render -1L 999_999_999 |> shouldEqual "-0.000000001"

        // Whole negative seconds have no fractional part to carry.
        render -1L 0 |> shouldEqual "-1.000000000"

        // ...and the ordinary case is unaffected.
        render 1L 500_000_000 |> shouldEqual "1.500000000"
        render 0L 0 |> shouldEqual "0.000000000"

    [<Test>]
    let ``permissions are stored for files and directories and derived for symlinks`` () : unit =
        let vfs =
            build
                [
                    mkfile (rootOf emptyFs) "f"
                    mkdir (rootOf emptyFs) "d"
                    mklink (rootOf emptyFs) "l" "f"
                ]

        let permissionsOf (p : string) : InodePermissions =
            let inode =
                VirtualFileSystem.resolveExisting limits (rootOf vfs) SymlinkPolicy.NoFollowFinal (path p) vfs
                |> ok

            match VirtualFileSystem.tryGet inode vfs with
            | Some entry -> VirtualFileSystem.permissions entry
            | None -> failwith "missing inode"

        permissionsOf "/f" |> shouldEqual (InodePermissions.Stored filePerms)
        permissionsOf "/d" |> shouldEqual (InodePermissions.Stored dirPerms)

        // Not `Stored 0o777`: a symlink's bits are a property of the platform
        // (Linux always 0o777; macOS applies the creating umask — probed), and
        // no syscall PawPrint models can make two links differ, so storing one
        // could only ever describe a filesystem no kernel produced.
        permissionsOf "/l" |> shouldEqual InodePermissions.PlatformSymlinkDefault

    [<Test>]
    let ``a fresh inode's four times are all the moment it was created`` () : unit =
        let vfs = build [ mkfile (rootOf emptyFs) "f" ]

        let file =
            VirtualFileSystem.resolveExisting limits (rootOf vfs) SymlinkPolicy.Follow (path "/f") vfs
            |> ok

        timesOf file vfs |> shouldEqual (InodeTimes.createdAt buildTime)

        // ...which is to say, all four really are the *supplied* moment rather
        // than a default that happens to look plausible.
        (timesOf file vfs).Birth |> shouldEqual buildTime

        UnixTimestamp.nanoseconds (timesOf file vfs).Modification
        |> shouldEqual 123_456_789

    [<Test>]
    let ``gaining an entry moves a directory's mtime and ctime, and nothing else`` () : unit =
        let later = UnixTimestamp.createOrFail "test" 1_700_000_500L 7

        let root = rootOf emptyFs
        let before = timesOf root emptyFs

        let vfs =
            VirtualFileSystem.createFile root (name "f") filePerms later noBytes emptyFs
            |> ok
            |> snd

        let after = timesOf root vfs

        // What a kernel moves: the directory's contents changed, so mtime; and
        // any change to the inode moves ctime with it.
        after.Modification |> shouldEqual later
        after.StatusChange |> shouldEqual later

        // What it does not: nothing read the directory, and it is not reborn.
        after.Access |> shouldEqual before.Access
        after.Birth |> shouldEqual before.Birth

    [<Test>]
    let ``a hard link moves the target's ctime but not its mtime`` () : unit =
        let later = UnixTimestamp.createOrFail "test" 1_700_000_900L 0
        let vfs = build [ mkfile (rootOf emptyFs) "f" ]

        let file =
            VirtualFileSystem.resolveExisting limits (rootOf vfs) SymlinkPolicy.Follow (path "/f") vfs
            |> ok

        let linked =
            VirtualFileSystem.hardLink (rootOf vfs) (name "f2") file later vfs |> ok

        let after = timesOf file linked

        // link(2) changes the inode's link count, which is a change to the
        // inode — but it touches no byte of the file, so mtime stays put. This
        // is the case that makes StatusChange worth storing separately rather
        // than deriving it from Modification.
        after.StatusChange |> shouldEqual later
        after.Modification |> shouldEqual buildTime
        after.Birth |> shouldEqual buildTime

        // The containing directory gained an entry, so its own pair moves.
        (timesOf (rootOf linked) linked).Modification |> shouldEqual later

    [<Test>]
    let ``every inode's times respect the order a kernel would have moved them`` () : unit =
        // Each generated step happens at its own moment (see `tickOf`), so a
        // timestamp copied from the wrong inode, or never moved at all, shows up
        // here rather than being indistinguishable from the right answer.
        let mutable observedLateModification = 0
        let mutable observedCtimeAheadOfMtime = 0

        let property (vfs : VirtualFileSystem) : unit =
            for inode, entry in Map.toList (VirtualFileSystem.inodes vfs) do
                let times = entry.Times

                // Nothing in this slice *reads*, so atime never moves off
                // creation. When `open`/`read` land, this is the line that has
                // to change — deliberately, rather than silently.
                times.Access |> shouldEqual times.Birth

                if times.Birth > times.Modification then
                    failwith $"inode %O{inode} was modified before it was born: %A{times}"

                if times.Modification > times.StatusChange then
                    failwith $"inode %O{inode} changed contents after its inode last changed: %A{times}"

                if times.Modification > times.Birth then
                    observedLateModification <- observedLateModification + 1

                if times.StatusChange > times.Modification then
                    observedCtimeAheadOfMtime <- observedCtimeAheadOfMtime + 1

        Check.One (config, Prop.forAll (Arb.fromGen filesystemGen) property)

        // Without these the property is satisfied by a model that never moves a
        // timestamp at all: every inode would trivially have all four equal, and
        // every comparison above would hold vacuously.
        observedLateModification |> shouldBeGreaterThan 100
        observedCtimeAheadOfMtime |> shouldBeGreaterThan 10
