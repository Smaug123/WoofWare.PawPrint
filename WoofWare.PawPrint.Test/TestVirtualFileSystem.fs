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
        Assert.Throws<Exception> (fun () ->
            PathLimits.create 0 4096 (NameLengthLimit.Utf8Bytes 255) SpliceLengthRecheck.NoRecheck
            |> ignore<PathLimits>
        )
        |> ignore<Exception>

        Assert.Throws<Exception> (fun () ->
            PathLimits.create 40 4096 (NameLengthLimit.Utf8Bytes 0) SpliceLengthRecheck.NoRecheck
            |> ignore<PathLimits>
        )
        |> ignore<Exception>

    [<Test>]
    let ``create refuses its two int arguments the wrong way round`` () : unit =
        // The one shape a type cannot catch: `MaxSymlinkTraversals` and
        // `PathMaxBytes` are both `int`, adjacent, and a swap would give a
        // kernel that permits 1024 traversals and a 32-byte PATH_MAX — wrong in
        // a way no test of *resolution* would obviously report.
        let exn =
            Assert.Throws<Exception> (fun () ->
                PathLimits.create 4096 40 (NameLengthLimit.Utf8Bytes 255) SpliceLengthRecheck.NoRecheck
                |> ignore<PathLimits>
            )

        exn.Message |> shouldContainText "wrong way round"

    // ------------------------------------------------------------- NAME_MAX

    let private darwinLimits : PathLimits =
        SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.macOsArm64

    let private linuxLimits : PathLimits =
        SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.linuxX64

    /// Resolve a bare name in the root of an otherwise empty filesystem, so the
    /// only thing that can be reported is the name's own length.
    let private resolveName (limits : PathLimits) (candidate : string) : Result<InodeNumber, UnixError> =
        VirtualFileSystem.resolveExisting limits (rootOf emptyFs) SymlinkPolicy.Follow (path ("/" + candidate)) emptyFs

    [<Test>]
    let ``a name of 255 ASCII characters is permitted and 256 is not, on both`` () : unit =
        // The row both platforms agree on. On its own it is satisfied by a
        // byte-counting implementation *and* by a UTF-16-counting one, which is
        // why the multi-byte test below exists.
        for limits in [ darwinLimits ; linuxLimits ] do
            resolveName limits (String.replicate 255 "a")
            |> shouldEqual (Error UnixError.ENOENT)

            resolveName limits (String.replicate 256 "a")
            |> shouldEqual (Error UnixError.ENAMETOOLONG)

    [<Test>]
    let ``NAME_MAX counts bytes on Linux and UTF-16 code units on Darwin`` () : unit =
        // The measured divergence, in the one case that separates the two
        // implementations. "中" is three UTF-8 bytes and one UTF-16 unit, so 255
        // of them are 765 bytes and 255 units:
        //
        //   * APFS permits it (probed: it resolves, and `creat` agrees), so a
        //     byte-counting implementation is wrong on macOS;
        //   * ext4 refuses it (probed), so a `String.Length` implementation is
        //     wrong on Linux — and `String.Length` is exactly the UTF-16 count,
        //     which is what makes that mistake invisible on a Mac.
        //
        // Both halves are needed. The Darwin half alone would also pass with no
        // NAME_MAX enforcement at all.
        let name255 = String.replicate 255 "中"

        resolveName darwinLimits name255 |> shouldEqual (Error UnixError.ENOENT)
        resolveName linuxLimits name255 |> shouldEqual (Error UnixError.ENAMETOOLONG)

        // ...and the Linux boundary in its own unit: 85 of them are exactly 255
        // bytes, 86 are 258.
        resolveName linuxLimits (String.replicate 85 "中")
        |> shouldEqual (Error UnixError.ENOENT)

        resolveName linuxLimits (String.replicate 86 "中")
        |> shouldEqual (Error UnixError.ENAMETOOLONG)

    [<Test>]
    let ``the Darwin boundary is UTF-16 code units, not characters`` () : unit =
        // An emoji is one character but two UTF-16 units, so this separates
        // "255 units" from "255 characters" — the latter would permit both.
        // Probed on APFS: 127 emoji + one ASCII (255 units) resolves, and one
        // more ASCII (256 units) is ENAMETOOLONG, though both are ~510 bytes.
        let emoji = "\U0001F600"

        resolveName darwinLimits (String.replicate 127 emoji + "a")
        |> shouldEqual (Error UnixError.ENOENT)

        resolveName darwinLimits (String.replicate 127 emoji + "aa")
        |> shouldEqual (Error UnixError.ENAMETOOLONG)

    [<Test>]
    let ``an over-long component under a missing parent is ENOENT, not ENAMETOOLONG`` () : unit =
        // Precedence, measured identically on both kernels: the walk fails at
        // the missing parent before it ever reaches the long name. An
        // implementation that screened the whole path for over-long components
        // up front would report ENAMETOOLONG here.
        let tooLong = String.replicate 300 "a"

        VirtualFileSystem.resolveExisting
            linuxLimits
            (rootOf emptyFs)
            SymlinkPolicy.Follow
            (path ("/nxdir/" + tooLong))
            emptyFs
        |> shouldEqual (Error UnixError.ENOENT)

        // ...whereas with the long component *first*, it is reached and refused.
        VirtualFileSystem.resolveExisting
            linuxLimits
            (rootOf emptyFs)
            SymlinkPolicy.Follow
            (path ("/" + tooLong + "/x"))
            emptyFs
        |> shouldEqual (Error UnixError.ENAMETOOLONG)

    [<Test>]
    let ``NAME_MAX applies to a component spliced in from a symlink target`` () : unit =
        // The reason this check lives in the walk rather than at the syscall
        // boundary: the guest's own path is short, and the over-long component
        // only exists after the link is expanded. A check on the incoming
        // pathname could not see this at all.
        let tooLong = String.replicate 300 "a"
        let vfs = build [ mklink (rootOf emptyFs) "l" tooLong ]

        VirtualFileSystem.resolveExisting linuxLimits (rootOf vfs) SymlinkPolicy.Follow (path "/l") vfs
        |> shouldEqual (Error UnixError.ENAMETOOLONG)

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
        // A chain of 33 to 40 links is exactly where Linux and macOS differ,
        // so each platform must get its own answer.
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
        // The limit is easy to apply only on the path where the walk
        // *succeeds*, so this pins that a failing walk is bounded too. It does
        // not distinguish enforcement mid-walk from enforcement after the
        // fact: the count is monotone, so both answer ELOOP here. What it does
        // distinguish is a limit that only bounds successful resolutions.
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
        // their length as st_size, so a target that was created as "a//b/" must
        // read back as "a//b/" or FileInfo.LinkTarget disagrees with every Unix.
        let raw = "a//b/"
        let vfs = build [ mklink (rootOf emptyFs) "l" raw ]

        let link =
            VirtualFileSystem.resolveExisting limits (rootOf vfs) SymlinkPolicy.NoFollowFinal (path "/l") vfs
            |> ok

        match VirtualFileSystem.tryGetContent link vfs with
        | Some (InodeContent.Symlink stored) ->
            SymlinkTarget.toString stored |> shouldEqual raw
            SymlinkTarget.toUtf8 stored |> Seq.length |> shouldEqual raw.Length
            // The traversal view keeps the spelling too — `UnixPath` is verbatim
            // for the same reason this is, so converting one to the other loses
            // nothing. Only `components` collapses, and only where the kernel
            // does.
            SymlinkTarget.toUnixPath stored |> UnixPath.toString |> shouldEqual raw

            SymlinkTarget.toUnixPath stored
            |> UnixPath.components
            |> List.length
            |> shouldEqual 2
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
        // particular the one about to be allocated: restricting the generator
        // to existing directories would hide a builder that returns Ok for a
        // self-parented, unreachable directory, since that failure needs a
        // parent equal to `nextInode`. A rejected step leaves the filesystem
        // alone, so widening the alphabet costs nothing.
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

    // ------------------------------------------- symlink splice length limits

    /// An absolute path of exactly `bytes` bytes that names nothing, built from
    /// 200-byte components so that `NAME_MAX` can never be what refuses it — on
    /// either flavour, since 200 is under 255 counted either way.
    let private danglingTarget (bytes : int) : string =
        let component_ = "/" + String.replicate 200 "z"
        let repeated = String.replicate (bytes / component_.Length + 1) component_
        repeated.Substring (0, bytes)

    /// Resolve "/L<suffix>" where L is a symlink whose dangling target is
    /// `targetBytes` bytes, under the given platform's limits.
    let private throughLink
        (platform : SimulatedUnixPlatform)
        (targetBytes : int)
        (suffix : string)
        : Result<ResolvedTarget, UnixError>
        =
        let vfs = build [ mklink (rootOf emptyFs) "L" (danglingTarget targetBytes) ]

        VirtualFileSystem.resolve
            (SimulatedUnixPlatform.pathLimits platform)
            (rootOf vfs)
            SymlinkPolicy.Follow
            (path ("/L" + suffix))
            vfs

    [<Test>]
    let ``the spliced path must still fit in PATH_MAX, on Darwin only`` () : unit =
        // Bisected on Darwin 25.6.0 (macOS 26.6): resolving "L<suffix>" through
        // a dangling target of T bytes, the largest T that still resolves.
        // Below it the dangling target gives ENOENT, above it ENAMETOOLONG, so
        // the boundary is directly observable — and both endpoints are asserted,
        // because a model that refused everything would pass a one-sided check.
        //
        // The rows are not redundant. "//a" costs what "/a" costs because the
        // kernel collapses the separator run adjacent to the component it just
        // consumed, while "/a//b" costs one byte more than "/a/b" because an
        // interior run is untouched; a model that rendered the remainder
        // canonically would agree with the first and not the second.
        let measured =
            [
                "/a", 1021
                "/a/", 1020
                "//a", 1021
                "///a", 1021
                "/a/b", 1019
                "/a//b", 1018
                "/a///b", 1017
                "/./a", 1019
                "/..", 1020
                "/a/../b", 1016
            ]

        for suffix, largestResolving in measured do
            throughLink SimulatedUnixPlatform.macOsArm64 largestResolving suffix
            |> shouldEqual (Error UnixError.ENOENT)

            throughLink SimulatedUnixPlatform.macOsArm64 (largestResolving + 1) suffix
            |> shouldEqual (Error UnixError.ENAMETOOLONG)

            // Linux performs no such check at any threshold: measured, a
            // 3842-byte target with an 806-byte remainder resolves at 4648
            // spliced, past its own PATH_MAX. So the same splice that Darwin
            // refuses merely fails to find the dangling target.
            throughLink SimulatedUnixPlatform.linuxX64 (largestResolving + 1) suffix
            |> shouldEqual (Error UnixError.ENOENT)

    [<Test>]
    let ``a splice with nothing left to resolve is bounded only by the target itself`` () : unit =
        // With no remainder the kernel has just the target and the NUL, so the
        // budget is PATH_MAX - 1 = 1023. Measured: a 1023-byte target resolves.
        throughLink SimulatedUnixPlatform.macOsArm64 1023 ""
        |> shouldEqual (Error UnixError.ENOENT)

        // 1024 is *not* measurable on a live Darwin: `symlink(2)` refuses to
        // create a target that long, so no real filesystem can hold one. A
        // PawPrint seed can, and this expectation is therefore extrapolated
        // from the formula rather than bisected out of a kernel — which is
        // worth saying plainly, because every other row here was measured.
        throughLink SimulatedUnixPlatform.macOsArm64 1024 ""
        |> shouldEqual (Error UnixError.ENAMETOOLONG)

        throughLink SimulatedUnixPlatform.linuxX64 1024 ""
        |> shouldEqual (Error UnixError.ENOENT)

    [<Test>]
    let ``a trailing separator run adjacent to the link costs nothing`` () : unit =
        // Measured: suffixes "", "/", "//" and "///" all behave identically,
        // because the kernel consumes a run with nothing after it entirely.
        // This is the pair of rows that kills a model counting the trailing
        // separator unconditionally, which would refuse a 1023-byte target.
        for suffix in [ "" ; "/" ; "//" ; "///" ] do
            throughLink SimulatedUnixPlatform.macOsArm64 1023 suffix
            |> shouldEqual (Error UnixError.ENOENT)

    [<Test>]
    let ``ELOOP is reported before ENAMETOOLONG when one splice would trip both`` () : unit =
        // Measured on Darwin: a chain whose last link both exhausts the
        // traversal budget and would overflow the length reports ELOOP, while
        // the same chain one link shorter reports ENAMETOOLONG. XNU tests
        // `ni_loopcnt` in `namei` before it ever reads the target.
        let chain (length : int) (lastTarget : string) : VirtualFileSystem =
            build
                [
                    for i in 1..length do
                        let next = if i = length then lastTarget else $"/c%d{i + 1}"
                        yield mklink (rootOf emptyFs) $"c%d{i}" next
                ]

        let resolve (length : int) (lastTarget : string) : Result<ResolvedTarget, UnixError> =
            let vfs = chain length lastTarget

            VirtualFileSystem.resolve
                (SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.macOsArm64)
                (rootOf vfs)
                SymlinkPolicy.Follow
                (path "/c1/a")
                vfs

        // Darwin's MAXSYMLINKS is 32. At 32 traversals the budget is intact and
        // the last link's over-long target is what decides.
        resolve 32 (danglingTarget 1022) |> shouldEqual (Error UnixError.ENAMETOOLONG)

        // At 33 the count is exhausted first, and the same over-long target is
        // never examined.
        resolve 33 (danglingTarget 1022) |> shouldEqual (Error UnixError.ELOOP)

        // Control: without the over-long target, 32 resolves and 33 is ELOOP,
        // so the pair above is really about precedence and not about the chain
        // length alone.
        resolve 32 (danglingTarget 100) |> shouldEqual (Error UnixError.ENOENT)
        resolve 33 (danglingTarget 100) |> shouldEqual (Error UnixError.ELOOP)

    [<Test>]
    let ``the limit applies to every splice, not just the first`` () : unit =
        // The buffer is replaced at each expansion rather than appended to, so
        // lengths do not accumulate — measured, a chain of ten links each with
        // a 500-byte absolute target resolves. What must still hold is that
        // *each* splice is checked, so a short first link cannot smuggle a long
        // second one past the rule.
        let vfs =
            build
                [
                    mklink (rootOf emptyFs) "a" "/b"
                    mklink (rootOf emptyFs) "b" (danglingTarget 1022)
                ]

        let resolve (platform : SimulatedUnixPlatform) : Result<ResolvedTarget, UnixError> =
            VirtualFileSystem.resolve
                (SimulatedUnixPlatform.pathLimits platform)
                (rootOf vfs)
                SymlinkPolicy.Follow
                (path "/a/a")
                vfs

        resolve SimulatedUnixPlatform.macOsArm64
        |> shouldEqual (Error UnixError.ENAMETOOLONG)

        resolve SimulatedUnixPlatform.linuxX64 |> shouldEqual (Error UnixError.ENOENT)

    [<Test>]
    let ``the splice budget counts UTF-8 bytes, not UTF-16 code units`` () : unit =
        // Every row above is ASCII and would pass either way. U+4E2D is three
        // UTF-8 bytes and one UTF-16 code unit, so a target spelled in it is
        // three times longer than `String.Length` reports — and the limit next
        // door, NAME_MAX, genuinely *is* code units on Darwin, which is exactly
        // what makes the wrong function look right here.
        //
        // 340 CJK characters in components of 80: 340 * 3 + 5 separators = 1025
        // raw bytes, but only 345 UTF-16 units. With no remainder the budget is
        // 1023, so bytes refuse this and code units would permit it four times
        // over.
        let cjk (chars : int) : string =
            let full = chars / 80
            let rest = chars % 80

            [
                for _ in 1..full -> "/" + String.replicate 80 "中"
                if rest > 0 then
                    yield "/" + String.replicate rest "中"
            ]
            |> String.concat ""

        let target = cjk 340
        target.Length |> shouldEqual 345
        Text.Encoding.UTF8.GetByteCount target |> shouldEqual 1025

        let vfs = build [ mklink (rootOf emptyFs) "L" target ]

        let resolve (platform : SimulatedUnixPlatform) : Result<ResolvedTarget, UnixError> =
            VirtualFileSystem.resolve
                (SimulatedUnixPlatform.pathLimits platform)
                (rootOf vfs)
                SymlinkPolicy.Follow
                (path "/L")
                vfs

        resolve SimulatedUnixPlatform.macOsArm64
        |> shouldEqual (Error UnixError.ENAMETOOLONG)

        resolve SimulatedUnixPlatform.linuxX64 |> shouldEqual (Error UnixError.ENOENT)

    [<Test>]
    let ``Linux resolves a splice well past its own PATH_MAX`` () : unit =
        // The Linux column needs a probe at *Linux's* scale. Every other case
        // here uses a target near Darwin's 1024, which a wrongly-re-checking
        // Linux would still resolve — mutation-tested: flipping Linux to
        // `Recheck` survives every other test in the suite.
        //
        // These are the measured numbers: on Linux 6.18.5, a symlink whose
        // target is 3842 bytes, resolved with an 806-byte remainder, resolves
        // at 4648 bytes spliced — past its own PATH_MAX of 4096. Darwin refuses
        // the same shape, and would refuse it even at its own smaller scale.
        let target = danglingTarget 3842

        let remainder =
            let component_ = String.replicate 200 "r" + "/"
            let repeated = String.replicate (806 / component_.Length + 1) component_
            repeated.Substring (0, 806)

        // The argument itself is comfortably within both platforms' PATH_MAX,
        // so only the *spliced* length can be what refuses it.
        let argument = "/L/" + remainder
        argument.Length |> shouldBeSmallerThan 1024

        let vfs = build [ mklink (rootOf emptyFs) "L" target ]

        let resolve (platform : SimulatedUnixPlatform) : Result<ResolvedTarget, UnixError> =
            VirtualFileSystem.resolve
                (SimulatedUnixPlatform.pathLimits platform)
                (rootOf vfs)
                SymlinkPolicy.Follow
                (path argument)
                vfs

        resolve SimulatedUnixPlatform.linuxX64 |> shouldEqual (Error UnixError.ENOENT)

        resolve SimulatedUnixPlatform.macOsArm64
        |> shouldEqual (Error UnixError.ENAMETOOLONG)

/// `readTransferCount` decides the whole of what `pread(2)` returns once its
/// error cases are out of the way, and getting it wrong is an off-by-one that
/// end-to-end tests report as "the file came back slightly wrong" from inside a
/// `StreamReader`. As a function of three integers it can be checked against
/// naive slicing instead, which is what this does.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestReadTransferCount =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 2000

    /// Offsets and lengths drawn from a small range, so that boundary cases —
    /// offset exactly at the end, count exactly reaching it, a zero-length file
    /// — come up constantly rather than once in a blue moon. `Gen.choose` and
    /// not the default `int` generator, which is size-bounded in a way that
    /// makes "offset just past the end" rare.
    let private smallCase : Gen<int64 * int * int> =
        gen {
            let! length = Gen.choose (0, 12)
            let! offset = Gen.choose (0, 14)
            let! count = Gen.choose (0, 14)
            return int64 offset, count, length
        }

    /// The oracle: how many bytes you get by actually taking the slice.
    let private naive (offset : int64) (count : int) (length : int) : int =
        if offset >= int64 length then
            0
        else
            let available = length - int offset
            min count available

    [<Test>]
    let ``agrees with naive slicing`` () : unit =
        let property (offset : int64, count : int, length : int) : bool =
            VirtualFileSystem.readTransferCount offset count length = naive offset count length

        Check.One (config, Prop.forAll (Arb.fromGen smallCase) property)

    /// The properties that make the result usable as a slice bound, stated
    /// directly rather than inferred from the oracle: a caller indexes
    /// `contents.[offset .. offset + result - 1]`, so all three of these must
    /// hold or that indexing throws.
    [<Test>]
    let ``the result is a valid slice of the file`` () : unit =
        let property (offset : int64, count : int, length : int) : bool =
            let result = VirtualFileSystem.readTransferCount offset count length

            result >= 0
            && result <= count
            && (result = 0 || offset + int64 result <= int64 length)

        Check.One (config, Prop.forAll (Arb.fromGen smallCase) property)

    /// A read is short only because the file ended, never for any other reason.
    /// This is what lets `File.ReadAllBytes` issue a single `pread` and trust
    /// the count, and it is the property a clamp that also bounded by, say, a
    /// buffer size would break.
    ///
    /// "Reached *or passed* the end", not "reached" — an offset beyond the end
    /// answers 0 without `offset + 0` landing on the length, so stating this as
    /// an equality fails on e.g. offset = 14, count = 5, length = 10.
    [<Test>]
    let ``a short read means the file ended`` () : unit =
        let property (offset : int64, count : int, length : int) : bool =
            let result = VirtualFileSystem.readTransferCount offset count length
            result = count || offset + int64 result >= int64 length

        Check.One (config, Prop.forAll (Arb.fromGen smallCase) property)

    /// Large offsets, which the small generator never reaches: an offset beyond
    /// `int` range must answer 0 rather than overflowing into a negative count
    /// or a wrapped index. `RandomAccess` will happily pass one.
    [<Test>]
    let ``an offset beyond int range reads nothing`` () : unit =
        for offset in [ int64 Int32.MaxValue ; int64 Int32.MaxValue + 1L ; Int64.MaxValue ] do
            VirtualFileSystem.readTransferCount offset 16 10 |> shouldEqual 0

    [<Test>]
    let ``worked examples`` () : unit =
        // The measured `pread` rows, as unit assertions: a 5-byte file.
        VirtualFileSystem.readTransferCount 0L 5 5 |> shouldEqual 5
        VirtualFileSystem.readTransferCount 0L 64 5 |> shouldEqual 5
        VirtualFileSystem.readTransferCount 3L 64 5 |> shouldEqual 2
        VirtualFileSystem.readTransferCount 5L 64 5 |> shouldEqual 0
        VirtualFileSystem.readTransferCount 99L 64 5 |> shouldEqual 0
        VirtualFileSystem.readTransferCount 0L 0 5 |> shouldEqual 0
        // ...and an empty file, where every offset is at the end.
        VirtualFileSystem.readTransferCount 0L 16 0 |> shouldEqual 0

/// `VirtualFileSystem.seekTarget`: the whole of what `lseek(2)` computes, once the descriptor and
/// the whence have been resolved. Property-tested here because as a function of four integers it
/// can be, where the same arithmetic inlined in a handler is reachable only through a guest — and
/// the two faults it distinguishes are *indistinguishable* through a Linux-flavoured guest, both
/// being EINVAL.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSeekTarget =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 2000

    let private whences : SeekWhence list =
        [ SeekWhence.Set ; SeekWhence.Current ; SeekWhence.End ]

    /// Drawn from a small range so that the boundaries — landing exactly on zero, exactly on the
    /// end, one before and one after — come up constantly. `Gen.choose` rather than the default
    /// `int` generator, which is size-bounded in a way that makes them rare.
    let private smallCase : Gen<SeekWhence * int64 * int64 * int64> =
        gen {
            let! whence = Gen.elements whences
            let! current = Gen.choose (0, 12)
            let! size = Gen.choose (0, 12)
            let! offset = Gen.choose (-14, 14)
            return whence, int64 current, int64 size, int64 offset
        }

    /// The same, but with the magnitudes that make overflow reachable. Without this the `Overflow`
    /// case never occurs and the whole distinction it exists for goes untested.
    let private hugeCase : Gen<SeekWhence * int64 * int64 * int64> =
        gen {
            let! whence = Gen.elements whences
            let! current = Gen.elements [ 0L ; 1L ; Int64.MaxValue - 1L ; Int64.MaxValue ]
            let! size = Gen.elements [ 0L ; 1L ; 5L ; Int64.MaxValue - 1L ; Int64.MaxValue ]

            let! offset =
                Gen.elements
                    [
                        Int64.MinValue
                        Int64.MinValue + 1L
                        -1L
                        0L
                        1L
                        Int64.MaxValue - 5L
                        Int64.MaxValue - 1L
                        Int64.MaxValue
                    ]

            return whence, current, size, offset
        }

    /// The oracle, in arbitrary precision, so it cannot share a bug with the implementation's
    /// overflow check: `bigint` addition simply cannot wrap.
    let private naive
        (whence : SeekWhence)
        (current : int64)
        (size : int64)
        (offset : int64)
        : Result<int64, SeekFault>
        =
        let basis =
            match whence with
            | SeekWhence.Set -> 0I
            | SeekWhence.Current -> bigint current
            | SeekWhence.End -> bigint size

        let target = basis + bigint offset

        if target > bigint Int64.MaxValue then
            Error SeekFault.Overflow
        elif target < 0I then
            Error SeekFault.Negative
        else
            Ok (int64 target)

    let private check (gen : Gen<SeekWhence * int64 * int64 * int64>) : unit =
        let property (whence : SeekWhence, current : int64, size : int64, offset : int64) : bool =
            VirtualFileSystem.seekTarget whence current (lazy size) offset = naive whence current size offset

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``agrees with arbitrary-precision arithmetic on small inputs`` () : unit = check smallCase

    /// Overflow is what this generator is for, so assert it actually occurs: a generator that never
    /// produced one would make the property above true of an implementation with no overflow check
    /// at all.
    [<Test>]
    let ``agrees with arbitrary-precision arithmetic on huge inputs`` () : unit =
        check hugeCase

        let overflows =
            [
                for whence in whences do
                    for current in [ 0L ; Int64.MaxValue ] do
                        for size in [ 0L ; 5L ; Int64.MaxValue ] do
                            for offset in [ 1L ; Int64.MaxValue ] do
                                match VirtualFileSystem.seekTarget whence current (lazy size) offset with
                                | Error SeekFault.Overflow -> yield (whence, current, size, offset)
                                | _ -> ()
            ]

        overflows |> shouldNotEqual []

    /// The result is always a position a kernel would accept: never negative, so a description's
    /// offset stays sound however wild the seek. This is what `FileDescriptorRegistry.setOffset`
    /// relies on rather than re-checking.
    [<Test>]
    let ``a successful seek lands on a non-negative position`` () : unit =
        let property (whence : SeekWhence, current : int64, size : int64, offset : int64) : bool =
            match VirtualFileSystem.seekTarget whence current (lazy size) offset with
            | Ok target -> target >= 0L
            | Error _ -> true

        Check.One (config, Prop.forAll (Arb.fromGen smallCase) property)
        Check.One (config, Prop.forAll (Arb.fromGen hugeCase) property)

    /// `SEEK_CUR` with a zero offset reports where the description already is, without moving it.
    /// Stated separately because it is not merely a special case: it is the call the BCL makes —
    /// `SafeFileHandle.GetCanSeek` and `OSFileStreamStrategy`'s constructor both issue exactly this
    /// — and it is the joint property that lets a guest read back what a `read` advanced.
    [<Test>]
    let ``SEEK_CUR by zero is the identity on the current position`` () : unit =
        let property (_, current : int64, size : int64, _) : bool =
            VirtualFileSystem.seekTarget SeekWhence.Current current (lazy size) 0L = Ok current

        Check.One (config, Prop.forAll (Arb.fromGen smallCase) property)
        Check.One (config, Prop.forAll (Arb.fromGen hugeCase) property)

    /// `SEEK_SET` ignores where the description is, which is the whole of what distinguishes it
    /// from `SEEK_CUR`. A model that measured everything from the current offset would satisfy the
    /// non-negativity property above.
    [<Test>]
    let ``SEEK_SET ignores the current position and the size`` () : unit =
        let property (_, current : int64, size : int64, offset : int64) : bool =
            let expected = if offset < 0L then Error SeekFault.Negative else Ok offset

            VirtualFileSystem.seekTarget SeekWhence.Set current (lazy size) offset = expected

        Check.One (config, Prop.forAll (Arb.fromGen smallCase) property)
        Check.One (config, Prop.forAll (Arb.fromGen hugeCase) property)

    /// The size is consulted *only* by `SEEK_END`. The
    /// `SystemNative_LSeek` handler passes a thunk that refuses for a directory, whose size is a
    /// filesystem artefact PawPrint will not invent, so `SEEK_SET` and `SEEK_CUR` on a directory
    /// work precisely because this holds.
    [<Test>]
    let ``only SEEK_END forces the size`` () : unit =
        let exploding : Lazy<int64> =
            lazy failwith "seekTarget forced the size for a whence that should not need it"

        VirtualFileSystem.seekTarget SeekWhence.Set 3L exploding 7L
        |> shouldEqual (Ok 7L)

        VirtualFileSystem.seekTarget SeekWhence.Current 3L exploding 7L
        |> shouldEqual (Ok 10L)

        VirtualFileSystem.seekTarget SeekWhence.Current 3L exploding -9L
        |> shouldEqual (Error SeekFault.Negative)

        // ...and `SEEK_END` does force it, so the test above is not passing because nothing ever
        // reads the size.
        Assert.Throws<exn> (fun () ->
            VirtualFileSystem.seekTarget SeekWhence.End 3L exploding 0L
            |> ignore<Result<int64, SeekFault>>
        )
        |> ignore<exn>

    [<Test>]
    let ``worked examples`` () : unit =
        // The measured `lseek` rows, as unit assertions: a 5-byte file, currently at offset 3.
        VirtualFileSystem.seekTarget SeekWhence.Set 3L (lazy 5L) 0L
        |> shouldEqual (Ok 0L)

        VirtualFileSystem.seekTarget SeekWhence.Current 3L (lazy 5L) 1L
        |> shouldEqual (Ok 4L)

        VirtualFileSystem.seekTarget SeekWhence.End 3L (lazy 5L) 0L
        |> shouldEqual (Ok 5L)
        // Past the end is legal — it is how sparse files are made.
        VirtualFileSystem.seekTarget SeekWhence.End 3L (lazy 5L) 100L
        |> shouldEqual (Ok 105L)

        VirtualFileSystem.seekTarget SeekWhence.Set 3L (lazy 5L) 1000L
        |> shouldEqual (Ok 1000L)
        // Landing exactly on zero is fine; below it is not.
        VirtualFileSystem.seekTarget SeekWhence.Current 3L (lazy 5L) -3L
        |> shouldEqual (Ok 0L)

        VirtualFileSystem.seekTarget SeekWhence.Current 3L (lazy 5L) -4L
        |> shouldEqual (Error SeekFault.Negative)

        VirtualFileSystem.seekTarget SeekWhence.Set 3L (lazy 5L) -1L
        |> shouldEqual (Error SeekFault.Negative)

        VirtualFileSystem.seekTarget SeekWhence.End 3L (lazy 5L) -6L
        |> shouldEqual (Error SeekFault.Negative)
        // The overflow boundary: INT64_MAX exactly is a position, one past it is not.
        VirtualFileSystem.seekTarget SeekWhence.End 3L (lazy 5L) (Int64.MaxValue - 5L)
        |> shouldEqual (Ok Int64.MaxValue)

        VirtualFileSystem.seekTarget SeekWhence.End 3L (lazy 5L) (Int64.MaxValue - 4L)
        |> shouldEqual (Error SeekFault.Overflow)

        VirtualFileSystem.seekTarget SeekWhence.Set 3L (lazy 5L) Int64.MaxValue
        |> shouldEqual (Ok Int64.MaxValue)

        VirtualFileSystem.seekTarget SeekWhence.Current Int64.MaxValue (lazy 5L) 1L
        |> shouldEqual (Error SeekFault.Overflow)
