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

    /// For the timestamp-ordering property alone, whose coverage guards count occurrences across
    /// the whole check; see the comment at its `Check.One`.
    let private timesConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 2000

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
        VirtualFileSystem.checkInvariants Set.empty emptyFs |> shouldEqual []

        VirtualFileSystem.inodes emptyFs |> Map.count |> shouldEqual 1

        // The root's parent is itself, so "/.." is "/".
        VirtualFileSystem.resolve
            limits
            CallerPrivilege.Privileged
            (rootOf emptyFs)
            SymlinkPolicy.Follow
            (path "/..")
            emptyFs
        |> shouldEqual (Ok (ResolvedTarget.Directory (rootOf emptyFs, FinalNavigation.Parent)))

    [<Test>]
    let ``the empty path is ENOENT, not the directory we started from`` () : unit =
        // The trap this guards: a walk over zero components would silently mean
        // "the start directory", which is a successful answer to a call every
        // Unix rejects.
        VirtualFileSystem.resolve
            limits
            CallerPrivilege.Privileged
            (rootOf emptyFs)
            SymlinkPolicy.Follow
            UnixPath.empty
            emptyFs
        |> shouldEqual (Error UnixError.ENOENT)

    [<Test>]
    let ``a relative path starting from a non-directory is ENOTDIR`` () : unit =
        let vfs = build [ mkfile (rootOf emptyFs) "f" ]

        let file =
            VirtualFileSystem.resolveExisting
                limits
                CallerPrivilege.Privileged
                (rootOf vfs)
                SymlinkPolicy.Follow
                (path "/f")
                vfs
            |> ok

        VirtualFileSystem.resolve limits CallerPrivilege.Privileged file SymlinkPolicy.Follow (path "a") vfs
        |> shouldEqual (Error UnixError.ENOTDIR)

    [<Test>]
    let ``a path cannot continue through a regular file`` () : unit =
        let vfs = build [ mkfile (rootOf emptyFs) "f" ]

        VirtualFileSystem.resolve limits CallerPrivilege.Privileged (rootOf vfs) SymlinkPolicy.Follow (path "/f/x") vfs
        |> shouldEqual (Error UnixError.ENOTDIR)

    [<Test>]
    let ``a free name in the final position is not an error`` () : unit =
        // The whole point of stopping short of the final lookup: mkdir and
        // open(O_CREAT) need this state, and only stat turns it into ENOENT.
        let vfs = emptyFs

        VirtualFileSystem.resolve limits CallerPrivilege.Privileged (rootOf vfs) SymlinkPolicy.Follow (path "/nx") vfs
        |> shouldEqual (Ok (ResolvedTarget.Entry (rootOf vfs, name "nx", None)))

        VirtualFileSystem.resolveExisting
            limits
            CallerPrivilege.Privileged
            (rootOf vfs)
            SymlinkPolicy.Follow
            (path "/nx")
            vfs
        |> shouldEqual (Error UnixError.ENOENT)

        // ...but a free name part-way along is ENOENT even so.
        VirtualFileSystem.resolve limits CallerPrivilege.Privileged (rootOf vfs) SymlinkPolicy.Follow (path "/nx/y") vfs
        |> shouldEqual (Error UnixError.ENOENT)

    // --------------------------------------------------- the trailing separator

    [<Test>]
    let ``a trailing separator is not desugared into a dot component`` () : unit =
        // Probed on macOS: mkdir("d/") succeeds while mkdir("nx/.") is ENOENT,
        // and rmdir("d/") succeeds while rmdir("d/.") is EINVAL. Desugaring
        // would collapse the Entry that mkdir("nx/") needs into a Directory,
        // and would make a free name report ENOENT.
        let vfs = emptyFs

        let resolution : Resolution =
            VirtualFileSystem.resolveFull
                limits
                CallerPrivilege.Privileged
                (rootOf vfs)
                SymlinkPolicy.Follow
                TrailingSeparatorPolicy.Demand
                (path "/nx/")
                vfs
            |> ok

        resolution.Target
        |> shouldEqual (ResolvedTarget.Entry (rootOf vfs, name "nx", None))

        resolution.TrailingSeparatorDemanded |> shouldEqual true

        // Whereas the genuinely-dotted path has no final name at all, which is
        // what makes rmdir able to tell the two apart and report EINVAL.
        let withDot =
            VirtualFileSystem.resolveFull
                limits
                CallerPrivilege.Privileged
                (rootOf vfs)
                SymlinkPolicy.Follow
                TrailingSeparatorPolicy.Demand
                (path "/nx/.")
                vfs

        withDot |> shouldEqual (Error UnixError.ENOENT)

    [<Test>]
    let ``a trailing separator on an existing non-directory is ENOTDIR`` () : unit =
        // The part of the trailing-separator rule every platform agrees on.
        let vfs = build [ mkfile (rootOf emptyFs) "f" ]

        VirtualFileSystem.resolve limits CallerPrivilege.Privileged (rootOf vfs) SymlinkPolicy.Follow (path "/f/") vfs
        |> shouldEqual (Error UnixError.ENOTDIR)

        // Without the separator the same path is perfectly fine.
        VirtualFileSystem.resolve limits CallerPrivilege.Privileged (rootOf vfs) SymlinkPolicy.Follow (path "/f") vfs
        |> shouldEqual (
            Ok (
                ResolvedTarget.Entry (
                    rootOf vfs,
                    name "f",
                    Some (
                        VirtualFileSystem.resolveExisting
                            limits
                            CallerPrivilege.Privileged
                            (rootOf vfs)
                            SymlinkPolicy.Follow
                            (path "/f")
                            vfs
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
            VirtualFileSystem.resolveExisting
                limits
                CallerPrivilege.Privileged
                (rootOf vfs)
                SymlinkPolicy.Follow
                (path "/d")
                vfs
            |> ok

        let withSlash =
            VirtualFileSystem.resolveFull
                limits
                CallerPrivilege.Privileged
                (rootOf vfs)
                SymlinkPolicy.NoFollowFinal
                TrailingSeparatorPolicy.Demand
                (path "/ld/")
                vfs
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
            VirtualFileSystem.resolveExisting
                limits
                CallerPrivilege.Privileged
                (rootOf vfs)
                SymlinkPolicy.NoFollowFinal
                (path "/ld")
                vfs
            |> ok

        match VirtualFileSystem.tryGetContent link vfs with
        | Some (InodeContent.Symlink _) -> ()
        | other -> failwith $"expected the symlink itself, got %A{other}"

    [<Test>]
    let ``a symlink target's own trailing separator takes effect only when final`` () : unit =
        let vfs = build [ mkfile (rootOf emptyFs) "f" ; mklink (rootOf emptyFs) "lf" "f/" ]

        // "lf" expands to "f/", whose trailing separator now demands that f be
        // a directory. It is not.
        VirtualFileSystem.resolve limits CallerPrivilege.Privileged (rootOf vfs) SymlinkPolicy.Follow (path "/lf") vfs
        |> shouldEqual (Error UnixError.ENOTDIR)

    // ------------------------------------------------------------- symlinks

    [<Test>]
    let ``a dangling final symlink under Follow is a free name, not an error`` () : unit =
        // open("/link", O_CREAT) where link -> /nx must create nx, so the walk
        // has to hand back the *target's* parent and name.
        let vfs = build [ mklink (rootOf emptyFs) "dang" "nx" ]

        VirtualFileSystem.resolve limits CallerPrivilege.Privileged (rootOf vfs) SymlinkPolicy.Follow (path "/dang") vfs
        |> shouldEqual (Ok (ResolvedTarget.Entry (rootOf vfs, name "nx", None)))

        // But a dangling link whose target's *parent* is missing is ENOENT,
        // because that failure happens part-way along.
        let vfs = build [ mklink (rootOf emptyFs) "deep" "nx1/nx2" ]

        VirtualFileSystem.resolve limits CallerPrivilege.Privileged (rootOf vfs) SymlinkPolicy.Follow (path "/deep") vfs
        |> shouldEqual (Error UnixError.ENOENT)

    [<Test>]
    let ``a rooted symlink target restarts at the root`` () : unit =
        let vfs =
            build
                [
                    mkdir (rootOf emptyFs) "a"
                    fun vfs ->
                        let a =
                            VirtualFileSystem.resolveExisting
                                limits
                                CallerPrivilege.Privileged
                                (rootOf vfs)
                                SymlinkPolicy.Follow
                                (path "/a")
                                vfs
                            |> ok

                        vfs |> mkfile a "f" |> mklink a "up" "/f2"
                    mkfile (rootOf emptyFs) "f2"
                ]

        let f2 =
            VirtualFileSystem.resolveExisting
                limits
                CallerPrivilege.Privileged
                (rootOf vfs)
                SymlinkPolicy.Follow
                (path "/f2")
                vfs
            |> ok

        VirtualFileSystem.resolveExisting
            limits
            CallerPrivilege.Privileged
            (rootOf vfs)
            SymlinkPolicy.Follow
            (path "/a/up")
            vfs
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
                VirtualFileSystem.resolveExisting
                    forged
                    CallerPrivilege.Privileged
                    (rootOf vfs)
                    SymlinkPolicy.Follow
                    (path "/l")
                    vfs
                |> ignore<Result<InodeNumber, UnixError>>
            )

        exn.Message |> shouldContainText "no Unix does"

        // ...and it is refused even where no symlink is involved, so that the
        // guard cannot be satisfied by a check that only runs at a traversal.
        Assert.Throws<Exception> (fun () ->
            VirtualFileSystem.resolveExisting
                forged
                CallerPrivilege.Privileged
                (rootOf vfs)
                SymlinkPolicy.Follow
                (path "/f")
                vfs
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

    // ------------------------------------------- TrailingSeparatorPolicy

    /// The filesystem the trailing-separator rows below resolve against, holding
    /// one of every shape a final component can have.
    let private separatorFs : VirtualFileSystem =
        let r = rootOf emptyFs

        build
            [
                mkdir r "d"
                mkfile r "f"
                mklink r "ld" "d"
                mklink r "dang" "nx"
                mklink r "cyc" "cyc"
                // A link whose *target* carries the separator, so the demand arrives
                // from a splice rather than from the guest's own path.
                mklink r "lslash" "d/"
                mklink r "cycslash" "cycslash/"
            ]

    let private refuse (candidate : string) : Result<Resolution, UnixError> =
        VirtualFileSystem.resolveFull
            limits
            CallerPrivilege.Privileged
            (rootOf separatorFs)
            SymlinkPolicy.Follow
            TrailingSeparatorPolicy.RefuseIsDirectory
            (path candidate)
            separatorFs

    let private demand (candidate : string) : Result<Resolution, UnixError> =
        VirtualFileSystem.resolveFull
            limits
            CallerPrivilege.Privileged
            (rootOf separatorFs)
            SymlinkPolicy.Follow
            TrailingSeparatorPolicy.Demand
            (path candidate)
            separatorFs

    [<Test>]
    let ``RefuseIsDirectory answers EISDIR for every final-component shape`` () : unit =
        // Measured on Linux with O_CREAT: the errno does not depend on what the
        // name turns out to be, because the refusal happens before the lookup.
        // Under `Demand` the same paths answer four *different* things, which is
        // what makes this table load-bearing rather than a restatement.
        for candidate in [ "/d/" ; "/f/" ; "/dang/" ; "/ld/" ; "/nx/" ] do
            refuse candidate |> shouldEqual (Error UnixError.EISDIR)

    [<Test>]
    let ``RefuseIsDirectory fires before the NAME_MAX check`` () : unit =
        // Measured on Linux: `<300 a>/` is EISDIR while `<300 a>` is
        // ENAMETOOLONG. So the refusal cannot be placed after
        // `PathLimits.nameWithinLimit`, and the second row is what proves the
        // limit is still enforced when there is no separator to refuse.
        let long = String.replicate 300 "a"

        refuse ("/" + long + "/") |> shouldEqual (Error UnixError.EISDIR)
        refuse ("/" + long) |> shouldEqual (Error UnixError.ENAMETOOLONG)

    [<Test>]
    let ``RefuseIsDirectory fires before a symlink is traversed`` () : unit =
        // Measured on Linux: `cyc/` with O_CREAT is EISDIR, *not* ELOOP, even
        // though `cyc` is a self-referential link that would otherwise exhaust
        // the traversal budget. This is the row that pins the check above the
        // symlink arm rather than merely above the lookup.
        refuse "/cyc/" |> shouldEqual (Error UnixError.EISDIR)
        demand "/cyc/" |> shouldEqual (Error UnixError.ELOOP)

    [<Test>]
    let ``RefuseIsDirectory sees a separator spliced in from a symlink target`` () : unit =
        // The guest's own path has no trailing separator here: it arrives when
        // the walk splices "d/" in. Measured on Linux, `l -> "d/"` opened with
        // O_CREAT is EISDIR, and `l -> "cyc2/"` is EISDIR rather than ELOOP —
        // so a check at the syscall boundary, on the path as passed, would be
        // wrong for both.
        refuse "/lslash" |> shouldEqual (Error UnixError.EISDIR)
        refuse "/cycslash" |> shouldEqual (Error UnixError.EISDIR)

        // Under `Demand` the same two resolve and loop respectively, so neither
        // row is an artefact of the link being broken.
        demand "/lslash"
        |> Result.map (fun (r : Resolution) -> r.TrailingSeparatorDemanded)
        |> shouldEqual (Ok true)

        demand "/cycslash" |> shouldEqual (Error UnixError.ELOOP)

    [<Test>]
    let ``RefuseIsDirectory does not pre-empt a failure on an earlier component`` () : unit =
        // Measured on Linux: `nodir/new/` is ENOENT and `f/new/` is ENOTDIR, not
        // EISDIR — the walk never reaches the final component in either. This is
        // what rules out checking the raw path text before resolving, and it is
        // the half a check placed too early would break.
        refuse "/nodir/new/" |> shouldEqual (Error UnixError.ENOENT)
        refuse "/f/new/" |> shouldEqual (Error UnixError.ENOTDIR)

    [<Test>]
    let ``RefuseIsDirectory leaves a path with no trailing separator alone`` () : unit =
        // The other half of the guard: the policy must key on the separator, not
        // on "this is a creating walk". Every one of these resolves exactly as it
        // does under `Demand`.
        for candidate in [ "/d" ; "/f" ; "/ld" ; "/nx" ] do
            refuse candidate |> shouldEqual (demand candidate)

    [<Test>]
    let ``RefuseIsDirectory does not touch a path that consumed no component`` () : unit =
        // "/" and "/." reach the `None` arm of the walk, which the policy never
        // sees. Darwin and Linux disagree about what a *creating* open then owes,
        // and that disagreement is settled by the caller from `FinalNavigation`,
        // not here.
        refuse "/" |> shouldEqual (demand "/")
        refuse "/." |> shouldEqual (demand "/.")
        refuse "/d/.." |> shouldEqual (demand "/d/..")

    // ------------------------------------------------------------- NAME_MAX

    let private darwinLimits : PathLimits =
        SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.macOsArm64

    let private linuxLimits : PathLimits =
        SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.linuxX64

    /// Resolve a bare name in the root of an otherwise empty filesystem, so the
    /// only thing that can be reported is the name's own length.
    let private resolveName (limits : PathLimits) (candidate : string) : Result<InodeNumber, UnixError> =
        VirtualFileSystem.resolveExisting
            limits
            CallerPrivilege.Privileged
            (rootOf emptyFs)
            SymlinkPolicy.Follow
            (path ("/" + candidate))
            emptyFs

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
            CallerPrivilege.Privileged
            (rootOf emptyFs)
            SymlinkPolicy.Follow
            (path ("/nxdir/" + tooLong))
            emptyFs
        |> shouldEqual (Error UnixError.ENOENT)

        // ...whereas with the long component *first*, it is reached and refused.
        VirtualFileSystem.resolveExisting
            linuxLimits
            CallerPrivilege.Privileged
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

        VirtualFileSystem.resolveExisting
            linuxLimits
            CallerPrivilege.Privileged
            (rootOf vfs)
            SymlinkPolicy.Follow
            (path "/l")
            vfs
        |> shouldEqual (Error UnixError.ENAMETOOLONG)

    [<Test>]
    let ``a symlink chain exactly at a platform's limit resolves`` () : unit =
        for platform in everyFlavour do
            let limits = SimulatedUnixPlatform.pathLimits platform
            let vfs = symlinkChain (PathLimits.maxSymlinkTraversals limits)

            VirtualFileSystem.resolveExisting
                limits
                CallerPrivilege.Privileged
                (rootOf vfs)
                SymlinkPolicy.Follow
                (path "/s1")
                vfs
            |> Result.isOk
            |> shouldEqual true

    [<Test>]
    let ``a symlink chain one past a platform's limit is ELOOP`` () : unit =
        for platform in everyFlavour do
            let limits = SimulatedUnixPlatform.pathLimits platform
            let vfs = symlinkChain (PathLimits.maxSymlinkTraversals limits + 1)

            VirtualFileSystem.resolveExisting
                limits
                CallerPrivilege.Privileged
                (rootOf vfs)
                SymlinkPolicy.Follow
                (path "/s1")
                vfs
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

        VirtualFileSystem.resolveExisting
            darwin
            CallerPrivilege.Privileged
            (rootOf vfs)
            SymlinkPolicy.Follow
            (path "/s1")
            vfs
        |> shouldEqual (Error UnixError.ELOOP)

        VirtualFileSystem.resolveExisting
            linux
            CallerPrivilege.Privileged
            (rootOf vfs)
            SymlinkPolicy.Follow
            (path "/s1")
            vfs
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

        VirtualFileSystem.resolveExisting
            darwin
            CallerPrivilege.Privileged
            (rootOf vfs)
            SymlinkPolicy.Follow
            (path "/s1")
            vfs
        |> shouldEqual (Error UnixError.ELOOP)

        VirtualFileSystem.resolveExisting
            linux
            CallerPrivilege.Privileged
            (rootOf vfs)
            SymlinkPolicy.Follow
            (path "/s1")
            vfs
        |> shouldEqual (Error UnixError.ENOENT)

    [<Test>]
    let ``a self-extending symlink terminates rather than growing forever`` () : unit =
        // The case that defeats cycle detection: "l" -> "l/x" never repeats a
        // (directory, remaining) state, it just grows the path. Only the
        // traversal count stops it, which is why there is no seen-state set.
        let vfs = build [ mklink (rootOf emptyFs) "l" "l/x" ]

        VirtualFileSystem.resolveExisting
            limits
            CallerPrivilege.Privileged
            (rootOf vfs)
            SymlinkPolicy.Follow
            (path "/l")
            vfs
        |> shouldEqual (Error UnixError.ELOOP)

    [<Test>]
    let ``a symlink cycle is ELOOP rather than a crash`` () : unit =
        // Both platforms report ELOOP for a cycle, so this must be answered
        // rather than referred back as a divergence — the count reaches the
        // no-platform-allows bound before it reaches the divergent band's top.
        let vfs =
            build [ mklink (rootOf emptyFs) "a" "b" ; mklink (rootOf emptyFs) "b" "a" ]

        VirtualFileSystem.resolveExisting
            limits
            CallerPrivilege.Privileged
            (rootOf vfs)
            SymlinkPolicy.Follow
            (path "/a")
            vfs
        |> shouldEqual (Error UnixError.ELOOP)

    [<Test>]
    let ``a symlink target is stored verbatim`` () : unit =
        // readlink(2) returns the stored bytes unchanged and lstat reports
        // their length as st_size, so a target that was created as "a//b/" must
        // read back as "a//b/" or FileInfo.LinkTarget disagrees with every Unix.
        let raw = "a//b/"
        let vfs = build [ mklink (rootOf emptyFs) "l" raw ]

        let link =
            VirtualFileSystem.resolveExisting
                limits
                CallerPrivilege.Privileged
                (rootOf vfs)
                SymlinkPolicy.NoFollowFinal
                (path "/l")
                vfs
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
            match
                VirtualFileSystem.resolve
                    limits
                    CallerPrivilege.Privileged
                    (rootOf vfs)
                    SymlinkPolicy.NoFollowFinal
                    (path candidate)
                    vfs
            with
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
            VirtualFileSystem.resolveExisting
                limits
                CallerPrivilege.Privileged
                root
                SymlinkPolicy.Follow
                (path "/f")
                vfs
            |> ok

        let directory =
            VirtualFileSystem.resolveExisting
                limits
                CallerPrivilege.Privileged
                root
                SymlinkPolicy.Follow
                (path "/d")
                vfs
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
        VirtualFileSystem.checkInvariants Set.empty linked |> shouldEqual []

        VirtualFileSystem.resolveExisting
            limits
            CallerPrivilege.Privileged
            root
            SymlinkPolicy.Follow
            (path "/f2")
            linked
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
            VirtualFileSystem.checkInvariants Set.empty vfs |> shouldEqual []

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
                            VirtualFileSystem.resolveExisting
                                limits
                                CallerPrivilege.Privileged
                                (rootOf vfs)
                                SymlinkPolicy.Follow
                                (path "/a")
                                vfs
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
                        CallerPrivilege.Privileged
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
        VirtualFileSystem.checkInvariants Set.empty vfs |> shouldEqual expected

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
            VirtualFileSystem.checkInvariants Set.empty vfs |> shouldEqual []

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
                            CallerPrivilege.Privileged
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
                VirtualFileSystem.resolveFull
                    limits
                    CallerPrivilege.Privileged
                    (rootOf vfs)
                    policy
                    TrailingSeparatorPolicy.Demand
                    (path candidate)
                    vfs
                |> ignore<Result<Resolution, UnixError>>

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.zip filesystemGen pathGen)) property)

    [<Test>]
    let ``resolveExisting agrees with resolve on what exists`` () : unit =
        let pathGen = Gen.elements [ "/" ; "/a" ; "/a/b" ; "/l" ; "/nx" ; "/a/nx" ; "/.." ]

        let property (vfs : VirtualFileSystem, candidate : string) : unit =
            let full =
                VirtualFileSystem.resolve
                    limits
                    CallerPrivilege.Privileged
                    (rootOf vfs)
                    SymlinkPolicy.Follow
                    (path candidate)
                    vfs

            let existing =
                VirtualFileSystem.resolveExisting
                    limits
                    CallerPrivilege.Privileged
                    (rootOf vfs)
                    SymlinkPolicy.Follow
                    (path candidate)
                    vfs

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
                VirtualFileSystem.resolveExisting
                    limits
                    CallerPrivilege.Privileged
                    (rootOf vfs)
                    SymlinkPolicy.NoFollowFinal
                    (path p)
                    vfs
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
            VirtualFileSystem.resolveExisting
                limits
                CallerPrivilege.Privileged
                (rootOf vfs)
                SymlinkPolicy.Follow
                (path "/f")
                vfs
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
            VirtualFileSystem.resolveExisting
                limits
                CallerPrivilege.Privileged
                (rootOf vfs)
                SymlinkPolicy.Follow
                (path "/f")
                vfs
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
    let ``writing moves a file's mtime and ctime, and nothing else's anything`` () : unit =
        let later = UnixTimestamp.createOrFail "test" 1_700_000_700L 42
        let vfs = build [ mkfile (rootOf emptyFs) "f" ]

        let file =
            VirtualFileSystem.resolveExisting
                limits
                CallerPrivilege.Privileged
                (rootOf vfs)
                SymlinkPolicy.Follow
                (path "/f")
                vfs
            |> ok

        let written =
            match
                VirtualFileSystem.writeFile
                    file
                    0L
                    (ImmutableArray.CreateRange [| 1uy ; 2uy |])
                    SetGroupIdOnWrite.StripWhenGroupExecutable
                    CallerPrivilege.Unprivileged
                    later
                    vfs
            with
            | Ok vfs -> vfs
            | Error refusal -> failwith $"expected success, got %O{refusal}"

        let after = timesOf file written

        // The contents changed, so mtime; and any change to the inode moves ctime
        // with it. Measured on both platforms.
        after.Modification |> shouldEqual later
        after.StatusChange |> shouldEqual later

        // Nothing *read* the file, and it was not reborn. atime staying put is
        // the measured behaviour rather than a simplification: a write does not
        // move it on either platform.
        after.Access |> shouldEqual buildTime
        after.Birth |> shouldEqual buildTime

        // The directory holding it is untouched: its own contents — the set of
        // names it binds — did not change.
        timesOf (rootOf written) written |> shouldEqual (timesOf (rootOf vfs) vfs)

        // ...and the bytes really did land, so this is not passing against a
        // no-op that happened to restamp the inode.
        match VirtualFileSystem.tryGetContent file written with
        | Some (InodeContent.RegularFile (contents, permissions)) ->
            Seq.toList contents |> shouldEqual [ 1uy ; 2uy ]
            // The mode is not collateral damage of rewriting the content.
            permissions |> shouldEqual filePerms
        | other -> failwith $"expected a regular file, got %O{other}"

    /// `writeFile` applies both the privilege and the flavour rule rather than
    /// merely having access to them: a version that threaded either in and then
    /// ignored it would pass every assertion in the fixture above.
    ///
    /// `0o2644` is what makes the *rule* load-bearing here. On `0o4755` the two
    /// flavours agree, so a `writeFile` that hardcoded either one would still
    /// answer every row correctly.
    [<Test>]
    let ``writeFile strips a written file's set-ID bits, per the flavour and the writer's privilege`` () : unit =
        let modeAfter (start : int) (rule : SetGroupIdOnWrite) (privilege : CallerPrivilege) : int =
            let vfs =
                VirtualFileSystem.createFile
                    (rootOf emptyFs)
                    (name "s")
                    (PermissionBits.parseOrFail "test" start)
                    buildTime
                    noBytes
                    emptyFs
                |> ok
                |> snd

            let file =
                VirtualFileSystem.resolveExisting
                    limits
                    CallerPrivilege.Privileged
                    (rootOf vfs)
                    SymlinkPolicy.Follow
                    (path "/s")
                    vfs
                |> ok

            let written =
                match
                    VirtualFileSystem.writeFile
                        file
                        0L
                        (ImmutableArray.CreateRange [| 7uy |])
                        rule
                        privilege
                        buildTime
                        vfs
                with
                | Ok vfs -> vfs
                | Error refusal -> failwith $"expected success, got %O{refusal}"

            match VirtualFileSystem.tryGetContent file written with
            | Some (InodeContent.RegularFile (_, permissions)) -> PermissionBits.toInt permissions
            | other -> failwith $"expected a regular file, got %O{other}"

        // Setuid: both flavours strip it, and root keeps it.
        for rule in [ SetGroupIdOnWrite.StripWhenGroupExecutable ; SetGroupIdOnWrite.StripAlways ] do
            modeAfter 0o4755 rule CallerPrivilege.Unprivileged |> shouldEqual 0o0755
            modeAfter 0o4755 rule CallerPrivilege.Privileged |> shouldEqual 0o4755

        // Setgid without group-execute: the flavours part company, and this is
        // what proves the rule reaches the stored mode rather than stopping at
        // `PermissionBits`.
        modeAfter 0o2644 SetGroupIdOnWrite.StripWhenGroupExecutable CallerPrivilege.Unprivileged
        |> shouldEqual 0o2644

        modeAfter 0o2644 SetGroupIdOnWrite.StripAlways CallerPrivilege.Unprivileged
        |> shouldEqual 0o0644

        modeAfter 0o2644 SetGroupIdOnWrite.StripAlways CallerPrivilege.Privileged
        |> shouldEqual 0o2644

    /// Where truncation parts company with `writeFile`: a write of no bytes is not
    /// a write and the caller short-circuits it, but a truncation to the length the
    /// file already has *is* a truncation and stamps the inode. Measured on both
    /// platforms.
    [<Test>]
    let ``truncating to the length a file already has still moves its mtime and ctime`` () : unit =
        let later = UnixTimestamp.createOrFail "test" 1_700_000_700L 42

        let vfs =
            let vfs = build [ mkfile (rootOf emptyFs) "f" ]

            let file =
                VirtualFileSystem.resolveExisting
                    limits
                    CallerPrivilege.Privileged
                    (rootOf vfs)
                    SymlinkPolicy.Follow
                    (path "/f")
                    vfs
                |> ok

            match
                VirtualFileSystem.writeFile
                    file
                    0L
                    (ImmutableArray.CreateRange [| 1uy ; 2uy ; 3uy |])
                    SetGroupIdOnWrite.StripWhenGroupExecutable
                    CallerPrivilege.Unprivileged
                    buildTime
                    vfs
            with
            | Ok vfs -> vfs
            | Error refusal -> failwith $"expected success, got %O{refusal}"

        let file =
            VirtualFileSystem.resolveExisting
                limits
                CallerPrivilege.Privileged
                (rootOf vfs)
                SymlinkPolicy.Follow
                (path "/f")
                vfs
            |> ok

        let truncated =
            match
                VirtualFileSystem.truncateFile
                    file
                    3L
                    SetIdBitsOnTruncation.Preserve
                    CallerPrivilege.Unprivileged
                    later
                    vfs
            with
            | Ok vfs -> vfs
            | Error refusal -> failwith $"expected success, got %O{refusal}"

        let after = timesOf file truncated

        after.Modification |> shouldEqual later
        after.StatusChange |> shouldEqual later

        // Neither of the two a truncation never moves.
        after.Access |> shouldEqual buildTime
        after.Birth |> shouldEqual buildTime

        // ...and the bytes are all still there, which is what makes this a no-op
        // truncation rather than a truncation that happened to leave three bytes.
        match VirtualFileSystem.tryGetContent file truncated with
        | Some (InodeContent.RegularFile (contents, _)) -> Seq.toList contents |> shouldEqual [ 1uy ; 2uy ; 3uy ]
        | other -> failwith $"expected a regular file, got %O{other}"

    /// `truncateFile` applies the rule rather than merely having it in scope: a
    /// version that took the rule and ignored it would pass every other assertion
    /// in this fixture.
    [<Test>]
    let ``truncateFile applies the set-ID rule it is given`` () : unit =
        let setuid = PermissionBits.parseOrFail "test" 0o4755

        let vfs =
            VirtualFileSystem.createFile (rootOf emptyFs) (name "s") setuid buildTime noBytes emptyFs
            |> ok
            |> snd

        let file =
            VirtualFileSystem.resolveExisting
                limits
                CallerPrivilege.Privileged
                (rootOf vfs)
                SymlinkPolicy.Follow
                (path "/s")
                vfs
            |> ok

        let modeAfter (rule : SetIdBitsOnTruncation) (privilege : CallerPrivilege) : int =
            let truncated =
                match VirtualFileSystem.truncateFile file 0L rule privilege buildTime vfs with
                | Ok vfs -> vfs
                | Error refusal -> failwith $"expected success, got %O{refusal}"

            match VirtualFileSystem.tryGetContent file truncated with
            | Some (InodeContent.RegularFile (_, permissions)) -> PermissionBits.toInt permissions
            | other -> failwith $"expected a regular file, got %O{other}"

        modeAfter SetIdBitsOnTruncation.Strip CallerPrivilege.Unprivileged
        |> shouldEqual 0o755

        modeAfter SetIdBitsOnTruncation.Preserve CallerPrivilege.Unprivileged
        |> shouldEqual 0o4755

        modeAfter SetIdBitsOnTruncation.Strip CallerPrivilege.Privileged
        |> shouldEqual 0o4755

        modeAfter SetIdBitsOnTruncation.Preserve CallerPrivilege.Privileged
        |> shouldEqual 0o4755

    /// The three shapes `truncateFile` refuses to answer for, each of which means
    /// a descriptor open for writing named something `open(2)` could not have
    /// given one for.
    [<Test>]
    let ``truncateFile fails loudly on anything but a regular file it contains`` () : unit =
        let vfs = build [ mkdir (rootOf emptyFs) "d" ; mklink (rootOf emptyFs) "l" "d" ]

        let directory =
            VirtualFileSystem.resolveExisting
                limits
                CallerPrivilege.Privileged
                (rootOf vfs)
                SymlinkPolicy.Follow
                (path "/d")
                vfs
            |> ok

        let link =
            VirtualFileSystem.resolveExisting
                limits
                CallerPrivilege.Privileged
                (rootOf vfs)
                SymlinkPolicy.NoFollowFinal
                (path "/l")
                vfs
            |> ok

        // Each arm is asserted by its message as well as by throwing, so a test
        // cannot pass because some *earlier* arm fired for a different reason.
        let shouldFailWith (substring : string) (inode : InodeNumber) : unit =
            let exn =
                Assert.Throws<exn> (fun () ->
                    VirtualFileSystem.truncateFile
                        inode
                        0L
                        SetIdBitsOnTruncation.Strip
                        CallerPrivilege.Unprivileged
                        buildTime
                        vfs
                    |> ignore<Result<VirtualFileSystem, FileTruncationRefusal>>
                )

            exn.Message |> shouldContainText substring

        shouldFailWith "is a directory" directory
        shouldFailWith "is a symbolic link" link
        shouldFailWith "is not in this filesystem" (InodeNumber 9999L)

    /// The measured table, as unit assertions, with every expectation written as
    /// an octal literal rather than computed — a version that asked
    /// `afterTruncation` for its own expectations would agree with any rule at
    /// all.
    ///
    /// Non-root on macOS 26.6 and Linux 6.18.5, and root on both.
    [<Test>]
    let ``truncation strips the set-ID bits only on Linux, and only for a non-root caller`` () : unit =
        let after (rule : SetIdBitsOnTruncation) (privilege : CallerPrivilege) (mode : int) : int =
            PermissionBits.parseOrFail "test" mode
            |> PermissionBits.afterTruncation rule privilege
            |> PermissionBits.toInt

        let linux = after SetIdBitsOnTruncation.Strip CallerPrivilege.Unprivileged
        let darwin = after SetIdBitsOnTruncation.Preserve CallerPrivilege.Unprivileged

        // Linux, unprivileged: setuid goes whatever the execute bits say; setgid
        // goes only alongside group-execute, because without it the bit means
        // mandatory locking rather than privilege; the sticky bit never moves.
        linux 0o4755 |> shouldEqual 0o0755
        linux 0o4644 |> shouldEqual 0o0644
        linux 0o2755 |> shouldEqual 0o0755
        linux 0o2644 |> shouldEqual 0o2644
        linux 0o6755 |> shouldEqual 0o0755
        linux 0o1755 |> shouldEqual 0o1755
        linux 0o0644 |> shouldEqual 0o0644
        linux 0o2600 |> shouldEqual 0o2600
        linux 0o2640 |> shouldEqual 0o2640
        linux 0o6644 |> shouldEqual 0o2644
        linux 0o3755 |> shouldEqual 0o1755

        // Darwin strips nothing at all, on any of them.
        darwin 0o4755 |> shouldEqual 0o4755
        darwin 0o4644 |> shouldEqual 0o4644
        darwin 0o2755 |> shouldEqual 0o2755
        darwin 0o2644 |> shouldEqual 0o2644
        darwin 0o6755 |> shouldEqual 0o6755
        darwin 0o1755 |> shouldEqual 0o1755
        darwin 0o2600 |> shouldEqual 0o2600
        darwin 0o2640 |> shouldEqual 0o2640
        darwin 0o6644 |> shouldEqual 0o6644
        darwin 0o3755 |> shouldEqual 0o3755

        // Root keeps everything, on either kernel.
        for rule in [ SetIdBitsOnTruncation.Strip ; SetIdBitsOnTruncation.Preserve ] do
            for mode in [ 0o4755 ; 0o4644 ; 0o2755 ; 0o2644 ; 0o6755 ; 0o1755 ; 0o6644 ; 0o3755 ] do
                after rule CallerPrivilege.Privileged mode |> shouldEqual mode

    /// The measured table, as unit assertions, with every expectation written as
    /// an octal literal rather than computed — a version that asked
    /// `afterContentChangingWrite` for its own expectations would agree with any
    /// rule at all.
    ///
    /// Non-root on macOS 26.6 and Linux 6.18.5, and root on both.
    [<Test>]
    let ``a content-changing write strips the set-ID bits, and the flavours differ over S_ISGID`` () : unit =
        let after (rule : SetGroupIdOnWrite) (privilege : CallerPrivilege) (mode : int) : int =
            PermissionBits.parseOrFail "test" mode
            |> PermissionBits.afterContentChangingWrite rule privilege
            |> PermissionBits.toInt

        let linux =
            after SetGroupIdOnWrite.StripWhenGroupExecutable CallerPrivilege.Unprivileged

        let darwin = after SetGroupIdOnWrite.StripAlways CallerPrivilege.Unprivileged

        // Both flavours agree about `S_ISUID` — it goes whatever the execute bits
        // say — and about the sticky bit, which never moves.
        for strip in [ linux ; darwin ] do
            strip 0o4755 |> shouldEqual 0o0755
            strip 0o4644 |> shouldEqual 0o0644
            strip 0o2755 |> shouldEqual 0o0755
            strip 0o6755 |> shouldEqual 0o0755
            strip 0o3755 |> shouldEqual 0o1755
            strip 0o1755 |> shouldEqual 0o1755
            strip 0o0644 |> shouldEqual 0o0644
            strip 0o0 |> shouldEqual 0o0

        // They disagree about `S_ISGID` on a file that is not group-executable.
        // On Linux the bit means mandatory locking rather than privilege and
        // survives; on Darwin it goes like any other set-ID bit.
        linux 0o2644 |> shouldEqual 0o2644
        linux 0o2600 |> shouldEqual 0o2600
        linux 0o2640 |> shouldEqual 0o2640
        linux 0o6644 |> shouldEqual 0o2644

        darwin 0o2644 |> shouldEqual 0o0644
        darwin 0o2600 |> shouldEqual 0o0600
        darwin 0o2640 |> shouldEqual 0o0640
        darwin 0o6644 |> shouldEqual 0o0644

        // Root keeps everything, on either kernel, which is the row that makes
        // this about privilege rather than about a mask applied unconditionally.
        for rule in [ SetGroupIdOnWrite.StripWhenGroupExecutable ; SetGroupIdOnWrite.StripAlways ] do
            for mode in [ 0o4755 ; 0o2755 ; 0o6755 ; 0o2644 ; 0o6644 ; 0o1755 ; 0o0644 ] do
                after rule CallerPrivilege.Privileged mode |> shouldEqual mode

    /// `0o6644` carries both set-ID bits with no group-execute bit, so the three
    /// rules anyone might plausibly implement give three different answers. A
    /// table that happened to omit it would let "strip both bits always" pass as
    /// Linux, which is precisely the confusion this function used to refuse.
    [<Test>]
    let ``the rules are distinguishable, and this is the row that distinguishes them`` () : unit =
        let after (rule : SetGroupIdOnWrite) (mode : int) : int =
            PermissionBits.parseOrFail "test" mode
            |> PermissionBits.afterContentChangingWrite rule CallerPrivilege.Unprivileged
            |> PermissionBits.toInt

        after SetGroupIdOnWrite.StripAlways 0o6644 |> shouldEqual 0o0644
        after SetGroupIdOnWrite.StripWhenGroupExecutable 0o6644 |> shouldEqual 0o2644

        // ...and "preserve everything", the third candidate, is what truncation
        // does on Darwin. Named here so that the write rule cannot quietly
        // acquire it.
        PermissionBits.parseOrFail "test" 0o6644
        |> PermissionBits.afterTruncation SetIdBitsOnTruncation.Preserve CallerPrivilege.Unprivileged
        |> PermissionBits.toInt
        |> shouldEqual 0o6644

    [<Test>]
    let ``writing to something that cannot hold bytes is an interpreter bug, not an errno`` () : unit =
        let vfs = build [ mkdir (rootOf emptyFs) "d" ; mklink (rootOf emptyFs) "l" "d" ]
        let some = ImmutableArray.CreateRange [| 1uy |]

        let directory =
            VirtualFileSystem.resolveExisting
                limits
                CallerPrivilege.Privileged
                (rootOf vfs)
                SymlinkPolicy.Follow
                (path "/d")
                vfs
            |> ok

        let link =
            VirtualFileSystem.resolveExisting
                limits
                CallerPrivilege.Privileged
                (rootOf vfs)
                SymlinkPolicy.NoFollowFinal
                (path "/l")
                vfs
            |> ok

        // A caller reaches `writeFile` only through a descriptor open for
        // writing, and `open` refuses to give one for anything but a regular
        // file — so these are crashes rather than errnos, and the messages say
        // which invariant was broken.
        let shouldFailWith (substring : string) (f : unit -> VirtualFileSystem) : unit =
            let exn = Assert.Throws<exn> (fun () -> f () |> ignore<VirtualFileSystem>)
            exn.Message |> shouldContainText substring

        shouldFailWith
            "is a directory"
            (fun () ->
                VirtualFileSystem.writeFile
                    directory
                    0L
                    some
                    SetGroupIdOnWrite.StripWhenGroupExecutable
                    CallerPrivilege.Unprivileged
                    buildTime
                    vfs
                |> function
                    | Ok vfs -> vfs
                    | Error refusal -> failwith $"%O{refusal}"
            )

        shouldFailWith
            "is a symbolic link"
            (fun () ->
                VirtualFileSystem.writeFile
                    link
                    0L
                    some
                    SetGroupIdOnWrite.StripWhenGroupExecutable
                    CallerPrivilege.Unprivileged
                    buildTime
                    vfs
                |> function
                    | Ok vfs -> vfs
                    | Error refusal -> failwith $"%O{refusal}"
            )

        shouldFailWith
            "is not in this filesystem"
            (fun () ->
                VirtualFileSystem.writeFile
                    (InodeNumber 9999L)
                    0L
                    some
                    SetGroupIdOnWrite.StripWhenGroupExecutable
                    CallerPrivilege.Unprivileged
                    buildTime
                    vfs
                |> function
                    | Ok vfs -> vfs
                    | Error refusal -> failwith $"%O{refusal}"
            )

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

        // More cases than the fixture's shared `config` runs, because the two coverage guards
        // below count occurrences across the whole check rather than asserting something about
        // each case, and at 300 cases the rarer of the two counts is not concentrated enough for
        // any useful threshold to be safe: sampled over 500 checks at 300 cases, the
        // ctime-ahead-of-mtime count ran min=9 median=26, so a threshold of 10 failed a few
        // percent of runs. Over 200 checks at 2000 cases it ran min=119 median=169 — the count
        // scales with the case count while its spread does not, which is what buys a threshold
        // both meaningful and reliable. The extra cases cost no measurable time, because most
        // generated filesystems are tiny.
        Check.One (timesConfig, Prop.forAll (Arb.fromGen filesystemGen) property)

        // Without these the property is satisfied by a model that never moves a timestamp at all:
        // every inode would trivially have all four equal, and every comparison above would hold
        // vacuously. They also fail if the *generator* stops reaching the states — the
        // ctime-ahead-of-mtime count is carried entirely by `Step.MakeHardLink`, which bumps a
        // target inode's ctime while leaving its mtime alone, and which is a no-op until some
        // earlier step has created a file to link to.
        //
        // Thresholds are set at roughly half the minimum sampled at this case count (2261 and 119
        // respectively), so each has a margin of more than 2x against a tail the measurement
        // actually saw, rather than against a guess. Both counts fall to exactly 0 under the
        // regressions they guard against, so anything below those minima discriminates.
        observedLateModification |> shouldBeGreaterThan 1000
        observedCtimeAheadOfMtime |> shouldBeGreaterThan 50

    // ------------------------------------------------------ unbind and forget

    /// The inode a path names, resolved privileged so that a permission bit can
    /// never be why a fixture could not find its own object.
    let private inodeAt (p : string) (vfs : VirtualFileSystem) : InodeNumber =
        VirtualFileSystem.resolveExisting
            limits
            CallerPrivilege.Privileged
            (rootOf vfs)
            SymlinkPolicy.Follow
            (path p)
            vfs
        |> ok

    let private unbindTime : UnixTimestamp =
        UnixTimestamp.createOrFail "test" 1_700_009_000L 11

    [<Test>]
    let ``unbind removes the name and answers the inode it named`` () : unit =
        let vfs =
            build [ mkdir (rootOf emptyFs) "d" ; fun v -> mkfile (inodeAt "/d" v) "f" v ]

        let directory = inodeAt "/d" vfs
        let file = inodeAt "/d/f" vfs

        let target, after =
            VirtualFileSystem.unbind directory (name "f") unbindTime vfs |> ok

        target |> shouldEqual file

        // The name is gone...
        VirtualFileSystem.resolveExisting
            limits
            CallerPrivilege.Privileged
            (rootOf after)
            SymlinkPolicy.Follow
            (path "/d/f")
            after
        |> shouldEqual (Error UnixError.ENOENT)

        // ...and the inode is not. Removing the last name is not what frees an
        // inode; see `VirtualFileSystem.forget`.
        VirtualFileSystem.tryGet file after |> Option.isSome |> shouldEqual true
        VirtualFileSystem.bindingCount file after |> shouldEqual 0

        // Which is exactly the state `checkInvariants` wants told about.
        VirtualFileSystem.checkInvariants Set.empty after
        |> shouldEqual [ VirtualFileSystemDefect.UnreachableFromRoot file ]

    [<Test>]
    let ``unbind moves the directory's mtime and ctime and only the target's ctime`` () : unit =
        // Measured on both platforms, watching the survivor through a held
        // descriptor's `fstat`: after `unlink`, the parent's mtime and ctime have
        // moved and the target's ctime has, while no atime and no mtime of the
        // target has. Identical for an inode that still has links left and for
        // one dropping to zero, which is why one primitive covers both.
        let vfs =
            build
                [
                    mkfile (rootOf emptyFs) "f"
                    fun v ->
                        VirtualFileSystem.hardLink (rootOf v) (name "f2") (inodeAt "/f" v) buildTime v
                        |> ok
                ]

        let file = inodeAt "/f" vfs
        let before = timesOf file vfs

        let _, after =
            VirtualFileSystem.unbind (rootOf vfs) (name "f2") unbindTime vfs |> ok

        let directoryTimes = timesOf (rootOf after) after
        directoryTimes.Modification |> shouldEqual unbindTime
        directoryTimes.StatusChange |> shouldEqual unbindTime
        directoryTimes.Access |> shouldEqual (timesOf (rootOf vfs) vfs).Access

        let targetTimes = timesOf file after
        targetTimes.StatusChange |> shouldEqual unbindTime
        targetTimes.Modification |> shouldEqual before.Modification
        targetTimes.Access |> shouldEqual before.Access
        targetTimes.Birth |> shouldEqual before.Birth

    [<Test>]
    let ``unbind refuses what it cannot name`` () : unit =
        let vfs = build [ mkdir (rootOf emptyFs) "d" ; mkfile (rootOf emptyFs) "f" ]

        // A name the directory does not hold.
        VirtualFileSystem.unbind (rootOf vfs) (name "nx") unbindTime vfs
        |> shouldEqual (Error UnixError.ENOENT)

        // A directory the graph does not hold.
        VirtualFileSystem.unbind (VirtualFileSystem.nextInode vfs) (name "d") unbindTime vfs
        |> shouldEqual (Error UnixError.ENOENT)

        // Something that is not a directory, which cannot hold a name at all.
        VirtualFileSystem.unbind (inodeAt "/f" vfs) (name "x") unbindTime vfs
        |> shouldEqual (Error UnixError.ENOTDIR)

    [<Test>]
    let ``unbind of a symlink's own inode is ENOTDIR`` () : unit =
        let vfs = build [ mkdir (rootOf emptyFs) "d" ; mklink (rootOf emptyFs) "ld" "d" ]

        // `resolveExisting` under `NoFollowFinal` gives the link itself.
        let link =
            VirtualFileSystem.resolveExisting
                limits
                CallerPrivilege.Privileged
                (rootOf vfs)
                SymlinkPolicy.NoFollowFinal
                (path "/ld")
                vfs
            |> ok

        VirtualFileSystem.unbind link (name "x") unbindTime vfs
        |> shouldEqual (Error UnixError.ENOTDIR)

    [<Test>]
    let ``a rejected unbind leaves the filesystem untouched`` () : unit =
        let vfs = build [ mkdir (rootOf emptyFs) "d" ]

        VirtualFileSystem.unbind (rootOf vfs) (name "nx") unbindTime vfs
        |> Result.isError
        |> shouldEqual true

        // Including the timestamps: a refusal that had already stamped the
        // directory would be invisible to a test comparing only the entries.
        timesOf (rootOf vfs) vfs |> shouldEqual (InodeTimes.createdAt buildTime)

    [<Test>]
    let ``unbind can orphan a populated directory`` () : unit =
        // `rename(2)` moves a directory by unbinding and rebinding it, and the
        // subtree is legitimately unreachable in between — so this primitive
        // must not refuse a directory that holds entries, and must not recurse.
        let vfs =
            build [ mkdir (rootOf emptyFs) "d" ; fun v -> mkfile (inodeAt "/d" v) "kid" v ]

        let directory = inodeAt "/d" vfs
        let kid = inodeAt "/d/kid" vfs

        let target, after =
            VirtualFileSystem.unbind (rootOf vfs) (name "d") unbindTime vfs |> ok

        target |> shouldEqual directory

        // The subtree is intact; only its attachment to the root has gone.
        VirtualFileSystem.bindingCount kid after |> shouldEqual 1
        VirtualFileSystem.tryGet kid after |> Option.isSome |> shouldEqual true

        VirtualFileSystem.checkInvariants Set.empty after
        |> shouldEqual
            [
                VirtualFileSystemDefect.UnreachableFromRoot directory
                VirtualFileSystemDefect.UnreachableFromRoot kid
            ]

    [<Test>]
    let ``unbind rejects a forged default name`` () : unit =
        // The chokepoint argument `bind` makes: `Unchecked.defaultof<FileName>`
        // matches no parsed name, so an unchecked unbind would silently remove
        // nothing and report ENOENT for a name the caller believed in.
        let vfs = build [ mkfile (rootOf emptyFs) "f" ]

        let thrown =
            Assert.Throws<Exception> (fun () ->
                VirtualFileSystem.unbind (rootOf vfs) Unchecked.defaultof<FileName> unbindTime vfs
                |> ignore<Result<InodeNumber * VirtualFileSystem, UnixError>>
            )

        thrown.Message |> shouldContainText "Unchecked.defaultof"

    // ----------------------------------------------------------- bindingCount

    [<Test>]
    let ``bindingCount counts every name and no other reference`` () : unit =
        let vfs =
            build
                [
                    mkdir (rootOf emptyFs) "d"
                    mkfile (rootOf emptyFs) "f"
                    fun v ->
                        VirtualFileSystem.hardLink (rootOf v) (name "f2") (inodeAt "/f" v) buildTime v
                        |> ok
                    fun v ->
                        VirtualFileSystem.hardLink (inodeAt "/d" v) (name "f3") (inodeAt "/f" v) buildTime v
                        |> ok
                    mklink (rootOf emptyFs) "ld" "d"
                ]

        // Three names, one of them in another directory.
        VirtualFileSystem.bindingCount (inodeAt "/f" vfs) vfs |> shouldEqual 3

        // A symlink pointing at `d` is not a name *for* `d`: it is an inode of
        // its own holding a string, and resolving it is a lookup rather than a
        // link. A count that walked symlink targets would say 2 here.
        VirtualFileSystem.bindingCount (inodeAt "/d" vfs) vfs |> shouldEqual 1

        // The root has no incoming entry at all, by construction.
        VirtualFileSystem.bindingCount (rootOf vfs) vfs |> shouldEqual 0

        // Nor does an inode the graph has never heard of.
        VirtualFileSystem.bindingCount (VirtualFileSystem.nextInode vfs) vfs
        |> shouldEqual 0

    [<Test>]
    let ``bindingCount follows unbind down`` () : unit =
        let vfs =
            build
                [
                    mkfile (rootOf emptyFs) "f"
                    fun v ->
                        VirtualFileSystem.hardLink (rootOf v) (name "f2") (inodeAt "/f" v) buildTime v
                        |> ok
                ]

        let file = inodeAt "/f" vfs
        VirtualFileSystem.bindingCount file vfs |> shouldEqual 2

        let _, once = VirtualFileSystem.unbind (rootOf vfs) (name "f2") unbindTime vfs |> ok
        VirtualFileSystem.bindingCount file once |> shouldEqual 1

        let _, twice =
            VirtualFileSystem.unbind (rootOf once) (name "f") unbindTime once |> ok

        VirtualFileSystem.bindingCount file twice |> shouldEqual 0

    // ---------------------------------------------------------------- forget

    [<Test>]
    let ``forget removes an unbound inode and leaves the graph sound`` () : unit =
        let vfs = build [ mkfile (rootOf emptyFs) "f" ]
        let file = inodeAt "/f" vfs

        let _, orphaned =
            VirtualFileSystem.unbind (rootOf vfs) (name "f") unbindTime vfs |> ok

        let forgotten = VirtualFileSystem.forget file orphaned

        VirtualFileSystem.tryGet file forgotten |> shouldEqual None
        VirtualFileSystem.checkInvariants Set.empty forgotten |> shouldEqual []

        // The number is not handed back out; a stale comparison must never be
        // able to say "same file" about a different one.
        VirtualFileSystem.nextInode forgotten
        |> shouldEqual (VirtualFileSystem.nextInode vfs)

    [<Test>]
    let ``forget refuses an inode something still names`` () : unit =
        let vfs = build [ mkfile (rootOf emptyFs) "f" ]

        let thrown =
            Assert.Throws<Exception> (fun () ->
                VirtualFileSystem.forget (inodeAt "/f" vfs) vfs |> ignore<VirtualFileSystem>
            )

        thrown.Message |> shouldContainText "still named by 1"

    [<Test>]
    let ``forget refuses the root and an absent inode`` () : unit =
        // Both are interpreter bugs rather than anything a guest can cause, and
        // both are silently catastrophic if allowed: forgetting the root leaves
        // no filesystem, and forgetting an absent inode hides the double-free
        // that produced the call.
        let root =
            Assert.Throws<Exception> (fun () ->
                VirtualFileSystem.forget (rootOf emptyFs) emptyFs |> ignore<VirtualFileSystem>
            )

        root.Message |> shouldContainText "root cannot be forgotten"

        let absent =
            Assert.Throws<Exception> (fun () ->
                VirtualFileSystem.forget (VirtualFileSystem.nextInode emptyFs) emptyFs
                |> ignore<VirtualFileSystem>
            )

        absent.Message |> shouldContainText "not in the graph"

    // ------------------------------------------------- the pinned-inode excuse

    [<Test>]
    let ``a pinned inode is excused unreachability and an unpinned one is not`` () : unit =
        let vfs = build [ mkfile (rootOf emptyFs) "f" ]
        let file = inodeAt "/f" vfs

        let _, orphaned =
            VirtualFileSystem.unbind (rootOf vfs) (name "f") unbindTime vfs |> ok

        VirtualFileSystem.checkInvariants Set.empty orphaned
        |> shouldEqual [ VirtualFileSystemDefect.UnreachableFromRoot file ]

        VirtualFileSystem.checkInvariants (Set.singleton file) orphaned
        |> shouldEqual []

        // Pinning some *other* inode does not excuse this one: an excuse that
        // fired for a non-empty set rather than for the named member would pass
        // the row above.
        VirtualFileSystem.checkInvariants (Set.singleton (VirtualFileSystem.nextInode orphaned)) orphaned
        |> shouldEqual [ VirtualFileSystemDefect.UnreachableFromRoot file ]

    [<Test>]
    let ``pinning a reachable inode changes nothing`` () : unit =
        // The overwhelmingly common case — a descriptor on a file that still has
        // its name — so the set must excuse unreachability rather than assert it.
        let vfs = build [ mkfile (rootOf emptyFs) "f" ]

        VirtualFileSystem.checkInvariants (Set.singleton (inodeAt "/f" vfs)) vfs
        |> shouldEqual []

    [<Test>]
    let ``pinning does not excuse any other defect`` () : unit =
        // The orphaned pair from the reachability tests, with both pinned: the
        // graph is still a graph no kernel could produce, because the two hold
        // each other rather than being held by a descriptor apiece. Only the
        // *unreachability* rule takes the set.
        let vfs =
            VirtualFileSystem.Unchecked.ofParts
                (Map.ofList [ one, dir one [ "a", two ] ; two, dir one [] ; three, regularFileInode ])
                one
                (InodeNumber 4L)

        // `three` is unreachable and pinned, so excused; `two` records `one` as
        // its parent, which is true, so the graph's only remaining complaint
        // would be about `three`.
        VirtualFileSystem.checkInvariants (Set.singleton three) vfs |> shouldEqual []

        VirtualFileSystem.checkInvariants Set.empty vfs
        |> shouldEqual [ VirtualFileSystemDefect.UnreachableFromRoot three ]

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
            CallerPrivilege.Privileged
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
                CallerPrivilege.Privileged
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
                CallerPrivilege.Privileged
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
                CallerPrivilege.Privileged
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
                CallerPrivilege.Privileged
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

/// `writtenLength` and `writtenContents` decide the whole of what a write does to
/// a regular file's bytes, once its error cases are out of the way — the read
/// path's `readTransferCount` in the other direction, and the same class of
/// off-by-one hides in it. As functions of two byte arrays and an offset they can
/// be checked against naive splicing, which is what this does.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestWrittenContents =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 2000

    /// Contents, offset and bytes drawn from a small range, so that the boundary
    /// cases — a write starting exactly at the end, one landing entirely inside,
    /// an empty file, a hole of exactly one byte — come up constantly. The full
    /// byte alphabet, deliberately: the hole is asserted by *index* below rather
    /// than by looking for a zero, so there is no need to exclude zero from the
    /// data and no risk of a property that only holds because it was excluded.
    let private smallCase : Gen<byte[] * int64 * byte[]> =
        gen {
            let! contents = Gen.arrayOf (Gen.choose (0, 255) |> Gen.map byte) |> Gen.resize 10
            let! offset = Gen.choose (0, 14)
            let! bytes = Gen.arrayOf (Gen.choose (0, 255) |> Gen.map byte) |> Gen.resize 6
            return contents, int64 offset, bytes
        }

    let private written (contents : byte[]) (offset : int64) (bytes : byte[]) : byte[] =
        match
            VirtualFileSystem.writtenContents
                (ImmutableArray.CreateRange contents)
                offset
                (ImmutableArray.CreateRange bytes)
        with
        | Ok result -> Seq.toArray result
        | Error refusal -> failwith $"expected success, got %O{refusal}"

    /// The oracle: the file you get by actually laying the bytes out.
    ///
    /// The empty-write case is stated separately because it is *not* the general
    /// rule specialised — a zero-length write does not extend the file to
    /// `offset`, which is what both platforms were measured doing.
    let private naive (contents : byte[]) (offset : int64) (bytes : byte[]) : byte[] =
        if bytes.Length = 0 then
            contents
        else

        let offset = int offset
        let length = max contents.Length (offset + bytes.Length)

        Array.init
            length
            (fun i ->
                if i >= offset && i < offset + bytes.Length then
                    bytes.[i - offset]
                elif i < contents.Length then
                    contents.[i]
                else
                    0uy
            )

    [<Test>]
    let ``agrees with naive splicing`` () : unit =
        let property (contents : byte[], offset : int64, bytes : byte[]) : bool =
            written contents offset bytes = naive contents offset bytes

        Check.One (config, Prop.forAll (Arb.fromGen smallCase) property)

    /// Stated directly rather than inferred from the oracle, because a caller
    /// reads the new length back through `stat` and the four claims are what it
    /// depends on: the prefix survives, the hole reads as zero, the written window
    /// is exactly what was passed, and a write inside the file does not truncate
    /// the tail.
    [<Test>]
    let ``a write overwrites its window, fills the hole with zeroes, and keeps the rest`` () : unit =
        let property (contents : byte[], offset : int64, bytes : byte[]) : bool =
            let result = written contents offset bytes
            let offset = int offset

            // An empty write has no window and opens no hole, so the four clauses
            // below do not apply to it — its rule is the whole of this. Stated
            // rather than folded in: writing it as one set of ranges is what makes
            // "the hole between the old end and `offset`" describe bytes that a
            // zero-length write never brought into existence.
            if bytes.Length = 0 then
                result = contents
            else

            let prefixKept =
                Seq.forall (fun i -> result.[i] = contents.[i]) (seq { 0 .. min offset contents.Length - 1 })

            let holeIsZero =
                Seq.forall (fun i -> result.[i] = 0uy) (seq { contents.Length .. offset - 1 })

            let windowWritten =
                Seq.forall (fun i -> result.[offset + i] = bytes.[i]) (seq { 0 .. bytes.Length - 1 })

            let tailKept =
                Seq.forall (fun i -> result.[i] = contents.[i]) (seq { offset + bytes.Length .. contents.Length - 1 })

            prefixKept && holeIsZero && windowWritten && tailKept

        Check.One (config, Prop.forAll (Arb.fromGen smallCase) property)

    /// The length is what `writtenLength` said it would be. These are the two
    /// halves a handler uses — `stat` reports the length while the bytes come back
    /// through `pread` — so a disagreement between them is a file whose reported
    /// size does not match its contents.
    [<Test>]
    let ``the resulting length is the one writtenLength predicts`` () : unit =
        let property (contents : byte[], offset : int64, bytes : byte[]) : bool =
            let result = written contents offset bytes

            VirtualFileSystem.writtenLength offset bytes.Length contents.Length = Ok result.Length

        Check.One (config, Prop.forAll (Arb.fromGen smallCase) property)

    /// The round trip a guest actually performs: write, then read the same window
    /// back. Composes the two halves of the model, so an offset convention that
    /// disagreed between them would show up here even though each is internally
    /// consistent.
    [<Test>]
    let ``bytes written at an offset read back from that offset`` () : unit =
        let property (contents : byte[], offset : int64, bytes : byte[]) : bool =
            let result = written contents offset bytes

            let transfer = VirtualFileSystem.readTransferCount offset bytes.Length result.Length

            transfer = bytes.Length
            && Seq.forall (fun i -> result.[int offset + i] = bytes.[i]) (seq { 0 .. bytes.Length - 1 })

        Check.One (config, Prop.forAll (Arb.fromGen smallCase) property)

    /// A zero-length write is the identity, at *any* offset — including one far
    /// past the end, where extending would have been the plausible thing to do.
    [<Test>]
    let ``an empty write changes nothing, however far past the end it is aimed`` () : unit =
        for offset in [ 0L ; 4L ; 10_000L ; Int64.MaxValue ] do
            written [| 1uy ; 2uy ; 3uy ; 4uy |] offset [||]
            |> shouldEqual [| 1uy ; 2uy ; 3uy ; 4uy |]

            VirtualFileSystem.writtenLength offset 0 4 |> shouldEqual (Ok 4)

    /// Both sides of the ceiling, which is why `writtenLength` exists separately:
    /// the accepting side would allocate two gigabytes if it had to go through
    /// `writtenContents`.
    [<Test>]
    let ``the length ceiling is refused from one byte past it, and not before`` () : unit =
        let ceiling = VirtualFileSystem.maxFileLength

        VirtualFileSystem.writtenLength (ceiling - 4L) 4 0
        |> shouldEqual (Ok (int ceiling))

        VirtualFileSystem.writtenLength (ceiling - 3L) 4 0
        |> shouldEqual (Error (FileWriteRefusal.WouldExceedMaxLength (ceiling - 3L, 4)))

        // A huge offset must be refused rather than wrapping onto a low sum that
        // the comparison would accept — which is what writing the check as
        // `offset + count > ceiling` would do.
        for offset in [ ceiling ; ceiling + 1L ; Int64.MaxValue ; Int64.MaxValue - 3L ] do
            VirtualFileSystem.writtenLength offset 4 0
            |> shouldEqual (Error (FileWriteRefusal.WouldExceedMaxLength (offset, 4)))

    [<Test>]
    let ``worked examples`` () : unit =
        let contents = [| byte 'a' ; byte 'b' ; byte 'c' ; byte 'd' |]

        // The measured `pwrite` row: "WXYZ" at offset 8 of a four-byte file gives
        // a twelve-byte file with a four-byte hole of zeroes.
        written contents 8L [| byte 'W' ; byte 'X' ; byte 'Y' ; byte 'Z' |]
        |> shouldEqual
            [|
                byte 'a'
                byte 'b'
                byte 'c'
                byte 'd'
                0uy
                0uy
                0uy
                0uy
                byte 'W'
                byte 'X'
                byte 'Y'
                byte 'Z'
            |]

        // Overwriting in place, which must not truncate the tail.
        written contents 1L [| byte 'Z' |]
        |> shouldEqual [| byte 'a' ; byte 'Z' ; byte 'c' ; byte 'd' |]

        // Straddling the end: part overwrite, part extension, no hole.
        written contents 3L [| byte 'Y' ; byte 'Z' |]
        |> shouldEqual [| byte 'a' ; byte 'b' ; byte 'c' ; byte 'Y' ; byte 'Z' |]

        // Writing into an empty file at a non-zero offset is all hole.
        written [||] 2L [| byte 'x' |] |> shouldEqual [| 0uy ; 0uy ; byte 'x' |]

/// `VirtualFileSystem.seekTarget`: the whole of what `lseek(2)` computes, once the descriptor and
/// the whence have been resolved. Property-tested here because as a function of four integers it
/// can be, where the same arithmetic inlined in a handler is reachable only through a guest — and
/// the two faults it distinguishes are *indistinguishable* through a Linux-flavoured guest, both
/// being EINVAL.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTruncatedContents =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 2000

    /// Lengths drawn from a small range around the file's own, so that the three
    /// interesting boundaries — shorter, exactly equal, longer — come up
    /// constantly. `Gen.choose` rather than the default `int` generator, which is
    /// size-bounded in a way that makes "exactly equal" rare.
    let private smallCase : Gen<byte[] * int64> =
        gen {
            let! length = Gen.choose (0, 12)
            let! contents = Gen.arrayOfLength length (Gen.choose (0, 255) |> Gen.map byte)
            let! target = Gen.choose (0, 14)
            return contents, int64 target
        }

    /// The oracle: take what fits, pad the rest with zeroes.
    let private naive (contents : byte[]) (length : int64) : byte[] =
        Array.init (int length) (fun i -> if i < contents.Length then contents.[i] else 0uy)

    [<Test>]
    let ``agrees with naive take-and-pad`` () : unit =
        let property (contents : byte[], length : int64) : bool =
            match VirtualFileSystem.truncatedContents (ImmutableArray.CreateRange contents) length with
            | Ok result -> result |> Seq.toArray = naive contents length
            | Error refusal -> failwith $"expected success, got %O{refusal}"

        Check.One (config, Prop.forAll (Arb.fromGen smallCase) property)

    /// Stated directly rather than inferred from the oracle, because it is the
    /// property a caller relies on when it hands the result back to `stat`.
    [<Test>]
    let ``the result is exactly as long as it was asked to be`` () : unit =
        let property (contents : byte[], length : int64) : bool =
            match VirtualFileSystem.truncatedContents (ImmutableArray.CreateRange contents) length with
            | Ok result -> int64 result.Length = length
            | Error refusal -> failwith $"expected success, got %O{refusal}"

        Check.One (config, Prop.forAll (Arb.fromGen smallCase) property)

    [<Test>]
    let ``a length the file already has returns it unchanged`` () : unit =
        let contents = ImmutableArray.CreateRange [| 1uy ; 2uy ; 3uy |]

        VirtualFileSystem.truncatedContents contents 3L |> shouldEqual (Ok contents)

    [<Test>]
    let ``a length beyond what the model can hold is refused rather than allocated`` () : unit =
        let contents = ImmutableArray.CreateRange [| 1uy ; 2uy ; 3uy |]
        let tooLong = VirtualFileSystem.maxFileLength + 1L

        VirtualFileSystem.truncatedContents contents tooLong
        |> shouldEqual (Error (FileTruncationRefusal.WouldExceedMaxLength tooLong))

    /// Both sides of the ceiling, which only `truncatedLength` can state:
    /// answering for the largest permitted length through `truncatedContents`
    /// would allocate two gigabytes to do it.
    [<Test>]
    let ``the ceiling is inclusive, and refuses exactly one byte above it`` () : unit =
        VirtualFileSystem.truncatedLength VirtualFileSystem.maxFileLength
        |> shouldEqual (Ok System.Array.MaxLength)

        let tooLong = VirtualFileSystem.maxFileLength + 1L

        VirtualFileSystem.truncatedLength tooLong
        |> shouldEqual (Error (FileTruncationRefusal.WouldExceedMaxLength tooLong))

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

/// `CreatingOpenRules.verdict` is the whole of what `open(O_CREAT)` decides, and
/// most of it is compared against a real kernel in
/// `TestVirtualFileSystemAgainstHost`. These are the rows that comparison cannot
/// reach: the flavour it is *not* running on, the paths its temporary root
/// cannot express, and the permission bits its corpus does not have. Every
/// expectation is a measurement against real `open(2)` on macOS 26.6/APFS and
/// Linux 6.x, at an unprivileged uid with umask 022.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCreatingOpenRules =

    let private name (s : string) : FileName = FileName.parseOrFail "test" s

    let private path (s : string) : UnixPath = UnixPath.parseOrFail "test" s

    let private buildTime : UnixTimestamp =
        UnixTimestamp.createOrFail "test" 1_700_000_000L 0

    let private linux : CreatingOpenRules =
        SimulatedUnixPlatform.creatingOpenRules SimulatedUnixPlatform.linuxX64

    let private darwin : CreatingOpenRules =
        SimulatedUnixPlatform.creatingOpenRules SimulatedUnixPlatform.macOsArm64

    let private mode (raw : int) : PermissionBits = PermissionBits.parseOrFail "test" raw

    /// A root holding a directory `d`, a file `f`, and a directory `locked`
    /// whose permission bits are given.
    let private treeWith (lockedBits : PermissionBits) : VirtualFileSystem =
        let vfs = VirtualFileSystem.empty buildTime
        let root = VirtualFileSystem.root vfs

        let apply (result : Result<InodeNumber * VirtualFileSystem, UnixError>) : VirtualFileSystem =
            match result with
            | Ok (_, vfs) -> vfs
            | Error error -> failwith $"could not build the tree: %O{error}"

        vfs
        |> fun vfs ->
            apply (VirtualFileSystem.createDirectory root (name "d") PermissionBits.defaultForDirectory buildTime vfs)
        |> fun vfs ->
            apply (
                VirtualFileSystem.createFile
                    root
                    (name "f")
                    PermissionBits.defaultForRegularFile
                    buildTime
                    ImmutableArray<byte>.Empty
                    vfs
            )
        |> fun vfs -> apply (VirtualFileSystem.createDirectory root (name "locked") lockedBits buildTime vfs)

    let private tree : VirtualFileSystem = treeWith PermissionBits.defaultForDirectory

    /// Resolve as a creating open of the given flavour would, then ask for the
    /// verdict — so the `Resolution` under test is one the walk really produces
    /// rather than one this test hand-assembled.
    let private verdict
        (rules : CreatingOpenRules)
        (privilege : CallerPrivilege)
        (exclusive : bool)
        (vfs : VirtualFileSystem)
        (candidate : string)
        : CreatingOpenVerdict
        =
        let limits = SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.linuxX64

        let policy =
            if exclusive then
                SymlinkPolicy.NoFollowFinal
            else
                SymlinkPolicy.Follow

        match
            VirtualFileSystem.resolveFull
                limits
                privilege
                (VirtualFileSystem.root vfs)
                policy
                rules.TrailingSeparator
                (path candidate)
                vfs
        with
        | Error error -> CreatingOpenVerdict.Refuse error
        | Ok resolution -> CreatingOpenRules.verdict rules privilege true exclusive resolution vfs

    [<Test>]
    let ``a path that consumed no component diverges between the two kernels`` () : unit =
        // Measured: `open("/", O_RDONLY|O_CREAT)` is EEXIST on macOS and EISDIR
        // on Linux. Pinned as a property of the *navigation* rather than of the
        // root inode -- on macOS "/." and "/../" reach the same inode and open
        // fine, and "/System/Volumes/Data", a writable volume's mount root, does
        // too. `TestVirtualFileSystemAgainstHost` cannot carry this row: it
        // prefixes every path with a temporary directory, so the kernel never
        // sees a path with no components.
        verdict darwin CallerPrivilege.Unprivileged false tree "/"
        |> shouldEqual (CreatingOpenVerdict.Refuse UnixError.EEXIST)

        verdict linux CallerPrivilege.Unprivileged false tree "/"
        |> shouldEqual (CreatingOpenVerdict.Refuse UnixError.EISDIR)

        // ...while the navigations that reach the same inode do not diverge on
        // Darwin, which is what makes this about `FinalNavigation` rather than
        // about the root.
        verdict darwin CallerPrivilege.Unprivileged false tree "/."
        |> shouldEqual (CreatingOpenVerdict.OpenExisting (VirtualFileSystem.root tree))

        verdict darwin CallerPrivilege.Unprivileged false tree "/d/.."
        |> shouldEqual (CreatingOpenVerdict.OpenExisting (VirtualFileSystem.root tree))

    [<Test>]
    let ``only Linux refuses a creating open that lands on a directory`` () : unit =
        // Measured: `open("d", O_RDONLY|O_CREAT)` is EISDIR on Linux and opens
        // the directory on macOS, where a plain `open("d", O_RDONLY)` succeeds
        // on both. This is the divergence CI checks from the other side.
        match verdict darwin CallerPrivilege.Unprivileged false tree "/d" with
        | CreatingOpenVerdict.OpenExisting _ -> ()
        | other -> failwith $"expected Darwin to open the directory, got %A{other}"

        verdict linux CallerPrivilege.Unprivileged false tree "/d"
        |> shouldEqual (CreatingOpenVerdict.Refuse UnixError.EISDIR)

    [<Test>]
    let ``O_EXCL on an existing directory beats the directory refusal`` () : unit =
        // Measured on both: `open(".", O_CREAT|O_EXCL)` is EEXIST while
        // `open(".", O_CREAT)` is EISDIR on Linux. So the two refusals are
        // ordered, and a handler that checked the directory rule first would
        // report EISDIR where every kernel reports EEXIST.
        for rules in [ linux ; darwin ] do
            verdict rules CallerPrivilege.Unprivileged true tree "/d"
            |> shouldEqual (CreatingOpenVerdict.Refuse UnixError.EEXIST)

    [<Test>]
    let ``binding a name needs both write and search on the holding directory`` () : unit =
        // Measured unanimously at uid 1000: 0o333 and 0o300 succeed, while 0o644
        // (write, no search), 0o555 (search, no write) and 0o111 (search only)
        // are all EACCES. The host oracle's corpus has no such directory, so
        // this is the only place the rule is stated.
        for permitted in [ 0o333 ; 0o300 ; 0o777 ] do
            match verdict linux CallerPrivilege.Unprivileged false (treeWith (mode permitted)) "/locked/new" with
            | CreatingOpenVerdict.Create (_, created) -> created |> shouldEqual (name "new")
            | other -> failwith $"expected mode 0o%s{Convert.ToString (permitted, 8)} to permit creation, got %A{other}"

        for refused in [ 0o644 ; 0o555 ; 0o111 ; 0o000 ] do
            verdict linux CallerPrivilege.Unprivileged false (treeWith (mode refused)) "/locked/new"
            |> shouldEqual (CreatingOpenVerdict.Refuse UnixError.EACCES)

    [<Test>]
    let ``root bypasses the permission rule but not the others`` () : unit =
        // Measured: as uid 0 a creating open succeeds in a mode-0000 directory.
        // The second half is what stops "privileged" being read as "unchecked":
        // root still gets EISDIR from Linux's directory rule.
        match verdict linux CallerPrivilege.Privileged false (treeWith (mode 0o000)) "/locked/new" with
        | CreatingOpenVerdict.Create _ -> ()
        | other -> failwith $"expected root to create in a mode-0000 directory, got %A{other}"

        verdict linux CallerPrivilege.Privileged false tree "/d"
        |> shouldEqual (CreatingOpenVerdict.Refuse UnixError.EISDIR)

    [<Test>]
    let ``a free name that demands a directory creates nothing`` () : unit =
        // Measured on Darwin: `open("nx/", O_CREAT)` and `open("nx/", O_CREAT|O_EXCL)`
        // are both ENOENT and leave the name free. Linux never reaches this arm,
        // having refused the path in the walk -- which the second row states, so
        // that a reader can see the two kernels reach the same "nothing was
        // created" by different routes.
        for exclusive in [ false ; true ] do
            verdict darwin CallerPrivilege.Unprivileged exclusive tree "/nx/"
            |> shouldEqual (CreatingOpenVerdict.Refuse UnixError.ENOENT)

            verdict linux CallerPrivilege.Unprivileged exclusive tree "/nx/"
            |> shouldEqual (CreatingOpenVerdict.Refuse UnixError.EISDIR)

    [<Test>]
    let ``a non-creating open never creates and never refuses a directory`` () : unit =
        // The guard on `creating`: with it false, the verdict must be exactly
        // what an ordinary `open` wants, whatever the flavour says about
        // creating opens.
        let limits = SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.linuxX64

        let resolveFor (candidate : string) : Resolution =
            match
                VirtualFileSystem.resolveFull
                    limits
                    CallerPrivilege.Privileged
                    (VirtualFileSystem.root tree)
                    SymlinkPolicy.Follow
                    TrailingSeparatorPolicy.Demand
                    (path candidate)
                    tree
            with
            | Ok resolution -> resolution
            | Error error -> failwith $"could not resolve %s{candidate}: %O{error}"

        for rules in [ linux ; darwin ] do
            match CreatingOpenRules.verdict rules CallerPrivilege.Unprivileged false false (resolveFor "/d") tree with
            | CreatingOpenVerdict.OpenExisting _ -> ()
            | other -> failwith $"a non-creating open of a directory must open it, got %A{other}"

            CreatingOpenRules.verdict rules CallerPrivilege.Unprivileged false false (resolveFor "/nx") tree
            |> shouldEqual (CreatingOpenVerdict.Refuse UnixError.ENOENT)

    [<Test>]
    let ``the created mode is masked by the platform and then by the umask`` () : unit =
        // Measured with umask 022: `mode 0o7777` creates 0o7755 on Linux and
        // 0o0755 on macOS, because XNU masks the mode with ACCESSPERMS and so a
        // Darwin guest cannot create a setuid, setgid or sticky file at all.
        let umask = mode 0o022

        CreatingOpenRules.createdPermissions linux umask 0o7777
        |> shouldEqual (mode 0o7755)

        CreatingOpenRules.createdPermissions darwin umask 0o7777
        |> shouldEqual (mode 0o0755)

        // Each special bit on its own, which is what separates "Darwin drops
        // setuid" from "Darwin drops all three".
        for raw, expected in [ 0o4644, 0o4644 ; 0o2644, 0o2644 ; 0o1644, 0o1644 ] do
            CreatingOpenRules.createdPermissions linux (mode 0o000) raw
            |> shouldEqual (mode expected)

            CreatingOpenRules.createdPermissions darwin (mode 0o000) raw
            |> shouldEqual (mode 0o644)

        // A bit above the permission word is dropped rather than rejected:
        // measured, `mode` 0o10777 creates 0o0755 on both.
        for rules in [ linux ; darwin ] do
            CreatingOpenRules.createdPermissions rules umask 0o10777
            |> shouldEqual (mode 0o0755)

        // A umask covering every permission bit clears them all; one of 0 masks
        // nothing.
        CreatingOpenRules.createdPermissions linux (mode 0o0777) 0o0777
        |> shouldEqual (mode 0o000)

        CreatingOpenRules.createdPermissions linux (mode 0o000) 0o0666
        |> shouldEqual (mode 0o666)

    [<Test>]
    let ``only the umask's permission bits take part`` () : unit =
        // Measured on Linux: `umask(2)` stores `mask & 0o777`, so `umask(0o4000)`
        // reads back 0o0000 and a requested 0o4644 stays 0o4644. Applying the
        // mask at full width would clear the set-user-ID bit instead, making a
        // setuid file impossible for a guest to create at all.
        for raw in [ 0o4000 ; 0o2000 ; 0o1000 ; 0o7000 ] do
            CreatingOpenRules.createdPermissions linux (mode raw) 0o7644
            |> shouldEqual (mode 0o7644)

        // ...and with low bits set too, only those low bits bite: measured,
        // `umask 0o7777` with mode 0o7777 creates 0o7000 on Linux, not 0o0000.
        CreatingOpenRules.createdPermissions linux (mode 0o7777) 0o7777
        |> shouldEqual (mode 0o7000)

        // On Darwin the upper mask bits are unobservable either way, because the
        // platform's own mask has already dropped them from the mode. Measured:
        // `umask 0o4000` with mode 0o4644 gives 0o0644, and `umask 0o7777` with
        // mode 0o7777 gives 0o0000.
        CreatingOpenRules.createdPermissions darwin (mode 0o4000) 0o4644
        |> shouldEqual (mode 0o0644)

        CreatingOpenRules.createdPermissions darwin (mode 0o7777) 0o7777
        |> shouldEqual (mode 0o0000)

/// `mkdir(2)`'s rules, in the rows `TestVirtualFileSystemAgainstHost` cannot
/// reach: the flavour it is *not* running on, the path its temporary root cannot
/// express, the permission bits its corpus does not have, and the identity of
/// the name a creation actually binds. Every expectation is a measurement
/// against real `mkdir(2)` on macOS 25.6/APFS at uid 501 and Linux 6.x arm64 at
/// uid 1000, umask 022, with a fresh tree per row.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMkDirRules =

    let private name (s : string) : FileName = FileName.parseOrFail "test" s

    let private path (s : string) : UnixPath = UnixPath.parseOrFail "test" s

    let private target (s : string) : SymlinkTarget = SymlinkTarget.parseOrFail "test" s

    let private buildTime : UnixTimestamp =
        UnixTimestamp.createOrFail "test" 1_700_000_000L 0

    let private linux : MkDirRules =
        SimulatedUnixPlatform.mkDirRules SimulatedUnixPlatform.linuxX64

    let private darwin : MkDirRules =
        SimulatedUnixPlatform.mkDirRules SimulatedUnixPlatform.macOsArm64

    let private mode (raw : int) : PermissionBits = PermissionBits.parseOrFail "test" raw

    /// A root holding `d` (a directory), `f` (a file), `lf -> f`, `ld -> d`,
    /// `dang -> nx` and `cyc -> cyc`, plus a directory `locked` whose permission
    /// bits are given.
    let private treeWith (lockedBits : PermissionBits) : VirtualFileSystem =
        let vfs = VirtualFileSystem.empty buildTime
        let root = VirtualFileSystem.root vfs

        let apply (result : Result<InodeNumber * VirtualFileSystem, UnixError>) : VirtualFileSystem =
            match result with
            | Ok (_, vfs) -> vfs
            | Error error -> failwith $"could not build the tree: %O{error}"

        vfs
        |> fun vfs ->
            apply (VirtualFileSystem.createDirectory root (name "d") PermissionBits.defaultForDirectory buildTime vfs)
        |> fun vfs ->
            apply (
                VirtualFileSystem.createFile
                    root
                    (name "f")
                    PermissionBits.defaultForRegularFile
                    buildTime
                    ImmutableArray<byte>.Empty
                    vfs
            )
        |> fun vfs -> apply (VirtualFileSystem.createSymlink root (name "lf") buildTime (target "f") vfs)
        |> fun vfs -> apply (VirtualFileSystem.createSymlink root (name "ld") buildTime (target "d") vfs)
        |> fun vfs -> apply (VirtualFileSystem.createSymlink root (name "dang") buildTime (target "nx") vfs)
        |> fun vfs -> apply (VirtualFileSystem.createSymlink root (name "cyc") buildTime (target "cyc") vfs)
        |> fun vfs ->
            // `locked` holds a child, so that a row can ask what an *existing*
            // name inside an unreachable directory answers. The builder applies
            // no permission rule of its own — those live in the verdict — so a
            // 0o000 directory can still be given one here.
            let locked, vfs =
                match VirtualFileSystem.createDirectory root (name "locked") lockedBits buildTime vfs with
                | Ok (inode, vfs) -> inode, vfs
                | Error error -> failwith $"could not build the tree: %O{error}"

            apply (
                VirtualFileSystem.createDirectory locked (name "kid") PermissionBits.defaultForDirectory buildTime vfs
            )

    let private tree : VirtualFileSystem = treeWith PermissionBits.defaultForDirectory

    /// Resolve as a `mkdir` of the given flavour would, then ask for the verdict
    /// — so the `Resolution` under test is one the walk really produces rather
    /// than one this test hand-assembled. Mirrors what `SystemNative_MkDir`
    /// does, which is why a wrong policy here would be a wrong policy there too.
    ///
    /// `privilege` reaches the walk as well as the verdict, because the two now
    /// split the permission rule between them: the walk refuses a directory this
    /// caller may not search, and the verdict one it may not write. A row that
    /// hands privilege only to the verdict would be testing a resolution no
    /// handler could produce.
    let private verdict
        (rules : MkDirRules)
        (privilege : CallerPrivilege)
        (vfs : VirtualFileSystem)
        (candidate : string)
        : MkDirVerdict
        =
        let limits = SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.linuxX64

        match
            VirtualFileSystem.resolveFull
                limits
                privilege
                (VirtualFileSystem.root vfs)
                SymlinkPolicy.NoFollowFinal
                rules.TrailingSeparator
                (path candidate)
                vfs
        with
        | Error error -> MkDirVerdict.Refuse error
        | Ok resolution -> MkDirRules.verdict privilege resolution vfs

    /// The name a verdict binds, so a row can say *what* was created rather than
    /// only that something was.
    let private bound (verdict : MkDirVerdict) : string =
        match verdict with
        | MkDirVerdict.Create (_, name, _) -> FileName.toString name
        | MkDirVerdict.Refuse error -> failwith $"expected a creation, got %O{error}"

    [<Test>]
    let ``a path that consumed no component is EEXIST on both kernels`` () : unit =
        // Measured: `mkdir("/")`, `mkdir(".")`, `mkdir("d/.")` and `mkdir("d/..")`
        // are all EEXIST on both. Unlike a creating `open`, which diverges here
        // (Darwin EEXIST, Linux EISDIR), and unlike `rmdir`, which owes all three
        // navigations *different* errnos. `TestVirtualFileSystemAgainstHost`
        // cannot carry the "/" row: it prefixes every path with a temporary
        // directory, so the kernel never sees a path with no components.
        for rules in [ linux ; darwin ] do
            for candidate in [ "/" ; "." ; "d/." ; "d/.." ; "/." ] do
                verdict rules CallerPrivilege.Unprivileged tree candidate
                |> shouldEqual (MkDirVerdict.Refuse UnixError.EEXIST)

    [<Test>]
    let ``an existing final name is EEXIST whatever it is`` () : unit =
        // Measured on both, with no trailing separator: a directory, a file, a
        // link to either, a dangling link and a cyclic link are all EEXIST.
        // `mkdir` never dereferences the name it is about to bind.
        for rules in [ linux ; darwin ] do
            for candidate in [ "d" ; "f" ; "lf" ; "ld" ; "dang" ; "cyc" ] do
                verdict rules CallerPrivilege.Unprivileged tree candidate
                |> shouldEqual (MkDirVerdict.Refuse UnixError.EEXIST)

    [<Test>]
    let ``a trailing separator reaches past the final component only on Darwin`` () : unit =
        // The divergence, and the reason `MkDirRules.TrailingSeparator` exists.
        // Linux's creating lookup never dereferences the last component, so every
        // one of these is EEXIST there.
        for candidate in [ "f/" ; "lf/" ; "ld/" ; "dang/" ; "cyc/" ] do
            verdict linux CallerPrivilege.Unprivileged tree candidate
            |> shouldEqual (MkDirVerdict.Refuse UnixError.EEXIST)

        // Darwin resolves it as a lookup would, so each row fails — or succeeds —
        // for its own reason. Measured individually rather than as a group: "f/"
        // and "lf/" are ENOTDIR, "ld/" lands on the directory the link names and
        // is EEXIST, and "cyc/" is ELOOP.
        verdict darwin CallerPrivilege.Unprivileged tree "f/"
        |> shouldEqual (MkDirVerdict.Refuse UnixError.ENOTDIR)

        verdict darwin CallerPrivilege.Unprivileged tree "lf/"
        |> shouldEqual (MkDirVerdict.Refuse UnixError.ENOTDIR)

        verdict darwin CallerPrivilege.Unprivileged tree "ld/"
        |> shouldEqual (MkDirVerdict.Refuse UnixError.EEXIST)

        verdict darwin CallerPrivilege.Unprivileged tree "cyc/"
        |> shouldEqual (MkDirVerdict.Refuse UnixError.ELOOP)

        // And the destructive one: measured on macOS, `mkdir("dang/")` creates
        // the link's *target*. The name bound is "nx", not "dang" — which is
        // what no host-side comparison can see, since a kernel reports only that
        // it succeeded.
        verdict darwin CallerPrivilege.Unprivileged tree "dang/"
        |> bound
        |> shouldEqual "nx"

    [<Test>]
    let ``a free name creates, with or without a trailing separator`` () : unit =
        // Measured on both: `mkdir("nx")` and `mkdir("nx/")` alike succeed. This
        // is the one place `mkdir` and a creating `open` disagree about a
        // resolution of the same shape — `open` owes a free name that demands a
        // directory ENOENT on Darwin.
        for rules in [ linux ; darwin ] do
            verdict rules CallerPrivilege.Unprivileged tree "new"
            |> bound
            |> shouldEqual "new"

            verdict rules CallerPrivilege.Unprivileged tree "new/"
            |> bound
            |> shouldEqual "new"

            verdict rules CallerPrivilege.Unprivileged tree "new//"
            |> bound
            |> shouldEqual "new"

    [<Test>]
    let ``binding a name needs both write and search on the holding directory`` () : unit =
        // Measured at uid 1000 on both: a 0o555 parent and a 0o666 parent are
        // each EACCES, while 0o300 — the bare pair — succeeds.
        for rules in [ linux ; darwin ] do
            for bits in [ 0o555 ; 0o666 ; 0o111 ; 0o644 ] do
                verdict rules CallerPrivilege.Unprivileged (treeWith (mode bits)) "locked/new"
                |> shouldEqual (MkDirVerdict.Refuse UnixError.EACCES)

            for bits in [ 0o300 ; 0o333 ; 0o755 ] do
                verdict rules CallerPrivilege.Unprivileged (treeWith (mode bits)) "locked/new"
                |> bound
                |> shouldEqual "new"

    [<Test>]
    let ``the holding directory's search bit is needed to look the name up at all`` () : unit =
        // Measured on both, and it is the *search* bit alone that decides
        // whether the lookup happens: with an existing child, a 0o666 parent and
        // a 0o200 parent are EACCES while a 0o100 parent — which cannot be
        // written — is EEXIST. So this check sits above the EEXIST arm, and the
        // write check sits below it.
        for rules in [ linux ; darwin ] do
            for bits in [ 0o666 ; 0o200 ; 0o000 ; 0o644 ] do
                verdict rules CallerPrivilege.Unprivileged (treeWith (mode bits)) "locked/kid"
                |> shouldEqual (MkDirVerdict.Refuse UnixError.EACCES)

            for bits in [ 0o100 ; 0o500 ; 0o555 ; 0o300 ] do
                verdict rules CallerPrivilege.Unprivileged (treeWith (mode bits)) "locked/kid"
                |> shouldEqual (MkDirVerdict.Refuse UnixError.EEXIST)

        // Root looks anything up.
        for rules in [ linux ; darwin ] do
            verdict rules CallerPrivilege.Privileged (treeWith (mode 0o000)) "locked/kid"
            |> shouldEqual (MkDirVerdict.Refuse UnixError.EEXIST)

    [<Test>]
    let ``an existing name beats the permission rule`` () : unit =
        // Measured on both: `mkdir` of a name that already exists inside a
        // directory the caller cannot write is EEXIST, not EACCES. This ordering
        // is the only thing that distinguishes the two checks, since a row that
        // trips just one cannot say which came first.
        let locked = treeWith (mode 0o555)

        let locked =
            match VirtualFileSystem.createDirectory (InodeNumber 1L) (name "unused") (mode 0o755) buildTime locked with
            | Ok (_, vfs) -> vfs
            | Error error -> failwith $"could not extend the tree: %O{error}"

        // "locked" itself is an existing name inside the (writable) root, so it
        // exercises the ordering without needing an entry inside the unwritable
        // directory — which the builder could not create anyway.
        for rules in [ linux ; darwin ] do
            verdict rules CallerPrivilege.Unprivileged locked "locked"
            |> shouldEqual (MkDirVerdict.Refuse UnixError.EEXIST)

    [<Test>]
    let ``root bypasses the permission rule but not the others`` () : unit =
        // Measured on Linux as uid 0: `mkdir` into a 0o555 directory succeeds.
        // The refusals above it do not care about privilege.
        for rules in [ linux ; darwin ] do
            verdict rules CallerPrivilege.Privileged (treeWith (mode 0o555)) "locked/new"
            |> bound
            |> shouldEqual "new"

            verdict rules CallerPrivilege.Privileged tree "d"
            |> shouldEqual (MkDirVerdict.Refuse UnixError.EEXIST)

            verdict rules CallerPrivilege.Privileged tree "/"
            |> shouldEqual (MkDirVerdict.Refuse UnixError.EEXIST)

    [<Test>]
    let ``the created mode is masked by the platform and then by the umask`` () : unit =
        // Measured at umask 022, unprivileged, in a parent *without* S_ISGID.
        // Linux keeps the sticky bit and drops both set-ID bits (`vfs_mkdir`
        // masks with S_IRWXUGO|S_ISVTX); Darwin drops all three. That Linux row
        // is what makes this mask its own fact rather than `open`'s, which keeps
        // all twelve bits there.
        let umask = mode 0o022
        let plainParent = mode 0o755

        for mkMode, expected in
            [
                0o777, 0o755
                0o7777, 0o1755
                0o1777, 0o1755
                0o2777, 0o755
                0o4777, 0o755
                0o10777, 0o755
                0o666, 0o644
                0o000, 0o000
            ] do
            MkDirRules.createdPermissions linux plainParent umask mkMode
            |> shouldEqual (mode expected)

        for mkMode, expected in
            [
                0o777, 0o755
                0o7777, 0o755
                0o1777, 0o755
                0o2777, 0o755
                0o4777, 0o755
                0o10777, 0o755
                0o666, 0o644
                0o000, 0o000
            ] do
            MkDirRules.createdPermissions darwin plainParent umask mkMode
            |> shouldEqual (mode expected)

        // A umask of 0 masks nothing, so the platform mask is visible alone.
        MkDirRules.createdPermissions linux plainParent (mode 0o000) 0o7777
        |> shouldEqual (mode 0o1777)

        MkDirRules.createdPermissions darwin plainParent (mode 0o000) 0o7777
        |> shouldEqual (mode 0o777)

    [<Test>]
    let ``set-group-ID is inherited from the parent only on Linux`` () : unit =
        // Measured with a parent chmod'ed to 0o2777 *and read back at 0o2777*
        // first, since a non-root chmod silently drops S_ISGID when the caller is
        // not in the directory's group — which reads exactly like a platform
        // that does not support the bit.
        let umask = mode 0o022
        let setGidParent = mode 0o2777
        let plainParent = mode 0o777

        for mkMode, expected in
            [
                // The OR happens *after* both masks: 0o7777 is masked down to
                // 0o1755 and then regains the bit, giving 0o3755.
                0o777, 0o2755
                0o7777, 0o3755
                0o1777, 0o3755
                0o2777, 0o2755
                0o4777, 0o2755
            ] do
            MkDirRules.createdPermissions linux setGidParent umask mkMode
            |> shouldEqual (mode expected)

        // Darwin inherits nothing: every mode gives the same answer it gives
        // under a plain parent.
        for mkMode in [ 0o777 ; 0o7777 ; 0o1777 ; 0o2777 ; 0o4777 ] do
            MkDirRules.createdPermissions darwin setGidParent umask mkMode
            |> shouldEqual (MkDirRules.createdPermissions darwin plainParent umask mkMode)

        // ...and on Linux the bit comes from the *parent*, not from the mode: a
        // parent without it leaves 0o2777 masked away to 0o755.
        MkDirRules.createdPermissions linux plainParent umask 0o2777
        |> shouldEqual (mode 0o755)

/// The directory *search* bit, which the walk consults before it consumes any
/// component. Every row is measured against real `lstat(2)` on macOS 25.6/APFS
/// at uid 501 and Linux 6.x arm64 at uid 1000 — the two kernels agree on all of
/// them, so nothing here is flavour-dependent.
///
/// `TestVirtualFileSystemAgainstHost` compares the same rule against whichever
/// kernel it runs on. These are the rows that fixture cannot carry: a mode whose
/// *read* bit is clear (its creating-open comparison would then fail for a
/// reason it does not model), and the privileged bypass, whose uid the suite
/// does not choose.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestWalkSearchPermission =

    let private name (s : string) : FileName = FileName.parseOrFail "test" s

    let private path (s : string) : UnixPath = UnixPath.parseOrFail "test" s

    let private target (s : string) : SymlinkTarget = SymlinkTarget.parseOrFail "test" s

    let private buildTime : UnixTimestamp =
        UnixTimestamp.createOrFail "test" 1_700_000_000L 0

    let private limits : PathLimits =
        SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.linuxX64

    let private mode (raw : int) : PermissionBits = PermissionBits.parseOrFail "test" raw

    /// A root holding `p` at the given mode — containing a subdirectory, a file
    /// and a cyclic symlink — plus, outside it, a link *into* `p` and a link to
    /// `p` itself.
    let private treeWith (bits : PermissionBits) : VirtualFileSystem =
        let vfs = VirtualFileSystem.empty buildTime
        let root = VirtualFileSystem.root vfs

        let dir (parent : InodeNumber) (n : string) (bits : PermissionBits) (vfs : VirtualFileSystem) =
            match VirtualFileSystem.createDirectory parent (name n) bits buildTime vfs with
            | Ok (inode, vfs) -> inode, vfs
            | Error error -> failwith $"could not build the tree: %O{error}"

        let p, vfs = dir root "p" bits vfs

        let _, vfs = dir p "kid" PermissionBits.defaultForDirectory vfs

        let vfs =
            match
                VirtualFileSystem.createFile
                    p
                    (name "f")
                    PermissionBits.defaultForRegularFile
                    buildTime
                    ImmutableArray<byte>.Empty
                    vfs
            with
            | Ok (_, vfs) -> vfs
            | Error error -> failwith $"could not build the tree: %O{error}"

        let vfs =
            match VirtualFileSystem.createSymlink p (name "cyc") buildTime (target "cyc") vfs with
            | Ok (_, vfs) -> vfs
            | Error error -> failwith $"could not build the tree: %O{error}"

        match VirtualFileSystem.createSymlink root (name "lp") buildTime (target "p") vfs with
        | Ok (_, vfs) -> vfs
        | Error error -> failwith $"could not build the tree: %O{error}"

    let private resolveAs
        (privilege : CallerPrivilege)
        (bits : PermissionBits)
        (candidate : string)
        : Result<ResolvedTarget, UnixError>
        =
        let vfs = treeWith bits

        VirtualFileSystem.resolve
            limits
            privilege
            (VirtualFileSystem.root vfs)
            SymlinkPolicy.Follow
            (path candidate)
            vfs

    let private refuses (bits : int) (candidate : string) : unit =
        match resolveAs CallerPrivilege.Unprivileged (mode bits) candidate with
        | Error UnixError.EACCES -> ()
        | other ->
            failwith $"resolving \"%s{candidate}\" under a 0o%04o{bits} directory should be EACCES, got %A{other}"

    let private resolves (bits : int) (candidate : string) : unit =
        match resolveAs CallerPrivilege.Unprivileged (mode bits) candidate with
        | Ok _ -> ()
        | other -> failwith $"resolving \"%s{candidate}\" under a 0o%04o{bits} directory should succeed, got %A{other}"

    [<Test>]
    let ``search denial beats every other reason the walk can refuse`` () : unit =
        // Each of these earns a *different* errno under a searchable directory —
        // the row below this one says which — so this is a precedence claim
        // rather than four restatements of one fact. The check sits above them
        // all because a kernel checks it as it steps into the directory, before
        // it has looked at the component at all.
        refuses 0o666 "/p/kid" // would be Ok
        refuses 0o666 "/p/nx" // would be ENOENT
        refuses 0o666 ("/p/" + String.replicate 300 "a") // would be ENAMETOOLONG
        refuses 0o666 "/p/cyc" // would be ELOOP
        refuses 0o666 "/p/f/x" // would be ENOTDIR

    [<Test>]
    let ``and those errnos are what the same paths earn when it is searchable`` () : unit =
        // The control for the row above: without it, "search denial beats X"
        // could pass against an implementation that refused everything.
        resolves 0o755 "/p/kid"

        resolveAs CallerPrivilege.Unprivileged (mode 0o755) "/p/nx"
        |> shouldEqual (Ok (ResolvedTarget.Entry (InodeNumber 2L, name "nx", None)))

        resolveAs CallerPrivilege.Unprivileged (mode 0o755) ("/p/" + String.replicate 300 "a")
        |> shouldEqual (Error UnixError.ENAMETOOLONG)

        resolveAs CallerPrivilege.Unprivileged (mode 0o755) "/p/cyc"
        |> shouldEqual (Error UnixError.ELOOP)

        resolveAs CallerPrivilege.Unprivileged (mode 0o755) "/p/f/x"
        |> shouldEqual (Error UnixError.ENOTDIR)

    [<Test>]
    let ``"." and ".." are lookups too`` () : unit =
        // Measured: `lstat("p/.")` and `lstat("p/..")` are EACCES exactly as
        // `lstat("p/kid")` is. So the check belongs above the dispatch on which
        // kind of component was consumed, not inside the name-lookup arm.
        refuses 0o666 "/p/."
        refuses 0o666 "/p/.."

        resolves 0o755 "/p/."
        resolves 0o755 "/p/.."

    [<Test>]
    let ``the object being named needs nothing`` () : unit =
        // Measured, and the negative half of the rule: `lstat("p")` succeeds on
        // an unsearchable `p`, because this walk never looks inside it.
        resolves 0o666 "/p"
        resolves 0o000 "/p"

        // ...including with a trailing separator, which is one more place where
        // "p/" is not "p/." — measured, `lstat("p/")` succeeds where
        // `lstat("p/.")` is EACCES.
        resolves 0o666 "/p/"
        refuses 0o666 "/p/."

    [<Test>]
    let ``a component spliced from a symlink target is looked up like any other`` () : unit =
        // "lp" is a link to "p", so this resolves "kid" *inside* p having got
        // there by splicing. A check that ran only on components the guest wrote
        // would let this through.
        refuses 0o666 "/lp/kid"
        resolves 0o755 "/lp/kid"

    [<Test>]
    let ``only the owner triple decides`` () : unit =
        // Measured on both kernels against a directory owned by the calling uid.
        // An ordinary corpus cannot show this: 0o666 and 0o755 have the same
        // execute bits in all three classes, so `bits &&& 0o111 <> 0` and
        // `bits &&& 0o001 <> 0` both match every row above.
        refuses 0o677 "/p/kid" // group and other may search; the owner may not
        resolves 0o100 "/p/kid" // the owner may search; nobody else may
        resolves 0o300 "/p/kid"
        refuses 0o477 "/p/kid"

    [<Test>]
    let ``root searches anything`` () : unit =
        // Measured as uid 0 on Linux: `lstat` inside a 0o000 directory succeeds.
        for candidate in [ "/p/kid" ; "/p/." ; "/p/.." ; "/lp/kid" ] do
            match resolveAs CallerPrivilege.Privileged (mode 0o000) candidate with
            | Ok _ -> ()
            | other -> failwith $"root resolving \"%s{candidate}\" should succeed, got %A{other}"

        // ...but privilege does not invent entries that are not there.
        resolveAs CallerPrivilege.Privileged (mode 0o000) "/p/f/x"
        |> shouldEqual (Error UnixError.ENOTDIR)
