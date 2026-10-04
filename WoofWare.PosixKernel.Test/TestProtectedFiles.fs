namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// Linux's `fs.protected_symlinks`, `fs.protected_regular` and
/// `fs.protected_fifos`: which links a walk may follow, and which existing
/// files an `O_CREAT` open may land on, in a sticky directory.
///
/// The rows come from `docs/plans/2026-08-23-posix-kernel-extraction/protected-sysctls.c`,
/// run on Linux 6.18.5 (aarch64, root in the container, ext4 and tmpfs) and on
/// Darwin 27.0 at uid 501; its output is beside it, and is embedded here and
/// replayed row by row on filesystems built as the probe built them. Its
/// sweeps compared the kernel with the prediction `followRefused` and
/// `creationRefused` transcribe, which the rules are held to over every mode.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestProtectedFiles =

    let private context : string = "TestProtectedFiles"

    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L

    let private uid (raw : uint32) : UserId = UserId.parseOrFail context raw
    let private gid (raw : uint32) : GroupId = GroupId.parseOrFail context raw

    /// Every inode the probe made, and every caller it became, was in group
    /// 100.
    let private group : uint32 = 100u

    let private owner (user : uint32) : InodeOwner =
        {
            User = uid user
            Group = gid group
        }

    let private caller (user : uint32) : Credentials =
        Credentials.ofIds (uid user) (gid group) [ gid group ]

    let private mode (bits : int) : PermissionBits = PermissionBits.parseOrFail context bits

    let private name (text : string) : DirectoryEntryName =
        DirectoryEntryName.parseOrFail context text

    let private path (p : string) : UnixPath = UnixPath.parseOrFail context p

    let private bytes (p : string) : PathArgumentBytes =
        PathArg.ofBytes (Text.Encoding.ASCII.GetBytes p)

    let private ok (result : Result<'a, 'e>) : 'a =
        match result with
        | Ok value -> value
        | Error error -> failwith $"expected Ok, got %A{error}"

    let private allSymlinkProtections : SymlinkProtection list =
        [ SymlinkProtection.Off ; SymlinkProtection.InWorldWritableStickyDirectories ]

    let private allCreationProtections : CreationProtection list =
        [
            CreationProtection.Off
            CreationProtection.InWorldWritableStickyDirectories
            CreationProtection.InGroupOrWorldWritableStickyDirectories
        ]

    /// The sysctl's value for each case.
    let private symlinkKnob (protection : SymlinkProtection) : int =
        match protection with
        | SymlinkProtection.Off -> 0
        | SymlinkProtection.InWorldWritableStickyDirectories -> 1

    let private creationKnob (protection : CreationProtection) : int =
        match protection with
        | CreationProtection.Off -> 0
        | CreationProtection.InWorldWritableStickyDirectories -> 1
        | CreationProtection.InGroupOrWorldWritableStickyDirectories -> 2

    // ------------------------------------------------------------- the rule

    /// `protected-sysctls.c`'s `predict_s`, which no measured row contradicted:
    /// a final link is refused when the knob is 1, its directory is sticky and
    /// world-writable, and its owner is neither the follower nor the
    /// directory's owner. Root is not exempt.
    let private followRefused
        (knob : int)
        (bits : int)
        (directory : uint32)
        (link : uint32)
        (follower : uint32)
        : bool
        =
        knob = 1 && bits &&& 0o1002 = 0o1002 && link <> follower && link <> directory

    /// `protected-sysctls.c`'s `sticky_create_refused`, and its rule for the
    /// kinds no knob governs.
    let private creationRefused
        (knob : int option)
        (bits : int)
        (directory : uint32)
        (existing : uint32)
        (creator : uint32)
        : bool
        =
        if bits &&& 0o1000 = 0 || existing = directory || existing = creator then
            false
        else

        match knob with
        | None -> bits &&& 0o002 <> 0
        | Some knob -> (knob >= 1 && bits &&& 0o002 <> 0) || (knob >= 2 && bits &&& 0o020 <> 0)

    let private users : uint32 list = [ 0u ; 1000u ; 1001u ; 1002u ]

    [<Test>]
    let ``refusesToFollow is the probe's prediction, over every mode, owner and follower`` () =
        for protection in allSymlinkProtections do
            for bits in 0..0o7777 do
                for d in users do
                    for l in users do
                        for f in users do
                            let actual =
                                ProtectedFiles.refusesToFollow protection (caller f) (owner d) (mode bits) (owner l)

                            if actual <> followRefused (symlinkKnob protection) bits d l f then
                                failwith
                                    $"%A{protection}, directory 0o%04o{bits} owned by %d{d}, link owned by %d{l}, follower %d{f}: refusesToFollow said %b{actual}"

    [<Test>]
    let ``refusesToFollow reads the effective user alone`` () =
        // Real 1000, effective 1001, following 1000's link in root's 01777
        // directory: refused, for the effective user owns nothing.
        let credentials =
            { caller 1001u with
                RealUser = uid 1000u
            }

        ProtectedFiles.refusesToFollow
            SymlinkProtection.InWorldWritableStickyDirectories
            credentials
            (owner 0u)
            (mode 0o1777)
            (owner 1000u)
        |> shouldEqual true

        ProtectedFiles.refusesToFollow
            SymlinkProtection.InWorldWritableStickyDirectories
            (Credentials.realIdsAsEffective credentials)
            (owner 0u)
            (mode 0o1777)
            (owner 1000u)
        |> shouldEqual false

    /// One inode of each kind, owned by `by`, read off a hand-built filesystem.
    let private inodesOwnedBy (by : InodeOwner) : (string * Inode) list =
        let vfs = VirtualFileSystem.empty epoch by
        let root = VirtualFileSystem.root vfs

        let vfs =
            VirtualFileSystem.createFile root (name "f") (mode 0o666) by epoch ImmutableArray.Empty vfs
            |> ok
            |> snd

        let vfs =
            VirtualFileSystem.createSymlink
                root
                (name "l")
                SymlinkModes.linux
                by
                epoch
                (SymlinkTarget.parseOrFail context "f")
                vfs
            |> ok
            |> snd

        let vfs =
            VirtualFileSystem.createDirectory root (name "d") (mode 0o777) by epoch vfs
            |> ok
            |> snd

        match VirtualFileSystem.tryGetContent root vfs with
        | Some (InodeContent.Directory content) ->
            [ "f" ; "l" ; "d" ]
            |> List.map (fun entry ->
                match VirtualFileSystem.tryGet content.Entries.[name entry] vfs with
                | Some inode -> entry, inode
                | None -> failwith $"%s{entry} is missing"
            )
        | _ -> failwith "the root is not a directory"

    [<Test>]
    let ``refusesCreatingOpen is the probe's prediction, over every mode, kind, owner and creator`` () =
        let inodes = users |> List.map (fun u -> u, inodesOwnedBy (owner u)) |> Map.ofList

        for regular in allCreationProtections do
            for fifos in allCreationProtections do
                let protection =
                    { ProtectedFiles.off with
                        RegularFiles = regular
                        Fifos = fifos
                        Symlinks = SymlinkProtection.InWorldWritableStickyDirectories
                    }

                for bits in 0..0o7777 do
                    for d in users do
                        for i in users do
                            for c in users do
                                for kind, inode in inodes.[i] do
                                    let knob =
                                        match kind with
                                        | "f" -> Some (creationKnob regular)
                                        | _ -> None

                                    let actual =
                                        ProtectedFiles.refusesCreatingOpen
                                            protection
                                            (caller c)
                                            (owner d)
                                            (mode bits)
                                            inode

                                    if actual <> creationRefused knob bits d i c then
                                        failwith
                                            $"%A{protection}, %s{kind} owned by %d{i} in a directory 0o%04o{bits} owned by %d{d}, creator %d{c}: refusesCreatingOpen said %b{actual}"

    [<Test>]
    let ``ProtectedFiles.off is every sysctl at 0`` () =
        ProtectedFiles.off.Symlinks |> symlinkKnob |> shouldEqual 0
        ProtectedFiles.off.RegularFiles |> creationKnob |> shouldEqual 0
        ProtectedFiles.off.Fifos |> creationKnob |> shouldEqual 0

    // ------------------------------------------------------------- the probe's output

    let private resource (logicalName : string) : string list =
        let assembly = Assembly.GetExecutingAssembly ()
        let resourceName = $"WoofWare.PosixKernel.Test.protectedSysctls.%s{logicalName}.txt"

        use stream =
            match assembly.GetManifestResourceStream resourceName with
            | null -> failwith $"embedded resource %s{resourceName} not found"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList

    let private linuxOutputs : string list = [ "linuxExt4" ; "linuxTmpfs" ]

    /// A probe line's kind, and its `key=value` fields.
    let private fields (line : string) : string * Map<string, string> =
        let parts = line.Split '\t'

        let pairs =
            parts
            |> Array.skip 1
            |> Array.choose (fun part ->
                match part.IndexOf '=' with
                | -1 -> None
                | i -> Some (part.Substring (0, i), part.Substring (i + 1))
            )
            |> Map.ofArray

        parts.[0], pairs

    let private rowsOf (kind : string) (lines : string list) : Map<string, string> list =
        lines
        |> List.choose (fun line ->
            let k, f = fields line
            if k = kind then Some f else None
        )

    [<Test>]
    let ``every sweep of the probe matched its prediction`` () =
        for output in linuxOutputs do
            let lines = resource output

            lines |> List.exists (fun line -> line.Contains "MISMATCH") |> shouldEqual false

            lines
            |> List.filter (fun line -> line.StartsWith "TOTAL")
            |> shouldEqual [ "TOTAL\tmismatches=0" ]

    [<Test>]
    let ``the kernel's default is every sysctl 0, and each admits exactly the values its cases name`` () =
        for output in linuxOutputs do
            let lines = resource output

            let defaults = rowsOf "DEFAULT" lines |> List.collect Map.toList |> Map.ofList

            defaults
            |> shouldEqual (
                Map.ofList
                    [
                        "protected_symlinks", "0"
                        "protected_regular", "0"
                        "protected_fifos", "0"
                        "protected_hardlinks", "0"
                    ]
            )

            let admitted (knob : string) : int list =
                lines
                |> List.find (fun line -> line.StartsWith $"ADMITS\t%s{knob}\t")
                |> fields
                |> snd
                |> Map.toList
                |> List.filter (fun (_, answer) -> answer = "ok")
                |> List.map (fst >> int)
                |> List.sort

            admitted "protected_symlinks"
            |> shouldEqual (allSymlinkProtections |> List.map symlinkKnob)

            admitted "protected_regular"
            |> shouldEqual (allCreationProtections |> List.map creationKnob)

            admitted "protected_fifos"
            |> shouldEqual (allCreationProtections |> List.map creationKnob)

    [<Test>]
    let ``a FIFO is screened exactly as a regular file is, value for value`` () =
        // No inode this library models is a FIFO, so `ProtectedFiles.Fifos`
        // decides nothing; this holds its documented meaning to the probe.
        let shared =
            [
                "open"
                "creat"
                "creat-rdwr"
                "excl"
                "via-link"
                "link-nofollow"
                "directory"
            ]

        for output in linuxOutputs do
            let lines = resource output

            let key (row : Map<string, string>) =
                row.["knob"], row.["dir"], row.["dirowner"], row.["owner"], row.["caller"]

            let regular =
                rowsOf "REGULAR" lines |> List.map (fun row -> key row, row) |> Map.ofList

            let fifos = rowsOf "FIFOS" lines

            fifos |> List.length |> shouldEqual 1152

            for fifo in fifos do
                let regularRow = regular.[key fifo]

                for field in shared do
                    (key fifo, field, fifo.[field])
                    |> shouldEqual (key fifo, field, regularRow.[field])

    // ------------------------------------------------------------- hand-built filesystems

    let private linux : SimulatedUnixPlatform = SimulatedUnixPlatform.linuxX64

    let private linuxLimit : int =
        PathLimits.maxSymlinkTraversals (SimulatedUnixPlatform.pathLimits linux)

    /// The inode `p` names, walked as root without following a final link.
    let private inodeAt (vfs : VirtualFileSystem) (p : string) : InodeNumber =
        match
            PathWalk.resolveExisting
                (SimulatedUnixPlatform.pathLimits linux)
                Owners.root
                SymlinkProtection.Off
                (VirtualFileSystem.root vfs)
                SymlinkPolicy.NoFollowFinal
                (path p)
                vfs
        with
        | Ok inode -> inode
        | Error error -> failwith $"%s{p} does not resolve in the hand-built filesystem: %O{error}"

    let private parentAndName (p : string) : string * string =
        let slash = p.LastIndexOf '/'
        (if slash = 0 then "/" else p.Substring (0, slash)), p.Substring (slash + 1)

    let private directory (p : string) (by : uint32) (bits : int) (vfs : VirtualFileSystem) : VirtualFileSystem =
        let parent, child = parentAndName p

        VirtualFileSystem.createDirectory (inodeAt vfs parent) (name child) (mode bits) (owner by) epoch vfs
        |> ok
        |> snd

    let private file (p : string) (by : uint32) (bits : int) (vfs : VirtualFileSystem) : VirtualFileSystem =
        let parent, child = parentAndName p

        VirtualFileSystem.createFile
            (inodeAt vfs parent)
            (name child)
            (mode bits)
            (owner by)
            epoch
            (ImmutableArray.CreateRange "abcd"B)
            vfs
        |> ok
        |> snd

    let private symlink (p : string) (by : uint32) (target : string) (vfs : VirtualFileSystem) : VirtualFileSystem =
        let parent, child = parentAndName p

        VirtualFileSystem.createSymlink
            (inodeAt vfs parent)
            (name child)
            SymlinkModes.linux
            (owner by)
            epoch
            (SymlinkTarget.parseOrFail context target)
            vfs
        |> ok
        |> snd

    /// A process on `platform` with `credentials` and the sysctls
    /// `protection`, in the root of `vfs`.
    let private systemOn
        (platform : SimulatedUnixPlatform)
        (protection : ProtectedFiles)
        (credentials : Credentials)
        (vfs : VirtualFileSystem)
        : UnixSystem<int, string>
        =
        let system : UnixSystem<int, string> =
            UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.withCredentials context credentials
            |> UnixBootImage.withProtectedFiles context protection
            |> UnixBootImage.boot

        { system with
            Machine =
                { system.Machine with
                    FileSystem = vfs
                }
            Process =
                { system.Process with
                    CurrentDirectoryInode = VirtualFileSystem.root vfs
                }
        }

    let private answerText (answer : SyscallAnswer) : string =
        match answer with
        | SyscallAnswer.Completed _ -> "ok"
        | SyscallAnswer.Failed error -> $"%A{error}"

    let private statText (policy : SymlinkPolicy) (p : string) (system : UnixSystem<int, string>) : string =
        match UnixPathResolution.stat policy (PathArg.ofPath (path p)) system |> ok with
        | FileStatusAnswer.Reported _ -> "ok"
        | FileStatusAnswer.Failed error -> $"%A{error}"

    let private readOnly : OpenFlags =
        {
            Access = FileAccessMode.ReadOnly
            Create = false
            Exclusive = false
            Truncate = false
            NoFollow = false
            CloseOnExec = false
            Synchronous = false
            DataSynchronous = false
            Directory = false
        }

    let private openText
        (flags : OpenFlags)
        (p : string)
        (system : UnixSystem<int, string>)
        : string * UnixSystem<int, string>
        =
        match OpenFlagWords.openPath flags (PathArg.ofPath (path p)) 0o644 system with
        | Ok (answer, after) -> answerText answer, after
        | Error refusal -> failwith $"open(%s{p}) was refused: %A{refusal}"

    let private exists (p : string) (system : UnixSystem<int, string>) : bool =
        match
            PathWalk.resolveExisting
                (SimulatedUnixPlatform.pathLimits system.Machine.UnixPlatform)
                Owners.root
                SymlinkProtection.Off
                (VirtualFileSystem.root system.Machine.FileSystem)
                SymlinkPolicy.NoFollowFinal
                (path p)
                system.Machine.FileSystem
        with
        | Ok _ -> true
        | Error (PathFailure.Errno UnixError.ENOENT) -> false
        | Error error -> failwith $"%s{p}: %O{error}"

    let private contentsOf (p : string) (system : UnixSystem<int, string>) : int =
        match VirtualFileSystem.tryGetContent (inodeAt system.Machine.FileSystem p) system.Machine.FileSystem with
        | Some (InodeContent.RegularFile (contents, _)) -> contents.Length
        | other -> failwith $"%s{p} is not a regular file: %A{other}"

    /// The probe's directories outside the sticky one: `/t` holding a file and
    /// a directory holding `x`, `/c` in which anyone may create, and `/v` for
    /// root's links into the directory under test.
    let private scaffold : VirtualFileSystem =
        VirtualFileSystem.empty epoch (owner 0u)
        |> directory "/t" 0u 0o755
        |> file "/t/file" 0u 0o644
        |> directory "/t/dir" 0u 0o755
        |> file "/t/dir/x" 0u 0o644
        |> directory "/c" 0u 0o777
        |> directory "/v" 0u 0o755

    let private octal (text : string) : int = Convert.ToInt32 (text, 8)

    // ------------------------------------------------------------- protected_symlinks, replayed

    /// The answer each `SYMLINKS` operation gives, under `knob`, in a directory
    /// `/s` (mode `bits`, owner `d`) holding links owned by `l`, to `f`.
    let private symlinkAnswers (knob : int) (bits : int) (d : uint32) (l : uint32) (f : uint32) : Map<string, string> =
        let vfs =
            scaffold
            |> directory "/s" d bits
            |> symlink "/s/lf" l "/t/file"
            |> symlink "/s/ld" l "/t/dir"
            |> symlink "/s/ldang" l "/c/dangle"
            |> symlink "/s/lcyc" l "lcyc"
            |> symlink "/v/via" 0u "/s/lf"

        let protection =
            { ProtectedFiles.off with
                Symlinks =
                    if knob = 1 then
                        SymlinkProtection.InWorldWritableStickyDirectories
                    else
                        SymlinkProtection.Off
            }

        let system = systemOn linux protection (caller f) vfs
        let stat = fun p -> statText SymlinkPolicy.Follow p system

        let creat, afterCreat =
            openText
                { readOnly with
                    Access = FileAccessMode.WriteOnly
                    Create = true
                }
                "/s/ldang"
                system

        let accessText (call : Result<SyscallAnswer, AccessRefusal>) : string = call |> ok |> answerText

        let readlinkText =
            match
                UnixNamespace.readlink (PathArg.ofPath (path "/s/lf")) UserBuffer.Mapped 4096 system
                |> ok
            with
            | ReadLinkAnswer.Reported _ -> "ok"
            | ReadLinkAnswer.Failed error -> $"%A{error}"

        [
            "stat", stat "/s/lf"
            "lstat", statText SymlinkPolicy.NoFollowFinal "/s/lf" system
            "open", fst (openText readOnly "/s/lf" system)
            "open-nofollow",
            fst (
                openText
                    { readOnly with
                        NoFollow = true
                    }
                    "/s/lf"
                    system
            )
            "readlink", readlinkText
            "access", accessText (UnixPathResolution.access (bytes "/s/lf") 4 system)
            "access-nofollow", accessText (UnixPathResolution.faccessat -100 (bytes "/s/lf") 0 0x100 system)
            "chown",
            UnixPathResolution.chown (PathArg.ofPath (path "/s/lf")) None None system
            |> ok
            |> fst
            |> answerText
            "lchown",
            UnixPathResolution.lchown (PathArg.ofPath (path "/s/lf")) None None system
            |> ok
            |> fst
            |> answerText
            "dir-slash", stat "/s/ld/"
            "dir-dot", stat "/s/ld/."
            "dir-child", stat "/s/ld/x"
            "chdir", Answered.chdir (PathArg.ofPath (path "/s/ld")) system |> fst |> answerText
            "opendir",
            fst (
                openText
                    { readOnly with
                        Directory = true
                    }
                    "/s/ld"
                    system
            )
            "dangling-stat", stat "/s/ldang"
            "dangling-creat", creat
            "dangling-created", (if exists "/c/dangle" afterCreat then "EEXIST" else "ok")
            "cycle-stat", stat "/s/lcyc"
            "via-chain", stat "/v/via"
            "unlink",
            UnixNamespace.unlink (PathArg.ofPath (path "/s/lf")) system
            |> ok
            |> fst
            |> answerText
        ]
        |> Map.ofList

    [<Test>]
    let ``every protected_symlinks row the probe measured, end to end`` () =
        for output in linuxOutputs do
            let rows = rowsOf "SYMLINKS" (resource output)
            rows |> List.length |> shouldEqual 768

            for row in rows do
                let expected =
                    row
                    |> Map.filter (fun key _ ->
                        not (List.contains key [ "knob" ; "dir" ; "dirowner" ; "linkowner" ; "caller" ])
                    )

                let actual =
                    symlinkAnswers
                        (int row.["knob"])
                        (octal row.["dir"])
                        (uint32 row.["dirowner"])
                        (uint32 row.["linkowner"])
                        (uint32 row.["caller"])

                (row.["knob"], row.["dir"], row.["dirowner"], row.["linkowner"], row.["caller"], actual)
                |> shouldEqual (
                    row.["knob"],
                    row.["dir"],
                    row.["dirowner"],
                    row.["linkowner"],
                    row.["caller"],
                    expected
                )

    [<Test>]
    let ``access(2) screens a link with the real user, and AT_EACCESS with the effective one`` () =
        let vfs =
            scaffold |> directory "/ids" 0u 0o1777 |> symlink "/ids/lf" 1000u "/t/file"

        let protection =
            { ProtectedFiles.off with
                Symlinks = SymlinkProtection.InWorldWritableStickyDirectories
            }

        for output in linuxOutputs do
            let rows = rowsOf "IDS" (resource output)
            rows |> List.length |> shouldEqual 4

            for row in rows do
                let real = uint32 row.["real"]
                let effective = uint32 row.["effective"]

                let credentials =
                    { caller effective with
                        RealUser = uid real
                    }

                let system = systemOn linux protection credentials vfs

                let actual =
                    Map.ofList
                        [
                            "access", UnixPathResolution.access (bytes "/ids/lf") 4 system |> ok |> answerText
                            "access-eaccess",
                            UnixPathResolution.faccessat -100 (bytes "/ids/lf") 4 0x200 system
                            |> ok
                            |> answerText
                            "stat", statText SymlinkPolicy.Follow "/ids/lf" system
                        ]

                (real, effective, actual)
                |> shouldEqual (real, effective, row |> Map.remove "real" |> Map.remove "effective")

    /// `/ord/lf`, owned by 1000 in root's 01777 directory, at the end of a
    /// chain of `n` of root's links in an ordinary directory, as the probe
    /// built it.
    let private chainOf (n : int) : VirtualFileSystem * string =
        let vfs =
            scaffold |> directory "/ord" 0u 0o1777 |> symlink "/ord/lf" 1000u "/t/file"

        if n = 0 then
            vfs, "/ord/lf"
        else

        let dir = $"/chain%02d{n}"

        let vfs =
            (directory dir 0u 0o755 vfs, [ 1..n ])
            ||> List.fold (fun vfs i -> symlink $"%s{dir}/c%d{i}" 0u (if i = n then "/ord/lf" else $"c%d{i + 1}") vfs)

        vfs, $"%s{dir}/c1"

    [<Test>]
    let ``the traversal budget is spent before the screen, and a refusal deep in a chain is refused rather than guessed``
        ()
        =
        linuxLimit |> shouldEqual 40

        let protection =
            { ProtectedFiles.off with
                Symlinks = SymlinkProtection.InWorldWritableStickyDirectories
            }

        for output in linuxOutputs do
            let rows = rowsOf "ORDER" (resource output)
            rows |> List.length |> shouldEqual 27

            for row in rows do
                let n = int row.["chain"]
                let follower = uint32 row.["caller"]
                let vfs, start = chainOf n
                let system = systemOn linux protection (caller follower) vfs
                // The protected link is traversal n + 1.
                let traversal = n + 1

                if follower <> 1000u && traversal <= linuxLimit && 2 * traversal - 1 >= linuxLimit then
                    // The kernel's answer here is EACCES or ELOOP by its cache.
                    [ "EACCES" ; "ELOOP" ] |> shouldContain row.["stat"]

                    let thrown =
                        try
                            statText SymlinkPolicy.Follow start system |> Some
                        with e when e.Message.Contains "fs.protected_symlinks" ->
                            None

                    (n, thrown) |> shouldEqual (n, None)
                else
                    (n, follower, row.["caches"], statText SymlinkPolicy.Follow start system)
                    |> shouldEqual (n, follower, row.["caches"], row.["stat"])

    // ------------------------------------------------------------- protected_regular, replayed

    /// The answer each `REGULAR` operation gives, under `knob`, in a directory
    /// `/s` (mode `bits`, owner `d`) holding a regular file, a link to it and a
    /// directory, each owned by `i`, to `c`.
    let private regularAnswers (knob : int) (bits : int) (d : uint32) (i : uint32) (c : uint32) : Map<string, string> =
        let vfs =
            scaffold
            |> directory "/s" d bits
            |> file "/s/o" i 0o666
            |> symlink "/s/l" i "/s/o"
            |> directory "/s/sub" i 0o777
            |> symlink "/v/via" 0u "/s/o"

        let protection =
            { ProtectedFiles.off with
                RegularFiles = allCreationProtections |> List.find (fun p -> creationKnob p = knob)
            }

        let system = systemOn linux protection (caller c) vfs

        let creating =
            { readOnly with
                Create = true
            }

        let creatTrunc, afterTrunc =
            openText
                { creating with
                    Access = FileAccessMode.WriteOnly
                    Truncate = true
                }
                "/s/o"
                system

        [
            "open", fst (openText readOnly "/s/o" system)
            "creat", fst (openText creating "/s/o" system)
            "creat-rdwr",
            fst (
                openText
                    { creating with
                        Access = FileAccessMode.ReadWrite
                    }
                    "/s/o"
                    system
            )
            "creat-trunc", creatTrunc
            "truncated", (if contentsOf "/s/o" afterTrunc = 0 then "EEXIST" else "ok")
            "excl",
            fst (
                openText
                    { creating with
                        Exclusive = true
                    }
                    "/s/o"
                    system
            )
            "via-link", fst (openText creating "/v/via" system)
            "link-nofollow",
            fst (
                openText
                    { creating with
                        NoFollow = true
                    }
                    "/s/l"
                    system
            )
            "directory", fst (openText creating "/s/sub" system)
        ]
        |> Map.ofList

    [<Test>]
    let ``every protected_regular row the probe measured, end to end`` () =
        for output in linuxOutputs do
            let rows = rowsOf "REGULAR" (resource output)
            rows |> List.length |> shouldEqual 1152

            for row in rows do
                let expected =
                    row
                    |> Map.filter (fun key _ ->
                        not (List.contains key [ "knob" ; "dir" ; "dirowner" ; "owner" ; "caller" ])
                    )

                let actual =
                    regularAnswers
                        (int row.["knob"])
                        (octal row.["dir"])
                        (uint32 row.["dirowner"])
                        (uint32 row.["owner"])
                        (uint32 row.["caller"])

                (row.["knob"], row.["dir"], row.["dirowner"], row.["owner"], row.["caller"], actual)
                |> shouldEqual (row.["knob"], row.["dir"], row.["dirowner"], row.["owner"], row.["caller"], expected)

    // ------------------------------------------------------------- every mode, end to end

    [<Test>]
    let ``the syscalls screen exactly as the rule says, for any directory mode and owners`` () =
        let uidGen = Gen.elements [ 0u ; 1000u ; 1001u ; 1002u ; 70000u ]

        let gen =
            gen {
                // Every search bit, so that only the screen can refuse.
                let! bits = Gen.choose (0, 0o7777) |> Gen.map (fun bits -> bits ||| 0o111)
                let! symlinks = Gen.elements allSymlinkProtections
                let! regular = Gen.elements allCreationProtections
                let! d = uidGen
                let! i = uidGen
                let! c = uidGen
                return bits, symlinks, regular, d, i, c
            }

        let property (bits : int, symlinks : SymlinkProtection, regular : CreationProtection, d, i, c) =
            let vfs =
                scaffold
                |> directory "/s" d bits
                |> file "/s/o" i 0o666
                |> symlink "/s/l" i "/t/file"

            let protection =
                {
                    Symlinks = symlinks
                    RegularFiles = regular
                    Fifos = CreationProtection.Off
                }

            let system = systemOn linux protection (caller c) vfs

            let expectedStat =
                if followRefused (symlinkKnob symlinks) bits d i c then
                    "EACCES"
                else
                    "ok"

            let expectedCreat =
                if creationRefused (Some (creationKnob regular)) bits d i c then
                    "EACCES"
                else
                    "ok"

            statText SymlinkPolicy.Follow "/s/l" system = expectedStat
            && fst (
                openText
                    { readOnly with
                        Create = true
                    }
                    "/s/o"
                    system
            ) = expectedCreat

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 2000, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``no sysctl changes any answer while the caller owns every inode`` () =
        // What a client gets when every inode of its seed is the process's own,
        // which is PawPrint's default.
        for knobs in
            [
                for symlinks in allSymlinkProtections do
                    for regular in allCreationProtections do
                        yield
                            {
                                Symlinks = symlinks
                                RegularFiles = regular
                                Fifos = regular
                            }
            ] do
            for bits in [ 0o755 ; 0o1777 ; 0o1775 ; 0o1757 ; 0o777 ] do
                for u in users do
                    symlinkAnswers (symlinkKnob knobs.Symlinks) bits u u u
                    |> shouldEqual (symlinkAnswers 0 bits u u u)

                    regularAnswers (creationKnob knobs.RegularFiles) bits u u u
                    |> shouldEqual (regularAnswers 0 bits u u u)

    // ------------------------------------------------------------- Darwin

    [<Test>]
    let ``Darwin answers its measured rows: no link or entry of a sticky directory is screened`` () =
        let lines = resource "darwin"
        let darwin = SimulatedUnixPlatform.macOsArm64
        let credentials = UnixSystem.defaultCredentials SimulatedUnixFlavour.Darwin
        let me = UserId.toUInt32 credentials.EffectiveUser
        // `DIR\t<mode>\towner=...`: the mode is the one field without a key.
        let dirs =
            lines
            |> List.filter (fun line -> line.StartsWith "DIR\t")
            |> List.map (fun line -> octal (line.Split '\t').[1], snd (fields line))

        dirs |> List.length |> shouldEqual 2

        let answers =
            lines
            |> List.filter (fun line -> line.StartsWith "DARWIN\t")
            |> List.map (fun line -> line.Split '\t' |> fun parts -> parts.[1], parts.[2])

        answers |> List.length |> shouldEqual 8

        for index, (bits, dir) in List.indexed dirs do
            dir.["owner"] |> shouldEqual (string me)

            let vfs =
                scaffold
                |> directory "/s" me bits
                |> symlink "/s/l" (uint32 dir.["linkowner"]) "/t/file"
                |> file "/s/f" (uint32 dir.["fileowner"]) 0o644

            let system = systemOn darwin ProtectedFiles.off credentials vfs

            let creating =
                { readOnly with
                    Create = true
                }

            let actual =
                [
                    statText SymlinkPolicy.Follow "/s/l" system
                    fst (
                        openText
                            { creating with
                                NoFollow = true
                            }
                            "/s/l"
                            system
                    )
                    fst (openText creating "/s/f" system)
                    fst (openText readOnly "/s/f" system)
                ]

            actual
            |> shouldEqual (answers |> List.skip (4 * index) |> List.take 4 |> List.map snd)

    [<Test>]
    let ``a Darwin machine refuses every sysctl but Off`` () =
        let image : UnixBootImage<int, string> =
            UnixSystem.initial SimulatedUnixPlatform.macOsArm64 UnixSystem.pipedStandardStreams 0 (CpuId 0)

        let system = UnixBootImage.boot image

        system.Machine.ProtectedFiles |> shouldEqual ProtectedFiles.off

        UnixBootImage.withProtectedFiles "test" ProtectedFiles.off image
        |> UnixBootImage.boot
        |> shouldEqual system

        for symlinks in allSymlinkProtections do
            for regular in allCreationProtections do
                for fifos in allCreationProtections do
                    let protection =
                        {
                            Symlinks = symlinks
                            RegularFiles = regular
                            Fifos = fifos
                        }

                    if protection <> ProtectedFiles.off then
                        let thrown =
                            try
                                UnixBootImage.withProtectedFiles "test" protection image |> ignore
                                None
                            with e ->
                                Some e.Message

                        match thrown with
                        | Some message -> message |> shouldContainText "test:"
                        | None -> failwith $"%A{protection} was admitted on Darwin"

                        let forged =
                            { system with
                                Machine =
                                    { system.Machine with
                                        ProtectedFiles = protection
                                    }
                            }

                        UnixSystem.checkInvariants forged
                        |> shouldContain (
                            UnixSystemDefect.ProtectedFilesNotOfFlavour (protection, SimulatedUnixFlavour.Darwin)
                        )

    [<Test>]
    let ``a Linux machine starts with every sysctl Off and admits any setting`` () =
        let image : UnixBootImage<int, string> =
            UnixSystem.initial linux UnixSystem.pipedStandardStreams 0 (CpuId 0)

        (UnixBootImage.boot image).Machine.ProtectedFiles
        |> shouldEqual ProtectedFiles.off

        for symlinks in allSymlinkProtections do
            for regular in allCreationProtections do
                for fifos in allCreationProtections do
                    let protection =
                        {
                            Symlinks = symlinks
                            RegularFiles = regular
                            Fifos = fifos
                        }

                    let set =
                        UnixBootImage.withProtectedFiles "test" protection image |> UnixBootImage.boot

                    set.Machine.ProtectedFiles |> shouldEqual protection
                    UnixSystem.checkInvariants set |> shouldEqual []
