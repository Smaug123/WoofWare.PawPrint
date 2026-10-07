namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// Darwin's `clonefile(2)`: which errno each refusal carries and the order of
/// the checks, and what the new name gets (mode, owner, timestamps).
///
/// The rows come from `docs/plans/2026-08-23-posix-kernel-extraction/clonefile-rules.c`,
/// run on Darwin 27.0 (APFS) at uid 501, whose output is beside it. Its error
/// rows are replayed here as literals against a filesystem built as the
/// probe's was; its mode sweep is replayed exhaustively.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCloneFile =

    let private context : string = "TestCloneFile"

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 300

    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L

    let private uid (raw : uint32) : UserId = UserId.parseOrFail context raw
    let private gid (raw : uint32) : GroupId = GroupId.parseOrFail context raw

    let private owner (user : uint32) (group : uint32) : InodeOwner =
        {
            User = uid user
            Group = gid group
        }

    let private mode (bits : int) : PermissionBits = PermissionBits.parseOrFail context bits

    let private name (text : string) : DirectoryEntryName =
        DirectoryEntryName.parseOrFail context text

    let private argument (text : string) : PathArgumentBytes =
        PathArg.ofBytes (Text.Encoding.Latin1.GetBytes text)

    /// uid 501 in groups 20 and 12: the probe's caller.
    let private u501 : Credentials =
        Credentials.ofIds (uid 501u) (gid 20u) [ gid 20u ; gid 12u ]

    let private mine : InodeOwner = owner 501u 20u

    let private ok (result : Result<'a, 'e>) : 'a =
        match result with
        | Ok value -> value
        | Error error -> failwith $"expected Ok, got %A{error}"

    /// The inode `p` names, walked as root without following a final link.
    let private inodeAt (vfs : VirtualFileSystem) (p : string) : InodeNumber =
        match
            PathWalk.resolveExisting
                (SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.macOsArm64)
                Owners.root
                SymlinkProtection.Off
                (VirtualFileSystem.root vfs)
                SymlinkPolicy.NoFollowFinal
                (UnixPath.parseOrFail context p)
                vfs
        with
        | Ok inode -> inode
        | Error error -> failwith $"%s{p} does not resolve in the hand-built filesystem: %O{error}"

    let private parentAndName (p : string) : string * string =
        let slash = p.LastIndexOf '/'
        (if slash = 0 then "/" else p.Substring (0, slash)), p.Substring (slash + 1)

    let private file (p : string) (by : InodeOwner) (bits : int) (contents : string) (vfs : VirtualFileSystem) =
        let parent, child = parentAndName p

        VirtualFileSystem.createFile
            (inodeAt vfs parent)
            (name child)
            (mode bits)
            by
            epoch
            (Text.Encoding.ASCII.GetBytes contents |> ImmutableArray.CreateRange)
            vfs
        |> ok
        |> snd

    let private directory (p : string) (by : InodeOwner) (bits : int) (vfs : VirtualFileSystem) =
        let parent, child = parentAndName p

        VirtualFileSystem.createDirectory (inodeAt vfs parent) (name child) (mode bits) by epoch vfs
        |> ok
        |> snd

    let private symlink (p : string) (target : string) (vfs : VirtualFileSystem) =
        let parent, child = parentAndName p

        VirtualFileSystem.createSymlink
            (inodeAt vfs parent)
            (name child)
            SymlinkModes.linux
            mine
            epoch
            (SymlinkTarget.parseOrFail context target)
            vfs
        |> ok
        |> snd

    let private chmodded (p : string) (bits : int) (vfs : VirtualFileSystem) =
        VirtualFileSystem.setPermissions (inodeAt vfs p) (mode bits) epoch vfs

    /// The probe's error tree, every inode the caller's.
    let private tree : VirtualFileSystem =
        let vfs =
            VirtualFileSystem.empty epoch mine
            |> file "/f" mine 0o644 "hello"
            |> file "/g" mine 0o644 "other"
            |> directory "/d" mine 0o755
            |> file "/d/in" mine 0o644 "in"
            |> directory "/e" mine 0o755
            |> symlink "/lf" "f"
            |> symlink "/ld" "d"
            |> symlink "/dang" "nx"
            |> symlink "/dangd" "d/nx"
            |> symlink "/cyc" "cyc"
            |> directory "/ro" mine 0o755
            |> file "/ro/x" mine 0o666 "x"
            |> chmodded "/ro" 0o555
            |> directory "/ns" mine 0o755
            |> file "/ns/x" mine 0o644 "x"
            |> chmodded "/ns" 0o666
            |> file "/unr" mine 0o000 "secret"
            |> file "/wo" mine 0o200 "secret"

        VirtualFileSystem.hardLink (VirtualFileSystem.root vfs) (name "hf") (inodeAt vfs "/f") epoch vfs
        |> ok

    let private later : int64 = 5_000_000_000L

    let private systemOnWith
        (configure : ProcessLaunch<int> -> ProcessLaunch<int>)
        (platform : SimulatedUnixPlatform)
        (credentials : Credentials)
        (vfs : VirtualFileSystem)
        : UnixSystem<int, string>
        =
        let system : UnixSystem<int, string> =
            UnixSystem.initial platform
            |> Launched.bootWith
                (Launched.credentials credentials >> configure)
                UnixSystem.pipedStandardStreams
                0
                (CpuId 0)

        { system with
            Machine =
                { UnixMachineState.advanceClock later system.Machine with
                    FileSystem = vfs
                }
            Process =
                { system.Process with
                    CurrentDirectoryInode = VirtualFileSystem.root vfs
                }
        }
        |> Launched.restand

    let private systemOn
        (platform : SimulatedUnixPlatform)
        (credentials : Credentials)
        (vfs : VirtualFileSystem)
        : UnixSystem<int, string>
        =
        systemOnWith id platform credentials vfs

    let private clone
        (source : string)
        (destination : string)
        (flags : int)
        (system : UnixSystem<int, string>)
        : Result<SyscallAnswer * UnixSystem<int, string>, CloneFileRefusal>
        =
        UnixNamespace.cloneFile (argument source) (argument destination) flags system

    let private answerText (result : Result<SyscallAnswer * UnixSystem<int, string>, CloneFileRefusal>) : string =
        match result with
        | Ok (SyscallAnswer.Completed 0L, _) -> "ok"
        | Ok (SyscallAnswer.Completed n, _) -> $"completed %d{n}"
        | Ok (SyscallAnswer.Failed error, _) -> $"%O{error}"
        | Error refusal -> $"refused: %A{refusal}"

    let private entryAt (system : UnixSystem<int, string>) (p : string) : Inode =
        (VirtualFileSystem.tryGet (inodeAt system.Machine.FileSystem p) system.Machine.FileSystem).Value

    [<Test>]
    let ``clonefile answers each measured row`` () : unit =
        let long = String ('n', 299)

        let rows =
            [
                "f", "new", 0x4, "ok"
                "f", "g", 0x4, "EEXIST"
                "f", "e", 0x4, "EEXIST"
                "f", "e/", 0x4, "EEXIST"
                "f", "lf", 0x4, "EEXIST"
                "f", "ld", 0x4, "EEXIST"
                "f", "dang", 0x4, "ok"
                "f", "dangd", 0x4, "ok"
                "f", "cyc", 0x4, "ELOOP"
                "f", "f", 0x4, "EEXIST"
                "f", "hf", 0x4, "EEXIST"
                "f", "new/", 0x4, "ENOENT"
                "f", "nx/new", 0x4, "ENOENT"
                "f", "f/new", 0x4, "ENOTDIR"
                "f", "ld/new", 0x4, "ok"
                "f", "ro/new", 0x4, "EACCES"
                "f", "ro/x", 0x4, "EEXIST"
                "f", "ns/new", 0x4, "EACCES"
                "f", "", 0x4, "ENOENT"
                "f", long, 0x4, "ENAMETOOLONG"
                "nx", "new", 0x4, "ENOENT"
                "nx", "g", 0x4, "ENOENT"
                "nx", "ro/new", 0x4, "ENOENT"
                "", "new", 0x4, "ENOENT"
                "f/", "new", 0x4, "ENOTDIR"
                "f/x", "new", 0x4, "ENOTDIR"
                "lf", "new", 0x4, "ok"
                "dang", "new", 0x4, "ENOENT"
                "cyc", "new", 0x4, "ELOOP"
                "unr", "new", 0x4, "EACCES"
                "wo", "new", 0x4, "EACCES"
                "unr", "g", 0x4, "EEXIST"
                "ns/x", "new", 0x4, "EACCES"
                "ns/x", "g", 0x4, "EACCES"
                "nx", "ro/x", 0x4, "ENOENT"
                "f", "new", 0x0, "ok"
                "f", "new", 0x2, "ok"
                "nx", "new", 0x100000, "EINVAL"
                "f", "g", 0x100000, "EINVAL"
                "f", "\xff\xfe", 0x4, "EILSEQ"
                "f", "ro/\xff\xfe", 0x4, "EACCES"
                "unr", "\xff\xfe", 0x4, "EACCES"
                "unr", long, 0x4, "ENAMETOOLONG"
                "unr", "nx/new", 0x4, "ENOENT"
                "unr", "ns/new", 0x4, "EACCES"
                "unr", "cyc", 0x4, "ELOOP"
                "f", "/", 0x4, "EEXIST"
                "f", ".", 0x4, "EEXIST"
            ]

        let system = systemOn SimulatedUnixPlatform.macOsArm64 u501 tree

        for source, destination, flags, expected in rows do
            let result = clone source destination flags system
            let actual = answerText result

            if actual <> expected then
                failwith
                    $"clonefile(%s{source}, %s{destination}, 0x%x{flags}): measured %s{expected}, the model %s{actual}"

            match result with
            | Ok (SyscallAnswer.Failed _, after) -> after |> shouldEqual system
            | _ -> ()

        // Where each success put its file: a dangling link is replaced by a
        // file at its target, which is the link's name resolved, and the link
        // itself stays.
        for destination, created in [ "dang", "/nx" ; "dangd", "/d/nx" ; "ld/new", "/d/new" ] do
            match clone "f" destination 0x4 system with
            | Ok (SyscallAnswer.Completed 0L, after) ->
                match (entryAt after created).Content with
                | InodeContent.RegularFile (contents, _) ->
                    Text.Encoding.ASCII.GetString (contents |> Seq.toArray) |> shouldEqual "hello"
                | other -> failwith $"%s{created}: %A{other}"

                match (entryAt after ("/" + destination.Split('/').[0])).Content with
                | InodeContent.Symlink _
                | InodeContent.Directory _ -> ()
                | other -> failwith $"%s{destination} was replaced: %A{other}"
            | other -> failwith $"clonefile(f, %s{destination}): %A{other}"

    [<Test>]
    let ``every flag bit above CLONE_RESOLVE_BENEATH is EINVAL, and the path-changing ones are refused`` () : unit =
        let system = systemOn SimulatedUnixPlatform.macOsArm64 u501 tree

        for bit in 0..31 do
            let flags = 1 <<< bit
            let result = clone "f" "new" flags system

            match bit, result with
            | (1 | 2), Ok (SyscallAnswer.Completed 0L, _) -> ()
            | (0 | 3 | 4), Error (CloneFileRefusal.UnmodelledFlags refused) -> refused |> shouldEqual flags
            | bit, Ok (SyscallAnswer.Failed UnixError.EINVAL, after) when bit >= 5 -> after |> shouldEqual system
            | _ -> failwith $"bit %d{bit}: %A{result}"

    [<Test>]
    let ``bad flags are answered before either pathname is read, and the source before the destination`` () : unit =
        let system = systemOn SimulatedUnixPlatform.macOsArm64 u501 tree

        UnixNamespace.cloneFile PathArgumentBytes.Unreadable PathArgumentBytes.Unreadable (1 <<< 20) system
        |> shouldEqual (Ok (SyscallAnswer.Failed UnixError.EINVAL, system))

        UnixNamespace.cloneFile PathArgumentBytes.Unreadable PathArgumentBytes.Unreadable 0x4 system
        |> shouldEqual (Ok (SyscallAnswer.Failed UnixError.EFAULT, system))

        UnixNamespace.cloneFile (argument "nx") PathArgumentBytes.Unreadable 0x4 system
        |> shouldEqual (Ok (SyscallAnswer.Failed UnixError.ENOENT, system))

        UnixNamespace.cloneFile (argument "f") PathArgumentBytes.Unreadable 0x4 system
        |> shouldEqual (Ok (SyscallAnswer.Failed UnixError.EFAULT, system))

    [<Test>]
    let ``a clone keeps the source's bits less both set-ID bits, for an owner in the source's group`` () : unit =
        // The probe's sweep: every mode an owner can give a source in a
        // directory of its own group, and in a wheel directory, where S_ISGID
        // cannot be set. A mode without the owner's read bit is EACCES.
        for directoryGroup, sourceGroup in [ 20u, 20u ; 0u, 0u ] do
            for bits in 0..0o7777 do
                let settable = directoryGroup = 20u || bits &&& 0o2000 = 0

                if settable then
                    let vfs =
                        VirtualFileSystem.empty epoch mine
                        |> directory "/m" (owner 501u directoryGroup) 0o755
                        |> file "/m/msrc" (owner 501u sourceGroup) bits "x"

                    let system = systemOn SimulatedUnixPlatform.macOsArm64 u501 vfs

                    match clone "m/msrc" "m/mdst" 0x4 system, bits &&& 0o400 <> 0 with
                    | Ok (SyscallAnswer.Completed 0L, after), true ->
                        let kept = Inode.permissions (entryAt after "/m/mdst")

                        if PermissionBits.toInt kept <> (bits &&& ~~~0o6000) then
                            failwith
                                $"0o%o{bits} in group %d{directoryGroup}: clone has 0o%o{PermissionBits.toInt kept}"
                    | Ok (SyscallAnswer.Failed UnixError.EACCES, _), false -> ()
                    | other, _ -> failwith $"0o%o{bits} in group %d{directoryGroup}: %A{other}"

    [<Test>]
    let ``a clone is owned as a new file in its directory, and carries the source's times`` () : unit =
        // The probe's META rows: uid is the caller's and gid the directory's,
        // whichever group the source is in; atime, mtime and birth are the
        // source's, ctime is now; the umask plays no part; the directory's
        // mtime and ctime move; the source does not change.
        let sourceTimes =
            {
                Access = UnixTimestamp.createOrFail context 1_000_000_000L 111_111_111
                Modification = UnixTimestamp.createOrFail context 900_000_000L 222_222_222
                StatusChange = UnixTimestamp.createOrFail context 1_200_000_000L 0
                Birth = UnixTimestamp.createOrFail context 800_000_000L 333_333_333
            }

        for sourceGroup, directoryGroup in [ 20u, 0u ; 0u, 20u ; 20u, 20u ] do
            let vfs =
                VirtualFileSystem.empty epoch mine
                |> directory "/w" (owner 0u directoryGroup) 0o777

            // Born at `sourceTimes.Birth`, then given its access and
            // modification times at `sourceTimes.StatusChange`.
            let vfs =
                VirtualFileSystem.createFile
                    (VirtualFileSystem.root vfs)
                    (name "src")
                    (mode 0o640)
                    (owner 501u sourceGroup)
                    sourceTimes.Birth
                    (Text.Encoding.ASCII.GetBytes "hello" |> ImmutableArray.CreateRange)
                    vfs
                |> ok
                |> snd

            let vfs =
                VirtualFileSystem.setTimes
                    (inodeAt vfs "/src")
                    sourceTimes.Access
                    sourceTimes.Modification
                    sourceTimes.StatusChange
                    vfs

            let system =
                systemOnWith (Launched.umask (mode 0o777)) SimulatedUnixPlatform.macOsArm64 u501 vfs

            let now = UnixMachineState.realtime system.Machine

            match clone "src" "w/dst" 0x4 system with
            | Ok (SyscallAnswer.Completed 0L, after) ->
                let clone = entryAt after "/w/dst"
                clone.Owner |> shouldEqual (owner 501u directoryGroup)

                clone.Times
                |> shouldEqual
                    { sourceTimes with
                        StatusChange = now
                    }

                Inode.permissions clone |> shouldEqual (mode 0o640)
                entryAt after "/src" |> shouldEqual (entryAt system "/src")

                let before = (entryAt system "/w").Times

                (entryAt after "/w").Times
                |> shouldEqual
                    { before with
                        Modification = now
                        StatusChange = now
                    }

                UnixSystem.checkInvariants after |> shouldEqual []
            | other -> failwith $"%A{other}"

    [<Test>]
    let ``clonefile refuses what it was not measured for`` () : unit =
        let darwin = systemOn SimulatedUnixPlatform.macOsArm64 u501 tree

        clone "d" "new" 0x4 darwin
        |> Result.map fst
        |> shouldEqual (Error (CloneFileRefusal.DirectorySource (inodeAt tree "/d")))

        clone "ld" "new" 0x4 darwin
        |> Result.map fst
        |> shouldEqual (Error (CloneFileRefusal.DirectorySource (inodeAt tree "/d")))

        clone "f" "new" 0x4 (systemOn SimulatedUnixPlatform.macOsArm64 Owners.root tree)
        |> Result.map fst
        |> shouldEqual (Error CloneFileRefusal.PrivilegedCaller)

        let linux =
            systemOn
                SimulatedUnixPlatform.linuxX64
                Owners.linuxDefaultCaller
                (VirtualFileSystem.empty epoch Owners.linuxDefault)

        clone "f" "new" 0x4 linux
        |> Result.map fst
        |> shouldEqual (Error (CloneFileRefusal.UnmodelledFlavour SimulatedUnixFlavour.Linux))

        let nfs =
            systemOnWith id SimulatedUnixPlatform.macOsArm64 u501 tree
            |> fun system ->
                { system with
                    Machine =
                        { system.Machine with
                            Mount = EmulatedMount.Nfs
                        }
                }

        clone "f" "new" 0x4 nfs
        |> Result.map fst
        |> shouldEqual (Error (CloneFileRefusal.UnmeasuredFileSystem EmulatedFileSystemType.Nfs))

        // Special bits on a source the caller does not own, or S_ISGID on one
        // whose group it is not in: unmeasured.
        for by, bits in [ owner 0u 0u, 0o4644 ; owner 0u 0u, 0o1644 ; owner 501u 0u, 0o2644 ] do
            let vfs = VirtualFileSystem.empty epoch mine |> file "/s" by bits "x"

            match clone "s" "c" 0x4 (systemOn SimulatedUnixPlatform.macOsArm64 u501 vfs) with
            | Error (CloneFileRefusal.UnmeasuredSpecialBits _) -> ()
            | other -> failwith $"0o%o{bits} owned by %A{by}: %A{other}"

        // Without special bits, someone else's readable file clones as the
        // probe's /etc/hosts row did.
        let vfs = VirtualFileSystem.empty epoch mine |> file "/s" (owner 0u 0u) 0o644 "x"

        match clone "s" "c" 0x4 (systemOn SimulatedUnixPlatform.macOsArm64 u501 vfs) with
        | Ok (SyscallAnswer.Completed 0L, after) ->
            let clone = entryAt after "/c"
            clone.Owner |> shouldEqual mine
            Inode.permissions clone |> shouldEqual (mode 0o644)
        | other -> failwith $"%A{other}"

    [<Test>]
    let ``nothing is cloned into a directory whose last name has gone`` () : unit =
        let vfs = tree |> directory "/orph" mine 0o755

        let system =
            let s = systemOn SimulatedUnixPlatform.macOsArm64 u501 vfs

            { s with
                Process =
                    { s.Process with
                        CurrentDirectoryInode = inodeAt vfs "/orph"
                    }
            }
            |> Launched.restand

        let system =
            match UnixNamespace.rmdir (PathArg.ofPath (UnixPath.parseOrFail context "/orph")) system with
            | Ok (SyscallAnswer.Completed 0L, system) -> system
            | other -> failwith $"rmdir: %A{other}"

        clone "/f" "new" 0x4 system |> answerText |> shouldEqual "ENOENT"

    [<Test>]
    let ``a successful clone changes exactly the new file and its directory`` () : unit =
        let names = [ "f" ; "g" ; "lf" ; "d/in" ; "ns/x" ; "unr" ; "wo" ; "ro/x" ; "nx" ]

        let destinations =
            [
                "new"
                "d/new"
                "e/new"
                "dang"
                "ld/new"
                "ro/new"
                "g"
                "nx/new"
                "\xff"
            ]

        let property (source : string, destination : string) : unit =
            let system = systemOn SimulatedUnixPlatform.macOsArm64 u501 tree

            match clone source destination 0x4 system with
            | Ok (SyscallAnswer.Completed 0L, after) ->
                let created =
                    VirtualFileSystem.inodes after.Machine.FileSystem
                    |> Map.filter (fun inode _ ->
                        not (Map.containsKey inode (VirtualFileSystem.inodes system.Machine.FileSystem))
                    )
                    |> Map.toList

                match created with
                | [ _, clone ] ->
                    let sourceEntry =
                        match
                            UnixPathResolution.resolvePath
                                AtDirectory.CurrentDirectory
                                SymlinkPolicy.Follow
                                (UnixPath.parseOrFail context source)
                                system
                        with
                        | Ok inode -> (VirtualFileSystem.tryGet inode system.Machine.FileSystem).Value
                        | Error error -> failwith $"%s{source}: %O{error}"

                    match clone.Content, sourceEntry.Content with
                    | InodeContent.RegularFile (cloned, _), InodeContent.RegularFile (original, _) ->
                        cloned |> Seq.toArray |> shouldEqual (original |> Seq.toArray)
                    | other -> failwith $"%A{other}"

                    // Every inode that was there before is unchanged, but for
                    // the one directory that gained the name.
                    let changed =
                        VirtualFileSystem.inodes system.Machine.FileSystem
                        |> Map.filter (fun inode entry ->
                            Map.find inode (VirtualFileSystem.inodes after.Machine.FileSystem) <> entry
                        )
                        |> Map.toList

                    match changed with
                    | [ _,
                        {
                            Content = InodeContent.Directory _
                        } ] -> ()
                    | other -> failwith $"changed: %A{other}"
                | other -> failwith $"created: %A{other}"
            | Ok (SyscallAnswer.Failed _, after) -> after |> shouldEqual system
            | other -> failwith $"%A{other}"

        let gen = Gen.zip (Gen.elements names) (Gen.elements destinations)
        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``step answers clonefile as the function does`` () : unit =
        let system = systemOn SimulatedUnixPlatform.macOsArm64 u501 tree

        for destination in [ "new" ; "g" ] do
            match UnixSystem.step 0 (Syscall.CloneFile (argument "f", argument destination, 0x4)) system with
            | Ok (SyscallOutcome.Answered answer, after) ->
                Ok (answer, after) |> shouldEqual (clone "f" destination 0x4 system)
            | other -> failwith $"%A{other}"
