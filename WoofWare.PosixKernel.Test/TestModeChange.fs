namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `chmod(2)` and `fchmod(2)`: who may change a mode, which bits the inode then
/// has, which timestamps move, how a path through a symbolic link resolves, and
/// what `fchmod` answers on each descriptor kind.
///
/// The rows come from `docs/plans/2026-08-23-posix-kernel-extraction/chmod-rules.c`,
/// run on Linux 6.18.5 (aarch64, root in the container, ext4 and tmpfs alike) and
/// on Darwin 27.0 at uid 501; its output is beside it. Its sweeps are replayed
/// here exhaustively against the same prediction it compared the kernels with,
/// and its individually measured rows as literals against filesystems built to
/// match the ones it measured.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestModeChange =

    let private context : string = "TestModeChange"

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

    let private path (p : string) : UnixPath = UnixPath.parseOrFail context p

    let private ok (result : Result<'a, 'e>) : 'a =
        match result with
        | Ok value -> value
        | Error error -> failwith $"expected Ok, got %A{error}"

    let private allStandings : Standing list =
        [
            for privilege in [ CallerPrivilege.Unprivileged ; CallerPrivilege.Privileged ] do
                for owns in [ false ; true ] do
                    for inGroup in [ false ; true ] do
                        yield
                            {
                                Privilege = privilege
                                Owns = owns
                                InGroup = inGroup
                            }
        ]

    let private allModes : int list = [ 0..0o7777 ]

    /// The high halves `chmod-rules.c` swept, each over every low word, as the
    /// 32-bit argument a caller passes. The kernel read none of them.
    let private highs : int list =
        [
            0
            0o10000
            0o20000
            0o30000
            0o40000
            0o50000
            0o60000
            0o70000
            0o100000
            0o110000
            0o120000
            0o140000
            0o170000
            0o200000
            0x10000
            0x20000
            0x100000
            0x40000000
            0x80000000
            0xFFFF0000
            0xFFFFF000
        ]

    let private rules : PrivilegedModeChange list =
        [ PrivilegedModeChange.SetsRequestedBits ; PrivilegedModeChange.Unmeasured ]

    // ------------------------------------------------------------- the rule

    /// The rule `chmod-rules.c` compared every kernel answer against: an
    /// unprivileged owner sets the twelve bits it asks for less `S_ISGID` when it
    /// is outside the inode's group, anyone else unprivileged is EPERM, and
    /// Linux's root sets all twelve. Bits above the twelve are ignored.
    let private reference
        (rule : PrivilegedModeChange)
        (standing : Standing)
        (raw : int)
        : Result<ModeChange, ModeChangeRefusal>
        =
        let asked = raw &&& 0o7777

        match standing.Privilege, rule with
        | CallerPrivilege.Privileged, PrivilegedModeChange.SetsRequestedBits -> Ok (ModeChange.Permitted (mode asked))
        | CallerPrivilege.Privileged, PrivilegedModeChange.Unmeasured ->
            Error (ModeChangeRefusal.UnmeasuredPrivilegedCaller (standing, mode asked))
        | CallerPrivilege.Unprivileged, _ ->
            if not standing.Owns then
                Ok ModeChange.Forbidden
            elif standing.InGroup then
                Ok (ModeChange.Permitted (mode asked))
            else
                Ok (ModeChange.Permitted (mode (asked &&& ~~~0o2000)))

    [<Test>]
    let ``afterModeChange answers the rule the probe measured, for every standing, mode and high half`` () : unit =
        for rule in rules do
            for standing in allStandings do
                for high in highs do
                    for low in allModes do
                        let raw = high ||| low
                        let actual = PermissionBits.afterModeChange rule standing raw

                        if actual <> reference rule standing raw then
                            failwith
                                $"%O{rule}, %O{standing}, mode 0x%08x{raw}: afterModeChange said %A{actual}, the probe's rule %A{reference rule standing raw}"

    [<Test>]
    let ``the measured rows, as literals`` () : unit =
        let unprivileged (owns : bool) (inGroup : bool) : Standing =
            {
                Privilege = CallerPrivilege.Unprivileged
                Owns = owns
                InGroup = inGroup
            }

        let root : Standing =
            {
                Privilege = CallerPrivilege.Privileged
                Owns = false
                InGroup = false
            }

        let permitted (bits : int) = Ok (ModeChange.Permitted (mode bits))

        // From `chmod-rules.c` and the ownership design's `ownership-probe.c`,
        // which agree; each row was measured on both flavours unless it is
        // root's, which is Linux's alone.
        let rows : (PrivilegedModeChange * Standing * int * Result<ModeChange, ModeChangeRefusal>) list =
            [
                // The owner outside the file's group loses S_ISGID and keeps
                // everything else, the sticky bit on a regular file included.
                PrivilegedModeChange.SetsRequestedBits, unprivileged true false, 0o2755, permitted 0o0755
                PrivilegedModeChange.SetsRequestedBits, unprivileged true false, 0o2745, permitted 0o0745
                PrivilegedModeChange.SetsRequestedBits, unprivileged true false, 0o6755, permitted 0o4755
                PrivilegedModeChange.SetsRequestedBits, unprivileged true false, 0o4755, permitted 0o4755
                PrivilegedModeChange.SetsRequestedBits, unprivileged true false, 0o1644, permitted 0o1644
                PrivilegedModeChange.Unmeasured, unprivileged true false, 0o2755, permitted 0o0755
                PrivilegedModeChange.Unmeasured, unprivileged true false, 0o6755, permitted 0o4755
                PrivilegedModeChange.Unmeasured, unprivileged true false, 0o1644, permitted 0o1644
                // The owner in the file's group keeps all of it.
                PrivilegedModeChange.SetsRequestedBits, unprivileged true true, 0o2755, permitted 0o2755
                PrivilegedModeChange.SetsRequestedBits, unprivileged true true, 0o6755, permitted 0o6755
                PrivilegedModeChange.Unmeasured, unprivileged true true, 0o2745, permitted 0o2745
                PrivilegedModeChange.Unmeasured, unprivileged true true, 0o1755, permitted 0o1755
                // `(mode_t)-1` is every bit.
                PrivilegedModeChange.SetsRequestedBits, unprivileged true true, -1, permitted 0o7777
                PrivilegedModeChange.Unmeasured, unprivileged true true, -1, permitted 0o7777
                // Anyone else is EPERM, even asking for the mode it already has.
                PrivilegedModeChange.SetsRequestedBits, unprivileged false true, 0o0600, Ok ModeChange.Forbidden
                PrivilegedModeChange.SetsRequestedBits, unprivileged false false, 0o2755, Ok ModeChange.Forbidden
                PrivilegedModeChange.Unmeasured, unprivileged false true, 0o0644, Ok ModeChange.Forbidden
                PrivilegedModeChange.Unmeasured, unprivileged false false, 0o1777, Ok ModeChange.Forbidden
                // Linux's root keeps S_ISGID in a group it is not in.
                PrivilegedModeChange.SetsRequestedBits, root, 0o2755, permitted 0o2755
                PrivilegedModeChange.Unmeasured,
                root,
                0o2755,
                Error (ModeChangeRefusal.UnmeasuredPrivilegedCaller (root, mode 0o2755))
            ]

        for rule, standing, requested, expected in rows do
            (rule, standing, requested, PermissionBits.afterModeChange rule standing requested)
            |> shouldEqual (rule, standing, requested, expected)

    [<Test>]
    let ``each platform names its flavour's privileged rule`` () : unit =
        for platform, expected in
            [
                SimulatedUnixPlatform.linuxX64, PrivilegedModeChange.SetsRequestedBits
                SimulatedUnixPlatform.linuxArm64, PrivilegedModeChange.SetsRequestedBits
                SimulatedUnixPlatform.macOsArm64, PrivilegedModeChange.Unmeasured
            ] do
            SimulatedUnixPlatform.privilegedModeChange platform |> shouldEqual expected

    // ------------------------------------------------------------- hand-built filesystems

    /// The inode `p` names, walked as root without following a final link.
    let private inodeAt (vfs : VirtualFileSystem) (p : string) : InodeNumber =
        match
            PathWalk.resolveExisting
                (SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.linuxX64)
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

    let private directory (p : string) (by : InodeOwner) (bits : int) (vfs : VirtualFileSystem) : VirtualFileSystem =
        let parent, child = parentAndName p

        VirtualFileSystem.createDirectory (inodeAt vfs parent) (name child) (mode bits) by epoch vfs
        |> ok
        |> snd

    let private file (p : string) (by : InodeOwner) (bits : int) (vfs : VirtualFileSystem) : VirtualFileSystem =
        let parent, child = parentAndName p

        VirtualFileSystem.createFile
            (inodeAt vfs parent)
            (name child)
            (mode bits)
            by
            epoch
            (ImmutableArray.CreateRange [| 1uy ; 2uy ; 3uy ; 4uy |])
            vfs
        |> ok
        |> snd

    let private symlink (p : string) (target : string) (vfs : VirtualFileSystem) : VirtualFileSystem =
        let parent, child = parentAndName p

        VirtualFileSystem.createSymlink
            (inodeAt vfs parent)
            (name child)
            Owners.linuxDefault
            epoch
            (SymlinkTarget.parseOrFail context target)
            vfs
        |> ok
        |> snd

    /// The clock `systemOn` starts every process at: after every inode of a
    /// hand-built filesystem was stamped, so a timestamp a syscall moves is
    /// visibly not the one it had.
    let private later : int64 = 5_000_000_000L

    /// A process on `platform` with `credentials`, in the root of `vfs`, whose
    /// clock is past every timestamp in `vfs`.
    let private systemOn
        (platform : SimulatedUnixPlatform)
        (credentials : Credentials)
        (vfs : VirtualFileSystem)
        : UnixSystem<int, string>
        =
        let system : UnixSystem<int, string> =
            UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)

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
        |> UnixSystem.withCredentials context credentials

    let private inodeOf (p : string) (system : UnixSystem<int, string>) : Inode =
        match VirtualFileSystem.tryGet (inodeAt system.Machine.FileSystem p) system.Machine.FileSystem with
        | Some inode -> inode
        | None -> failwith $"%s{p} is absent"

    let private modeOf (p : string) (system : UnixSystem<int, string>) : int =
        match Inode.permissions (inodeOf p system) with
        | InodePermissions.Stored bits -> PermissionBits.toInt bits
        | InodePermissions.PlatformSymlinkDefault -> failwith $"%s{p} is a symlink"

    /// u1000, whose effective group is 1000 and supplementary groups 1000 and
    /// 2000: the probe's unprivileged Linux caller.
    let private u1000 : Credentials =
        Credentials.ofIds (uid 1000u) (gid 1000u) [ gid 1000u ; gid 2000u ]

    /// uid 501 in groups 20 and 12: the probe's Darwin caller.
    let private u501 : Credentials =
        Credentials.ofIds (uid 501u) (gid 20u) [ gid 20u ; gid 12u ]

    /// What `chmod` did, stated as the probe printed it: the errno or success,
    /// and the mode the inode then has.
    let private chmodAnswer (p : string) (requested : int) (system : UnixSystem<int, string>) =
        match UnixPathResolution.chmod (PathArg.ofPath (path p)) requested system with
        | Ok (answer, after) -> answer, after
        | Error refusal -> failwith $"chmod(%s{p}, 0o%o{requested}) was refused: %s{ChModRefusal.describe refusal}"

    /// Replay the probe's standing sweep: every mode asked of a file and of a
    /// directory, from the start mode each time, checking the answer, the mode
    /// the inode then has, which of its timestamps moved, and that a refusal
    /// changed nothing at all.
    let private sweep (standing : Standing) (system : UnixSystem<int, string>) : unit =
        let now = UnixMachineState.realtime system.Machine

        for p in [ "/t/f" ; "/t/d" ] do
            let before = inodeOf p system

            for requested in allModes do
                let answer, after = chmodAnswer p requested system

                let expected =
                    reference PrivilegedModeChange.SetsRequestedBits standing requested |> ok

                match expected, answer with
                | ModeChange.Forbidden, SyscallAnswer.Failed UnixError.EPERM ->
                    if after <> system then
                        failwith $"%O{standing}: chmod(%s{p}, 0o%04o{requested}) answered EPERM but changed the system"
                | ModeChange.Permitted bits, SyscallAnswer.Completed 0L ->
                    let changed = inodeOf p after

                    if modeOf p after <> PermissionBits.toInt bits then
                        failwith
                            $"%O{standing}: chmod(%s{p}, 0o%04o{requested}) left 0o%04o{modeOf p after}, expected %O{bits}"

                    changed.Times
                    |> shouldEqual
                        { before.Times with
                            StatusChange = now
                        }

                    changed.Owner |> shouldEqual before.Owner
                | _ ->
                    failwith $"%O{standing}: chmod(%s{p}, 0o%04o{requested}) answered %A{answer}, expected %A{expected}"

    /// `/t` holding a 0600 file and a 0700 directory owned by `by`, as the probe
    /// prepared each row.
    let private tree (by : InodeOwner) : VirtualFileSystem =
        VirtualFileSystem.empty epoch (owner 0u 0u)
        |> directory "/t" (owner 0u 0u) 0o755
        |> file "/t/f" by 0o600
        |> directory "/t/d" by 0o700

    [<Test>]
    let ``Linux chmod answers every mode as the probe measured, for every standing`` () : unit =
        let rows : (string * Credentials * InodeOwner) list =
            [
                "owner, group = egid", u1000, owner 1000u 1000u
                "owner, group = supplementary", u1000, owner 1000u 2000u
                "owner, group not a member", u1000, owner 1000u 3000u
                "non-owner, group = egid", u1000, owner 1001u 1000u
                "non-owner, group = supplementary", u1000, owner 1001u 2000u
                "non-owner, group not a member", u1000, owner 1001u 3000u
                "root, owner, in group", Owners.root, owner 0u 0u
                "root, owner, group not a member", Owners.root, owner 0u 3000u
                "root, non-owner, in group", Owners.root, owner 1001u 0u
                "root, non-owner, group not a member", Owners.root, owner 1001u 3000u
            ]

        for label, credentials, by in rows do
            let standing = Standing.toward credentials by
            // The label is what the probe measured; the standing is what the
            // rule reads. Assert they are the same row.
            (label, standing.Owns, standing.InGroup)
            |> shouldEqual (label, not (label.Contains "non-owner"), not (label.Contains "not a member"))

            sweep standing (systemOn SimulatedUnixPlatform.linuxX64 credentials (tree by))

    [<Test>]
    let ``Darwin chmod answers every mode as the probe measured, for every unprivileged standing`` () : unit =
        // The owner rows swept every mode on the probe's own inodes; the
        // non-owner rows are someone else's inodes, chmodded to the mode they
        // already had (EPERM, and nothing moved).
        let rows : (string * InodeOwner) list =
            [
                "owner, group = egid", owner 501u 20u
                "owner, group = supplementary 12", owner 501u 12u
                "owner, group not a member (wheel)", owner 501u 0u
                "non-owner, group = egid", owner 503u 20u
                "non-owner, group not a member", owner 0u 0u
            ]

        for _, by in rows do
            sweep (Standing.toward u501 by) (systemOn SimulatedUnixPlatform.macOsArm64 u501 (tree by))

    [<Test>]
    let ``Darwin chmod refuses a privileged caller, whoever owns the inode`` () : unit =
        for by in [ owner 0u 0u ; owner 0u 3000u ; owner 501u 20u ] do
            let system = systemOn SimulatedUnixPlatform.macOsArm64 Owners.root (tree by)
            let standing = Standing.toward Owners.root by

            UnixPathResolution.chmod (PathArg.ofPath (path "/t/f")) 0o644 system
            |> shouldEqual (
                Error (
                    ChModRefusal.UnmeasuredModeChange (
                        inodeAt system.Machine.FileSystem "/t/f",
                        ModeChangeRefusal.UnmeasuredPrivilegedCaller (standing, mode 0o644)
                    )
                )
            )

    [<Test>]
    let ``chmod to the mode the inode already has still moves its ctime`` () : unit =
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            let credentials =
                UnixSystem.defaultCredentials (SimulatedUnixPlatform.flavour platform)

            let system = systemOn platform credentials (tree (InodeOwner.ofProcess credentials))
            let now = UnixMachineState.realtime system.Machine

            for p, bits in [ "/t/f", 0o600 ; "/t/d", 0o700 ] do
                let before = inodeOf p system

                match chmodAnswer p bits system with
                | SyscallAnswer.Completed 0L, after ->
                    (inodeOf p after).Times
                    |> shouldEqual
                        { before.Times with
                            StatusChange = now
                        }

                    // Nothing but that inode changed: its parent's times, in
                    // particular, are where they were.
                    (inodeOf "/t" after).Times |> shouldEqual (inodeOf "/t" system).Times
                | other -> failwith $"chmod(%s{p}, 0o%o{bits}): %A{other}"

    // ------------------------------------------------------------- paths

    /// `/p` holding a file, a directory, links to each, a dangling link, a link
    /// to itself, and an unsearchable directory holding a file.
    let private pathTree (by : InodeOwner) : VirtualFileSystem =
        VirtualFileSystem.empty epoch by
        |> directory "/p" by 0o755
        |> file "/p/f" by 0o600
        |> directory "/p/d" by 0o700
        |> symlink "/p/lf" "f"
        |> symlink "/p/ld" "d"
        |> symlink "/p/dang" "nowhere"
        |> symlink "/p/loop" "loop"
        |> directory "/p/shut" by 0o600
        |> file "/p/shut/inner" by 0o600

    [<Test>]
    let ``chmod follows a final symlink, and every other failure is the walk's`` () : unit =
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            let credentials =
                UnixSystem.defaultCredentials (SimulatedUnixPlatform.flavour platform)

            let system =
                systemOn platform credentials (pathTree (InodeOwner.ofProcess credentials))

            // Through the link: the target changes, and the link itself does
            // not, not even its ctime.
            match chmodAnswer "/p/lf" 0o640 system with
            | SyscallAnswer.Completed 0L, after ->
                modeOf "/p/f" after |> shouldEqual 0o640
                inodeOf "/p/lf" after |> shouldEqual (inodeOf "/p/lf" system)
            | other -> failwith $"chmod through a link: %A{other}"

            for p, bits, changed in [ "/p/ld", 0o750, "/p/d" ; "/p/ld/", 0o755, "/p/d" ; "/p/d/", 0o711, "/p/d" ] do
                match chmodAnswer p bits system with
                | SyscallAnswer.Completed 0L, after -> (p, modeOf changed after) |> shouldEqual (p, bits)
                | other -> failwith $"chmod(%s{p}): %A{other}"

            for p, error in
                [
                    "/p/f/", UnixError.ENOTDIR
                    "/p/lf/", UnixError.ENOTDIR
                    "/p/dang", UnixError.ENOENT
                    "/p/loop", UnixError.ELOOP
                    "/p/absent", UnixError.ENOENT
                    "", UnixError.ENOENT
                    "/p/f/under", UnixError.ENOTDIR
                    // The walk's own search check, which the owner fails on a 0600
                    // directory like anyone else.
                    "/p/shut/inner", UnixError.EACCES
                ] do
                match chmodAnswer p 0o644 system with
                | SyscallAnswer.Failed actual, after ->
                    (p, actual) |> shouldEqual (p, error)
                    after |> shouldEqual system
                | other -> failwith $"chmod(%s{p}): %A{other}"

    // ------------------------------------------------------------- fchmod

    let private withDescriptors
        (registry : FileDescriptorRegistry)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        { system with
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    let private opened
        (p : string)
        (access : FileAccessMode)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        let fd, registry =
            FileDescriptorRegistry.openFile (inodeAt system.Machine.FileSystem p) access system.Process.FileDescriptors

        fd, withDescriptors registry system

    let private openedDirectory (p : string) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let fd, registry =
            FileDescriptorRegistry.openDirectory (inodeAt system.Machine.FileSystem p) system.Process.FileDescriptors

        fd, withDescriptors registry system

    let private fchmodAnswer (fd : int) (requested : int) (system : UnixSystem<int, string>) =
        match UnixPathResolution.fchmod fd requested system with
        | Ok answer -> answer
        | Error refusal -> failwith $"fchmod(%d{fd}, 0o%o{requested}) was refused: %s{FChModRefusal.describe refusal}"

    [<Test>]
    let ``fchmod changes a file or directory through a descriptor of any access mode`` () : unit =
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            let credentials =
                UnixSystem.defaultCredentials (SimulatedUnixPlatform.flavour platform)

            let system =
                systemOn platform credentials (pathTree (InodeOwner.ofProcess credentials))

            let now = UnixMachineState.realtime system.Machine

            for access, bits in
                [
                    FileAccessMode.ReadOnly, 0o640
                    FileAccessMode.WriteOnly, 0o604
                    FileAccessMode.ReadWrite, 0o4644
                ] do
                let fd, withFd = opened "/p/f" access system

                match fchmodAnswer fd bits withFd with
                | SyscallAnswer.Completed 0L, after ->
                    modeOf "/p/f" after |> shouldEqual bits
                    (inodeOf "/p/f" after).Times.StatusChange |> shouldEqual now
                | other -> failwith $"fchmod through %O{access}: %A{other}"

            let fd, withFd = openedDirectory "/p/d" system

            match fchmodAnswer fd 0o750 withFd with
            | SyscallAnswer.Completed 0L, after -> modeOf "/p/d" after |> shouldEqual 0o750
            | other -> failwith $"fchmod of a directory: %A{other}"

    [<Test>]
    let ``fchmod reaches a file whose last name has gone`` () : unit =
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            let credentials =
                UnixSystem.defaultCredentials (SimulatedUnixPlatform.flavour platform)

            let system =
                systemOn platform credentials (pathTree (InodeOwner.ofProcess credentials))

            let inode = inodeAt system.Machine.FileSystem "/p/f"
            let fd, withFd = opened "/p/f" FileAccessMode.ReadOnly system
            let _, unlinked = Answered.unlink (path "/p/f") withFd

            match fchmodAnswer fd 0o400 unlinked with
            | SyscallAnswer.Completed 0L, after ->
                match VirtualFileSystem.tryGet inode after.Machine.FileSystem with
                | Some orphan -> Inode.permissions orphan |> shouldEqual (InodePermissions.Stored (mode 0o400))
                | None -> failwith "the orphan was freed while a descriptor held it"
            | other -> failwith $"fchmod of an orphan: %A{other}"

    [<Test>]
    let ``fchmod of someone else's file is EPERM, however it was opened`` () : unit =
        let vfs =
            VirtualFileSystem.empty epoch (owner 0u 0u)
            |> file "/rootfile" (owner 0u 0u) 0o644

        for platform, credentials in
            [
                SimulatedUnixPlatform.linuxX64, u1000
                SimulatedUnixPlatform.macOsArm64, u501
            ] do
            let system = systemOn platform credentials vfs
            let fd, withFd = opened "/rootfile" FileAccessMode.ReadOnly system

            fchmodAnswer fd 0o644 withFd
            |> shouldEqual (SyscallAnswer.Failed UnixError.EPERM, withFd)

    [<Test>]
    let ``fchmod of a descriptor that is not open is EBADF`` () : unit =
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            let system =
                UnixSystem.initial<int, string> platform UnixSystem.pipedStandardStreams 0 (CpuId 0)

            for fd in [ 1000 ; -1 ; 3 ] do
                fchmodAnswer fd 0o644 system
                |> shouldEqual (SyscallAnswer.Failed UnixError.EBADF, system)

    [<Test>]
    let ``fchmod of a descriptor with no inode answers as each flavour measured`` () : unit =
        let linux =
            UnixSystem.initial<int, string> SimulatedUnixPlatform.linuxX64 UnixSystem.pipedStandardStreams 0 (CpuId 0)

        let darwin =
            UnixSystem.initial<int, string> SimulatedUnixPlatform.macOsArm64 UnixSystem.pipedStandardStreams 0 (CpuId 0)

        let port (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
            let fd, registry =
                FileDescriptorRegistry.createSocketEventPort system.Process.FileDescriptors

            fd, withDescriptors registry system

        // Linux's epoll instance is on the shared anonymous inode, whose mode
        // fchmod may not change: EOPNOTSUPP. Darwin's kqueue, like every
        // descriptor that is not a vnode there, is EINVAL.
        let fd, withPort = port linux

        fchmodAnswer fd 0o600 withPort
        |> shouldEqual (SyscallAnswer.Failed UnixError.EOPNOTSUPP, withPort)

        let fd, withPort = port darwin

        fchmodAnswer fd 0o600 withPort
        |> shouldEqual (SyscallAnswer.Failed UnixError.EINVAL, withPort)

        // A socket and a standard stream (one end of a pipe the process was
        // launched with) are EINVAL on Darwin. On Linux both have a mode fchmod
        // changes if the caller may, and whether it may turns on an owner this
        // kernel does not hold, so it refuses.
        for domain, kind, protocol in
            [
                SocketDomain.Inet, SocketKind.Stream, SocketProtocol.Tcp
                SocketDomain.Inet, SocketKind.Datagram, SocketProtocol.Udp
                SocketDomain.Inet6, SocketKind.Stream, SocketProtocol.Tcp
                SocketDomain.Unix, SocketKind.Stream, SocketProtocol.Default
            ] do
            let fd, withSocket = NewSocket.create domain kind protocol darwin

            fchmodAnswer fd 0o600 withSocket
            |> shouldEqual (SyscallAnswer.Failed UnixError.EINVAL, withSocket)

            let fd, withSocket = NewSocket.create domain kind protocol linux

            let socket =
                match FileDescriptorRegistry.tryFindObject fd withSocket.Process.FileDescriptors with
                | Some (OpenFileObject.Socket socket) -> socket
                | other -> failwith $"fd %d{fd} is not a socket: %A{other}"

            UnixPathResolution.fchmod fd 0o600 withSocket
            |> shouldEqual (Error (FChModRefusal.Socket socket))

        for fd in [ 0 ; 1 ; 2 ] do
            fchmodAnswer fd 0o600 darwin
            |> shouldEqual (SyscallAnswer.Failed UnixError.EINVAL, darwin)

            // `UnixSystem.initial` makes the launch table's pipes first, in
            // descriptor order.
            UnixPathResolution.fchmod fd 0o600 linux
            |> shouldEqual (Error (FChModRefusal.LaunchedPipe (PipeId (int64 fd))))

    // ------------------------------------------------------------- pipes

    let private pipeOrFail (system : UnixSystem<int, string>) : (int * int) * UnixSystem<int, string> =
        match UnixPipe.pipe2 0 UserBuffer.Mapped system with
        | Ok (Pipe2Answer.Created (readFd, writeFd), system) -> (readFd, writeFd), system
        | other -> failwith $"pipe2 did not make a pipe: %A{other}"

    let private fstatOrFail (fd : int) (system : UnixSystem<int, string>) : FileStatus =
        match UnixPathResolution.fstat fd system with
        | Ok (FileStatusAnswer.Reported status) -> status
        | other -> failwith $"fstat(%d{fd}): %A{other}"

    /// A process on `platform` whose clock is past its creation, holding a pipe
    /// it made as `creator`, and then acting as `caller`.
    let private withPipe
        (platform : SimulatedUnixPlatform)
        (creator : Credentials)
        (caller : Credentials)
        : (int * int) * UnixSystem<int, string>
        =
        let system =
            UnixSystem.initial<int, string> platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixSystem.withCredentials context creator

        let fds, system = pipeOrFail system

        let system =
            { system with
                Machine = UnixMachineState.advanceClock later system.Machine
            }
            |> UnixSystem.withCredentials context caller

        fds, system

    [<Test>]
    let ``Linux fchmod changes a pipe's mode through either end, as the probe measured, for every standing`` () : unit =
        // The probe's rows: root hands the pipe to an owner and group, and the
        // row's caller asks every mode of it. Here the pipe is made by that
        // owner, who then becomes the caller.
        let rows : (string * Credentials * Credentials) list =
            [
                "owner, group = egid", u1000, u1000
                "owner, group = supplementary", Credentials.ofIds (uid 1000u) (gid 2000u) [], u1000
                "owner, group not a member", Credentials.ofIds (uid 1000u) (gid 3000u) [], u1000
                "non-owner, group = egid", Credentials.ofIds (uid 1001u) (gid 1000u) [], u1000
                "non-owner, group not a member", Credentials.ofIds (uid 1001u) (gid 3000u) [], u1000
                "root, non-owner, group not a member", Credentials.ofIds (uid 1001u) (gid 3000u) [], Owners.root
            ]

        for label, creator, caller in rows do
            let (readFd, writeFd), system =
                withPipe SimulatedUnixPlatform.linuxX64 creator caller

            let standing = Standing.toward caller (InodeOwner.ofProcess creator)
            let now = UnixMachineState.realtime system.Machine
            let before = fstatOrFail readFd system

            for requested in allModes do
                let fd = if requested % 2 = 0 then readFd else writeFd
                let answer, after = fchmodAnswer fd requested system
                let readEnd = fstatOrFail readFd after
                let writeEnd = fstatOrFail writeFd after

                match reference PrivilegedModeChange.SetsRequestedBits standing requested |> ok, answer with
                | ModeChange.Forbidden, SyscallAnswer.Failed UnixError.EPERM ->
                    if after <> system then
                        failwith $"%s{label}: fchmod(0o%04o{requested}) answered EPERM but changed the system"
                | ModeChange.Permitted bits, SyscallAnswer.Completed 0L ->
                    let expected =
                        { before with
                            Mode = 0o010000 ||| PermissionBits.toInt bits
                            StatusChangeTime = now
                        }

                    (label, requested, readEnd) |> shouldEqual (label, requested, expected)

                    (label, requested, writeEnd.Mode, writeEnd.StatusChangeTime)
                    |> shouldEqual (label, requested, expected.Mode, now)
                | expected, _ ->
                    failwith $"%s{label}: fchmod(0o%04o{requested}) answered %A{answer}, expected %A{expected}"

    [<Test>]
    let ``Darwin fchmod of a pipe end is EINVAL and changes nothing`` () : unit =
        let (readFd, writeFd), system = withPipe SimulatedUnixPlatform.macOsArm64 u501 u501

        for fd in [ readFd ; writeFd ] do
            fchmodAnswer fd 0o600 system
            |> shouldEqual (SyscallAnswer.Failed UnixError.EINVAL, system)

            (fstatOrFail fd system).Mode |> shouldEqual 0o010660

    [<Test>]
    let ``Darwin fchmod refuses a privileged caller`` () : unit =
        let by = owner 501u 20u
        let system = systemOn SimulatedUnixPlatform.macOsArm64 Owners.root (tree by)
        let fd, withFd = opened "/t/f" FileAccessMode.ReadOnly system

        UnixPathResolution.fchmod fd 0o4755 withFd
        |> shouldEqual (
            Error (
                FChModRefusal.UnmeasuredModeChange (
                    inodeAt system.Machine.FileSystem "/t/f",
                    ModeChangeRefusal.UnmeasuredPrivilegedCaller (Standing.toward Owners.root by, mode 0o4755)
                )
            )
        )

    // ------------------------------------------------------------- step

    [<Test>]
    let ``chmod and fchmod through step agree with the primitives`` () : unit =
        let credentials = UnixSystem.defaultCredentials SimulatedUnixFlavour.Linux

        let system =
            systemOn SimulatedUnixPlatform.linuxX64 credentials (pathTree (InodeOwner.ofProcess credentials))

        let fd, withFd = opened "/p/f" FileAccessMode.ReadOnly system

        let answered (outcome : Result<SyscallOutcome * UnixSystem<int, string>, SyscallRefusal<int>>) =
            match outcome with
            | Ok (SyscallOutcome.Answered answer, after) -> answer, after
            | other -> failwith $"step: %A{other}"

        // A success, so the comparison covers the state the call moves, and a
        // failure.
        UnixSystem.step 1 (Syscall.ChMod (PathArg.ofPath (path "/p/lf"), 0o640)) system
        |> answered
        |> shouldEqual (chmodAnswer "/p/lf" 0o640 system)

        UnixSystem.step 1 (Syscall.ChMod (PathArg.ofPath (path "/p/dang"), 0o640)) system
        |> answered
        |> shouldEqual (chmodAnswer "/p/dang" 0o640 system)

        UnixSystem.step 1 (Syscall.FChMod (fd, 0o604)) withFd
        |> answered
        |> shouldEqual (fchmodAnswer fd 0o604 withFd)

        // And the refusals come back as the syscall's own.
        let darwin =
            systemOn SimulatedUnixPlatform.macOsArm64 Owners.root (tree (owner 501u 20u))

        let fd, withFd = opened "/t/f" FileAccessMode.ReadOnly darwin

        match UnixSystem.step 1 (Syscall.ChMod (PathArg.ofPath (path "/t/f"), 0o644)) darwin with
        | Error (SyscallRefusal.ChMod refusal) ->
            Error refusal
            |> shouldEqual (
                UnixPathResolution.chmod (PathArg.ofPath (path "/t/f")) 0o644 darwin
                |> Result.map ignore
            )
        | other -> failwith $"step chmod as Darwin root: %A{other}"

        match UnixSystem.step 1 (Syscall.FChMod (fd, 0o644)) withFd with
        | Error (SyscallRefusal.FChMod refusal) ->
            Error refusal
            |> shouldEqual (UnixPathResolution.fchmod fd 0o644 withFd |> Result.map ignore)
        | other -> failwith $"step fchmod as Darwin root: %A{other}"
