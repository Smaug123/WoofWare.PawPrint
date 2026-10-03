namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `access(2)` and `faccessat(2)`: which mode and flag words each kernel
/// accepts, and in what order it screens them against the path's copy-in and
/// the `dirfd`; how the path resolves; which IDs decide the answer; which
/// permission triple is consulted; and what root is granted.
///
/// The rows come from `docs/plans/2026-08-23-posix-kernel-extraction/access-rules.c`,
/// run on Linux 6.18.5 (aarch64, root in the container, ext4 and tmpfs) and on
/// Darwin 27.0 at uid 501; its output is beside it. Its sweeps are replayed
/// here exhaustively against the same prediction it compared the kernels
/// with, and its individually measured rows as literals against filesystems
/// built to match the ones it measured.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestAccess =

    let private context : string = "TestAccess"

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 1000

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

    let private bytes (p : string) : PathArgumentBytes =
        PathArg.ofBytes (System.Text.Encoding.ASCII.GetBytes p)

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

    /// What `access-rules.c` calls the inode it asks about.
    type private Kind =
        | File
        | Dir
        | Link

    /// `predict` from `access-rules.c`, which every kernel row was compared
    /// against: whether the call is refused EACCES. Unprivileged, every
    /// requested bit must be in the one triple the standing selects; privileged,
    /// only execute on a non-directory can be refused, and only when no execute
    /// bit at all is set.
    let private predictDenied
        (privileged : bool)
        (owns : bool)
        (inGroup : bool)
        (kind : Kind)
        (bits : int)
        (amode : int)
        : bool
        =
        if amode = 0 then
            false
        elif privileged then
            amode &&& 1 <> 0 && kind <> Kind.Dir && bits &&& 0o111 = 0
        else
            let triple =
                if owns then (bits >>> 6) &&& 7
                elif inGroup then (bits >>> 3) &&& 7
                else bits &&& 7

            amode &&& 7 &&& ~~~triple <> 0

    // ------------------------------------------------------------- the permission rule

    let private contentOf (kind : Kind) (bits : PermissionBits) : InodeContent =
        match kind with
        | Kind.File -> InodeContent.RegularFile (ImmutableArray.Empty, bits)
        | Kind.Dir ->
            InodeContent.Directory
                {
                    Entries = Map.empty
                    Parent = VirtualFileSystem.root (VirtualFileSystem.empty epoch (owner 0u 0u))
                    Permissions = bits
                }
        | Kind.Link -> InodeContent.Symlink (SymlinkTarget.parseOrFail context "target")

    [<Test>]
    let ``denied answers the probe's prediction for every standing, mode, question and kind`` () : unit =
        for rule in [ PrivilegedExecution.NeedsAnExecuteBit ; PrivilegedExecution.Unmeasured ] do
            for standing in allStandings do
                for kind in [ Kind.File ; Kind.Dir ; Kind.Link ] do
                    for raw in 0..0o7777 do
                        let bits = mode raw
                        let content = contentOf kind bits

                        for amode in 0..7 do
                            let privileged = standing.Privilege = CallerPrivilege.Privileged

                            let expected =
                                if
                                    rule = PrivilegedExecution.Unmeasured
                                    && privileged
                                    && amode &&& 1 <> 0
                                    && kind <> Kind.Dir
                                then
                                    Error (ExecutionRefusal.UnmeasuredPrivilegedCaller (standing, bits))
                                else
                                    Ok (predictDenied privileged standing.Owns standing.InGroup kind raw amode)

                            let actual =
                                AccessRules.denied rule standing content bits (AccessQuestion.ofLowBits amode)

                            if actual <> expected then
                                failwith
                                    $"%O{rule}, %O{standing}, %A{kind} 0o%04o{raw}, amode %d{amode}: denied said %A{actual}, the probe's rule %A{expected}"

    [<Test>]
    let ``root's execute rows, as measured`` () : unit =
        // `ownership-probe.c`'s ACCESS-X rows, Linux 6.18.5: a real and
        // effective root, on a regular file root does not own.
        let root : Standing =
            {
                Privilege = CallerPrivilege.Privileged
                Owns = false
                InGroup = false
            }

        for bits, denied in [ 0o644, true ; 0o100, false ; 0o010, false ; 0o001, false ; 0o000, true ] do
            PermissionBits.executionDenied PrivilegedExecution.NeedsAnExecuteBit root (mode bits)
            |> shouldEqual (Ok denied)

            PermissionBits.executionDenied PrivilegedExecution.Unmeasured root (mode bits)
            |> shouldEqual (Error (ExecutionRefusal.UnmeasuredPrivilegedCaller (root, mode bits)))

        // A directory with no bits at all is still searchable by root.
        AccessRules.denied
            PrivilegedExecution.NeedsAnExecuteBit
            root
            (contentOf Kind.Dir (mode 0))
            (mode 0)
            (AccessQuestion.ofLowBits 1)
        |> shouldEqual (Ok false)

    [<Test>]
    let ``each platform names its flavour's privileged execution rule`` () : unit =
        for platform, expected in
            [
                SimulatedUnixPlatform.linuxX64, PrivilegedExecution.NeedsAnExecuteBit
                SimulatedUnixPlatform.linuxArm64, PrivilegedExecution.NeedsAnExecuteBit
                SimulatedUnixPlatform.macOsArm64, PrivilegedExecution.Unmeasured
            ] do
            SimulatedUnixPlatform.privilegedExecution platform |> shouldEqual expected

    // ------------------------------------------------------------- which IDs

    [<Test>]
    let ``realIdsAsEffective puts the real IDs where the effective ones were, and touches nothing else`` () : unit =
        let property (credentials : Credentials) : unit =
            let checking = Credentials.realIdsAsEffective credentials

            checking
            |> shouldEqual
                { credentials with
                    EffectiveUser = credentials.RealUser
                    EffectiveGroup = credentials.RealGroup
                }

        Check.One (config, Prop.forAll (Arb.fromGen CredentialsGen.credentials) property)

    [<Test>]
    let ``the standing realIdsAsEffective gives is the one the real IDs select`` () : unit =
        // A naive oracle, written from the probe's rule rather than from
        // `Standing.toward`: privileged exactly when the real uid is 0, the
        // owner exactly when the real uid is the inode's, and in its group when
        // the real gid or a supplementary group is.
        let property (credentials : Credentials) (inode : InodeOwner) : unit =
            let standing = Standing.toward (Credentials.realIdsAsEffective credentials) inode

            standing
            |> shouldEqual
                {
                    Privilege =
                        if credentials.RealUser = UserId.root then
                            CallerPrivilege.Privileged
                        else
                            CallerPrivilege.Unprivileged
                    Owns = inode.User = credentials.RealUser
                    InGroup =
                        inode.Group = credentials.RealGroup
                        || List.contains inode.Group credentials.SupplementaryGroups
                }

        let inodeOwner : Gen<InodeOwner> =
            Gen.map2
                (fun user group ->
                    {
                        User = user
                        Group = group
                    }
                )
                CredentialsGen.userId
                CredentialsGen.groupId

        Check.One (
            config,
            Prop.forAll (Arb.fromGen (Gen.zip CredentialsGen.credentials inodeOwner)) (fun (c, o) -> property c o)
        )

    // ------------------------------------------------------------- the argument screens

    [<Test>]
    let ``atDirectory knows each flavour's AT_FDCWD and no other`` () : unit =
        // CONST rows: AT_FDCWD is -100 on Linux and -2 on Darwin.
        AccessRules.atDirectory SimulatedUnixFlavour.Linux -100
        |> shouldEqual AtDirectory.CurrentDirectory

        AccessRules.atDirectory SimulatedUnixFlavour.Linux -2
        |> shouldEqual (AtDirectory.Descriptor -2)

        AccessRules.atDirectory SimulatedUnixFlavour.Darwin -2
        |> shouldEqual AtDirectory.CurrentDirectory

        AccessRules.atDirectory SimulatedUnixFlavour.Darwin -100
        |> shouldEqual (AtDirectory.Descriptor -100)

        for flavour in [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ] do
            for fd in [ -1 ; 0 ; 3 ; 12345 ] do
                AccessRules.atDirectory flavour fd |> shouldEqual (AtDirectory.Descriptor fd)

    /// Whether the screen let the word through, failed it, or refused it, as
    /// the probe could tell: EINVAL or not. The probe asked every flag bit
    /// against F_OK on a 0777 file; a refused flag is one the kernel accepted.
    let private screenedAs (screen : AccessScreen) : string =
        match screen with
        | AccessScreen.Screened _ -> "ok"
        | AccessScreen.Failed error -> $"%O{error}"
        | AccessScreen.Refused _ -> "refused"

    [<Test>]
    let ``every single bit of the mode word screens as measured`` () : unit =
        // MODE-BIT rows: Linux accepts bits 0 to 2 and answers EINVAL for every
        // other; Darwin accepts every word.
        for bit in 0..31 do
            let word = 1 <<< bit

            AccessRules.screen SimulatedUnixFlavour.Linux word 0
            |> screenedAs
            |> shouldEqual (if bit < 3 then "ok" else "EINVAL")

            AccessRules.screen SimulatedUnixFlavour.Darwin word 0
            |> screenedAs
            |> shouldEqual "ok"

        for word in [ -1 ; 0x7FFFFFFF ; 0x8 ; 0xF ] do
            AccessRules.screen SimulatedUnixFlavour.Linux word 0
            |> screenedAs
            |> shouldEqual "EINVAL"

    [<Test>]
    let ``every single bit of the flag word screens as measured`` () : unit =
        // FLAG-BIT rows. Linux accepts AT_SYMLINK_NOFOLLOW (bit 8),
        // AT_EACCESS (9) and AT_EMPTY_PATH (12). Darwin accepts AT_EACCESS
        // (4) and AT_SYMLINK_NOFOLLOW (5), and also AT_SYMLINK_NOFOLLOW_ANY
        // (11), AT_RESOLVE_BENEATH (13) and AT_UNIQUE (15), whose meaning is
        // not modelled.
        for bit in 0..31 do
            let word = 1 <<< bit

            AccessRules.screen SimulatedUnixFlavour.Linux 0 word
            |> screenedAs
            |> shouldEqual (if List.contains bit [ 8 ; 9 ; 12 ] then "ok" else "EINVAL")

            AccessRules.screen SimulatedUnixFlavour.Darwin 0 word
            |> screenedAs
            |> shouldEqual (
                if List.contains bit [ 4 ; 5 ] then "ok"
                elif List.contains bit [ 11 ; 13 ; 15 ] then "refused"
                else "EINVAL"
            )

    [<Test>]
    let ``Linux screens the mode word before the flag word`` () : unit =
        // ORDER: a bad mode with bad flags is EINVAL either way, so the order
        // shows only in that both are EINVAL ahead of everything after them;
        // what matters here is that a good mode with bad flags still fails.
        AccessRules.screen SimulatedUnixFlavour.Linux 8 0x40000000
        |> shouldEqual (AccessScreen.Failed UnixError.EINVAL)

        AccessRules.screen SimulatedUnixFlavour.Linux 0 0x40000000
        |> shouldEqual (AccessScreen.Failed UnixError.EINVAL)

    [<Test>]
    let ``what a screened word says`` () : unit =
        let property (flavour : SimulatedUnixFlavour) (word : int) (flagChoice : bool * bool * bool) : unit =
            let noFollow, eAccess, emptyPath = flagChoice

            let flags =
                match flavour with
                | SimulatedUnixFlavour.Linux ->
                    (if noFollow then 0x100 else 0)
                    ||| (if eAccess then 0x200 else 0)
                    ||| (if emptyPath then 0x1000 else 0)
                | SimulatedUnixFlavour.Darwin -> (if noFollow then 0x20 else 0) ||| (if eAccess then 0x10 else 0)

            let expected : AccessScreen =
                match flavour with
                | SimulatedUnixFlavour.Linux when word &&& ~~~7 <> 0 -> AccessScreen.Failed UnixError.EINVAL
                | _ ->
                    AccessScreen.Screened
                        {
                            Question =
                                {
                                    Read = word &&& 4 <> 0
                                    Write = word &&& 2 <> 0
                                    Execute = word &&& 1 <> 0
                                }
                            ExtendedRights =
                                match flavour with
                                | SimulatedUnixFlavour.Linux -> 0
                                | SimulatedUnixFlavour.Darwin -> word &&& 0x3FFE00
                            Ids = if eAccess then AccessIds.Effective else AccessIds.Real
                            FinalSymlink =
                                if noFollow then
                                    SymlinkPolicy.NoFollowFinal
                                else
                                    SymlinkPolicy.Follow
                            EmptyPath =
                                match flavour with
                                | SimulatedUnixFlavour.Linux when emptyPath -> AccessEmptyPath.NamesStartingPoint
                                | SimulatedUnixFlavour.Linux -> AccessEmptyPath.NoSuchEntryBeforeDescriptor
                                | SimulatedUnixFlavour.Darwin -> AccessEmptyPath.NoSuchEntryAfterDescriptor
                        }

            AccessRules.screen flavour word flags |> shouldEqual expected

        let words : Gen<int> =
            Gen.oneof [ Gen.choose (0, 7) ; ArbMap.defaults |> ArbMap.generate<int> ]

        let flavours =
            Gen.elements [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]

        let choices = ArbMap.defaults |> ArbMap.generate<bool * bool * bool>

        Check.One (
            config,
            Prop.forAll (Arb.fromGen (Gen.zip3 flavours words choices)) (fun (f, w, c) -> property f w c)
        )

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

    let private symlink (p : string) (by : InodeOwner) (target : string) (vfs : VirtualFileSystem) : VirtualFileSystem =
        let parent, child = parentAndName p

        VirtualFileSystem.createSymlink
            (inodeAt vfs parent)
            (name child)
            by
            epoch
            (SymlinkTarget.parseOrFail context target)
            vfs
        |> ok
        |> snd

    /// A process on `platform` with `credentials`, in the directory `cwd` of
    /// `vfs`.
    let private systemOn
        (platform : SimulatedUnixPlatform)
        (credentials : Credentials)
        (cwd : string)
        (vfs : VirtualFileSystem)
        : UnixSystem<int, string>
        =
        let system : UnixSystem<int, string> =
            UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.withCredentials context credentials
            |> UnixBootImage.boot

        { system with
            Machine =
                { system.Machine with
                    FileSystem = vfs
                }
            Process =
                { system.Process with
                    CurrentDirectoryInode = inodeAt vfs cwd
                }
        }

    /// What an `access`-family call answered, as the probe printed it.
    let private answered (result : Result<SyscallAnswer, AccessRefusal>) : string =
        match result with
        | Ok (SyscallAnswer.Completed 0L) -> "ok"
        | Ok (SyscallAnswer.Failed error) -> string<UnixError> error
        | Ok (SyscallAnswer.Completed other) ->
            failwith $"access answered %d{other}, which is not a value it can return"
        | Error refusal -> failwith $"access was refused: %s{AccessRefusal.describe refusal}"

    let private grantedAnswer : Result<SyscallAnswer, AccessRefusal> =
        Ok (SyscallAnswer.Completed 0L)

    let private deniedAnswer : Result<SyscallAnswer, AccessRefusal> =
        Ok (SyscallAnswer.Failed UnixError.EACCES)

    let private linuxAtFdCwd : int = -100
    let private darwinAtFdCwd : int = -2

    // ------------------------------------------------------------- Linux: credentials, end to end

    /// One row of the probe's credential sweep: real and effective user and
    /// group, and the supplementary groups. The probe set the saved IDs to the
    /// effective ones.
    type private CredentialRow =
        {
            Label : string
            RealUser : uint32
            EffectiveUser : uint32
            RealGroup : uint32
            EffectiveGroup : uint32
            Groups : uint32 list
        }

    let private credentialRows : CredentialRow list =
        let row label ruid euid rgid egid groups =
            {
                Label = label
                RealUser = ruid
                EffectiveUser = euid
                RealGroup = rgid
                EffectiveGroup = egid
                Groups = groups
            }

        [
            row "owner" 1001u 1001u 3000u 3000u []
            row "group-primary" 1000u 1000u 2000u 2000u []
            row "group-supplementary" 1000u 1000u 1000u 1000u [ 2000u ]
            row "other" 1000u 1000u 1000u 1000u [ 3000u ]
            row "root" 0u 0u 0u 0u [ 0u ]
            row "real-root/effective-other" 0u 1000u 1000u 1000u []
            row "real-other/effective-root" 1000u 0u 1000u 1000u []
            row "real-owner/effective-other" 1001u 1000u 1000u 1000u []
            row "real-other/effective-owner" 1000u 1001u 1000u 1000u []
            row "real-group/effective-other" 1000u 1000u 2000u 1000u []
            row "real-other/effective-group" 1000u 1000u 1000u 2000u []
            row "real-root-group/effective-other" 1000u 1000u 0u 1000u []
        ]

    let private credentialsOf (row : CredentialRow) : Credentials =
        {
            RealUser = uid row.RealUser
            EffectiveUser = uid row.EffectiveUser
            SavedUser = uid row.EffectiveUser
            RealGroup = gid row.RealGroup
            EffectiveGroup = gid row.EffectiveGroup
            SavedGroup = gid row.EffectiveGroup
            SupplementaryGroups = row.Groups |> List.map gid
        }

    /// The probe's `standing`: privilege, ownership and membership, from the
    /// row's real IDs for `access(2)` or its effective ones for `AT_EACCESS`,
    /// towards the sweep's owner 1001 and group 2000.
    let private probeStanding (row : CredentialRow) (effective : bool) : bool * bool * bool =
        let user = if effective then row.EffectiveUser else row.RealUser
        let group = if effective then row.EffectiveGroup else row.RealGroup
        user = 0u, user = 1001u, group = 2000u || List.contains 2000u row.Groups

    /// Every permission word, which is where the triple is chosen; the special
    /// bits play no part, as the probe's full 4096-mode sweep found and as the
    /// exhaustive rule test above checks.
    let private sweptModes : int list = [ 0..0o777 ]

    /// `/creds`, as the probe built it under its base: a file and a directory
    /// for every swept mode and a link, each owned by 1001:2000, and a 0700
    /// directory of 1001's holding a 0666 file.
    let private credentialTree : VirtualFileSystem =
        let mutable vfs =
            VirtualFileSystem.empty epoch (owner 0u 0u)
            |> directory "/creds" (owner 0u 0u) 0o755

        for bits in sweptModes do
            vfs <-
                vfs
                |> file $"/creds/f_%04o{bits}" (owner 1001u 2000u) bits
                |> directory $"/creds/d_%04o{bits}" (owner 1001u 2000u) bits

        vfs
        |> symlink "/creds/link" (owner 1001u 2000u) "f_0000"
        |> directory "/creds/walk_owner_0700" (owner 1001u 2000u) 0o700
        |> file "/creds/walk_owner_0700/f" (owner 0u 0u) 0o666

    [<Test>]
    let ``Linux access and AT_EACCESS answer every mode as the probe measured, for every credential row`` () : unit =
        for row in credentialRows do
            let system =
                systemOn SimulatedUnixPlatform.linuxX64 (credentialsOf row) "/creds" credentialTree

            for kind in [ Kind.File ; Kind.Dir ; Kind.Link ] do
                let targets =
                    match kind with
                    | Kind.File -> sweptModes |> List.map (fun bits -> $"/creds/f_%04o{bits}", bits)
                    | Kind.Dir -> sweptModes |> List.map (fun bits -> $"/creds/d_%04o{bits}", bits)
                    // A Linux link's mode is always 0777.
                    | Kind.Link -> [ "/creds/link", 0o777 ]

                for target, bits in targets do
                    for amode in 0..7 do
                        for effective in [ false ; true ] do
                            let privileged, owns, inGroup = probeStanding row effective

                            let expected =
                                if predictDenied privileged owns inGroup kind bits amode then
                                    deniedAnswer
                                else
                                    grantedAnswer

                            let flags =
                                (if effective then 0x200 else 0) ||| (if kind = Kind.Link then 0x100 else 0)

                            let actual =
                                if effective || kind = Kind.Link then
                                    UnixPathResolution.faccessat linuxAtFdCwd (bytes target) amode flags system
                                else
                                    UnixPathResolution.access (bytes target) amode system

                            // Compared as values rather than through `answered`,
                            // whose rendering of an errno costs more than the
                            // call it renders.
                            if actual <> expected then
                                let call = if effective then "AT_EACCESS" else "access"

                                failwith
                                    $"%s{row.Label}: %s{target}, amode %d{amode}, %s{call}: answered %s{answered actual}, the probe measured %s{answered expected}"

    [<Test>]
    let ``Linux access walks the path with the real IDs, and AT_EACCESS with the effective ones`` () : unit =
        // WALK rows: a 0700 directory of 1001's, holding a file anyone may
        // read, asked F_OK.
        let rows : (string * string * string) list =
            [
                "owner", "ok", "ok"
                "group-primary", "EACCES", "EACCES"
                "group-supplementary", "EACCES", "EACCES"
                "other", "EACCES", "EACCES"
                "root", "ok", "ok"
                "real-root/effective-other", "ok", "EACCES"
                "real-other/effective-root", "EACCES", "ok"
                "real-owner/effective-other", "ok", "EACCES"
                "real-other/effective-owner", "EACCES", "ok"
                "real-group/effective-other", "EACCES", "EACCES"
                "real-other/effective-group", "EACCES", "EACCES"
                "real-root-group/effective-other", "EACCES", "EACCES"
            ]

        for label, viaAccess, viaEAccess in rows do
            let row = credentialRows |> List.find (fun row -> row.Label = label)

            let system =
                systemOn SimulatedUnixPlatform.linuxX64 (credentialsOf row) "/creds" credentialTree

            let target = bytes "walk_owner_0700/f"

            (label,
             UnixPathResolution.access target 0 system |> answered,
             UnixPathResolution.faccessat linuxAtFdCwd target 0 0x200 system |> answered)
            |> shouldEqual (label, viaAccess, viaEAccess)

    // ------------------------------------------------------------- Darwin, end to end

    /// uid 501 in groups 20 and 12: the probe's Darwin caller.
    let private u501 : Credentials =
        Credentials.ofIds (uid 501u) (gid 20u) [ gid 20u ; gid 12u ]

    [<Test>]
    let ``Darwin access answers every mode as the probe measured, for the owner`` () : unit =
        // OWN-SWEEP: uid 501's own file and directory, through access and
        // AT_EACCESS alike.
        let mutable vfs =
            VirtualFileSystem.empty epoch (owner 0u 0u)
            |> directory "/own" (owner 501u 20u) 0o755

        for bits in sweptModes do
            vfs <-
                vfs
                |> file $"/own/f_%04o{bits}" (owner 501u 20u) bits
                |> directory $"/own/d_%04o{bits}" (owner 501u 20u) bits

        let system = systemOn SimulatedUnixPlatform.macOsArm64 u501 "/own" vfs

        for kind in [ Kind.File ; Kind.Dir ] do
            for bits in sweptModes do
                let target =
                    match kind with
                    | Kind.File -> $"f_%04o{bits}"
                    | _ -> $"d_%04o{bits}"

                for amode in 0..7 do
                    let expected =
                        if predictDenied false true true kind bits amode then
                            deniedAnswer
                        else
                            grantedAnswer

                    let viaAccess = UnixPathResolution.access (bytes target) amode system

                    let viaEAccess =
                        UnixPathResolution.faccessat darwinAtFdCwd (bytes target) amode 0x10 system

                    if viaAccess <> expected || viaEAccess <> expected then
                        failwith
                            $"%s{target}, amode %d{amode}: access answered %s{answered viaAccess} and AT_EACCESS %s{answered viaEAccess}, the probe measured %s{answered expected}"

    [<Test>]
    let ``Darwin refuses root's execute question on a non-directory, and answers the rest`` () : unit =
        let vfs =
            VirtualFileSystem.empty epoch (owner 0u 0u)
            |> directory "/t" (owner 0u 0u) 0o755
            |> file "/t/f" (owner 501u 20u) 0o755
            |> directory "/t/d" (owner 501u 20u) 0o000
            |> symlink "/t/l" (owner 501u 20u) "f"

        let system = systemOn SimulatedUnixPlatform.macOsArm64 Owners.root "/t" vfs
        let rootStanding = Standing.toward Owners.root (owner 501u 20u)

        UnixPathResolution.access (bytes "f") 1 system
        |> shouldEqual (
            Error (
                AccessRefusal.UnmeasuredExecution (
                    inodeAt vfs "/t/f",
                    ExecutionRefusal.UnmeasuredPrivilegedCaller (rootStanding, mode 0o755)
                )
            )
        )

        // Through a link it is the target that is asked about; of the link
        // itself, it is the link.
        UnixPathResolution.access (bytes "l") 5 system
        |> shouldEqual (
            Error (
                AccessRefusal.UnmeasuredExecution (
                    inodeAt vfs "/t/f",
                    ExecutionRefusal.UnmeasuredPrivilegedCaller (rootStanding, mode 0o755)
                )
            )
        )

        UnixPathResolution.faccessat darwinAtFdCwd (bytes "l") 1 0x20 system
        |> shouldEqual (
            Error (
                AccessRefusal.UnmeasuredExecution (
                    inodeAt vfs "/t/l",
                    ExecutionRefusal.UnmeasuredPrivilegedCaller (
                        rootStanding,
                        SimulatedUnixPlatform.symlinkPermissions SimulatedUnixPlatform.macOsArm64
                    )
                )
            )
        )

        // Search is not execution: root searching a directory is the same
        // question every path walk asks.
        UnixPathResolution.access (bytes "d") 1 system |> answered |> shouldEqual "ok"
        // The path is resolved before the question is refused.
        UnixPathResolution.access (bytes "nx") 1 system
        |> answered
        |> shouldEqual "ENOENT"

    [<Test>]
    let ``Darwin's extended rights are refused once the path resolves, and its other high bits ignored`` () : unit =
        let vfs =
            VirtualFileSystem.empty epoch (owner 501u 20u)
            |> file "/f" (owner 501u 20u) 0o777
            |> file "/z" (owner 501u 20u) 0o000

        let system = systemOn SimulatedUnixPlatform.macOsArm64 u501 "/" vfs

        // ORDER: bit 9 and bit 21 against a path that does not resolve.
        UnixPathResolution.access (bytes "/nx") (1 <<< 9) system
        |> answered
        |> shouldEqual "ENOENT"

        UnixPathResolution.access (bytes "/nx") (1 <<< 21) system
        |> answered
        |> shouldEqual "ENOENT"

        UnixPathResolution.access (bytes "f/x") (1 <<< 21) system
        |> answered
        |> shouldEqual "ENOTDIR"

        for bit in 9..21 do
            UnixPathResolution.access (bytes "f") ((1 <<< bit) ||| 4) system
            |> shouldEqual (Error (AccessRefusal.ExtendedRights (inodeAt vfs "/f", 1 <<< bit)))

        // MODE-BIT and IGNORED-SWEEP: the rest change nothing.
        for bit in List.append [ 3..8 ] [ 22..31 ] do
            UnixPathResolution.access (bytes "z") (1 <<< bit) system
            |> answered
            |> shouldEqual "ok"

            UnixPathResolution.access (bytes "z") ((1 <<< bit) ||| 4) system
            |> answered
            |> shouldEqual "EACCES"

    [<Test>]
    let ``Darwin refuses the flags it accepts and this library does not model`` () : unit =
        let system =
            systemOn
                SimulatedUnixPlatform.macOsArm64
                u501
                "/"
                (VirtualFileSystem.empty epoch (owner 501u 20u)
                 |> file "/f" (owner 501u 20u) 0o777)

        for flag in [ 0x800 ; 0x2000 ; 0x8000 ] do
            UnixPathResolution.faccessat darwinAtFdCwd (bytes "/f") 0 flag system
            |> shouldEqual (Error (AccessRefusal.UnmodelledFlags flag))

    // ------------------------------------------------------------- paths

    /// `pd` as the probe built it: a 0644 file, a 0755 directory, and links to
    /// each, a dangling one and a cyclic one, all the caller's.
    let private pathTree (by : InodeOwner) : VirtualFileSystem =
        VirtualFileSystem.empty epoch by
        |> directory "/pd" by 0o755
        |> file "/pd/f" by 0o644
        |> directory "/pd/sub" by 0o755
        |> symlink "/pd/lf" by "f"
        |> symlink "/pd/ld" by "sub"
        |> symlink "/pd/dang" by "nx"
        |> symlink "/pd/cyc" by "cyc"

    [<Test>]
    let ``every path the probe asked about answers as it measured, on both flavours`` () : unit =
        // PATH rows, which agreed on both flavours row for row: access(F_OK),
        // access(W_OK), and AT_SYMLINK_NOFOLLOW with each. Darwin answered
        // "/" W_OK with EROFS, its system volume being read-only; this
        // library's root is writable, so that row is left out.
        let longName = System.String ('a', 299)

        let rows : (string * string * string * string * string) list =
            [
                "f", "ok", "ok", "ok", "ok"
                "f/", "ENOTDIR", "ENOTDIR", "ENOTDIR", "ENOTDIR"
                "f/.", "ENOTDIR", "ENOTDIR", "ENOTDIR", "ENOTDIR"
                "f/x", "ENOTDIR", "ENOTDIR", "ENOTDIR", "ENOTDIR"
                "sub", "ok", "ok", "ok", "ok"
                "sub/", "ok", "ok", "ok", "ok"
                "sub/.", "ok", "ok", "ok", "ok"
                "sub/..", "ok", "ok", "ok", "ok"
                "lf", "ok", "ok", "ok", "ok"
                "lf/", "ENOTDIR", "ENOTDIR", "ENOTDIR", "ENOTDIR"
                "ld", "ok", "ok", "ok", "ok"
                "ld/", "ok", "ok", "ok", "ok"
                "dang", "ENOENT", "ENOENT", "ok", "ok"
                "dang/", "ENOENT", "ENOENT", "ENOENT", "ENOENT"
                "cyc", "ELOOP", "ELOOP", "ok", "ok"
                "cyc/", "ELOOP", "ELOOP", "ELOOP", "ELOOP"
                "nx", "ENOENT", "ENOENT", "ENOENT", "ENOENT"
                "nx/", "ENOENT", "ENOENT", "ENOENT", "ENOENT"
                ".", "ok", "ok", "ok", "ok"
                "..", "ok", "ok", "ok", "ok"
                longName, "ENAMETOOLONG", "ENAMETOOLONG", "ENAMETOOLONG", "ENAMETOOLONG"
            ]

        // The probe ran as root on Linux and as the owner of everything on
        // Darwin; each is asked here as it was measured.
        for platform, credentials, noFollow, atFdCwd in
            [
                SimulatedUnixPlatform.linuxX64, Owners.root, 0x100, linuxAtFdCwd
                SimulatedUnixPlatform.macOsArm64, u501, 0x20, darwinAtFdCwd
            ] do
            let system =
                systemOn platform credentials "/pd" (pathTree (InodeOwner.ofProcess credentials))

            for p, existsAnswer, writeAnswer, existsNoFollow, writeNoFollow in rows do
                let label = if p = longName then "<299 a>" else p

                (label,
                 UnixPathResolution.access (bytes p) 0 system |> answered,
                 UnixPathResolution.access (bytes p) 2 system |> answered,
                 UnixPathResolution.faccessat atFdCwd (bytes p) 0 noFollow system |> answered,
                 UnixPathResolution.faccessat atFdCwd (bytes p) 2 noFollow system |> answered)
                |> shouldEqual (label, existsAnswer, writeAnswer, existsNoFollow, writeNoFollow)

    [<Test>]
    let ``a Linux symbolic link asked about itself is granted everything, whoever asks`` () : unit =
        // CRED-SWEEP's symlink rows: a link's mode is 0777 on Linux, so every
        // standing is granted every question, root's execute included.
        let vfs =
            VirtualFileSystem.empty epoch (owner 0u 0u)
            |> symlink "/l" (owner 1001u 2000u) "nx"

        for credentials in [ Owners.root ; Credentials.ofIds (uid 1000u) (gid 1000u) [] ] do
            let system = systemOn SimulatedUnixPlatform.linuxX64 credentials "/" vfs

            for amode in 0..7 do
                UnixPathResolution.faccessat linuxAtFdCwd (bytes "/l") amode 0x100 system
                |> answered
                |> shouldEqual "ok"

    // ------------------------------------------------------------- the dirfd and the order of the screens

    /// The probe's base: a 0777 file and a 0000 file of the caller's.
    let private screenTree (by : InodeOwner) : VirtualFileSystem =
        VirtualFileSystem.empty epoch by
        |> directory "/b" by 0o755
        |> file "/b/screen_f" by 0o777
        |> file "/b/screen_z" by 0o000

    let private plainOpen : OpenFlags =
        {
            Access = FileAccessMode.ReadOnly
            Create = false
            Exclusive = false
            Truncate = false
            NoFollow = false
            CloseOnExec = false
            Synchronous = false
            Directory = false
        }

    let private openOrFail (p : string) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match Answered.openPath plainOpen (path p) 0 system with
        | SyscallAnswer.Completed fd, system -> int fd, system
        | other -> failwith $"open(%s{p}) did not open: %O{other}"

    /// The probe's base as the current directory, with a descriptor open on it
    /// and one on its 0777 file.
    let private screenSystem
        (platform : SimulatedUnixPlatform)
        (credentials : Credentials)
        : int * int * UnixSystem<int, string>
        =
        let system =
            systemOn platform credentials "/b" (screenTree (InodeOwner.ofProcess credentials))

        let directoryFd, system = openOrFail "/b" system
        let fileFd, system = openOrFail "/b/screen_f" system
        directoryFd, fileFd, system

    [<Test>]
    let ``every dirfd the probe tried answers as it measured`` () : unit =
        // DIRFD rows: an absolute path, a relative one, a relative absent one,
        // the empty path, and (Linux) the empty path with AT_EMPTY_PATH asking
        // F_OK and W_OK.
        let linuxDirectory, linuxFile, linux =
            screenSystem SimulatedUnixPlatform.linuxX64 Owners.root

        let linuxRows : (string * int * string list) list =
            [
                "AT_FDCWD", -100, [ "ok" ; "ok" ; "ENOENT" ; "ENOENT" ; "ok" ; "ok" ]
                "-1", -1, [ "ok" ; "EBADF" ; "EBADF" ; "ENOENT" ; "EBADF" ; "EBADF" ]
                "12345 (closed)", 12345, [ "ok" ; "EBADF" ; "EBADF" ; "ENOENT" ; "EBADF" ; "EBADF" ]
                "-100", -100, [ "ok" ; "ok" ; "ENOENT" ; "ENOENT" ; "ok" ; "ok" ]
                "-2", -2, [ "ok" ; "EBADF" ; "EBADF" ; "ENOENT" ; "EBADF" ; "EBADF" ]
                "a directory", linuxDirectory, [ "ok" ; "ok" ; "ENOENT" ; "ENOENT" ; "ok" ; "ok" ]
                "a file", linuxFile, [ "ok" ; "ENOTDIR" ; "ENOTDIR" ; "ENOENT" ; "ok" ; "ok" ]
            ]

        for label, dirfd, expected in linuxRows do
            let ask (p : string) (amode : int) (flags : int) =
                UnixPathResolution.faccessat dirfd (bytes p) amode flags linux |> answered

            (label,
             [
                 ask "/b/screen_f" 0 0
                 ask "screen_f" 0 0
                 ask "screen_nx" 0 0
                 ask "" 0 0
                 ask "" 0 0x1000
                 ask "" 2 0x1000
             ])
            |> shouldEqual (label, expected)

        let darwinDirectory, darwinFile, darwin =
            screenSystem SimulatedUnixPlatform.macOsArm64 u501

        let darwinRows : (string * int * string list) list =
            [
                "AT_FDCWD", -2, [ "ok" ; "ok" ; "ENOENT" ; "ENOENT" ]
                "-1", -1, [ "ok" ; "EBADF" ; "EBADF" ; "EBADF" ]
                "12345 (closed)", 12345, [ "ok" ; "EBADF" ; "EBADF" ; "EBADF" ]
                "-100", -100, [ "ok" ; "EBADF" ; "EBADF" ; "EBADF" ]
                "-2", -2, [ "ok" ; "ok" ; "ENOENT" ; "ENOENT" ]
                "a directory", darwinDirectory, [ "ok" ; "ok" ; "ENOENT" ; "ENOENT" ]
                "a file", darwinFile, [ "ok" ; "ENOTDIR" ; "ENOTDIR" ; "ENOTDIR" ]
            ]

        for label, dirfd, expected in darwinRows do
            let ask (p : string) =
                UnixPathResolution.faccessat dirfd (bytes p) 0 0 darwin |> answered

            (label, [ ask "/b/screen_f" ; ask "screen_f" ; ask "screen_nx" ; ask "" ])
            |> shouldEqual (label, expected)

    [<Test>]
    let ``the screens, the copy-in and the dirfd fall in the order the probe measured`` () : unit =
        // ORDER and COPYIN rows. "bad" is mode 8, flags 0x40000000 and dirfd
        // 12345; NULL is an unreadable pointer; PATH_MAX bytes is that many
        // with no NUL inside.
        let row
            (system : UnixSystem<int, string>)
            (dirfd : int)
            (p : PathArgumentBytes)
            (amode : int)
            (flags : int)
            : string
            =
            UnixPathResolution.faccessat dirfd p amode flags system |> answered

        let tooLong (platform : SimulatedUnixPlatform) : PathArgumentBytes =
            let length = PathLimits.pathMaxBytes (SimulatedUnixPlatform.pathLimits platform)
            bytes (System.String ('a', length))

        let _, linuxFile, linux = screenSystem SimulatedUnixPlatform.linuxX64 Owners.root
        let linuxTooLong = tooLong SimulatedUnixPlatform.linuxX64
        let nx = bytes "/b/screen_nx"
        let f = bytes "/b/screen_f"
        let null' = PathArgumentBytes.Unreadable

        [
            UnixPathResolution.access nx 8 linux |> answered
            row linux -100 nx 8 0
            row linux -100 nx 0 0x40000000
            row linux -100 f 8 0x40000000
            row linux 12345 (bytes "screen_f") 8 0
            row linux 12345 (bytes "screen_f") 0 0x40000000
            row linux 12345 (bytes "nx") 0 0
            UnixPathResolution.access (bytes "") 8 linux |> answered
            UnixPathResolution.access null' 8 linux |> answered
            UnixPathResolution.access null' 0 linux |> answered
            row linux -100 null' 0 0
            row linux -100 null' 8 0
            row linux -100 null' 0 0x40000000
            row linux 12345 null' 0 0
            row linux linuxFile null' 0 0
            row linux -100 linuxTooLong 0 0
            row linux -100 linuxTooLong 8 0
            row linux -100 linuxTooLong 0 0x40000000
            row linux 12345 linuxTooLong 0 0
            row linux -100 (bytes "") 8 0
            row linux -100 (bytes "") 0 0x40000000
            row linux 12345 (bytes "") 0 0
            row linux -100 null' 0 0x1000
            row linux linuxFile null' 0 0x1000
            row linux 12345 null' 0 0x1000
            row linux linuxFile (bytes "screen_f") 0 0x1000
            row linux linuxFile f 0 0x1000
        ]
        |> shouldEqual
            [
                "EINVAL"
                "EINVAL"
                "EINVAL"
                "EINVAL"
                "EINVAL"
                "EINVAL"
                "EBADF"
                "EINVAL"
                "EINVAL"
                "EFAULT"
                "EFAULT"
                "EINVAL"
                "EINVAL"
                "EFAULT"
                "EFAULT"
                "ENAMETOOLONG"
                "EINVAL"
                "EINVAL"
                "ENAMETOOLONG"
                "EINVAL"
                "EINVAL"
                "ENOENT"
                "EFAULT"
                "EFAULT"
                "EFAULT"
                "ENOTDIR"
                "ok"
            ]

        let _, darwinFile, darwin = screenSystem SimulatedUnixPlatform.macOsArm64 u501
        let darwinTooLong = tooLong SimulatedUnixPlatform.macOsArm64

        [
            UnixPathResolution.access nx 8 darwin |> answered
            row darwin -2 nx 8 0
            row darwin -2 nx 0 0x40000000
            row darwin -2 f 8 0x40000000
            row darwin 12345 (bytes "screen_f") 8 0
            row darwin 12345 (bytes "screen_f") 0 0x40000000
            row darwin 12345 (bytes "nx") 0 0
            UnixPathResolution.access (bytes "") 8 darwin |> answered
            UnixPathResolution.access null' 8 darwin |> answered
            UnixPathResolution.access null' 0 darwin |> answered
            row darwin -2 null' 0 0
            row darwin -2 null' 8 0
            row darwin -2 null' 0 0x40000000
            row darwin 12345 null' 0 0
            row darwin darwinFile null' 0 0
            row darwin -2 darwinTooLong 0 0
            row darwin -2 darwinTooLong 8 0
            row darwin -2 darwinTooLong 0 0x40000000
            row darwin 12345 darwinTooLong 0 0
            row darwin -2 (bytes "") 8 0
            row darwin -2 (bytes "") 0 0x40000000
            row darwin 12345 (bytes "") 0 0
        ]
        |> shouldEqual
            [
                "ENOENT"
                "ENOENT"
                "EINVAL"
                "EINVAL"
                "EBADF"
                "EINVAL"
                "EBADF"
                "ENOENT"
                "EFAULT"
                "EFAULT"
                "EFAULT"
                "EFAULT"
                "EINVAL"
                "EFAULT"
                "EFAULT"
                "ENAMETOOLONG"
                "ENAMETOOLONG"
                "EINVAL"
                "ENAMETOOLONG"
                "ENOENT"
                "EINVAL"
                "EBADF"
            ]

    [<Test>]
    let ``Linux's AT_EMPTY_PATH with AT_FDCWD asks about the current directory itself`` () : unit =
        // An unprivileged owner of a 0500 current directory: F_OK, R_OK and
        // X_OK are granted, and W_OK is not.
        let by = owner 1000u 1000u

        let vfs =
            VirtualFileSystem.empty epoch (owner 0u 0u)
            |> directory "/x" (owner 0u 0u) 0o755
            |> directory "/x/here" by 0o500

        let system =
            systemOn SimulatedUnixPlatform.linuxX64 (Credentials.ofIds (uid 1000u) (gid 1000u) []) "/x/here" vfs

        [ 0 ; 4 ; 2 ; 1 ]
        |> List.map (fun amode ->
            UnixPathResolution.faccessat linuxAtFdCwd (bytes "") amode 0x1000 system
            |> answered
        )
        |> shouldEqual [ "ok" ; "ok" ; "EACCES" ; "ok" ]

    [<Test>]
    let ``a dirfd naming neither a directory nor a regular file is refused only when a path starts from it`` () : unit =
        for platform, atFdCwd in
            [
                SimulatedUnixPlatform.linuxX64, linuxAtFdCwd
                SimulatedUnixPlatform.macOsArm64, darwinAtFdCwd
            ] do
            let credentials =
                UnixSystem.defaultCredentials (SimulatedUnixPlatform.flavour platform)

            let system =
                systemOn platform credentials "/b" (screenTree (InodeOwner.ofProcess credentials))

            let (readFd, _), system =
                match UnixPipe.pipe2 0 UserBuffer.Mapped system with
                | Ok (Pipe2Answer.Created (readFd, writeFd), system) -> (readFd, writeFd), system
                | other -> failwith $"pipe2 did not make a pipe: %A{other}"

            UnixPathResolution.faccessat readFd (bytes "screen_f") 0 0 system
            |> shouldEqual (Error (AccessRefusal.UnmodelledDescriptor readFd))

            // An absolute path never looks at the dirfd.
            UnixPathResolution.faccessat readFd (bytes "/b/screen_f") 0 0 system
            |> answered
            |> shouldEqual "ok"

            // Neither does AT_FDCWD's path, of course.
            UnixPathResolution.faccessat atFdCwd (bytes "screen_f") 0 0 system
            |> answered
            |> shouldEqual "ok"

    [<Test>]
    let ``access is faccessat from the current directory with no flags`` () : unit =
        let property (amode : int) (p : string) (linux : bool) : unit =
            let platform, atFdCwd, credentials =
                if linux then
                    SimulatedUnixPlatform.linuxX64, linuxAtFdCwd, Credentials.ofIds (uid 1000u) (gid 1000u) []
                else
                    SimulatedUnixPlatform.macOsArm64, darwinAtFdCwd, u501

            let system =
                systemOn platform credentials "/pd" (pathTree (InodeOwner.ofProcess credentials))

            UnixPathResolution.access (bytes p) amode system
            |> shouldEqual (UnixPathResolution.faccessat atFdCwd (bytes p) amode 0 system)

        let paths =
            Gen.elements [ "f" ; "sub" ; "lf" ; "dang" ; "cyc" ; "nx" ; "" ; "/pd/f" ; "f/" ; "/" ]

        let amodes =
            Gen.oneof [ Gen.choose (0, 7) ; ArbMap.defaults |> ArbMap.generate<int> ]

        Check.One (
            config.WithMaxTest 200,
            Prop.forAll
                (Arb.fromGen (Gen.zip3 amodes paths (ArbMap.defaults |> ArbMap.generate<bool>)))
                (fun (a, p, l) -> property a p l)
        )

    [<Test>]
    let ``access and faccessat are reachable through the syscall step, and change nothing`` () : unit =
        let credentials = UnixSystem.defaultCredentials SimulatedUnixFlavour.Linux

        let system =
            systemOn SimulatedUnixPlatform.linuxX64 credentials "/b" (screenTree (InodeOwner.ofProcess credentials))

        UnixSystem.step 0 (Syscall.Access (bytes "screen_f", 4)) system
        |> shouldEqual (Ok (SyscallOutcome.Answered (SyscallAnswer.Completed 0L), system))

        UnixSystem.step 0 (Syscall.Access (bytes "screen_z", 4)) system
        |> shouldEqual (Ok (SyscallOutcome.Answered (SyscallAnswer.Failed UnixError.EACCES), system))

        UnixSystem.step 0 (Syscall.FAccessAt (linuxAtFdCwd, bytes "screen_nx", 0, 0x200)) system
        |> shouldEqual (Ok (SyscallOutcome.Answered (SyscallAnswer.Failed UnixError.ENOENT), system))

        let darwin =
            systemOn SimulatedUnixPlatform.macOsArm64 u501 "/b" (screenTree (owner 501u 20u))

        UnixSystem.step 0 (Syscall.FAccessAt (darwinAtFdCwd, bytes "screen_f", 0, 0x800)) darwin
        |> shouldEqual (Error (SyscallRefusal.Access (AccessRefusal.UnmodelledFlags 0x800)))

    [<Test>]
    let ``the screen phase answers a bad word before the path is read, and otherwise asks for it`` () : unit =
        let progress (result : Result<AccessProgress<int, string>, AccessRefusal>) : string =
            match result with
            | Ok (AccessProgress.Answered (SyscallAnswer.Failed error)) -> string<UnixError> error
            | Ok (AccessProgress.Answered other) -> failwith $"answered %A{other} without a path"
            | Ok (AccessProgress.NeedsPath _) -> "needs the path"
            | Error refusal -> $"refused: %s{AccessRefusal.describe refusal}"

        let linuxCredentials = UnixSystem.defaultCredentials SimulatedUnixFlavour.Linux

        let linux =
            systemOn
                SimulatedUnixPlatform.linuxX64
                linuxCredentials
                "/b"
                (screenTree (InodeOwner.ofProcess linuxCredentials))

        let darwin =
            systemOn SimulatedUnixPlatform.macOsArm64 u501 "/b" (screenTree (owner 501u 20u))

        [
            UnixPathResolution.accessScreenPhase 8 linux |> progress
            UnixPathResolution.faccessatScreenPhase linuxAtFdCwd 0 0x40000000 linux
            |> progress
            UnixPathResolution.accessScreenPhase 4 linux |> progress
            UnixPathResolution.accessScreenPhase 8 darwin |> progress
            UnixPathResolution.faccessatScreenPhase darwinAtFdCwd 0 0x40000000 darwin
            |> progress
        ]
        |> shouldEqual [ "EINVAL" ; "EINVAL" ; "needs the path" ; "needs the path" ; "EINVAL" ]

        match UnixPathResolution.faccessatScreenPhase 12345 4 0 linux with
        | Ok (AccessProgress.NeedsPath paused) ->
            UnixPathResolution.accessWithPath (bytes "/b/screen_f") paused
            |> answered
            |> shouldEqual "ok"

            UnixPathResolution.accessWithPath (bytes "screen_f") paused
            |> answered
            |> shouldEqual "EBADF"
        | _ -> failwith "a good mode word did not ask for the path"
