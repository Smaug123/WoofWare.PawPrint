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

/// `chown(2)`, `lchown(2)` and `fchown(2)`: who may change an inode's owner and
/// group, which set-ID bits a change clears, which timestamps move, how a path
/// through a symbolic link resolves for each, and what `fchown` answers on each
/// descriptor kind.
///
/// The rows come from `docs/plans/2026-08-23-posix-kernel-extraction/chown-rules.c`,
/// run on Linux 6.18.5 (aarch64, root in the container, ext4 and tmpfs alike) and
/// on Darwin 27.0 at uid 501; its output is beside it, and is embedded here and
/// replayed row by row. Its sweeps compared the kernels with the prediction
/// `reference` transcribes, which is held to the rule exhaustively; the
/// syscalls are then held to that prediction end to end on filesystems built to
/// match the ones it measured.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestOwnerChange =

    let private context : string = "TestOwnerChange"

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

    let private allRequests : OwnerChangeRequest list =
        [
            for user in
                [
                    RequestedUser.Unchanged
                    RequestedUser.Current
                    RequestedUser.Callers
                    RequestedUser.Other
                ] do
                for group in
                    [
                        RequestedGroup.Unchanged
                        RequestedGroup.Current
                        RequestedGroup.CallersGroup
                        RequestedGroup.Other
                    ] do
                    yield
                        {
                            UserAsked = user
                            GroupAsked = group
                        }
        ]

    let private nothingAsked : OwnerChangeRequest =
        {
            UserAsked = RequestedUser.Unchanged
            GroupAsked = RequestedGroup.Unchanged
        }

    let private rules : OwnerChangeRule list =
        [
            OwnerChangeRule.ClearsSetIdFromNonDirectories
            OwnerChangeRule.ClearsSetIdWhenAnIdIsNamed
        ]

    // ------------------------------------------------------------- the rule

    /// The rule `chown-rules.c`'s `predict` compared every kernel answer
    /// against, with no mismatch, extended by the refusals of the Darwin rows
    /// nobody has measured.
    let private reference
        (rule : OwnerChangeRule)
        (standing : Standing)
        (request : OwnerChangeRequest)
        (target : OwnerChangeTarget)
        (raw : int)
        : Result<OwnerChange, OwnerChangeRefusal>
        =
        let unprivileged = standing.Privilege = CallerPrivilege.Unprivileged

        match rule with
        | OwnerChangeRule.ClearsSetIdFromNonDirectories ->
            let userAllowed =
                match request.UserAsked with
                | RequestedUser.Unchanged -> true
                | RequestedUser.Current -> standing.Owns
                | RequestedUser.Callers
                | RequestedUser.Other -> false

            let groupAllowed =
                match request.GroupAsked with
                | RequestedGroup.Unchanged -> true
                | RequestedGroup.Current
                | RequestedGroup.CallersGroup -> standing.Owns
                | RequestedGroup.Other -> false

            if unprivileged && not (userAllowed && groupAllowed) then
                Ok OwnerChange.Forbidden
            elif target = OwnerChangeTarget.Directory then
                Ok (OwnerChange.Changed (mode raw))
            else

            let kill =
                (raw &&& 0o4000)
                ||| (if
                         raw &&& 0o2000 <> 0
                         && (raw &&& 0o10 <> 0 || not (standing.InGroup || not unprivileged))
                     then
                         0o2000
                     else
                         0)

            if kill <> 0 && not standing.Owns && unprivileged then
                Ok OwnerChange.Forbidden
            else
                Ok (OwnerChange.Changed (mode (raw &&& ~~~kill)))
        | OwnerChangeRule.ClearsSetIdWhenAnIdIsNamed ->
            let bits = mode raw

            if not unprivileged then
                Error (OwnerChangeRefusal.UnmeasuredPrivilegedCaller (standing, request, bits))
            else

            let measured =
                raw &&& 0o6000 = 0
                || standing.Owns && (standing.InGroup || raw &&& 0o2000 = 0)
                || not standing.Owns && request = nothingAsked && raw &&& 0o2000 = 0

            if not measured then
                Error (OwnerChangeRefusal.UnmeasuredSetIdChange (standing, request, bits))
            elif request = nothingAsked then
                Ok OwnerChange.Untouched
            else

            let userAllowed =
                match request.UserAsked with
                | RequestedUser.Unchanged
                | RequestedUser.Current -> true
                | RequestedUser.Callers
                | RequestedUser.Other -> false

            let groupAllowed =
                match request.GroupAsked with
                | RequestedGroup.Unchanged
                | RequestedGroup.Current -> true
                | RequestedGroup.CallersGroup -> standing.Owns
                | RequestedGroup.Other -> false

            if userAllowed && groupAllowed then
                Ok (OwnerChange.Changed (mode (raw &&& ~~~0o6000)))
            else
                Ok OwnerChange.Forbidden

    [<Test>]
    let ``verdict answers the probe's rule for every standing, request, target and mode`` () : unit =
        for rule in rules do
            for standing in allStandings do
                for request in allRequests do
                    for target in [ OwnerChangeTarget.Directory ; OwnerChangeTarget.NonDirectory ] do
                        for raw in 0..0o7777 do
                            let actual = OwnerChangeRules.verdict rule standing request target (mode raw)
                            let expected = reference rule standing request target raw

                            if actual <> expected then
                                failwith
                                    $"%O{rule}, %A{standing}, %A{request}, %O{target}, mode 0o%04o{raw}: verdict said %A{actual}, the probe's rule %A{expected}"

    [<Test>]
    let ``each platform names its flavour's rule`` () : unit =
        for platform, expected in
            [
                SimulatedUnixPlatform.linuxX64, OwnerChangeRule.ClearsSetIdFromNonDirectories
                SimulatedUnixPlatform.linuxArm64, OwnerChangeRule.ClearsSetIdFromNonDirectories
                SimulatedUnixPlatform.macOsArm64, OwnerChangeRule.ClearsSetIdWhenAnIdIsNamed
            ] do
            SimulatedUnixPlatform.ownerChangeRule platform |> shouldEqual expected

    // ------------------------------------------------------------- classifying a request

    /// A naive reading of which IDs a request names, against the inode's owner
    /// and the caller's own IDs.
    let private naiveClassify
        (credentials : Credentials)
        (by : InodeOwner)
        (user : UserId option)
        (group : GroupId option)
        : OwnerChangeRequest
        =
        let callersGroups =
            Set.ofList (credentials.EffectiveGroup :: credentials.SupplementaryGroups)

        {
            UserAsked =
                match user with
                | None -> RequestedUser.Unchanged
                | Some u when u = by.User -> RequestedUser.Current
                | Some u when u = credentials.EffectiveUser -> RequestedUser.Callers
                | Some _ -> RequestedUser.Other
            GroupAsked =
                match group with
                | None -> RequestedGroup.Unchanged
                | Some g when g = by.Group -> RequestedGroup.Current
                | Some g when Set.contains g callersGroups -> RequestedGroup.CallersGroup
                | Some _ -> RequestedGroup.Other
        }

    [<Test>]
    let ``classify agrees with a naive reading of the IDs`` () : unit =
        // IDs from a pool small enough that the inode's owner, the caller's
        // IDs and the requested IDs routinely coincide.
        let small = Gen.elements [ 0u ; 1u ; 2u ; 3u ; 4u ]
        let user = small |> Gen.map uid
        let group = small |> Gen.map gid

        let credentials =
            gen {
                let! real = user
                let! effective = user
                let! realGroup = group
                let! effectiveGroup = group
                let! count = Gen.choose (0, 3)
                let! groups = Gen.listOfLength count group

                return
                    { Credentials.ofIds effective effectiveGroup groups with
                        RealUser = real
                        RealGroup = realGroup
                    }
            }

        let generator =
            gen {
                let! credentials = credentials
                let! byUser = user
                let! byGroup = group
                let! asked = Gen.optionOf user
                let! askedGroup = Gen.optionOf group

                return
                    credentials,
                    {
                        User = byUser
                        Group = byGroup
                    },
                    asked,
                    askedGroup
            }

        let property (credentials : Credentials, by : InodeOwner, asked : UserId option, askedGroup : GroupId option) =
            OwnerChangeRequest.classify credentials by asked askedGroup = naiveClassify credentials by asked askedGroup

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 5000, Prop.forAll (Arb.fromGen generator) property)

    // ------------------------------------------------------------- the probe's rows, replayed

    let private resource (flavour : string) : string list =
        let assembly = Assembly.GetExecutingAssembly ()
        let resourceName = $"WoofWare.PosixKernel.Test.chownRules.%s{flavour}.txt"

        use stream =
            match assembly.GetManifestResourceStream resourceName with
            | null -> failwith $"embedded resource %s{resourceName} not found"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList

    /// `(u,g)` from a probe's `chown(u,g)` or `lchown(u,g)`, -1 being `None`.
    let private asked (call : string) : UserId option * GroupId option =
        let inner =
            call.Substring (call.IndexOf '(' + 1, call.IndexOf ')' - call.IndexOf '(' - 1)

        match inner.Split ',' |> Array.map int64 with
        | [| u ; g |] ->
            (if u = -1L then None else Some (uid (uint32 u))), (if g = -1L then None else Some (gid (uint32 g)))
        | _ -> failwith $"cannot read the IDs of %s{call}"

    let private ownerText (text : string) : InodeOwner =
        match text.Split ':' |> Array.map uint32 with
        | [| u ; g |] -> owner u g
        | _ -> failwith $"cannot read the owner %s{text}"

    /// The probe's class names, as it printed them beside each call.
    let private printedClasses (request : OwnerChangeRequest) : string =
        let user =
            match request.UserAsked with
            | RequestedUser.Unchanged -> "-1"
            | RequestedUser.Current -> "current"
            | RequestedUser.Callers -> "caller"
            | RequestedUser.Other -> "other"

        let group =
            match request.GroupAsked with
            | RequestedGroup.Unchanged -> "-1"
            | RequestedGroup.Current -> "current"
            | RequestedGroup.CallersGroup -> "member"
            | RequestedGroup.Other -> "other"

        $"[user %s{user}, group %s{group}]"

    /// What the probe printed for an answer: the errno, the bits and the owner
    /// the inode then had, and whether its ctime moved.
    let private outcome
        (verdict : OwnerChange)
        (before : InodeOwner)
        (bits : int)
        (asked : UserId option * GroupId option)
        : string * int * InodeOwner * bool
        =
        let user, group = asked

        match verdict with
        | OwnerChange.Forbidden -> "EPERM", bits, before, false
        | OwnerChange.Untouched -> "ok", bits, before, false
        | OwnerChange.Changed bits ->
            "ok",
            PermissionBits.toInt bits,
            {
                User = defaultArg user before.User
                Group = defaultArg group before.Group
            },
            true

    /// The probe's unprivileged Linux caller.
    let private u1000 : Credentials =
        Credentials.ofIds (uid 1000u) (gid 1000u) [ gid 1000u ; gid 2000u ]

    /// The probe's Linux root, whose only group is 0.
    let private linuxRoot : Credentials =
        Credentials.ofIds UserId.root (gid 0u) [ gid 0u ]

    /// The probe's Darwin caller, with the groups `id` reported for it.
    let private u501 : Credentials =
        Credentials.ofIds (uid 501u) (gid 20u) [ gid 20u ; gid 12u ; gid 61u ; gid 100u ; gid 701u ]

    /// Each Linux standing row of the probe: the caller and the inode's owner.
    let private linuxRows : Map<string, Credentials * InodeOwner> =
        Map.ofList
            [
                "owner, group = egid", (u1000, owner 1000u 1000u)
                "owner, group = supplementary", (u1000, owner 1000u 2000u)
                "owner, group not a member", (u1000, owner 1000u 3000u)
                "non-owner, group = egid", (u1000, owner 1001u 1000u)
                "non-owner, group = supplementary", (u1000, owner 1001u 2000u)
                "non-owner, group not a member", (u1000, owner 1001u 3000u)
                "root, owner, in group", (linuxRoot, owner 0u 0u)
                "root, owner, group not a member", (linuxRoot, owner 0u 3000u)
                "root, non-owner, in group", (linuxRoot, owner 1001u 0u)
                "root, non-owner, group not a member", (linuxRoot, owner 1001u 3000u)
            ]

    /// Each Darwin owner row of the probe: the inode's group.
    let private darwinRows : Map<string, InodeOwner> =
        Map.ofList
            [
                "owner, group = egid", owner 501u 20u
                "owner, group = supplementary 12", owner 501u 12u
                "owner, group not a member (wheel)", owner 501u 0u
            ]

    let private targetOf (kind : string) : OwnerChangeTarget =
        match kind with
        | "file" -> OwnerChangeTarget.NonDirectory
        | "dir" -> OwnerChangeTarget.Directory
        | other -> failwith $"unknown kind %s{other}"

    /// Check one probe row: `call` asked by `credentials` of a `target` inode
    /// owned by `by` and carrying `bits`, answered `errno`, leaving `after`
    /// and `afterOwner`; `ctimeMoved` is `None` where the row did not say.
    let private replay
        (rule : OwnerChangeRule)
        (line : string)
        (credentials : Credentials)
        (by : InodeOwner)
        (target : OwnerChangeTarget)
        (bits : int)
        (call : string)
        (classes : string option)
        (errno : string)
        (after : int option)
        (afterOwner : InodeOwner)
        (ctimeMoved : bool option)
        : unit
        =
        let ids = asked call
        let request = OwnerChangeRequest.classify credentials by (fst ids) (snd ids)

        match classes with
        | Some printed -> (line, printedClasses request) |> shouldEqual (line, printed)
        | None -> ()

        let verdict =
            match OwnerChangeRules.verdict rule (Standing.toward credentials by) request target (mode bits) with
            | Ok verdict -> verdict
            | Error refusal -> failwith $"%s{line}: a measured row was refused: %A{refusal}"

        let expectedErrno, expectedBits, expectedOwner, moves = outcome verdict by bits ids
        (line, errno, afterOwner) |> shouldEqual (line, expectedErrno, expectedOwner)

        match after with
        | Some after -> (line, after) |> shouldEqual (line, expectedBits)
        | None -> ()

        match ctimeMoved with
        | Some moved -> (line, moved) |> shouldEqual (line, moves)
        | None -> ()

    let private ctimeOf (times : string) : bool =
        if times.Contains "ctime=moved" then true
        elif times.Contains "ctime=kept" then false
        else failwith $"no ctime in %s{times}"

    /// `after=0755 1000:2000`.
    let private afterOf (text : string) : int * InodeOwner =
        match text.Substring("after=".Length).Split ' ' with
        | [| bits ; by |] -> Convert.ToInt32 (bits, 8), ownerText by
        | _ -> failwith $"cannot read %s{text}"

    [<Test>]
    let ``every Linux row the probe measured`` () : unit =
        let rule = OwnerChangeRule.ClearsSetIdFromNonDirectories
        let lines = resource "linux"
        let mutable rows = 0

        for line in lines do
            match line.Split '\t' |> Array.toList with
            | [ "ROW" ; label ; kind ; call ; before ; errno ; after ] ->
                let credentials, by = linuxRows.[label]
                let afterBits, afterOwner = afterOf after
                // The class names follow the call, after its closing parenthesis.
                let classes = call.Substring (call.IndexOf ')' + 2)

                replay
                    rule
                    line
                    credentials
                    by
                    (targetOf kind)
                    (Convert.ToInt32 (before, 8))
                    call
                    (Some classes)
                    errno
                    (Some afterBits)
                    afterOwner
                    None

                rows <- rows + 1
            | [ "TIMES" ; label ; kindAndMode ; call ; errno ; after ; times ] ->
                let credentials, by = linuxRows.[label]
                let afterBits, afterOwner = afterOf after

                match kindAndMode.Split ' ' with
                | [| kind ; bits |] ->
                    replay
                        rule
                        line
                        credentials
                        by
                        (targetOf kind)
                        (Convert.ToInt32 (bits, 8))
                        call
                        None
                        errno
                        (Some afterBits)
                        afterOwner
                        (Some (ctimeOf times))
                | _ -> failwith $"cannot read %s{line}"

                rows <- rows + 1
            | [ "LINK" ; label ; call ; errno ; bitsText ; owners ; times ] ->
                let credentials, by = linuxRows.[label]
                bitsText |> shouldEqual "mode 0777 -> 0777"

                match owners.Split " -> " with
                | [| before ; afterOwner |] ->
                    ownerText before |> shouldEqual by

                    replay
                        rule
                        line
                        credentials
                        by
                        OwnerChangeTarget.NonDirectory
                        0o777
                        call
                        None
                        errno
                        None
                        (ownerText afterOwner)
                        (Some (ctimeOf times))
                | _ -> failwith $"cannot read %s{line}"

                rows <- rows + 1
            | "SWEEP" :: _
            | "PIPE-SWEEP" :: _ -> line.EndsWith "mismatches=0" |> shouldEqual true
            | _ -> ()

        // 378 sweeps of seven literal modes, 1134 timestamp rows and 189
        // links.
        rows |> shouldEqual (378 * 7 + 1134 + 189)

    [<Test>]
    let ``every Darwin row the probe measured`` () : unit =
        let rule = OwnerChangeRule.ClearsSetIdWhenAnIdIsNamed
        let lines = resource "darwin"
        let mutable rows = 0

        for line in lines do
            match line.Split '\t' |> Array.toList with
            | [ "ROW" ; label ; kind ; call ; before ; errno ; after ] ->
                let by = darwinRows.[label]
                let afterBits, afterOwner = afterOf after
                let classes = call.Substring (call.IndexOf ')' + 2)

                replay
                    rule
                    line
                    u501
                    by
                    (targetOf kind)
                    (Convert.ToInt32 (before, 8))
                    call
                    (Some classes)
                    errno
                    (Some afterBits)
                    afterOwner
                    None

                rows <- rows + 1
            | [ "TIMES" ; label ; kindAndMode ; call ; errno ; after ; times ] ->
                let by = darwinRows.[label]
                let afterBits, afterOwner = afterOf after

                match kindAndMode.Split ' ' with
                | [| kind ; bits |] ->
                    replay
                        rule
                        line
                        u501
                        by
                        (targetOf kind)
                        (Convert.ToInt32 (bits, 8))
                        call
                        None
                        errno
                        (Some afterBits)
                        afterOwner
                        (Some (ctimeOf times))
                | _ -> failwith $"cannot read %s{line}"

                rows <- rows + 1
            | [ "FOREIGN" ; label ; _path ; before ; call ; errno ; after ; times ; _ ] ->
                match before.Split ' ', after.Substring("after=".Length).Split ' ' with
                | [| by ; bits |], [| afterOwner ; afterBits |] ->
                    let target =
                        if label.Contains "dir" then
                            OwnerChangeTarget.Directory
                        else
                            OwnerChangeTarget.NonDirectory

                    replay
                        rule
                        line
                        u501
                        (ownerText by)
                        target
                        (Convert.ToInt32 (bits, 8))
                        call
                        (Some (call.Substring (call.IndexOf ')' + 2)))
                        errno
                        (Some (Convert.ToInt32 (afterBits, 8)))
                        (ownerText afterOwner)
                        (Some (ctimeOf times))
                | _ -> failwith $"cannot read %s{line}"

                rows <- rows + 1
            | "SWEEP" :: _ -> line.EndsWith "mismatches=0" |> shouldEqual true
            | "FOREIGN" :: _ -> failwith $"an unread FOREIGN row: %s{line}"
            | _ -> ()

        // 120 sweeps of seven literal modes (the wheel row's 02xxx modes
        // printed as the mode that stuck), 480 timestamp rows, and 34 rows
        // against other users' inodes.
        rows |> shouldEqual (120 * 7 + 480 + 34)

    [<Test>]
    let ``Darwin refuses the rows nobody has measured`` () : unit =
        let rule = OwnerChangeRule.ClearsSetIdWhenAnIdIsNamed

        let standing (privilege : CallerPrivilege) (owns : bool) (inGroup : bool) : Standing =
            {
                Privilege = privilege
                Owns = owns
                InGroup = inGroup
            }

        let groupOnly : OwnerChangeRequest =
            {
                UserAsked = RequestedUser.Unchanged
                GroupAsked = RequestedGroup.Current
            }

        let rows : (Standing * OwnerChangeRequest * int * Result<OwnerChange, OwnerChangeRefusal>) list =
            [
                // Root, whatever it asks and whoever owns the inode.
                standing CallerPrivilege.Privileged true true,
                nothingAsked,
                0o644,
                Error (
                    OwnerChangeRefusal.UnmeasuredPrivilegedCaller (
                        standing CallerPrivilege.Privileged true true,
                        nothingAsked,
                        mode 0o644
                    )
                )
                // An owner outside the group, of an inode carrying S_ISGID,
                // even asking nothing.
                standing CallerPrivilege.Unprivileged true false,
                nothingAsked,
                0o2755,
                Error (
                    OwnerChangeRefusal.UnmeasuredSetIdChange (
                        standing CallerPrivilege.Unprivileged true false,
                        nothingAsked,
                        mode 0o2755
                    )
                )
                // ...but not of an inode carrying only S_ISUID.
                standing CallerPrivilege.Unprivileged true false,
                groupOnly,
                0o4755,
                Ok (OwnerChange.Changed (mode 0o755))
                // A non-owner naming an ID of a set-ID inode.
                standing CallerPrivilege.Unprivileged false true,
                groupOnly,
                0o4755,
                Error (
                    OwnerChangeRefusal.UnmeasuredSetIdChange (
                        standing CallerPrivilege.Unprivileged false true,
                        groupOnly,
                        mode 0o4755
                    )
                )
                // A non-owner asking nothing of an S_ISGID inode.
                standing CallerPrivilege.Unprivileged false false,
                nothingAsked,
                0o2755,
                Error (
                    OwnerChangeRefusal.UnmeasuredSetIdChange (
                        standing CallerPrivilege.Unprivileged false false,
                        nothingAsked,
                        mode 0o2755
                    )
                )
                // ...but asking nothing of an S_ISUID inode is measured.
                standing CallerPrivilege.Unprivileged false false, nothingAsked, 0o4755, Ok OwnerChange.Untouched
            ]

        for standing, request, bits, expected in rows do
            OwnerChangeRules.verdict rule standing request OwnerChangeTarget.NonDirectory (mode bits)
            |> shouldEqual expected

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
            SymlinkModes.linux
            by
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
            |> UnixBootImage.withCredentials context credentials
            |> UnixBootImage.boot

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

    let private inodeOf (p : string) (system : UnixSystem<int, string>) : Inode =
        match VirtualFileSystem.tryGet (inodeAt system.Machine.FileSystem p) system.Machine.FileSystem with
        | Some inode -> inode
        | None -> failwith $"%s{p} is absent"

    let private bitsOf (p : string) (system : UnixSystem<int, string>) : int =
        PermissionBits.toInt (Inode.permissions (inodeOf p system))

    /// Give the inode at `p` the bits `bits`, without moving anything else:
    /// every inode of a hand-built filesystem was stamped at `epoch`, so
    /// stamping its ctime at `epoch` again leaves it where it was.
    let private withBits (p : string) (bits : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        let vfs = system.Machine.FileSystem
        let inode = inodeAt vfs p

        match VirtualFileSystem.tryGet inode vfs with
        | Some entry when entry.Times.StatusChange = epoch -> ()
        | other -> failwith $"%s{p} was not stamped at the epoch: %A{other}"

        { system with
            Machine =
                { system.Machine with
                    FileSystem = VirtualFileSystem.setPermissions inode (mode bits) epoch vfs
                }
        }

    let private chownAnswer
        (p : string)
        (user : UserId option)
        (group : GroupId option)
        (system : UnixSystem<int, string>)
        : SyscallAnswer * UnixSystem<int, string>
        =
        match UnixPathResolution.chown (PathArg.ofPath (path p)) user group system with
        | Ok answer -> answer
        | Error refusal -> failwith $"chown(%s{p}) was refused: %s{ChOwnRefusal.describe refusal}"

    let private lchownAnswer
        (p : string)
        (user : UserId option)
        (group : GroupId option)
        (system : UnixSystem<int, string>)
        : SyscallAnswer * UnixSystem<int, string>
        =
        match UnixPathResolution.lchown (PathArg.ofPath (path p)) user group system with
        | Ok answer -> answer
        | Error refusal -> failwith $"lchown(%s{p}) was refused: %s{ChOwnRefusal.describe refusal}"

    /// `/t` holding a file and a directory owned by `by`, as the probe
    /// prepared each row.
    let private tree (by : InodeOwner) : VirtualFileSystem =
        VirtualFileSystem.empty epoch (owner 0u 0u)
        |> directory "/t" (owner 0u 0u) 0o755
        |> file "/t/f" by 0o600
        |> directory "/t/d" by 0o700

    /// Hold `chown` on `system` to `reference` for every mode of `/t/f` and
    /// `/t/d`: the answer, the bits and owner the inode then has, its
    /// timestamps, and that a call changing nothing changed nothing at all.
    let private sweep
        (rule : OwnerChangeRule)
        (credentials : Credentials)
        (by : InodeOwner)
        (asks : (UserId option * GroupId option) list)
        (system : UnixSystem<int, string>)
        : unit
        =
        let now = UnixMachineState.realtime system.Machine
        let standing = Standing.toward credentials by

        for p, target in [ "/t/f", OwnerChangeTarget.NonDirectory ; "/t/d", OwnerChangeTarget.Directory ] do
            for user, group in asks do
                let request = OwnerChangeRequest.classify credentials by user group

                for raw in 0..0o7777 do
                    let system = withBits p raw system
                    let before = inodeOf p system

                    match
                        reference rule standing request target raw,
                        UnixPathResolution.chown (PathArg.ofPath (path p)) user group system
                    with
                    | Error expected, Error actual ->
                        actual
                        |> shouldEqual (
                            ChOwnRefusal.UnmeasuredOwnerChange (inodeAt system.Machine.FileSystem p, expected)
                        )
                    | Ok OwnerChange.Forbidden, Ok (SyscallAnswer.Failed UnixError.EPERM, after)
                    | Ok OwnerChange.Untouched, Ok (SyscallAnswer.Completed 0L, after) ->
                        if after <> system then
                            failwith
                                $"%A{standing}, %A{request}, %s{p} at 0o%04o{raw}: answered without a change, but changed the system"
                    | Ok (OwnerChange.Changed bits), Ok (SyscallAnswer.Completed 0L, after) ->
                        let changed = inodeOf p after

                        (p, raw, request, bitsOf p after, changed.Owner, changed.Times)
                        |> shouldEqual (
                            p,
                            raw,
                            request,
                            PermissionBits.toInt bits,
                            {
                                User = defaultArg user by.User
                                Group = defaultArg group by.Group
                            },
                            { before.Times with
                                StatusChange = now
                            }
                        )
                    | expected, actual ->
                        failwith
                            $"%A{standing}, %A{request}, %s{p} at 0o%04o{raw}: chown answered %A{actual}, the rule %A{expected}"

    [<Test>]
    let ``Linux chown answers every mode as the probe measured, for every standing`` () : unit =
        for KeyValue (label, (credentials, by)) in linuxRows do
            let caller = credentials.EffectiveUser
            // One request of each kind the probe asked, for this row.
            let asks =
                [
                    None, None
                    Some by.User, None
                    Some caller, None
                    Some (uid 1002u), None
                    None, Some by.Group
                    None, Some (gid 2000u)
                    None, Some (gid 3001u)
                    Some by.User, Some (gid 1000u)
                ]

            (label, asks.Length) |> shouldEqual (label, 8)

            sweep
                OwnerChangeRule.ClearsSetIdFromNonDirectories
                credentials
                by
                asks
                (systemOn SimulatedUnixPlatform.linuxX64 credentials (tree by))

    [<Test>]
    let ``Darwin chown answers every mode as the probe measured, and refuses where it did not`` () : unit =
        let rows =
            [
                owner 501u 20u
                owner 501u 12u
                owner 501u 0u
                // Other users' inodes: a group u501 is in, and one it is not.
                owner 503u 20u
                owner 0u 0u
            ]

        for by in rows do
            let asks =
                [
                    None, None
                    Some by.User, None
                    Some (uid 501u), None
                    Some (uid 0u), None
                    None, Some by.Group
                    None, Some (gid 12u)
                    None, Some (gid 4242u)
                    Some by.User, Some (gid 20u)
                ]

            sweep
                OwnerChangeRule.ClearsSetIdWhenAnIdIsNamed
                u501
                by
                asks
                (systemOn SimulatedUnixPlatform.macOsArm64 u501 (tree by))

        // Root on Darwin is refused whatever it asks.
        let by = owner 501u 20u

        sweep
            OwnerChangeRule.ClearsSetIdWhenAnIdIsNamed
            Owners.root
            by
            [ None, None ; Some (uid 1002u), None ]
            (systemOn SimulatedUnixPlatform.macOsArm64 Owners.root (tree by))

    // ------------------------------------------------------------- paths

    /// `/p` holding a file, a directory, links to each, a dangling link, a
    /// link to itself, an unsearchable directory holding a file, and a file
    /// root owns, everything else owned by `by`.
    let private pathTree (by : InodeOwner) : VirtualFileSystem =
        VirtualFileSystem.empty epoch by
        |> directory "/p" by 0o755
        |> file "/p/f" by 0o600
        |> directory "/p/d" by 0o700
        |> symlink "/p/lf" by "f"
        |> symlink "/p/ld" by "d"
        |> symlink "/p/dang" by "nowhere"
        |> symlink "/p/loop" by "loop"
        |> directory "/p/shut" by 0o600
        |> file "/p/shut/inner" by 0o600
        |> file "/p/theirs" (owner 0u 0u) 0o644

    /// The probe's path rows, for an unprivileged caller in `egid` and a
    /// supplementary `memberGroup`.
    let private pathRows (platform : SimulatedUnixPlatform) (credentials : Credentials) (memberGroup : GroupId) : unit =
        let egid = credentials.EffectiveGroup
        let by = InodeOwner.ofProcess credentials
        let system = systemOn platform credentials (pathTree by)
        let now = UnixMachineState.realtime system.Machine

        let changedOwner (p : string) (expected : InodeOwner) (after : UnixSystem<int, string>) : unit =
            let changed = inodeOf p after
            let before = inodeOf p system

            (p, changed.Owner, changed.Times)
            |> shouldEqual (
                p,
                expected,
                { before.Times with
                    StatusChange = now
                }
            )

        let unchanged (p : string) (after : UnixSystem<int, string>) : unit =
            (p, inodeOf p after) |> shouldEqual (p, inodeOf p system)

        // Through a link to a file: the target changes, and the link does not.
        match chownAnswer "/p/lf" None (Some memberGroup) system with
        | SyscallAnswer.Completed 0L, after ->
            changedOwner
                "/p/f"
                { by with
                    Group = memberGroup
                }
                after

            unchanged "/p/lf" after
        | other -> failwith $"chown through a link: %A{other}"

        // lchown of the same link: the link changes, and the target does not.
        match lchownAnswer "/p/lf" None (Some memberGroup) system with
        | SyscallAnswer.Completed 0L, after ->
            changedOwner
                "/p/lf"
                { by with
                    Group = memberGroup
                }
                after

            unchanged "/p/f" after
        | other -> failwith $"lchown of a link: %A{other}"

        for p, call, changed in
            [
                "/p/ld", chownAnswer, "/p/d"
                "/p/ld", lchownAnswer, "/p/ld"
                "/p/ld/", lchownAnswer, "/p/d"
                "/p/ld/", chownAnswer, "/p/d"
                "/p/d/", chownAnswer, "/p/d"
                "/p/d/", lchownAnswer, "/p/d"
                "/p/dang", lchownAnswer, "/p/dang"
                "/p/loop", lchownAnswer, "/p/loop"
            ] do
            match call p None (Some memberGroup) system with
            | SyscallAnswer.Completed 0L, after ->
                changedOwner
                    changed
                    { by with
                        Group = memberGroup
                    }
                    after
            | other -> failwith $"%s{p}: %A{other}"

        for p, call, user, error in
            [
                "/p/f/", chownAnswer, None, UnixError.ENOTDIR
                "/p/f/", lchownAnswer, None, UnixError.ENOTDIR
                "/p/lf/", chownAnswer, None, UnixError.ENOTDIR
                "/p/lf/", lchownAnswer, None, UnixError.ENOTDIR
                "/p/dang", chownAnswer, None, UnixError.ENOENT
                "/p/loop", chownAnswer, None, UnixError.ELOOP
                "/p/absent", chownAnswer, None, UnixError.ENOENT
                "/p/absent", lchownAnswer, None, UnixError.ENOENT
                "", chownAnswer, None, UnixError.ENOENT
                "", lchownAnswer, None, UnixError.ENOENT
                "/p/f/under", chownAnswer, None, UnixError.ENOTDIR
                "/p/f/under", lchownAnswer, None, UnixError.ENOTDIR
                // The walk's own search check comes first, even naming someone
                // else's uid.
                "/p/shut/inner", chownAnswer, None, UnixError.EACCES
                "/p/shut/inner", lchownAnswer, None, UnixError.EACCES
                "/p/shut/inner", chownAnswer, Some (uid 1002u), UnixError.EACCES
                // So do the walk's errors for someone else's file.
                "/p/theirs/", chownAnswer, Some (uid 1002u), UnixError.ENOTDIR
                "/p/theirs/under", chownAnswer, Some (uid 1002u), UnixError.ENOTDIR
                "/p/theirs", chownAnswer, Some (uid 1002u), UnixError.EPERM
            ] do
            match call p user (Some egid) system with
            | SyscallAnswer.Failed actual, after ->
                (p, actual) |> shouldEqual (p, error)
                after |> shouldEqual system
            | other -> failwith $"%s{p}: %A{other}"

    [<Test>]
    let ``Linux chown follows a final symlink, lchown does not, and every other failure is the walk's`` () : unit =
        pathRows SimulatedUnixPlatform.linuxX64 u1000 (gid 2000u)

    [<Test>]
    let ``Darwin chown follows a final symlink, lchown does not, and every other failure is the walk's`` () : unit =
        pathRows SimulatedUnixPlatform.macOsArm64 u501 (gid 12u)

    [<Test>]
    let ``Linux root's walk searches an unsearchable directory, and root may name anyone`` () : unit =
        let by = InodeOwner.ofProcess linuxRoot
        let system = systemOn SimulatedUnixPlatform.linuxX64 linuxRoot (pathTree by)

        match chownAnswer "/p/shut/inner" (Some (uid 1002u)) (Some (gid 3001u)) system with
        | SyscallAnswer.Completed 0L, after -> (inodeOf "/p/shut/inner" after).Owner |> shouldEqual (owner 1002u 3001u)
        | other -> failwith $"%A{other}"

    [<Test>]
    let ``a call naming no ID moves ctime on Linux and nothing at all on Darwin`` () : unit =
        for platform, credentials, moves in
            [
                SimulatedUnixPlatform.linuxX64, u1000, true
                SimulatedUnixPlatform.linuxX64, linuxRoot, true
                SimulatedUnixPlatform.macOsArm64, u501, false
            ] do
            let by = InodeOwner.ofProcess credentials
            let system = systemOn platform credentials (pathTree by)
            let now = UnixMachineState.realtime system.Machine

            for p, call in [ "/p/f", chownAnswer ; "/p/d", chownAnswer ; "/p/lf", lchownAnswer ] do
                match call p None None system with
                | SyscallAnswer.Completed 0L, after ->
                    if moves then
                        (inodeOf p after).Times
                        |> shouldEqual
                            { (inodeOf p system).Times with
                                StatusChange = now
                            }
                    else
                        after |> shouldEqual system
                | other -> failwith $"%s{p}: %A{other}"

    // ------------------------------------------------------------- fchown

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

    let private fchownAnswer
        (fd : int)
        (user : UserId option)
        (group : GroupId option)
        (system : UnixSystem<int, string>)
        : SyscallAnswer * UnixSystem<int, string>
        =
        match UnixPathResolution.fchown fd user group system with
        | Ok answer -> answer
        | Error refusal -> failwith $"fchown(%d{fd}) was refused: %s{FChOwnRefusal.describe refusal}"

    [<Test>]
    let ``fchown changes a file or directory through a descriptor of any access mode`` () : unit =
        for platform, credentials, memberGroup in
            [
                SimulatedUnixPlatform.linuxX64, u1000, gid 2000u
                SimulatedUnixPlatform.macOsArm64, u501, gid 12u
            ] do
            let by = InodeOwner.ofProcess credentials
            let system = systemOn platform credentials (pathTree by)
            let now = UnixMachineState.realtime system.Machine

            for access in
                [
                    FileAccessMode.ReadOnly
                    FileAccessMode.WriteOnly
                    FileAccessMode.ReadWrite
                ] do
                let fd, withFd = opened "/p/f" access system

                match fchownAnswer fd None (Some memberGroup) withFd with
                | SyscallAnswer.Completed 0L, after ->
                    let changed = inodeOf "/p/f" after

                    (access, changed.Owner)
                    |> shouldEqual (
                        access,
                        { by with
                            Group = memberGroup
                        }
                    )

                    changed.Times.StatusChange |> shouldEqual now
                | other -> failwith $"fchown through %O{access}: %A{other}"

            let fd, withFd = openedDirectory "/p/d" system

            match fchownAnswer fd None (Some memberGroup) withFd with
            | SyscallAnswer.Completed 0L, after ->
                (inodeOf "/p/d" after).Owner
                |> shouldEqual
                    { by with
                        Group = memberGroup
                    }
            | other -> failwith $"fchown of a directory: %A{other}"

            // A file whose last name has gone.
            let inode = inodeAt system.Machine.FileSystem "/p/f"
            let fd, withFd = opened "/p/f" FileAccessMode.ReadOnly system
            let _, unlinked = Answered.unlink (path "/p/f") withFd

            match fchownAnswer fd None (Some memberGroup) unlinked with
            | SyscallAnswer.Completed 0L, after ->
                match VirtualFileSystem.tryGet inode after.Machine.FileSystem with
                | Some orphan ->
                    orphan.Owner
                    |> shouldEqual
                        { by with
                            Group = memberGroup
                        }
                | None -> failwith "the orphan was freed while a descriptor held it"
            | other -> failwith $"fchown of an orphan: %A{other}"

    [<Test>]
    let ``fchown of someone else's file answers as chown does, however it was opened`` () : unit =
        for platform, credentials, untouched in
            [
                SimulatedUnixPlatform.linuxX64, u1000, false
                SimulatedUnixPlatform.macOsArm64, u501, true
            ] do
            let system =
                systemOn platform credentials (pathTree (InodeOwner.ofProcess credentials))

            let fd, withFd = opened "/p/theirs" FileAccessMode.ReadOnly system

            fchownAnswer fd (Some (uid 1002u)) None withFd
            |> shouldEqual (SyscallAnswer.Failed UnixError.EPERM, withFd)

            fchownAnswer fd None (Some (gid 4242u)) withFd
            |> shouldEqual (SyscallAnswer.Failed UnixError.EPERM, withFd)

            match fchownAnswer fd None None withFd with
            | SyscallAnswer.Completed 0L, after ->
                if untouched then
                    after |> shouldEqual withFd
                else
                    (inodeOf "/p/theirs" after).Times.StatusChange
                    |> shouldEqual (UnixMachineState.realtime system.Machine)
            | other -> failwith $"fchown(-1, -1) of someone else's file: %A{other}"

    [<Test>]
    let ``fchown of a descriptor that is not open is EBADF`` () : unit =
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            let system =
                UnixSystem.initial<int, string> platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
                |> UnixBootImage.boot

            for fd in [ 1000 ; -1 ; 3 ] do
                fchownAnswer fd None None system
                |> shouldEqual (SyscallAnswer.Failed UnixError.EBADF, system)

    [<Test>]
    let ``fchown of a descriptor with no inode answers as each flavour measured`` () : unit =
        let linux =
            UnixSystem.initial<int, string> SimulatedUnixPlatform.linuxX64 UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.boot

        let darwin =
            UnixSystem.initial<int, string> SimulatedUnixPlatform.macOsArm64 UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.boot

        // The flavour's own event port: an epoll instance, or a kqueue.
        let port (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
            let create =
                match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
                | SimulatedUnixFlavour.Linux -> FileDescriptorRegistry.createEpoll
                | SimulatedUnixFlavour.Darwin -> FileDescriptorRegistry.createKqueue

            let fd, registry = create system.Process.FileDescriptors

            fd, withDescriptors registry system

        let asks = [ None, None ; None, Some (gid 2000u) ; Some (uid 1002u), None ]

        // Linux's epoll instance is EOPNOTSUPP whoever asks and whatever it
        // asks; Darwin's kqueue is EINVAL.
        for user, group in asks do
            let fd, withPort = port linux

            fchownAnswer fd user group withPort
            |> shouldEqual (SyscallAnswer.Failed UnixError.EOPNOTSUPP, withPort)

            let fd, withPort = port darwin

            fchownAnswer fd user group withPort
            |> shouldEqual (SyscallAnswer.Failed UnixError.EINVAL, withPort)

        // A socket and a standard stream (one end of a pipe the process was
        // launched with) are EINVAL on Darwin. On Linux both have an owner
        // fchown changes if the caller may, and this kernel holds neither
        // owner, so it refuses.
        for domain, kind, protocol in
            [
                SocketDomain.Inet, SocketKind.Stream, SocketProtocol.Tcp
                SocketDomain.Inet, SocketKind.Datagram, SocketProtocol.Udp
                SocketDomain.Inet6, SocketKind.Stream, SocketProtocol.Tcp
                SocketDomain.Unix, SocketKind.Stream, SocketProtocol.Default
            ] do
            for user, group in asks do
                let fd, withSocket = NewSocket.create domain kind protocol darwin

                fchownAnswer fd user group withSocket
                |> shouldEqual (SyscallAnswer.Failed UnixError.EINVAL, withSocket)

                let fd, withSocket = NewSocket.create domain kind protocol linux

                let socket =
                    match FileDescriptorRegistry.tryFindObject fd withSocket.Process.FileDescriptors with
                    | Some (OpenFileObject.Socket socket) -> socket
                    | other -> failwith $"fd %d{fd} is not a socket: %A{other}"

                UnixPathResolution.fchown fd user group withSocket
                |> shouldEqual (Error (FChOwnRefusal.Socket socket))

        for fd in [ 0 ; 1 ; 2 ] do
            fchownAnswer fd None None darwin
            |> shouldEqual (SyscallAnswer.Failed UnixError.EINVAL, darwin)

            // `UnixSystem.initial` makes the launch table's pipes first, in
            // descriptor order.
            UnixPathResolution.fchown fd None None linux
            |> shouldEqual (Error (FChOwnRefusal.LaunchedPipe (PipeId (int64 fd))))

    [<Test>]
    let ``Darwin fchown refuses a privileged caller`` () : unit =
        let by = owner 501u 20u
        let system = systemOn SimulatedUnixPlatform.macOsArm64 Owners.root (tree by)
        let fd, withFd = opened "/t/f" FileAccessMode.ReadOnly system

        UnixPathResolution.fchown fd None (Some (gid 12u)) withFd
        |> shouldEqual (
            Error (
                FChOwnRefusal.UnmeasuredOwnerChange (
                    inodeAt system.Machine.FileSystem "/t/f",
                    OwnerChangeRefusal.UnmeasuredPrivilegedCaller (
                        Standing.toward Owners.root by,
                        {
                            UserAsked = RequestedUser.Unchanged
                            GroupAsked = RequestedGroup.Other
                        },
                        mode 0o600
                    )
                )
            )
        )

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
    ///
    /// Where the two differ, the process starts as root, makes the pipe while
    /// acting as `creator`, and then becomes `caller`, all through the syscalls.
    /// Only on Linux: this library models no call that changes a Darwin
    /// process's IDs, so there the two must agree.
    let private withPipe
        (platform : SimulatedUnixPlatform)
        (creator : Credentials)
        (caller : Credentials)
        : (int * int) * UnixSystem<int, string>
        =
        let changesIds = creator <> caller

        let system =
            UnixSystem.initial<int, string> platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.withCredentials context (if changesIds then Owners.root else creator)
            |> UnixBootImage.boot

        let system =
            if changesIds then
                Become.temporarily creator system
            else
                system

        let fds, system = pipeOrFail system

        let system =
            { system with
                Machine = UnixMachineState.advanceClock later system.Machine
            }

        let system =
            if changesIds then
                system |> Become.rootAgain |> Become.fully caller
            else
                system

        fds, system

    /// The pipe `fd` names given the bits `bits`, nothing else moving.
    let private withPipeBits (fd : int) (bits : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        let pipeId =
            match FileDescriptorRegistry.tryFindObject fd system.Process.FileDescriptors with
            | Some (OpenFileObject.Pipe pipeId) -> pipeId
            | other -> failwith $"fd %d{fd} is not a pipe: %A{other}"

        let pipe = UnixMachineState.pipe pipeId system.Machine

        let status =
            match pipe.Origin with
            | PipeOrigin.Made status -> status
            | PipeOrigin.Launched _ -> failwith "a launched pipe"

        { system with
            Machine =
                { system.Machine with
                    Pipes =
                        Map.add
                            pipeId
                            { pipe with
                                Origin =
                                    PipeOrigin.Made
                                        { status with
                                            Permissions = mode bits
                                        }
                            }
                            system.Machine.Pipes
                }
        }

    [<Test>]
    let ``Linux fchown changes a pipe's owner through either end, as the probe measured, for every standing``
        ()
        : unit
        =
        // The probe's rows: root hands the pipe to an owner and group, and the
        // row's caller then asks of it. Here the pipe is made by that owner,
        // who then becomes the caller.
        let rows : (string * Credentials * Credentials) list =
            [
                "owner, group = egid", u1000, u1000
                "owner, group = supplementary", Credentials.ofIds (uid 1000u) (gid 2000u) [], u1000
                "owner, group not a member", Credentials.ofIds (uid 1000u) (gid 3000u) [], u1000
                "non-owner, group = egid", Credentials.ofIds (uid 1001u) (gid 1000u) [], u1000
                "non-owner, group not a member", Credentials.ofIds (uid 1001u) (gid 3000u) [], u1000
                "root, non-owner, group not a member", Credentials.ofIds (uid 1001u) (gid 3000u) [], linuxRoot
            ]

        for label, creator, caller in rows do
            let (readFd, writeFd), system =
                withPipe SimulatedUnixPlatform.linuxX64 creator caller

            let by = InodeOwner.ofProcess creator
            let standing = Standing.toward caller by
            let now = UnixMachineState.realtime system.Machine

            for user, group in
                [
                    None, None
                    Some by.User, None
                    None, Some (gid 2000u)
                    Some (uid 1002u), None
                    None, Some (gid 3001u)
                ] do
                let request = OwnerChangeRequest.classify caller by user group

                for raw in 0..0o7777 do
                    let system = withPipeBits readFd raw system
                    let before = fstatOrFail readFd system
                    let fd = if raw % 2 = 0 then readFd else writeFd
                    let answer, after = fchownAnswer fd user group system

                    match
                        reference
                            OwnerChangeRule.ClearsSetIdFromNonDirectories
                            standing
                            request
                            OwnerChangeTarget.NonDirectory
                            raw
                        |> ok,
                        answer
                    with
                    | OwnerChange.Forbidden, SyscallAnswer.Failed UnixError.EPERM ->
                        if after <> system then
                            failwith
                                $"%s{label}: fchown(%A{request}) at 0o%04o{raw} answered EPERM but changed the system"
                    | OwnerChange.Changed bits, SyscallAnswer.Completed 0L ->
                        let expected =
                            { before with
                                Mode = 0o010000 ||| PermissionBits.toInt bits
                                UserId = defaultArg user by.User
                                GroupId = defaultArg group by.Group
                                StatusChangeTime = now
                            }

                        (label, raw, fstatOrFail readFd after) |> shouldEqual (label, raw, expected)
                        let writeEnd = fstatOrFail writeFd after

                        (label, raw, writeEnd.Mode, writeEnd.UserId, writeEnd.GroupId, writeEnd.StatusChangeTime)
                        |> shouldEqual (label, raw, expected.Mode, expected.UserId, expected.GroupId, now)
                    | expected, _ ->
                        failwith
                            $"%s{label}: fchown(%A{request}) at 0o%04o{raw} answered %A{answer}, expected %A{expected}"

    [<Test>]
    let ``Darwin fchown of a pipe end is EINVAL and changes nothing`` () : unit =
        let (readFd, writeFd), system = withPipe SimulatedUnixPlatform.macOsArm64 u501 u501

        for fd in [ readFd ; writeFd ] do
            for user, group in [ None, None ; None, Some (gid 12u) ; Some (uid 1002u), None ] do
                fchownAnswer fd user group system
                |> shouldEqual (SyscallAnswer.Failed UnixError.EINVAL, system)

    // ------------------------------------------------------------- step

    [<Test>]
    let ``chown, lchown and fchown through step agree with the primitives`` () : unit =
        let system =
            systemOn SimulatedUnixPlatform.linuxX64 u1000 (pathTree (InodeOwner.ofProcess u1000))

        let fd, withFd = opened "/p/f" FileAccessMode.ReadOnly system

        let answered (outcome : Result<SyscallOutcome * UnixSystem<int, string>, SyscallRefusal<int>>) =
            match outcome with
            | Ok (SyscallOutcome.Answered answer, after) -> answer, after
            | other -> failwith $"step: %A{other}"

        let memberGroup = Some (gid 2000u)

        UnixSystem.step 1 (Syscall.ChOwn (PathArg.ofPath (path "/p/lf"), None, memberGroup)) system
        |> answered
        |> shouldEqual (chownAnswer "/p/lf" None memberGroup system)

        UnixSystem.step 1 (Syscall.LChOwn (PathArg.ofPath (path "/p/lf"), None, memberGroup)) system
        |> answered
        |> shouldEqual (lchownAnswer "/p/lf" None memberGroup system)

        UnixSystem.step 1 (Syscall.ChOwn (PathArg.ofPath (path "/p/theirs"), Some (uid 1002u), None)) system
        |> answered
        |> shouldEqual (chownAnswer "/p/theirs" (Some (uid 1002u)) None system)

        UnixSystem.step 1 (Syscall.FChOwn (fd, None, memberGroup)) withFd
        |> answered
        |> shouldEqual (fchownAnswer fd None memberGroup withFd)

        // And the refusals come back as the syscall's own.
        let darwin =
            systemOn SimulatedUnixPlatform.macOsArm64 Owners.root (tree (owner 501u 20u))

        let fd, withFd = opened "/t/f" FileAccessMode.ReadOnly darwin

        for call in
            [
                Syscall.ChOwn (PathArg.ofPath (path "/t/f"), None, None)
                Syscall.LChOwn (PathArg.ofPath (path "/t/f"), None, None)
            ] do
            match UnixSystem.step 1 call darwin with
            | Error (SyscallRefusal.ChOwn refusal) ->
                Error refusal
                |> shouldEqual (
                    UnixPathResolution.chown (PathArg.ofPath (path "/t/f")) None None darwin
                    |> Result.map ignore
                )
            | other -> failwith $"step %A{call} as Darwin root: %A{other}"

        match UnixSystem.step 1 (Syscall.FChOwn (fd, None, None)) withFd with
        | Error (SyscallRefusal.FChOwn refusal) ->
            Error refusal
            |> shouldEqual (UnixPathResolution.fchown fd None None withFd |> Result.map ignore)
        | other -> failwith $"step fchown as Darwin root: %A{other}"
