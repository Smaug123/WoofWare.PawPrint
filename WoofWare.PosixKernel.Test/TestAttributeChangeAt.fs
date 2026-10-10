namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `fchmodat(2)` and `fchownat(2)` held to `chmod-chown-at.c`, which measured
/// what `at-dirfd.c` left open about their flags: `AT_SYMLINK_NOFOLLOW` on
/// each kind of final component (NOFOLLOW, LCHOWN, LINKMODE, LINKTIMES), what
/// a Darwin link's own mode then does (LINKUSE), Linux's `AT_EMPTY_PATH` for
/// each kind of `dirfd` (EMPTY, EMPTYPATH), a rejected flag beside an accepted
/// one (FLAGMIX), and a link's and a regular file's set-ID bits under `chown`
/// (LINKSETID); and to `lchmod-rules.c`, which measured a link's own mode
/// further, against `lchmod(3)`: every path (CALLS), every mode on a link, a
/// file and a directory (MODES), the timestamps (TIMES), the umask (UMASK),
/// the bits above `0o7777` (HIGH) and a `dirfd` (DIRFD). Each row is replayed
/// in a cell made as the probe made it.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestAttributeChangeAt =

    let private context : string = "TestAttributeChangeAt"
    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L

    let private name (text : string) : DirectoryEntryName =
        DirectoryEntryName.parseOrFail context text

    let private perms (bits : int) : PermissionBits = PermissionBits.parseOrFail context bits
    let private uid (n : uint32) : UserId = UserId.parseOrFail context n
    let private gid (n : uint32) : GroupId = GroupId.parseOrFail context n

    let private ownedBy (user : uint32) (group : uint32) : InodeOwner =
        {
            User = uid user
            Group = gid group
        }

    // ------------------------------------------------------------ the probe's output

    /// The fields of the probe's lines beginning `tag`, from `resource`.
    let private probeLines (resource : string) (tag : string) : string list list =
        use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream resource

        if isNull stream then
            failwith $"%s{context}: no embedded resource %s{resource}"

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun line -> line.Split '\t' |> List.ofArray)
        |> Seq.filter (fun fields -> List.head fields = tag)
        |> Seq.map List.tail
        |> List.ofSeq

    let private linuxResource : string =
        "WoofWare.PosixKernel.Test.chmodChownAt.linux.txt"

    let private darwinResource : string =
        "WoofWare.PosixKernel.Test.chmodChownAt.darwin.txt"

    let private lchmodLinuxResource : string =
        "WoofWare.PosixKernel.Test.lchmodRules.linux.txt"

    let private lchmodDarwinResource : string =
        "WoofWare.PosixKernel.Test.lchmodRules.darwin.txt"

    /// A cell's label and expectation: `label=expected`, split at the first
    /// `=` after `from`.
    let private cell (text : string) : string * string =
        let at = text.IndexOf '='
        text.Substring (0, at), text.Substring (at + 1)

    // ------------------------------------------------------------ the probe's cell

    /// Who ran a row: the platform, the caller, and the group its new inodes
    /// got. Linux gives a new inode the caller's effective group; the Darwin
    /// probe ran in a directory of group 0, whose group BSD semantics hand on.
    type private Caller =
        {
            Platform : SimulatedUnixPlatform
            Credentials : Credentials
            NewInodeGroup : uint32
        }

    let private callerOf (platform : SimulatedUnixPlatform) (field : string) : Caller =
        let id = uint32 (field.Substring "caller=".Length)

        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux ->
            {
                Platform = platform
                Credentials =
                    if id = 0u then
                        Owners.root
                    else
                        Credentials.ofIds (uid id) (gid id) []
                NewInodeGroup = id
            }
        | SimulatedUnixFlavour.Darwin ->
            {
                Platform = platform
                Credentials = Credentials.ofIds (uid id) (gid 20u) []
                NewInodeGroup = 0u
            }

    let private atFdCwd (caller : Caller) : int =
        AtDirectory.atFdCwd (SimulatedUnixPlatform.flavour caller.Platform)

    let private noFollow (caller : Caller) : int =
        match SimulatedUnixPlatform.flavour caller.Platform with
        | SimulatedUnixFlavour.Linux -> 0x100
        | SimulatedUnixFlavour.Darwin -> 0x20

    let private linuxAtEmptyPath : int = 0x1000

    /// The probes' cell, as the cwd `/c/w` (root's, mode 0777): `f`, `g` (0644),
    /// `d/` (0755) holding `x` and `l -> ../f`, `gone/`, `lf -> f`, `ld -> d`,
    /// `dang -> nx`, `cyc -> cyc`, all the caller's; on Linux uid 2000's
    /// `tl -> f`, `tf` and `td/`, and `ro`, which root opens on the caller's
    /// behalf. Darwin's other user's are root's: the links `/tmp` and
    /// `/private/etc/localtime`, the file `/private/etc/hosts` and the
    /// directory `/private/var/root` (0750). The machine's `localtime` names a
    /// zone file; this one names `hosts`, which the rows observe only as a
    /// 0644 file, as the zone file is. The system is root's, for whatever the
    /// row makes before it drops to the caller.
    let private boot (caller : Caller) : UnixSystem<int, string> =
        let mine =
            Some (ownedBy (UserId.toUInt32 caller.Credentials.EffectiveUser) caller.NewInodeGroup)

        let file owner =
            SeedEntry.File (ImmutableArray<byte>.Empty, perms 0o644, owner)

        let link target owner =
            SeedEntry.Symlink (SymlinkTarget.parseOrFail context target, owner)

        let dir bits owner (entries : (string * SeedEntry) list) =
            SeedEntry.Directory (entries |> List.map (fun (n, e) -> name n, e) |> Map.ofList, perms bits, owner)

        let theirs = Some (ownedBy 2000u 2000u)

        let cell =
            [
                "f", file mine
                "g", file mine
                "ro", file mine
                "d", dir 0o755 mine [ "x", file mine ; "l", link "../f" mine ]
                "gone", dir 0o755 mine []
                "lf", link "f" mine
                "ld", link "d" mine
                "dang", link "nx" mine
                "cyc", link "cyc" mine
            ]
            @ (
                match SimulatedUnixPlatform.flavour caller.Platform with
                | SimulatedUnixFlavour.Linux ->
                    [ "tl", link "f" theirs ; "tf", file theirs ; "td", dir 0o755 theirs [] ]
                | SimulatedUnixFlavour.Darwin -> []
            )

        let darwinSystem =
            match SimulatedUnixPlatform.flavour caller.Platform with
            | SimulatedUnixFlavour.Linux -> []
            | SimulatedUnixFlavour.Darwin ->
                [
                    "tmp", link "private/tmp" None
                    "private",
                    dir
                        0o755
                        None
                        [
                            "tmp", dir 0o1777 None []
                            "etc", dir 0o755 None [ "hosts", file None ; "localtime", link "hosts" None ]
                            "var", dir 0o755 None [ "root", dir 0o750 None [] ]
                        ]
                ]

        let seed =
            [ "c", dir 0o755 None [ "w", dir 0o777 None cell ] ] @ darwinSystem
            |> List.map (fun (n, e) -> name n, e)
            |> Map.ofList

        let image : UnixBootImage<int, string> = UnixSystem.initial caller.Platform

        match UnixBootImage.withFileSystem epoch (ownedBy 0u 0u) seed image with
        | Ok image ->
            Launched.bootWith
                (Launched.credentials Owners.root
                 >> ProcessLaunch.withCurrentDirectory (AbsoluteUnixPath.parseOrFail context "/c/w"))
                UnixSystem.pipedStandardStreams
                0
                (CpuId 0)
                image
        | Error fault -> failwith $"%s{context}: could not build the probe's cell: %A{fault}"

    /// `system` with the caller's credentials, as the probe's child had once it
    /// dropped.
    let private dropped (caller : Caller) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        { system with
            Process =
                { system.Process with
                    Credentials = caller.Credentials
                }
        }

    /// The permission bits `stat` (or `lstat`, under `NoFollowFinal`) reports
    /// for `path`, or `None` where it fails.
    let private modeOf (policy : SymlinkPolicy) (path : string) (system : UnixSystem<int, string>) : int option =
        match UnixPathResolution.stat policy (PathArg.ofText path) system with
        | Ok (FileStatusAnswer.Reported status) -> Some (status.Mode &&& 0o7777)
        | Ok (FileStatusAnswer.Failed _) -> None
        | Error refusal -> failwith $"%s{context}: stat(%s{path}) was refused: %s{StatRefusal.describe refusal}"

    let private ownerOf (path : string) (system : UnixSystem<int, string>) : string =
        match UnixPathResolution.stat SymlinkPolicy.NoFollowFinal (PathArg.ofText path) system with
        | Ok (FileStatusAnswer.Reported status) ->
            $"%d{UserId.toUInt32 status.UserId}:%d{GroupId.toUInt32 status.GroupId}"
        | Ok (FileStatusAnswer.Failed _) -> "-"
        | Error refusal -> failwith $"%s{context}: lstat(%s{path}) was refused: %s{StatRefusal.describe refusal}"

    let private rendered (mode : int option) : string =
        match mode with
        | Some mode -> $"%04o{mode}"
        | None -> "-"

    let private errno (answer : SyscallAnswer) : string =
        match answer with
        | SyscallAnswer.Completed _ -> "ok"
        | SyscallAnswer.Failed error -> $"%A{error}"

    /// The pathname a NOFOLLOW or LCHOWN cell names.
    let private pathOf (caller : Caller) (label : string) : string =
        match label, SimulatedUnixPlatform.flavour caller.Platform with
        | "theirs-link", SimulatedUnixFlavour.Linux -> "tl"
        | "theirs-file", SimulatedUnixFlavour.Linux -> "tf"
        | "theirs-link", SimulatedUnixFlavour.Darwin -> "/tmp"
        | "theirs-file", SimulatedUnixFlavour.Darwin -> "/private/etc/hosts"
        | label, _ -> label

    /// The pathname a CALLS cell of `lchmod-rules.c` names. Its Darwin
    /// "theirs-link" is not `chmod-chown-at.c`'s `/tmp`, which SIP restricts.
    let private lchmodPathOf (caller : Caller) (label : string) : string =
        match label, SimulatedUnixPlatform.flavour caller.Platform with
        | "theirs-link", SimulatedUnixFlavour.Darwin -> "/private/etc/localtime"
        | "theirs-dir", SimulatedUnixFlavour.Linux -> "td"
        | "theirs-dir", SimulatedUnixFlavour.Darwin -> "/private/var/root"
        | label, _ -> pathOf caller label

    let private envelopes : (SimulatedUnixPlatform * string) list =
        [
            SimulatedUnixPlatform.linuxX64, linuxResource
            SimulatedUnixPlatform.macOsArm64, darwinResource
        ]

    let private lchmodEnvelopes : (SimulatedUnixPlatform * string) list =
        [
            SimulatedUnixPlatform.linuxX64, lchmodLinuxResource
            SimulatedUnixPlatform.macOsArm64, lchmodDarwinResource
        ]

    /// `fchmodat(AT_FDCWD, path, mode, AT_SYMLINK_NOFOLLOW)` from `system`, as
    /// the probes print it: the answer, then the mode `lstat` and `stat` report
    /// of `path`, before > after.
    let private noFollowOutcome
        (caller : Caller)
        (path : string)
        (mode : int)
        (system : UnixSystem<int, string>)
        : string
        =
        let lstatBefore = modeOf SymlinkPolicy.NoFollowFinal path system
        let statBefore = modeOf SymlinkPolicy.Follow path system

        match UnixPathResolution.fchmodat (atFdCwd caller) (PathArg.ofText path) mode (noFollow caller) system with
        | Error refusal -> $"refused: %s{FChModAtRefusal.describe refusal}"
        | Ok (answer, after) ->
            $"%s{errno answer} l:%s{rendered lstatBefore}>%s{rendered (modeOf SymlinkPolicy.NoFollowFinal path after)} s:%s{rendered statBefore}>%s{rendered (modeOf SymlinkPolicy.Follow path after)}"

    [<Test>]
    let ``fchmodat with AT_SYMLINK_NOFOLLOW answers every final component the probe tried`` () : unit =
        for platform, resource in envelopes do
            let rows = probeLines resource "NOFOLLOW"
            rows.Length |> shouldBeGreaterThan 0

            for row in rows do
                // glibc's fchmodat and the fchmodat2 syscall answered alike.
                let other = rows |> List.find (fun r -> r.[0] = row.[0] && r.[1] <> row.[1])

                other.[2..] |> shouldEqual row.[2..]

            for row in rows |> List.filter (fun row -> row.[1] = "syscall") do
                let caller = callerOf platform row.[0]

                [
                    for text in row.[2..] do
                        let label, expected = cell text
                        let path = pathOf caller label
                        let system = boot caller |> dropped caller

                        // Another user's link or file is asked for the mode it
                        // already has.
                        let mode =
                            if label.StartsWith "theirs" then
                                Option.get (modeOf SymlinkPolicy.NoFollowFinal path system)
                            else
                                0o640

                        let actual = noFollowOutcome caller path mode system

                        if actual <> expected then
                            yield $"%s{row.[0]} %s{label}: the probe answered %s{expected}, this library %s{actual}"
                ]
                |> shouldEqual []

    [<Test>]
    let ``fchownat with AT_SYMLINK_NOFOLLOW is lchown, for every final component the probe tried`` () : unit =
        for platform, resource in envelopes do
            let rows = probeLines resource "LCHOWN"
            rows.Length |> shouldBeGreaterThan 0

            for row in rows do
                let caller = callerOf platform row.[0]
                let egid = Some caller.Credentials.EffectiveGroup

                [
                    for text in row.[1..] do
                        let label, expected = cell text
                        let path = pathOf caller label

                        let outcome (result : Result<SyscallAnswer * UnixSystem<int, string>, string>) : string =
                            match result with
                            | Ok (answer, after) -> $"%s{errno answer} %s{ownerOf path after}"
                            | Error refusal -> $"refused: %s{refusal}"

                        let viaAt =
                            UnixPathResolution.fchownat
                                (atFdCwd caller)
                                (PathArg.ofText path)
                                None
                                egid
                                (noFollow caller)
                                (boot caller |> dropped caller)
                            |> Result.mapError FChOwnAtRefusal.describe
                            |> outcome

                        let viaLChOwn =
                            UnixPathResolution.lchown (PathArg.ofText path) None egid (boot caller |> dropped caller)
                            |> Result.mapError ChOwnRefusal.describe
                            |> outcome

                        let actual = $"%s{viaAt} / lchown %s{viaLChOwn}"

                        if actual <> expected then
                            yield $"%s{row.[0]} %s{label}: the probe answered %s{expected}, this library %s{actual}"
                ]
                |> shouldEqual []

    /// What `lchmod-rules.c`'s MODES makes its subjects of.
    [<RequireQualifiedAccess>]
    type private Subject =
        | Link
        | File
        | Directory

    let private creating : OpenFlags =
        {
            Access = FileAccessMode.WriteOnly
            Create = true
            Exclusive = true
            Truncate = false
            NoFollow = false
            CloseOnExec = false
            Synchronous = false
            DataSynchronous = false
            Directory = false
        }

    /// A cell holding the caller's own `subject` at each of `names` (a link to
    /// `f`, a 0644 file or a 0755 directory), in the caller's group or out of
    /// it, and the system it is in, which the caller has dropped to.
    let private ownSubjects
        (caller : Caller)
        (subject : Subject)
        (names : string list)
        (inGroup : bool)
        : UnixSystem<int, string>
        =
        let made (system : UnixSystem<int, string>) (name : string) =
            match subject with
            | Subject.Link ->
                match UnixNamespace.symlink (PathArg.ofText "f") (PathArg.ofText name) system with
                | Ok (SyscallAnswer.Completed _, system) -> system
                | other -> failwith $"%s{context}: symlink(f, %s{name}): %A{other}"
            | Subject.File ->
                match Answered.openPath creating (UnixPath.parseOrFail context name) 0o644 system with
                | SyscallAnswer.Completed _, system -> system
                | other -> failwith $"%s{context}: open(%s{name}, O_CREAT): %A{other}"
            | Subject.Directory ->
                match Answered.mkdir (PathArg.ofText name) 0o755 system with
                | SyscallAnswer.Completed _, system -> system
                | other -> failwith $"%s{context}: mkdir(%s{name}): %A{other}"

        let lchowned (user : UserId option) (group : GroupId) (system : UnixSystem<int, string>) (name : string) =
            match UnixPathResolution.lchown (PathArg.ofText name) user (Some group) system with
            | Ok (SyscallAnswer.Completed _, system) -> system
            | other -> failwith $"%s{context}: lchown(%s{name}): %A{other}"

        match SimulatedUnixPlatform.flavour caller.Platform with
        | SimulatedUnixFlavour.Linux ->
            // Made by root and handed over, as the probes did.
            let group =
                if inGroup then
                    caller.Credentials.EffectiveGroup
                else
                    gid 2000u

            let system = names |> List.fold made (boot caller)

            names
            |> List.fold (lchowned (Some caller.Credentials.EffectiveUser) group) system
            |> dropped caller
        | SimulatedUnixFlavour.Darwin ->
            // Made by the caller, in a directory of group 0, which it is not
            // in, and handed to its own group for the in-group case.
            let system = names |> List.fold made (boot caller |> dropped caller)

            if inGroup then
                names |> List.fold (lchowned None caller.Credentials.EffectiveGroup) system
            else
                system

    /// What asking `fchmodat(AT_SYMLINK_NOFOLLOW)` for every mode from 0 to
    /// `0o7777` in turn of one path came to, counted as the probes count it.
    type private Sweep =
        {
            /// Each answer, with how often it came, in the order each first came.
            Answers : (string * int) list
            /// Successes whose mode is not the one `want` names.
            Unexpected : int
            /// Failures that left the mode as it was.
            FailuresLeftMode : int
            /// Failures that changed anything at all in the system.
            FailuresChangedSystem : int
            /// Successes that asked for `S_ISUID`, `S_ISGID` and `S_ISVTX`, and
            /// kept it.
            KeptSetUserId : int
            KeptSetGroupId : int
            KeptSticky : int
        }

    let private sweep (caller : Caller) (path : string) (want : int -> int) (system : UnixSystem<int, string>) : Sweep =
        let empty =
            {
                Answers = []
                Unexpected = 0
                FailuresLeftMode = 0
                FailuresChangedSystem = 0
                KeptSetUserId = 0
                KeptSetGroupId = 0
                KeptSticky = 0
            }

        let count (answer : string) (answers : (string * int) list) =
            if List.exists (fun (a, _) -> a = answer) answers then
                answers |> List.map (fun (a, n) -> if a = answer then a, n + 1 else a, n)
            else
                answers @ [ answer, 1 ]

        let kept (bit : int) (mode : int) (after : int) =
            if mode &&& bit <> 0 && after &&& bit <> 0 then 1 else 0

        [ 0..0o7777 ]
        |> List.fold
            (fun (sweep, system) mode ->
                let before = modeOf SymlinkPolicy.NoFollowFinal path system

                match
                    UnixPathResolution.fchmodat (atFdCwd caller) (PathArg.ofText path) mode (noFollow caller) system
                with
                | Error refusal ->
                    failwith $"%s{context}: %s{path} mode %04o{mode}: %s{FChModAtRefusal.describe refusal}"
                | Ok (SyscallAnswer.Completed _, after) ->
                    let changed = Option.get (modeOf SymlinkPolicy.NoFollowFinal path after)

                    { sweep with
                        Answers = count "ok" sweep.Answers
                        Unexpected = sweep.Unexpected + (if changed = want mode then 0 else 1)
                        KeptSetUserId = sweep.KeptSetUserId + kept 0o4000 mode changed
                        KeptSetGroupId = sweep.KeptSetGroupId + kept 0o2000 mode changed
                        KeptSticky = sweep.KeptSticky + kept 0o1000 mode changed
                    },
                    after
                | Ok (SyscallAnswer.Failed error, after) ->
                    { sweep with
                        Answers = count $"%A{error}" sweep.Answers
                        FailuresLeftMode =
                            sweep.FailuresLeftMode
                            + (if modeOf SymlinkPolicy.NoFollowFinal path after = before then
                                   1
                               else
                                   0)
                        FailuresChangedSystem = sweep.FailuresChangedSystem + (if after = system then 0 else 1)
                    },
                    after
            )
            (empty, system)
        |> fun (swept, final) ->
            // Whatever bits a link took are ones its flavour lets it have.
            UnixSystem.checkInvariants final |> shouldEqual []
            swept

    let private groupOf (path : string) (system : UnixSystem<int, string>) : string =
        let owner = ownerOf path system
        owner.Substring (owner.IndexOf ':' + 1)

    [<Test>]
    let ``every mode asked of a link answers as the probe measured`` () : unit =
        for platform, resource in envelopes do
            let rows = probeLines resource "LINKMODE"
            rows.Length |> shouldBeGreaterThan 0

            for row in rows do
                let caller = callerOf platform row.[0]

                match row.[1] with
                | "theirs" ->
                    let system = boot caller |> dropped caller
                    let path = pathOf caller "theirs-link"
                    let before = modeOf SymlinkPolicy.NoFollowFinal path system

                    match
                        UnixPathResolution.fchmodat
                            (atFdCwd caller)
                            (PathArg.ofText path)
                            (Option.get before)
                            (noFollow caller)
                            system
                    with
                    | Ok (answer, after) ->
                        $"%s{errno answer} %s{rendered before}>%s{rendered (modeOf SymlinkPolicy.NoFollowFinal path after)}"
                        |> shouldEqual row.[2]
                    | Error refusal -> failwith $"%s{context}: theirs: %s{FChModAtRefusal.describe refusal}"
                | group ->

                let inGroup = group = "in-group"
                let system = ownSubjects caller Subject.Link [ "sl" ] inGroup

                // Every bit asked for, less S_ISGID outside the link's group.
                let swept =
                    sweep caller "sl" (fun mode -> if inGroup then mode else mode &&& ~~~0o2000) system

                swept.FailuresChangedSystem |> shouldEqual 0
                let group = groupOf "sl" system

                $"group=%s{group}"
                :: (swept.Answers |> List.map (fun (answer, count) -> $"%s{answer}=%d{count}"))
                @ [
                    $"mismatches=%d{swept.Unexpected}"
                    $"failures-left-mode=%d{swept.FailuresLeftMode}"
                ]
                |> shouldEqual row.[2..]

    /// Whether each of an inode's timestamps moved, as the probes print it.
    let private timesMoved (before : FileStatus option) (after : FileStatus option) : string =
        match before, after with
        | Some before, Some after ->
            let moved (field : FileStatus -> UnixTimestamp) =
                if field before = field after then "kept" else "moved"

            $"atime=%s{moved (fun s -> s.AccessTime)} mtime=%s{moved (fun s -> s.ModificationTime)} ctime=%s{moved (fun s -> s.StatusChangeTime)}"
        | _ -> "-"

    let private statusOf
        (policy : SymlinkPolicy)
        (path : string)
        (system : UnixSystem<int, string>)
        : FileStatus option
        =
        match UnixPathResolution.stat policy (PathArg.ofText path) system with
        | Ok (FileStatusAnswer.Reported status) -> Some status
        | Ok (FileStatusAnswer.Failed _) -> None
        | Error refusal -> failwith $"%s{context}: stat(%s{path}) was refused: %s{StatRefusal.describe refusal}"

    /// `fchmodat(AT_SYMLINK_NOFOLLOW)` of `link`, asking `mode` (or the mode
    /// it has), on a clock moved on since the cell was made: the answer, then
    /// which timestamps of the link, of `target` (named directly, never
    /// through the link) and of `directory` moved.
    let private timesAfter
        (caller : Caller)
        (link : string)
        (mode : int option)
        (target : string)
        (directory : string)
        : string list
        =
        let system = boot caller |> dropped caller |> UnixSystem.advanceClock 20_000_000L
        let linkBefore = statusOf SymlinkPolicy.NoFollowFinal link system
        let targetBefore = statusOf SymlinkPolicy.NoFollowFinal target system
        let directoryBefore = statusOf SymlinkPolicy.NoFollowFinal directory system

        let mode =
            match mode with
            | Some mode -> mode
            | None -> Option.get (modeOf SymlinkPolicy.NoFollowFinal link system)

        match UnixPathResolution.fchmodat (atFdCwd caller) (PathArg.ofText link) mode (noFollow caller) system with
        | Error refusal -> [ $"refused: %s{FChModAtRefusal.describe refusal}" ]
        | Ok (answer, after) ->
            [
                errno answer
                $"link %s{timesMoved linkBefore (statusOf SymlinkPolicy.NoFollowFinal link after)}"
                $"target %s{timesMoved targetBefore (statusOf SymlinkPolicy.NoFollowFinal target after)}"
                $"directory %s{timesMoved directoryBefore (statusOf SymlinkPolicy.NoFollowFinal directory after)}"
            ]

    [<Test>]
    let ``a link's own mode change moves its ctime alone on Darwin, and Linux's refusal moves nothing`` () : unit =
        for platform, resource in envelopes do
            let rows = probeLines resource "LINKTIMES"
            rows.Length |> shouldBeGreaterThan 0

            for row in rows do
                let caller = callerOf platform row.[0]
                let mode = Convert.ToInt32 (row.[1], 8)

                // This probe did not print the directory's.
                timesAfter caller "lf" (Some mode) "f" "."
                |> List.truncate 3
                |> shouldEqual row.[2..]

    [<Test>]
    let ``Darwin's chown clears a file's or a link's set-ID bits exactly when it names an ID`` () : unit =
        let caller = callerOf SimulatedUnixPlatform.macOsArm64 "caller=501"
        let egid = caller.Credentials.EffectiveGroup
        let rows = probeLines darwinResource "LINKSETID"

        rows
        |> List.map (fun row -> row.[1])
        |> List.distinct
        |> shouldEqual [ "link" ; "file" ]

        [
            for row in rows do
                // e.g. "chmod(6777)=ok 6777 chown(-1,egid)=ok 0777"
                let words = row.[2].Split ' '
                let mode = Convert.ToInt32 (words.[0].Substring ("chmod(".Length, 4), 8)

                let group =
                    if words.[2].StartsWith "chown(-1,-1)" then
                        None
                    else
                        Some egid

                // The probe chmodded the link with lchmod, in the caller's
                // group, where `lchmod-rules.c` (MODES) measured it to answer
                // every mode as fchmodat with AT_SYMLINK_NOFOLLOW does.
                let path, flags =
                    match row.[1] with
                    | "link" -> "lf", noFollow caller
                    | _ -> "f", 0

                let system =
                    match
                        UnixPathResolution.fchownat
                            (atFdCwd caller)
                            (PathArg.ofText path)
                            None
                            (Some egid)
                            flags
                            (boot caller |> dropped caller)
                    with
                    | Ok (SyscallAnswer.Completed _, system) -> system
                    | other -> failwith $"%s{context}: chown(%s{path}, -1, egid): %A{other}"

                let system =
                    match UnixPathResolution.fchmodat (atFdCwd caller) (PathArg.ofText path) mode flags system with
                    | Ok (SyscallAnswer.Completed _, system) -> system
                    | other -> failwith $"%s{context}: chmod(%s{path}, %o{mode}): %A{other}"

                match UnixPathResolution.fchownat (atFdCwd caller) (PathArg.ofText path) None group flags system with
                | Ok (answer, after) ->
                    let before = rendered (modeOf SymlinkPolicy.NoFollowFinal path system)
                    let changed = rendered (modeOf SymlinkPolicy.NoFollowFinal path after)
                    let chownLabel = words.[2].Substring (0, words.[2].IndexOf '=')

                    let actual = $"%s{words.[0]} %s{before} %s{chownLabel}=%s{errno answer} %s{changed}"

                    if actual <> row.[2] then
                        yield $"%s{row.[1]}: the probe answered %s{row.[2]}, this library %s{actual}"
                | Error refusal -> yield $"refused: %s{FChOwnAtRefusal.describe refusal}"
        ]
        |> shouldEqual []

    // ------------------------------------------------------------ Linux's AT_EMPTY_PATH

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

    let private opened (p : string) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match Answered.openPath readOnly (UnixPath.parseOrFail context p) 0 system with
        | SyscallAnswer.Completed fd, system -> int fd, system
        | other -> failwith $"%s{context}: open(%s{p}) did not open: %O{other}"

    let private completed (what : string) (answer : SyscallAnswer * UnixSystem<int, string>) : UnixSystem<int, string> =
        match answer with
        | SyscallAnswer.Completed _, system -> system
        | other -> failwith $"%s{context}: %s{what} failed: %O{other}"

    /// The probe's `dirfd` of each kind, made as the probe made it: by the
    /// caller, except `root-opened`, which root opens before it drops. `None`
    /// for a kind this library cannot make.
    let private fixture (caller : Caller) (kind : string) : (int * UnixSystem<int, string>) option =
        let system = boot caller

        let system =
            if kind = "root-opened" then
                system
            else
                dropped caller system

        let made =
            match kind with
            | "cwd" -> Some (atFdCwd caller, system)
            | "minus1" -> Some (-1, system)
            | "closed" -> Some (999, system)
            | "dir" -> Some (opened "d" system)
            | "file" -> Some (opened "f" system)
            | "pipe-read" ->
                match UnixPipe.pipe2 0 UserBuffer.Mapped system with
                | Ok (Pipe2Answer.Created (readFd, _), system) -> Some (readFd, system)
                | other -> failwith $"%s{context}: pipe2: %A{other}"
            | "pipe-write" ->
                match UnixPipe.pipe2 0 UserBuffer.Mapped system with
                | Ok (Pipe2Answer.Created (_, writeFd), system) -> Some (writeFd, system)
                | other -> failwith $"%s{context}: pipe2: %A{other}"
            | "socket" -> Some (NewSocket.create SocketDomain.Unix SocketKind.Stream SocketProtocol.Default system)
            | "epoll" ->
                match UnixPoll.epollCreate1 0 system with
                | Ok (Ok created) -> Some created
                | other -> failwith $"%s{context}: epoll_create1: %A{other}"
            | "devnull" -> Some (opened "/dev/null" system)
            | "orphan" ->
                let fd, system = opened "gone" system

                Some (
                    fd,
                    Answered.rmdir (UnixPath.parseOrFail context "gone") system
                    |> completed "rmdir(gone)"
                )
            | "unlinked" ->
                let fd, system = opened "g" system

                Some (
                    fd,
                    Answered.unlink (UnixPath.parseOrFail context "g") system
                    |> completed "unlink(g)"
                )
            | "locked" ->
                let fd, system = opened "d" system

                match UnixPathResolution.chmod (PathArg.ofText "d") 0 system with
                | Ok answer -> Some (fd, completed "chmod(d, 0)" answer)
                | Error refusal -> failwith $"%s{context}: chmod(d, 0): %s{ChModRefusal.describe refusal}"
            | "moved" ->
                let fd, system = opened "d" system

                match UnixNamespace.rename (PathArg.ofText "d") (PathArg.ofText "d-moved") system with
                | Ok answer -> Some (fd, completed "rename(d, d-moved)" answer)
                | Error refusal -> failwith $"%s{context}: rename: %s{RenameRefusal.describe refusal}"
            | "theirs" -> Some (opened "tf" system)
            | "root-opened" -> Some (opened "ro" system)
            // O_PATH is not modelled.
            | "opath-file"
            | "opath-link" -> None
            | other -> failwith $"%s{context}: the probe has no dirfd kind called %s{other}"

        made |> Option.map (fun (fd, system) -> fd, dropped caller system)

    /// The mode and owners the probe's EMPTY rows ask for: the mode with
    /// group read flipped (`/dev/null` asked for the mode it has), and the
    /// caller's own group or uid 2000 (`/dev/null` given to nobody).
    let private emptyRequest
        (caller : Caller)
        (kind : string)
        (dirfd : int)
        (system : UnixSystem<int, string>)
        : int * (UserId option * GroupId option) list
        =
        let current =
            let status =
                if kind = "cwd" then
                    UnixPathResolution.stat SymlinkPolicy.Follow (PathArg.ofText ".") system
                    |> Result.mapError StatRefusal.describe
                else
                    UnixPathResolution.fstat dirfd system |> Result.mapError FStatRefusal.describe

            // A descriptor this library will not report the status of (a
            // socket, an epoll instance) is asked for a mode of its own: the
            // call and its reference are asked the same.
            match status with
            | Ok (FileStatusAnswer.Reported status) -> status.Mode &&& 0o7777
            | Ok (FileStatusAnswer.Failed _)
            | Error _ -> 0o644

        let devnull = kind = "devnull"

        (if devnull then current else current ^^^ 0o040),
        [
            None, Some (gid caller.NewInodeGroup)
            (if devnull then None else Some (uid 2000u)), None
        ]

    [<Test>]
    let ``Linux's AT_EMPTY_PATH with the empty path is fchmod or fchown of dirfd, and NULL is EFAULT`` () : unit =
        let rows = probeLines linuxResource "EMPTY"
        rows.Length |> shouldBeGreaterThan 0

        let unreplayed =
            [
                for row in rows do
                    let caller = callerOf SimulatedUnixPlatform.linuxX64 row.[0]
                    let op = row.[1]
                    let kind = row.[2]
                    let _, expectedReference = cell row.[3]

                    match fixture caller kind with
                    | None -> yield kind
                    | Some (dirfd, system) ->

                    let mode, owners = emptyRequest caller kind dirfd system

                    let user, group =
                        match op with
                        | "fchmodat" -> None, None
                        | "fchownat(-1,own)" -> owners.[0]
                        | "fchownat(2000,-1)" -> owners.[1]
                        | other -> failwith $"%s{context}: the probe has no EMPTY operation %s{other}"

                    // The call itself, and what fchmod or fchown of the
                    // descriptor (chmod or chown of "." for AT_FDCWD) does.
                    let call (path : PathArgumentBytes) (flags : int) =
                        if op = "fchmodat" then
                            UnixPathResolution.fchmodat dirfd path mode flags system
                            |> Result.mapError FChModAtRefusal.describe
                        else
                            UnixPathResolution.fchownat dirfd path user group flags system
                            |> Result.mapError FChOwnAtRefusal.describe

                    let reference =
                        match op, kind with
                        | "fchmodat", "cwd" ->
                            UnixPathResolution.chmod (PathArg.ofText ".") mode system
                            |> Result.mapError ChModRefusal.describe
                        | "fchmodat", _ ->
                            UnixPathResolution.fchmod dirfd mode system
                            |> Result.mapError FChModRefusal.describe
                        | _, "cwd" ->
                            UnixPathResolution.chown (PathArg.ofText ".") user group system
                            |> Result.mapError ChOwnRefusal.describe
                        | _, _ ->
                            UnixPathResolution.fchown dirfd user group system
                            |> Result.mapError FChOwnRefusal.describe

                    match reference with
                    | Ok (answer, _) when errno answer <> expectedReference ->
                        yield
                            $"%s{row.[0]} %s{op} %s{kind}: the reference answered %s{errno answer}, the probe's %s{expectedReference}"
                    | Ok _
                    | Error _ -> ()

                    for text in row.[4..] do
                        let label, expected = cell text
                        let flags = linuxAtEmptyPath ||| (if label.EndsWith "NOFOLLOW" then 0x100 else 0)

                        if label.StartsWith "NULL" then
                            if not (expected.StartsWith "EFAULT(") then
                                yield $"%s{row.[0]} %s{op} %s{kind} %s{label}: the probe answered %s{expected}"

                            if
                                call PathArgumentBytes.Unreadable flags
                                <> Ok (SyscallAnswer.Failed UnixError.EFAULT, system)
                            then
                                yield $"%s{row.[0]} %s{op} %s{kind} %s{label}: not EFAULT"
                        else
                            if expected <> "same" then
                                yield $"%s{row.[0]} %s{op} %s{kind} %s{label}: the probe answered %s{expected}"

                            match call (PathArg.ofText "") flags, reference with
                            | Ok outcome, Ok expected when outcome = expected -> ()
                            | Error refusal, Error expected when refusal = expected -> ()
                            | outcome, expected ->
                                yield
                                    $"%s{row.[0]} %s{op} %s{kind} %s{label}: this library answered %A{outcome} where the reference answered %A{expected}"
            ]

        unreplayed
        |> List.filter (fun r -> r <> "opath-file" && r <> "opath-link")
        |> shouldEqual []

    [<Test>]
    let ``Linux's AT_EMPTY_PATH changes nothing for a path that is not empty`` () : unit =
        let rows = probeLines linuxResource "EMPTYPATH"
        rows.Length |> shouldBeGreaterThan 0

        [
            for row in rows do
                let caller = callerOf SimulatedUnixPlatform.linuxX64 row.[0]
                let kind = row.[1]

                for text in row.[2..] do
                    let label, expected = cell text

                    match fixture caller kind with
                    | None -> ()
                    | Some (dirfd, system) ->

                    let flags =
                        if label.EndsWith " AT_EMPTY_PATH" then
                            linuxAtEmptyPath
                        else
                            0

                    let path =
                        match label.Replace (" AT_EMPTY_PATH", "") with
                        | "rooted f" -> "/c/w/f"
                        | path -> path

                    let actual =
                        match UnixPathResolution.fchmodat dirfd (PathArg.ofText path) 0o600 flags system with
                        | Ok (answer, after) ->
                            let mode =
                                match UnixPathResolution.fstatat dirfd (PathArg.ofText path) 0 after with
                                | Ok (FileStatusAnswer.Reported status) -> Some (status.Mode &&& 0o7777)
                                | Ok (FileStatusAnswer.Failed _) -> None
                                | Error refusal -> failwith $"%s{context}: fstatat: %s{FStatAtRefusal.describe refusal}"

                            $"%s{errno answer}:%s{rendered mode}"
                        | Error refusal -> $"refused: %s{FChModAtRefusal.describe refusal}"

                    if actual <> expected then
                        yield
                            $"%s{row.[0]} %s{kind} %s{label}: the probe answered %s{expected}, this library %s{actual}"
        ]
        |> shouldEqual []

    // ------------------------------------------------------------ flag words

    [<Test>]
    let ``a rejected flag beside an accepted one is EINVAL`` () : unit =
        for platform, resource in envelopes do
            for row in probeLines resource "FLAGMIX" do
                let caller = callerOf platform row.[0]

                [
                    for text in row.[2..] do
                        let label, expected = cell text
                        let flags = label.Split '|' |> Array.sumBy (fun bit -> Convert.ToInt32 (bit, 16))
                        let system = boot caller |> dropped caller

                        let actual =
                            match row.[1] with
                            | "fchmodat" ->
                                UnixPathResolution.fchmodat (atFdCwd caller) (PathArg.ofText "f") 0o644 flags system
                                |> Result.map (fst >> errno)
                                |> Result.mapError (fun r -> r = FChModAtRefusal.UnmodelledFlags flags)
                            | _ ->
                                UnixPathResolution.fchownat (atFdCwd caller) (PathArg.ofText "f") None None flags system
                                |> Result.map (fst >> errno)
                                |> Result.mapError (fun r -> r = FChOwnAtRefusal.UnmodelledFlags flags)

                        match actual with
                        | Ok actual when actual = expected -> ()
                        // A flag this library does not model, which the probe
                        // saw accepted.
                        | Error true when expected = "ok" && flags &&& (0x800 ||| 0x2000 ||| 0x8000) <> 0 -> ()
                        | actual ->
                            yield
                                $"%s{row.[0]} %s{row.[1]} %s{label}: the probe answered %s{expected}, this library %A{actual}"
                ]
                |> shouldEqual []

    // ------------------------------------------------------------ a Darwin link's own mode, in use

    let private openAnswer (path : string) (system : UnixSystem<int, string>) : string =
        Answered.openPath readOnly (UnixPath.parseOrFail context path) 0 system
        |> fst
        |> errno

    let private statAnswer (policy : SymlinkPolicy) (path : string) (system : UnixSystem<int, string>) : string =
        match UnixPathResolution.stat policy (PathArg.ofText path) system with
        | Ok (FileStatusAnswer.Reported _) -> "ok"
        | Ok (FileStatusAnswer.Failed error) -> $"%A{error}"
        | Error refusal -> failwith $"%s{context}: stat(%s{path}) was refused: %s{StatRefusal.describe refusal}"

    [<Test>]
    let ``what a Darwin link's own mode changes for its owner is readlink and faccessat of the link alone`` () : unit =
        let caller = callerOf SimulatedUnixPlatform.macOsArm64 "caller=501"
        let rows = probeLines darwinResource "LINKUSE"
        rows.Length |> shouldBeGreaterThan 0

        [
            for row in rows do
                let mode = Convert.ToInt32 (row.[1], 8)

                // The probe gave both links the mode with lchmod, as their
                // owner in its own group, where `lchmod-rules.c` (MODES)
                // measured it to answer as fchmodat with AT_SYMLINK_NOFOLLOW.
                let system =
                    [ "lf" ; "ld" ]
                    |> List.fold
                        (fun system link ->
                            match
                                UnixPathResolution.fchmodat
                                    (atFdCwd caller)
                                    (PathArg.ofText link)
                                    mode
                                    (noFollow caller)
                                    system
                            with
                            | Ok (SyscallAnswer.Completed _, system) -> system
                            | other -> failwith $"%s{context}: fchmodat(%s{link}, %04o{mode}): %A{other}"
                        )
                        (boot caller |> dropped caller)

                let readlink =
                    match UnixNamespace.readlink (PathArg.ofText "lf") UserBuffer.Mapped 64 system with
                    | Ok (ReadLinkAnswer.Reported _) -> "ok"
                    | Ok (ReadLinkAnswer.Failed error) -> $"%A{error}"
                    | Error refusal -> $"refused: %s{ReadLinkRefusal.describe refusal}"

                let access =
                    match
                        UnixPathResolution.faccessat (atFdCwd caller) (PathArg.ofText "lf") 4 (noFollow caller) system
                    with
                    | Ok answer -> errno answer
                    | Error refusal -> $"refused: %s{AccessRefusal.describe refusal}"

                let lmode = rendered (modeOf SymlinkPolicy.NoFollowFinal "lf" system)
                let stat = statAnswer SymlinkPolicy.Follow "lf" system
                let opened = openAnswer "lf" system
                let walk = statAnswer SymlinkPolicy.Follow "ld/x" system

                let actual =
                    [
                        $"lmode=%s{lmode}"
                        $"stat=%s{stat}"
                        $"readlink=%s{readlink}"
                        $"open=%s{opened}"
                        $"walk=%s{walk}"
                        $"faccessat(R_OK,NOFOLLOW)=%s{access}"
                    ]

                if actual <> row.[2..] then
                    yield $"%s{row.[1]}: the probe answered %A{row.[2..]}, this library %A{actual}"
        ]
        |> shouldEqual []

    // ------------------------------------------------------------ lchmod-rules.c

    /// The probe's rows tagged `tag`, keyed by caller and call, for the calls
    /// this library has: `fchmodat` through libc and, on Linux, the
    /// `fchmodat2` syscall, which every row checks agree.
    let private fchmodatRows (resource : string) (tag : string) : string list list =
        let rows = probeLines resource tag
        rows.Length |> shouldBeGreaterThan 0

        let ofCall (call : string) =
            rows
            |> List.filter (fun row -> row.[1] = call)
            |> List.map (fun row -> row.[0] :: row.[2..])

        let raw = ofCall "fchmodat2"

        if not raw.IsEmpty then
            raw |> shouldEqual (ofCall "fchmodat")

        rows |> List.filter (fun row -> row.[1] = "fchmodat")

    [<Test>]
    let ``lchmod is fchmodat with AT_SYMLINK_NOFOLLOW on Linux, and on Darwin answers another user's inode otherwise``
        ()
        : unit
        =
        // What a client implementing lchmod(3) should know: on Linux it is
        // this library's call, and on Darwin it is setattrlist(2), which this
        // library does not model.
        for platform, resource in lchmodEnvelopes do
            let rows = probeLines resource "CALLS"

            for caller in rows |> List.map (fun row -> row.[0]) |> List.distinct do
                let ofCall (call : string) =
                    rows
                    |> List.find (fun row -> row.[0] = caller && row.[1] = call)
                    |> List.skip 2
                    |> List.map cell

                let differing =
                    List.zip (ofCall "lchmod") (ofCall "fchmodat")
                    |> List.filter (fun (l, f) -> l <> f)

                match SimulatedUnixPlatform.flavour platform with
                | SimulatedUnixFlavour.Linux -> differing |> shouldEqual []
                | SimulatedUnixFlavour.Darwin ->
                    differing
                    |> List.map (fun ((label, l), (_, f)) -> label, l.Split(' ').[0], f.Split(' ').[0])
                    |> shouldEqual
                        [
                            "theirs-link", "EACCES", "EPERM"
                            "theirs-file", "EACCES", "EPERM"
                            "theirs-dir", "EACCES", "EPERM"
                        ]

    [<Test>]
    let ``fchmodat with AT_SYMLINK_NOFOLLOW answers every path lchmod-rules tried`` () : unit =
        for platform, resource in lchmodEnvelopes do
            [
                for row in fchmodatRows resource "CALLS" do
                    let caller = callerOf platform row.[0]

                    for text in row.[2..] do
                        let label, expected = cell text

                        let actual =
                            noFollowOutcome caller (lchmodPathOf caller label) 0o640 (boot caller |> dropped caller)

                        if actual <> expected then
                            yield
                                $"%O{platform} %s{row.[0]} %s{label}: the probe answered %s{expected}, this library %s{actual}"
            ]
            |> shouldEqual []

    [<Test>]
    let ``every mode asked of a link, a file or a directory answers as lchmod-rules measured`` () : unit =
        for platform, resource in lchmodEnvelopes do
            let rows = probeLines resource "MODES"
            rows.Length |> shouldBeGreaterThan 0

            // Each row counts lchmod's answers, and says where fchmodat's (for a
            // link) or chmod's (otherwise) differed. Where they did not, the
            // counts are this library's call's too.
            let agreeing, differing =
                rows |> List.partition (fun row -> row.[3..] |> List.contains "differs=0")

            // Darwin's lchmod is setattrlist(2), which refuses S_ISGID outside
            // the group where chmod drops it; fchmodat's answers there are
            // `chmod-chown-at.c`'s LINKMODE rows, replayed above.
            differing
            |> List.map (fun row -> row.[1], row.[2])
            |> shouldEqual (
                match SimulatedUnixPlatform.flavour platform with
                | SimulatedUnixFlavour.Linux -> []
                | SimulatedUnixFlavour.Darwin ->
                    [ "link", "out-of-group" ; "file", "out-of-group" ; "dir", "out-of-group" ]
            )

            [
                for row in agreeing do
                    let caller = callerOf platform row.[0]
                    let inGroup = row.[2] = "in-group"
                    let root = UserId.toUInt32 caller.Credentials.EffectiveUser = 0u

                    let subject =
                        match row.[1] with
                        | "link" -> Subject.Link
                        | "file" -> Subject.File
                        | "dir" -> Subject.Directory
                        | other -> failwith $"%s{context}: the probe has no MODES subject %s{other}"

                    let system = ownSubjects caller subject [ "a" ] inGroup

                    // Every bit asked for, less S_ISGID outside the group, except
                    // for Linux's root, which keeps every bit.
                    let swept =
                        sweep caller "a" (fun mode -> if inGroup || root then mode else mode &&& ~~~0o2000) system

                    let group = groupOf "a" system

                    let actual =
                        $"group=%s{group}"
                        :: (swept.Answers |> List.map (fun (answer, count) -> $"%s{answer}=%d{count}"))
                        @ [
                            "differs=0"
                            $"unexpected=%d{swept.Unexpected}"
                            $"kept-suid=%d{swept.KeptSetUserId}"
                            $"kept-sgid=%d{swept.KeptSetGroupId}"
                            $"kept-svtx=%d{swept.KeptSticky}"
                        ]

                    if swept.FailuresChangedSystem <> 0 then
                        yield
                            $"%s{row.[0]} %s{row.[1]} %s{row.[2]}: %d{swept.FailuresChangedSystem} failures changed the system"

                    if actual <> row.[3..] then
                        yield
                            $"%s{row.[0]} %s{row.[1]} %s{row.[2]}: the probe answered %A{row.[3..]}, this library %A{actual}"
            ]
            |> shouldEqual []

    [<Test>]
    let ``a link's own mode change moves its ctime alone, even to the mode it has, and a refusal moves nothing``
        ()
        : unit
        =
        for platform, resource in lchmodEnvelopes do
            [
                for row in fchmodatRows resource "TIMES" do
                    let caller = callerOf platform row.[0]
                    let theirs = row.[2].StartsWith "theirs-link"

                    let link =
                        if theirs then
                            lchmodPathOf caller "theirs-link"
                        else
                            row.[2].Split(' ').[0]

                    let mode =
                        if row.[2].EndsWith " same" then
                            None
                        else
                            Some (Convert.ToInt32 (row.[2].Split(' ').[1], 8))

                    // Each link's target by its own name, and the directory
                    // holding it.
                    let target, directory =
                        match link, SimulatedUnixPlatform.flavour platform with
                        | "dang", _ -> "nx", "."
                        | "/private/etc/localtime", SimulatedUnixFlavour.Darwin -> "/private/etc/hosts", "/private/etc"
                        | _ -> "f", "."

                    let actual = timesAfter caller link mode target directory

                    if actual <> row.[3..] then
                        yield $"%s{row.[0]} %s{row.[2]}: the probe answered %A{row.[3..]}, this library %A{actual}"
            ]
            |> shouldEqual []

    [<Test>]
    let ``the umask plays no part in a link's own mode, and the bits above 0o7777 are ignored`` () : unit =
        for platform, resource in lchmodEnvelopes do
            [
                for row in fchmodatRows resource "UMASK" do
                    let caller = callerOf platform row.[0]
                    let umask = Convert.ToInt32 (row.[2], 8)
                    let _, system = boot caller |> dropped caller |> UnixSystem.umask umask

                    let actual =
                        match
                            UnixPathResolution.fchmodat
                                (atFdCwd caller)
                                (PathArg.ofText "lf")
                                0o777
                                (noFollow caller)
                                system
                        with
                        | Ok (answer, after) ->
                            let lmode = rendered (modeOf SymlinkPolicy.NoFollowFinal "lf" after)
                            $"%s{errno answer} %s{lmode}"
                        | Error refusal -> $"refused: %s{FChModAtRefusal.describe refusal}"

                    if actual <> row.[3] then
                        yield $"UMASK %s{row.[0]}: the probe answered %s{row.[3]}, this library %s{actual}"

                for row in fchmodatRows resource "HIGH" do
                    let caller = callerOf platform row.[0]
                    let path = row.[2]

                    let actual =
                        match
                            UnixPathResolution.fchmodat
                                (atFdCwd caller)
                                (PathArg.ofText path)
                                0o170640
                                (noFollow caller)
                                (boot caller |> dropped caller)
                        with
                        | Ok (answer, after) ->
                            $"%s{errno answer} l:%s{rendered (modeOf SymlinkPolicy.NoFollowFinal path after)} s:%s{rendered (modeOf SymlinkPolicy.Follow path after)}"
                        | Error refusal -> $"refused: %s{FChModAtRefusal.describe refusal}"

                    if actual <> row.[3] then
                        yield $"HIGH %s{row.[0]} %s{path}: the probe answered %s{row.[3]}, this library %s{actual}"
            ]
            |> shouldEqual []

    [<Test>]
    let ``a link's own mode changes from a dirfd as from the current directory`` () : unit =
        for platform, resource in lchmodEnvelopes do
            [
                for row in fchmodatRows resource "DIRFD" do
                    let caller = callerOf platform row.[0]

                    let flags =
                        match row.[2] with
                        | "AT_SYMLINK_NOFOLLOW" -> noFollow caller
                        | _ -> 0

                    let dirfd, system = opened "d" (boot caller |> dropped caller)

                    let actual =
                        match UnixPathResolution.fchmodat dirfd (PathArg.ofText "l") 0o640 flags system with
                        | Ok (answer, after) ->
                            let link = rendered (modeOf SymlinkPolicy.NoFollowFinal "d/l" after)
                            let target = rendered (modeOf SymlinkPolicy.NoFollowFinal "f" after)
                            $"%s{errno answer} l:%s{link} f:%s{target}"
                        | Error refusal -> $"refused: %s{FChModAtRefusal.describe refusal}"

                    if actual <> row.[3] then
                        yield $"%s{row.[0]} %s{row.[2]}: the probe answered %s{row.[3]}, this library %s{actual}"
            ]
            |> shouldEqual []
