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
/// each kind of final component (NOFOLLOW, LCHOWN, LINKMODE, LINKTIMES),
/// Linux's `AT_EMPTY_PATH` for each kind of `dirfd` (EMPTY, EMPTYPATH), a
/// rejected flag beside an accepted one (FLAGMIX), and, beside a symbolic
/// link's, a regular file's set-ID bits under `chown` (LINKSETID). Each row is
/// replayed in a cell made as the probe made it.
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

    /// The probe's cell, as the cwd `/c/w` (root's, mode 0777): `f`, `g` (0644),
    /// `d/` (0755) holding `x`, `gone/`, `lf -> f`, `ld -> d`, `dang -> nx`,
    /// `cyc -> cyc`, all the caller's; on Linux uid 2000's `tl -> f` and `tf`,
    /// and `ro`, which root opens on the caller's behalf. Darwin's other user's
    /// link and file are `/tmp` and `/private/etc/hosts`, root's. The system is
    /// root's, for whatever the row makes before it drops to the caller.
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
                "d", dir 0o755 mine [ "x", file mine ]
                "gone", dir 0o755 mine []
                "lf", link "f" mine
                "ld", link "d" mine
                "dang", link "nx" mine
                "cyc", link "cyc" mine
            ]
            @ (
                match SimulatedUnixPlatform.flavour caller.Platform with
                | SimulatedUnixFlavour.Linux -> [ "tl", link "f" theirs ; "tf", file theirs ]
                | SimulatedUnixFlavour.Darwin -> []
            )

        let darwinSystem =
            match SimulatedUnixPlatform.flavour caller.Platform with
            | SimulatedUnixFlavour.Linux -> []
            | SimulatedUnixFlavour.Darwin ->
                [
                    "tmp", link "private/tmp" None
                    "private",
                    dir 0o755 None [ "tmp", dir 0o1777 None [] ; "etc", dir 0o755 None [ "hosts", file None ] ]
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

    let private envelopes : (SimulatedUnixPlatform * string) list =
        [
            SimulatedUnixPlatform.linuxX64, linuxResource
            SimulatedUnixPlatform.macOsArm64, darwinResource
        ]

    /// The NOFOLLOW cells this library refuses rather than answers: Darwin's
    /// owner changing its own link's mode.
    let private refusedLinks : Set<string> = set [ "lf" ; "ld" ; "dang" ; "cyc" ]

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
                let flavour = SimulatedUnixPlatform.flavour platform

                let mismatches, refused =
                    row.[2..]
                    |> List.fold
                        (fun (mismatches, refused) text ->
                            let label, expected = cell text
                            let path = pathOf caller label
                            let system = boot caller |> dropped caller
                            let lstatBefore = modeOf SymlinkPolicy.NoFollowFinal path system
                            let statBefore = modeOf SymlinkPolicy.Follow path system

                            let mode =
                                if label.StartsWith "theirs" then
                                    Option.get lstatBefore
                                else
                                    0o640

                            match
                                UnixPathResolution.fchmodat
                                    (atFdCwd caller)
                                    (PathArg.ofText path)
                                    mode
                                    (noFollow caller)
                                    system
                            with
                            | Error (FChModAtRefusal.ChMod (ChModRefusal.SymlinkMode (_, bits))) ->
                                // The probe's link took exactly the bits the
                                // refusal says it would.
                                let measured = expected.Substring(expected.IndexOf "l:").Split(' ').[0]

                                measured
                                |> shouldEqual $"l:%04o{Option.get lstatBefore}>%04o{PermissionBits.toInt bits}"

                                mismatches, Set.add label refused
                            | Error refusal ->
                                failwith
                                    $"%s{context}: %A{flavour} %s{label}: refused: %s{FChModAtRefusal.describe refusal}"
                            | Ok (answer, after) ->
                                let actual =
                                    $"%s{errno answer} l:%s{rendered lstatBefore}>%s{rendered (modeOf SymlinkPolicy.NoFollowFinal path after)} s:%s{rendered statBefore}>%s{rendered (modeOf SymlinkPolicy.Follow path after)}"

                                if actual = expected then
                                    mismatches, refused
                                else
                                    $"%s{row.[0]} %s{label}: the probe answered %s{expected}, this library %s{actual}"
                                    :: mismatches,
                                    refused
                        )
                        ([], Set.empty)

                mismatches |> shouldEqual []

                match flavour with
                | SimulatedUnixFlavour.Linux -> refused |> shouldEqual Set.empty
                | SimulatedUnixFlavour.Darwin -> refused |> shouldEqual refusedLinks

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

    /// A cell with the caller's own link `sl -> f`, in the caller's group or
    /// out of it, and the system it is in, which the caller has dropped to.
    let private ownLink (caller : Caller) (inGroup : bool) : UnixSystem<int, string> =
        let symlinked (system : UnixSystem<int, string>) =
            match UnixNamespace.symlink (PathArg.ofText "f") (PathArg.ofText "sl") system with
            | Ok (SyscallAnswer.Completed _, system) -> system
            | other -> failwith $"%s{context}: symlink(f, sl): %A{other}"

        let lchowned (user : UserId option) (group : GroupId) (system : UnixSystem<int, string>) =
            match UnixPathResolution.lchown (PathArg.ofText "sl") user (Some group) system with
            | Ok (SyscallAnswer.Completed _, system) -> system
            | other -> failwith $"%s{context}: lchown(sl): %A{other}"

        match SimulatedUnixPlatform.flavour caller.Platform with
        | SimulatedUnixFlavour.Linux ->
            // Made by root and handed over, as the probe did.
            let group =
                if inGroup then
                    caller.Credentials.EffectiveGroup
                else
                    gid 2000u

            boot caller
            |> symlinked
            |> lchowned (Some caller.Credentials.EffectiveUser) group
            |> dropped caller
        | SimulatedUnixFlavour.Darwin ->
            // Made by the caller, in a directory of group 0, which it is not
            // in, and handed to its own group for the in-group case.
            let system = boot caller |> dropped caller |> symlinked

            if inGroup then
                lchowned None caller.Credentials.EffectiveGroup system
            else
                system

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
                let system = ownLink caller inGroup

                match SimulatedUnixPlatform.flavour platform with
                | SimulatedUnixFlavour.Linux ->
                    let answers =
                        [
                            for mode in 0..0o7777 do
                                match
                                    UnixPathResolution.fchmodat
                                        (atFdCwd caller)
                                        (PathArg.ofText "sl")
                                        mode
                                        (noFollow caller)
                                        system
                                with
                                | Ok (answer, after) when after = system -> errno answer
                                | Ok (answer, _) -> $"%s{errno answer} and changed the system"
                                | Error refusal -> $"refused: %s{FChModAtRefusal.describe refusal}"
                        ]
                        |> List.countBy id

                    let linkGroup = ownerOf "sl" system
                    let group = linkGroup.Substring (linkGroup.IndexOf ':' + 1)

                    $"group=%s{group}"
                    :: (answers |> List.map (fun (answer, count) -> $"%s{answer}=%d{count}"))
                    @ [ "mismatches=0" ; $"failures-left-mode=%d{List.sumBy snd answers}" ]
                    |> shouldEqual row.[2..]
                | SimulatedUnixFlavour.Darwin ->
                    // The probe set every mode as asked, less S_ISGID outside
                    // the link's group; this library refuses each, naming the
                    // bits Darwin set.
                    row.[2..]
                    |> List.tail
                    |> shouldEqual [ "ok=4096" ; "mismatches=0" ; "failures-left-mode=0" ]

                    for mode in 0..0o7777 do
                        let expected = if inGroup then mode else mode &&& ~~~0o2000

                        match
                            UnixPathResolution.fchmodat
                                (atFdCwd caller)
                                (PathArg.ofText "sl")
                                mode
                                (noFollow caller)
                                system
                        with
                        | Error (FChModAtRefusal.ChMod (ChModRefusal.SymlinkMode (_, bits))) ->
                            PermissionBits.toInt bits |> shouldEqual expected
                        | other -> failwith $"%s{context}: Darwin link mode %04o{mode}: %A{other}"

    [<Test>]
    let ``Linux's refusal to change a link's mode moves none of its timestamps`` () : unit =
        let row =
            probeLines linuxResource "LINKTIMES" |> List.map (fun row -> row.[0], row.[1..])

        for field, cells in row do
            let caller = callerOf SimulatedUnixPlatform.linuxX64 field

            cells
            |> shouldEqual
                [
                    "0700"
                    "EOPNOTSUPP"
                    "link atime=kept mtime=kept ctime=kept"
                    "target atime=kept mtime=kept ctime=kept"
                ]

            let system = boot caller |> dropped caller

            UnixPathResolution.fchmodat (atFdCwd caller) (PathArg.ofText "lf") 0o700 (noFollow caller) system
            |> shouldEqual (Ok (SyscallAnswer.Failed UnixError.EOPNOTSUPP, system))

    [<Test>]
    let ``Darwin's chown clears a file's set-ID bits exactly when it names an ID`` () : unit =
        let caller = callerOf SimulatedUnixPlatform.macOsArm64 "caller=501"
        let egid = caller.Credentials.EffectiveGroup

        [
            for row in
                probeLines darwinResource "LINKSETID"
                |> List.filter (fun row -> row.[1] = "file") do
                // e.g. "chmod(6777)=ok 6777 chown(-1,egid)=ok 0777"
                let words = row.[2].Split ' '
                let mode = Convert.ToInt32 (words.[0].Substring ("chmod(".Length, 4), 8)

                let group =
                    if words.[2].StartsWith "chown(-1,-1)" then
                        None
                    else
                        Some egid

                let system =
                    match
                        UnixPathResolution.chown (PathArg.ofText "f") None (Some egid) (boot caller |> dropped caller)
                    with
                    | Ok (SyscallAnswer.Completed _, system) -> system
                    | other -> failwith $"%s{context}: chown(f, -1, egid): %A{other}"

                let system =
                    match UnixPathResolution.chmod (PathArg.ofText "f") mode system with
                    | Ok (SyscallAnswer.Completed _, system) -> system
                    | other -> failwith $"%s{context}: chmod(f, %o{mode}): %A{other}"

                match UnixPathResolution.chown (PathArg.ofText "f") None group system with
                | Ok (answer, after) ->
                    let before = rendered (modeOf SymlinkPolicy.Follow "f" system)
                    let changed = rendered (modeOf SymlinkPolicy.Follow "f" after)
                    let chownLabel = words.[2].Substring (0, words.[2].IndexOf '=')

                    let actual = $"%s{words.[0]} %s{before} %s{chownLabel}=%s{errno answer} %s{changed}"

                    if actual <> row.[2] then
                        yield $"the probe answered %s{row.[2]}, this library %s{actual}"
                | Error refusal -> yield $"refused: %s{ChOwnRefusal.describe refusal}"
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
