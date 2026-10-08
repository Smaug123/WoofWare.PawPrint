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

/// Whether `readlink(2)` and `readlinkat(2)` consult the symbolic link's own
/// permission bits, held to `readlink-mode.c`'s rows end to end. Darwin answers
/// EACCES to a caller whose standing selects a triple without the read bit,
/// once the path is known to name a link and before a size of zero or the
/// buffer is looked at. Linux never looks, which links whose modes debugfs set
/// on an ext4 image showed, since no syscall gives a Linux link any mode but
/// 0777. A property holds the rule over every mode and generated credentials.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestReadLinkMode =

    let private context : string = "TestReadLinkMode"
    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L
    let private bits (raw : int) : PermissionBits = PermissionBits.parseOrFail context raw

    let private name (text : string) : DirectoryEntryName =
        DirectoryEntryName.parseOrFail context text

    let private uid (raw : uint32) : UserId = UserId.parseOrFail context raw
    let private gid (raw : uint32) : GroupId = GroupId.parseOrFail context raw
    let private linux : SimulatedUnixPlatform = SimulatedUnixPlatform.linuxX64
    let private darwin : SimulatedUnixPlatform = SimulatedUnixPlatform.macOsArm64
    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 2000

    let private rootOwner : InodeOwner =
        {
            User = uid 0u
            Group = gid 0u
        }

    // ------------------------------------------------------------ the probe's output

    let private darwinResource : string =
        "WoofWare.PosixKernel.Test.readlinkMode.darwin.txt"

    let private linuxResource : string =
        "WoofWare.PosixKernel.Test.readlinkMode.linux.txt"

    /// The probe's rows tagged `tag`, split into their tab-separated cells.
    let private probeRows (resource : string) (tag : string) : string[] list =
        use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream resource

        if isNull stream then
            failwith $"%s{context}: no embedded resource %s{resource}"

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun line -> line.Split '\t')
        |> Seq.filter (fun fields -> fields.[0] = tag)
        |> List.ofSeq

    /// The value of a row's `key=value` cell.
    let private cell (key : string) (row : string[]) : string =
        let prefix = key + "="

        match row |> Array.tryFind (fun c -> c.StartsWith (prefix, StringComparison.Ordinal)) with
        | Some c -> c.Substring prefix.Length
        | None -> failwith $"%s{context}: no %s{key}= cell in %A{row}"

    let private octal (text : string) : int = Convert.ToInt32 (text, 8)

    /// Who the Darwin probe ran as, read from its RUN row.
    let private darwinCaller : Credentials =
        match probeRows darwinResource "RUN" with
        | [ row ] ->
            let groups =
                (cell "groups" row).Split ','
                |> Seq.map (fun g -> gid (UInt32.Parse g))
                |> List.ofSeq

            Credentials.ofIds (uid (UInt32.Parse (cell "uid" row))) (gid (UInt32.Parse (cell "egid" row))) groups
        | rows -> failwith $"%s{context}: expected one RUN row, got %d{rows.Length}"

    /// The Linux probe's readers: root, and uid 1000 with gid 1000 and no
    /// supplementary groups, as it dropped to.
    let private linuxReader (raw : string) : Credentials =
        match raw with
        | "0" -> Owners.root
        | "1000" -> Credentials.ofIds (uid 1000u) (gid 1000u) []
        | other -> failwith $"%s{context}: the Linux probe had no reader %s{other}"

    // ------------------------------------------------------------ the probe's fixtures

    /// One entry of a hand-built directory.
    type private Entry =
        /// An empty 0644 file the directory's owner owns.
        | File of string
        /// A directory with its mode, owner and entries.
        | Directory of string * mode : int * owner : InodeOwner * Entry list
        /// A symbolic link with its target, mode and owner.
        | Link of string * target : string * mode : int * owner : InodeOwner

    let rec private populate
        (directory : InodeNumber)
        (owner : InodeOwner)
        (entries : Entry list)
        (vfs : VirtualFileSystem)
        : VirtualFileSystem
        =
        let created (what : string) (result : Result<InodeNumber * VirtualFileSystem, UnixError>) =
            match result with
            | Ok created -> created
            | Error error -> failwith $"%s{context}: could not create %s{what}: %O{error}"

        entries
        |> List.fold
            (fun vfs entry ->
                match entry with
                | Entry.File n ->
                    VirtualFileSystem.createFile directory (name n) (bits 0o644) owner epoch ImmutableArray.Empty vfs
                    |> created n
                    |> snd
                | Entry.Directory (n, mode, dirOwner, children) ->
                    let inode, vfs =
                        VirtualFileSystem.createDirectory directory (name n) (bits mode) dirOwner epoch vfs
                        |> created n

                    populate inode dirOwner children vfs
                | Entry.Link (n, target, mode, linkOwner) ->
                    VirtualFileSystem.createSymlink
                        directory
                        (name n)
                        (bits mode)
                        linkOwner
                        epoch
                        (SymlinkTarget.parseOrFail context target)
                        vfs
                    |> created n
                    |> snd
            )
            vfs

    /// A process on `platform` as `caller`, whose current directory is `/w`:
    /// a directory `cellOwner` owns with `cellMode`, holding `entries`. The
    /// root directory is root's, and 0755.
    let private boot
        (platform : SimulatedUnixPlatform)
        (caller : Credentials)
        (cellOwner : InodeOwner)
        (cellMode : int)
        (entries : Entry list)
        : UnixSystem<int, string>
        =
        let system : UnixSystem<int, string> =
            UnixSystem.initial platform
            |> Launched.bootWith (Launched.credentials caller) UnixSystem.pipedStandardStreams 0 (CpuId 0)

        let vfs = VirtualFileSystem.empty epoch rootOwner

        let cellInode, vfs =
            match
                VirtualFileSystem.createDirectory
                    (VirtualFileSystem.root vfs)
                    (name "w")
                    (bits cellMode)
                    cellOwner
                    epoch
                    vfs
            with
            | Ok created -> created
            | Error error -> failwith $"%s{context}: could not create the cell: %O{error}"

        let vfs = populate cellInode cellOwner entries vfs

        { system with
            Machine =
                { system.Machine with
                    FileSystem = vfs
                }
            Process =
                { system.Process with
                    CurrentDirectoryInode = cellInode
                }
        }
        |> Launched.restand

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

    let private opened (path : string) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match Answered.openPath readOnly (UnixPath.parseOrFail context path) 0 system with
        | SyscallAnswer.Completed fd, system -> int fd, system
        | other -> failwith $"%s{context}: open(%s{path}) did not open: %O{other}"

    // ------------------------------------------------------------ what readlink answered

    /// As the probe printed an answer: "ok:" and the bytes, or the errno.
    let private rendered (answer : Result<ReadLinkAnswer, ReadLinkRefusal>) : string =
        match answer with
        | Ok (ReadLinkAnswer.Reported bytes) -> "ok:" + Text.Encoding.ASCII.GetString (bytes.AsSpan ())
        | Ok (ReadLinkAnswer.Failed error) -> $"%A{error}"
        | Error refusal -> $"refused: %s{ReadLinkRefusal.describe refusal}"

    /// As the probe printed an answer about a link that is not the caller's:
    /// "ok(" and the length ")", or the errno.
    let private renderedLength (answer : Result<ReadLinkAnswer, ReadLinkRefusal>) : string =
        match answer with
        | Ok (ReadLinkAnswer.Reported bytes) -> $"ok(%d{bytes.Length})"
        | Ok (ReadLinkAnswer.Failed error) -> $"%A{error}"
        | Error refusal -> $"refused: %s{ReadLinkRefusal.describe refusal}"

    let private readlink
        (path : string)
        (buffer : UserBuffer)
        (capacity : int)
        (system : UnixSystem<int, string>)
        : Result<ReadLinkAnswer, ReadLinkRefusal>
        =
        UnixNamespace.readlink (PathArg.ofText path) buffer capacity system

    // ------------------------------------------------------------ Darwin: the owner

    /// The probe's link `l -> t`, owned by `owner` with `mode`, beside `t`.
    let private ownLink (owner : InodeOwner) (mode : int) : UnixSystem<int, string> =
        boot darwin darwinCaller owner 0o777 [ Entry.File "t" ; Entry.Link ("l", "t", mode, owner) ]

    [<Test>]
    let ``Darwin's owner reads its link exactly when the probe did, for every mode the probe gave it`` () : unit =
        let rows = probeRows darwinResource "OWNER"
        rows.Length |> shouldEqual 512
        let owner = InodeOwner.ofProcess darwinCaller

        [
            for row in rows do
                // The probe's buffer was 64 bytes.
                let actual =
                    readlink "l" UserBuffer.Mapped 64 (ownLink owner (octal (cell "lmode" row)))
                    |> rendered

                let expected = cell "readlink" row

                if actual <> expected then
                    yield $"""mode %s{cell "lmode" row}: the probe answered %s{expected}, this library %s{actual}"""
        ]
        |> shouldEqual []

    [<Test>]
    let ``Darwin's owner sweep: the owner's read bit alone decides, in the link's group and out of it`` () : unit =
        let rows = probeRows darwinResource "OWNERSWEEP"

        rows
        |> List.map (fun row ->
            row.[2], cell "group" row, cell "modes" row, cell "chmod-failures" row, cell "mismatches" row
        )
        |> shouldEqual [ "in-group", "20", "4096", "0", "0" ; "out-of-group", "0", "4096", "0", "0" ]

        // The prediction the probe found no mismatch against, over all 4096
        // modes in each of the two groups it swept.
        for row in rows do
            let owner =
                {
                    User = darwinCaller.EffectiveUser
                    Group = gid (UInt32.Parse (cell "group" row))
                }

            [
                for mode in 0..0o7777 do
                    let actual = readlink "l" UserBuffer.Mapped 64 (ownLink owner mode) |> rendered
                    let expected = if mode &&& 0o400 <> 0 then "ok:t" else "EACCES"

                    if actual <> expected then
                        yield $"%s{row.[2]} mode %o{mode}: the probe's rule says %s{expected}, this library %s{actual}"
            ]
            |> shouldEqual []

    [<Test>]
    let ``Darwin follows each change of the link's mode, both ways`` () : unit =
        let rows = probeRows darwinResource "CHMOD"
        rows.Length |> shouldEqual 9
        let owner = InodeOwner.ofProcess darwinCaller

        [
            for row in rows do
                // Every change the probe asked for took.
                if cell "fchmodat" row <> "ok" || cell "lmode" row <> cell "mode" row then
                    yield $"the probe's mode change did not take: %A{row}"

                let actual =
                    readlink "l" UserBuffer.Mapped 64 (ownLink owner (octal (cell "lmode" row)))
                    |> rendered

                let expected = cell "readlink" row

                if actual <> expected then
                    yield $"""mode %s{cell "lmode" row}: the probe answered %s{expected}, this library %s{actual}"""
        ]
        |> shouldEqual []

    // ------------------------------------------------------------ Darwin: the order

    /// The ORDER cell: `t`, `d/` holding `x -> ../t` (0755), and four links of
    /// mode 0: `l -> t`, `ld -> d`, `dang -> nx` and `cyc -> cyc`.
    let private orderCell () : UnixSystem<int, string> =
        let owner = InodeOwner.ofProcess darwinCaller

        boot
            darwin
            darwinCaller
            owner
            0o777
            [
                Entry.File "t"
                Entry.Directory ("d", 0o755, owner, [ Entry.Link ("x", "../t", 0o755, owner) ])
                Entry.Link ("l", "t", 0, owner)
                Entry.Link ("ld", "d", 0, owner)
                Entry.Link ("dang", "nx", 0, owner)
                Entry.Link ("cyc", "cyc", 0, owner)
            ]

    /// A `buf` argument as the probe wrote it.
    let private bufferArgument (text : string) : UserBuffer =
        match text with
        | "buf" -> UserBuffer.Mapped
        | "NULL" -> UserBuffer.Unmapped 0UL
        | "(char*)8" -> UserBuffer.Unmapped 8UL
        | other -> failwith $"%s{context}: the probe passed no buffer %s{other}"

    let private pathArgument (text : string) : string = if text = "\"\"" then "" else text

    /// The probe's ORDER rows this library cannot make: `open(O_SYMLINK)` is
    /// not modelled. (It was EACCES, so the probe had no descriptor to try
    /// `readlinkat(fd, "")` on.)
    let private orderNotReplayed : Set<string> = set [ "open(l, O_SYMLINK|O_RDONLY)" ]

    /// What this library answers to one ORDER row's call, as the probe printed it.
    let private orderCall (label : string) : string =
        let system = orderCell ()

        let arguments (call : string) : string list =
            label.Substring(call.Length + 1, label.Length - call.Length - 2).Split ", "
            |> List.ofArray

        if label.StartsWith ("readlinkat(", StringComparison.Ordinal) then
            match arguments "readlinkat" with
            | [ dirfd ; path ; buffer ; size ] ->
                let dirfd, system =
                    match dirfd with
                    | "AT_FDCWD" -> -2, system
                    | "cell" -> opened "." system
                    | "d" -> opened "d" system
                    | other -> failwith $"%s{context}: the probe passed no dirfd %s{other}"

                UnixNamespace.readlinkat
                    dirfd
                    (PathArg.ofText (pathArgument path))
                    (bufferArgument buffer)
                    (int size)
                    system
                |> rendered
            | other -> failwith $"%s{context}: cannot read %s{label}: %A{other}"
        elif label.StartsWith ("readlink(", StringComparison.Ordinal) then
            match arguments "readlink" with
            | [ path ; buffer ; size ] ->
                readlink (pathArgument path) (bufferArgument buffer) (int size) system
                |> rendered
            | other -> failwith $"%s{context}: cannot read %s{label}: %A{other}"
        else

        let status (policy : SymlinkPolicy) : string =
            match UnixPathResolution.stat policy (PathArg.ofText "l") system with
            | Ok (FileStatusAnswer.Reported _) -> "ok"
            | Ok (FileStatusAnswer.Failed error) -> $"%A{error}"
            | Error refusal -> $"refused: %A{refusal}"

        match label with
        | "lstat(l)" -> status SymlinkPolicy.NoFollowFinal
        | "stat(l)" -> status SymlinkPolicy.Follow
        | "open(l, O_RDONLY)" ->
            match Answered.openPath readOnly (UnixPath.parseOrFail context "l") 0 system with
            | SyscallAnswer.Completed _, _ -> "ok"
            | SyscallAnswer.Failed error, _ -> $"%A{error}"
        | other -> failwith $"%s{context}: the probe made no call %s{other}"

    [<Test>]
    let ``Darwin's refusal falls where the probe found it among readlink's other answers`` () : unit =
        let rows = probeRows darwinResource "ORDER"

        match rows with
        | fixture :: _ ->
            // The cell `orderCell` builds is the one the probe measured in.
            fixture.[2] |> shouldEqual "fixture"
            fixture.[3] |> shouldEqual "l=0000 ld=0000 dang=0000 cyc=0000 d/x=0755"
        | [] -> failwith $"%s{context}: no ORDER rows"

        let calls = rows |> List.tail |> List.map (fun row -> row.[2], row.[3])
        calls.Length |> shouldEqual 27

        calls
        |> List.map fst
        |> List.filter orderNotReplayed.Contains
        |> Set.ofList
        |> shouldEqual orderNotReplayed

        [
            for label, expected in calls do
                if not (orderNotReplayed.Contains label) then
                    let actual = orderCall label

                    if actual <> expected then
                        yield $"%s{label}: the probe answered %s{expected}, this library %s{actual}"
        ]
        |> shouldEqual []

    // ------------------------------------------------------------ Darwin: other users' links

    [<Test>]
    let ``Darwin reads another user's link by the triple the caller's standing selects`` () : unit =
        let rows = probeRows darwinResource "FOREIGN"
        rows.Length |> shouldEqual 4

        [
            for row in rows do
                let owner =
                    {
                        User = uid (UInt32.Parse (cell "uid" row))
                        Group = gid (UInt32.Parse (cell "gid" row))
                    }

                if Credentials.isInGroup darwinCaller owner.Group <> (cell "in-group" row = "1") then
                    yield $"the caller's groups are not the probe's: %A{row}"

                if cell "linkat" row <> "ok" || cell "hardlink" row <> "same-inode" then
                    yield $"the probe made no hard link to the very link: %A{row}"

                let mode = octal (cell "lmode" row)

                // A target as long as the one the probe read, wherever it read one.
                let target =
                    let direct = cell "direct" row

                    if direct.StartsWith ("ok(", StringComparison.Ordinal) then
                        String ('x', int (direct.Substring (3, direct.Length - 4)))
                    else
                        "x"

                let callerOwns = InodeOwner.ofProcess darwinCaller

                // Through the hard link, in a directory the caller owns.
                let viaHardLink =
                    boot darwin darwinCaller callerOwns 0o777 [ Entry.Link ("h", target, mode, owner) ]
                    |> readlink "h" UserBuffer.Mapped 4096
                    |> renderedLength

                // Where it is: in a directory root owns, which anyone may search.
                let direct =
                    boot
                        darwin
                        darwinCaller
                        callerOwns
                        0o777
                        [
                            Entry.Directory ("r", 0o755, rootOwner, [ Entry.Link ("l", target, mode, owner) ])
                        ]
                    |> readlink "r/l" UserBuffer.Mapped 4096
                    |> renderedLength

                if viaHardLink <> cell "via-hardlink" row || direct <> cell "direct" row then
                    yield
                        $"""%s{cell "path" row}: the probe answered %s{cell "direct" row}/%s{cell "via-hardlink" row}, this library %s{direct}/%s{viaHardLink}"""
        ]
        |> shouldEqual []

    // ------------------------------------------------------------ Linux

    [<Test>]
    let ``Linux reads a link symlink(2) made, whoever made it`` () : unit =
        let rows = probeRows linuxResource "PLAIN"
        rows.Length |> shouldEqual 3

        [
            for row in rows do
                let linkOwner =
                    match cell "link-owner" row with
                    | "0" -> rootOwner
                    | "1000" ->
                        {
                            User = uid 1000u
                            Group = gid 1000u
                        }
                    | other -> failwith $"%s{context}: the Linux probe had no link owner %s{other}"

                let system =
                    boot
                        linux
                        (linuxReader (cell "reader" row))
                        rootOwner
                        0o777
                        [ Entry.File "t" ; Entry.Link ("l", "t", octal (cell "lmode" row), linkOwner) ]

                let actual = readlink "l" UserBuffer.Mapped 64 system |> rendered

                if actual <> cell "readlink" row then
                    yield $"%A{row}: this library answered %s{actual}"
        ]
        |> shouldEqual []

    /// A Linux process as `reader` in the probe's crafted directory: root's
    /// and 0755, holding `t` and a link `l -> t` with `owner` and `mode`.
    let private craftedLink (reader : Credentials) (owner : InodeOwner) (mode : int) : UnixSystem<int, string> =
        boot linux reader rootOwner 0o755 [ Entry.File "t" ; Entry.Link ("l", "t", mode, owner) ]

    let private craftedOwner (row : string[]) : InodeOwner =
        {
            User = uid (UInt32.Parse (cell "uid" row))
            Group = gid (UInt32.Parse (cell "gid" row))
        }

    [<Test>]
    let ``Linux reads a link whatever mode it carries, for root and for each triple`` () : unit =
        // No Linux syscall makes these links (the flavour's invariant refuses
        // them), but a filesystem image can hold them, and readlink must answer
        // for one as Linux did.
        let rows = probeRows linuxResource "CRAFTEDROW"
        rows.Length |> shouldEqual (2 * 3 * 8)

        [
            for row in rows do
                let system =
                    craftedLink (linuxReader (cell "reader" row)) (craftedOwner row) (octal (cell "lmode" row))

                let actual = readlink "l" UserBuffer.Mapped 64 system |> rendered

                if actual <> cell "readlink" row then
                    yield $"%A{row}: this library answered %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``Linux's sweep of every mode for each reader and owner matches`` () : unit =
        let rows = probeRows linuxResource "CRAFTED"
        rows.Length |> shouldEqual 6

        [
            for row in rows do
                cell "setup-mismatch" row |> shouldEqual "0"
                let reader = linuxReader (cell "reader" row)
                let owner = craftedOwner row

                let answers =
                    [ 0..0o7777 ]
                    |> List.map (fun mode ->
                        readlink "l" UserBuffer.Mapped 64 (craftedLink reader owner mode) |> rendered
                    )

                let ok = answers |> List.filter ((=) "ok:t") |> List.length
                let denied = answers |> List.filter ((=) "EACCES") |> List.length
                let actual = $"ok=%d{ok} EACCES=%d{denied} other=%d{answers.Length - ok - denied}"

                let expected =
                    $"""ok=%s{cell "ok" row} EACCES=%s{cell "EACCES" row} other=%s{cell "other" row}"""

                if actual <> expected then
                    yield
                        $"""reader %s{cell "reader" row}, %s{cell "class" row}: the probe counted %s{expected}, this library %s{actual}"""
        ]
        |> shouldEqual []

    // ------------------------------------------------------------ the rule, generated

    type private Case =
        {
            Platform : SimulatedUnixPlatform
            User : uint32
            Group : uint32
            Supplementary : uint32 list
            LinkUser : uint32
            LinkGroup : uint32
            Mode : int
            Capacity : int
            Buffer : UserBuffer
            FromDescriptor : bool
        }

    let private caseGen : Gen<Case> =
        // Small pools, so that the caller owns the link, or is in its group by
        // its effective gid or a supplementary one, often.
        gen {
            let! platform = Gen.elements [ linux ; darwin ]
            let! user = Gen.elements [ 0u ; 501u ; 1000u ; 2000u ]
            let! group = Gen.elements [ 0u ; 20u ; 1000u ; 2000u ]
            let! supplementary = Gen.elements [ [] ; [ 20u ] ; [ 1000u ] ; [ 0u ; 2000u ] ; [ 20u ; 1000u ; 2000u ] ]
            let! linkUser = Gen.elements [ 0u ; 501u ; 1000u ; 2000u ]
            let! linkGroup = Gen.elements [ 0u ; 20u ; 1000u ; 2000u ]
            let! mode = Gen.choose (0, 0o7777)
            let! capacity = Gen.elements [ -1 ; 0 ; 1 ; 64 ]
            let! buffer = Gen.elements [ UserBuffer.Mapped ; UserBuffer.Unmapped 8UL ]
            let! fromDescriptor = Gen.elements [ false ; true ]

            return
                {
                    Platform = platform
                    User = user
                    Group = group
                    Supplementary = supplementary
                    LinkUser = linkUser
                    LinkGroup = linkGroup
                    Mode = mode
                    Capacity = capacity
                    Buffer = buffer
                    FromDescriptor = fromDescriptor
                }
        }

    /// What the probe's rule predicts, written from the rows rather than from
    /// the library's permission machinery.
    let private predicted (case : Case) : string =
        let target = "target"

        let delivered () : string =
            match case.Buffer with
            | UserBuffer.Mapped -> "ok:" + target.Substring (0, min case.Capacity target.Length)
            | _ -> "EFAULT"

        match SimulatedUnixPlatform.flavour case.Platform with
        | SimulatedUnixFlavour.Linux -> if case.Capacity <= 0 then "EINVAL" else delivered ()
        | SimulatedUnixFlavour.Darwin ->
            if case.Capacity < 0 then
                "EINVAL"
            elif case.User = 0u then
                "refused"
            else
                let readBit =
                    if case.LinkUser = case.User then
                        0o400
                    elif case.LinkGroup = case.Group || List.contains case.LinkGroup case.Supplementary then
                        0o040
                    else
                        0o004

                if case.Mode &&& readBit = 0 then "EACCES"
                elif case.Capacity = 0 then "ok:"
                else delivered ()

    let private answered (case : Case) : string =
        let caller =
            Credentials.ofIds (uid case.User) (gid case.Group) (case.Supplementary |> List.map gid)

        let linkOwner =
            {
                User = uid case.LinkUser
                Group = gid case.LinkGroup
            }

        let system =
            boot case.Platform caller rootOwner 0o777 [ Entry.Link ("l", "target", case.Mode, linkOwner) ]

        let answer =
            if case.FromDescriptor then
                let dirfd, system = opened "." system
                UnixNamespace.readlinkat dirfd (PathArg.ofText "l") case.Buffer case.Capacity system
            else
                readlink "l" case.Buffer case.Capacity system

        match answer with
        | Error (ReadLinkRefusal.UnmeasuredLinkRead (_, LinkReadRefusal.UnmeasuredPrivilegedCaller (standing, linkBits))) when
            standing = Standing.toward caller linkOwner && linkBits = bits case.Mode
            ->
            "refused"
        | answer -> rendered answer

    [<Test>]
    let ``readlink and readlinkat answer every mode and caller as the probe's rule predicts`` () : unit =
        let property (case : Case) : unit =
            (case, answered case) |> shouldEqual (case, predicted case)

        Check.One (config, Prop.forAll (Arb.fromGen caseGen) property)

    [<Test>]
    let ``linkReadDenied is deniedTo's read question on Darwin, and nothing on Linux, for every standing and mode``
        ()
        : unit
        =
        for privilege in [ CallerPrivilege.Privileged ; CallerPrivilege.Unprivileged ] do
            for owns in [ false ; true ] do
                for inGroup in [ false ; true ] do
                    let standing =
                        {
                            Privilege = privilege
                            Owns = owns
                            InGroup = inGroup
                        }

                    for mode in 0..0o7777 do
                        PermissionBits.linkReadDenied (SimulatedUnixPlatform.linkReadRule linux) standing (bits mode)
                        |> shouldEqual (Ok false)

                        let expected =
                            match privilege with
                            | CallerPrivilege.Privileged ->
                                Error (LinkReadRefusal.UnmeasuredPrivilegedCaller (standing, bits mode))
                            | CallerPrivilege.Unprivileged ->
                                Ok (PermissionBits.deniedTo standing AccessRequest.Read (bits mode))

                        PermissionBits.linkReadDenied (SimulatedUnixPlatform.linkReadRule darwin) standing (bits mode)
                        |> shouldEqual expected
