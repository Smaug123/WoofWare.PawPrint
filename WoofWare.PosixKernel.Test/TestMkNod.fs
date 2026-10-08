namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `mknod(2)` and `mknodat(2)` held to `mknodat-rules.c`, which measured what
/// `at-dirfd.c` left open: whether making a regular file with `mknod` decides
/// anything differently from `open(O_CREAT|O_EXCL)` (PATH, MODE, TIMES), what
/// each value of the type field makes or answers (TYPE, PATHTYPE, WIDE), and
/// where the type is screened against the path's own failures (ORDER); and to
/// `link-symlink.c`'s MKNOD rows. Every row is replayed in a cell made as the
/// probe made it. Where the probe made something other than a regular file,
/// this library must refuse, naming what it would have made; everywhere else it
/// must answer exactly what the probe answered, and make exactly what it made.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMkNod =

    let private context : string = "TestMkNod"
    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L

    let private name (text : string) : DirectoryEntryName =
        DirectoryEntryName.parseOrFail context text

    let private perms (bits : int) : PermissionBits = PermissionBits.parseOrFail context bits
    let private gid (n : uint32) : GroupId = GroupId.parseOrFail context n

    // ------------------------------------------------------------ the probe's envelopes

    type private Envelope =
        {
            Label : string
            Platform : SimulatedUnixPlatform
            Credentials : Credentials
            Resource : string
            /// The probe's `caller=` field for this caller.
            Caller : string
        }

    let private linuxResource : string =
        "WoofWare.PosixKernel.Test.mknodatRules.linux.txt"

    let private linuxRoot : Envelope =
        {
            Label = "Linux root"
            Platform = SimulatedUnixPlatform.linuxX64
            Credentials = Owners.root
            Resource = linuxResource
            Caller = "caller=0"
        }

    let private linuxUser : Envelope =
        {
            Label = "Linux uid 1000"
            Platform = SimulatedUnixPlatform.linuxX64
            Credentials = Credentials.ofIds (UserId.parseOrFail context 1000u) (gid 1000u) []
            Resource = linuxResource
            Caller = "caller=1000"
        }

    let private darwinUser : Envelope =
        {
            Label = "Darwin uid 501"
            Platform = SimulatedUnixPlatform.macOsArm64
            Credentials = Credentials.ofIds (UserId.parseOrFail context 501u) (gid 20u) []
            Resource = "WoofWare.PosixKernel.Test.mknodatRules.darwin.txt"
            Caller = "caller=501"
        }

    let private envelopes : Envelope list = [ linuxRoot ; linuxUser ; darwinUser ]

    let private flavourOf (envelope : Envelope) : SimulatedUnixFlavour =
        SimulatedUnixPlatform.flavour envelope.Platform

    let private readResource (resource : string) : string =
        use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream resource

        if isNull stream then
            failwith $"%s{context}: no embedded resource %s{resource}"

        use reader = new StreamReader (stream)
        reader.ReadToEnd ()

    /// The fields after the filesystem of every line the probe printed under
    /// `tag` for `envelope`'s caller, on any filesystem: the Linux probe ran
    /// on ext4 and on tmpfs, and the two agree in every row.
    let private rowsOf (envelope : Envelope) (tag : string) : string list list =
        readResource envelope.Resource
        |> fun text -> text.Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun line -> line.Split '\t' |> List.ofArray)
        |> Seq.filter (fun fields -> List.head fields = tag && fields.[2] = envelope.Caller)
        |> Seq.map (List.skip 3)
        |> List.ofSeq

    let private field (prefix : string) (cell : string) : string =
        if cell.StartsWith prefix then
            cell.Substring prefix.Length
        else
            failwith $"%s{context}: expected a %s{prefix} field, got %s{cell}"

    /// A `name=answer` cell, split at its first `=`.
    let private labelled (cell : string) : string * string =
        let at = cell.IndexOf '='
        cell.Substring (0, at), cell.Substring (at + 1)

    // ------------------------------------------------------------ the probe's cell

    /// `mknodat-rules.c`'s cell under `/c`, with the current directory at
    /// `cwd` and the process's umask `umask`: `w/` holding `d/`, which holds a
    /// file `f`, a directory `sub/`, the links `dang -> nx2`, `cyc -> cyc`,
    /// `lf -> f` and `ld -> sub`, an unwritable `ro/` holding `e/`, and a
    /// set-group-ID `sg/`, all the caller's; and beside `w/`, a set-group-ID
    /// `xg/` made before the caller dropped. As Linux root, `sg/` and `xg/`
    /// are of group 4321, which is not the caller's, and `xg/` is that group's
    /// for uid 1000 too, and root's. On Darwin every entry has the group of the
    /// probe's scratch directory, wheel, which its caller is not in.
    let private cell (envelope : Envelope) (cwd : string) (umask : int) : UnixSystem<int, string> =
        let caller = InodeOwner.ofProcess envelope.Credentials

        let foreign =
            { InodeOwner.ofProcess Owners.root with
                Group = gid 4321u
            }

        let owner, setGroupIdOwner, crossGroupOwner, cellOwner =
            match flavourOf envelope with
            | SimulatedUnixFlavour.Linux ->
                if envelope = linuxRoot then
                    caller, Some foreign, Some foreign, None
                else
                    caller, None, Some foreign, Some (InodeOwner.ofProcess Owners.root)
            | SimulatedUnixFlavour.Darwin ->
                let wheel =
                    { caller with
                        Group = gid 0u
                    }

                wheel, None, None, None

        let file = SeedEntry.File (ImmutableArray<byte>.Empty, perms 0o644, None)

        let link (target : string) =
            SeedEntry.Symlink (SymlinkTarget.parseOrFail context target, None)

        let dir (bits : int) (owner : InodeOwner option) (entries : (string * SeedEntry) list) =
            SeedEntry.Directory (entries |> List.map (fun (n, e) -> name n, e) |> Map.ofList, perms bits, owner)

        let seed =
            Map.ofList
                [
                    name "c",
                    dir
                        0o777
                        cellOwner
                        [
                            "w",
                            dir
                                0o755
                                None
                                [
                                    "d",
                                    dir
                                        0o755
                                        None
                                        [
                                            "f", file
                                            "sub", dir 0o755 None []
                                            "dang", link "nx2"
                                            "cyc", link "cyc"
                                            "lf", link "f"
                                            "ld", link "sub"
                                            "ro", dir 0o555 None [ "e", dir 0o755 None [] ]
                                            "sg", dir 0o2775 setGroupIdOwner []
                                        ]
                                ]
                            "xg", dir 0o2777 crossGroupOwner []
                        ]
                ]

        let image : UnixBootImage<int, string> = UnixSystem.initial envelope.Platform

        match UnixBootImage.withFileSystem epoch owner seed image with
        | Ok image ->
            Launched.bootWith
                (Launched.credentials envelope.Credentials
                 >> Launched.umask (perms umask)
                 >> ProcessLaunch.withCurrentDirectory (AbsoluteUnixPath.parseOrFail context cwd))
                UnixSystem.pipedStandardStreams
                0
                (CpuId 0)
                image
        | Error fault -> failwith $"%s{context}: could not build mknodat-rules.c's cell: %A{fault}"

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

    /// `open(path, O_WRONLY|O_CREAT|O_EXCL, mode)`, in the flavour's numbering.
    let private openExclusive
        (path : PathArgumentBytes)
        (mode : int)
        (system : UnixSystem<int, string>)
        : Result<SyscallAnswer * UnixSystem<int, string>, OpenRefusal>
        =
        let flags =
            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
            | SimulatedUnixFlavour.Linux -> 0x1 ||| 0x40 ||| 0x80
            | SimulatedUnixFlavour.Darwin -> 0x1 ||| 0x200 ||| 0x800

        UnixNamespace.openPath flags path mode system

    // ------------------------------------------------------------ rendering

    /// Bytes as the probe prints a name: printable ASCII as itself, any other
    /// byte as `\xNN`.
    let private escapedBytes (bytes : byte seq) : string =
        bytes
        |> Seq.map (fun b ->
            if b < 0x20uy || b >= 0x7fuy then
                $"\\x%02x{b}"
            else
                string (char b)
        )
        |> String.concat ""

    /// Every path under the cell `/c`, relative to it, with the inode each
    /// names, never following a link.
    let private cellPaths (system : UnixSystem<int, string>) : Map<string, InodeNumber> =
        let vfs = system.Machine.FileSystem

        let rec under (prefix : string) (inode : InodeNumber) : (string * InodeNumber) seq =
            match VirtualFileSystem.tryGetDirectory inode vfs with
            | None -> Seq.empty
            | Some content ->
                content.Entries
                |> Map.toSeq
                |> Seq.collect (fun (entry, child) ->
                    let path =
                        prefix
                        + escapedBytes (DirectoryEntryName.toByteString entry |> UnixByteString.toBytes)

                    Seq.append (Seq.singleton (path, child)) (under (path + "/") child)
                )

        let root =
            match
                PathWalk.resolveExisting
                    (SimulatedUnixPlatform.pathLimits system.Machine.UnixPlatform)
                    Owners.root
                    SymlinkProtection.Off
                    (VirtualFileSystem.root vfs)
                    SymlinkPolicy.NoFollowFinal
                    (UnixPath.parseOrFail context "/c")
                    vfs
            with
            | Ok inode -> inode
            | Error failure -> failwith $"%s{context}: /c does not resolve: %A{failure}"

        under "" root |> Map.ofSeq

    /// The directory holding `inode` by `path`, a path under the cell.
    let private holder (path : string) (system : UnixSystem<int, string>) : Inode =
        let paths = cellPaths system

        let parent =
            match path.LastIndexOf '/' with
            | -1 -> "/c"
            | at -> "/c/" + path.Substring (0, at)

        let inode =
            if parent = "/c" then
                match
                    PathWalk.resolveExisting
                        (SimulatedUnixPlatform.pathLimits system.Machine.UnixPlatform)
                        Owners.root
                        SymlinkProtection.Off
                        (VirtualFileSystem.root system.Machine.FileSystem)
                        SymlinkPolicy.NoFollowFinal
                        (UnixPath.parseOrFail context "/c")
                        system.Machine.FileSystem
                with
                | Ok inode -> inode
                | Error failure -> failwith $"%s{context}: /c does not resolve: %A{failure}"
            else
                paths.[parent.Substring "/c/".Length]

        match VirtualFileSystem.tryGet inode system.Machine.FileSystem with
        | Some inode -> inode
        | None -> failwith $"%s{context}: %s{parent} names no inode"

    /// What the probe prints for a call made in `before`: the errno, or "ok"
    /// with what was made, its permission bits, whose group it took and where
    /// under the cell it is; or, for a refusal, which node this library would
    /// not make.
    let private rendered<'Refusal>
        (refusal : 'Refusal -> string)
        (before : UnixSystem<int, string>)
        (result : Result<SyscallAnswer * UnixSystem<int, string>, 'Refusal>)
        : string
        =
        match result with
        | Error r -> refusal r
        | Ok (SyscallAnswer.Failed error, _) -> $"%A{error}"
        | Ok (SyscallAnswer.Completed _, after) ->

        let existed = cellPaths before
        let now = cellPaths after

        let made =
            now |> Map.filter (fun path _ -> not (existed.ContainsKey path)) |> Map.toList

        match made with
        | [ path, inode ] ->
            let entry =
                match VirtualFileSystem.tryGet inode after.Machine.FileSystem with
                | Some entry -> entry
                | None -> failwith $"%s{context}: %s{path} names no inode"

            let kind =
                match entry.Content with
                | InodeContent.RegularFile _ -> "reg"
                | InodeContent.Directory _ -> "dir"
                | InodeContent.Symlink _ -> "lnk"
                | InodeContent.CharacterDevice _ -> "chr"

            let parent = holder path after
            let egid = after.Process.Credentials.EffectiveGroup

            let group =
                if entry.Owner.Group = parent.Owner.Group then
                    if entry.Owner.Group = egid then "both" else "parent"
                elif entry.Owner.Group = egid then
                    "egid"
                else
                    "other"

            let mode = PermissionBits.toInt (Inode.permissions entry)
            $"ok:%s{kind}:mode=%04o{mode}:gid=%s{group}:new=%s{path}"
        | other -> failwith $"%s{context}: the call succeeded and made %A{other}"

    let private mknodRefusal (refusal : MkNodRefusal) : string =
        match refusal with
        | MkNodRefusal.Fifo -> "refused:fifo"
        | MkNodRefusal.Socket -> "refused:sock"
        | MkNodRefusal.CharacterDevice dev -> $"refused:chr:0x%x{dev}"
        | MkNodRefusal.BlockDevice dev -> $"refused:blk:0x%x{dev}"
        | MkNodRefusal.UnmeasuredPrivilegedCaller
        | MkNodRefusal.Path _ -> $"refused: %s{MkNodRefusal.describe refusal}"

    let private openRefusal (refusal : OpenRefusal) : string =
        $"refused: %s{OpenRefusal.describe refusal}"

    /// `makedev(1, 3)` as each flavour's kernel takes it.
    let private deviceOneThree (envelope : Envelope) : uint32 =
        match flavourOf envelope with
        | SimulatedUnixFlavour.Linux -> 0x103u
        | SimulatedUnixFlavour.Darwin -> 0x1000003u

    /// What this library must answer where the probe answered `probe` to a
    /// `mknod` of `mode` and `dev`: the same, except that wherever the probe
    /// made something other than a regular file this library refuses, naming
    /// it, and that Darwin's FIFO, which is `mkfifo(2)`, is refused whatever
    /// the probe answered.
    let private expected (envelope : Envelope) (mode : int) (dev : uint32) (probe : string) : string =
        match flavourOf envelope, NodeTypeField.ofMode mode with
        | SimulatedUnixFlavour.Darwin, NodeTypeField.Fifo -> "refused:fifo"
        | _ ->
            if probe.StartsWith "ok:fifo" then "refused:fifo"
            elif probe.StartsWith "ok:sock" then "refused:sock"
            elif probe.StartsWith "ok:chr" then $"refused:chr:0x%x{dev}"
            elif probe.StartsWith "ok:blk" then $"refused:blk:0x%x{dev}"
            else probe

    // ------------------------------------------------------------ the rows

    let private pathOf (label : string) : PathArgumentBytes =
        match label with
        | "xff3" -> PathArg.ofBytes [ 0xffuy ; 0xffuy ; 0xffuy ]
        | "ro/xff3" -> PathArg.ofBytes [ byte 'r' ; byte 'o' ; byte '/' ; 0xffuy ; 0xffuy ; 0xffuy ]
        | "xg/x" -> PathArg.ofText "../../xg/x"
        | text -> PathArg.ofText text

    let private octal (text : string) : int = Convert.ToInt32 (text, 8)

    /// One PATH or MODE row: `mknodat` from a descriptor on `d` with the cwd
    /// at `w`, `mknod` and `open(O_CREAT|O_EXCL)` with the cwd at `d`.
    let private replayTriple (envelope : Envelope) (row : string list) : string list =
        match row with
        | [ label ; mode ; umask ; at ; plain ; opened' ] ->
            let mode = octal (field "mode=" mode)
            let umask = octal (field "umask=" umask)
            let at = field "at=" at
            let plain = field "plain=" plain
            let openAnswer = field "open=" opened'
            let path = pathOf label
            let row = $"%s{envelope.Label} %s{label} mode=0%o{mode} umask=%03o{umask}"

            [
                let fd, system = opened "d" (cell envelope "/c/w" umask)

                let actual =
                    UnixNamespace.mknodat fd path mode 0u system |> rendered mknodRefusal system

                if actual <> expected envelope mode 0u at then
                    yield $"%s{row}: the probe's mknodat answered %s{at}, this library %s{actual}"

                let system = cell envelope "/c/w/d" umask

                let actual = UnixNamespace.mknod path mode 0u system |> rendered mknodRefusal system

                if actual <> expected envelope mode 0u plain then
                    yield $"%s{row}: the probe's mknod answered %s{plain}, this library %s{actual}"

                let actual =
                    openExclusive path (mode &&& 0o7777) system |> rendered openRefusal system

                if actual <> openAnswer then
                    yield $"%s{row}: the probe's open answered %s{openAnswer}, this library %s{actual}"
            ]
        | other -> failwith $"%s{context}: a malformed row %A{other}"

    let private replayPath (envelope : Envelope) : unit =
        let rows = rowsOf envelope "PATH"
        // 27 pathnames, on each filesystem the probe ran on.
        rows.Length % 27 |> shouldEqual 0
        rows.Length |> shouldBeGreaterThan 0
        rows |> List.collect (replayTriple envelope) |> shouldEqual []

    [<Test>]
    let ``a regular file's pathname rows answer as measured as Linux root`` () : unit = replayPath linuxRoot

    [<Test>]
    let ``a regular file's pathname rows answer as measured as a Linux user`` () : unit = replayPath linuxUser

    [<Test>]
    let ``a regular file's pathname rows answer as measured on Darwin`` () : unit = replayPath darwinUser

    let private replayMode (envelope : Envelope) : unit =
        let rows = rowsOf envelope "MODE"
        // Two type fields, four umasks, nine permission words, three places.
        rows.Length % (2 * 4 * 9 * 3) |> shouldEqual 0
        rows.Length |> shouldBeGreaterThan 0
        rows |> List.collect (replayTriple envelope) |> shouldEqual []

    [<Test>]
    let ``a regular file's mode and group are open's, as Linux root measured`` () : unit = replayMode linuxRoot

    [<Test>]
    let ``a regular file's mode and group are open's, as a Linux user measured`` () : unit = replayMode linuxUser

    [<Test>]
    let ``mknod of a regular file is refused on Darwin before its mode is read`` () : unit = replayMode darwinUser

    [<Test>]
    let ``on Linux mknod and mknodat create every regular file open would, and diverge only at a trailing separator``
        ()
        : unit
        =
        // The probe's own finding, which the replay above holds this library
        // to: mknod's walk is mkdir's and symlink's, not open's, and nothing
        // else about the two differs.
        for envelope in [ linuxRoot ; linuxUser ] do
            let differing =
                rowsOf envelope "PATH" @ rowsOf envelope "MODE"
                |> List.choose (fun row ->
                    match row with
                    | [ label ; _ ; _ ; at ; plain ; opened' ] ->
                        let at = field "at=" at
                        let plain = field "plain=" plain
                        let openAnswer = field "open=" opened'

                        if at <> plain then
                            failwith $"%s{context}: %s{label}: mknodat %s{at} against mknod %s{plain}"

                        if at = openAnswer then
                            None
                        else
                            Some (label, at, openAnswer)
                    | other -> failwith $"%s{context}: a malformed row %A{other}"
                )
                |> List.distinct

            differing
            |> shouldEqual
                [
                    "nx/", "ENOENT", "EISDIR"
                    "nx//", "ENOENT", "EISDIR"
                    "f/", "EEXIST", "EISDIR"
                    "dang/", "EEXIST", "EISDIR"
                    "cyc/", "EEXIST", "EISDIR"
                    "lf/", "EEXIST", "EISDIR"
                    "ld/", "EEXIST", "EISDIR"
                ]

    let private replayPathType (envelope : Envelope) : unit =
        let rows = rowsOf envelope "PATHTYPE"
        rows.Length |> shouldBeGreaterThan 0

        [
            for row in rows do
                match row with
                | label :: cells ->
                    for cellText in cells do
                        let kind, probe = labelled cellText

                        let mode, dev =
                            match kind with
                            | "fifo" -> 0o010644, 0u
                            | "sock" -> 0o140644, 0u
                            | "chr:1,3" -> 0o020644, deviceOneThree envelope
                            | "blk:1,3" -> 0o060644, deviceOneThree envelope
                            | "chr:0,0" -> 0o020644, 0u
                            | other -> failwith $"%s{context}: the probe has no node kind %s{other}"

                        let system = cell envelope "/c/w/d" 0o022

                        let actual =
                            UnixNamespace.mknod (pathOf label) mode dev system
                            |> rendered mknodRefusal system

                        let want = expected envelope mode dev probe

                        if actual <> want then
                            yield
                                $"%s{envelope.Label} %s{label} %s{kind}: the probe answered %s{probe}, so this library owes %s{want}, and answered %s{actual}"
                | [] -> failwith $"%s{context}: an empty PATHTYPE row"
        ]
        |> shouldEqual []

    [<Test>]
    let ``every other type answers what a regular file answers short of being made, under every envelope`` () : unit =
        for envelope in envelopes do
            replayPathType envelope

    let private replayType (envelope : Envelope) : unit =
        let rows = rowsOf envelope "TYPE"
        rows.Length % 32 |> shouldEqual 0
        rows.Length |> shouldBeGreaterThan 0

        [
            for row in rows do
                match row with
                | typeField :: devField :: cells ->
                    let typeBits = octal (field "type=" typeField)

                    let dev =
                        match field "dev=" devField with
                        | "0" -> 0u
                        | "1,3" -> deviceOneThree envelope
                        | other -> failwith $"%s{context}: the probe has no dev %s{other}"

                    let mode = typeBits ||| 0o644

                    for cellText in cells do
                        let target, probe = labelled cellText

                        let actual =
                            match target with
                            | "orphan:nx" ->
                                let system = cell envelope "/c/w/d" 0o022

                                let system =
                                    Answered.mkdir (PathArg.ofText "gone") 0o755 system |> completed "mkdir(gone)"

                                let fd, system = opened "gone" system

                                let system =
                                    Answered.rmdir (UnixPath.parseOrFail context "gone") system
                                    |> completed "rmdir(gone)"

                                UnixNamespace.mknodat fd (PathArg.ofText "nx") mode dev system
                                |> rendered mknodRefusal system
                            | target ->
                                let system = cell envelope "/c/w/d" 0o022

                                UnixNamespace.mknod (pathOf target) mode dev system
                                |> rendered mknodRefusal system

                        let want = expected envelope mode dev probe

                        if actual <> want then
                            yield
                                $"%s{envelope.Label} type=0%06o{typeBits} dev=0x%x{dev} %s{target}: the probe answered %s{probe}, so this library owes %s{want}, and answered %s{actual}"
                | other -> failwith $"%s{context}: a malformed row %A{other}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``every value of the type field answers as measured as Linux root`` () : unit = replayType linuxRoot

    [<Test>]
    let ``every value of the type field answers as measured as a Linux user`` () : unit = replayType linuxUser

    [<Test>]
    let ``every value of the type field answers as measured on Darwin`` () : unit = replayType darwinUser

    [<Test>]
    let ``bits above the type field change nothing, under every envelope`` () : unit =
        [
            for envelope in envelopes do
                for row in rowsOf envelope "WIDE" do
                    match row with
                    | [ modeField ; cellText ] ->
                        let mode = Convert.ToUInt32 (field "mode=0x" modeField, 16) |> int

                        let _, probe = labelled cellText
                        let system = cell envelope "/c/w/d" 0o022

                        let actual =
                            UnixNamespace.mknod (PathArg.ofText "nx") mode 0u system
                            |> rendered mknodRefusal system

                        let want = expected envelope mode 0u probe

                        if actual <> want then
                            yield
                                $"%s{envelope.Label} 0x%08x{mode}: the probe answered %s{probe}, so this library owes %s{want}, and answered %s{actual}"
                    | other -> failwith $"%s{context}: a malformed row %A{other}"
        ]
        |> shouldEqual []

    let private replayOrder (envelope : Envelope) : unit =
        let rows = rowsOf envelope "ORDER"
        rows.Length % 24 |> shouldEqual 0
        rows.Length |> shouldBeGreaterThan 0

        let overlong =
            PathArg.ofText (
                String.init
                    (PathLimits.pathMaxBytes (SimulatedUnixPlatform.pathLimits envelope.Platform))
                    (fun i -> if i % 2 = 0 then "a" else "/")
            )

        [
            for row in rows do
                match row with
                | typeField :: dirfdField :: cells ->
                    let mode = octal (field "type=" typeField) ||| 0o644
                    let dev = deviceOneThree envelope

                    for cellText in cells do
                        let label, probe = labelled cellText
                        let system = cell envelope "/c/w" 0o022

                        let dirfd, system =
                            match field "dirfd=" dirfdField with
                            | "dir" -> opened "d" system
                            | "file" -> opened "d/f" system
                            | "minus1" -> -1, system
                            | other -> failwith $"%s{context}: the probe has no dirfd %s{other}"

                        let path =
                            match label with
                            | "NULL"
                            | "PROT_NONE" -> PathArgumentBytes.Unreadable
                            | "overlong" -> overlong
                            | "empty" -> PathArg.ofText ""
                            | text -> PathArg.ofText text

                        let actual =
                            UnixNamespace.mknodat dirfd path mode dev system |> rendered mknodRefusal system

                        let want = expected envelope mode dev probe

                        if actual <> want then
                            yield
                                $"%s{envelope.Label} %s{typeField} %s{dirfdField} %s{label}: the probe answered %s{probe}, so this library owes %s{want}, and answered %s{actual}"
                | other -> failwith $"%s{context}: a malformed row %A{other}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``the type is screened where measured, against the path and the dirfd, as Linux root`` () : unit =
        replayOrder linuxRoot

    [<Test>]
    let ``the type is screened where measured, against the path and the dirfd, as a Linux user`` () : unit =
        replayOrder linuxUser

    [<Test>]
    let ``the type is screened where measured, against the path and the dirfd, on Darwin`` () : unit =
        replayOrder darwinUser

    [<Test>]
    let ``a regular file's times move as open's do, as Linux measured`` () : unit =
        [
            for envelope in [ linuxRoot ; linuxUser ] do
                for row in rowsOf envelope "TIMES" do
                    match row with
                    | [ cellText ] ->
                        let call, probe = labelled cellText
                        let before = cell envelope "/c/w/d" 0o022

                        // The probe slept before it called.
                        let system =
                            { before with
                                Machine = UnixMachineState.advanceClock 50_000_000L before.Machine
                            }

                        let result =
                            match call with
                            | "mknod" ->
                                UnixNamespace.mknod (PathArg.ofText "nx") 0o100644 0u system
                                |> Result.mapError MkNodRefusal.describe
                            | "open" ->
                                openExclusive (PathArg.ofText "nx") 0o644 system
                                |> Result.mapError OpenRefusal.describe
                            | other -> failwith $"%s{context}: the probe made no %s{other} call"

                        let actual =
                            match result with
                            | Error refusal -> $"refused: %s{refusal}"
                            | Ok (SyscallAnswer.Failed error, _) -> $"%A{error}"
                            | Ok (SyscallAnswer.Completed _, after) ->
                                let paths = cellPaths after
                                let vfs = after.Machine.FileSystem
                                let dirBefore = (holder "w/d/nx" before).Times
                                let dirAfter = (holder "w/d/nx" after).Times
                                let made = paths.["w/d/nx"]

                                let madeInode =
                                    match VirtualFileSystem.tryGet made vfs with
                                    | Some inode -> inode
                                    | None -> failwith $"%s{context}: w/d/nx names no inode"

                                let moved (a : UnixTimestamp) (b : UnixTimestamp) = if a <> b then "moved" else "kept"

                                let yesNo (b : bool) = if b then "yes" else "no"
                                let t = madeInode.Times

                                let size =
                                    match madeInode.Content with
                                    | InodeContent.RegularFile (contents, _) -> contents.Length
                                    | other -> failwith $"%s{context}: made %A{other}"

                                String.concat
                                    ":"
                                    [
                                        "ok"
                                        $"dir-mtime=%s{moved dirBefore.Modification dirAfter.Modification}"
                                        $"dir-ctime=%s{moved dirBefore.StatusChange dirAfter.StatusChange}"
                                        $"new-a=m=c=%s{yesNo (t.Access = t.Modification && t.Modification = t.StatusChange)}"
                                        $"new-m=dir-m=%s{yesNo (t.Modification = dirAfter.Modification)}"
                                        $"size=%d{size}"
                                        $"nlink=%d{VirtualFileSystem.bindingCount made vfs}"
                                    ]

                        if actual <> probe then
                            yield $"%s{envelope.Label} %s{call}: the probe answered %s{probe}, this library %s{actual}"
                    | other -> failwith $"%s{context}: a malformed row %A{other}"
        ]
        |> shouldEqual []

    // ------------------------------------------------------------ link-symlink.c's MKNOD rows

    /// `link-symlink.c`'s MKNOD rows: `mknod("n", mode, makedev(1, 3))` into a
    /// free name of a cell holding a file `f`, and the same onto `f`, under
    /// umask 022. Its `mkfifo` row is not `mknod`'s.
    [<Test>]
    let ``link-symlink.c's mknod rows answer as measured, under every envelope`` () : unit =
        let resources =
            [
                linuxRoot, "WoofWare.PosixKernel.Test.linkSymlink.linuxRoot.txt"
                linuxUser, "WoofWare.PosixKernel.Test.linkSymlink.linuxUser.txt"
                darwinUser, "WoofWare.PosixKernel.Test.linkSymlink.darwin.txt"
            ]

        let modeOf (label : string) : int =
            match label with
            | "S_IFREG" -> 0o100644
            | "type0" -> 0o644
            | "S_IFIFO" -> 0o010644
            | "S_IFCHR" -> 0o020644
            | "S_IFBLK" -> 0o060644
            | "S_IFDIR" -> 0o040644
            | "S_IFLNK" -> 0o120644
            | "S_IFSOCK" -> 0o140644
            | "S_IFMT" -> 0o170644
            | other -> failwith $"%s{context}: link-symlink.c has no MKNOD row %s{other}"

        // "ok(type=0100000 perm=0644 rdev=0)" as this fixture's renderer
        // prints a regular file made in the cell's own directory.
        let canonical (probe : string) : string =
            match probe with
            | "ok(type=0100000 perm=0644 rdev=0)" -> "ok:reg:mode=0644"
            | "ok(type=010000 perm=0644 rdev=0)" -> "ok:fifo"
            | "ok(type=0140000 perm=0644 rdev=0)" -> "ok:sock"
            | "ok(type=020000 perm=0644 rdev=259)" -> "ok:chr"
            | "ok(type=060000 perm=0644 rdev=259)" -> "ok:blk"
            | other -> other

        let stripped (actual : string) : string =
            // Only the kind and the mode: the cell here is not the probe's.
            if actual.StartsWith "ok:" then
                actual.Split(':').[0..2] |> String.concat ":"
            else
                actual

        [
            for envelope, resource in resources do
                let rows =
                    (readResource resource).Split ('\n', StringSplitOptions.RemoveEmptyEntries)
                    |> Seq.map (fun line -> line.Split '\t' |> List.ofArray)
                    |> Seq.filter (fun fields -> List.head fields = "MKNOD" && fields.[1] <> "mkfifo")
                    |> List.ofSeq

                rows.Length |> shouldEqual 9

                for row in rows do
                    match row with
                    | [ _ ; label ; answers ] ->
                        let mode = modeOf label
                        let dev = deviceOneThree envelope

                        let fresh, ontoExisting =
                            match answers.Split " onto-existing=" with
                            | [| fresh ; ontoExisting |] -> fresh, ontoExisting
                            | _ -> failwith $"%s{context}: a malformed MKNOD answer %s{answers}"

                        for path, probe in [ "nx", fresh ; "f", ontoExisting ] do
                            let system = cell envelope "/c/w/d" 0o022

                            let actual =
                                UnixNamespace.mknod (PathArg.ofText path) mode dev system
                                |> rendered mknodRefusal system
                                |> stripped

                            let want = expected envelope mode dev (canonical probe)

                            if actual <> want then
                                yield
                                    $"%s{envelope.Label} %s{label} onto %s{path}: the probe answered %s{probe}, so this library owes %s{want}, and answered %s{actual}"
                    | other -> failwith $"%s{context}: a malformed row %A{other}"
        ]
        |> shouldEqual []

    // ------------------------------------------------------------ the screen

    [<Test>]
    let ``Linux reads the type field and nothing above it`` () : unit =
        for high in [ 0 ; 0x10000 ; 0x7fff0000 ; Int32.MinValue ] do
            for kind in 0..15 do
                let mode = high ||| (kind <<< 12) ||| 0o644

                NodeTypeField.ofMode mode
                |> shouldEqual (NodeTypeField.ofMode ((kind <<< 12) ||| 0o644))

    // ------------------------------------------------------------ Darwin's refusals

    [<Test>]
    let ``Darwin refuses a FIFO for any caller, and a privileged caller's other types, before the path is read``
        ()
        : unit
        =
        // Nothing measured Darwin's root, and its FIFO is mkfifo(2), whose
        // rules are not measured: each is refused ahead of every failure the
        // path or the dirfd could give.
        let darwinRoot =
            { darwinUser with
                Credentials = Owners.root
                Label = "Darwin root"
            }

        [
            for envelope in [ darwinUser ; darwinRoot ] do
                for kind in 0..15 do
                    let mode = (kind <<< 12) ||| 0o644

                    for dirfd, path in
                        [
                            -1, PathArgumentBytes.Unreadable
                            -2, PathArg.ofText ""
                            -2, PathArg.ofText "nx"
                        ] do
                        let system = cell envelope "/c/w/d" 0o022

                        let actual =
                            UnixNamespace.mknodat dirfd path mode 0u system
                            |> Result.map (fun (answer, _) -> answer)

                        let want =
                            match NodeTypeField.ofMode mode, envelope = darwinUser with
                            | NodeTypeField.Fifo, _ -> Error MkNodRefusal.Fifo
                            | _, true -> Ok (SyscallAnswer.Failed UnixError.EPERM)
                            | _, false -> Error MkNodRefusal.UnmeasuredPrivilegedCaller

                        if actual <> want then
                            yield $"%s{envelope.Label} 0o%o{mode} dirfd %d{dirfd}: wanted %A{want}, got %A{actual}"
        ]
        |> shouldEqual []
