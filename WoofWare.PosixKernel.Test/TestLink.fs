namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `link(2)` and `linkat(2)`, held to every row `link-symlink.c` (LINKSRC,
/// LINKDST, LINKDEV, LINKPERM) and `link-rules.c` (ORDER, DESTORDER,
/// DESTMORE, STICKY, EMPTY, EMLINK) measured, under each envelope they ran
/// in, and to `at-dirfd.c`'s flag rows. `TestStartingDirectory` replays the
/// `linkat` rows of `at-dirfd.c`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestLink =

    let private context : string = "TestLink"
    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L
    let private bits (raw : int) : PermissionBits = PermissionBits.parseOrFail context raw

    let private name (text : string) : DirectoryEntryName =
        DirectoryEntryName.parseOrFail context text

    let private text (raw : string) : PathArgumentBytes = PathArg.ofText raw
    let private uid (raw : uint32) : UserId = UserId.parseOrFail context raw
    let private gid (raw : uint32) : GroupId = GroupId.parseOrFail context raw

    let private linux : SimulatedUnixPlatform = SimulatedUnixPlatform.linuxX64
    let private darwin : SimulatedUnixPlatform = SimulatedUnixPlatform.macOsArm64

    let private rootOwner : InodeOwner =
        {
            User = UserId.root
            Group = gid 0u
        }

    // ------------------------------------------------------------ the probes' output

    let private lines (resource : string) : string list list =
        use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream resource

        if isNull stream then
            failwith $"%s{context}: no embedded resource %s{resource}"

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun line -> line.Split '\t' |> List.ofArray)
        |> List.ofSeq

    let private section (resource : string) (name : string) : string list list =
        lines resource
        |> List.filter (fun fields -> List.head fields = name)
        |> List.map List.tail

    let private probeAnswer (cell : string) : string =
        let head =
            match cell.IndexOf '(' with
            | -1 -> cell.Trim ()
            | at -> cell.Substring(0, at).Trim ()

        match head with
        | "errno92" -> "EILSEQ"
        | other -> other

    // ------------------------------------------------------------ building a cell

    /// One entry of a hand-built cell, by its path under the cell.
    type private Entry =
        | File of mode : int * owner : InodeOwner option
        | Dir of mode : int * owner : InodeOwner option
        | Link of target : string * owner : InodeOwner option

    /// A seed whose `/c` is the cell, holding `entries` (parents listed before
    /// their children).
    let private seedOf
        (cellOwner : InodeOwner option)
        (entries : (string * Entry) list)
        : Map<DirectoryEntryName, SeedEntry>
        =
        let rec build (prefix : string) : Map<DirectoryEntryName, SeedEntry> =
            entries
            |> List.filter (fun (p, _) -> p.StartsWith prefix && not (p.Substring(prefix.Length).Contains '/'))
            |> List.map (fun (p, entry) ->
                let leaf = p.Substring prefix.Length

                let seeded =
                    match entry with
                    | File (mode, owner) -> SeedEntry.File (ImmutableArray<byte>.Empty, bits mode, owner)
                    | Dir (mode, owner) -> SeedEntry.Directory (build (p + "/"), bits mode, owner)
                    | Link (target, owner) -> SeedEntry.Symlink (SymlinkTarget.parseOrFail context target, owner)

                name leaf, seeded
            )
            |> Map.ofList

        Map.ofList [ name "c", SeedEntry.Directory (build "", bits 0o777, cellOwner) ]

    let private boot
        (platform : SimulatedUnixPlatform)
        (credentials : Credentials)
        (protection : HardlinkProtection)
        (seed : Map<DirectoryEntryName, SeedEntry>)
        : UnixSystem<int, string>
        =
        let image : UnixBootImage<int, string> =
            UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.withCredentials context credentials
            |> UnixBootImage.withProtectedFiles
                context
                { ProtectedFiles.off with
                    Hardlinks = protection
                }

        match
            UnixBootImage.withFileSystemAndCurrentDirectory
                epoch
                (InodeOwner.ofProcess credentials)
                seed
                (AbsoluteUnixPath.parseOrFail context "/c")
                image
        with
        | Ok image -> UnixBootImage.boot image
        | Error fault -> failwith $"%s{context}: could not build the cell: %A{fault}"

    let private rendered (result : Result<SyscallAnswer * UnixSystem<int, string>, LinkRefusal>) : string =
        match result with
        | Ok (SyscallAnswer.Completed _, _) -> "ok"
        | Ok (SyscallAnswer.Failed error, _) -> $"%A{error}"
        | Error refusal -> $"refused: %s{LinkRefusal.describe refusal}"

    let private refusedForDevices (answer : string) : bool = answer.StartsWith "refused: the path"

    let private kindAt (p : string) (system : UnixSystem<int, string>) : string * int64 * int64 =
        match UnixPathResolution.stat SymlinkPolicy.NoFollowFinal (text p) system with
        | Ok (FileStatusAnswer.Reported status) ->
            let kind =
                match status.Mode &&& 0o170000 with
                | 0o120000 -> "symlink"
                | 0o040000 -> "dir"
                | 0o100000 -> "file"
                | _ -> "other"

            kind,
            (match status.Inode with
             | InodeNumber n -> n),
            status.LinkCount
        | Ok (FileStatusAnswer.Failed _) -> "absent", 0L, 0L
        | Error refusal -> failwith $"%s{context}: lstat(%s{p}) was refused: %A{refusal}"

    // ------------------------------------------------------------ link-symlink.c

    type private Envelope =
        {
            Label : string
            Platform : SimulatedUnixPlatform
            Credentials : Credentials
            Resource : string
            /// `link-symlink.c` ran LINKPERM's other-user rows by building them
            /// as root and dropping to uid 1000.
            DropsToUser : bool
            Protection : HardlinkProtection
        }

    let private user1000 : Credentials = Credentials.ofIds (uid 1000u) (gid 1000u) []

    let private symlinkEnvelopes : Envelope list =
        [
            {
                Label = "Linux root"
                Platform = linux
                Credentials = Owners.root
                Resource = "WoofWare.PosixKernel.Test.linkSymlink.linuxRoot.txt"
                DropsToUser = true
                Protection = HardlinkProtection.Off
            }
            {
                Label = "Linux root, protected_hardlinks=1"
                Platform = linux
                Credentials = Owners.root
                Resource = "WoofWare.PosixKernel.Test.linkSymlink.linuxRootProtected.txt"
                DropsToUser = true
                Protection = HardlinkProtection.NonOwnersNeedReadAndWrite
            }
            {
                Label = "Linux uid 1000"
                Platform = linux
                Credentials = user1000
                Resource = "WoofWare.PosixKernel.Test.linkSymlink.linuxUser.txt"
                DropsToUser = false
                Protection = HardlinkProtection.Off
            }
            {
                Label = "Darwin uid 501"
                Platform = darwin
                Credentials = Credentials.ofIds (uid 501u) (gid 20u) []
                Resource = "WoofWare.PosixKernel.Test.linkSymlink.darwin.txt"
                DropsToUser = false
                Protection = HardlinkProtection.Off
            }
        ]

    /// `link-symlink.c`'s cell: f, g, d/, e/, lf -> f, ld -> d, dang -> nx,
    /// cyc -> cyc, dl -> nx2.
    let private symlinkCell : (string * Entry) list =
        [
            "f", File (0o644, None)
            "g", File (0o644, None)
            "d", Dir (0o755, None)
            "e", Dir (0o755, None)
            "lf", Link ("f", None)
            "ld", Link ("d", None)
            "dang", Link ("nx", None)
            "cyc", Link ("cyc", None)
            "dl", Link ("nx2", None)
        ]

    let private howFlags (platform : SimulatedUnixPlatform) (how : string) : int option =
        match how, SimulatedUnixPlatform.flavour platform with
        | "link", _ -> None
        | "linkat(0)", _ -> Some 0
        | "linkat(FOLLOW)", SimulatedUnixFlavour.Linux -> Some 0x400
        | "linkat(FOLLOW)", SimulatedUnixFlavour.Darwin -> Some 0x40
        | other, _ -> failwith $"%s{context}: the probe has no way to link called %s{other}"

    let private linkBy
        (platform : SimulatedUnixPlatform)
        (how : int option)
        (source : string)
        (dest : string)
        (system : UnixSystem<int, string>)
        =
        match how with
        | None -> UnixNamespace.link (text source) (text dest) system
        | Some flags ->
            let atFdCwd = AtDirectory.atFdCwd (SimulatedUnixPlatform.flavour platform)
            UnixNamespace.linkat atFdCwd (text source) atFdCwd (text dest) flags system

    /// LINKSRC: link each source kind to "n" three ways; on success, what "n"
    /// is (the source itself or f, which a followed link names) and the
    /// source's st_nlink.
    let private replayLinkSource (envelope : Envelope) : unit =
        let rows = section envelope.Resource "LINKSRC"
        rows.Length |> shouldEqual 11

        [
            for row in rows do
                let source = row.[0]

                for cell in row.[1..] do
                    let at = cell.IndexOf '='
                    let how = cell.Substring (0, at)
                    let probe = cell.Substring (at + 1)

                    let system =
                        boot envelope.Platform envelope.Credentials envelope.Protection (seedOf None symlinkCell)

                    let _, sourceInode, _ = kindAt source system
                    let _, fInode, _ = kindAt "f" system

                    let actual =
                        match linkBy envelope.Platform (howFlags envelope.Platform how) source "n" system with
                        | Ok (SyscallAnswer.Completed _, after) ->
                            let kind, inode, _ = kindAt "n" after
                            let _, _, nlink = kindAt source after

                            let what =
                                if inode = sourceInode then "the-name-itself"
                                elif inode = fInode then "f"
                                else "other"

                            $"ok(n=%s{kind}:%s{what} nlink=%d{nlink})"
                        | other -> rendered other

                    // The probe also reports which timestamps moved, which this
                    // replay leaves to `VirtualFileSystem.hardLink`'s own tests.
                    let expected =
                        match probe.IndexOf " srcctime" with
                        | -1 -> probe
                        | at -> probe.Substring (0, at) + ")"

                    if actual <> expected then
                        yield $"%s{source} %s{how}: the probe answered %s{expected}, this library %s{actual}"
        ]
        |> shouldEqual []

    /// LINKDST: link f to each destination kind.
    let private replayLinkDestination (envelope : Envelope) : unit =
        let rows = section envelope.Resource "LINKDST"
        rows.Length |> shouldEqual 14

        [
            for row in rows do
                let dest = row.[0]
                let probe = probeAnswer row.[1]

                let system =
                    boot envelope.Platform envelope.Credentials envelope.Protection (seedOf None symlinkCell)

                let actual = UnixNamespace.link (text "f") (text dest) system |> rendered

                if actual <> probe then
                    yield $"%s{dest}: the probe answered %s{probe}, this library %s{actual}"
        ]
        |> shouldEqual []

    /// LINKDEV: across the root filesystem and /dev. This library's device
    /// filesystem holds only the nodes it has drivers for, so a name it does
    /// not hold there is refused rather than looked up; every other row is
    /// answered as measured.
    let private replayLinkDevices (envelope : Envelope) : unit =
        let rows = section envelope.Resource "LINKDEV"
        rows.Length |> shouldEqual 6

        [
            for row in rows do
                let pair = row.[0].Split " -> "
                let probe = probeAnswer row.[1]

                let system =
                    boot envelope.Platform envelope.Credentials envelope.Protection (seedOf None symlinkCell)

                let actual = UnixNamespace.link (text pair.[0]) (text pair.[1]) system |> rendered

                let refusedHere =
                    match SimulatedUnixPlatform.flavour envelope.Platform with
                    // Darwin's devfs is not modelled at all.
                    | SimulatedUnixFlavour.Darwin ->
                        pair.[0].StartsWith "/dev" || (pair.[1].StartsWith "/dev" && pair.[0] <> "nx")
                    // Only the device nodes this library has drivers for exist.
                    | SimulatedUnixFlavour.Linux -> pair.[0] = "/dev/nx" || (pair.[1] = "/dev/n" && pair.[0] <> "nx")

                let ok =
                    if refusedHere then
                        refusedForDevices actual
                    else
                        actual = probe

                if not ok then
                    yield $"%s{row.[0]}: the probe answered %s{probe}, this library %s{actual}"
        ]
        |> shouldEqual []

    /// LINKPERM: an unprivileged caller's rows. The other-user rows exist only
    /// where the probe built root's files and dropped to uid 1000.
    let private replayLinkPermissions (envelope : Envelope) : unit =
        let rows = section envelope.Resource "LINKPERM"
        rows.Length |> shouldEqual 8

        let me =
            if envelope.DropsToUser then
                user1000
            else
                envelope.Credentials

        let mine = Some (InodeOwner.ofProcess me)

        [
            for row in rows do
                let label = row.[0]
                let probe = probeAnswer row.[1]

                let others =
                    if envelope.DropsToUser then
                        [
                            "rootfile600", File (0o600, Some rootOwner)
                            "rootfile644", File (0o644, Some rootOwner)
                            "rootfile666", File (0o666, Some rootOwner)
                        ]
                    else
                        []

                let cell (eMode : int) (eHolds : (string * Entry) list) (dMode : int) (dHolds : (string * Entry) list) =
                    [ "f", File (0o644, mine) ; "g", File (0o644, mine) ; "d", Dir (dMode, mine) ]
                    @ dHolds
                    @ [ "e", Dir (eMode, mine) ]
                    @ eHolds
                    @ others

                let entries, source, dest =
                    match label with
                    | "dest-dir-0555" -> cell 0o555 [] 0o755 [], "f", "e/n"
                    | "src-dir-0600" -> cell 0o755 [] 0o600 [ "d/x", File (0o644, mine) ], "d/x", "n"
                    | "src-dir-0600-absent" -> cell 0o755 [] 0o600 [ "d/x", File (0o644, mine) ], "d/nx", "n"
                    | "others-file-0600" -> cell 0o755 [] 0o755 [], "rootfile600", "n"
                    | "others-file-0644" -> cell 0o755 [] 0o755 [], "rootfile644", "n"
                    | "others-file-0666" -> cell 0o755 [] 0o755 [], "rootfile666", "n"
                    | "absent-src+dest-dir-0555" -> cell 0o555 [] 0o755 [], "nx", "e/n"
                    | "existing-dest+dest-dir-0555" -> cell 0o555 [ "e/n", File (0o644, mine) ] 0o755 [], "f", "e/n"
                    | other -> failwith $"%s{context}: the probe has no LINKPERM row %s{other}"

                let system = boot envelope.Platform me envelope.Protection (seedOf mine entries)
                let actual = UnixNamespace.link (text source) (text dest) system |> rendered

                if actual <> probe then
                    yield $"%s{label}: the probe answered %s{probe}, this library %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``every source kind links as measured, each way, under every envelope`` () : unit =
        for envelope in symlinkEnvelopes do
            replayLinkSource envelope

    [<Test>]
    let ``every destination kind answers as measured, under every envelope`` () : unit =
        for envelope in symlinkEnvelopes do
            replayLinkDestination envelope

    [<Test>]
    let ``crossing into and out of /dev answers as measured, or refuses a name it cannot know`` () : unit =
        for envelope in symlinkEnvelopes do
            replayLinkDevices envelope

    [<Test>]
    let ``an unprivileged caller's rows answer as measured, protected_hardlinks included`` () : unit =
        for envelope in symlinkEnvelopes do
            replayLinkPermissions envelope

    // ------------------------------------------------------------ link-rules.c

    let private rulesLinux : string = "WoofWare.PosixKernel.Test.linkRules.linux.txt"
    let private rulesDarwin : string = "WoofWare.PosixKernel.Test.linkRules.darwin.txt"

    /// `link-rules.c`'s cell, as `caller` sees it; the other user is root.
    let private rulesCell (darwinFlavour : bool) (caller : InodeOwner) : (string * Entry) list =
        let mine = Some caller
        let root = Some rootOwner

        let third =
            Some
                {
                    User = uid 2000u
                    Group = gid 2000u
                }

        let others =
            if darwinFlavour then
                [ "o644", File (0o644, root) ]
            else
                [
                    "o600", File (0o600, root)
                    "o644", File (0o644, root)
                    "o666", File (0o666, root)
                    "o4666", File (0o4666, root)
                    "o2676", File (0o2676, root)
                    "o2666", File (0o2666, root)
                    "olnk", Link ("f", root)
                    "odir", Dir (0o777, root)
                    "st", Dir (0o1777, root)
                    "st/so", File (0o666, root)
                    "x600", File (0o600, third)
                    "x622", File (0o622, third)
                    "x4666", File (0o4666, third)
                    "xlnk", Link ("f", third)
                ]

        [
            "f", File (0o644, mine)
            "g", File (0o644, mine)
            "d", Dir (0o755, mine)
            "u", Dir (0o555, mine)
            "u/ug", File (0o644, (if darwinFlavour then mine else root))
            "s", Dir (0o600, mine)
            "s/sf", File (0o644, mine)
        ]
        @ others

    let private rulesSystem (darwinFlavour : bool) (protection : int) (caller : int) : UnixSystem<int, string> =
        let platform = if darwinFlavour then darwin else linux

        let credentials =
            match darwinFlavour, caller with
            | true, _ -> Credentials.ofIds (uid 501u) (gid 20u) []
            | false, 0 -> Owners.root
            | false, _ -> user1000

        let hardlinks =
            match protection with
            | 1 -> HardlinkProtection.NonOwnersNeedReadAndWrite
            | _ -> HardlinkProtection.Off

        let owner = InodeOwner.ofProcess credentials
        boot platform credentials hardlinks (seedOf (Some owner) (rulesCell darwinFlavour owner))

    /// The field `key=value` of one of the probe's header cells.
    let private header (row : string list) (key : string) : string =
        row
        |> List.pick (fun cell ->
            if cell.StartsWith (key + "=") then
                Some (cell.Substring (key.Length + 1))
            else
                None
        )

    let private replayOrder (resource : string) (darwinFlavour : bool) : unit =
        let rows = section resource "ORDER"
        rows.Length |> shouldBeGreaterThan 0

        [
            for row in rows do
                let protection = int (header row "protected_hardlinks")
                let caller = int (header row "caller")
                let source = header row "source"

                for cell in row.[3..] do
                    let at = cell.IndexOf '='
                    let dest = cell.Substring (0, at)
                    let probe = probeAnswer (cell.Substring (at + 1))
                    let system = rulesSystem darwinFlavour protection caller
                    let actual = UnixNamespace.link (text source) (text dest) system |> rendered

                    // A free name in /dev is refused unless the source alone
                    // decides the call: this library's device filesystem knows
                    // only the nodes it has drivers for.
                    let sourceDecides =
                        source = "nx"
                        || (source = "s/sf" && (darwinFlavour || caller <> 0))
                        || (darwinFlavour && source = "d")

                    let ok =
                        if dest = "/dev/n" && not sourceDecides then
                            refusedForDevices actual
                        else
                            actual = probe

                    if not ok then
                        yield
                            $"protected_hardlinks=%d{protection} caller=%d{caller} %s{source} -> %s{dest}: the probe answered %s{probe}, this library %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``every source against every destination answers as measured on Linux`` () : unit = replayOrder rulesLinux false

    [<Test>]
    let ``every source against every destination answers as measured on Darwin`` () : unit =
        replayOrder rulesDarwin true

    let private overlong (platform : SimulatedUnixPlatform) : PathArgumentBytes =
        text (String ('a', PathLimits.pathMaxBytes (SimulatedUnixPlatform.pathLimits platform)))

    let private replayDestinationOrder (resource : string) (darwinFlavour : bool) : unit =
        let rows = section resource "DESTORDER"
        rows.Length |> shouldBeGreaterThan 0
        let platform = if darwinFlavour then darwin else linux
        let atFdCwd = AtDirectory.atFdCwd (SimulatedUnixPlatform.flavour platform)

        [
            for row in rows do
                let protection = int (header row "protected_hardlinks")
                let caller = int (header row "caller")
                let source = header row "source"

                for cell in row.[3..] do
                    let at = cell.LastIndexOf '='
                    let label = cell.Substring (0, at)
                    let probe = probeAnswer (cell.Substring (at + 1))

                    let dest, destFd =
                        match label with
                        | "NULL" -> PathArgumentBytes.Unreadable, atFdCwd
                        | "empty" -> text "", atFdCwd
                        | "overlong" -> overlong platform, atFdCwd
                        | "dirfd=-1" -> text "n", -1
                        | other -> failwith $"%s{context}: the probe has no DESTORDER cell %s{other}"

                    let system = rulesSystem darwinFlavour protection caller

                    let actual =
                        UnixNamespace.linkat atFdCwd (text source) destFd dest 0 system |> rendered

                    if actual <> probe then
                        yield
                            $"protected_hardlinks=%d{protection} caller=%d{caller} %s{source} -> %s{label}: the probe answered %s{probe}, this library %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``a destination that cannot be read or looked up wins as measured on Linux`` () : unit =
        replayDestinationOrder rulesLinux false

    [<Test>]
    let ``a destination that cannot be read or looked up wins as measured on Darwin`` () : unit =
        replayDestinationOrder rulesDarwin true

    let private replayDestinationMore (resource : string) (darwinFlavour : bool) : unit =
        let rows = section resource "DESTMORE"
        rows.Length |> shouldBeGreaterThan 0

        [
            for row in rows do
                let protection = int (header row "protected_hardlinks")
                let caller = int (header row "caller")
                let label = row.[2]
                let probe = probeAnswer row.[3]

                let dest =
                    match label with
                    | "unwritable: free name/" -> text "u/n/"
                    | "unwritable: taken name" -> text "u/ug"
                    | "writable: unbindable name" -> PathArg.ofBytes [ 0xFFuy ; 0xFEuy ]
                    | "unwritable: unbindable name" -> PathArg.ofBytes [ byte 'u' ; byte '/' ; 0xFFuy ; 0xFEuy ]
                    | other -> failwith $"%s{context}: the probe has no DESTMORE row %s{other}"

                let actual =
                    UnixNamespace.link (text "f") dest (rulesSystem darwinFlavour protection caller)
                    |> rendered

                if actual <> probe then
                    yield
                        $"protected_hardlinks=%d{protection} caller=%d{caller} %s{label}: the probe answered %s{probe}, this library %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``the destination's own refusals fall in the order measured on Linux`` () : unit =
        replayDestinationMore rulesLinux false

    [<Test>]
    let ``the destination's own refusals fall in the order measured on Darwin`` () : unit =
        replayDestinationMore rulesDarwin true

    [<Test>]
    let ``a sticky directory on either side changes nothing`` () : unit =
        for resource, darwinFlavour in [ rulesLinux, false ; rulesDarwin, true ] do
            for row in section resource "STICKY" do
                let protection = int (header row "protected_hardlinks")
                let caller = int (header row "caller")
                let probe = probeAnswer row.[3]

                let source, dest =
                    if row.[2].StartsWith "own file" then
                        "f", "st/n"
                    else
                        "st/so", "n"

                let system =
                    if darwinFlavour then
                        // Darwin's rows used /private/tmp and a root-owned 0644
                        // file; this cell's st/ stands for the first, and its
                        // o644 for the second.
                        let owner = InodeOwner.ofProcess (Credentials.ofIds (uid 501u) (gid 20u) [])

                        rulesCell true owner
                        @ [ "st", Dir (0o1777, Some rootOwner) ; "st/so", File (0o644, Some rootOwner) ]
                        |> seedOf (Some owner)
                        |> boot darwin (Credentials.ofIds (uid 501u) (gid 20u) []) HardlinkProtection.Off
                    else
                        rulesSystem false protection caller

                UnixNamespace.link (text source) (text dest) system
                |> rendered
                |> fun actual ->
                    (row.[0], row.[1], row.[2], actual)
                    |> shouldEqual (row.[0], row.[1], row.[2], probe)

    /// `p` opened read-only, as the system's caller.
    let private opened (p : string) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let flags =
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

        match Answered.openPath flags (UnixPath.parseOrFail context p) 0 system with
        | SyscallAnswer.Completed fd, system -> int fd, system
        | other -> failwith $"%s{context}: open(%s{p}) did not open: %O{other}"

    let private linuxEmptyPath : int = 0x1000
    let private linuxAtFdCwd : int = -100

    /// What this library must answer a Linux `linkat` that the probe answered
    /// `probe`: refuse an unprivileged `AT_EMPTY_PATH` whose path is relative
    /// to a descriptor, whose answer turns on credentials it does not record,
    /// and otherwise answer as the probe did.
    let private expectedEmptyPath (caller : int) (dirfd : int) (path : string) (flags : int) (probe : string) : string =
        if
            caller <> 0
            && flags &&& linuxEmptyPath <> 0
            && dirfd <> linuxAtFdCwd
            && not (path.StartsWith "/")
        then
            $"refused: %s{LinkRefusal.describe LinkRefusal.OpenTimeCredentials}"
        else
            probe

    [<Test>]
    let ``Linux's AT_EMPTY_PATH answers as measured, unless an unprivileged caller's answer turns on a descriptor's credentials``
        ()
        : unit
        =
        let rows = section rulesLinux "EMPTY"
        rows.Length |> shouldEqual 32

        [
            for row in rows do
                let protection = int (header row "protected_hardlinks")
                let caller = int (header row "caller")
                let label = row.[2]
                let probe = probeAnswer row.[3]
                let system = rulesSystem false protection caller

                let emptyPath = linuxEmptyPath
                let follow = 0x400

                let fd, path, flags, system =
                    match label with
                    | "own file" ->
                        let fd, system = opened "f" system
                        fd, "", emptyPath, system
                    | "own directory" ->
                        let fd, system = opened "d" system
                        fd, "", emptyPath, system
                    | "own file, unlinked" ->
                        let fd, system = opened "g" system
                        let _, system = Answered.unlink (UnixPath.parseOrFail context "g") system
                        fd, "", emptyPath, system
                    | "another user's 0644 file" ->
                        let fd, system = opened "o644" system
                        fd, "", emptyPath, system
                    | "own file, flags 0" ->
                        let fd, system = opened "f" system
                        fd, "", 0, system
                    | "directory with ../f" ->
                        let fd, system = opened "d" system
                        fd, "../f", emptyPath, system
                    | "AT_FDCWD" -> -100, "", emptyPath, system
                    | "another user's 0644 file, with AT_SYMLINK_FOLLOW" ->
                        let fd, system = opened "o644" system
                        fd, "", emptyPath ||| follow, system
                    | other -> failwith $"%s{context}: the probe has no EMPTY row %s{other}"

                let actual =
                    UnixNamespace.linkat fd (text path) -100 (text "n") flags system |> rendered

                let expected = expectedEmptyPath caller fd path flags probe

                if actual <> expected then
                    yield
                        $"protected_hardlinks=%d{protection} caller=%d{caller} %s{label}: the probe answered %s{probe}, this library %s{actual}"
        ]
        |> shouldEqual []

    // -------------------------------------------------------- link-empty-path.c

    let private emptyPathLinux : string =
        "WoofWare.PosixKernel.Test.linkEmptyPath.linux.txt"

    /// `link-empty-path.c`'s cell: f, g (the caller's, 0644), u/ (0555) and d/.
    let private emptyPathSystem (caller : int) : UnixSystem<int, string> =
        let credentials = if caller = 0 then Owners.root else user1000
        let owner = Some (InodeOwner.ofProcess credentials)

        [
            "f", File (0o644, owner)
            "g", File (0o644, owner)
            "u", Dir (0o555, owner)
            "d", Dir (0o777, owner)
        ]
        |> seedOf (Some rootOwner)
        |> boot linux credentials HardlinkProtection.Off

    [<Test>]
    let ``AT_EMPTY_PATH's credential check is refused wherever Linux makes it, and modelled where it does not``
        ()
        : unit
        =
        let rows = section emptyPathLinux "CRED"
        rows.Length |> shouldEqual 16

        [
            for row in rows do
                let label = row.[0]
                let probe = probeAnswer row.[1]
                let caller = if label.StartsWith "root calling" then 0 else 1000
                let system = emptyPathSystem caller

                // This library cannot tell who opened a descriptor, so each is
                // opened by the caller: the rows that differ only in that must
                // all be refused.
                let descriptorOf (what : string) (system : UnixSystem<int, string>) =
                    if what.Contains "dirfd" then opened "." system
                    elif what.Contains "file descriptor" then opened "f" system
                    else linuxAtFdCwd, system

                let dirfd, system = descriptorOf label system

                let path, flags =
                    if label.EndsWith ", flags 0" then
                        "f", 0
                    elif label.Contains "rooted path" then
                        "/c/f", linuxEmptyPath
                    elif label.Contains "\"x\"" then
                        "x", linuxEmptyPath
                    elif label.Contains "\"f\"" then
                        "f", linuxEmptyPath
                    elif label.Contains "\"\"" then
                        "", linuxEmptyPath
                    else
                        failwith $"%s{context}: the probe has no CRED row %s{label}"

                let actual =
                    UnixNamespace.linkat dirfd (text path) linuxAtFdCwd (text "n") flags system
                    |> rendered

                let expected = expectedEmptyPath caller dirfd path flags probe

                if actual <> expected then
                    yield
                        $"%s{label}: the probe answered %s{probe}, so this library must answer %s{expected}; it answered %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``an unlinked file's ENOENT comes after every refusal of its destination`` () : unit =
        let rows = section emptyPathLinux "UNLINKED"
        rows.Length |> shouldEqual 22

        [
            for row in rows do
                let caller = int (header row "caller")
                let label = row.[1]
                let probe = probeAnswer row.[2]
                let system = emptyPathSystem caller
                let fd, system = opened "g" system
                let _, system = Answered.unlink (UnixPath.parseOrFail context "g") system

                let dest, destFd =
                    match label with
                    | "\"n\"" -> text "n", linuxAtFdCwd
                    | "taken name \"f\"" -> text "f", linuxAtFdCwd
                    | "NULL" -> PathArgumentBytes.Unreadable, linuxAtFdCwd
                    | "\"\"" -> text "", linuxAtFdCwd
                    | "PATH_MAX bytes" -> overlong linux, linuxAtFdCwd
                    | "newdirfd -1, \"n\"" -> text "n", -1
                    | "\"/dev/n\"" -> text "/dev/n", linuxAtFdCwd
                    | "unwritable \"u/n\"" -> text "u/n", linuxAtFdCwd
                    | "\"f/n\"" -> text "f/n", linuxAtFdCwd
                    | "\"n/\"" -> text "n/", linuxAtFdCwd
                    | "\"nx/n\"" -> text "nx/n", linuxAtFdCwd
                    | other -> failwith $"%s{context}: the probe has no UNLINKED row %s{other}"

                let actual =
                    UnixNamespace.linkat fd (text "") destFd dest linuxEmptyPath system |> rendered

                let expected = expectedEmptyPath caller fd "" linuxEmptyPath probe

                // A free name in /dev is refused: this library's device
                // filesystem knows only the nodes it has drivers for.
                let ok =
                    if caller = 0 && label = "\"/dev/n\"" then
                        refusedForDevices actual
                    else
                        actual = expected

                if not ok then
                    yield
                        $"caller=%d{caller} %s{label}: the probe answered %s{probe}, so this library must answer %s{expected}; it answered %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``tmpfs and APFS give one file every name the probe gave it, and Darwin caps what it reports`` () : unit =
        let emlink = section rulesLinux "EMLINK" @ section rulesDarwin "EMLINK"

        // Only the filesystems this library models: tmpfs on Linux and APFS on
        // Darwin. The probe's ext4 row (EMLINK at 65000) has no counterpart.
        let modelled = emlink |> List.filter (fun row -> not (row.[0].StartsWith "/tmp/"))

        modelled.Length |> shouldEqual 2

        for row in modelled do
            let platform = if row.[0].StartsWith "/dev/shm" then linux else darwin
            let links = int (row.[1].Substring "links=".Length)
            let reported = int64 (row.[3].Substring "st_nlink=".Length)
            row.[2] |> shouldEqual "then=none"

            let credentials =
                UnixSystem.defaultCredentials (SimulatedUnixPlatform.flavour platform)

            let owner = InodeOwner.ofProcess credentials

            let mutable system =
                boot platform credentials HardlinkProtection.Off (seedOf (Some owner) [ "f", File (0o644, None) ])

            for i in 0 .. links - 1 do
                match UnixNamespace.link (text "f") (text $"l%d{i}") system with
                | Ok (SyscallAnswer.Completed _, after) -> system <- after
                | other -> failwith $"%s{context}: link %d{i} did not succeed: %A{other}"

            let _, _, nlink = kindAt "f" system
            (row.[0], nlink) |> shouldEqual (row.[0], reported)

    // ------------------------------------------------------------ the flag word

    [<Test>]
    let ``linkat screens every single flag bit as measured`` () : unit =
        for resource, flavour in
            [
                "WoofWare.PosixKernel.Test.atDirfd.linuxUser.txt", SimulatedUnixFlavour.Linux
                "WoofWare.PosixKernel.Test.atDirfd.darwin.txt", SimulatedUnixFlavour.Darwin
            ] do
            let row = section resource "FLAGS" |> List.find (fun row -> row.[0] = "linkat")

            let rejected =
                row.[2].Substring("rejected:".Length).Split (' ', StringSplitOptions.RemoveEmptyEntries)
                |> Array.map (fun bit -> Convert.ToInt32 ((bit.Split '=').[0].Substring 2, 16))
                |> Set.ofArray

            for bit in 0..31 do
                let flag = 1 <<< bit

                match LinkRules.screen flavour flag, rejected.Contains flag with
                | LinkScreen.Failed UnixError.EINVAL, true -> ()
                | (LinkScreen.Screened _ | LinkScreen.Unmodelled _), false -> ()
                | other, measured ->
                    failwith $"%O{flavour} 0x%x{flag}: screened as %A{other}, the probe rejected it: %b{measured}"

    // ------------------------------------------------------------ what is refused

    [<Test>]
    let ``a link on NFS is refused`` () : unit =
        let image : UnixBootImage<int, string> =
            UnixSystem.initial linux UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.withMount (Some EmulatedMount.Nfs)

        let credentials = UnixSystem.defaultCredentials SimulatedUnixFlavour.Linux

        let system =
            match
                UnixBootImage.withFileSystemAndCurrentDirectory
                    epoch
                    (InodeOwner.ofProcess credentials)
                    (seedOf None [ "f", File (0o644, None) ])
                    (AbsoluteUnixPath.parseOrFail context "/c")
                    image
            with
            | Ok image -> UnixBootImage.boot image
            | Error fault -> failwith $"%A{fault}"

        UnixNamespace.link (text "f") (text "n") system
        |> rendered
        |> shouldEqual
            $"refused: %s{LinkRefusal.describe (LinkRefusal.UnmeasuredFileSystem EmulatedFileSystemType.Nfs)}"

    [<Test>]
    let ``LinkAt through the syscall step is linkat`` () : unit =
        for darwinFlavour in [ false ; true ] do
            let platform = if darwinFlavour then darwin else linux
            let atFdCwd = AtDirectory.atFdCwd (SimulatedUnixPlatform.flavour platform)

            for source, dest in [ "f", "n" ; "f", "g" ; "d", "n" ; "nx", "n" ] do
                let system = rulesSystem darwinFlavour 0 1000

                let direct =
                    match UnixNamespace.linkat atFdCwd (text source) atFdCwd (text dest) 0 system with
                    | Ok (answer, after) -> Ok (SyscallOutcome.Answered answer, after)
                    | Error refusal -> Error (SyscallRefusal.Link refusal)

                UnixSystem.step 0 (Syscall.LinkAt (atFdCwd, text source, atFdCwd, text dest, 0)) system
                |> shouldEqual direct
