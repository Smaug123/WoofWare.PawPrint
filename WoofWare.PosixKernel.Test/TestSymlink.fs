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

/// `symlink(2)` and `symlinkat(2)`, held to `link-symlink.c`'s rows under each
/// envelope it ran in: the targets and names it tried (SYMLINK), which failure
/// each argument wins with when several are bad at once (SYMORDER), an
/// unprivileged caller's rows (SYMPERM), a new link's mode under each umask
/// (SYMMODE) and its group (SYMGROUP). `TestStartingDirectory` replays the
/// `symlinkat` rows of `at-dirfd.c`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSymlink =

    let private context : string = "TestSymlink"
    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L
    let private bits (raw : int) : PermissionBits = PermissionBits.parseOrFail context raw

    let private name (text : string) : DirectoryEntryName =
        DirectoryEntryName.parseOrFail context text

    // ------------------------------------------------------------ the probe's output

    type private Envelope =
        {
            Label : string
            Platform : SimulatedUnixPlatform
            Credentials : Credentials
            Resource : string
        }

    let private linuxRoot : Envelope =
        {
            Label = "Linux root"
            Platform = SimulatedUnixPlatform.linuxX64
            Credentials = Owners.root
            Resource = "WoofWare.PosixKernel.Test.linkSymlink.linuxRoot.txt"
        }

    let private linuxUser : Envelope =
        {
            Label = "Linux uid 1000"
            Platform = SimulatedUnixPlatform.linuxX64
            Credentials = Credentials.ofIds (UserId.parseOrFail context 1000u) (GroupId.parseOrFail context 1000u) []
            Resource = "WoofWare.PosixKernel.Test.linkSymlink.linuxUser.txt"
        }

    let private darwinUser : Envelope =
        {
            Label = "Darwin uid 501"
            Platform = SimulatedUnixPlatform.macOsArm64
            Credentials =
                Credentials.ofIds
                    (UserId.parseOrFail context 501u)
                    (GroupId.parseOrFail context 20u)
                    [ GroupId.parseOrFail context 12u ]
            Resource = "WoofWare.PosixKernel.Test.linkSymlink.darwin.txt"
        }

    let private envelopes : Envelope list = [ linuxRoot ; linuxUser ; darwinUser ]

    let private isDarwin (envelope : Envelope) : bool =
        SimulatedUnixPlatform.flavour envelope.Platform = SimulatedUnixFlavour.Darwin

    /// The probe's lines of one section, split on tabs, without the section.
    let private section (envelope : Envelope) (name : string) : string list list =
        use stream =
            Assembly.GetExecutingAssembly().GetManifestResourceStream envelope.Resource

        if isNull stream then
            failwith $"%s{context}: no embedded resource %s{envelope.Resource}"

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun line -> line.Split '\t' |> List.ofArray)
        |> Seq.filter (fun fields -> List.head fields = name)
        |> Seq.map List.tail
        |> List.ofSeq

    /// The errno at the head of one of the probe's cells, in this library's
    /// spelling: Darwin's EILSEQ prints as its number.
    let private probeAnswer (cell : string) : string =
        let head =
            match cell.IndexOf '(' with
            | -1 -> cell.Trim ()
            | at -> cell.Substring(0, at).Trim ()

        match head with
        | "errno92" -> "EILSEQ"
        | other -> other

    // ------------------------------------------------------------ the probe's fixture

    let private file : SeedEntry =
        SeedEntry.File (ImmutableArray<byte>.Empty, bits 0o644, None)

    let private link (target : string) : SeedEntry =
        SeedEntry.Symlink (SymlinkTarget.parseOrFail context target, None)

    let private dir (bitsOf : int) (group : GroupId option) (entries : (string * SeedEntry) list) : SeedEntry =
        let owner =
            group
            |> Option.map (fun group ->
                {
                    User = UserId.root
                    Group = group
                }
            )

        SeedEntry.Directory (entries |> List.map (fun (n, e) -> name n, e) |> Map.ofList, bits bitsOf, owner)

    /// The probe's cell, which is the cwd: f and g, d/ and e/, and links
    /// lf -> f, ld -> d, dang -> nx, cyc -> cyc and dl -> nx2; plus, for the
    /// SYMPERM rows, u/ (0555, holding f and dl -> nx), s/ (0600, holding f)
    /// and w/.
    let private cell (eGroup : GroupId option) (eBits : int) : Map<DirectoryEntryName, SeedEntry> =
        let eEntry =
            match eGroup with
            | None -> dir eBits None []
            | Some group ->
                SeedEntry.Directory (
                    Map.empty,
                    bits eBits,
                    Some
                        {
                            User = UserId.root
                            Group = group
                        }
                )

        Map.ofList
            [
                name "c",
                dir
                    0o777
                    None
                    [
                        "f", file
                        "g", file
                        "d", dir 0o755 None []
                        "e", eEntry
                        "lf", link "f"
                        "ld", link "d"
                        "dang", link "nx"
                        "cyc", link "cyc"
                        "dl", link "nx2"
                        "u", dir 0o555 None [ "f", file ; "dl", link "nx" ]
                        "s", dir 0o600 None [ "f", file ]
                        "w", dir 0o755 None []
                    ]
            ]

    let private boot
        (envelope : Envelope)
        (umask : int)
        (seed : Map<DirectoryEntryName, SeedEntry>)
        : UnixSystem<int, string>
        =
        let image : UnixBootImage<int, string> =
            UnixSystem.initial envelope.Platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.withCredentials context envelope.Credentials
            |> UnixBootImage.withUmask context (bits umask)

        match
            UnixBootImage.withFileSystemAndCurrentDirectory
                epoch
                (InodeOwner.ofProcess envelope.Credentials)
                seed
                (AbsoluteUnixPath.parseOrFail context "/c")
                image
        with
        | Ok image -> UnixBootImage.boot image
        | Error fault -> failwith $"%s{context}: could not build the probe's cell: %A{fault}"

    let private probeCell (envelope : Envelope) : UnixSystem<int, string> = boot envelope 0o022 (cell None 0o755)

    let private ofBytes (raw : byte list) : PathArgumentBytes = PathArg.ofBytes raw

    let private text (raw : string) : PathArgumentBytes = PathArg.ofText raw

    let private pathMax (envelope : Envelope) : int =
        PathLimits.pathMaxBytes (SimulatedUnixPlatform.pathLimits envelope.Platform)

    let private rendered (result : Result<SyscallAnswer * UnixSystem<int, string>, SymlinkRefusal>) : string =
        match result with
        | Ok (SyscallAnswer.Completed _, _) -> "ok"
        | Ok (SyscallAnswer.Failed error, _) -> $"%A{error}"
        | Error SymlinkRefusal.EmptyTarget -> "refused: empty target"
        | Error refusal -> $"refused: %s{SymlinkRefusal.describe refusal}"

    /// What `lstat` makes of `p` in `system`: absent, or the kind it names.
    let private kindAt (p : string) (system : UnixSystem<int, string>) : string =
        match UnixPathResolution.stat SymlinkPolicy.NoFollowFinal (text p) system with
        | Ok (FileStatusAnswer.Reported status) ->
            match status.Mode &&& 0o170000 with
            | 0o120000 -> "symlink"
            | 0o040000 -> "dir"
            | 0o100000 -> "file"
            | _ -> "other"
        | Ok (FileStatusAnswer.Failed _) -> "absent"
        | Error refusal -> failwith $"%s{context}: lstat(%s{p}) was refused: %A{refusal}"

    // ------------------------------------------------------------ SYMLINK

    /// The probe's SYMLINK rows: a label, the target and the name.
    let private symlinkRow (envelope : Envelope) (label : string) : PathArgumentBytes * PathArgumentBytes * string =
        let long = String ('a', pathMax envelope - 1)
        let full = String ('a', pathMax envelope)

        match label with
        | "t -> n" -> text "t", text "n", "n"
        | "'' -> n" -> text "", text "n", "n"
        | "PATH_MAX-1 bytes -> n" -> text long, text "n", "n"
        | "PATH_MAX bytes -> n" -> text full, text "n", "n"
        | "t -> f" -> text "t", text "f", "f"
        | "t -> dl (dangling)" -> text "t", text "dl", "dl"
        | "t -> lf" -> text "t", text "lf", "lf"
        | "t -> d" -> text "t", text "d", "d"
        | "t -> n/" -> text "t", text "n/", "n"
        | "t -> dl/ (dangling)" -> text "t", text "dl/", "dl"
        | "t -> f/" -> text "t", text "f/", "f"
        | "t -> d/" -> text "t", text "d/", "d"
        | "t -> nxdir/n" -> text "t", text "nxdir/n", "nxdir/n"
        | "t -> f/n" -> text "t", text "f/n", "f/n"
        | "t -> ld/n" -> text "t", text "ld/n", "ld/n"
        | "t -> ." -> text "t", text ".", "."
        | "t -> ''" -> text "t", text "", ""
        | other -> failwith $"%s{context}: the probe has no SYMLINK row %s{other}"

    /// What this library answers for one SYMLINK row, as the probe printed it:
    /// the errno, then what is at nx2, nx and n afterwards, then (on success)
    /// the new link's size.
    let private replaySymlinkRow (envelope : Envelope) (label : string) : string =
        let target, path, created = symlinkRow envelope label
        let system = probeCell envelope

        match UnixNamespace.symlink target path system with
        | Error SymlinkRefusal.EmptyTarget -> "refused: empty target"
        | Error refusal -> $"refused: %s{SymlinkRefusal.describe refusal}"
        | Ok (answer, after) ->
            let nx2 = kindAt "nx2" after
            let nx = kindAt "nx" after
            let n = kindAt "n" after
            let kinds = $"(nx2=%s{nx2} nx=%s{nx} n=%s{n})"

            match answer with
            | SyscallAnswer.Failed error -> $"%A{error}%s{kinds}"
            | SyscallAnswer.Completed _ ->
                match UnixPathResolution.stat SymlinkPolicy.NoFollowFinal (text created) after with
                | Ok (FileStatusAnswer.Reported status) -> $"ok%s{kinds}(size=%d{status.Size})"
                | other -> failwith $"%s{context}: the new link at %s{created} does not stat: %A{other}"

    /// The probe's row, normalised as `replaySymlinkRow` renders: the size is
    /// `readlink`'s count and `lstat`'s alike.
    let private expectedSymlinkRow (envelope : Envelope) (cell : string) : string =
        if isDarwin envelope && cell.StartsWith "ok" && cell.Contains "size=0)" then
            // Darwin's empty target: this library refuses to create the link.
            "refused: empty target"
        else
            match cell.IndexOf "(readlink=" with
            | -1 -> cell
            | at ->
                let size = cell.Substring(cell.IndexOf "size=" + "size=".Length).TrimEnd ')'
                $"%s{cell.Substring (0, at)}(size=%s{size})"

    let private replaySymlink (envelope : Envelope) : unit =
        let rows = section envelope "SYMLINK"
        rows.Length |> shouldEqual 17

        [
            for row in rows do
                let label = row.[0]
                let expected = expectedSymlinkRow envelope row.[1]
                let actual = replaySymlinkRow envelope label

                if actual <> expected then
                    yield $"%s{label}: the probe answered %s{expected}, this library %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``every target and name the probe tried answers as measured, as Linux root`` () : unit = replaySymlink linuxRoot

    [<Test>]
    let ``every target and name the probe tried answers as measured, as a Linux user`` () : unit =
        replaySymlink linuxUser

    [<Test>]
    let ``every target and name the probe tried answers as measured, on Darwin`` () : unit = replaySymlink darwinUser

    // ------------------------------------------------------------ SYMORDER

    let private orderTarget (envelope : Envelope) (label : string) : PathArgumentBytes =
        match label with
        | "t" -> text "t"
        | "empty" -> text ""
        | "NULL" -> PathArgumentBytes.Unreadable
        | "overlong" -> text (String ('a', pathMax envelope))
        | other -> failwith $"%s{context}: the probe has no SYMORDER target %s{other}"

    let private orderName (envelope : Envelope) (label : string) : PathArgumentBytes =
        match label with
        | "NULL" -> PathArgumentBytes.Unreadable
        | "empty" -> text ""
        | "overlong" -> text (String ('a', pathMax envelope))
        | other -> text other

    let private replayOrder (envelope : Envelope) : unit =
        let rows = section envelope "SYMORDER"
        rows.Length |> shouldEqual 8
        let atFdCwd = AtDirectory.atFdCwd (SimulatedUnixPlatform.flavour envelope.Platform)

        [
            for row in rows do
                // "target=t dirfd=AT_FDCWD", then one cell per name.
                let header = row.[0].Split ' '
                let targetLabel = header.[0].Substring "target=".Length

                let dirfd =
                    match header.[1].Substring "dirfd=".Length with
                    | "AT_FDCWD" -> atFdCwd
                    | other -> int other

                for cell in row.[1..] do
                    let at = cell.IndexOf '='
                    let nameLabel = cell.Substring (0, at)
                    let probe = probeAnswer (cell.Substring (at + 1))

                    let expected =
                        if probe = "ok" && targetLabel = "empty" then
                            "refused: empty target"
                        else
                            probe

                    let actual =
                        UnixNamespace.symlinkat
                            (orderTarget envelope targetLabel)
                            dirfd
                            (orderName envelope nameLabel)
                            (probeCell envelope)
                        |> rendered

                    if actual <> expected then
                        yield
                            $"%s{row.[0]} name=%s{nameLabel}: the probe answered %s{expected}, this library %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``each bad argument wins as the probe measured, as Linux root`` () : unit = replayOrder linuxRoot

    [<Test>]
    let ``each bad argument wins as the probe measured, as a Linux user`` () : unit = replayOrder linuxUser

    [<Test>]
    let ``each bad argument wins as the probe measured, on Darwin`` () : unit = replayOrder darwinUser

    // ------------------------------------------------------------ SYMPERM

    let private permRow (label : string) : PathArgumentBytes * PathArgumentBytes =
        let unbindable = [ byte 'w' ; byte '/' ; 0xFFuy ; 0xFEuy ]

        match label with
        | "unwritable: free name" -> text "t", text "u/n"
        | "unwritable: taken name" -> text "t", text "u/f"
        | "unwritable: free name/" -> text "t", text "u/n/"
        | "unwritable: dangling/" -> text "t", text "u/dl/"
        | "unwritable: file/" -> text "t", text "u/f/"
        | "unsearchable: free name" -> text "t", text "s/n"
        | "unsearchable: taken name" -> text "t", text "s/f"
        | "writable: unbindable name" -> text "t", ofBytes unbindable
        | "unwritable: unbindable name" -> text "t", ofBytes [ byte 'u' ; byte '/' ; 0xFFuy ; 0xFEuy ]
        | "writable: unbindable target" -> ofBytes [ 0xFFuy ; 0xFEuy ], text "w/n"
        | "writable: name of 292 bytes" -> text "t", text ("w/" + String ('a', 292))
        | other -> failwith $"%s{context}: the probe has no SYMPERM row %s{other}"

    let private replayPermissions (envelope : Envelope) : unit =
        let rows = section envelope "SYMPERM"
        rows.Length |> shouldEqual 11

        [
            for row in rows do
                let label = row.[0]
                let target, path = permRow label

                let actual = UnixNamespace.symlink target path (probeCell envelope) |> rendered

                let expected = probeAnswer row.[1]

                if actual <> expected then
                    yield $"%s{label}: the probe answered %s{expected}, this library %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``an unprivileged caller's rows answer as the probe measured, on Linux`` () : unit = replayPermissions linuxUser

    [<Test>]
    let ``an unprivileged caller's rows answer as the probe measured, on Darwin`` () : unit =
        replayPermissions darwinUser

    // ------------------------------------------------------------ SYMMODE and SYMGROUP

    let private modeOf (p : string) (system : UnixSystem<int, string>) : int =
        match UnixPathResolution.stat SymlinkPolicy.NoFollowFinal (text p) system with
        | Ok (FileStatusAnswer.Reported status) -> status.Mode &&& 0o7777
        | other -> failwith $"%s{context}: lstat(%s{p}) did not report: %A{other}"

    let private created (result : Result<SyscallAnswer * UnixSystem<int, string>, SymlinkRefusal>) =
        match result with
        | Ok (SyscallAnswer.Completed 0L, system) -> system
        | other -> failwith $"%s{context}: symlink did not create: %A{other}"

    [<Test>]
    let ``a new link's mode is what the probe measured under the process's umask`` () : unit =
        for envelope in envelopes do
            let rows = section envelope "SYMMODE"
            rows.Length |> shouldEqual 5

            [
                for row in rows do
                    let umask = Convert.ToInt32 (row.[0].Substring "umask=".Length, 8)

                    let mode =
                        Convert.ToInt32 (row.[1].Substring (row.[1].IndexOf "mode=" + "mode=".Length), 8)

                    let system =
                        UnixNamespace.symlink (text "t") (text "n") (boot envelope umask (cell None 0o755))
                        |> created

                    umask, modeOf "n" system, mode
            ]
            |> List.filter (fun (_, actual, expected) -> actual <> expected)
            |> fun mismatches -> (envelope.Label, mismatches) |> shouldEqual (envelope.Label, [])

    [<Test>]
    let ``a new link's group is the directory's or the caller's as the probe measured`` () : unit =
        // Linux as root, in a directory of group 1234; Darwin as uid 501, in a
        // directory of group 12, one of its supplementary groups.
        for envelope in [ linuxRoot ; darwinUser ] do
            let rows = section envelope "SYMGROUP"
            rows.Length |> shouldEqual 2

            for row in rows do
                let setgid = row.[0] = "setgid=1"
                let fields = row.[1].Split ' '

                let field (key : string) : uint32 =
                    fields
                    |> Array.find (fun f -> f.StartsWith (key + "="))
                    |> fun f -> uint32 (f.Substring (key.Length + 1))

                let directoryGroup = GroupId.parseOrFail context (field "dir-gid")
                let seed = cell (Some directoryGroup) (if setgid then 0o2777 else 0o777)

                let system =
                    UnixNamespace.symlink (text "t") (text "e/n") (boot envelope 0o022 seed)
                    |> created

                let group =
                    match UnixPathResolution.stat SymlinkPolicy.NoFollowFinal (text "e/n") system with
                    | Ok (FileStatusAnswer.Reported status) -> GroupId.toUInt32 status.GroupId
                    | other -> failwith $"%s{context}: lstat(e/n) did not report: %A{other}"

                (envelope.Label, row.[0], group)
                |> shouldEqual (envelope.Label, row.[0], field "link-gid")

    // ------------------------------------------------------------ the target, byte for byte

    [<Test>]
    let ``a new link reads back its target byte for byte, and its size is the target's length`` () : unit =
        let targets : Gen<byte list> =
            gen {
                let! length = Gen.choose (1, 300)
                // Any byte but NUL, '/' generously represented.
                let! body = Gen.listOfLength length (Gen.oneof [ Gen.choose (1, 255) ; Gen.constant 47 ])
                return body |> List.map byte
            }

        let property (target : byte list, darwin : bool) : unit =
            let envelope = if darwin then darwinUser else linuxUser

            let system =
                UnixNamespace.symlink (ofBytes target) (text "n") (probeCell envelope)
                |> created

            match UnixNamespace.readlink (text "n") UserBuffer.Mapped 4096 system with
            | Ok (ReadLinkAnswer.Reported written) -> written |> List.ofSeq |> shouldEqual target
            | other -> failwith $"%s{context}: readlink did not report: %A{other}"

            match UnixPathResolution.stat SymlinkPolicy.NoFollowFinal (text "n") system with
            | Ok (FileStatusAnswer.Reported status) -> status.Size |> shouldEqual (int64 target.Length)
            | other -> failwith $"%s{context}: lstat(n) did not report: %A{other}"

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll (Arb.fromGen (Gen.zip targets (ArbMap.defaults |> ArbMap.generate<bool>))) property
        )

    // ------------------------------------------------------------ the syscall step

    [<Test>]
    let ``SymlinkAt through the syscall step is symlinkat`` () : unit =
        for envelope in envelopes do
            let atFdCwd = AtDirectory.atFdCwd (SimulatedUnixPlatform.flavour envelope.Platform)

            for target, path in [ "t", "n" ; "t", "f" ; "t", "nxdir/n" ; "", "n" ] do
                let system = probeCell envelope

                let direct =
                    match UnixNamespace.symlinkat (text target) atFdCwd (text path) system with
                    | Ok (answer, after) -> Ok (SyscallOutcome.Answered answer, after)
                    | Error refusal -> Error (SyscallRefusal.Symlink refusal)

                UnixSystem.step 0 (Syscall.SymlinkAt (text target, atFdCwd, text path)) system
                |> shouldEqual direct
