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

/// `open(2)` takes its flag word raw, in each platform's own numbering, and
/// decodes it as that kernel does. Held three ways:
///
/// - the probe's output (`open-flags.c`, one file per platform), against
///   `OpenFlagWords.table`: every bit the probe saw act is one the table
///   defines, and every number the headers print is the table's;
/// - the decoder against a reference built from that table, over generated
///   words and exhaustively over the modelled bits;
/// - every row the probe measured, replayed through `UnixNamespace.openPath`
///   on a filesystem shaped like the probe's, cell by cell, with the new
///   descriptor's `F_GETFL` and `F_GETFD` words;
/// - and, for every request the parsed entry point accepted before the word
///   was raw, the raw word against that entry point: the same answer and the
///   same system.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestOpenFlagWord =

    let private context : string = "TestOpenFlagWord"

    let private platforms : SimulatedUnixPlatform list =
        [
            SimulatedUnixPlatform.linuxX64
            SimulatedUnixPlatform.linuxArm64
            SimulatedUnixPlatform.macOsArm64
        ]

    /// The probe output measured on `platform`'s kernel.
    let private resourceFor (platform : SimulatedUnixPlatform) : string =
        match SimulatedUnixPlatform.flavour platform, SimulatedUnixPlatform.architecture platform with
        | SimulatedUnixFlavour.Linux, SimulatedUnixArchitecture.X64 -> "linuxX64"
        | SimulatedUnixFlavour.Linux, SimulatedUnixArchitecture.Arm64 -> "linuxArm64"
        | SimulatedUnixFlavour.Darwin, SimulatedUnixArchitecture.Arm64 -> "darwin"
        | flavour, architecture -> failwith $"no probe output for %O{flavour} on %O{architecture}"

    let private probeLines (platform : SimulatedUnixPlatform) : string list =
        let resource = $"WoofWare.PosixKernel.Test.openFlags.%s{resourceFor platform}.txt"

        use stream =
            match Assembly.GetExecutingAssembly().GetManifestResourceStream resource with
            | null -> failwith $"embedded resource %s{resource} not found"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList

    /// One `FLAGS` row: its section, its word, and each column's cell.
    type private ProbeRow =
        {
            Section : string
            Word : int
            Cells : (string * string) list
        }

    let private probeRows (platform : SimulatedUnixPlatform) : ProbeRow list =
        probeLines platform
        |> List.filter (fun line -> line.StartsWith "FLAGS\t")
        |> List.map (fun line ->
            match line.Split '\t' |> Array.toList with
            | _ :: section :: word :: cells ->
                {
                    Section = section
                    Word = Convert.ToUInt32 (word, 16) |> int
                    Cells =
                        cells
                        |> List.map (fun cell ->
                            match cell.Split ('=', 2) with
                            | [| column ; answer |] -> column, answer
                            | _ -> failwith $"malformed cell %s{cell} in %s{line}"
                        )
                }
            | _ -> failwith $"malformed row %s{line}"
        )

    // ------------------------------------------------- the table against the probe

    /// Bits the flavour defines that a single-bit row cannot see act: `O_EXCL`
    /// does nothing without `O_CREAT`, `O_NOCTTY` nothing on a file that is
    /// not a terminal, Linux's `O_LARGEFILE` is set on every 64-bit
    /// description already, Darwin's `O_SYMLINK` opens the link itself, which
    /// none of the probe's columns tells from its target, and `O_POPUP` is in
    /// the headers but nothing the probe looked at moved.
    let private invisibleAlone : Set<string> =
        Set.ofList [ "O_EXCL" ; "O_NOCTTY" ; "O_LARGEFILE" ; "O_SYMLINK" ; "O_POPUP" ]

    [<Test>]
    let ``every bit the probe saw act alone is one the table defines`` () : unit =
        for platform in platforms do
            let single =
                probeRows platform
                |> List.filter (fun row -> row.Section = "single")
                |> List.map (fun row -> row.Word, row.Cells)
                |> Map.ofList

            let table = OpenFlagWords.table platform

            let mismatches =
                [
                    for b in 2..31 do
                        let bit = 1 <<< b

                        let acts =
                            [ 0..3 ]
                            |> List.exists (fun access -> single.[access ||| bit] <> single.[access])

                        match table |> List.tryFind (fun (defined, _, _) -> defined = bit) with
                        | Some (_, name, _) when not acts && not (invisibleAlone.Contains name) ->
                            yield $"%O{platform}: %s{name} (0x%x{bit}) is defined, but the probe saw it do nothing"
                        | Some (_, name, _) when acts && invisibleAlone.Contains name ->
                            yield $"%O{platform}: %s{name} (0x%x{bit}) acted, but is listed as invisible alone"
                        | None when acts -> yield $"%O{platform}: 0x%x{bit} acted, and the table does not define it"
                        | _ -> ()
                ]

            mismatches |> shouldEqual []

    [<Test>]
    let ``the table numbers each flag as the probe's headers do`` () : unit =
        for platform in platforms do
            let table = OpenFlagWords.table platform

            let named (name : string) : int option =
                table |> List.tryPick (fun (bit, n, _) -> if n = name then Some bit else None)

            let headers =
                probeLines platform
                |> List.filter (fun line -> line.StartsWith "HEADER\t")
                |> List.map (fun line ->
                    match line.Split '\t' with
                    | [| _ ; name ; value |] -> name, Convert.ToUInt32 (value, 16) |> int
                    | _ -> failwith $"malformed header %s{line}"
                )

            let checkedCount =
                headers
                |> List.sumBy (fun (name, value) ->
                    let expected =
                        match name with
                        // Composites of bits the table names one at a time.
                        | "O_SYNC" when SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux ->
                            Some (
                                OpenFlagWords.bitOf platform OpenFlagBit.Synchronous
                                ||| OpenFlagWords.bitOf platform OpenFlagBit.DataSynchronous
                            )
                        | "O_TMPFILE" ->
                            Some (
                                OpenFlagWords.bitOf platform (OpenFlagBit.Unmodelled UnmodelledOpenFlag.TemporaryFile)
                                ||| OpenFlagWords.bitOf platform OpenFlagBit.Directory
                            )
                        // glibc's 64-bit headers define O_LARGEFILE as 0; the
                        // kernel's number is the one the table holds.
                        | "O_LARGEFILE" -> None
                        | _ -> named name

                    match expected with
                    | Some expected ->
                        (name, value) |> shouldEqual (name, expected)
                        1
                    | None -> 0
                )

            checkedCount |> shouldBeGreaterThan 9

    // ------------------------------------------------------ the decoder's reference

    /// What `platform`'s kernel does with `word`, by the table: refused if it
    /// holds a bit the library does not model, then the screens the probe saw
    /// ahead of the path, then the decoded request.
    let private reference (platform : SimulatedUnixPlatform) (word : int) : OpenFlagWord.Decoding =
        let present =
            OpenFlagWords.table platform
            |> List.filter (fun (bit, _, _) -> word &&& bit <> 0)
            |> List.sortBy (fun (bit, _, _) -> uint32 bit)
            |> List.map (fun (_, _, meaning) -> meaning)

        let has (meaning : OpenFlagBit) = List.contains meaning present

        let unmodelled =
            present
            |> List.choose (fun meaning ->
                match meaning with
                | OpenFlagBit.Unmodelled flag -> Some flag
                | OpenFlagBit.DataSynchronous when not (has OpenFlagBit.Synchronous) ->
                    Some UnmodelledOpenFlag.DataSynchronous
                | _ -> None
            )

        if not unmodelled.IsEmpty then
            OpenFlagWord.Decoding.Refused (OpenRefusal.UnmodelledFlags (word, unmodelled))
        elif has OpenFlagBit.Create && has OpenFlagBit.Directory then
            OpenFlagWord.Decoding.Fails UnixError.EINVAL
        elif word &&& 3 = 3 then
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Darwin -> OpenFlagWord.Decoding.Fails UnixError.EINVAL
            | SimulatedUnixFlavour.Linux -> OpenFlagWord.Decoding.Refused (OpenRefusal.IoctlOnlyAccessMode word)
        else

        let flags : OpenFlags =
            {
                Access =
                    match word &&& 3 with
                    | 0 -> FileAccessMode.ReadOnly
                    | 1 -> FileAccessMode.WriteOnly
                    | _ -> FileAccessMode.ReadWrite
                Create = has OpenFlagBit.Create
                Exclusive = has OpenFlagBit.Exclusive
                Truncate = has OpenFlagBit.Truncate
                NoFollow = has OpenFlagBit.NoFollow
                CloseOnExec = has OpenFlagBit.CloseOnExec
                Synchronous = has OpenFlagBit.Synchronous
                DataSynchronous =
                    match SimulatedUnixPlatform.flavour platform with
                    | SimulatedUnixFlavour.Linux -> has OpenFlagBit.Synchronous
                    | SimulatedUnixFlavour.Darwin -> has OpenFlagBit.DataSynchronous
                Directory = has OpenFlagBit.Directory
            }

        if
            flags.Directory
            && (flags.Access <> FileAccessMode.ReadOnly || flags.Truncate || flags.NoFollow)
        then
            OpenFlagWord.Decoding.Refused (OpenRefusal.UnmodelledDirectoryOpen word)
        else
            OpenFlagWord.Decoding.Decoded flags

    /// A word mostly of bits `platform` defines, so that every rule is reached,
    /// with now and then an undefined one.
    let private wordGen (platform : SimulatedUnixPlatform) : Gen<int> =
        let table = OpenFlagWords.table platform

        let structured =
            gen {
                let! access = Gen.choose (0, 3)
                let! density = Gen.elements [ 2 ; 4 ; 8 ; 16 ]

                let! defined =
                    table
                    |> List.map (fun (bit, _, _) ->
                        Gen.choose (1, density) |> Gen.map (fun roll -> if roll = 1 then bit else 0)
                    )
                    |> Gen.sequenceToList

                let! noise = Gen.elements [ 0 ; 0 ; 0 ; 1 <<< 23 ; 1 <<< 30 ; 0x4 ; 0x1000 ]
                return access ||| List.fold (|||) 0 defined ||| noise
            }

        Gen.oneof [ structured ; Gen.choose (Int32.MinValue, Int32.MaxValue) ]

    [<Test>]
    let ``the decoder reads every word as the table says`` () : unit =
        for platform in platforms do
            let property =
                Prop.forAll
                    (Arb.fromGen (wordGen platform))
                    (fun word ->
                        OpenFlagWord.decode platform word = reference platform word
                        |> Prop.label $"%O{platform} 0x%x{word}"
                    )

            Check.One (Config.QuickThrowOnFailure.WithMaxTest 20000, property)

    [<Test>]
    let ``the decoder reads every combination of the bits it models as the table says`` () : unit =
        for platform in platforms do
            let modelled =
                OpenFlagWords.table platform
                |> List.choose (fun (bit, _, meaning) ->
                    match meaning with
                    | OpenFlagBit.Unmodelled _ -> None
                    | _ -> Some bit
                )

            let words =
                modelled
                |> List.fold (fun words bit -> words @ (words |> List.map (fun word -> word ||| bit))) [ 0 ; 1 ; 2 ; 3 ]

            words.Length |> shouldEqual (4 <<< modelled.Length)

            for word in words do
                (word, OpenFlagWord.decode platform word)
                |> shouldEqual (word, reference platform word)

    // --------------------------------------------------------- the probe's rows

    let private name (text : string) : DirectoryEntryName =
        DirectoryEntryName.parseOrFail context text

    /// Who ran the probe on `platform`, and so owns its files: root on Linux,
    /// uid 501 on Darwin.
    let private probeOwner (platform : SimulatedUnixPlatform) : Credentials =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> Owners.root
        | SimulatedUnixFlavour.Darwin ->
            Credentials.ofIds (UserId.parseOrFail context 501u) (GroupId.parseOrFail context 20u) []

    /// `probeSystem`, with the process running as `caller` rather than as the
    /// user who owns the files.
    let private probeSystemAs (caller : Credentials) (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        let credentials = probeOwner platform

        let bits (mode : int) = PermissionBits.parseOrFail context mode

        let probeDirectory =
            Map.ofList
                [
                    name "f", SeedEntry.File (ImmutableArray.CreateRange "abc"B, bits 0o644, None)
                    name "d", SeedEntry.Directory (Map.empty, bits 0o755, None)
                    name "l", SeedEntry.Symlink (SymlinkTarget.parseOrFail context "f", None)
                    name "g", SeedEntry.File (ImmutableArray<byte>.Empty, bits 0o644, None)
                    name "h", SeedEntry.File (ImmutableArray<byte>.Empty, bits 0o644, None)
                ]

        let seed =
            Map.ofList
                [
                    name "w",
                    SeedEntry.Directory (
                        Map.ofList [ name "p", SeedEntry.Directory (probeDirectory, bits 0o755, None) ],
                        bits 0o755,
                        None
                    )
                ]

        let system : UnixBootImage<int, string> = UnixSystem.initial platform

        match
            UnixBootImage.withFileSystem
                (UnixTimestamp.ofMillisecondsSinceEpoch 0L)
                (InodeOwner.ofProcess credentials)
                seed
                system
        with
        | Ok image ->
            (Launched.bootWith
                (Launched.credentials caller
                 >> ProcessLaunch.withCurrentDirectory (AbsoluteUnixPath.parseOrFail context "/w/p"))
                UnixSystem.pipedStandardStreams
                0
                (CpuId 0))
                image
        | Error fault -> failwith $"could not build the system: %A{fault}"

    /// The probe's directory, `/w/p` here: `f` holding "abc" (0644), `d`
    /// (0755), `l -> f`, and `g` and `h`, two links to one empty file on the
    /// real kernels and two files here. Only `O_UNIQUE`, which the library
    /// refuses, reads a link count.
    let private probeSystem (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        probeSystemAs (probeOwner platform) platform

    let private argument (column : string) : PathArgumentBytes =
        match column with
        | "NULL" -> PathArgumentBytes.Unreadable
        | "abs" -> PathArg.ofText "/w/p/f"
        | "up" -> PathArg.ofText "../p/f"
        | other -> PathArg.ofText other

    /// What became of `f` and `nx`, in the probe's notation.
    let private aftermath (system : UnixSystem<int, string>) : string =
        let size (path : string) : int64 option =
            match UnixPathResolution.stat SymlinkPolicy.NoFollowFinal (PathArg.ofText path) system with
            | Ok (FileStatusAnswer.Reported status) -> Some status.Size
            | Ok (FileStatusAnswer.Failed _) -> None
            | Error refusal -> failwith $"stat %s{path} was refused: %A{refusal}"

        let truncated = if size "/w/p/f" = Some 3L then "" else ",trunc"
        let created = if (size "/w/p/nx").IsSome then ",created" else ""
        truncated + created

    /// What `fcntl(fd, command)` answers, as the probe prints it.
    let private flagWord (fd : int) (command : int) (system : UnixSystem<int, string>) : string =
        match UnixDescriptor.fcntl fd command 0 system with
        | Ok (SyscallAnswer.Completed word, _) -> $"0x%x{word}"
        | Ok (SyscallAnswer.Failed error, _) -> $"%A{error}"
        | Error refusal -> $"refused (%s{FcntlRefusal.describe refusal})"

    [<Test>]
    let ``every row the probe measured is answered as measured`` () : unit =
        for platform in platforms do
            let system = probeSystem platform
            let rows = probeRows platform
            rows.Length |> shouldBeGreaterThan 4000

            let mutable replayed = 0

            let mismatches =
                [
                    for row in rows do
                        match reference platform row.Word with
                        | OpenFlagWord.Decoding.Refused _ ->
                            // Each refused cell must be a refusal, whatever the path.
                            for column, _ in row.Cells do
                                match UnixNamespace.openPath row.Word (argument column) 0o644 system with
                                | Error _ -> ()
                                | Ok answer ->
                                    yield
                                        $"%O{platform} 0x%08x{row.Word} %s{column}: refused by the table, answered %A{fst answer}"
                        | OpenFlagWord.Decoding.Fails _
                        | OpenFlagWord.Decoding.Decoded _ ->
                            for column, measured in row.Cells do
                                replayed <- replayed + 1

                                let modelled =
                                    match UnixNamespace.openPath row.Word (argument column) 0o644 system with
                                    | Ok (SyscallAnswer.Failed error, _) -> $"%A{error}"
                                    | Ok (SyscallAnswer.Completed fd, after) ->
                                        // F_GETFL (3) and F_GETFD (1), which
                                        // the probe printed as it closed the
                                        // descriptor.
                                        let fd = int fd

                                        $"ok/%s{flagWord fd 3 after}/%s{flagWord fd 1 after}" + aftermath after
                                    | Error refusal -> $"refused (%s{OpenRefusal.describe refusal})"

                                if modelled <> measured then
                                    yield
                                        $"%O{platform} %s{row.Section} 0x%08x{row.Word} %s{column}: measured %s{measured}, modelled %s{modelled}"
                ]

            mismatches |> List.truncate 30 |> shouldEqual []
            // The rows the library answers, as opposed to refuses, must be
            // most of what the probe swept over the bits it models.
            replayed |> shouldBeGreaterThan 4000

    // ---------------------------------------------- against the parsed entry point

    /// Every request the parsed entry point took, on `platform`: Linux's
    /// `O_SYNC` always carries `O_DSYNC`, and Darwin's may or may not.
    let private everyRequest (platform : SimulatedUnixPlatform) : OpenFlags list =
        let words =
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Linux -> 127
            | SimulatedUnixFlavour.Darwin -> 255

        [
            for access in
                [
                    FileAccessMode.ReadOnly
                    FileAccessMode.WriteOnly
                    FileAccessMode.ReadWrite
                ] do
                for bits in 0..words do
                    let set (i : int) = bits &&& (1 <<< i) <> 0

                    // Darwin's decoder admits O_DSYNC only beside O_SYNC.
                    if not (set 7) || set 5 then
                        yield
                            {
                                Access = access
                                Create = set 0
                                Exclusive = set 1
                                Truncate = set 2
                                NoFollow = set 3
                                CloseOnExec = set 4
                                Synchronous = set 5
                                DataSynchronous =
                                    match SimulatedUnixPlatform.flavour platform with
                                    | SimulatedUnixFlavour.Linux -> set 5
                                    | SimulatedUnixFlavour.Darwin -> set 7
                                Directory = set 6
                            }
        ]

    /// Whether the parsed entry point answered `flags` rather than throwing:
    /// it took `O_DIRECTORY` only alone with `O_RDONLY`.
    let private parsedEntryPointAccepted (flags : OpenFlags) : bool =
        not (
            flags.Directory
            && (flags.Access <> FileAccessMode.ReadOnly
                || flags.Create
                || flags.Truncate
                || flags.NoFollow)
        )

    /// The entry point as it was when it took parsed flags: copy the path in,
    /// then open it.
    let private parsedEntryPoint
        (flags : OpenFlags)
        (path : PathArgumentBytes)
        (system : UnixSystem<int, string>)
        : Result<SyscallAnswer * UnixSystem<int, string>, OpenRefusal>
        =
        match UnixPathResolution.copyIn path system with
        | Error error -> Ok (SyscallAnswer.Failed error, system)
        | Ok path -> UnixNamespace.openPathParsed AtDirectory.CurrentDirectory flags path 0o644 system

    [<Test>]
    let ``every request the parsed entry point took decodes from its word`` () : unit =
        for platform in platforms do
            for flags in everyRequest platform |> List.filter parsedEntryPointAccepted do
                OpenFlagWord.decode platform (OpenFlagWords.encode platform flags)
                |> shouldEqual (OpenFlagWord.Decoding.Decoded flags)

    [<Test>]
    let ``the raw word is answered exactly as the parsed entry point answered its request`` () : unit =
        let columns =
            [ "NULL" ; "nx" ; "f" ; "d" ; "l" ; "abs" ; "up" ; "f/" ; "d/" ; "nx/" ; "" ]

        let stranger =
            Credentials.ofIds (UserId.parseOrFail context 4242u) (GroupId.parseOrFail context 4242u) []

        let mutable compared = 0

        for platform in platforms do
            let owner = probeSystem platform
            // Someone the files do not belong to, for whom the permission
            // bits decide.
            let other = probeSystemAs stranger platform

            for system in [ owner ; other ] do
                for flags in everyRequest platform |> List.filter parsedEntryPointAccepted do
                    let word = OpenFlagWords.encode platform flags

                    for column in columns do
                        let path = argument column
                        compared <- compared + 1

                        (column, flags, UnixNamespace.openPath word path 0o644 system)
                        |> shouldEqual (column, flags, parsedEntryPoint flags path system)

        compared |> shouldBeGreaterThan 10000
