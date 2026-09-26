namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Text
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// A path is bytes, not text: every syscall that hands a name back hands back
/// the bytes it was given, and the rules that measure a name measure what the
/// platform measures.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixPathBytes =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private linux : UnixSystem<int, string> =
        UnixSystem.initial SimulatedUnixPlatform.linuxX64

    let private darwin : UnixSystem<int, string> =
        UnixSystem.initial SimulatedUnixPlatform.macOsArm64

    let private byteString (bytes : byte seq) : UnixByteString =
        match UnixByteString.ofBytes (ImmutableArray.CreateRange bytes) with
        | Ok value -> value
        | Error defect -> failwith $"test bytes %s{UnixByteString.describe defect}"

    let private pathOf (bytes : byte seq) : UnixPath =
        UnixPath.ofByteString (byteString bytes)

    let private nameOf (bytes : byte seq) : DirectoryEntryName =
        match DirectoryEntryName.ofByteString (byteString bytes) with
        | Ok name -> name
        | Error error -> failwith $"test name: %s{DirectoryEntryName.describe error}"

    let private completed (answer : SyscallAnswer * UnixSystem<int, string>) : UnixSystem<int, string> =
        match answer with
        | SyscallAnswer.Completed 0L, system -> system
        | other -> failwith $"expected a success, got %A{other}"

    let private slash : byte = UnixPathText.separatorByte

    // ------------------------------------------------ names come back as bytes

    [<Test>]
    let ``getcwd reports a directory name that is not UTF-8 byte for byte`` () : unit =
        let directory = [ slash ; 0xFFuy ; 0xFEuy ]

        let system =
            linux
            |> UnixNamespace.mkdir (pathOf directory) 0o777
            |> completed
            |> UnixPathResolution.chdir (pathOf directory)
            |> completed

        match UnixPathResolution.getcwd UserBuffer.Mapped 4096UL system with
        | Ok (GetCwdAnswer.Reported bytes) -> List.ofSeq bytes |> shouldEqual (directory @ [ 0uy ])
        | other -> failwith $"expected a path, got %A{other}"

    [<Test>]
    let ``readdir hands back names that are not UTF-8 byte for byte`` () : unit =
        // `E4 B8` is the first two bytes of a three-byte character: not UTF-8,
        // and a name Linux is measured to hand back unchanged.
        let names = [ [ 0xFFuy ] ; [ 0xE4uy ; 0xB8uy ] ]
        let parent = [ slash ; byte 'd' ]

        let system =
            names
            |> List.fold
                (fun system name ->
                    UnixNamespace.mkdir (pathOf (parent @ [ slash ] @ name)) 0o777 system
                    |> completed
                )
                (UnixNamespace.mkdir (pathOf parent) 0o777 linux |> completed)

        let stream, system =
            match UnixNamespace.opendir (pathOf parent) system with
            | OpenDirAnswer.Opened stream, system -> stream, system
            | other -> failwith $"expected a stream, got %A{other}"

        let rec drain (system : UnixSystem<int, string>) (acc : byte list list) : byte list list =
            match UnixNamespace.readdir stream system with
            | ReadDirAnswer.EndOfStream, _ -> acc
            | ReadDirAnswer.Entry (name, _), system -> drain system (List.ofSeq name :: acc)
            | ReadDirAnswer.Failed error, _ -> failwith $"readdir failed with %O{error}"

        drain system []
        |> List.sort
        |> shouldEqual (List.sort ([ [ byte '.' ] ; [ byte '.' ; byte '.' ] ] @ names))

    // ------------------------------------------------------------- NAME_MAX

    /// Plan §1.5's bisection: each unit, repeated, and the count at which a
    /// Darwin lookup flips to ENAMETOOLONG.
    let private darwinFlips : (byte list * int) list =
        [
            [ byte 'a' ], 256
            [ 0xE4uy ; 0xB8uy ; 0xADuy ], 256
            // The row that proves a unit count exists: 512 bytes is nowhere near
            // any byte cap, so what binds it counts UTF-16 code units.
            [ 0xF0uy ; 0x9Fuy ; 0x98uy ; 0x80uy ], 128
            [ 0xEDuy ; 0xA0uy ; 0x80uy ], 256
            [ 0xE0uy ; 0x80uy ; 0x81uy ], 256
            [ 0xF5uy ; 0x80uy ; 0x80uy ; 0x80uy ], 192
            [ 0xC0uy ; 0x80uy ], 383
            [ 0xE4uy ; 0xB8uy ], 383
            [ 0xFFuy ], 766
            [ 0x80uy ], 766
        ]

    let private repeated (unit : byte list) (count : int) : DirectoryEntryName =
        nameOf (List.replicate count unit |> List.concat)

    [<Test>]
    let ``Darwin's NAME_MAX flips exactly where it was measured to`` () : unit =
        let limits = SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.macOsArm64

        for unit, flip in darwinFlips do
            PathLimits.nameWithinLimit limits (repeated unit (flip - 1)) |> shouldEqual true
            PathLimits.nameWithinLimit limits (repeated unit flip) |> shouldEqual false

    [<Test>]
    let ``Darwin counts strictly-valid UTF-8 in code units and anything else in bytes`` () : unit =
        // A three-byte unit cannot tell 255 code units from 765 bytes, since both
        // flip at the same count. 300 ASCII characters in front break the tie:
        // over the unit cap, far under the byte cap.
        let limits = SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.macOsArm64
        let prefix = List.replicate 300 (byte 'a')

        let countedInUnits =
            [
                [ 0xE4uy ; 0xB8uy ; 0xADuy ]
                [ 0xF0uy ; 0x9Fuy ; 0x98uy ; 0x80uy ]
                // U+FFFF: valid UTF-8, so measured in units, whatever APFS then
                // thinks of binding it.
                [ 0xEFuy ; 0xBFuy ; 0xBFuy ]
            ]

        let countedInBytes =
            [
                [ 0xEDuy ; 0xA0uy ; 0x80uy ]
                [ 0xE0uy ; 0x80uy ; 0x81uy ]
                [ 0xC0uy ; 0x80uy ]
                [ 0xF5uy ; 0x80uy ; 0x80uy ; 0x80uy ]
                [ 0xFFuy ]
                [ 0xE4uy ; 0xB8uy ]
            ]

        for suffix in countedInUnits do
            PathLimits.nameWithinLimit limits (nameOf (prefix @ suffix))
            |> shouldEqual false

        for suffix in countedInBytes do
            PathLimits.nameWithinLimit limits (nameOf (prefix @ suffix)) |> shouldEqual true

    [<Test>]
    let ``Linux counts every name in bytes`` () : unit =
        let limits = SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.linuxX64

        PathLimits.nameWithinLimit limits (repeated [ 0xFFuy ] 255) |> shouldEqual true
        PathLimits.nameWithinLimit limits (repeated [ 0xFFuy ] 256) |> shouldEqual false

    // ------------------------------------------- binding a name that is not UTF-8

    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L

    /// `system` with its filesystem replaced by `seed`, and the root as its
    /// current directory. There is no `symlink` syscall, so seeding is how a
    /// link comes to exist.
    let private seeded (seed : (DirectoryEntryName * SeedEntry) list) (system : UnixSystem<int, string>) =
        match UnixSystem.withFileSystemAndCurrentDirectory epoch (Map.ofList seed) AbsoluteUnixPath.root system with
        | Ok system -> system
        | Error fault -> failwith $"seeding failed: %A{fault}"

    let private text : string -> byte list = BindingProbes.text

    [<Test>]
    let ``Darwin refuses to bind a name that is not UTF-8, after every other refusal`` () : unit =
        // Run as `UnixSystem.initial`'s unprivileged caller, as the probes were.
        let system = seeded (Map.toList BindingProbes.tree) darwin

        for description, call, expected in BindingProbes.rows do
            let actual = BindingProbes.runModel call system

            if actual <> expected then
                failwith $"Darwin %s{description}: expected %A{expected}, got %A{actual}"

    [<Test>]
    let ``nothing is bound inside a removed directory, whatever the name`` () : unit =
        // Measured on Darwin by `darwin-eilseq-is-last.c`: the orphan's ENOENT
        // beats the encoding for all three binding calls. Not a row of
        // `BindingProbes`, because it needs a current directory the host test
        // cannot give its own process.
        let orphaned =
            seeded (Map.toList BindingProbes.tree) darwin
            |> UnixNamespace.mkdir (pathOf (text "/gone")) 0o777
            |> completed
            |> UnixPathResolution.chdir (pathOf (text "/gone"))
            |> completed
            |> UnixNamespace.rmdir (pathOf (text "../gone"))
            |> completed

        let creating : OpenFlags =
            {
                Access = FileAccessMode.WriteOnly
                Create = true
                Exclusive = false
                Truncate = false
                NoFollow = false
                CloseOnExec = false
                Synchronous = false
                Directory = false
            }

        for name in [ text "g" ; [ 0xFFuy ] ] do
            fst (UnixNamespace.mkdir (pathOf name) 0o777 orphaned)
            |> shouldEqual (SyscallAnswer.Failed UnixError.ENOENT)

            fst (UnixNamespace.openPath creating (pathOf name) 0o666 orphaned)
            |> shouldEqual (SyscallAnswer.Failed UnixError.ENOENT)

            match
                UnixNamespace.rename
                    (PathArgumentBytes.Bytes (ImmutableArray.CreateRange (text "/d/f")))
                    (PathArgumentBytes.Bytes (ImmutableArray.CreateRange name))
                    orphaned
            with
            | Ok (answer, _) -> answer |> shouldEqual (SyscallAnswer.Failed UnixError.ENOENT)
            | Error refusal -> failwith $"rename refused its arguments: %A{refusal}"

    /// A name made of units that mix valid UTF-8 with sequences that are not,
    /// with no separator, no NUL, and never "." or "..".
    let private nameBytesGen : Gen<byte list> =
        Gen.elements
            [
                text "a"
                text "é"
                text "中"
                text "😀"
                [ 0xFFuy ]
                [ 0x80uy ]
                [ 0xE4uy ; 0xB8uy ]
                [ 0xC0uy ; 0x80uy ]
                [ 0xEDuy ; 0xA0uy ; 0x80uy ]
            ]
        |> Gen.nonEmptyListOf
        |> Gen.map List.concat

    /// The three ways to bind a free name in the writable root.
    let private bindings (name : byte list) : (string * BindingProbeCall) list =
        [
            "mkdir", BindingProbeCall.Mkdir name
            "open(O_CREAT)", BindingProbeCall.OpenCreate name
            "rename", BindingProbeCall.Rename (text "f", name)
        ]

    let private withFile (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        seeded
            [
                DirectoryEntryName.parseOrFail "test" "f", SeedEntry.file ImmutableArray.Empty
            ]
            system

    [<Test>]
    let ``Linux never answers EILSEQ`` () : unit =
        let system = withFile linux

        let property (name : byte list) : unit =
            for operation, call in bindings name do
                match BindingProbes.runModel call system with
                | Some UnixError.EILSEQ -> failwith $"Linux %s{operation} answered EILSEQ"
                | Some UnixError.ENAMETOOLONG when name.Length > 255 -> ()
                | None when name.Length <= 255 -> ()
                | other -> failwith $"Linux %s{operation} of a %d{name.Length}-byte name answered %A{other}"

        Check.One (config, Prop.forAll (Arb.fromGen nameBytesGen) property)

    [<Test>]
    let ``Darwin refuses a binding every earlier rule permits iff the name is not UTF-8`` () : unit =
        // The model's rule, not APFS's: strictly-valid UTF-8 over-admits (plan
        // §1.1.1, and the test below). The precondition is not decoration --
        // EILSEQ is the last check, so an over-long or unwritable case answers
        // something else whatever the bytes.
        let system = withFile darwin
        let limits = SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.macOsArm64

        let property (bytes : byte list) : unit =
            let name = nameOf bytes

            if PathLimits.nameWithinLimit limits name then
                let expected =
                    match DirectoryEntryName.tryToString name with
                    | Some _ -> None
                    | None -> Some UnixError.EILSEQ

                for operation, call in bindings bytes do
                    let actual = BindingProbes.runModel call system

                    if actual <> expected then
                        failwith
                            $"Darwin %s{operation} of \"%s{DirectoryEntryName.toEscaped name}\": expected %A{expected}, got %A{actual}"

        Check.One (config, Prop.forAll (Arb.fromGen nameBytesGen) property)

    [<Test>]
    let ``Darwin binds the valid UTF-8 that APFS refuses, by choice`` () : unit =
        // Plan §1.1.1: the model admits every strictly-valid UTF-8 name, and
        // APFS refuses these three classes of them. Pinned so that a change to
        // either side is a decision rather than an accident; a faithful model
        // (a `BindableEntryNames.AppleUnicode` case) is what would flip them.
        let system = withFile darwin

        for description, name in BindingProbes.overAdmitted do
            for operation, call in bindings name do
                match BindingProbes.runModel call system with
                | None -> ()
                | other -> failwith $"Darwin %s{operation} of %s{description}: expected success, got %A{other}"

    // -------------------------------------------------------- symlink targets

    let private targetOf (bytes : byte seq) : SymlinkTarget =
        match SymlinkTarget.ofByteString (byteString bytes) with
        | Ok target -> target
        | Error error -> failwith $"test target: %s{SymlinkTarget.describe error}"

    /// Non-empty, NUL-free, and mixing valid UTF-8 with bytes that are not.
    let private targetBytesGen : Gen<byte list> =
        Gen.frequency
            [
                3, Gen.elements [ slash ; byte '.' ; byte 'a' ]
                2, (ArbMap.defaults |> ArbMap.generate<byte> |> Gen.filter (fun b -> b <> 0uy))
                1, Gen.constant 0xE4uy
                1, Gen.constant 0xB8uy
            ]
        |> Gen.nonEmptyListOf

    [<Test>]
    let ``readlink and lstat report a seeded target byte for byte`` () : unit =
        let property (bytes : byte list) : unit =
            for system in [ linux ; darwin ] do
                let system =
                    seeded [ nameOf [ byte 'l' ], SeedEntry.Symlink (targetOf bytes) ] system

                match UnixNamespace.readlink (pathOf [ slash ; byte 'l' ]) UserBuffer.Mapped 8192 system with
                | Ok (ReadLinkAnswer.Reported reported) -> List.ofSeq reported |> shouldEqual bytes
                | other -> failwith $"expected a target, got %A{other}"

                match UnixPathResolution.stat SymlinkPolicy.NoFollowFinal (pathOf [ slash ; byte 'l' ]) system with
                | Ok (FileStatusAnswer.Reported status) -> status.Size |> shouldEqual (int64 bytes.Length)
                | other -> failwith $"expected a status, got %A{other}"

        Check.One (config, Prop.forAll (Arb.fromGen targetBytesGen) property)

    [<Test>]
    let ``a walk through a link follows the target's bytes`` () : unit =
        // A directory named 0xFF and a link to it: only a walk that splices the
        // target's own bytes, rather than some rendering of them, arrives.
        let system =
            seeded
                [
                    nameOf [ 0xFFuy ], SeedEntry.directory Map.empty
                    nameOf [ byte 'l' ], SeedEntry.Symlink (targetOf [ 0xFFuy ])
                ]
                linux

        match UnixPathResolution.stat SymlinkPolicy.Follow (pathOf [ slash ; byte 'l' ; slash ; byte '.' ]) system with
        | Ok (FileStatusAnswer.Reported status) -> status.Mode &&& 0o170000 |> shouldEqual 0o040000
        | other -> failwith $"expected the directory, got %A{other}"

    [<Test>]
    let ``Darwin's splice limit counts a target's bytes, not what it would decode to`` () : unit =
        // Darwin refuses a splice when target + remainder + NUL exceeds 1024
        // bytes. Through "/l/a" the remainder is "/a", so a 1021-byte target
        // fits and a 1022-byte one does not. `E4 B8` is two bytes that a lenient
        // decode makes into one character, and an escaped rendering into eight,
        // so a count taken from either lands on the wrong side.
        let target (length : int) : byte list =
            let body = List.replicate 300 [ 0xE4uy ; 0xB8uy ; slash ] |> List.concat
            body @ List.replicate (length - body.Length) (byte 'a')

        let lookup (length : int) : Result<FileStatusAnswer, StatRefusal> =
            let system =
                seeded [ nameOf [ byte 'l' ], SeedEntry.Symlink (targetOf (target length)) ] darwin

            UnixPathResolution.stat SymlinkPolicy.Follow (pathOf [ slash ; byte 'l' ; slash ; byte 'a' ]) system

        // Fits, so the walk proceeds into the target, whose first component
        // does not exist.
        lookup 1021 |> shouldEqual (Ok (FileStatusAnswer.Failed UnixError.ENOENT))
        lookup 1022 |> shouldEqual (Ok (FileStatusAnswer.Failed UnixError.ENAMETOOLONG))

    // ------------------------------------------------------------ PathCursor

    /// Bytes weighted towards the ones the walk treats specially.
    let private pathBytesGen : Gen<byte list> =
        Gen.frequency
            [
                4, Gen.constant slash
                3, Gen.constant (byte '.')
                3, Gen.elements [ byte 'a' ; 0xFFuy ; 0xE4uy ; 0xB8uy ; 0x80uy ]
            ]
        |> Gen.listOf

    /// The reference: split on the separator, drop empty segments, and read
    /// "." and ".." as themselves.
    let private referenceComponents (bytes : byte list) : PathComponent list =
        let rec split (current : byte list) (acc : byte list list) (rest : byte list) : byte list list =
            match rest with
            | [] -> List.rev (List.rev current :: acc)
            | b :: rest when b = slash -> split [] (List.rev current :: acc) rest
            | b :: rest -> split (b :: current) acc rest

        split [] [] bytes
        |> List.filter (not << List.isEmpty)
        |> List.map (fun segment ->
            match segment with
            | [ 46uy ] -> PathComponent.Current
            | [ 46uy ; 46uy ] -> PathComponent.Parent
            | name -> PathComponent.Name (nameOf name)
        )

    [<Test>]
    let ``the cursor walks bytes into the components a reference split gives`` () : unit =
        let property (bytes : byte list) : unit =
            UnixPath.components (pathOf bytes) |> shouldEqual (referenceComponents bytes)

        Check.One (config, Prop.forAll (Arb.fromGen pathBytesGen) property)

    [<Test>]
    let ``remainingBytes is the literal count of bytes the walk has not passed`` () : unit =
        let property (bytes : byte list) : unit =
            let start = PathCursor.ofPath (pathOf bytes)
            PathCursor.remainingBytes start |> shouldEqual bytes.Length

            let rec walk (cursor : PathCursor) : unit =
                match PathCursor.next cursor with
                | None -> ()
                | Some (_, next) ->
                    // Never grows, and never counts a byte twice.
                    PathCursor.remainingBytes next
                    |> shouldBeSmallerThan (PathCursor.remainingBytes cursor)

                    walk next

            walk start

        Check.One (config, Prop.forAll (Arb.fromGen pathBytesGen) property)

    // ------------------------------------------- the recorded pre-byte table

    let private unescape (text : string) : string =
        let builder = StringBuilder ()
        let mutable i = 0

        while i < text.Length do
            if text.[i] = '\\' then
                match text.[i + 1] with
                | '\\' -> builder.Append '\\' |> ignore<StringBuilder>
                | 't' -> builder.Append '\t' |> ignore<StringBuilder>
                | 'n' -> builder.Append '\n' |> ignore<StringBuilder>
                | 'r' -> builder.Append '\r' |> ignore<StringBuilder>
                | '0' -> builder.Append '\000' |> ignore<StringBuilder>
                | other -> failwith $"recorded.tsv: unknown escape \\%c{other}"

                i <- i + 2
            else
                builder.Append text.[i] |> ignore<StringBuilder>
                i <- i + 1

        builder.ToString ()

    let private recorded : (string * string * string) list =
        let assembly = Assembly.GetExecutingAssembly ()

        let name =
            assembly.GetManifestResourceNames ()
            |> Array.find (fun name -> name.EndsWith "recorded.tsv")

        use stream = assembly.GetManifestResourceStream name
        use reader = new StreamReader (stream, UTF8Encoding (false, true))

        reader.ReadToEnd().Split '\n'
        |> Array.filter (fun line -> line <> "")
        |> Array.map (fun line ->
            match line.Split ('\t', 3) with
            | [| flavour ; outcome ; input |] -> flavour, outcome, unescape input
            | _ -> failwith $"recorded.tsv: malformed row %s{line}"
        )
        |> List.ofArray

    [<Test>]
    let ``PathArgument.parse agrees with the table recorded before paths were bytes`` () : unit =
        // Every input is valid UTF-8, the domain the old decoding parse could
        // answer; there, nothing should have changed.
        recorded.Length |> shouldBeGreaterThan 500

        for flavour, expected, input in recorded do
            let platform =
                match flavour with
                | "linux" -> SimulatedUnixPlatform.linuxX64
                | "darwin" -> SimulatedUnixPlatform.macOsArm64
                | other -> failwith $"recorded.tsv: unknown flavour %s{other}"

            let bytes = UTF8Encoding(false, true).GetBytes input

            let actual =
                match
                    PathArgument.parse (SimulatedUnixPlatform.pathLimits platform) (ImmutableArray.CreateRange bytes)
                with
                | Ok (PathArgument.Parsed path) ->
                    if Seq.toArray (UnixByteString.toBytes (UnixPath.toByteString path)) = bytes then
                        "Parsed"
                    else
                        "Parsed, but not verbatim"
                | Ok (PathArgument.Failed error) -> $"Failed %A{error}"
                | Error (PathArgumentRefusal.InteriorNul offset) -> $"InteriorNul %d{offset}"

            if actual <> expected then
                failwith $"%s{flavour}, %d{bytes.Length} bytes: recorded %s{expected}, now %s{actual}"
