namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open System.Collections.Immutable
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNativeKernel32 =
    let private errorEnvVarNotFound : int = 203

    /// The bytes `value` occupies in the PAL's environment, which holds UTF-8.
    ///
    /// Spelled out from the encoding's width rule rather than taken from the
    /// encoder that builds the bytes under test: an oracle that shared the
    /// implementation's helper could not tell whether it was counting bytes or
    /// code units.
    let private utf8ByteCount (value : string) : int =
        let mutable count = 0
        let mutable i = 0

        while i < value.Length do
            let c = value.[i]

            if
                System.Char.IsHighSurrogate c
                && i + 1 < value.Length
                && System.Char.IsLowSurrogate value.[i + 1]
            then
                count <- count + 4
                i <- i + 2
            else
                count <-
                    count
                    + (if c < '\u0080' then 1
                       elif c < '\u0800' then 2
                       else 3)

                i <- i + 1

        count

    type private EnvironmentVariableCase =
        {
            BufferSize : int
            Value : string option
        }

    /// Strings a generated value is built from, chosen so that a value's UTF-8
    /// byte length and its UTF-16 code-unit length come apart: one byte, two,
    /// three, and a surrogate pair (four bytes over two code units). An
    /// ASCII-only alphabet cannot tell the two lengths apart, which is how a
    /// code-unit count passed for a byte count.
    let private valueAlphabet : string list = [ "x" ; "é" ; "中" ; "\U0001F436" ]

    let private genEnvironmentVariableCase : Gen<EnvironmentVariableCase> =
        let genValue =
            Gen.frequency
                [
                    1, Gen.constant None
                    4,
                    gen {
                        let! length = Gen.choose (0, 180)
                        let! pieces = Gen.listOfLength length (Gen.elements valueAlphabet)
                        return Some (System.String.Concat pieces)
                    }
                ]

        gen {
            let! value = genValue

            let requiredSize =
                value
                |> Option.map (fun value -> utf8ByteCount value + 1)
                |> Option.defaultValue 1

            // The band a code-unit count gets wrong: buffers that would hold the
            // code units but not the bytes.
            let bandLow =
                value |> Option.map (fun value -> value.Length + 1) |> Option.defaultValue 1

            let! bufferSize =
                Gen.frequency
                    [
                        1, Gen.constant 0
                        2, Gen.choose (0, max 0 (requiredSize - 1))
                        2, Gen.choose (bandLow, max bandLow (requiredSize - 1))
                        2, Gen.choose (requiredSize, requiredSize + 32)
                    ]

            return
                {
                    BufferSize = bufferSize
                    Value = value
                }
        }

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    /// `s` as the bytes the kernel holds for it.
    let private bytesOf (s : string) : UnixByteString =
        match UnixByteString.ofString s with
        | Ok bytes -> bytes
        | Error defect -> failwith $"test string %A{s}: %s{UnixPathText.describe defect}"

    /// `bytes` as a kernel byte string, for the bytes no .NET string encodes to.
    let private rawBytes (bytes : byte list) : UnixByteString =
        match UnixByteString.ofBytes (ImmutableArray.CreateRange bytes) with
        | Ok s -> s
        | Error defect -> failwith $"test bytes: %s{UnixByteString.describe defect}"

    let private assertPlan
        (bufferSize : int)
        (value : string option)
        (expectedReturnLength : uint32)
        (expectedLastError : int)
        (expectedValueToWrite : string option)
        : unit
        =
        let actual =
            NativeKernel32.planGetEnvironmentVariableW bufferSize (Option.map bytesOf value)

        actual.ReturnLength |> shouldEqual expectedReturnLength
        actual.LastError |> shouldEqual expectedLastError
        actual.ValueToWrite |> shouldEqual expectedValueToWrite

    [<Test>]
    let ``GetEnvironmentVariableW plan handles exact buffer edges`` () : unit =
        assertPlan 0 None 0u errorEnvVarNotFound None
        assertPlan 0 (Some "") 1u 0 None
        assertPlan 1 (Some "") 0u 0 (Some "")
        assertPlan 3 (Some "abc") 4u 0 None
        assertPlan 4 (Some "abc") 3u 0 (Some "abc")

    [<Test>]
    let ``GetEnvironmentVariableW plan reports the required size in UTF-8 bytes`` () : unit =
        // Rows measured on the real runtime, through CoreLib's own
        // `Interop.Kernel32.GetEnvironmentVariable` wrapper: the value fits when
        // its UTF-8 byte length is below `nSize`, and then the return is its
        // length in UTF-16 code units; otherwise the return is the byte length
        // plus one, however many code units the value has.
        let eAcute = System.String ('é', 100) // 100 code units, 200 bytes
        assertPlan 128 (Some eAcute) 201u 0 None
        assertPlan 200 (Some eAcute) 201u 0 None
        assertPlan 201 (Some eAcute) 100u 0 (Some eAcute)

        let cjk = System.String ('中', 4) // 4 code units, 12 bytes
        assertPlan 12 (Some cjk) 13u 0 None
        assertPlan 13 (Some cjk) 4u 0 (Some cjk)

        let astral = System.String.Concat (Array.create 3 "\U0001F436") // 6 code units, 12 bytes
        assertPlan 12 (Some astral) 13u 0 None
        assertPlan 13 (Some astral) 6u 0 (Some astral)

    [<Test>]
    let ``GetEnvironmentVariableW decodes a value only when it fits`` () : unit =
        // Three bytes that are not UTF-8: a lone continuation byte, then a lead
        // byte with nothing after it. The PAL decodes a value only after
        // deciding it fits, so a buffer too small for it answers the size
        // without meeting the bytes PawPrint does not know how to decode.
        let invalid = Some (rawBytes [ 0x41uy ; 0x80uy ; 0xC3uy ])

        let tooSmall = NativeKernel32.planGetEnvironmentVariableW 3 invalid
        tooSmall.ReturnLength |> shouldEqual 4u
        tooSmall.ValueToWrite |> shouldEqual None

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                NativeKernel32.planGetEnvironmentVariableW 4 invalid
                |> ignore<NativeKernel32.GetEnvironmentVariableWPlan>
            )

        exn.Message |> shouldContainText "GetEnvironmentVariableW"
        exn.Message |> shouldContainText "\\x80"

    [<Test>]
    let ``GetEnvironmentVariableW plan matches the PAL's buffer contract`` () : unit =
        let mutable missing = 0
        let mutable tooSmall = 0
        let mutable tooSmallForBytesOnly = 0
        let mutable fits = 0

        let property (case : EnvironmentVariableCase) : unit =
            let actual =
                NativeKernel32.planGetEnvironmentVariableW case.BufferSize (Option.map bytesOf case.Value)

            match case.Value with
            | None ->
                missing <- missing + 1
                actual.ReturnLength |> shouldEqual 0u
                actual.LastError |> shouldEqual errorEnvVarNotFound
                actual.ValueToWrite |> shouldEqual None
            | Some value ->
                let byteLength = utf8ByteCount value

                if byteLength >= case.BufferSize then
                    tooSmall <- tooSmall + 1

                    if value.Length < case.BufferSize then
                        tooSmallForBytesOnly <- tooSmallForBytesOnly + 1

                    actual.ReturnLength |> shouldEqual (uint32 (byteLength + 1))
                    actual.LastError |> shouldEqual 0
                    actual.ValueToWrite |> shouldEqual None
                else
                    fits <- fits + 1
                    actual.ReturnLength |> shouldEqual (uint32 value.Length)
                    actual.LastError |> shouldEqual 0
                    actual.ValueToWrite |> shouldEqual (Some value)

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genEnvironmentVariableCase) property)

        missing > 20 |> shouldEqual true
        tooSmall > 50 |> shouldEqual true
        // The cases only a byte count gets right: the buffer holds every code
        // unit but not every byte. Without this the property could be satisfied
        // by an ASCII-shaped generator.
        tooSmallForBytesOnly > 50 |> shouldEqual true
        fits > 50 |> shouldEqual true

    /// The UTF-16 code units of an environment block, paired out of its bytes
    /// little-endian.
    ///
    /// Deliberately not `Encoding.Unicode.GetString`: what a guest does to this
    /// block is reinterpret its bytes as `char`s, not decode text. Pairing the
    /// bytes by hand is what a guest's `char*` dereference sees.
    let private codeUnitsOfBlock (bytes : byte array) : char array =
        if bytes.Length % 2 <> 0 then
            failwith $"environment block had an odd byte length %d{bytes.Length}"

        Array.init (bytes.Length / 2) (fun i -> char (uint16 bytes.[i * 2] ||| (uint16 bytes.[i * 2 + 1] <<< 8)))

    /// The environment a guest reads back out of `bytes`, by the algorithm
    /// CoreLib's own `Environment.GetEnvironmentVariables` uses
    /// (`Environment.Variables.Windows.cs`): walk NUL-terminated entries,
    /// stop at the first empty one, skip any entry whose first `=` is not
    /// after the first code unit, split each survivor at that `=`, and keep the
    /// first of two entries naming the same variable (`Hashtable.Add` throws on
    /// the second, and CoreLib swallows that).
    ///
    /// This is a transcription rather than an independent oracle, so encoder and
    /// parser could in principle share a misconception. What anchors it is that
    /// the *real* parse loop — CoreLib's IL, interpreted — runs over
    /// `environmentBlockBytes` output in the end-to-end tests
    /// (`TestPureCases`'s `Environment.GetEnvironmentVariables` cases). Those
    /// pin the transcription at a handful of points; this extends the coverage
    /// to inputs an end-to-end test is too slow to reach.
    let private parseEnvironmentBlock (bytes : byte array) : Map<string, string> =
        let units = codeUnitsOfBlock bytes

        let rec go (acc : Map<string, string>) (start : int) : Map<string, string> =
            match System.Array.IndexOf (units, char 0, start) with
            | -1 -> failwith $"environment block has no terminator for the entry starting at code unit %d{start}"
            // The empty entry closes the block.
            | terminator when terminator = start -> acc
            | terminator ->
                let entry = System.String (units, start, terminator - start)

                let acc =
                    match entry.IndexOf '=' with
                    | i when i > 0 ->
                        let name = entry.Substring (0, i)

                        if Map.containsKey name acc then
                            acc
                        else
                            Map.add name (entry.Substring (i + 1)) acc
                    | _ -> acc

                go acc (terminator + 1)

        go Map.empty 0

    /// What a guest's `GetEnvironmentVariables` should report for `entries`,
    /// stated over the entries themselves rather than over any block: the
    /// entries before the first empty one, those whose first `=` comes after
    /// their first character, and of those the first to name each variable.
    let private expectedVariables (entries : string list) : Map<string, string> =
        entries
        |> List.takeWhile (fun entry -> entry <> "")
        |> List.choose (fun entry ->
            match entry.IndexOf '=' with
            | i when i > 0 -> Some (entry.Substring (0, i), entry.Substring (i + 1))
            | _ -> None
        )
        |> List.distinctBy fst
        |> Map.ofList

    /// Strings a generated entry is built from. NUL and unpaired surrogates
    /// appear in neither, because no kernel environment entry decodes to them.
    ///
    /// Deliberately not ASCII-only, so that a byte budget and a code-unit
    /// budget come apart, and deliberately rich in `=`, so that entries with
    /// none, entries beginning with one and values containing one all turn up.
    let private entryAlphabet : string list =
        [ "A" ; "a" ; "_" ; "0" ; "=" ; "=" ; "é" ; "中" ; "\U0001F436" ]

    let private genEntry : Gen<string> =
        Gen.frequency
            [
                // A small pool, so that duplicate names and prefixes of one
                // another (`A`, `AA`) turn up in the same environment often.
                3, Gen.elements [ "A=1" ; "A=2" ; "AA=3" ; "A" ; "=A" ; "a=" ; "A==" ]
                1, Gen.constant ""
                4,
                gen {
                    // Mostly short, occasionally long enough that no fixed-size
                    // buffer assumption could hide in the encoder.
                    let! length = Gen.frequency [ 3, Gen.choose (1, 6) ; 1, Gen.choose (120, 140) ]
                    let! pieces = Gen.listOfLength length (Gen.elements entryAlphabet)
                    return System.String.Concat pieces
                }
            ]

    let private genEnvironment : Gen<string list> =
        gen {
            let! count = Gen.frequency [ 1, Gen.constant 0 ; 1, Gen.constant 1 ; 4, Gen.choose (2, 12) ]
            return! Gen.listOfLength count genEntry
        }

    [<Test>]
    let ``environment block holds every entry, in order`` () : unit =
        // The block is the PAL's conversion of its snapshot of `environ`: every
        // entry, decoded, in the order the process was started with, whatever
        // its reader goes on to make of it. That includes the entries after an
        // empty one, whose lone NUL reads as the end of the block to every
        // reader that walks it (CoreLib's `GetEnvironmentVariables`, and
        // `CLRConfig::Initialize`), so those entries are in the memory the guest
        // is handed but no such reader reaches them; the parse property below
        // pins that half.
        let property (entries : string list) : unit =
            let units =
                NativeKernel32.environmentBlockBytes (List.map bytesOf entries)
                |> codeUnitsOfBlock

            System.String units
            |> shouldEqual (
                System.String.Concat (entries |> List.map (fun entry -> entry + "\000"))
                + "\000"
            )

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genEnvironment) property)

    [<Test>]
    let ``environment block parses as the variables CoreLib reports`` () : unit =
        let mutable empties = 0
        let mutable duplicateNames = 0
        let mutable withoutEquals = 0
        let mutable leadingEquals = 0
        let mutable emptyEntryBeforeEnd = 0
        let mutable withAstral = 0

        let property (entries : string list) : unit =
            NativeKernel32.environmentBlockBytes (List.map bytesOf entries)
            |> parseEnvironmentBlock
            |> shouldEqual (expectedVariables entries)

            if List.isEmpty entries then
                empties <- empties + 1

            let names =
                entries
                |> List.choose (fun entry ->
                    match entry.IndexOf '=' with
                    | i when i > 0 -> Some (entry.Substring (0, i))
                    | _ -> None
                )

            if List.length (List.distinct names) < List.length names then
                duplicateNames <- duplicateNames + 1

            if entries |> List.exists (fun entry -> entry <> "" && not (entry.Contains '=')) then
                withoutEquals <- withoutEquals + 1

            if entries |> List.exists (fun entry -> entry.StartsWith '=') then
                leadingEquals <- leadingEquals + 1

            match List.tryFindIndex (fun entry -> entry = "") entries with
            | Some i when i < List.length entries - 1 -> emptyEntryBeforeEnd <- emptyEntryBeforeEnd + 1
            | _ -> ()

            if entries |> List.exists (fun entry -> entry.Contains "\U0001F436") then
                withAstral <- withAstral + 1

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genEnvironment) property)

        // Every shape the parse is meant to be interesting for really did turn
        // up. Without these, narrowing the generator later would silently reduce
        // the property to a claim about short, distinct, well-formed pairs.
        empties > 20 |> shouldEqual true
        duplicateNames > 20 |> shouldEqual true
        withoutEquals > 20 |> shouldEqual true
        leadingEquals > 20 |> shouldEqual true
        emptyEntryBeforeEnd > 20 |> shouldEqual true
        withAstral > 20 |> shouldEqual true

    /// A NUL code unit as a string, for spelling block layouts out readably.
    let private nul : string = string (char 0)

    /// Little-endian UTF-16 bytes of `s`, spelled out here rather than taken from
    /// `Encoding.Unicode` so that these expectations and the encoder under test
    /// cannot agree merely by sharing a helper.
    let private expectedBytes (s : string) : byte array =
        s.ToCharArray ()
        |> Array.collect (fun c -> [| byte (uint16 c % 256us) ; byte (uint16 c / 256us) |])

    [<Test>]
    let ``empty environment is a lone NUL code unit`` () : unit =
        // Not a null pointer, and not a zero-length block: the PAL mallocs one
        // WCHAR and writes a NUL into it, returning null only when that malloc
        // fails. CoreLib turns a null return into an OutOfMemoryException, so
        // the difference is guest-visible.
        NativeKernel32.environmentBlockBytes [] |> shouldEqual [| 0uy ; 0uy |]

    [<Test>]
    let ``environment block is each entry NUL-terminated, in kernel order, NUL-closed`` () : unit =
        NativeKernel32.environmentBlockBytes [ bytesOf "FOO=bar" ]
        |> shouldEqual (expectedBytes ("FOO=bar" + nul + nul))

        // The kernel's order, not the names' order; a duplicate name twice; an
        // entry with no `=`; an entry beginning with `=`. All are in the PAL's
        // block, because it converts its `environ` snapshot entry for entry.
        NativeKernel32.environmentBlockBytes (List.map bytesOf [ "b=1" ; "a=2" ; "b=3" ; "NOEQUALS" ; "=hidden" ])
        |> shouldEqual (
            expectedBytes (
                "b=1"
                + nul
                + "a=2"
                + nul
                + "b=3"
                + nul
                + "NOEQUALS"
                + nul
                + "=hidden"
                + nul
                + nul
            )
        )

        // An empty value keeps its `=`, so the variable is present-and-empty
        // rather than absent: an entry with no `=` is one CoreLib discards.
        NativeKernel32.environmentBlockBytes [ bytesOf "FOO=" ]
        |> shouldEqual (expectedBytes ("FOO=" + nul + nul))

    [<Test>]
    let ``environment block decodes UTF-8 to UTF-16 code units`` () : unit =
        // An astral character is four bytes in the kernel and two code units,
        // a surrogate pair, in the block.
        NativeKernel32.environmentBlockBytes [ bytesOf "K=\U0001F436" ]
        |> shouldEqual
            [|
                0x4Buy
                0uy
                0x3Duy
                0uy
                0x3Duy
                0xD8uy
                0x36uy
                0xDCuy
                0uy
                0uy
                0uy
                0uy
            |]

    [<Test>]
    let ``environment block refuses an entry that is not UTF-8`` () : unit =
        // PawPrint does not model how the PAL's decoder substitutes for bytes
        // that are not UTF-8, so the block fails rather than guess. Its own
        // configuration cannot produce such an entry; another route into the
        // kernel's environment can.
        let exn =
            Assert.Throws<System.Exception> (fun () ->
                NativeKernel32.environmentBlockBytes [ bytesOf "OK=1" ; rawBytes [ 0x42uy ; 0x3Duy ; 0xFFuy ] ]
                |> ignore<byte array>
            )

        exn.Message |> shouldContainText "GetEnvironmentStringsW"
        exn.Message |> shouldContainText "\\xFF"
