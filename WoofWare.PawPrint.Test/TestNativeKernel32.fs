namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNativeKernel32 =
    let private errorEnvVarNotFound : int = 203

    /// The bytes `value` occupies in the PAL's environment, which holds UTF-8.
    ///
    /// Spelled out from the encoding's width rule rather than taken from
    /// `Encoding.UTF8.GetByteCount`, which is what `planGetEnvironmentVariableW`
    /// itself uses: an oracle that shared the implementation's helper could not
    /// tell whether it was counting bytes or code units.
    ///
    /// An unpaired surrogate counts three. `Encoding.UTF8` replaces one with
    /// U+FFFD, which is three bytes, and its generalised-UTF-8 encoding is three
    /// bytes too, so either reading of such a value gives the same count.
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

    /// Code units a generated value is built from, chosen so that a value's
    /// UTF-8 byte length and its UTF-16 code-unit length come apart: one
    /// byte, two, three, a surrogate pair (four bytes over two code units), and
    /// an unpaired surrogate. An ASCII-only alphabet cannot tell the two
    /// lengths apart, which is how a code-unit count passed for a byte count.
    let private valueAlphabet : char list =
        [ 'x' ; 'é' ; '中' ; char 0xD83D ; char 0xDC36 ; char 0xD800 ]

    let private genEnvironmentVariableCase : Gen<EnvironmentVariableCase> =
        let genValue =
            Gen.frequency
                [
                    1, Gen.constant None
                    4,
                    gen {
                        let! length = Gen.choose (0, 260)
                        let! chars = Gen.listOfLength length (Gen.elements valueAlphabet)
                        return Some (System.String (List.toArray chars))
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

    let private assertPlan
        (bufferSize : int)
        (value : string option)
        (expectedReturnLength : uint32)
        (expectedLastError : int)
        (expectedValueToWrite : string option)
        : unit
        =
        let actual = NativeKernel32.planGetEnvironmentVariableW bufferSize value

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

        // An unpaired surrogate has no measured row -- no real environment can
        // hold one, since the PAL's environment is bytes -- but PawPrint's table
        // can, and hands the code unit to a guest verbatim. It is counted as the
        // three bytes both of its generalised-UTF-8 form and of the U+FFFD
        // `Encoding.UTF8` would substitute, so the two readings agree.
        let lone = System.String [| char 0xD800 |]
        assertPlan 3 (Some lone) 4u 0 None
        assertPlan 4 (Some lone) 1u 0 (Some lone)

    [<Test>]
    let ``GetEnvironmentVariableW plan matches the PAL's buffer contract`` () : unit =
        let mutable missing = 0
        let mutable tooSmall = 0
        let mutable tooSmallForBytesOnly = 0
        let mutable fits = 0

        let property (case : EnvironmentVariableCase) : unit =
            let actual = NativeKernel32.planGetEnvironmentVariableW case.BufferSize case.Value

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
    /// Deliberately not `Encoding.Unicode.GetString`: that is not faithful at
    /// the code-unit level (it replaces an unpaired surrogate with U+FFFD), and
    /// what a guest does to this block is reinterpret its bytes as `char`s, not
    /// decode text. Pairing the bytes by hand is what a guest's `char*`
    /// dereference sees.
    let private codeUnitsOfBlock (bytes : byte array) : char array =
        if bytes.Length % 2 <> 0 then
            failwith $"environment block had an odd byte length %d{bytes.Length}"

        Array.init (bytes.Length / 2) (fun i -> char (uint16 bytes.[i * 2] ||| (uint16 bytes.[i * 2 + 1] <<< 8)))

    /// The environment a guest reads back out of `bytes`, by the algorithm
    /// CoreLib's own `Environment.GetEnvironmentVariables` uses
    /// (`Environment.Variables.Windows.cs`): walk NUL-terminated entries,
    /// stop at the first empty one, skip any entry whose first `=` is not
    /// after the first code unit, and split each survivor at that `=`.
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
                    | i when i > 0 -> Map.add (entry.Substring (0, i)) (entry.Substring (i + 1)) acc
                    // CoreLib skips an entry with no `=`, and one beginning with
                    // `=`; `environmentBlockBytes` must never emit either.
                    | _ -> acc

                go acc (terminator + 1)

        go Map.empty 0

    /// Code units a generated name or value is built from. `=` is added for
    /// values only; NUL appears in neither, because `environmentBlockBytes`
    /// refuses it by contract — generating it here would be asking the
    /// round-trip property to hold of an input the encoder is specified to
    /// reject, and the refusals have their own test below.
    ///
    /// Deliberately not ASCII-only. An ASCII alphabet cannot distinguish a byte
    /// budget from a code-unit budget, and cannot see an encoder mangle an
    /// unpaired surrogate to U+FFFD — which this encoder did, when it went
    /// through `Encoding.Unicode`.
    let private interestingCodeUnits : char list =
        [
            'A'
            'a'
            '_'
            '0'
            // Two and three UTF-8 bytes respectively, one UTF-16 code unit each.
            'é'
            '中'
            // A surrogate *pair*: one character, two code units, four UTF-8 bytes.
            char 0xD83D
            char 0xDC36
            // *Unpaired* surrogates, high and low. Legal in a .NET string, and
            // silently destroyed by `Encoding.Unicode`.
            char 0xD800
            char 0xDFFF
        ]

    let private genFromAlphabet (alphabet : char list) (minLength : int) : Gen<string> =
        gen {
            // Mostly short, occasionally long enough that no fixed-size buffer
            // assumption could hide in the encoder.
            let! extra = Gen.frequency [ 3, Gen.choose (0, 6) ; 1, Gen.choose (120, 140) ]

            let! chars = Gen.listOfLength (minLength + extra) (Gen.elements alphabet)
            return System.String (List.toArray chars)
        }

    /// Names are non-empty and `=`-free, which is what an environment block can
    /// express. Drawn from a tiny pool as well as freely, so that names which are
    /// prefixes of one another (`A`, `AA`) turn up in the same block often.
    let private genName : Gen<string> =
        Gen.frequency
            [
                1, Gen.elements [ "A" ; "AA" ; "AAA" ; "a" ; "_" ]
                3, genFromAlphabet interestingCodeUnits 1
            ]

    /// Values may be empty, and may contain `=`: `FOO=a=b` is an ordinary
    /// variable whose value is `a=b`, so an encoder or parser splitting at the
    /// *last* `=` would get it wrong.
    let private genValue : Gen<string> =
        Gen.frequency
            [
                1, Gen.constant ""
                1, Gen.elements [ "=" ; "a=b" ; "==" ; "a=" ]
                3, genFromAlphabet ('=' :: interestingCodeUnits) 0
            ]

    let private genEnvironment : Gen<Map<string, string>> =
        gen {
            let! count = Gen.frequency [ 1, Gen.constant 0 ; 1, Gen.constant 1 ; 4, Gen.choose (2, 12) ]
            let! entries = Gen.listOfLength count (Gen.zip genName genValue)
            return Map.ofList entries
        }

    [<Test>]
    let ``environment block round-trips through CoreLib's parse algorithm`` () : unit =
        let mutable empties = 0
        let mutable withEqualsInValue = 0
        let mutable withEmptyValue = 0
        let mutable withAstral = 0
        let mutable withUnpairedSurrogate = 0

        let property (environment : Map<string, string>) : unit =
            let bytes = NativeKernel32.environmentBlockBytes environment
            parseEnvironmentBlock bytes |> shouldEqual environment

            if Map.isEmpty environment then
                empties <- empties + 1

            for KeyValue (_, value) in environment do
                if value.Contains '=' then
                    withEqualsInValue <- withEqualsInValue + 1

                if value = "" then
                    withEmptyValue <- withEmptyValue + 1

            for KeyValue (name, value) in environment do
                for s in [ name ; value ] do
                    for i in 0 .. s.Length - 1 do
                        if System.Char.IsHighSurrogate s.[i] then
                            if i + 1 < s.Length && System.Char.IsLowSurrogate s.[i + 1] then
                                withAstral <- withAstral + 1
                            else
                                withUnpairedSurrogate <- withUnpairedSurrogate + 1
                        elif System.Char.IsLowSurrogate s.[i] then
                            withUnpairedSurrogate <- withUnpairedSurrogate + 1

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genEnvironment) property)

        // Every shape the round-trip is meant to be interesting for really did
        // turn up. Without these, narrowing the generator later would silently
        // reduce the property to a claim about short ASCII pairs.
        empties > 20 |> shouldEqual true
        withEqualsInValue > 20 |> shouldEqual true
        withEmptyValue > 20 |> shouldEqual true
        withAstral > 20 |> shouldEqual true
        withUnpairedSurrogate > 20 |> shouldEqual true

    /// A NUL code unit as a string, for spelling block layouts out readably.
    let private nul : string = string (char 0)

    /// Little-endian UTF-16 bytes of `s`, spelled out here rather than taken from
    /// `Encoding.Unicode` so that these expectations and the encoder under test
    /// cannot agree merely by sharing a helper. (`Encoding.Unicode` would also
    /// mangle the unpaired surrogate the last layout test relies on.)
    let private expectedBytes (s : string) : byte array =
        s.ToCharArray ()
        |> Array.collect (fun c -> [| byte (uint16 c % 256us) ; byte (uint16 c / 256us) |])

    [<Test>]
    let ``empty environment is a lone NUL code unit`` () : unit =
        // Not a null pointer, and not a zero-length block: the PAL mallocs one
        // WCHAR and writes a NUL into it, returning null only when that malloc
        // fails. CoreLib turns a null return into an OutOfMemoryException, so
        // the difference is guest-visible.
        NativeKernel32.environmentBlockBytes Map.empty |> shouldEqual [| 0uy ; 0uy |]

    [<Test>]
    let ``environment block is name=value per entry, NUL-terminated, NUL-closed`` () : unit =
        NativeKernel32.environmentBlockBytes (Map.ofList [ "FOO", "bar" ])
        |> shouldEqual (expectedBytes ("FOO=bar" + nul + nul))

        // Entries come out in ordinal name order, which is what makes the block a
        // function of the environment alone. This pins the documented behaviour
        // rather than a requirement: the real block carries the host's `environ`
        // order, and nothing a guest may rely on depends on either.
        NativeKernel32.environmentBlockBytes (Map.ofList [ "b", "1" ; "a", "2" ])
        |> shouldEqual (expectedBytes ("a=2" + nul + "b=1" + nul + nul))

        // An empty value keeps its `=`, so the variable is present-and-empty
        // rather than absent: an entry with no `=` is one CoreLib discards.
        NativeKernel32.environmentBlockBytes (Map.ofList [ "FOO", "" ])
        |> shouldEqual (expectedBytes ("FOO=" + nul + nul))

    [<Test>]
    let ``environment block writes code units verbatim`` () : unit =
        // An unpaired surrogate is a legal `char` in a .NET string, and
        // `GetEnvironmentVariableW` hands one to a guest verbatim. Encoding this
        // block as *text* would substitute U+FFFD, making the two environment
        // APIs disagree about the same table.
        let lone = System.String [| char 0xD800 |]

        NativeKernel32.environmentBlockBytes (Map.ofList [ "K", lone ])
        |> shouldEqual
            [|
                0x4Buy
                0x00uy
                0x3Duy
                0x00uy
                0x00uy
                0xD8uy
                0x00uy
                0x00uy
                0x00uy
                0x00uy
            |]

    [<Test>]
    let ``environment block refuses an entry it cannot express`` () : unit =
        // Each of these is a table no real environment list can express, so
        // flattening it would hand a guest variables differing from the ones
        // `Environment.GetEnvironmentVariable` reports for the same table.
        //
        // `UnixProcessState.withEnvironment` already rejects these when the table is
        // built, so a host cannot reach this through `KernelConfig`; what this
        // covers is the record-copied kernel that never passed through that
        // writer, which is exactly how the map arrives here. (The boundary itself
        // is covered by `TestEnvironmentEntryInvariant`.)
        //
        // One input per rejected shape, each provoking that shape alone, so
        // dropping any single check leaves this test failing on exactly one row.
        let refused =
            [
                "empty name", "", "value"
                "'=' in name", "A=B", "value"
                "NUL in name", "A" + nul + "B", "value"
                "NUL in value", "A", "va" + nul + "ue"
            ]

        for description, name, value in refused do
            let exn =
                Assert.Throws<System.Exception> (fun () ->
                    NativeKernel32.environmentBlockBytes (Map.ofList [ name, value ])
                    |> ignore<byte array>
                )

            // Names the entry point, and says how such a table can have got here
            // at all, so a failing run is not read as a host-configuration bug
            // that `withEnvironment` would already have caught.
            exn.Message |> shouldContainText "GetEnvironmentStringsW"
            exn.Message |> shouldContainText "record-copy"
            description |> shouldNotEqual ""

    [<Test>]
    let ``environment block accepts the shapes a real environ can hold`` () : unit =
        // Controls for the refusals above, so the rule cannot be satisfied by
        // refusing everything: a value may contain `=`, a value may be empty, and
        // either half may hold non-ASCII.
        let environment =
            Map.ofList
                [
                    "A", ""
                    "B", "x=y=z"
                    "\u00e9\u4e2d", "\U0001F436"
                    "lower_case.name-1", "v"
                ]

        NativeKernel32.environmentBlockBytes environment
        |> parseEnvironmentBlock
        |> shouldEqual environment
