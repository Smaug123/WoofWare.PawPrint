namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.Text
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixByteString =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    /// The strict decoder this type's `tryToString` is specified against. It is
    /// deliberately a different implementation from the one under test, which
    /// decodes with `System.Text.Unicode.Utf8`: an oracle that shares the
    /// implementation tests nothing.
    let private strictUtf8 : UTF8Encoding = UTF8Encoding (false, true)

    let private decodesStrictly (bytes : byte[]) : string option =
        try
            Some (strictUtf8.GetString bytes)
        with :? DecoderFallbackException ->
            None

    /// Arbitrary NUL-free bytes. Overwhelmingly *not* valid UTF-8 once there is
    /// more than a byte or two of it, which is the half of the domain that has
    /// never been representable before.
    let private rawBytesGen : Gen<byte[]> =
        ArbMap.defaults
        |> ArbMap.generate<byte>
        |> Gen.filter (fun b -> b <> 0uy)
        |> Gen.listOf
        |> Gen.map List.toArray

    /// Bytes that *are* valid UTF-8, including multi-byte and astral sequences:
    /// the other half of the domain, and the one `tryToString` must round-trip.
    let private utf8BytesGen : Gen<byte[]> =
        gen {
            let! chars =
                ArbMap.defaults
                |> ArbMap.generate<char>
                |> Gen.filter (fun c -> c <> '\000' && not (Char.IsSurrogate c))
                |> Gen.listOf

            // Astral characters are the four-byte UTF-8 case, and the surrogate
            // filter above excludes them, so splice one in rather than hoping.
            let! includeAstral = Gen.frequency [ 1, Gen.constant true ; 3, Gen.constant false ]

            let chars =
                if includeAstral then
                    chars @ [ '\uD83D' ; '\uDC36' ]
                else
                    chars

            return strictUtf8.GetBytes (String (List.toArray chars))
        }

    /// Both halves of the domain, plus their concatenations: a name that is
    /// valid UTF-8 up to a point and then is not is the shape that separates a
    /// byte-wise implementation from a decode-the-whole-thing one.
    let private bytesGen : Gen<byte[]> =
        Gen.frequency
            [
                3, rawBytesGen
                3, utf8BytesGen
                2, Gen.map2 Array.append utf8BytesGen rawBytesGen
                2, Gen.map2 Array.append rawBytesGen utf8BytesGen
            ]

    let private ofBytesOk (bytes : byte[]) : UnixByteString =
        match UnixByteString.ofBytes (ImmutableArray.CreateRange bytes) with
        | Ok s -> s
        | Error defect -> failwith $"expected NUL-free bytes to parse: %s{UnixByteString.describe defect}"

    let private toArray (s : UnixByteString) : byte[] = UnixByteString.toBytes s |> Seq.toArray

    // ------------------------------------------------------------ construction

    [<Test>]
    let ``ofBytes round-trips every NUL-free byte string`` () : unit =
        let property (bytes : byte[]) : unit =
            ofBytesOk bytes |> toArray |> shouldEqual bytes

        Check.One (config, Prop.forAll (Arb.fromGen bytesGen) property)

    /// Bytes drawn so that NUL turns up often enough to exercise the rejection,
    /// which a uniform byte generator would hit only one time in 256.
    let private nulProneBytesGen : Gen<byte[]> =
        Gen.frequency [ 3, (ArbMap.defaults |> ArbMap.generate<byte>) ; 1, Gen.constant 0uy ]
        |> Gen.listOf
        |> Gen.map List.toArray

    [<Test>]
    let ``ofBytes refuses an interior NUL, naming where it is`` () : unit =
        let property (bytes : byte[]) : unit =
            match UnixByteString.ofBytes (ImmutableArray.CreateRange bytes) with
            | Ok _ -> Array.contains 0uy bytes |> shouldEqual false
            | Error (UnixByteStringDefect.ContainsNul index) ->
                bytes.[index] |> shouldEqual 0uy
                // The *first* NUL, so the report is deterministic.
                bytes.[0 .. index - 1] |> Array.contains 0uy |> shouldEqual false

        Check.One (config, Prop.forAll (Arb.fromGen nulProneBytesGen) property)

    [<Test>]
    let ``length agrees with the bytes`` () : unit =
        let property (bytes : byte[]) : unit =
            ofBytesOk bytes |> UnixByteString.length |> shouldEqual bytes.Length

        Check.One (config, Prop.forAll (Arb.fromGen bytesGen) property)

    // ---------------------------------------------------------------- equality

    [<Test>]
    let ``equality and hashing are structural`` () : unit =
        let property (bytes : byte[]) : unit =
            // Two independently built values over copies of the same bytes: a
            // reference-equality implementation passes `a = a` and fails this.
            let left = ofBytesOk (Array.copy bytes)
            let right = ofBytesOk (Array.copy bytes)

            left |> shouldEqual right
            hash left |> shouldEqual (hash right)
            left.Equals (box right) |> shouldEqual true

        Check.One (config, Prop.forAll (Arb.fromGen bytesGen) property)

    [<Test>]
    let ``distinct bytes are distinct values`` () : unit =
        let property (left : byte[], right : byte[]) : unit =
            let leftValue = ofBytesOk left
            let rightValue = ofBytesOk right

            (leftValue = rightValue) |> shouldEqual (left = right)

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.zip bytesGen bytesGen)) property)

    // ------------------------------------------------------------- comparison

    /// Pairs whose lengths differ often, and which include proper prefixes.
    /// §2.1 of the plan measured that a bare `ImmutableArray<byte>` compares
    /// fine at equal lengths and *throws* at unequal ones, so a generator that
    /// does not vary the length passes against the broken implementation.
    let private pairGen : Gen<byte[] * byte[]> =
        let prefixPair =
            gen {
                let! prefix = bytesGen
                let! extra = Gen.filter (fun b -> b <> 0uy) (ArbMap.defaults |> ArbMap.generate<byte>)
                let! extras = Gen.listOf (Gen.constant extra)
                return prefix, Array.append prefix (List.toArray (extra :: extras))
            }

        Gen.frequency
            [
                4, Gen.zip bytesGen bytesGen
                2, prefixPair
                1, Gen.constant ([| 1uy |], [| 1uy ; 2uy |])
            ]

    [<Test>]
    let ``comparison is lexicographic on unsigned bytes, across differing lengths`` () : unit =
        let property (left : byte[], right : byte[]) : unit =
            let expected = compare (Array.toList left) (Array.toList right)

            compare (ofBytesOk left) (ofBytesOk right)
            |> sign
            |> shouldEqual (sign expected)

        Check.One (config, Prop.forAll (Arb.fromGen pairGen) property)

    [<Test>]
    let ``comparison is a total order consistent with equality`` () : unit =
        let property (left : byte[], right : byte[]) : unit =
            let leftValue = ofBytesOk left
            let rightValue = ofBytesOk right

            // Antisymmetry, and agreement with equality in both directions.
            sign (compare leftValue rightValue)
            |> shouldEqual (-(sign (compare rightValue leftValue)))

            (compare leftValue rightValue = 0) |> shouldEqual (leftValue = rightValue)

        Check.One (config, Prop.forAll (Arb.fromGen pairGen) property)

    [<Test>]
    let ``comparison is transitive`` () : unit =
        let property (values : byte[] list) : unit =
            let sorted = values |> List.map ofBytesOk |> List.sort

            // If `compare` were not transitive the sort would not be a sort;
            // check every adjacent pair really is ordered.
            for left, right in List.pairwise sorted do
                compare left right |> shouldBeSmallerThan 1

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.listOf bytesGen)) property)

    [<Test>]
    let ``Map, Set and sort work over keys of differing lengths`` () : unit =
        let property (values : byte[] list) : unit =
            // Distinct *and* of differing lengths: the combination that a bare
            // `ImmutableArray<byte>` key throws `ArgumentException` on.
            let values = values |> List.distinct

            let map = values |> List.mapi (fun i bytes -> ofBytesOk bytes, i) |> Map.ofList

            for i, bytes in List.indexed values do
                // Look up through a *freshly built* value, not the one inserted.
                Map.tryFind (ofBytesOk (Array.copy bytes)) map |> shouldEqual (Some i)

            let set = values |> List.map ofBytesOk |> Set.ofList
            Set.count set |> shouldEqual (List.length values)

            values
            |> List.map ofBytesOk
            |> List.sort
            |> List.length
            |> shouldEqual (List.length values)

        let differingLengths : Gen<byte[] list> =
            Gen.listOf bytesGen
            |> Gen.map (List.mapi (fun i bytes -> Array.append bytes (Array.replicate i 1uy)))

        Check.One (config, Prop.forAll (Arb.fromGen differingLengths) property)

    [<Test>]
    let ``a value of another type is unequal, and not comparable`` () : unit =
        let value = ofBytesOk [| 1uy |]

        value.Equals (box 1uy) |> shouldEqual false
        value.Equals (null : obj) |> shouldEqual false

        // Ordering against a foreign type is a programming error, not a
        // verdict: answering it would let a mixed collection sort silently.
        Assert.Throws<ArgumentException> (fun () -> (value :> IComparable).CompareTo (box 1uy) |> ignore<int>)
        |> ignore<ArgumentException>

    // --------------------------------------------------------------- tryToString

    [<Test>]
    let ``tryToString round-trips bytes that are valid UTF-8`` () : unit =
        let property (bytes : byte[]) : unit =
            match UnixByteString.tryToString (ofBytesOk bytes) with
            | None -> failwith $"expected valid UTF-8 to decode: %A{bytes}"
            | Some decoded ->
                decoded |> shouldEqual (strictUtf8.GetString bytes)

                match UnixByteString.ofString decoded with
                | Ok reencoded -> reencoded |> shouldEqual (ofBytesOk bytes)
                | Error defect -> failwith $"re-encoding failed: %s{UnixPathText.describe defect}"

        Check.One (config, Prop.forAll (Arb.fromGen utf8BytesGen) property)

    [<Test>]
    let ``tryToString is None exactly when the bytes are not valid UTF-8`` () : unit =
        let property (bytes : byte[]) : unit =
            let actual = UnixByteString.tryToString (ofBytesOk bytes)
            actual |> shouldEqual (decodesStrictly bytes)

        Check.One (config, Prop.forAll (Arb.fromGen bytesGen) property)

    [<Test>]
    let ``ofString refuses a string that has no UTF-8 encoding`` () : unit =
        // Built from `char` values: the F# lexer turns a lone surrogate in a
        // string literal into U+FFFD, so a literal would test the wrong thing.
        UnixByteString.ofString (String [| 'a' ; char 0xD83D ; 'b' |])
        |> shouldEqual (Error (UnixPathTextDefect.UnpairedSurrogate 1))

        UnixByteString.ofString "a\000b"
        |> shouldEqual (Error (UnixPathTextDefect.ContainsNul 1))

    // ---------------------------------------------------------------- toEscaped

    [<Test>]
    let ``toEscaped is total`` () : unit =
        let property (bytes : byte[]) : unit =
            UnixByteString.toEscaped (ofBytesOk bytes) |> ignore<string>

        Check.One (config, Prop.forAll (Arb.fromGen bytesGen) property)

    [<Test>]
    let ``toEscaped is injective`` () : unit =
        let property (left : byte[], right : byte[]) : unit =
            let leftEscaped = UnixByteString.toEscaped (ofBytesOk left)
            let rightEscaped = UnixByteString.toEscaped (ofBytesOk right)

            (leftEscaped = rightEscaped) |> shouldEqual (left = right)

        Check.One (config, Prop.forAll (Arb.fromGen pairGen) property)

    [<Test>]
    let ``toEscaped renders the cases a diagnostic has to tell apart`` () : unit =
        let escaped (bytes : byte list) : string =
            UnixByteString.toEscaped (ofBytesOk (List.toArray bytes))

        // Valid UTF-8 runs verbatim.
        escaped [ 0x61uy ; 0x62uy ] |> shouldEqual "ab"
        escaped (List.ofArray (strictUtf8.GetBytes "é")) |> shouldEqual "é"

        // A byte that is not valid UTF-8 renders as an escape.
        escaped [ 0xffuy ] |> shouldEqual "\\xFF"

        // The literal backslash doubles, which is what keeps the rendering
        // injective: without it these two four-byte names would print alike.
        escaped [ 0x5cuy ; 0x78uy ; 0x66uy ; 0x66uy ] |> shouldEqual "\\\\xff"

        // Valid UTF-8 either side of an invalid byte, so the escape does not
        // swallow the rest of the string.
        escaped [ 0x61uy ; 0xffuy ; 0x62uy ] |> shouldEqual "a\\xFFb"

    [<Test>]
    let ``ToString is the escaped rendering`` () : unit =
        let property (bytes : byte[]) : unit =
            let value = ofBytesOk bytes
            value.ToString () |> shouldEqual (UnixByteString.toEscaped value)

        Check.One (config, Prop.forAll (Arb.fromGen bytesGen) property)

    // -------------------------------------------------------------- assertValid

    [<Test>]
    let ``assertValid accepts anything ofBytes produced`` () : unit =
        let property (bytes : byte[]) : unit =
            let value = ofBytesOk bytes
            UnixByteString.assertValid "test" value |> shouldEqual value

        Check.One (config, Prop.forAll (Arb.fromGen bytesGen) property)

    [<Test>]
    let ``assertValid rejects a forged value, naming its context`` () : unit =
        // `Unchecked.defaultof` carries a *default* `ImmutableArray`, whose
        // underlying array is null; reading `.Length` on it throws a bare
        // NullReferenceException, so the check has to come first.
        // `Assert.Catch` rather than `Assert.Throws`, so that a
        // NullReferenceException reaches the assertion below rather than
        // failing on the exception type and hiding which check fired.
        let exn =
            Assert.Catch<Exception> (fun () ->
                UnixByteString.assertValid "seed manifest entry" Unchecked.defaultof<UnixByteString>
                |> ignore<UnixByteString>
            )

        exn.GetType () |> shouldNotEqual typeof<NullReferenceException>
        exn.Message |> shouldContainText "seed manifest entry"
