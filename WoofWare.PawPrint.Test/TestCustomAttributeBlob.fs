namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.Text
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCustomAttributeBlob =

    /// Encode a PackedLen per ECMA-335 II.23.2.
    let private encodePackedLen (length : int) : byte array =
        if length < 0 then
            failwithf "negative length %d" length
        elif length < 0x80 then
            [| byte length |]
        elif length < 0x4000 then
            let hi = (length >>> 8) ||| 0x80
            let lo = length &&& 0xFF
            [| byte hi ; byte lo |]
        elif length < 0x20000000 then
            let b0 = ((length >>> 24) &&& 0x1F) ||| 0xC0
            let b1 = (length >>> 16) &&& 0xFF
            let b2 = (length >>> 8) &&& 0xFF
            let b3 = length &&& 0xFF
            [| byte b0 ; byte b1 ; byte b2 ; byte b3 |]
        else
            failwithf "length %d exceeds PackedLen maximum" length

    /// Build a CustomAttrib blob carrying a single SerString fixed arg.
    /// If `s` is None, encodes the null sentinel.
    let private buildBlob (s : string option) : ImmutableArray<byte> =
        let prolog = [| 0x01uy ; 0x00uy |] // little-endian 0x0001

        let serString =
            match s with
            | None -> [| 0xFFuy |]
            | Some str ->
                let utf8 = Encoding.UTF8.GetBytes (str : string)
                Array.append (encodePackedLen utf8.Length) utf8

        Array.append prolog serString |> ImmutableArray.Create<byte>

    [<Test>]
    let ``decodes empty string`` () : unit =
        CustomAttribute.tryReadLeadingSerString (buildBlob (Some ""))
        |> shouldEqual (Ok (Some ""))

    [<Test>]
    let ``decodes short ASCII string`` () : unit =
        CustomAttribute.tryReadLeadingSerString (buildBlob (Some "Hello"))
        |> shouldEqual (Ok (Some "Hello"))

    [<Test>]
    let ``decodes null sentinel`` () : unit =
        CustomAttribute.tryReadLeadingSerString (buildBlob None)
        |> shouldEqual (Ok None)

    [<Test>]
    let ``decodes two-byte packed length`` () : unit =
        let s = String.replicate 200 "a"

        CustomAttribute.tryReadLeadingSerString (buildBlob (Some s))
        |> shouldEqual (Ok (Some s))

    [<Test>]
    let ``decodes four-byte packed length`` () : unit =
        let s = String.replicate 0x4000 "z"

        CustomAttribute.tryReadLeadingSerString (buildBlob (Some s))
        |> shouldEqual (Ok (Some s))

    [<Test>]
    let ``decodes multibyte UTF-8`` () : unit =
        let s = "é中\U0001F600" // é + 中 + emoji

        CustomAttribute.tryReadLeadingSerString (buildBlob (Some s))
        |> shouldEqual (Ok (Some s))

    [<Test>]
    let ``rejects wrong prolog`` () : unit =
        let blob = ImmutableArray.Create<byte> ([| 0x02uy ; 0x00uy ; 0x00uy |])

        match CustomAttribute.tryReadLeadingSerString blob with
        | Error msg -> msg |> shouldContainText "prolog"
        | Ok r -> failwithf "expected Error, got Ok %A" r

    [<Test>]
    let ``rejects blob shorter than prolog`` () : unit =
        let blob = ImmutableArray.Create<byte> ([| 0x01uy |])

        match CustomAttribute.tryReadLeadingSerString blob with
        | Error _ -> ()
        | Ok r -> failwithf "expected Error, got Ok %A" r

    [<Test>]
    let ``rejects empty blob`` () : unit =
        match CustomAttribute.tryReadLeadingSerString ImmutableArray.Empty with
        | Error _ -> ()
        | Ok r -> failwithf "expected Error, got Ok %A" r

    [<Test>]
    let ``rejects truncated string body`` () : unit =
        // Prolog + claims-length-5 + only 2 body bytes
        let blob =
            ImmutableArray.Create<byte> ([| 0x01uy ; 0x00uy ; 0x05uy ; byte 'a' ; byte 'b' |])

        match CustomAttribute.tryReadLeadingSerString blob with
        | Error msg -> msg |> shouldContainText "truncated"
        | Ok r -> failwithf "expected Error, got Ok %A" r

    [<Test>]
    let ``rejects truncated two-byte length`` () : unit =
        // Prolog + first byte of a 2-byte packed length, no second byte
        let blob = ImmutableArray.Create<byte> ([| 0x01uy ; 0x00uy ; 0x80uy |])

        match CustomAttribute.tryReadLeadingSerString blob with
        | Error _ -> ()
        | Ok r -> failwithf "expected Error, got Ok %A" r

    [<Test>]
    let ``rejects truncated four-byte length`` () : unit =
        // Prolog + first byte (0xC0) of a 4-byte packed length, no remaining bytes
        let blob = ImmutableArray.Create<byte> ([| 0x01uy ; 0x00uy ; 0xC0uy |])

        match CustomAttribute.tryReadLeadingSerString blob with
        | Error _ -> ()
        | Ok r -> failwithf "expected Error, got Ok %A" r

    [<Test>]
    let ``ignores trailing bytes after SerString`` () : unit =
        // Real IVT blob has NumNamed (2 bytes) + named args after the SerString.
        // We only need the leading string; tail must be ignored.
        let prolog = [| 0x01uy ; 0x00uy |]
        let body = Encoding.UTF8.GetBytes "ok"
        let serString = Array.append (encodePackedLen body.Length) body
        let trailing = [| 0x00uy ; 0x00uy ; 0xAAuy ; 0xBBuy |]

        let blob =
            Array.concat [ prolog ; serString ; trailing ] |> ImmutableArray.Create<byte>

        CustomAttribute.tryReadLeadingSerString blob |> shouldEqual (Ok (Some "ok"))

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 200

    [<Test>]
    let ``round-trips arbitrary unicode strings`` () : unit =
        let property (NonNull (s : string)) : bool =
            let blob = buildBlob (Some s)

            match CustomAttribute.tryReadLeadingSerString blob with
            | Ok (Some out) -> out = s
            | _ -> false

        Check.One (propertyConfig, property)

    // ----- readFixedArgs ----------------------------------------------------

    /// Encode a single CustomAttribFixedArg per ECMA-335 II.23.3.
    let rec private encodeFixedArg (arg : CustomAttribFixedArg) : byte array =
        match arg with
        | CustomAttribFixedArg.Bool b -> [| (if b then 1uy else 0uy) |]
        | CustomAttribFixedArg.Char c ->
            let v = uint16 c
            [| byte (v &&& 0xFFus) ; byte ((v >>> 8) &&& 0xFFus) |]
        | CustomAttribFixedArg.I1 v -> [| byte v |]
        | CustomAttribFixedArg.U1 v -> [| v |]
        | CustomAttribFixedArg.I2 v ->
            let u = uint16 v
            [| byte (u &&& 0xFFus) ; byte ((u >>> 8) &&& 0xFFus) |]
        | CustomAttribFixedArg.U2 v -> [| byte (v &&& 0xFFus) ; byte ((v >>> 8) &&& 0xFFus) |]
        | CustomAttribFixedArg.I4 v -> System.BitConverter.GetBytes (v)
        | CustomAttribFixedArg.U4 v -> System.BitConverter.GetBytes (v)
        | CustomAttribFixedArg.I8 v -> System.BitConverter.GetBytes (v)
        | CustomAttribFixedArg.U8 v -> System.BitConverter.GetBytes (v)
        | CustomAttribFixedArg.R4 v -> System.BitConverter.GetBytes (v)
        | CustomAttribFixedArg.R8 v -> System.BitConverter.GetBytes (v)
        | CustomAttribFixedArg.String None -> [| 0xFFuy |]
        | CustomAttribFixedArg.String (Some s) ->
            let utf8 = Encoding.UTF8.GetBytes (s : string)
            Array.append (encodePackedLen utf8.Length) utf8
        | CustomAttribFixedArg.Array None -> System.BitConverter.GetBytes (0xFFFFFFFFu)
        | CustomAttribFixedArg.Array (Some elts) ->
            let count = System.BitConverter.GetBytes (uint32 (List.length elts))
            let body = elts |> List.collect (encodeFixedArg >> Array.toList) |> List.toArray
            Array.append count body
        // ECMA-335 II.23.3: an enum is encoded exactly as its underlying value, with no tag and
        // no name. That identity is the point of the `enum arg encodes exactly as its underlying
        // primitive` test below.
        | CustomAttribFixedArg.Enum underlying -> encodeFixedArg underlying

    /// Recover the declaration-site `CustomAttribArgShape` for a fixed arg. For SZARRAYs we
    /// recurse into the first element; empty and null arrays would lose the
    /// element type, so callers must supply the shape directly for those cases.
    let rec private shapeOfArg (arg : CustomAttribFixedArg) : CustomAttribArgShape =
        match arg with
        | CustomAttribFixedArg.Bool _ -> CustomAttribArgShape.Primitive PrimitiveType.Boolean
        | CustomAttribFixedArg.Char _ -> CustomAttribArgShape.Primitive PrimitiveType.Char
        | CustomAttribFixedArg.I1 _ -> CustomAttribArgShape.Primitive PrimitiveType.SByte
        | CustomAttribFixedArg.U1 _ -> CustomAttribArgShape.Primitive PrimitiveType.Byte
        | CustomAttribFixedArg.I2 _ -> CustomAttribArgShape.Primitive PrimitiveType.Int16
        | CustomAttribFixedArg.U2 _ -> CustomAttribArgShape.Primitive PrimitiveType.UInt16
        | CustomAttribFixedArg.I4 _ -> CustomAttribArgShape.Primitive PrimitiveType.Int32
        | CustomAttribFixedArg.U4 _ -> CustomAttribArgShape.Primitive PrimitiveType.UInt32
        | CustomAttribFixedArg.I8 _ -> CustomAttribArgShape.Primitive PrimitiveType.Int64
        | CustomAttribFixedArg.U8 _ -> CustomAttribArgShape.Primitive PrimitiveType.UInt64
        | CustomAttribFixedArg.R4 _ -> CustomAttribArgShape.Primitive PrimitiveType.Single
        | CustomAttribFixedArg.R8 _ -> CustomAttribArgShape.Primitive PrimitiveType.Double
        | CustomAttribFixedArg.String _ -> CustomAttribArgShape.Primitive PrimitiveType.String
        | CustomAttribFixedArg.Array (Some (head :: _)) -> CustomAttribArgShape.SzArray (shapeOfArg head)
        | CustomAttribFixedArg.Enum underlying ->
            match shapeOfArg underlying with
            | CustomAttribArgShape.Primitive p ->
                EnumUnderlyingType.ofPrimitive p
                |> Option.defaultWith (fun () ->
                    failwith $"shapeOfArg: %O{p} cannot be an enum's underlying type, so this arg is unconstructible"
                )
                |> CustomAttribArgShape.Enum
            | other -> failwith $"shapeOfArg: enum payload had non-primitive shape %O{other}"
        | CustomAttribFixedArg.Array None
        | CustomAttribFixedArg.Array (Some []) ->
            failwith
                "shapeOfArg cannot infer the element shape for a null or empty Array; supply the SZARRAY shape directly in the test"

    /// Build a CustomAttrib blob with the given fixed args and optional trailing bytes
    /// (intended to represent the NumNamed count + named-args section).
    let private buildFixedArgsBlob (args : CustomAttribFixedArg list) (trailing : byte array) : ImmutableArray<byte> =
        let prolog = [| 0x01uy ; 0x00uy |]
        let body = args |> List.collect (encodeFixedArg >> Array.toList) |> List.toArray
        Array.concat [ prolog ; body ; trailing ] |> ImmutableArray.Create<byte>

    [<Test>]
    let ``readFixedArgs decodes empty list`` () : unit =
        let blob = buildFixedArgsBlob [] [||]

        match CustomAttribute.readFixedArgs [] blob with
        | Ok (args, offset) ->
            args |> shouldEqual []
            offset |> shouldEqual 2
        | Error e -> failwithf "expected Ok, got Error %s" e

    [<Test>]
    let ``readFixedArgs decodes single bool true`` () : unit =
        let args = [ CustomAttribFixedArg.Bool true ]
        let blob = buildFixedArgsBlob args [||]

        match CustomAttribute.readFixedArgs (args |> List.map shapeOfArg) blob with
        | Ok (decoded, offset) ->
            decoded |> shouldEqual args
            offset |> shouldEqual 3
        | Error e -> failwithf "expected Ok, got Error %s" e

    [<Test>]
    let ``readFixedArgs reports offset of trailing bytes`` () : unit =
        let args =
            [ CustomAttribFixedArg.I4 0x11223344 ; CustomAttribFixedArg.String (Some "ok") ]

        let trailing = [| 0x05uy ; 0x06uy ; 0x07uy |]
        let blob = buildFixedArgsBlob args trailing

        match CustomAttribute.readFixedArgs (args |> List.map shapeOfArg) blob with
        | Ok (decoded, offset) ->
            decoded |> shouldEqual args
            // 2 (prolog) + 4 (int32) + 1 (PackedLen for "ok") + 2 ("ok")
            offset |> shouldEqual 9
            // Trailing bytes were not consumed.
            blob.[offset] |> shouldEqual 0x05uy
        | Error e -> failwithf "expected Ok, got Error %s" e

    [<Test>]
    let ``readFixedArgs decodes each primitive`` () : unit =
        let cases : CustomAttribFixedArg list =
            [
                CustomAttribFixedArg.Bool false
                CustomAttribFixedArg.Bool true
                CustomAttribFixedArg.Char 'A'
                CustomAttribFixedArg.Char '中'
                CustomAttribFixedArg.I1 -1y
                CustomAttribFixedArg.I1 127y
                CustomAttribFixedArg.U1 255uy
                CustomAttribFixedArg.I2 -12345s
                CustomAttribFixedArg.U2 65535us
                CustomAttribFixedArg.I4 -2147483648
                CustomAttribFixedArg.U4 4294967295u
                CustomAttribFixedArg.I8 -9223372036854775808L
                CustomAttribFixedArg.U8 18446744073709551615UL
                CustomAttribFixedArg.R4 3.14159f
                CustomAttribFixedArg.R8 2.718281828459045
                CustomAttribFixedArg.String None
                CustomAttribFixedArg.String (Some "")
                CustomAttribFixedArg.String (Some "hello")
            ]

        for arg in cases do
            let blob = buildFixedArgsBlob [ arg ] [||]

            match CustomAttribute.readFixedArgs [ shapeOfArg arg ] blob with
            | Ok ([ decoded ], _) -> decoded |> shouldEqual arg
            | Ok (decoded, _) -> failwithf "expected single arg for %A, got %A" arg decoded
            | Error e -> failwithf "expected Ok for %A, got Error %s" arg e

    [<Test>]
    let ``readFixedArgs rejects bad prolog`` () : unit =
        let blob = ImmutableArray.Create<byte> ([| 0x00uy ; 0x00uy ; 0x01uy |])

        match CustomAttribute.readFixedArgs [ CustomAttribArgShape.Primitive PrimitiveType.Boolean ] blob with
        | Error msg -> msg |> shouldContainText "prolog"
        | Ok r -> failwithf "expected Error, got Ok %A" r

    [<Test>]
    let ``readFixedArgs rejects truncated primitive`` () : unit =
        // Prolog + 3 bytes; an Int32 needs 4 body bytes.
        let blob =
            ImmutableArray.Create<byte> ([| 0x01uy ; 0x00uy ; 0xAAuy ; 0xBBuy ; 0xCCuy |])

        match CustomAttribute.readFixedArgs [ CustomAttribArgShape.Primitive PrimitiveType.Int32 ] blob with
        | Error _ -> ()
        | Ok r -> failwithf "expected Error, got Ok %A" r

    [<Test>]
    let ``readFixedArgs rejects unsupported primitive`` () : unit =
        let blob = ImmutableArray.Create<byte> ([| 0x01uy ; 0x00uy |])

        match CustomAttribute.readFixedArgs [ CustomAttribArgShape.Primitive PrimitiveType.Object ] blob with
        | Error msg -> msg |> shouldContainText "TODO"
        | Ok r -> failwithf "expected Error, got Ok %A" r

    let private genFixedArg : Gen<CustomAttribFixedArg> =
        let serString : Gen<string option> =
            Gen.frequency
                [
                    1, Gen.constant None
                    1, Gen.constant (Some "")
                    8,
                    ArbMap.defaults
                    |> ArbMap.generate<NonNull<string>>
                    |> Gen.map (fun (NonNull s) -> Some s)
                ]

        Gen.oneof
            [
                ArbMap.defaults |> ArbMap.generate<bool> |> Gen.map CustomAttribFixedArg.Bool
                ArbMap.defaults |> ArbMap.generate<char> |> Gen.map CustomAttribFixedArg.Char
                ArbMap.defaults |> ArbMap.generate<sbyte> |> Gen.map CustomAttribFixedArg.I1
                ArbMap.defaults |> ArbMap.generate<byte> |> Gen.map CustomAttribFixedArg.U1
                ArbMap.defaults |> ArbMap.generate<int16> |> Gen.map CustomAttribFixedArg.I2
                ArbMap.defaults |> ArbMap.generate<uint16> |> Gen.map CustomAttribFixedArg.U2
                ArbMap.defaults |> ArbMap.generate<int32> |> Gen.map CustomAttribFixedArg.I4
                ArbMap.defaults |> ArbMap.generate<uint32> |> Gen.map CustomAttribFixedArg.U4
                ArbMap.defaults |> ArbMap.generate<int64> |> Gen.map CustomAttribFixedArg.I8
                ArbMap.defaults |> ArbMap.generate<uint64> |> Gen.map CustomAttribFixedArg.U8
                ArbMap.defaults
                |> ArbMap.generate<NormalFloat>
                |> Gen.map (fun (NormalFloat f) -> CustomAttribFixedArg.R4 (float32 f))
                ArbMap.defaults
                |> ArbMap.generate<NormalFloat>
                |> Gen.map (fun (NormalFloat f) -> CustomAttribFixedArg.R8 f)
                serString |> Gen.map CustomAttribFixedArg.String
            ]

    [<Test>]
    let ``readFixedArgs round-trips arbitrary primitive arg lists`` () : unit =
        let property (args : CustomAttribFixedArg list) : bool =
            let blob = buildFixedArgsBlob args [||]
            let types = args |> List.map shapeOfArg

            match CustomAttribute.readFixedArgs types blob with
            | Ok (decoded, offset) -> decoded = args && offset = blob.Length
            | Error _ -> false

        let argsGen : Gen<CustomAttribFixedArg list> = Gen.listOf genFixedArg
        let argsArb : Arbitrary<CustomAttribFixedArg list> = Arb.fromGen argsGen
        Check.One (propertyConfig, Prop.forAll argsArb property)

    // ----- SZARRAY decoding ------------------------------------------------

    /// Convenience for the dominant Roslyn-emitted shape: NullableAttribute(byte[]).
    let private szarrayByte =
        CustomAttribArgShape.SzArray (CustomAttribArgShape.Primitive PrimitiveType.Byte)

    [<Test>]
    let ``readFixedArgs decodes null byte[]`` () : unit =
        // Prolog + NumElem = 0xFFFFFFFF (null sentinel).
        let blob =
            ImmutableArray.Create<byte> ([| 0x01uy ; 0x00uy ; 0xFFuy ; 0xFFuy ; 0xFFuy ; 0xFFuy |])

        match CustomAttribute.readFixedArgs [ szarrayByte ] blob with
        | Ok ([ CustomAttribFixedArg.Array None ], offset) -> offset |> shouldEqual blob.Length
        | other -> failwithf "expected Ok [Array None], got %A" other

    [<Test>]
    let ``readFixedArgs decodes empty byte[]`` () : unit =
        // Prolog + NumElem = 0.
        let blob =
            ImmutableArray.Create<byte> ([| 0x01uy ; 0x00uy ; 0x00uy ; 0x00uy ; 0x00uy ; 0x00uy |])

        match CustomAttribute.readFixedArgs [ szarrayByte ] blob with
        | Ok ([ CustomAttribFixedArg.Array (Some []) ], offset) -> offset |> shouldEqual blob.Length
        | other -> failwithf "expected Ok [Array (Some [])], got %A" other

    [<Test>]
    let ``readFixedArgs decodes single-byte byte[]`` () : unit =
        // The NullableAttribute(byte) shape: prolog + NumElem=1 + one byte.
        let arg = CustomAttribFixedArg.Array (Some [ CustomAttribFixedArg.U1 0x2Auy ])
        let blob = buildFixedArgsBlob [ arg ] [||]

        match CustomAttribute.readFixedArgs [ szarrayByte ] blob with
        | Ok ([ decoded ], offset) ->
            decoded |> shouldEqual arg
            offset |> shouldEqual blob.Length
        | other -> failwithf "expected Ok [single arg], got %A" other

    [<Test>]
    let ``readFixedArgs decodes multi-element int32[]`` () : unit =
        let arg =
            CustomAttribFixedArg.Array (
                Some
                    [
                        CustomAttribFixedArg.I4 -1
                        CustomAttribFixedArg.I4 0
                        CustomAttribFixedArg.I4 0x11223344
                    ]
            )

        let blob = buildFixedArgsBlob [ arg ] [||]

        let szarrayInt =
            CustomAttribArgShape.SzArray (CustomAttribArgShape.Primitive PrimitiveType.Int32)

        match CustomAttribute.readFixedArgs [ szarrayInt ] blob with
        | Ok ([ decoded ], offset) ->
            decoded |> shouldEqual arg
            offset |> shouldEqual blob.Length
        | other -> failwithf "expected Ok [single arg], got %A" other

    [<Test>]
    let ``readFixedArgs decodes string[] including null sentinels`` () : unit =
        let arg =
            CustomAttribFixedArg.Array (
                Some
                    [
                        CustomAttribFixedArg.String (Some "alpha")
                        CustomAttribFixedArg.String None
                        CustomAttribFixedArg.String (Some "")
                    ]
            )

        let blob = buildFixedArgsBlob [ arg ] [||]

        let szarrayStr =
            CustomAttribArgShape.SzArray (CustomAttribArgShape.Primitive PrimitiveType.String)

        match CustomAttribute.readFixedArgs [ szarrayStr ] blob with
        | Ok ([ decoded ], offset) ->
            decoded |> shouldEqual arg
            offset |> shouldEqual blob.Length
        | other -> failwithf "expected Ok [single arg], got %A" other

    [<Test>]
    let ``readFixedArgs reports truncated NumElem`` () : unit =
        // Prolog + only 3 of the 4 NumElem bytes.
        let blob =
            ImmutableArray.Create<byte> ([| 0x01uy ; 0x00uy ; 0x01uy ; 0x00uy ; 0x00uy |])

        match CustomAttribute.readFixedArgs [ szarrayByte ] blob with
        | Error _ -> ()
        | Ok r -> failwithf "expected Error, got Ok %A" r

    [<Test>]
    let ``readFixedArgs reports truncated final element`` () : unit =
        // Prolog + NumElem=2 + only one of the two int32 elements.
        let blob =
            ImmutableArray.Create<byte> (
                [|
                    0x01uy
                    0x00uy
                    0x02uy
                    0x00uy
                    0x00uy
                    0x00uy
                    0xAAuy
                    0xBBuy
                    0xCCuy
                    0xDDuy
                |]
            )

        let szarrayInt =
            CustomAttribArgShape.SzArray (CustomAttribArgShape.Primitive PrimitiveType.Int32)

        match CustomAttribute.readFixedArgs [ szarrayInt ] blob with
        | Error _ -> ()
        | Ok r -> failwithf "expected Error, got Ok %A" r

    /// Generators paired by element shape so SZARRAY tests can produce
    /// homogeneous element lists without losing the type for empty/null arrays.
    let private elementGens : (CustomAttribArgShape * Gen<CustomAttribFixedArg>) list =
        let prim (p : PrimitiveType) g = CustomAttribArgShape.Primitive p, g

        /// An enum element whose underlying type is `p`: same bytes as `p`, wrapped.
        let enumOf (p : PrimitiveType) (g : Gen<CustomAttribFixedArg>) =
            let underlying =
                EnumUnderlyingType.ofPrimitive p
                |> Option.defaultWith (fun () -> failwith $"%O{p} cannot underlie an enum")

            CustomAttribArgShape.Enum underlying, g |> Gen.map CustomAttribFixedArg.Enum

        let serString : Gen<string option> =
            Gen.frequency
                [
                    1, Gen.constant None
                    1, Gen.constant (Some "")
                    8,
                    ArbMap.defaults
                    |> ArbMap.generate<NonNull<string>>
                    |> Gen.map (fun (NonNull s) -> Some s)
                ]

        [
            prim PrimitiveType.Boolean (ArbMap.defaults |> ArbMap.generate<bool> |> Gen.map CustomAttribFixedArg.Bool)
            prim PrimitiveType.Char (ArbMap.defaults |> ArbMap.generate<char> |> Gen.map CustomAttribFixedArg.Char)
            prim PrimitiveType.SByte (ArbMap.defaults |> ArbMap.generate<sbyte> |> Gen.map CustomAttribFixedArg.I1)
            prim PrimitiveType.Byte (ArbMap.defaults |> ArbMap.generate<byte> |> Gen.map CustomAttribFixedArg.U1)
            prim PrimitiveType.Int16 (ArbMap.defaults |> ArbMap.generate<int16> |> Gen.map CustomAttribFixedArg.I2)
            prim PrimitiveType.UInt16 (ArbMap.defaults |> ArbMap.generate<uint16> |> Gen.map CustomAttribFixedArg.U2)
            prim PrimitiveType.Int32 (ArbMap.defaults |> ArbMap.generate<int32> |> Gen.map CustomAttribFixedArg.I4)
            prim PrimitiveType.UInt32 (ArbMap.defaults |> ArbMap.generate<uint32> |> Gen.map CustomAttribFixedArg.U4)
            prim PrimitiveType.Int64 (ArbMap.defaults |> ArbMap.generate<int64> |> Gen.map CustomAttribFixedArg.I8)
            prim PrimitiveType.UInt64 (ArbMap.defaults |> ArbMap.generate<uint64> |> Gen.map CustomAttribFixedArg.U8)
            prim
                PrimitiveType.Single
                (ArbMap.defaults
                 |> ArbMap.generate<NormalFloat>
                 |> Gen.map (fun (NormalFloat f) -> CustomAttribFixedArg.R4 (float32 f)))
            prim
                PrimitiveType.Double
                (ArbMap.defaults
                 |> ArbMap.generate<NormalFloat>
                 |> Gen.map (fun (NormalFloat f) -> CustomAttribFixedArg.R8 f))
            prim PrimitiveType.String (serString |> Gen.map CustomAttribFixedArg.String)
            // Enum elements, so the SZARRAY property also covers arrays of enums. A byte-underlying
            // enum beside an int64-underlying one is what makes a wrong-width read observable as a
            // decode failure rather than a wrong-but-plausible value.
            enumOf PrimitiveType.Byte (ArbMap.defaults |> ArbMap.generate<byte> |> Gen.map CustomAttribFixedArg.U1)
            enumOf PrimitiveType.Int64 (ArbMap.defaults |> ArbMap.generate<int64> |> Gen.map CustomAttribFixedArg.I8)
        ]

    [<Test>]
    let ``readFixedArgs round-trips SZARRAY of primitive`` () : unit =
        let property (eltIdx : NonNegativeInt) (kind : int) (elems : CustomAttribFixedArg list) : bool =
            let eltType, _ = elementGens.[(eltIdx.Get) % elementGens.Length]
            let szarrayType = CustomAttribArgShape.SzArray eltType

            let arg =
                match (kind % 3 + 3) % 3 with
                | 0 -> CustomAttribFixedArg.Array None
                | 1 -> CustomAttribFixedArg.Array (Some [])
                | _ -> CustomAttribFixedArg.Array (Some elems)

            let blob = buildFixedArgsBlob [ arg ] [||]

            match CustomAttribute.readFixedArgs [ szarrayType ] blob with
            | Ok ([ decoded ], offset) -> decoded = arg && offset = blob.Length
            | _ -> false

        // Element values are drawn from the chosen element type; build a coupled generator
        // so the property's `elems` list is homogeneous with respect to `eltIdx`.
        let coupledGen : Gen<NonNegativeInt * int * CustomAttribFixedArg list> =
            gen {
                let! idxArb = ArbMap.defaults |> ArbMap.generate<NonNegativeInt>
                let! kind = ArbMap.defaults |> ArbMap.generate<int>
                let _, eltGen = elementGens.[(idxArb.Get) % elementGens.Length]
                let! elems = Gen.listOf eltGen
                return idxArb, kind, elems
            }

        let arb = Arb.fromGen coupledGen

        Check.One (propertyConfig, Prop.forAll arb (fun (eltIdx, kind, elems) -> property eltIdx kind elems))

    // ----- enum fixed args --------------------------------------------------

    /// Every legal enum underlying type (ECMA-335 II.14.3), with a value chosen so that reading it
    /// at the wrong width, or with the wrong signedness, produces a different answer: each
    /// multi-byte value has distinct non-zero bytes, and each unsigned value is above its signed
    /// counterpart's range.
    let private underlyingCases : (EnumUnderlyingType * CustomAttribFixedArg * int) list =
        [
            EnumUnderlyingType.Boolean, CustomAttribFixedArg.Bool true, 1
            EnumUnderlyingType.Char, CustomAttribFixedArg.Char 'ሴ', 2
            EnumUnderlyingType.SByte, CustomAttribFixedArg.I1 -37y, 1
            EnumUnderlyingType.Byte, CustomAttribFixedArg.U1 200uy, 1
            EnumUnderlyingType.Int16, CustomAttribFixedArg.I2 -3000s, 2
            EnumUnderlyingType.UInt16, CustomAttribFixedArg.U2 60000us, 2
            EnumUnderlyingType.Int32, CustomAttribFixedArg.I4 -123456789, 4
            EnumUnderlyingType.UInt32, CustomAttribFixedArg.U4 4000000000u, 4
            EnumUnderlyingType.Int64, CustomAttribFixedArg.I8 -1234567890123456789L, 8
            EnumUnderlyingType.UInt64, CustomAttribFixedArg.U8 18000000000000000000UL, 8
        ]

    [<Test>]
    let ``readFixedArgs decodes an enum of each legal underlying type`` () : unit =
        for underlying, value, width in underlyingCases do
            let arg = CustomAttribFixedArg.Enum value
            let blob = buildFixedArgsBlob [ arg ] [||]

            match CustomAttribute.readFixedArgs [ CustomAttribArgShape.Enum underlying ] blob with
            | Ok ([ decoded ], offset) ->
                decoded |> shouldEqual arg
                // The offset is the point: a decoder that read the right value at the wrong width
                // would desynchronise every subsequent argument in the blob.
                offset |> shouldEqual (2 + width)
            | other -> failwithf "expected a single decoded enum for %O, got %A" underlying other

    [<Test>]
    let ``an enum arg encodes exactly as its underlying primitive`` () : unit =
        // ECMA-335 II.23.3 gives an enum fixed arg no tag of its own, so the same bytes must decode
        // either way depending only on the shape supplied. This is what forces the width to come
        // from the caller's resolution rather than from the blob.
        for underlying, value, _ in underlyingCases do
            let blob = buildFixedArgsBlob [ value ] [||]

            let asPrimitive =
                CustomAttribArgShape.Primitive (EnumUnderlyingType.toPrimitive underlying)

            let decodedAsPrimitive = CustomAttribute.readFixedArgs [ asPrimitive ] blob

            let decodedAsEnum =
                CustomAttribute.readFixedArgs [ CustomAttribArgShape.Enum underlying ] blob

            decodedAsPrimitive |> shouldEqual (Ok ([ value ], blob.Length))

            decodedAsEnum
            |> shouldEqual (Ok ([ CustomAttribFixedArg.Enum value ], blob.Length))

    [<Test>]
    let ``an enum arg does not consume the arguments after it`` () : unit =
        // A width bug is most damaging where it is least visible: the enum decodes to something
        // plausible and the *next* argument silently reads shifted bytes. Pin the whole sequence.
        let args =
            [
                CustomAttribFixedArg.Enum (CustomAttribFixedArg.U1 200uy)
                CustomAttribFixedArg.I4 4242
                CustomAttribFixedArg.Enum (CustomAttribFixedArg.I8 -1234567890123456789L)
                CustomAttribFixedArg.String (Some "tail")
            ]

        let blob = buildFixedArgsBlob args [||]

        let shapes =
            [
                CustomAttribArgShape.Enum EnumUnderlyingType.Byte
                CustomAttribArgShape.Primitive PrimitiveType.Int32
                CustomAttribArgShape.Enum EnumUnderlyingType.Int64
                CustomAttribArgShape.Primitive PrimitiveType.String
            ]

        CustomAttribute.readFixedArgs shapes blob
        |> shouldEqual (Ok (args, blob.Length))

    [<Test>]
    let ``EnumUnderlyingType admits exactly the ECMA-335 II 14 3 integer types`` () : unit =
        // `CustomAttribArgShape.Enum` takes an `EnumUnderlyingType` rather than a `PrimitiveType`
        // precisely so the decoder's enum arm needs no error branch. That guarantee is only worth
        // anything if this rejection is real.
        for rejected in
            [
                PrimitiveType.Single
                PrimitiveType.Double
                PrimitiveType.String
                PrimitiveType.TypedReference
                PrimitiveType.IntPtr
                PrimitiveType.UIntPtr
                PrimitiveType.Object
            ] do
            EnumUnderlyingType.ofPrimitive rejected |> shouldEqual None

        for underlying, _, _ in underlyingCases do
            EnumUnderlyingType.toPrimitive underlying
            |> EnumUnderlyingType.ofPrimitive
            |> shouldEqual (Some underlying)

    // ------------------------------------------------------------------
    // Named args (ECMA-335 II.23.3 `NamedArg`)
    // ------------------------------------------------------------------

    /// Encode a `FieldOrPropType`. Byte values are `CorSerializationType` (corhdr.h), which
    /// aliases `CorElementType` for BOOLEAN..R8, STRING and SZARRAY.
    let rec private encodeFieldOrPropType (t : CustomAttribFieldOrPropType) : byte array =
        match t with
        | CustomAttribFieldOrPropType.Primitive pt ->
            let b =
                match pt with
                | PrimitiveType.Boolean -> 0x02uy
                | PrimitiveType.Char -> 0x03uy
                | PrimitiveType.SByte -> 0x04uy
                | PrimitiveType.Byte -> 0x05uy
                | PrimitiveType.Int16 -> 0x06uy
                | PrimitiveType.UInt16 -> 0x07uy
                | PrimitiveType.Int32 -> 0x08uy
                | PrimitiveType.UInt32 -> 0x09uy
                | PrimitiveType.Int64 -> 0x0Auy
                | PrimitiveType.UInt64 -> 0x0Buy
                | PrimitiveType.Single -> 0x0Cuy
                | PrimitiveType.Double -> 0x0Duy
                | PrimitiveType.String -> 0x0Euy
                | other -> failwithf "%O has no CorSerializationType byte, so it is not encodable here" other

            [| b |]
        | CustomAttribFieldOrPropType.SzArray elt -> Array.append [| 0x1Duy |] (encodeFieldOrPropType elt)
        | CustomAttribFieldOrPropType.Type -> [| 0x50uy |]
        | CustomAttribFieldOrPropType.TaggedObject -> [| 0x51uy |]
        | CustomAttribFieldOrPropType.Enum typeName ->
            let name =
                match typeName with
                | None -> [| 0xFFuy |]
                | Some n ->
                    let utf8 = Encoding.UTF8.GetBytes (n : string)
                    Array.append (encodePackedLen utf8.Length) utf8

            Array.append [| 0x55uy |] name

    let private encodeSerString (s : string option) : byte array =
        match s with
        | None -> [| 0xFFuy |]
        | Some str ->
            let utf8 = Encoding.UTF8.GetBytes (str : string)
            Array.append (encodePackedLen utf8.Length) utf8

    /// Encode a whole `NamedArg`: kind tag, FieldOrPropType, member name, then the value.
    let private encodeNamedArg (header : CustomAttribNamedArgHeader) (value : CustomAttribFixedArg) : byte array =
        let kind =
            match header.Kind with
            | CustomAttribNamedArgKind.Field -> 0x53uy
            | CustomAttribNamedArgKind.Property -> 0x54uy

        Array.concat
            [
                [| kind |]
                encodeFieldOrPropType header.ElemType
                encodeSerString header.Name
                encodeFixedArg value
            ]

    let private immutable (bytes : byte array) : ImmutableArray<byte> = ImmutableArray.Create<byte> bytes

    [<Test>]
    let ``readFieldOrPropType decodes every primitive`` () : unit =
        let cases =
            [
                0x02uy, PrimitiveType.Boolean
                0x03uy, PrimitiveType.Char
                0x04uy, PrimitiveType.SByte
                0x05uy, PrimitiveType.Byte
                0x06uy, PrimitiveType.Int16
                0x07uy, PrimitiveType.UInt16
                0x08uy, PrimitiveType.Int32
                0x09uy, PrimitiveType.UInt32
                0x0Auy, PrimitiveType.Int64
                0x0Buy, PrimitiveType.UInt64
                0x0Cuy, PrimitiveType.Single
                0x0Duy, PrimitiveType.Double
                0x0Euy, PrimitiveType.String
            ]

        for tag, expected in cases do
            match CustomAttribute.readFieldOrPropType (immutable [| tag |]) 0 with
            | Ok (decoded, next) ->
                decoded |> shouldEqual (CustomAttribFieldOrPropType.Primitive expected)
                next |> shouldEqual 1
            | Error e -> failwithf "byte 0x%02X: expected Ok, got Error %s" tag e

    [<Test>]
    let ``readFieldOrPropType decodes Type and TaggedObject`` () : unit =
        match CustomAttribute.readFieldOrPropType (immutable [| 0x50uy |]) 0 with
        | Ok (decoded, next) ->
            decoded |> shouldEqual CustomAttribFieldOrPropType.Type
            next |> shouldEqual 1
        | Error e -> failwithf "expected Ok Type, got Error %s" e

        match CustomAttribute.readFieldOrPropType (immutable [| 0x51uy |]) 0 with
        | Ok (decoded, next) ->
            decoded |> shouldEqual CustomAttribFieldOrPropType.TaggedObject
            next |> shouldEqual 1
        | Error e -> failwithf "expected Ok TaggedObject, got Error %s" e

    [<Test>]
    let ``readFieldOrPropType decodes SzArray and nests`` () : unit =
        let single =
            CustomAttribFieldOrPropType.SzArray (CustomAttribFieldOrPropType.Primitive PrimitiveType.Int32)

        match CustomAttribute.readFieldOrPropType (immutable (encodeFieldOrPropType single)) 0 with
        | Ok (decoded, next) ->
            decoded |> shouldEqual single
            next |> shouldEqual 2
        | Error e -> failwithf "expected Ok, got Error %s" e

        // ECMA-335's grammar is recursive here; CoreCLR's named-arg path admits only one level.
        // This reader follows the grammar, and the resolution step is where arrays are refused.
        let nested = CustomAttribFieldOrPropType.SzArray single

        match CustomAttribute.readFieldOrPropType (immutable (encodeFieldOrPropType nested)) 0 with
        | Ok (decoded, next) ->
            decoded |> shouldEqual nested
            next |> shouldEqual 3
        | Error e -> failwithf "expected Ok nested, got Error %s" e

    [<Test>]
    let ``readFieldOrPropType decodes Enum with its type name`` () : unit =
        let name = "MyNs.MyEnum, MyAsm, Version=1.0.0.0"
        let t = CustomAttribFieldOrPropType.Enum (Some name)
        let encoded = encodeFieldOrPropType t

        match CustomAttribute.readFieldOrPropType (immutable encoded) 0 with
        | Ok (decoded, next) ->
            decoded |> shouldEqual t
            next |> shouldEqual encoded.Length
        | Error e -> failwithf "expected Ok, got Error %s" e

    [<Test>]
    let ``readFieldOrPropType rejects an unknown tag`` () : unit =
        match CustomAttribute.readFieldOrPropType (immutable [| 0x99uy |]) 0 with
        | Ok other -> failwithf "expected Error for tag 0x99, got Ok %A" other
        | Error e -> e |> shouldContainText "not a valid FieldOrPropType"

    [<Test>]
    let ``readFieldOrPropType rejects an empty slice`` () : unit =
        match CustomAttribute.readFieldOrPropType (immutable [||]) 0 with
        | Ok other -> failwithf "expected Error, got Ok %A" other
        | Error e -> e |> shouldContainText "FieldOrPropType begins at offset 0"

    [<Test>]
    let ``readNamedArgHeader distinguishes field from property`` () : unit =
        for tag, expected in
            [
                0x53uy, CustomAttribNamedArgKind.Field
                0x54uy, CustomAttribNamedArgKind.Property
            ] do
            let bytes = Array.concat [ [| tag ; 0x08uy |] ; encodeSerString (Some "N") ]

            match CustomAttribute.readNamedArgHeader (immutable bytes) 0 with
            | Ok (header, next) ->
                header.Kind |> shouldEqual expected

                header.ElemType
                |> shouldEqual (CustomAttribFieldOrPropType.Primitive PrimitiveType.Int32)

                header.Name |> shouldEqual (Some "N")
                next |> shouldEqual bytes.Length
            | Error e -> failwithf "tag 0x%02X: expected Ok, got Error %s" tag e

    /// Both the enum type name and the member name are SerStrings, so telling the two fields apart
    /// requires a header carrying two *distinct* strings with assertions on each. The truncation
    /// cases below cannot distinguish them, since either order consumes the same byte count.
    [<Test>]
    let ``readNamedArgHeader reads the enum type name before the member name`` () : unit =
        let header =
            {
                Kind = CustomAttribNamedArgKind.Property
                ElemType = CustomAttribFieldOrPropType.Enum (Some "Some.Enum.Type")
                Name = Some "TheMemberName"
            }

        let bytes =
            Array.concat
                [
                    [| 0x54uy |]
                    encodeFieldOrPropType header.ElemType
                    encodeSerString header.Name
                ]

        match CustomAttribute.readNamedArgHeader (immutable bytes) 0 with
        | Ok (decoded, next) ->
            decoded.ElemType
            |> shouldEqual (CustomAttribFieldOrPropType.Enum (Some "Some.Enum.Type"))

            decoded.Name |> shouldEqual (Some "TheMemberName")
            next |> shouldEqual bytes.Length
        | Error e -> failwithf "expected Ok, got Error %s" e

    [<Test>]
    let ``readNamedArgHeader preserves a null member name`` () : unit =
        let bytes = [| 0x53uy ; 0x08uy ; 0xFFuy |]

        match CustomAttribute.readNamedArgHeader (immutable bytes) 0 with
        | Ok (header, next) ->
            header.Name |> shouldEqual None
            next |> shouldEqual 3
        | Error e -> failwithf "expected Ok, got Error %s" e

    [<Test>]
    let ``readNamedArgHeader rejects a bad kind tag`` () : unit =
        match CustomAttribute.readNamedArgHeader (immutable [| 0x52uy ; 0x08uy ; 0x00uy |]) 0 with
        | Ok other -> failwithf "expected Error, got Ok %A" other
        | Error e -> e |> shouldContainText "neither FIELD (0x53) nor PROPERTY (0x54)"

    /// Truncate a well-formed named arg at every length short of complete and assert each one is a
    /// clean Error rather than a crash or a silent short read. This covers "before the kind tag",
    /// "before the type byte", "mid enum name", "mid member name" and "mid value" without having to
    /// hand-pick the offsets.
    [<Test>]
    let ``readNamedArgHeader and readElem reject every truncation`` () : unit =
        let header =
            {
                Kind = CustomAttribNamedArgKind.Property
                ElemType = CustomAttribFieldOrPropType.Primitive PrimitiveType.String
                Name = Some "Label"
            }

        let full = encodeNamedArg header (CustomAttribFixedArg.String (Some "hello"))

        for len in 0 .. full.Length - 1 do
            let truncated = immutable (Array.sub full 0 len)

            let outcome =
                match CustomAttribute.readNamedArgHeader truncated 0 with
                | Error _ -> Error "header"
                | Ok (h, valueOffset) ->
                    CustomAttribute.readElem (CustomAttribArgShape.Primitive PrimitiveType.String) truncated valueOffset
                    |> Result.map (fun _ -> h)

            match outcome with
            | Error _ -> ()
            | Ok _ -> failwithf "truncation to %d byte(s) of %d decoded successfully; it must not" len full.Length

    [<Test>]
    let ``readNamedArgHeader plus readElem round-trips an arbitrary primitive named arg`` () : unit =
        let genKind =
            Gen.elements [ CustomAttribNamedArgKind.Field ; CustomAttribNamedArgKind.Property ]

        // Include the empty string, the null string and non-ASCII: a narrowed alphabet is exactly
        // what hides an encoder-and-decoder-agree bug.
        let genName =
            Gen.oneof
                [
                    Gen.constant None
                    Gen.constant (Some "")
                    Gen.constant (Some "é中\U0001F600")
                    ArbMap.defaults
                    |> ArbMap.generate<NonNull<string>>
                    |> Gen.map (fun s -> Some s.Get)
                ]

        // Full-range numerics: FsCheck's default int generator is size-bounded to roughly
        // [-100, 100] under Quick, which would never exercise the wide encodings.
        let genValue : Gen<CustomAttribFixedArg> =
            Gen.oneof
                [
                    Gen.elements [ true ; false ] |> Gen.map CustomAttribFixedArg.Bool
                    Gen.choose (0, 0xFFFF) |> Gen.map (fun c -> CustomAttribFixedArg.Char (char c))
                    Gen.choose (-128, 127) |> Gen.map (sbyte >> CustomAttribFixedArg.I1)
                    Gen.choose (0, 255) |> Gen.map (byte >> CustomAttribFixedArg.U1)
                    Gen.choose (-32768, 32767) |> Gen.map (int16 >> CustomAttribFixedArg.I2)
                    Gen.choose (0, 65535) |> Gen.map (uint16 >> CustomAttribFixedArg.U2)
                    Gen.choose (System.Int32.MinValue, System.Int32.MaxValue)
                    |> Gen.map CustomAttribFixedArg.I4
                    Gen.choose (System.Int32.MinValue, System.Int32.MaxValue)
                    |> Gen.map (uint32 >> CustomAttribFixedArg.U4)
                    Gen.choose64 (System.Int64.MinValue, System.Int64.MaxValue)
                    |> Gen.map CustomAttribFixedArg.I8
                    Gen.choose64 (System.Int64.MinValue, System.Int64.MaxValue)
                    |> Gen.map (uint64 >> CustomAttribFixedArg.U8)
                    // Generated from raw bits rather than via NormalFloat, so NaN payloads and
                    // infinities are in the alphabet; comparison below is bitwise for the same
                    // reason.
                    Gen.choose (System.Int32.MinValue, System.Int32.MaxValue)
                    |> Gen.map (fun b -> CustomAttribFixedArg.R4 (System.BitConverter.Int32BitsToSingle b))
                    Gen.choose64 (System.Int64.MinValue, System.Int64.MaxValue)
                    |> Gen.map (fun b -> CustomAttribFixedArg.R8 (System.BitConverter.Int64BitsToDouble b))
                    genName |> Gen.map CustomAttribFixedArg.String
                ]

        let property (kind : CustomAttribNamedArgKind) (name : string option) (value : CustomAttribFixedArg) =
            let primitive =
                match shapeOfArg value with
                | CustomAttribArgShape.Primitive pt -> pt
                | other -> failwithf "generator produced non-primitive shape %O" other

            let header =
                {
                    Kind = kind
                    ElemType = CustomAttribFieldOrPropType.Primitive primitive
                    Name = name
                }

            let bytes = immutable (encodeNamedArg header value)

            match CustomAttribute.readNamedArgHeader bytes 0 with
            | Error e -> failwithf "header decode failed: %s" e
            | Ok (decodedHeader, valueOffset) ->

            decodedHeader |> shouldEqual header

            match CustomAttribute.readElem (CustomAttribArgShape.Primitive primitive) bytes valueOffset with
            | Error e -> failwithf "value decode failed: %s" e
            | Ok (decodedValue, next) ->

            // Bitwise for floats, so a NaN round-trip is a pass rather than a structural mismatch.
            let sameValue =
                match value, decodedValue with
                | CustomAttribFixedArg.R4 a, CustomAttribFixedArg.R4 b ->
                    System.BitConverter.SingleToInt32Bits a = System.BitConverter.SingleToInt32Bits b
                | CustomAttribFixedArg.R8 a, CustomAttribFixedArg.R8 b ->
                    System.BitConverter.DoubleToInt64Bits a = System.BitConverter.DoubleToInt64Bits b
                | a, b -> a = b

            if not sameValue then
                failwithf "value round-trip mismatch: encoded %A, decoded %A" value decodedValue

            next |> shouldEqual bytes.Length

        let arb = Gen.zip3 genKind genName genValue |> Arb.fromGen

        Prop.forAll arb (fun (kind, name, value) -> property kind name value)
        |> Check.QuickThrowOnFailure

    /// Minimal `ICustomAttributeTypeProvider` for the oracle test below. The corpus is primitives
    /// and strings only, so the type-name-resolving members are unreachable and say so.
    type private OracleTypeProvider () =
        interface System.Reflection.Metadata.ICustomAttributeTypeProvider<string> with
            member _.GetPrimitiveType (code : System.Reflection.Metadata.PrimitiveTypeCode) : string = string code
            member _.GetSystemType () : string = "System.Type"
            member _.GetSZArrayType (elementType : string) : string = elementType + "[]"
            member _.IsSystemType (_ : string) : bool = false

            member _.GetTypeFromSerializedName (name : string) : string =
                failwithf "oracle corpus should contain no serialized type names, got %s" name

            member _.GetUnderlyingEnumType (type_ : string) : System.Reflection.Metadata.PrimitiveTypeCode =
                failwithf "oracle corpus should contain no enums, got %s" type_

            member _.GetTypeFromDefinition (_, _, _) : string =
                failwith "oracle corpus should reference no TypeDefs"

            member _.GetTypeFromReference (_, _, _) : string =
                failwith "oracle corpus should reference no TypeRefs"

    /// <summary>
    /// Differential test against <c>System.Reflection.Metadata</c>'s own <c>CustomAttributeDecoder</c>.
    /// </summary>
    /// <remarks>
    /// The round-trip property above encodes with a reference encoder that this repository also
    /// wrote, so it cannot catch a misreading of ECMA-335 II.23.3 that the encoder shares. This can:
    /// the blob comes from Roslyn and the expectation comes from an independent decoder shipped by
    /// the BCL.
    /// </remarks>
    [<Test>]
    let ``named-arg decode agrees with System.Reflection.Metadata`` () : unit =
        let source =
            """
using System;

[AttributeUsage(AttributeTargets.Class, AllowMultiple = true)]
public sealed class SampleAttribute : Attribute
{
    public SampleAttribute(int ctorArg) { CtorArg = ctorArg; }
    public int CtorArg { get; }

    public string Str { get; set; }
    public bool Flag { get; set; }
    public char Ch { get; set; }
    public sbyte I1 { get; set; }
    public byte U1 { get; set; }
    public short I2 { get; set; }
    public ushort U2 { get; set; }
    public int I4;
    public uint U4;
    public long I8;
    public ulong U8;
    public float R4 { get; set; }
    public double R8 { get; set; }
    public string Empty { get; set; }
    public string Nul { get; set; }
}

[Sample(7, Str = "hello", Flag = true, Ch = 'Z', I1 = -128, U1 = 255, I2 = -32768, U2 = 65535,
        I4 = int.MinValue, U4 = uint.MaxValue, I8 = long.MinValue, U8 = ulong.MaxValue,
        R4 = 1.5f, R8 = -2.25, Empty = "", Nul = null)]
public sealed class Decorated { }

public static class Program { public static int Main() => 0; }
"""

        let image = Roslyn.compile [ source ]

        use peReader =
            new System.Reflection.PortableExecutable.PEReader (ImmutableArray.Create<byte> image)

        let reader : System.Reflection.Metadata.MetadataReader =
            System.Reflection.Metadata.PEReaderExtensions.GetMetadataReader peReader

        let decoratedHandle =
            reader.TypeDefinitions
            |> Seq.find (fun h -> reader.GetString ((reader.GetTypeDefinition h).Name) = "Decorated")

        let attrHandle =
            (reader.GetTypeDefinition decoratedHandle).GetCustomAttributes ()
            |> Seq.exactlyOne

        let attr = reader.GetCustomAttribute attrHandle
        let blob = ImmutableArray.CreateRange (reader.GetBlobBytes attr.Value)

        // The oracle: the BCL's own decoder over the same bytes.
        let oracle = attr.DecodeValue (OracleTypeProvider ())

        // Walk to the named-args region using readFixedArgs (separately tested), then read the
        // uint16 NumNamed count that follows it.
        let afterFixed =
            match CustomAttribute.readFixedArgs [ CustomAttribArgShape.Primitive PrimitiveType.Int32 ] blob with
            | Ok (_, next) -> next
            | Error e -> failwithf "could not walk the fixed args: %s" e

        let namedCount =
            int (uint16 blob.[afterFixed] ||| (uint16 blob.[afterFixed + 1] <<< 8))

        namedCount |> shouldEqual oracle.NamedArguments.Length

        let mutable cursor = afterFixed + 2

        for i in 0 .. namedCount - 1 do
            let expected = oracle.NamedArguments.[i]

            match CustomAttribute.readNamedArgHeader blob cursor with
            | Error e -> failwithf "named arg %d: header decode failed: %s" i e
            | Ok (header, valueOffset) ->

            let expectedKind =
                match expected.Kind with
                | System.Reflection.Metadata.CustomAttributeNamedArgumentKind.Field -> CustomAttribNamedArgKind.Field
                | System.Reflection.Metadata.CustomAttributeNamedArgumentKind.Property ->
                    CustomAttribNamedArgKind.Property
                | other -> failwithf "unexpected named-arg kind %O" other

            header.Kind |> shouldEqual expectedKind
            header.Name |> shouldEqual (Some expected.Name)

            let shape =
                match header.ElemType with
                | CustomAttribFieldOrPropType.Primitive pt -> CustomAttribArgShape.Primitive pt
                | other -> failwithf "named arg %d (%s): corpus should be primitives only, got %O" i expected.Name other

            match CustomAttribute.readElem shape blob valueOffset with
            | Error e -> failwithf "named arg %d (%s): value decode failed: %s" i expected.Name e
            | Ok (decoded, next) ->

            // SRM hands back the value boxed as its CLR type; compare against ours unwrapped.
            let ours : obj =
                match decoded with
                | CustomAttribFixedArg.Bool b -> box b
                | CustomAttribFixedArg.Char c -> box c
                | CustomAttribFixedArg.I1 v -> box v
                | CustomAttribFixedArg.U1 v -> box v
                | CustomAttribFixedArg.I2 v -> box v
                | CustomAttribFixedArg.U2 v -> box v
                | CustomAttribFixedArg.I4 v -> box v
                | CustomAttribFixedArg.U4 v -> box v
                | CustomAttribFixedArg.I8 v -> box v
                | CustomAttribFixedArg.U8 v -> box v
                | CustomAttribFixedArg.R4 v -> box v
                | CustomAttribFixedArg.R8 v -> box v
                | CustomAttribFixedArg.String None -> null
                | CustomAttribFixedArg.String (Some s) -> box s
                | other -> failwithf "named arg %d (%s): unexpected decoded shape %A" i expected.Name other

            if not (System.Object.Equals (ours, expected.Value)) then
                failwithf
                    "named arg %d (%s): we decoded %A, System.Reflection.Metadata decoded %A"
                    i
                    expected.Name
                    ours
                    expected.Value

            cursor <- next

        // Mirrors the managed caller's `blobStart != blobEnd` check: a cursor that drifts anywhere
        // in the loop above lands here even if every individual value happened to match.
        cursor |> shouldEqual blob.Length
