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

    /// Recover the declaration-site `TypeDefn` for a fixed arg. For SZARRAYs we
    /// recurse into the first element; empty and null arrays would lose the
    /// element type, so callers must supply the type directly for those cases.
    let rec private typeOfArg (arg : CustomAttribFixedArg) : TypeDefn =
        match arg with
        | CustomAttribFixedArg.Bool _ -> TypeDefn.PrimitiveType PrimitiveType.Boolean
        | CustomAttribFixedArg.Char _ -> TypeDefn.PrimitiveType PrimitiveType.Char
        | CustomAttribFixedArg.I1 _ -> TypeDefn.PrimitiveType PrimitiveType.SByte
        | CustomAttribFixedArg.U1 _ -> TypeDefn.PrimitiveType PrimitiveType.Byte
        | CustomAttribFixedArg.I2 _ -> TypeDefn.PrimitiveType PrimitiveType.Int16
        | CustomAttribFixedArg.U2 _ -> TypeDefn.PrimitiveType PrimitiveType.UInt16
        | CustomAttribFixedArg.I4 _ -> TypeDefn.PrimitiveType PrimitiveType.Int32
        | CustomAttribFixedArg.U4 _ -> TypeDefn.PrimitiveType PrimitiveType.UInt32
        | CustomAttribFixedArg.I8 _ -> TypeDefn.PrimitiveType PrimitiveType.Int64
        | CustomAttribFixedArg.U8 _ -> TypeDefn.PrimitiveType PrimitiveType.UInt64
        | CustomAttribFixedArg.R4 _ -> TypeDefn.PrimitiveType PrimitiveType.Single
        | CustomAttribFixedArg.R8 _ -> TypeDefn.PrimitiveType PrimitiveType.Double
        | CustomAttribFixedArg.String _ -> TypeDefn.PrimitiveType PrimitiveType.String
        | CustomAttribFixedArg.Array (Some (head :: _)) -> TypeDefn.OneDimensionalArrayLowerBoundZero (typeOfArg head)
        | CustomAttribFixedArg.Array None
        | CustomAttribFixedArg.Array (Some []) ->
            failwith
                "typeOfArg cannot infer element TypeDefn for a null or empty Array; supply the SZARRAY type directly in the test"

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

        match CustomAttribute.readFixedArgs (args |> List.map typeOfArg) blob with
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

        match CustomAttribute.readFixedArgs (args |> List.map typeOfArg) blob with
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

            match CustomAttribute.readFixedArgs [ typeOfArg arg ] blob with
            | Ok ([ decoded ], _) -> decoded |> shouldEqual arg
            | Ok (decoded, _) -> failwithf "expected single arg for %A, got %A" arg decoded
            | Error e -> failwithf "expected Ok for %A, got Error %s" arg e

    [<Test>]
    let ``readFixedArgs rejects bad prolog`` () : unit =
        let blob = ImmutableArray.Create<byte> ([| 0x00uy ; 0x00uy ; 0x01uy |])

        match CustomAttribute.readFixedArgs [ TypeDefn.PrimitiveType PrimitiveType.Boolean ] blob with
        | Error msg -> msg |> shouldContainText "prolog"
        | Ok r -> failwithf "expected Error, got Ok %A" r

    [<Test>]
    let ``readFixedArgs rejects truncated primitive`` () : unit =
        // Prolog + 3 bytes; an Int32 needs 4 body bytes.
        let blob =
            ImmutableArray.Create<byte> ([| 0x01uy ; 0x00uy ; 0xAAuy ; 0xBBuy ; 0xCCuy |])

        match CustomAttribute.readFixedArgs [ TypeDefn.PrimitiveType PrimitiveType.Int32 ] blob with
        | Error _ -> ()
        | Ok r -> failwithf "expected Error, got Ok %A" r

    [<Test>]
    let ``readFixedArgs rejects unsupported primitive`` () : unit =
        let blob = ImmutableArray.Create<byte> ([| 0x01uy ; 0x00uy |])

        match CustomAttribute.readFixedArgs [ TypeDefn.PrimitiveType PrimitiveType.Object ] blob with
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
            let types = args |> List.map typeOfArg

            match CustomAttribute.readFixedArgs types blob with
            | Ok (decoded, offset) -> decoded = args && offset = blob.Length
            | Error _ -> false

        let argsGen : Gen<CustomAttribFixedArg list> = Gen.listOf genFixedArg
        let argsArb : Arbitrary<CustomAttribFixedArg list> = Arb.fromGen argsGen
        Check.One (propertyConfig, Prop.forAll argsArb property)

    // ----- SZARRAY decoding ------------------------------------------------

    /// Convenience for the dominant Roslyn-emitted shape: NullableAttribute(byte[]).
    let private szarrayByte =
        TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.PrimitiveType PrimitiveType.Byte)

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
            TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.PrimitiveType PrimitiveType.Int32)

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
            TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.PrimitiveType PrimitiveType.String)

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
            TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.PrimitiveType PrimitiveType.Int32)

        match CustomAttribute.readFixedArgs [ szarrayInt ] blob with
        | Error _ -> ()
        | Ok r -> failwithf "expected Error, got Ok %A" r

    /// Generators paired by element TypeDefn so SZARRAY tests can produce
    /// homogeneous element lists without losing the type for empty/null arrays.
    let private elementGens : (TypeDefn * Gen<CustomAttribFixedArg>) list =
        let prim (p : PrimitiveType) g = TypeDefn.PrimitiveType p, g

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
        ]

    [<Test>]
    let ``readFixedArgs round-trips SZARRAY of primitive`` () : unit =
        let property (eltIdx : NonNegativeInt) (kind : int) (elems : CustomAttribFixedArg list) : bool =
            let eltType, _ = elementGens.[(eltIdx.Get) % elementGens.Length]
            let szarrayType = TypeDefn.OneDimensionalArrayLowerBoundZero eltType

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
