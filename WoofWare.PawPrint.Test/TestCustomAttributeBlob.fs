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
    let private encodeFixedArg (arg : CustomAttribFixedArg) : byte array =
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

    let private typeOfArg (arg : CustomAttribFixedArg) : TypeDefn =
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
