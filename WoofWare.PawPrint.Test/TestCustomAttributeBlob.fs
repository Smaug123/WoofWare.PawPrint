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
