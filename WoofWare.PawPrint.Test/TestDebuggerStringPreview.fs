namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Text
open System.Text.Json
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The heap listing previews a guest string, which may hold lone surrogates that `Utf8JsonWriter`
/// refuses to write.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDebuggerStringPreview =

    /// Whether `text` is well-formed UTF-16, by a strict encoder rather than by hand.
    let private wellFormed (text : string) : bool =
        try
            UTF8Encoding(false, true).GetBytes text |> ignore
            true
        with :? EncoderFallbackException ->
            false

    /// Text built from ordinary characters, valid surrogate pairs and lone surrogate halves, either
    /// short or long enough to straddle the preview's cut with every kind of chunk.
    let private genText : Gen<string> =
        let chunk =
            Gen.frequency
                [
                    6, Gen.elements [ "a" ; "é" ; "中" ]
                    2, Gen.elements [ "😀" ; "􏿿" ; "𐀀" ]
                    1, Gen.elements [ "\ud800" ; "\udbff" ]
                    1, Gen.elements [ "\udc00" ; "\udfff" ]
                ]

        gen {
            let! count = Gen.oneof [ Gen.choose (0, 20) ; Gen.choose (200, 270) ]

            let! chunks = Gen.listOfLength count chunk
            return String.concat "" chunks
        }

    let private jsonRoundTrip (text : string) : string =
        use stream = new MemoryStream ()

        do
            use writer = new Utf8JsonWriter (stream)
            writer.WriteStringValue text

        use document = JsonDocument.Parse (stream.ToArray ())
        document.RootElement.GetString ()

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 5000

    [<Test>]
    let ``A string preview is the string's prefix, writable as JSON, and present for well-formed text`` () : unit =
        let property (text : string) : unit =
            let preview, truncated = DebuggerServer.stringPreview text
            truncated |> shouldEqual (text.Length > DebuggerServer.stringPreviewLength)

            let expectedLength =
                if not truncated then
                    text.Length
                elif Char.IsHighSurrogate text.[DebuggerServer.stringPreviewLength - 1] then
                    DebuggerServer.stringPreviewLength - 1
                else
                    DebuggerServer.stringPreviewLength

            let prefix = text.Substring (0, expectedLength)

            match preview with
            | Some preview ->
                preview |> shouldEqual prefix
                wellFormed preview |> shouldEqual true
                jsonRoundTrip preview |> shouldEqual preview
            | None -> wellFormed prefix |> shouldEqual false

            // Truncation never costs a well-formed string its preview.
            if wellFormed text then
                preview |> shouldEqual (Some prefix)

        Check.One (config, Prop.forAll (Arb.fromGen genText) property)
