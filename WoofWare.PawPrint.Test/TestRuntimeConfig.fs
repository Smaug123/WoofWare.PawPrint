namespace WoofWare.PawPrint.Test

open System
open System.Text.Json
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Tests for the `runtimeconfig.json` reader.
///
/// The oracle throughout is `hostpolicy`, which is what populates `AppContext` on a real
/// runtime: `runtime_config.cpp`'s `configProperties` loop takes `GetString()` for a JSON
/// string and otherwise re-serialises the value with a `rapidjson::Writer`. The expected
/// strings in `theMeasuredHostpolicyMapping` below were not derived from that source by
/// reading — they were *measured*, by handing a real .NET 10 host a `runtimeconfig.json`
/// containing each shape and printing what `AppContext.GetData` returned.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestRuntimeConfig =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    /// Wrap a `configProperties` body in the surrounding document the SDK emits.
    let private document (configProperties : string) : string =
        $$"""
        {
          "runtimeOptions": {
            "tfm": "net10.0",
            "framework": { "name": "Microsoft.NETCore.App", "version": "10.0.0" },
            "configProperties": {{configProperties}}
          }
        }
        """

    let private parseOk (json : string) : Map<string, string> =
        match RuntimeConfig.parse json with
        | Ok props -> AppContextProperties.toMap props
        | Error e -> failwith $"expected a successful parse, but got: %s{e}"

    let private parseError (json : string) : string =
        match RuntimeConfig.parse json with
        | Error e -> e
        | Ok props -> failwith $"expected a failed parse, but got: %O{AppContextProperties.toMap props |> Map.toList}"

    // ------------------------------------------------------------------
    // The measured oracle.
    // ------------------------------------------------------------------

    /// Every row here was produced by a real .NET 10 host, not by reading hostpolicy's
    /// source. If one of these ever fails, PawPrint has started lying to guests about
    /// what their own `runtimeconfig.json` says.
    let private theMeasuredHostpolicyMapping : (string * string) list =
        [
            // Strings arrive unescaped: hostpolicy takes `GetString()`, so what the guest
            // sees is the *decoded* value, not the JSON spelling of it.
            "\"hello world\"", "hello world"
            "\"he said \\\"hi\\\"\"", "he said \"hi\""
            "\"a\\\\b\\tc\\nd\"", "a\\b\tc\nd"
            "\"\"", ""
            "\"\\ud83d\\udc36 pawprint\"", "\U0001F436 pawprint"
            // Booleans are the shape every feature switch uses, `EventSource.IsSupported`
            // included, and are what `AppContext.TryGetSwitch` can `bool.TryParse`.
            "true", "true"
            "false", "false"
            // Integers keep full int64/uint64 precision: rapidjson stores them as integers,
            // so this is exact rather than routed through a double.
            "50", "50"
            "-7", "-7"
            "2147483648", "2147483648"
            "9007199254740993", "9007199254740993" // not representable as a double
            "0", "0"
            "-0", "0" // canonicalised: rapidjson has no negative integer zero
            "-9223372036854775808", "-9223372036854775808" // int64 min
            "18446744073709551615", "18446744073709551615" // uint64 max
            "null", "null"
        ]

    [<Test>]
    let ``matches the measured hostpolicy mapping`` () =
        for jsonValue, expected in theMeasuredHostpolicyMapping do
            let parsed = parseOk (document $"""{{ "P": {jsonValue} }}""")
            parsed |> shouldEqual (Map.ofList [ "P", expected ])

    // ------------------------------------------------------------------
    // Values we deliberately refuse.
    // ------------------------------------------------------------------

    /// Reals, arrays and objects are re-serialised by rapidjson's `Writer`, which for
    /// numbers means Grisu2 plus `dtoa.h`'s five-branch `Prettify` — `1e2` becomes
    /// `100.0`, `0.1000000000000000055511151231257827` becomes `0.1`, `1e-7` stays
    /// `1e-7`. PawPrint does not reproduce that formatting, so rather than seed a string
    /// that differs from what the guest would see on CoreCLR, we refuse the file and say
    /// so. Each of these was measured too; `wouldHaveBeen` is what a real host produces.
    ///
    /// The third component is the classification the diagnostic must report. It is pinned
    /// because `JsonElement.TryGetInt64` happens to reject anything with a fraction or an
    /// exponent all by itself, so the integral/real classification does not change *whether*
    /// we refuse — only what we tell the user. Left untested, it could silently start
    /// describing `1.5` as a number that "exceeds int64", which is simply false.
    let private theRefusedShapes : (string * string * string) list =
        [
            "1.5", "1.5", "non-integer numeric"
            "1.0", "1.0", "non-integer numeric"
            "1e2", "100.0", "non-integer numeric"
            "1E+2", "100.0", "non-integer numeric"
            "-0.0", "-0.0", "non-integer numeric"
            "5.0", "5.0", "non-integer numeric"
            "1e-7", "1e-7", "non-integer numeric"
            "0.1000000000000000055511151231257827", "0.1", "non-integer numeric"
            // Integer-shaped, but wider than uint64, so a real host holds it as a double.
            "99999999999999999999", "100000000000000000000.0", "out-of-range"
            """[1, "two", false]""", """[1,"two",false]""", "array"
            """{"a": 1, "b": [2]}""", """{"a":1,"b":[2]}""", "object"
        ]

    [<Test>]
    let ``refuses the value shapes whose formatting we do not reproduce`` () =
        for jsonValue, _wouldHaveBeen, expectedKind in theRefusedShapes do
            let err = parseError (document $"""{{ "Some.Property": {jsonValue} }}""")
            // The diagnostic has to name the offending key, or a user with a 40-property
            // config has no way to find it, and has to classify it truthfully.
            err |> shouldContainText "Some.Property"
            err |> shouldContainText expectedKind

    [<Test>]
    let ``a refused value does not discard the properties around it`` () =
        // Whole-file refusal is deliberate: seeding a partial property set would leave a
        // guest silently missing a feature switch, which is the failure we are avoiding.
        parseError (document """{ "Good": "yes", "Bad": 1.5, "AlsoGood": true }""")
        |> shouldContainText "Bad"

    // ------------------------------------------------------------------
    // Document structure.
    // ------------------------------------------------------------------

    [<Test>]
    let ``absent configProperties yields no properties`` () =
        // hostpolicy's `FindMember` simply misses, which is not an error: a
        // runtimeconfig.json with no configProperties is completely normal.
        parseOk """{ "runtimeOptions": { "tfm": "net10.0" } }"""
        |> shouldEqual Map.empty

        parseOk """{ "runtimeOptions": {} }""" |> shouldEqual Map.empty
        parseOk (document "{}") |> shouldEqual Map.empty

    [<Test>]
    let ``a present document with no runtimeOptions is invalid`` () =
        // `ensure_parsed` returns false when the document has no `runtimeOptions` member, so
        // a real host rejects the config outright. Only an *absent* file is benign, and that
        // distinction belongs to the caller — `HostRuntimeConfig` makes it.
        parseError """{}""" |> shouldContainText "runtimeOptions"
        parseError """{ "notRuntimeOptions": {} }""" |> ignore

    [<Test>]
    let ``an explicitly null runtimeOptions supplies nothing`` () =
        // `parse_opts` short-circuits to success on a null value, so this is a valid config
        // rather than a wrong-shape error.
        parseOk """{ "runtimeOptions": null }""" |> shouldEqual Map.empty

    [<Test>]
    let ``duplicate sections take the first occurrence`` () =
        // rapidjson's `FindMember` returns the first match. `JsonElement.TryGetProperty`
        // returns the *last*, so this is exactly where a naive reader silently disagrees with
        // the host about which block is in force.
        parseOk
            """{ "runtimeOptions": { "configProperties": { "P": "first" } },
                 "runtimeOptions": { "configProperties": { "P": "second" } } }"""
        |> shouldEqual (Map.ofList [ "P", "first" ])

        parseOk
            """{ "runtimeOptions": { "configProperties": { "P": "first" },
                                     "configProperties": { "P": "second" } } }"""
        |> shouldEqual (Map.ofList [ "P", "first" ])

    [<Test>]
    let ``sibling runtimeOptions keys are ignored`` () =
        // `tfm`, `framework`, `rollForward`, … are hostfxr's business, not ours.
        parseOk (document """{ "P": "v" }""") |> shouldEqual (Map.ofList [ "P", "v" ])

    [<Test>]
    let ``duplicate keys take the last occurrence`` () =
        // rapidjson keeps both members; hostpolicy's `m_properties[name] = …` assignment
        // means the later one wins.
        parseOk (document """{ "P": "first", "P": "second" }""")
        |> shouldEqual (Map.ofList [ "P", "second" ])

    [<Test>]
    let ``a shadowed unrenderable value does not condemn the file`` () =
        // hostpolicy renders every occurrence but keeps only the last, so the *effective*
        // value here is a plain string and the config is fine. Rejecting on the shadowed
        // `1.5` would refuse a file a real host runs happily.
        parseOk (document """{ "P": 1.5, "P": "final" }""")
        |> shouldEqual (Map.ofList [ "P", "final" ])

        // ... and the converse still fails: it is the *last* value that has to be renderable.
        parseError (document """{ "P": "fine", "P": 1.5 }""")
        |> shouldContainText "non-integer numeric"

    [<Test>]
    let ``names and values are truncated at an interior NUL`` () =
        // Both go into a `pal::string_t` assigned from a `char_t*`, which stops at the NUL.
        // For values this is invisible either way (the guest's `new string(char*)` would stop
        // there too), but for *names* it decides identity: these two are one property to a
        // real host, and the later value wins. Were they kept distinct, both would reach
        // `AppContext.Setup` and its `Dictionary.Add` would throw on the duplicate key.
        parseOk (document "{ \"A\\u0000X\": \"one\", \"A\\u0000Y\": \"two\" }")
        |> shouldEqual (Map.ofList [ "A", "two" ])

        parseOk (document "{ \"P\": \"keep\\u0000drop\" }")
        |> shouldEqual (Map.ofList [ "P", "keep" ])

    [<Test>]
    let ``comments are permitted`` () =
        // The host parses with `kParseCommentsFlag` (json_parser.cpp), so a config with
        // comments is one a real runtime would accept; refusing it would make PawPrint
        // stricter than the thing it emulates.
        parseOk (
            document
                """{ /* why */ "P": "v" // trailing
            }"""
        )
        |> shouldEqual (Map.ofList [ "P", "v" ])

    [<Test>]
    let ``malformed documents are refused`` () =
        parseError "" |> ignore
        parseError "not json at all" |> ignore
        parseError """{ "runtimeOptions": """ |> ignore
        // rapidjson is not given kParseTrailingCommasFlag, so nor do we allow them.
        parseError (document """{ "P": "v", }""") |> ignore

    [<Test>]
    let ``sections of the wrong shape are refused`` () =
        // `parse_opts` returns false for a non-object, non-null `runtimeOptions`, and calls
        // `GetObject()` on `configProperties` without checking — undefined behaviour on any
        // other kind, so there is no behaviour to match and we refuse.
        parseError """{ "runtimeOptions": [] }""" |> ignore
        parseError """{ "runtimeOptions": "no" }""" |> ignore
        parseError """{ "runtimeOptions": { "configProperties": [] } }""" |> ignore
        parseError """{ "runtimeOptions": { "configProperties": "no" } }""" |> ignore
        parseError """{ "runtimeOptions": { "configProperties": null } }""" |> ignore
        parseError """[]""" |> ignore

    // ------------------------------------------------------------------
    // Properties.
    // ------------------------------------------------------------------

    /// A string that can appear as a JSON key or value. Unpaired surrogates are excluded
    /// because they cannot survive a UTF-8 round trip through a real file, so hostpolicy
    /// would never see one; everything else — control characters, quotes, backslashes,
    /// astral planes, and embedded NULs — is fair game.
    let private jsonStringGen : Gen<string> =
        gen {
            let! chars =
                ArbMap.defaults
                |> ArbMap.generate<char>
                |> Gen.filter (Char.IsSurrogate >> not)
                |> Gen.listOf

            let! includeAstral = Gen.frequency [ 1, Gen.constant true ; 4, Gen.constant false ]

            let chars =
                if includeAstral then
                    '\uD83D' :: '\uDC36' :: chars
                else
                    chars

            return System.String (List.toArray chars)
        }

    [<Test>]
    let ``string values survive verbatim up to NUL truncation`` () =
        // The whole rule for string values in one property: what a guest sees is the JSON
        // string *decoded* (so every escape the serialiser chose has to be undone) and then
        // cut at its first NUL, with names cut the same way and later entries winning the
        // resulting collisions.
        //
        // NULs stay in the generator's alphabet rather than being filtered out. Truncation is
        // the specified behaviour, not an inconvenience, and a generator that dodged it would
        // stop the property from saying anything about the case where it bites.
        let expected (entries : (string * string) list) : Map<string, string> =
            // Deliberately restated with `Split` rather than the implementation's
            // `IndexOf`/`Substring`, so this is a second opinion and not a transcription.
            let cut (s : string) = s.Split('\000').[0]

            (Map.empty, entries)
            ||> List.fold (fun acc (k, v) -> Map.add (cut k) (cut v) acc)

        let property (kvs : (string * string) list) : bool =
            // `Map.toList` is ordered by the *untruncated* key, and that order is the
            // document order we then emit, so last-wins is well defined on both sides.
            let entries = Map.ofList kvs |> Map.toList

            let body =
                entries
                |> List.map (fun (k, v) -> $"{JsonSerializer.Serialize k}: {JsonSerializer.Serialize v}")
                |> String.concat ", "

            parseOk (document $"{{ {body} }}") = expected entries

        let gen = Gen.zip jsonStringGen jsonStringGen |> Gen.listOf

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``integers render as their canonical decimal form`` () =
        // int64 and uint64 between them are exactly the range rapidjson keeps as an
        // integer; anything wider becomes a double, which we refuse.
        let property (i : int64) : bool =
            parseOk (document $"""{{ "P": {i} }}""") = Map.ofList [ "P", string<int64> i ]

        Check.One (config, Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary<int64>) property)

    [<Test>]
    let ``unsigned integers above int64 render as their canonical decimal form`` () =
        let property (u : uint64) : bool =
            parseOk (document $"""{{ "P": {u} }}""") = Map.ofList [ "P", string<uint64> u ]

        Check.One (config, Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary<uint64>) property)

    // ------------------------------------------------------------------
    // Direct construction.
    // ------------------------------------------------------------------

    [<Test>]
    let ``ofMap applies the same NUL truncation`` () =
        AppContextProperties.ofMap (Map.ofList [ "A\000X", "v\000w" ])
        |> AppContextProperties.toMap
        |> shouldEqual (Map.ofList [ "A", "v" ])

    [<Test>]
    let ``ofMap rejects names that collide only after truncation`` () =
        // A `Map` has no document order, so there is no principled way to pick a winner;
        // silently dropping one would hand the caller a property set that is not the one they
        // asked for.
        let exn =
            Assert.Throws<exn> (fun () ->
                AppContextProperties.ofMap (Map.ofList [ "A\000X", "one" ; "A\000Y", "two" ])
                |> ignore
            )

        exn.Message |> shouldContainText "NUL"

    // ------------------------------------------------------------------
    // Path derivation.
    // ------------------------------------------------------------------

    [<Test>]
    let ``the config sits beside the assembly`` () =
        RuntimeConfig.pathForAssembly "/a/b/App.dll"
        |> shouldEqual "/a/b/App.runtimeconfig.json"

        // A dot in a directory name must not be mistaken for the extension, and only the
        // final extension of a multi-dot filename is replaced.
        RuntimeConfig.pathForAssembly "/a/v1.2/My.App.dll"
        |> shouldEqual "/a/v1.2/My.App.runtimeconfig.json"

        RuntimeConfig.pathForAssembly "App.dll" |> shouldEqual "App.runtimeconfig.json"
