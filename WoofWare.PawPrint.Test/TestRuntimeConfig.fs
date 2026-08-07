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
    let ``absent sections yield no properties`` () =
        // hostpolicy's `FindMember` simply misses, which is not an error: a
        // runtimeconfig.json with no configProperties is completely normal.
        parseOk """{ "runtimeOptions": { "tfm": "net10.0" } }"""
        |> shouldEqual Map.empty

        parseOk """{ "runtimeOptions": {} }""" |> shouldEqual Map.empty
        parseOk """{}""" |> shouldEqual Map.empty
        parseOk (document "{}") |> shouldEqual Map.empty

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
        // hostpolicy calls `GetObject()` on these without checking, which is undefined
        // behaviour on a non-object. There is no behaviour to match, so we refuse.
        parseError """{ "runtimeOptions": [] }""" |> ignore
        parseError """{ "runtimeOptions": "no" }""" |> ignore
        parseError """{ "runtimeOptions": { "configProperties": [] } }""" |> ignore
        parseError """{ "runtimeOptions": { "configProperties": "no" } }""" |> ignore
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
    let ``string values survive verbatim`` () =
        let property (kvs : (string * string) list) : bool =
            let kvs = Map.ofList kvs

            let body =
                kvs
                |> Map.toList
                |> List.map (fun (k, v) -> $"{JsonSerializer.Serialize k}: {JsonSerializer.Serialize v}")
                |> String.concat ", "

            parseOk (document $"{{ {body} }}") = kvs

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
