namespace WoofWare.PawPrint.Test

open System
open System.IO
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

    /// `parse` takes the file's bytes, because the encoding rules are part of what it is
    /// reproducing; the overwhelming majority of these tests care only about the JSON, so
    /// they go through UTF-8 here and the encoding-specific ones build their own bytes.
    let private parseBytesOk (json : byte[]) : Map<string, string> =
        match RuntimeConfig.parse json with
        | Ok props -> AppContextProperties.toMap props
        | Error e -> failwith $"expected a successful parse, but got: %s{e.Message}"

    let private parseBytesErrorCase (json : byte[]) : RuntimeConfigError =
        match RuntimeConfig.parse json with
        | Error e -> e
        | Ok props -> failwith $"expected a failed parse, but got: %O{AppContextProperties.toMap props |> Map.toList}"

    let private parseBytesError (json : byte[]) : string = (parseBytesErrorCase json).Message

    let private parseOk (json : string) : Map<string, string> =
        parseBytesOk (Text.Encoding.UTF8.GetBytes json)

    let private parseError (json : string) : string =
        parseBytesError (Text.Encoding.UTF8.GetBytes json)

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
    // Names the hosting layer owns.
    // ------------------------------------------------------------------

    /// Populated by `hostpolicy_context.cpp` *before* it walks `configProperties`, so a config
    /// entry reusing one of these names makes `coreclr_property_bag_t::add` return false and
    /// the launch fail with `LibHostDuplicateProperty` — plus `HOST_RUNTIME_CONTRACT`, which
    /// is added afterwards but checked the same way.
    let private hostOwnedNames : string list =
        [
            "TRUSTED_PLATFORM_ASSEMBLIES"
            "NATIVE_DLL_SEARCH_DIRECTORIES"
            "PLATFORM_RESOURCE_ROOTS"
            "APP_CONTEXT_BASE_DIRECTORY"
            "APP_CONTEXT_DEPS_FILES"
            "FX_DEPS_FILE"
            "PROBING_DIRECTORIES"
            "RUNTIME_IDENTIFIER"
            "HOST_RUNTIME_CONTRACT"
        ]

    [<Literal>]
    let private SetAppPathsSwitch = "Microsoft.NETCore.DotNetHostPolicy.SetAppPaths"

    /// The host-owned check belongs to `combine`, not to the per-file parser, because a real
    /// host merges the dev sidecar and the main config into one property set and only then
    /// detects the duplicate. These go through it; a single file's worth is `main` alone.
    let private combineOk (dev : Map<string, string>) (main : Map<string, string>) : Map<string, string> =
        match RuntimeConfig.combine (AppContextProperties.ofMap dev) (AppContextProperties.ofMap main) with
        | Ok props -> AppContextProperties.toMap props
        | Error e -> failwith $"expected a successful combine, but got: %s{e.Message}"

    let private combineError (dev : Map<string, string>) (main : Map<string, string>) : string =
        match RuntimeConfig.combine (AppContextProperties.ofMap dev) (AppContextProperties.ofMap main) with
        | Error e -> e.Message
        | Ok props -> failwith $"expected a failed combine, but got: %O{AppContextProperties.toMap props |> Map.toList}"

    [<Test>]
    let ``a property the hosting layer owns is refused`` () =
        // Accepting one would run a configuration that cannot launch on a real runtime, and
        // would hand the guest a forged built-in: PawPrint populates none of these itself
        // (see docs/divergences.md), so there is nothing to collide with and the value would
        // simply be believed.
        for name in hostOwnedNames do
            combineError Map.empty (Map.ofList [ name, "forged" ]) |> shouldContainText name

    [<Test>]
    let ``a host-owned name is fatal wherever it came from`` () =
        // hostpolicy merges the dev sidecar into the same `m_properties` the main config
        // writes to, and the duplicate is only detected afterwards, when `hostpolicy_context`
        // fills the property bag from the merged set. So a host-owned name in the *dev* file
        // is just as fatal, and must not be quietly dropped as "a broken dev file".
        for name in hostOwnedNames do
            combineError (Map.ofList [ name, "forged" ]) Map.empty |> shouldContainText name

    [<Test>]
    let ``the comparison is case-sensitive, as the host's is`` () =
        // `coreclr_property_bag_t::add` looks the key up in an `unordered_map` with default
        // equality, so only the exact spelling collides. A lowercased name is an ordinary
        // property to a real host, and must be one here.
        combineOk Map.empty (Map.ofList [ "trusted_platform_assemblies", "not the real one" ])
        |> shouldEqual (Map.ofList [ "trusted_platform_assemblies", "not the real one" ])

    [<Test>]
    let ``STARTUP_HOOKS is not host-owned`` () =
        // Deliberately not in the list: hostpolicy adds it *after* the config loop, ignores
        // `add`'s result, and explicitly reads a config-supplied value back via `try_get` so
        // it can append the environment's hooks to it. Config is a supported source here.
        combineOk Map.empty (Map.ofList [ "STARTUP_HOOKS", "/hooks/one.dll" ])
        |> shouldEqual (Map.ofList [ "STARTUP_HOOKS", "/hooks/one.dll" ])

    [<Test>]
    let ``APP_PATHS is host-owned only when the SetAppPaths switch asks for it`` () =
        // The conditional case: `APP_PATHS` is added after the loop and only when
        // `Microsoft.NETCore.DotNetHostPolicy.SetAppPaths` is true, so on its own it is a
        // perfectly ordinary property and only the pair is fatal.
        combineOk Map.empty (Map.ofList [ "APP_PATHS", "/somewhere" ])
        |> shouldEqual (Map.ofList [ "APP_PATHS", "/somewhere" ])

        combineError Map.empty (Map.ofList [ "APP_PATHS", "/somewhere" ; SetAppPathsSwitch, "true" ])
        |> shouldContainText "APP_PATHS"

        // The switch is compared case-insensitively in both name and value (`pal::strcasecmp`),
        // and anything other than "true" leaves APP_PATHS alone.
        combineError
            Map.empty
            (Map.ofList
                [
                    "APP_PATHS", "/somewhere"
                    "microsoft.netcore.dotnethostpolicy.setapppaths", "TRUE"
                ])
        |> shouldContainText "APP_PATHS"

        combineOk Map.empty (Map.ofList [ "APP_PATHS", "/somewhere" ; SetAppPathsSwitch, "false" ])
        |> Map.containsKey "APP_PATHS"
        |> shouldEqual true

    [<Test>]
    let ``the APP_PATHS pair is fatal even when split across the two sidecars`` () =
        // Neither file is objectionable alone, which is exactly why this check cannot live in
        // the per-file parser: a real host merges first and only then discovers the pair.
        combineError (Map.ofList [ "APP_PATHS", "/somewhere" ]) (Map.ofList [ SetAppPathsSwitch, "true" ])
        |> shouldContainText "APP_PATHS"

        combineError (Map.ofList [ SetAppPathsSwitch, "true" ]) (Map.ofList [ "APP_PATHS", "/somewhere" ])
        |> shouldContainText "APP_PATHS"

    [<Test>]
    let ``the main config can switch APP_PATHS back off`` () =
        // The merged value is what a real host sees, and the main config overrides the dev
        // one, so a dev sidecar asking for APP_PATHS that main turns off is not the fatal
        // pair. Checking the dev file on its own would have got this backwards.
        combineOk
            (Map.ofList [ "APP_PATHS", "/somewhere" ; SetAppPathsSwitch, "true" ])
            (Map.ofList [ SetAppPathsSwitch, "false" ])
        |> Map.containsKey "APP_PATHS"
        |> shouldEqual true

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

    // ------------------------------------------------------------------
    // Encoding and framing, which are properties of the *bytes*.
    // ------------------------------------------------------------------

    [<Test>]
    let ``content after the root value is ignored`` () =
        // The host parses with `kParseStopWhenDoneFlag`, which suppresses rapidjson's
        // "document root not singular" error, so it launches these happily. Refusing them
        // would make PawPrint unable to run a program that really runs.
        parseOk (document """{ "P": "v" }""" + """ {"another": "document"}""")
        |> shouldEqual (Map.ofList [ "P", "v" ])

        parseOk (document """{ "P": "v" }""" + " not even json")
        |> shouldEqual (Map.ofList [ "P", "v" ])

    [<Test>]
    let ``nesting we never look at does not make a document invalid`` () =
        // The same argument as trailing content, in a different guise: `Utf8JsonReader`
        // defaults to a maximum depth of 64, and rapidjson has no configured depth limit at
        // all, so at the default we would refuse a file that a real host parses without
        // comment. Depth here is well past 64 and sits under a section we ignore entirely.
        let deep = String.replicate 200 "[" + String.replicate 200 "]"

        let json =
            $$"""{ "runtimeOptions": { "ignoredSection": {{deep}}, "configProperties": { "P": "v" } } }"""

        parseOk json |> shouldEqual (Map.ofList [ "P", "v" ])

    [<Test>]
    let ``but nesting is still bounded, and exceeding it is a clean error`` () =
        // The bound is ours rather than hostpolicy's, and it exists because `JsonDocument`'s
        // cost is quadratic in the depth: without it a pathological file is an apparent hang
        // instead of a refusal. Deliberately not asserting the exact ceiling — what matters is
        // that one exists, and that reaching it yields an `Error` rather than an escaping
        // exception or a wait.
        let deep = String.replicate 100_000 "[" + String.replicate 100_000 "]"

        parseError $$"""{ "runtimeOptions": { "ignoredSection": {{deep}} } }"""
        |> shouldContainText "depth"

    /// A one-property document whose name and value bytes are spliced in raw, so that a test
    /// can put bytes there which no `string` could have held.
    let private documentWithRawBytes (nameBytes : byte list) (valueBytes : byte list) : byte[] =
        [
            yield! Text.Encoding.UTF8.GetBytes "{ \"runtimeOptions\": { \"configProperties\": { \""
            yield! nameBytes
            yield! Text.Encoding.UTF8.GetBytes "\": \""
            yield! valueBytes
            yield! Text.Encoding.UTF8.GetBytes "\" } } }"
        ]
        |> Array.ofList

    /// `C0 AF` is an overlong encoding of `/`: a JSON string token as far as the reader is
    /// concerned, but not valid UTF-8.
    let private invalidUtf8 : byte list = [ 0xC0uy ; 0xAFuy ]

    let private asciiBytes (s : string) : byte list =
        Text.Encoding.UTF8.GetBytes s |> List.ofArray

    [<Test>]
    let ``invalid UTF-8 in a value is refused, not thrown`` () =
        // `Utf8JsonReader` accepts the token without validating its encoding and `JsonDocument`
        // transcodes lazily, so this surfaces as an `InvalidOperationException` out of
        // `GetString()` — long after parsing "succeeded". `parse` promises a `Result`, and a
        // caller that treats a bad file as non-fatal (the dev config) depends on that promise.
        let err = parseBytesError (documentWithRawBytes (asciiBytes "P") invalidUtf8)
        err |> shouldContainText "P"
        err |> shouldContainText "UTF-8"

    [<Test>]
    let ``invalid UTF-8 in a name is refused, not thrown`` () =
        // Same failure, reached through `JsonProperty.Name` instead. The diagnostic cannot
        // quote the name — materialising it is precisely what fails — so it must locate the
        // property some other way.
        let err =
            parseBytesError (documentWithRawBytes (asciiBytes "Good" @ invalidUtf8) (asciiBytes "v"))

        err |> shouldContainText "UTF-8"

    [<Test>]
    let ``invalid UTF-8 elsewhere in the document is nobody's business`` () =
        // hostpolicy does not validate encoding either (rapidjson is not given
        // `kParseValidateEncodingFlag`), so bytes we never materialise into a property must
        // not condemn a file that a real host reads without complaint.
        let bytes =
            [
                yield! Text.Encoding.UTF8.GetBytes "{ \"runtimeOptions\": { \"ignoredSection\": { \"k\": \""
                yield! invalidUtf8
                yield! Text.Encoding.UTF8.GetBytes "\" }, \"configProperties\": { \"P\": \"v\" } } }"
            ]
            |> Array.ofList

        parseBytesOk bytes |> shouldEqual (Map.ofList [ "P", "v" ])

    [<Test>]
    let ``invalid UTF-8 in an ignored member's name is nobody's business either`` () =
        // Not just ignored *values*: locating `runtimeOptions` must not transcode the names it
        // walks past, or a sibling member nobody reads takes the launch down. Comparing a name
        // without materialising it is what `JsonProperty.NameEquals` is for.
        let bytes =
            [
                yield! Text.Encoding.UTF8.GetBytes "{ \"ignored"
                yield! invalidUtf8
                yield!
                    Text.Encoding.UTF8.GetBytes
                        "\": 1, \"runtimeOptions\": { \"configProperties\": { \"P\": \"v\" } } }"
            ]
            |> Array.ofList

        parseBytesOk bytes |> shouldEqual (Map.ofList [ "P", "v" ])

    [<Test>]
    let ``an escaped section name still matches`` () =
        // Comparing raw bytes must not cost us escape handling: rapidjson decodes escapes in
        // names, so `runtimeOptions` is `runtimeOptions` to a real host, and a section
        // spelled that way has to keep working here.
        // Both section names carry their leading letter as a JSON `\u` escape: a spelling no
        // SDK emits, but one every JSON parser must treat as the plain name.
        let r = "\\u0072"
        let c = "\\u0063"

        parseOk $$"""{ "{{r}}untimeOptions": { "{{c}}onfigProperties": { "P": "v" } } }"""
        |> shouldEqual (Map.ofList [ "P", "v" ])

    // ------------------------------------------------------------------
    // Which kind of failure it was.
    // ------------------------------------------------------------------

    /// `parse` fails for two quite different reasons, and `HostRuntimeConfig` acts on the
    /// difference: a dev sidecar may be ignored exactly when hostpolicy would ignore it. Left
    /// untested the classification would be free to drift, and the drift would be silent —
    /// misclassifying a `NotReproducible` as `HostWouldReject` turns "we cannot run this
    /// faithfully" into "launch without those properties".
    let private classificationCases : (string * byte[] * bool) list =
        // (description, bytes, isHostWouldReject)
        [
            "malformed JSON", Text.Encoding.UTF8.GetBytes "}{ not json", true
            "no runtimeOptions", Text.Encoding.UTF8.GetBytes """{ "other": 1 }""", true
            "runtimeOptions of the wrong shape", Text.Encoding.UTF8.GetBytes """{ "runtimeOptions": [] }""", true
            "configProperties of the wrong shape",
            Text.Encoding.UTF8.GetBytes """{ "runtimeOptions": { "configProperties": [] } }""",
            true
            "a root that is not an object", Text.Encoding.UTF8.GetBytes "[]", true
            // hostpolicy parses UTF-8 and skips only a UTF-8 BOM, so it refuses this outright.
            "a UTF-16 document",
            Array.append
                (Text.Encoding.Unicode.GetPreamble ())
                (Text.Encoding.Unicode.GetBytes (document """{ "P": "v" }""")),
            true
            // These four a real host reads without complaint; only we cannot reproduce them.
            "a real-valued property", Text.Encoding.UTF8.GetBytes (document """{ "P": 1.5 }"""), false
            "an array-valued property", Text.Encoding.UTF8.GetBytes (document """{ "P": [] }"""), false
            "an object-valued property", Text.Encoding.UTF8.GetBytes (document """{ "P": {} }"""), false
            "invalid UTF-8 in a value", documentWithRawBytes (asciiBytes "P") invalidUtf8, false
        ]

    [<Test>]
    let ``failures are classified by whether a real host would also refuse the file`` () =
        for description, bytes, expectedHostWouldReject in classificationCases do
            let actual =
                match parseBytesErrorCase bytes with
                | RuntimeConfigError.HostWouldReject _ -> true
                | RuntimeConfigError.NotReproducible _ -> false

            if actual <> expectedHostWouldReject then
                failwith $"%s{description}: expected HostWouldReject=%b{expectedHostWouldReject} but got %b{actual}"

    [<Test>]
    let ``a UTF-8 BOM is skipped`` () =
        // `parse_file` steps over exactly these three bytes and parses the rest.
        let bom = [| 0xEFuy ; 0xBBuy ; 0xBFuy |]

        let bytes =
            Array.append bom (Text.Encoding.UTF8.GetBytes (document """{ "P": "v" }"""))

        parseBytesOk bytes |> shouldEqual (Map.ofList [ "P", "v" ])

    [<Test>]
    let ``other encodings are refused`` () =
        // hostpolicy mmaps the file and parses it as UTF-8, skipping only a UTF-8 BOM, so a
        // UTF-16 or UTF-32 config fails there with "Invalid value" at offset 0 and the app
        // does not launch. `File.ReadAllText` would have sniffed the BOM and decoded it,
        // which is why `parse` takes bytes rather than a string.
        let json = document """{ "P": "v" }"""

        parseBytesError (
            Text.Encoding.Unicode.GetPreamble () |> Array.append
            <| Text.Encoding.Unicode.GetBytes json
        )
        |> ignore

        parseBytesError (
            Text.Encoding.UTF32.GetPreamble () |> Array.append
            <| Text.Encoding.UTF32.GetBytes json
        )
        |> ignore

        // ... and a sanity check that the same document in UTF-8 is fine, so the assertions
        // above are about the encoding rather than about the document.
        parseOk json |> shouldEqual (Map.ofList [ "P", "v" ])

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
    let ``overlay lets the later config win key by key`` () =
        // How hostpolicy combines runtimeconfig.dev.json with the main config: the dev one
        // populates `m_properties`, the main one assigns over the top.
        let dev =
            AppContextProperties.ofMap (Map.ofList [ "Shared", "from dev" ; "DevOnly", "dev" ])

        let main =
            AppContextProperties.ofMap (Map.ofList [ "Shared", "from main" ; "MainOnly", "main" ])

        AppContextProperties.overlay dev main
        |> AppContextProperties.toMap
        |> shouldEqual (Map.ofList [ "Shared", "from main" ; "DevOnly", "dev" ; "MainOnly", "main" ])

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

    [<Test>]
    let ``the dev config sits beside the assembly too`` () =
        RuntimeConfig.devPathForAssembly "/a/b/App.dll"
        |> shouldEqual "/a/b/App.runtimeconfig.dev.json"

        RuntimeConfig.devPathForAssembly "/a/v1.2/My.App.dll"
        |> shouldEqual "/a/v1.2/My.App.runtimeconfig.dev.json"

    // ------------------------------------------------------------------
    // The CLI's filesystem half.
    // ------------------------------------------------------------------

    /// Lay out a scratch directory containing a fake `App.dll` plus whichever sidecars the
    /// test wants, and ask `HostRuntimeConfig` what a guest there would be seeded with.
    let private withSidecars (main : string option) (dev : string option) (f : string -> unit) : unit =
        let dir =
            Path.Combine (Path.GetTempPath (), $"pawprint-runtimeconfig-%s{Path.GetRandomFileName ()}")

        Directory.CreateDirectory dir |> ignore

        try
            let dll = Path.Combine (dir, "App.dll")
            File.WriteAllText (dll, "not a real assembly; nothing here opens it")

            match main with
            | None -> ()
            | Some contents -> File.WriteAllText (RuntimeConfig.pathForAssembly dll, contents)

            match dev with
            | None -> ()
            | Some contents -> File.WriteAllText (RuntimeConfig.devPathForAssembly dll, contents)

            f dll
        finally
            Directory.Delete (dir, true)

    [<Test>]
    let ``the main config overrides the dev config`` () =
        withSidecars
            (Some (document """{ "Shared": "from main", "MainOnly": "main" }"""))
            (Some (document """{ "Shared": "from dev", "DevOnly": "dev" }"""))
            (fun dll ->
                HostRuntimeConfig.forAssembly dll
                |> AppContextProperties.toMap
                |> shouldEqual (Map.ofList [ "Shared", "from main" ; "DevOnly", "dev" ; "MainOnly", "main" ])
            )

    [<Test>]
    let ``a dev config alone still supplies properties`` () =
        // hostpolicy reads the dev config whether or not the main one exists, so a switch set
        // only there must still reach the guest.
        withSidecars
            None
            (Some (document """{ "DevOnly": "dev" }"""))
            (fun dll ->
                HostRuntimeConfig.forAssembly dll
                |> AppContextProperties.toMap
                |> shouldEqual (Map.ofList [ "DevOnly", "dev" ])
            )

    [<Test>]
    let ``a broken dev config is not fatal`` () =
        // `ensure_dev_config_parsed`'s failures are swallowed: `ensure_parsed` emits a verbose
        // trace and carries on, and `parse_opts`' return value is discarded at the call site.
        // A dev config is a developer convenience; a broken one must not stop the app.
        for broken in [ "}{ not json" ; """{}""" ; """{ "runtimeOptions": [] }""" ] do
            withSidecars
                (Some (document """{ "P": "v" }"""))
                (Some broken)
                (fun dll ->
                    HostRuntimeConfig.forAssembly dll
                    |> AppContextProperties.toMap
                    |> shouldEqual (Map.ofList [ "P", "v" ])
                )

    /// Strip every permission bit from `path`, and report whether that actually made it
    /// unreadable — it does not for root, who bypasses the mode entirely.
    let private makeUnreadable (path : string) : bool =
        File.SetUnixFileMode (path, UnixFileMode.None)

        try
            File.ReadAllBytes path |> ignore
            false
        with _ ->
            true

    [<Test>]
    let ``an unreadable dev config is not fatal`` () =
        // hostpolicy does not distinguish "will not open" from "will not parse": both are
        // `parse_file` returning false, and for the dev config `ensure_parsed` merely traces
        // that and carries on. So a dev sidecar we cannot read must be as harmless as a dev
        // sidecar full of nonsense — losing the app to a stray permission bit or a deletion
        // race would be ours alone.
        if OperatingSystem.IsWindows () then
            Assert.Ignore "file modes are a Unix concept"

        withSidecars
            (Some (document """{ "P": "v" }"""))
            (Some (document """{ "DevOnly": "dev" }"""))
            (fun dll ->
                if not (makeUnreadable (RuntimeConfig.devPathForAssembly dll)) then
                    Assert.Ignore "could not make the file unreadable; running as root?"

                HostRuntimeConfig.forAssembly dll
                |> AppContextProperties.toMap
                |> shouldEqual (Map.ofList [ "P", "v" ])
            )

    [<Test>]
    let ``an unreadable main config is fatal`` () =
        // The other side of the asymmetry above, pinned so that nobody tidies the two into
        // agreement: for the main config `ensure_parsed` propagates `parse_file`'s failure,
        // and the app does not launch. Running the guest with its feature switches silently
        // dropped is the failure this whole change exists to avoid.
        if OperatingSystem.IsWindows () then
            Assert.Ignore "file modes are a Unix concept"

        withSidecars
            (Some (document """{ "P": "v" }"""))
            None
            (fun dll ->
                if not (makeUnreadable (RuntimeConfig.pathForAssembly dll)) then
                    Assert.Ignore "could not make the file unreadable; running as root?"

                // The exact exception is the filesystem's business (macOS and Linux differ on
                // whether this is an `UnauthorizedAccessException` or an `IOException`); what
                // this test pins is that it escapes rather than being swallowed.
                let exn = Assert.Catch (fun () -> HostRuntimeConfig.forAssembly dll |> ignore)

                exn.Message |> shouldContainText "App.runtimeconfig.json"
            )

    [<Test>]
    let ``a dev config with invalid UTF-8 fails cleanly rather than throwing`` () =
        // Two claims at once. `parse` must contain the transcoding failure as an `Error`
        // rather than letting `InvalidOperationException` escape — asserted by the exception
        // type below, since an escaping one would not be our `failwith`.
        //
        // And that error must be fatal, not swallowed: a real host reads these bytes happily,
        // substituting U+FFFD, so the property exists on a real launch. Dropping the file
        // would start the guest a property short. This is the `NotReproducible` half of the
        // classification, and the reason a dev sidecar is not simply "ignore all failures".
        let dir =
            Path.Combine (Path.GetTempPath (), $"pawprint-runtimeconfig-%s{Path.GetRandomFileName ()}")

        Directory.CreateDirectory dir |> ignore

        try
            let dll = Path.Combine (dir, "App.dll")
            File.WriteAllText (dll, "not a real assembly; nothing here opens it")
            File.WriteAllText (RuntimeConfig.pathForAssembly dll, document """{ "P": "v" }""")

            File.WriteAllBytes (
                RuntimeConfig.devPathForAssembly dll,
                documentWithRawBytes (asciiBytes "DevOnly") invalidUtf8
            )

            let exn = Assert.Throws<exn> (fun () -> HostRuntimeConfig.forAssembly dll |> ignore)
            exn.Message |> shouldContainText "UTF-8"
            exn.Message |> shouldContainText "App.runtimeconfig.dev.json"
        finally
            Directory.Delete (dir, true)

    [<Test>]
    let ``a broken main config is fatal`` () =
        // The opposite of the dev config: this one is the guest's actual configuration, and
        // running with its feature switches silently dropped is the failure we are avoiding.
        withSidecars
            (Some "}{ not json")
            None
            (fun dll ->
                let exn = Assert.Throws<exn> (fun () -> HostRuntimeConfig.forAssembly dll |> ignore)
                exn.Message |> shouldContainText "App.runtimeconfig.json"
            )

    [<Test>]
    let ``a dev config we cannot reproduce is fatal, unlike one that is merely broken`` () =
        // `parse` fails for two quite different reasons, and only one of them is hostpolicy's
        // own. A value rapidjson renders perfectly well and we decline to approximate is a
        // file a real launch *acts on*; dropping it would start the guest with a property
        // missing. Contrast `a broken dev config is not fatal` directly above, where the file
        // is one hostpolicy also refuses.
        withSidecars
            (Some (document """{ "P": "v" }"""))
            (Some (document """{ "DevOnly": 1.5 }"""))
            (fun dll ->
                let exn = Assert.Throws<exn> (fun () -> HostRuntimeConfig.forAssembly dll |> ignore)
                exn.Message |> shouldContainText "DevOnly"
            )

    [<Test>]
    let ``an unreproducible dev value cannot smuggle a host-owned name past the check`` () =
        // The case that motivated splitting the error: CoreCLR renders `[]` happily, so the
        // property exists and the launch dies on the duplicate. Swallowing our refusal to
        // render it would drop the property, and with it the collision — PawPrint would run
        // a configuration that cannot start on a real runtime.
        withSidecars
            (Some (document """{ "P": "v" }"""))
            (Some (document """{ "APP_CONTEXT_BASE_DIRECTORY": [] }"""))
            (fun dll ->
                let exn = Assert.Throws<exn> (fun () -> HostRuntimeConfig.forAssembly dll |> ignore)
                exn.Message |> shouldContainText "APP_CONTEXT_BASE_DIRECTORY"
            )

    [<Test>]
    let ``a host-owned name in the dev sidecar stops the launch`` () =
        // The whole point of validating after the merge: this file parses perfectly well, so
        // "a broken dev config is not fatal" must not swallow it. A real host keeps the
        // property and dies with LibHostDuplicateProperty; dropping it would launch a guest
        // whose configuration could never have run.
        withSidecars
            (Some (document """{ "P": "v" }"""))
            (Some (document """{ "APP_CONTEXT_BASE_DIRECTORY": "/forged" }"""))
            (fun dll ->
                let exn = Assert.Throws<exn> (fun () -> HostRuntimeConfig.forAssembly dll |> ignore)
                exn.Message |> shouldContainText "APP_CONTEXT_BASE_DIRECTORY"
            )

    [<Test>]
    let ``the APP_PATHS pair split across sidecars stops the launch`` () =
        withSidecars
            (Some (document $$"""{ "{{SetAppPathsSwitch}}": "true" }"""))
            (Some (document """{ "APP_PATHS": "/somewhere" }"""))
            (fun dll ->
                let exn = Assert.Throws<exn> (fun () -> HostRuntimeConfig.forAssembly dll |> ignore)
                exn.Message |> shouldContainText "APP_PATHS"
            )

    [<Test>]
    let ``no sidecars at all means no properties`` () =
        // PawPrint is routinely pointed at a bare dll, and `ensure_parsed` treats a
        // non-existent config as success.
        withSidecars
            None
            None
            (fun dll ->
                HostRuntimeConfig.forAssembly dll
                |> AppContextProperties.toMap
                |> shouldEqual Map.empty
            )
