namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// A real process's environment is a list of byte strings, the `envp` it was
/// started with, and not a map: the map every environment API presents is a
/// *view* of that list, split at each entry's first `=`. So `KernelConfig`
/// describes the list, and these tests pin which lists a host may configure and
/// how they compose with PawPrint's defaults.
///
/// Measured against real .NET with a hand-built `envp`, which is the only way to
/// get such entries into a process (`Environment.SetEnvironmentVariable` refuses
/// to create them):
///
///   entry `A=B=C`     -> `GetEnvironmentVariable "A"` = "B=C", `"A=B"` = null,
///                        and enumeration yields the key `A` and no key `A=B`
///   entry `=C`        -> invisible to both APIs
///   `DUP=1`, `DUP=2`  -> both APIs report `DUP` = "1"
///
/// A host may configure any of those. What it may not configure is an entry no
/// `envp` could hold at all, or one whose bytes the PAL could never decode into
/// the string the host wrote.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEnvironmentEntryInvariant =
    /// A NUL code unit as a string, so no source file has to contain one.
    let private nul : string = string (char 0)

    /// One entry per rejected shape, each provoking that shape alone.
    let private rejected : (string * string) list =
        [
            "null entry", null
            "NUL in the name", "A" + nul + "B=value"
            "NUL in the value", "A=va" + nul + "ue"
            "unpaired high surrogate", "A=" + string (char 0xD800)
            "unpaired low surrogate", string (char 0xDFFF) + "=value"
        ]

    /// Entries a real `envp` really can hold, so the rule cannot be satisfied by
    /// refusing everything. Deliberately including every shape a map could not
    /// express: an entry with no `=`, one beginning with `=`, an empty entry, and
    /// a duplicated name.
    let private accepted : string list =
        [
            "PLAIN=1"
            "EMPTY_VALUE="
            "EQUALS_IN_VALUE=a=b=c"
            "lower.case-name_1=v"
            "é中=\U0001F436"
            "NO_EQUALS"
            "=LEADING_EQUALS"
            ""
            "DUP=1"
            "DUP=2"
        ]

    [<Test>]
    let ``the entry rule names what is wrong`` () : unit =
        for description, entry in rejected do
            match EnvironmentPal.tryEncodeEntry entry with
            | Ok _ -> failwith $"expected %s{description} to be rejected, but the rule accepted it"
            | Error problem -> problem |> shouldNotEqual ""

    [<Test>]
    let ``the entry rule accepts what a real envp can hold`` () : unit =
        for entry in accepted do
            match EnvironmentPal.tryEncodeEntry entry with
            | Ok bytes -> UnixByteString.tryToString bytes |> shouldEqual (Some entry)
            | Error problem -> failwith $"expected %A{entry} to be accepted, but the rule said: %s{problem}"

    [<Test>]
    let ``applying a KernelConfig rejects an entry no process could hold`` () : unit =
        // `KernelConfig.toKernel` is the path every host takes, so this is where
        // the rejection has to fire. The message names the knob because this
        // call site passes that name; asserting it here is what stops the name
        // drifting to one no host has heard of.
        for description, entry in rejected do
            let config =
                { KernelConfig.Default with
                    Environment = [ "FINE=1" ; entry ]
                }

            let exn =
                Assert.Throws<System.Exception> (fun () -> KernelConfig.toKernel config |> ignore<EmulatedKernel>)

            exn.Message |> shouldContainText "KernelConfig.Environment"
            description |> shouldNotEqual ""

    [<Test>]
    let ``applying a KernelConfig keeps every entry, in order, after the defaults`` () : unit =
        let kernel =
            KernelConfig.toKernel
                { KernelConfig.Default with
                    Environment = accepted
                }

        kernel.Environment
        |> List.map UnixByteString.tryToString
        |> shouldEqual (List.map Some (EmulatedKernel.defaultEnvironment @ accepted))

    /// The default's name, spelled out rather than read from
    /// `EmulatedKernel.defaultEnvironment`, so that the composition property
    /// below states the rule rather than reusing the implementation's parse.
    let private defaultName : string = "DOTNET_SYSTEM_GLOBALIZATION_INVARIANT"

    [<Test>]
    let ``the defaults come first, unless an entry names them`` () : unit =
        EmulatedKernel.defaultEnvironment |> shouldEqual [ defaultName + "=1" ]

        let mutable displaced = 0
        let mutable kept = 0

        let genEntry : Gen<string> =
            Gen.elements
                [
                    defaultName + "=0"
                    defaultName + "="
                    defaultName
                    // Not the default's name: a prefix of it, an extension of it,
                    // and another case of it.
                    "DOTNET_SYSTEM_GLOBALIZATION=1"
                    defaultName + "_X=1"
                    defaultName.ToLowerInvariant () + "=1"
                    "OTHER=1"
                    "OTHER=1"
                    "=" + defaultName
                    ""
                ]

        let property (entries : string list) : unit =
            let names =
                entries
                |> List.exists (fun entry -> entry = defaultName || entry.StartsWith (defaultName + "="))

            let expected = (if names then [] else [ defaultName + "=1" ]) @ entries

            if names then
                displaced <- displaced + 1
            else
                kept <- kept + 1

            let kernel =
                KernelConfig.toKernel
                    { KernelConfig.Default with
                        Environment = entries
                    }

            kernel.Environment
            |> List.map UnixByteString.tryToString
            |> shouldEqual (List.map Some expected)

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 300, Prop.forAll (Arb.fromGen (Gen.listOf genEntry)) property)

        displaced > 20 |> shouldEqual true
        kept > 20 |> shouldEqual true

    [<Test>]
    let ``the default KernelConfig holds exactly the defaults`` () : unit =
        // The control for the config-path tests above: `defaultEnvironment` must
        // itself satisfy the rule, or every run would fail.
        let kernel = KernelConfig.toKernel KernelConfig.Default

        kernel.Environment
        |> List.map UnixByteString.tryToString
        |> shouldEqual (List.map Some EmulatedKernel.defaultEnvironment)

    [<Test>]
    let ``nameValueEntry refuses a pair no entry reads back as`` () : unit =
        let refused =
            [
                "null name", null, "value"
                "null value", "A", null
                "empty name", "", "value"
                "'=' in the name", "A=B", "value"
            ]

        for description, name, value in refused do
            let exn =
                Assert.Throws<System.Exception> (fun () -> EnvironmentPal.nameValueEntry name value |> ignore<string>)

            exn.Message |> shouldContainText "nameValueEntry"
            description |> shouldNotEqual ""

        EnvironmentPal.nameValueEntry "A" "" |> shouldEqual "A="
        EnvironmentPal.nameValueEntry "A" "b=c" |> shouldEqual "A=b=c"
