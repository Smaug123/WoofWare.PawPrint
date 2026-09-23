namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// How CLRConfig reads a knob out of the environment, which differs from a plain
/// lookup once the environment holds an empty entry: `CLRConfig::Initialize`'s
/// walk of the environment block stops there, and a knob it did not record reads
/// as unset.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestClrConfigEnvironment =

    let private environmentOf (entries : string list) : UnixByteString list =
        entries
        |> List.map (fun entry ->
            match UnixByteString.ofString entry with
            | Ok bytes -> bytes
            | Error defect -> failwith $"test entry %A{entry}: %s{UnixPathText.describe defect}"
        )

    /// Rows measured by `execve`-ing the real runtime (Darwin 25.6 with .NET
    /// 10.0.7, Linux 6.18.5 with .NET 10.0.11; the two agreed on every row) with
    /// exactly `HOME=/tmp` followed by these entries, and reading
    /// `Environment.ProcessorCount`: `Some "5"` (or the value shown) where the
    /// runtime reported that count, `None` where it reported the machine's own.
    let private measured : (string * string list * string option) list =
        [
            "control", [ "DOTNET_PROCESSOR_COUNT=5" ], Some "5"
            "after an empty entry", [ "" ; "DOTNET_PROCESSOR_COUNT=5" ], None
            "cache disabled before the empty entry",
            [ "DOTNET_DisableConfigCache=1" ; "" ; "DOTNET_PROCESSOR_COUNT=5" ],
            Some "5"
            "cache disabled after the empty entry",
            [ "" ; "DOTNET_DisableConfigCache=1" ; "DOTNET_PROCESSOR_COUNT=5" ],
            Some "5"
            "cache disabled with zero", [ "DOTNET_DisableConfigCache=0" ; "" ; "DOTNET_PROCESSOR_COUNT=5" ], None
            "cache disabled with hex", [ "DOTNET_DisableConfigCache=0x10" ; "" ; "DOTNET_PROCESSOR_COUNT=5" ], Some "5"
            "lower-case prefix records the name",
            [ "dotnet_PROCESSOR_COUNT=7" ; "" ; "DOTNET_PROCESSOR_COUNT=5" ],
            Some "5"
            "COMPlus_ records the name", [ "COMPlus_PROCESSOR_COUNT=6" ; "" ; "DOTNET_PROCESSOR_COUNT=5" ], Some "5"
            "a Bloom collision records the name", [ "DOTNET_XR=1" ; "" ; "DOTNET_PROCESSOR_COUNT=5" ], Some "5"
            "no Bloom collision", [ "DOTNET_XS=1" ; "" ; "DOTNET_PROCESSOR_COUNT=5" ], None
            "U+017F folds to S", [ "COMPLU\u017F_PROCESSOR_COUNT=6" ; "" ; "DOTNET_PROCESSOR_COUNT=5" ], Some "5"
            "an entry with no = records nothing", [ "DOTNET_PROCESSOR_COUNT" ; "" ; "COMPlus_PROCESSOR_COUNT=6" ], None
            "an empty value records the name and falls back",
            [ "DOTNET_PROCESSOR_COUNT=" ; "" ; "COMPlus_PROCESSOR_COUNT=6" ],
            Some "6"
            "the first of two entries wins", [ "DOTNET_PROCESSOR_COUNT=5" ; "DOTNET_PROCESSOR_COUNT=6" ], Some "5"
            // An empty knob name is recorded by hashing the rest of the entry,
            // `=` and value included, and `=W` hashes to `PROCESSOR_COUNT`'s bit.
            "an empty knob name hashes its value", [ "DOTNET_=W" ; "" ; "DOTNET_PROCESSOR_COUNT=5" ], Some "5"
            "an empty knob name, no collision", [ "DOTNET_=V" ; "" ; "DOTNET_PROCESSOR_COUNT=5" ], None
        ]

    [<Test>]
    let ``CLRConfig reads each measured environment as the real runtime did`` () : unit =
        for description, entries, expected in measured do
            let environment = environmentOf ("HOME=/tmp" :: entries)

            ClrConfigEnvironment.tryGetValue "test" environment "PROCESSOR_COUNT"
            |> fun actual ->
                if actual <> expected then
                    failwith $"%s{description}: expected %A{expected}, got %A{actual}"

    [<Test>]
    let ``the processor count ignores a knob set after an empty entry`` () : unit =
        // The whole chain through the kernel, which is what the guest sees; the
        // default entry PawPrint puts first records a different bit.
        let kernel (entries : string list) : EmulatedKernel =
            KernelConfig.toKernel
                { KernelConfig.Default with
                    ProcessorCount = 3
                    Environment = entries
                }

        EmulatedKernel.effectiveProcessorCount (kernel [ "" ; "DOTNET_PROCESSOR_COUNT=5" ])
        |> shouldEqual 3

        EmulatedKernel.effectiveProcessorCount (kernel [ "DOTNET_PROCESSOR_COUNT=5" ; "" ])
        |> shouldEqual 5

    [<Test>]
    let ``a name too long for CLRConfig's buffer reads as unset`` () : unit =
        let at (length : int) : string option =
            let name = System.String ('K', length)
            ClrConfigEnvironment.tryGetValue "test" (environmentOf [ $"DOTNET_%s{name}=1" ]) name

        at 54 |> shouldEqual (Some "1")
        at 55 |> shouldEqual None

    /// Entries drawn so that knob names recur, prefixes vary in case, values are
    /// sometimes empty, and some entries have no `=`; never empty, because an
    /// empty entry is the one thing that makes the walk miss a name.
    let private genEntry : Gen<string> =
        Gen.oneof
            [
                Gen.elements [ "DOTNET_" ; "COMPlus_" ; "dotnet_" ; "complus_" ; "DOTNETX" ; "OTHER_" ; "" ]
                |> Gen.map2 (fun name prefix -> prefix + name) (Gen.elements [ "A" ; "B" ; "PROCESSOR_COUNT" ; "XR" ])
                |> Gen.map2 (fun value entry -> entry + value) (Gen.elements [ "=1" ; "=" ; "=0x1" ; "" ])
            ]

    /// A plain lookup of `DOTNET_<name>` then `COMPlus_<name>`, with an empty
    /// value counting as unset: what CLRConfig reads when its cache cannot
    /// have missed anything.
    let private lookUpInFull (environment : UnixByteString list) (name : string) : string option =
        let tryVariable (variable : string) : string option =
            match EnvironmentPal.tryGetValue "test" variable environment with
            | Some "" -> None
            | other -> other

        match tryVariable ("DOTNET_" + name) with
        | Some value -> Some value
        | None -> tryVariable ("COMPlus_" + name)

    [<Test>]
    let ``without an empty entry the cache never changes an answer`` () : unit =
        let mutable found = 0

        let property (entries : string list, name : string) : unit =
            let environment = environmentOf entries
            let expected = lookUpInFull environment name

            if expected.IsSome then
                found <- found + 1

            ClrConfigEnvironment.tryGetValue "test" environment name |> shouldEqual expected

        let gen =
            Gen.zip (Gen.listOf genEntry) (Gen.elements [ "A" ; "B" ; "PROCESSOR_COUNT" ; "XR" ; "DisableConfigCache" ])

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 1000, Prop.forAll (Arb.fromGen gen) property)
        found > 100 |> shouldEqual true

    [<Test>]
    let ``with the cache disabled every knob is looked up in full`` () : unit =
        let property (before : string list, after : string list, name : string) : unit =
            let environment =
                environmentOf (before @ [ "DOTNET_DisableConfigCache=1" ; "" ] @ after)

            ClrConfigEnvironment.tryGetValue "test" environment name
            |> shouldEqual (lookUpInFull environment name)

        let gen =
            Gen.zip3 (Gen.listOf genEntry) (Gen.listOf genEntry) (Gen.elements [ "A" ; "B" ; "PROCESSOR_COUNT" ; "XR" ])

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 500, Prop.forAll (Arb.fromGen gen) property)
