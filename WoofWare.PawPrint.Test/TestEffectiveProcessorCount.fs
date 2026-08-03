namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `EmulatedKernel.effectiveProcessorCount` reproduces CoreCLR's
/// `GetCurrentProcessCpuCount` (coreclr/utilcode/util.cpp): the
/// `PROCESSOR_COUNT` configuration knob wins when it parses to a value in
/// `(0, 0xffff]`, otherwise the detected count — which for PawPrint is the
/// host-configured `EmulatedKernel.ProcessorCount` — is used.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEffectiveProcessorCount =

    /// Upper bound CoreCLR accepts from the knob (`MAX_PROCESSOR_COUNT`).
    let private maxConfigured = 0xffff

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    /// A kernel whose detection-equivalent count is `detected` and whose
    /// environment carries exactly the supplied overrides.
    let private kernelWith (detected : int) (overrides : (string * string) list) : EmulatedKernel =
        let kernel = EmulatedKernel.initial |> EmulatedKernel.withProcessorCount detected

        kernel |> EmulatedKernel.withEnvironment (Map.ofList overrides)

    [<Test>]
    let ``with no override, the configured count is reported`` () =
        let property (detected : int) : bool =
            let detected = 1 + abs (detected % maxConfigured)
            EmulatedKernel.effectiveProcessorCount (kernelWith detected []) = detected

        Check.One (propertyConfig, Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary<int>) property)

    [<Test>]
    let ``an in-range DOTNET_ override always wins over the configured count`` () =
        // Both are drawn independently, so this also pins down that the
        // override is not merely coincidentally equal to the detected value.
        let property (detectedSeed : int, overrideSeed : int) : bool =
            let detected = 1 + abs (detectedSeed % maxConfigured)
            let overrideValue = 1 + abs (overrideSeed % maxConfigured)

            let kernel = kernelWith detected [ "DOTNET_PROCESSOR_COUNT", string overrideValue ]

            EmulatedKernel.effectiveProcessorCount kernel = overrideValue

        Check.One (propertyConfig, Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary<int * int>) property)

    [<Test>]
    let ``an out-of-range override is ignored in favour of the configured count`` () =
        let property (detectedSeed : int, overrideValue : int) : bool =
            let detected = 1 + abs (detectedSeed % maxConfigured)

            // Anything at or below zero, or above MAX_PROCESSOR_COUNT.
            let outOfRange =
                if overrideValue > 0 && overrideValue <= maxConfigured then
                    overrideValue + maxConfigured
                else
                    overrideValue

            let kernel = kernelWith detected [ "DOTNET_PROCESSOR_COUNT", string outOfRange ]

            EmulatedKernel.effectiveProcessorCount kernel = detected

        Check.One (propertyConfig, Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary<int * int>) property)

    [<Test>]
    let ``the result is always a legal ProcessorCount`` () =
        // The value reaches guest code that divides by it, so the postcondition
        // that matters most is simply "positive", whatever the environment says.
        let property (detectedSeed : int, raw : NonNull<string>) : bool =
            let detected = 1 + abs (detectedSeed % maxConfigured)

            let kernel = kernelWith detected [ "DOTNET_PROCESSOR_COUNT", raw.Get ]

            EmulatedKernel.effectiveProcessorCount kernel >= 1

        Check.One (propertyConfig, Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary<int * NonNull<string>>) property)

    [<Test>]
    let ``DOTNET_ takes precedence over COMPlus_`` () =
        // CLRConfig consults the COMPlus_ fallback only when the DOTNET_ name is
        // absent, so a present-but-unusable DOTNET_ value must NOT fall through
        // to COMPlus_ — it falls through to detection instead.
        let kernel =
            kernelWith 3 [ "DOTNET_PROCESSOR_COUNT", "7" ; "COMPlus_PROCESSOR_COUNT", "9" ]

        EmulatedKernel.effectiveProcessorCount kernel |> shouldEqual 7

        let kernel =
            kernelWith 3 [ "DOTNET_PROCESSOR_COUNT", "bogus" ; "COMPlus_PROCESSOR_COUNT", "9" ]

        EmulatedKernel.effectiveProcessorCount kernel |> shouldEqual 3

    [<Test>]
    let ``COMPlus_ is honoured when DOTNET_ is absent`` () =
        kernelWith 3 [ "COMPlus_PROCESSOR_COUNT", "9" ]
        |> EmulatedKernel.effectiveProcessorCount
        |> shouldEqual 9

    /// Table of `strtoul`-shaped parse cases. CoreCLR reads the knob with
    /// `u16_strtoul(val, &endPtr, 10)` and accepts the result when at least one
    /// digit was consumed, so trailing garbage is tolerated and a leading
    /// non-digit is not. Failure substitutes the knob's declared default of 0,
    /// which then loses the `0 < value` test.
    let strtoulCases : obj array list =
        [
            // Plain decimal, and decimal despite looking like hex input: the
            // knob is declared ParseIntegerAsBase10, unlike most CLRConfig
            // DWORDs which parse as hex.
            [| box "8" ; box 8 |]
            [| box "10" ; box 10 |]
            // Trailing garbage is ignored by strtoul once digits are consumed.
            [| box "4abc" ; box 4 |]
            [| box "6 " ; box 6 |]
            // Leading whitespace is skipped.
            [| box "  12" ; box 12 |]
            // A leading '+' is accepted by strtoul.
            [| box "+5" ; box 5 |]
            // No digits consumed => failure => fall back to detection (3).
            [| box "" ; box 3 |]
            [| box "abc" ; box 3 |]
            [| box "   " ; box 3 |]
            // Parses, but outside (0, MAX_PROCESSOR_COUNT].
            [| box "0" ; box 3 |]
            [| box "65536" ; box 3 |]
            [| box "999999999999999999999" ; box 3 |]
            // Negative: rejected here; CoreCLR's strtoul would wrap it to a
            // value far above MAX_PROCESSOR_COUNT and reject it too.
            [| box "-1" ; box 3 |]
        ]

    [<TestCaseSource(nameof strtoulCases)>]
    let ``knob parsing matches CoreCLR's strtoul shape`` (raw : string, expected : int) =
        kernelWith 3 [ "DOTNET_PROCESSOR_COUNT", raw ]
        |> EmulatedKernel.effectiveProcessorCount
        |> shouldEqual expected
