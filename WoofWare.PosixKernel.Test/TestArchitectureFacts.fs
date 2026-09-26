namespace WoofWare.PosixKernel.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The facts that follow from a platform's architecture and page size, checked
/// against the values measured on real kernels, and the rule that no machine can
/// mix one architecture's facts with another's.
///
/// Every expected value here is a literal from
/// docs/plans/2026-08-23-posix-kernel-extraction/architecture-facts-linux.c and
/// architecture-facts-darwin.c, never a call to the derivation under test.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestArchitectureFacts =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 1000

    let private flavours : SimulatedUnixFlavour list =
        [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]

    let private architectures : SimulatedUnixArchitecture list =
        [ SimulatedUnixArchitecture.X64 ; SimulatedUnixArchitecture.Arm64 ]

    let private pageSizes : SimulatedPageSize list =
        [
            SimulatedPageSize.FourKiB
            SimulatedPageSize.SixteenKiB
            SimulatedPageSize.SixtyFourKiB
        ]

    /// The kernels that have been measured, with the page size in bytes and
    /// `getrandom`'s per-call cap (`None` where the flavour has no `getrandom`).
    let private measuredKernels
        : ((SimulatedUnixFlavour * SimulatedUnixArchitecture * SimulatedPageSize) * int * uint64 option) list =
        [
            (SimulatedUnixFlavour.Linux, SimulatedUnixArchitecture.X64, SimulatedPageSize.FourKiB),
            4096,
            Some 0x7FFF_F000UL
            (SimulatedUnixFlavour.Linux, SimulatedUnixArchitecture.Arm64, SimulatedPageSize.FourKiB),
            4096,
            Some 0x7FFF_F000UL
            (SimulatedUnixFlavour.Darwin, SimulatedUnixArchitecture.Arm64, SimulatedPageSize.SixteenKiB), 16384, None
        ]

    /// `sizeof(struct epoll_event)` and the largest `maxevents` that is not
    /// EINVAL, per architecture.
    let private measuredEpoll : (SimulatedUnixArchitecture * int * int) list =
        [
            SimulatedUnixArchitecture.X64, 12, 178_956_970
            SimulatedUnixArchitecture.Arm64, 16, 134_217_727
        ]

    /// Every `TASK_SIZE_MAX` observed, with the architecture it was observed on.
    let private observedLimits : (uint64 * SimulatedUnixArchitecture) list =
        [
            0x0000_7FFF_FFFF_F000UL, SimulatedUnixArchitecture.X64
            0x00FF_FFFF_FFFF_F000UL, SimulatedUnixArchitecture.X64
            0x0001_0000_0000_0000UL, SimulatedUnixArchitecture.Arm64
        ]

    /// Each preset, and the buffer check a fresh machine on it applies.
    let private presetChecks : (SimulatedUnixPlatform * UserBufferCheck) list =
        [
            SimulatedUnixPlatform.linuxX64, UserBufferCheck.BeforeOperation 0x0000_7FFF_FFFF_F000UL
            SimulatedUnixPlatform.linuxArm64, UserBufferCheck.BeforeOperation 0x0001_0000_0000_0000UL
            SimulatedUnixPlatform.macOsArm64, UserBufferCheck.AtCopyTime
        ]

    let private isMeasured
        (flavour : SimulatedUnixFlavour)
        (architecture : SimulatedUnixArchitecture)
        (pageSize : SimulatedPageSize)
        : bool
        =
        measuredKernels
        |> List.exists (fun (kernel, _, _) -> kernel = (flavour, architecture, pageSize))

    /// Releases a `uname` could report, plus a few it could not, so that the two
    /// ways `create` can refuse meet each other.
    let private releaseGen : Gen<string> =
        Gen.oneof
            [
                Gen.elements [ "6.17.0-1022-azure" ; "6.18.5" ; "27.0.0" ; "x" ]
                Gen.elements [ "" ; "6.8.0-é" ; String.replicate 256 "a" ]
                (ArbMap.defaults |> ArbMap.generate<string>)
                |> Gen.map (fun s -> if isNull s then "" else s)
            ]

    let private combinationGen : Gen<SimulatedUnixFlavour * SimulatedUnixArchitecture * SimulatedPageSize * string> =
        gen {
            let! flavour = Gen.elements flavours
            let! architecture = Gen.elements architectures
            let! pageSize = Gen.elements pageSizes
            let! release = releaseGen
            return flavour, architecture, pageSize, release
        }

    let private releaseIsValid (release : string) : bool =
        not (String.IsNullOrEmpty release)
        && release.Length <= 255
        && release |> Seq.forall (fun c -> c >= ' ' && c <= '~')

    /// Every platform `create` admits, with each release a preset uses.
    let private admittedPlatforms : SimulatedUnixPlatform list =
        [
            for flavour in flavours do
                for architecture in architectures do
                    for pageSize in pageSizes do
                        match SimulatedUnixPlatform.create flavour architecture pageSize "6.18.5" with
                        | Ok platform -> yield platform
                        | Error _ -> ()
        ]

    [<Test>]
    let ``every measured kernel is admitted, and nothing else is`` () : unit =
        admittedPlatforms |> List.length |> shouldEqual (List.length measuredKernels)

        let property (flavour, architecture, pageSize, release) : unit =
            match SimulatedUnixPlatform.create flavour architecture pageSize release with
            | Ok platform ->
                releaseIsValid release |> shouldEqual true
                isMeasured flavour architecture pageSize |> shouldEqual true
                SimulatedUnixPlatform.flavour platform |> shouldEqual flavour
                SimulatedUnixPlatform.architecture platform |> shouldEqual architecture
                SimulatedUnixPlatform.pageSize platform |> shouldEqual pageSize
                SimulatedUnixPlatform.unixRelease platform |> shouldEqual release
            | Error (SimulatedUnixPlatformError.Release _) -> releaseIsValid release |> shouldEqual false
            | Error (SimulatedUnixPlatformError.UnmeasuredKernel (f, a, p)) ->
                // The release is judged first, so this refusal is about the
                // combination alone.
                releaseIsValid release |> shouldEqual true
                (f, a, p) |> shouldEqual (flavour, architecture, pageSize)
                isMeasured flavour architecture pageSize |> shouldEqual false

        Check.One (config, Prop.forAll (Arb.fromGen combinationGen) property)

    [<Test>]
    let ``each preset is a measured kernel`` () : unit =
        let describe (platform : SimulatedUnixPlatform) =
            SimulatedUnixPlatform.flavour platform,
            SimulatedUnixPlatform.architecture platform,
            SimulatedUnixPlatform.pageSize platform

        describe SimulatedUnixPlatform.linuxX64
        |> shouldEqual (SimulatedUnixFlavour.Linux, SimulatedUnixArchitecture.X64, SimulatedPageSize.FourKiB)

        describe SimulatedUnixPlatform.linuxArm64
        |> shouldEqual (SimulatedUnixFlavour.Linux, SimulatedUnixArchitecture.Arm64, SimulatedPageSize.FourKiB)

        describe SimulatedUnixPlatform.macOsArm64
        |> shouldEqual (SimulatedUnixFlavour.Darwin, SimulatedUnixArchitecture.Arm64, SimulatedPageSize.SixteenKiB)

    [<Test>]
    let ``page sizes and getrandom's cap agree with the measured table`` () : unit =
        for (flavour, architecture, pageSize), pageBytes, maxTransfer in measuredKernels do
            let platform =
                SimulatedUnixPlatform.createOrFail "test" flavour architecture pageSize "6.18.5"

            SimulatedPageSize.bytes (SimulatedUnixPlatform.pageSize platform)
            |> shouldEqual pageBytes

            match maxTransfer with
            | None -> ()
            | Some expected -> UnixEntropy.getRandomMaxTransfer platform |> shouldEqual expected

    [<Test>]
    let ``epoll's constants agree with the measured table`` () : unit =
        for architecture, size, cap in measuredEpoll do
            LinuxEpollLimits.eventSize architecture |> shouldEqual size
            LinuxEpollLimits.maxEvents architecture |> shouldEqual cap

            // The property the admission screen relies on: every count the cap
            // admits has a byte extent inside `int32`, and one more does not, so
            // the bound is tight rather than merely sufficient.
            int64 cap * int64 size <= int64 Int32.MaxValue |> shouldEqual true
            int64 (cap + 1) * int64 size > int64 Int32.MaxValue |> shouldEqual true

    [<Test>]
    let ``each observed limit belongs to the architecture it was observed on`` () : unit =
        let property (limit : uint64) : unit =
            let expected =
                observedLimits
                |> List.tryFind (fun (observed, _) -> observed = limit)
                |> Option.map snd

            ObservedUserAddressLimit.architectureOf limit |> shouldEqual expected

        let limitGen =
            Gen.oneof
                [
                    Gen.elements (observedLimits |> List.map fst)
                    Gen.elements (observedLimits |> List.collect (fun (l, _) -> [ l - 1UL ; l + 1UL ]))
                    ArbMap.defaults |> ArbMap.generate<uint64>
                ]

        Check.One (config, Prop.forAll (Arb.fromGen limitGen) property)

    /// A fresh machine applies its platform's own check, and a limit can be set
    /// only where the platform screens, and only to one its architecture has.
    [<Test>]
    let ``no machine mixes one architecture's address limit with another's`` () : unit =
        for platform, expected in presetChecks do
            let system = UnixSystem.initial<int, string> platform
            UnixMachineState.userBufferCheck system.Machine |> shouldEqual expected
            UnixSystem.checkInvariants system |> shouldEqual []

        let limitGen =
            Gen.oneof
                [
                    Gen.elements (observedLimits |> List.map fst)
                    Gen.elements [ 0UL ; 1UL ; UInt64.MaxValue ]
                    ArbMap.defaults |> ArbMap.generate<uint64>
                ]

        let property (platform : SimulatedUnixPlatform, limit : uint64) : unit =
            let machine = (UnixSystem.initial<int, string> platform).Machine

            let admissible =
                SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux
                && observedLimits
                   |> List.contains (limit, SimulatedUnixPlatform.architecture platform)

            let outcome =
                try
                    Ok (UnixMachineState.withUserAddressLimit limit machine)
                with e ->
                    Error e.Message

            match outcome with
            | Ok after ->
                admissible |> shouldEqual true

                UnixMachineState.userBufferCheck after
                |> shouldEqual (UserBufferCheck.BeforeOperation limit)
            | Error message ->
                admissible |> shouldEqual false
                message |> shouldContainText "UnixMachineState.withUserAddressLimit"

        let gen = Gen.zip (Gen.elements (presetChecks |> List.map fst)) limitGen

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    /// A record update is the one way past `withUserAddressLimit`, and
    /// `checkInvariants` is what catches it.
    [<Test>]
    let ``a check its platform cannot have is a defect`` () : unit =
        let checkGen : Gen<UserBufferCheck> =
            Gen.oneof
                [
                    Gen.constant UserBufferCheck.AtCopyTime
                    Gen.elements (observedLimits |> List.map (fst >> UserBufferCheck.BeforeOperation))
                    ArbMap.defaults
                    |> ArbMap.generate<uint64>
                    |> Gen.map UserBufferCheck.BeforeOperation
                ]

        let property (platform : SimulatedUnixPlatform, check : UserBufferCheck) : unit =
            let system = UnixSystem.initial<int, string> platform

            let forged =
                { system with
                    Machine =
                        { system.Machine with
                            UserBufferCheck = check
                        }
                }

            let sound =
                match SimulatedUnixPlatform.flavour platform, check with
                | SimulatedUnixFlavour.Darwin, UserBufferCheck.AtCopyTime -> true
                | SimulatedUnixFlavour.Linux, UserBufferCheck.BeforeOperation limit ->
                    observedLimits
                    |> List.contains (limit, SimulatedUnixPlatform.architecture platform)
                | _ -> false

            UnixSystem.checkInvariants forged
            |> shouldEqual (
                if sound then
                    []
                else
                    [ UnixSystemDefect.UserBufferCheckNotOfPlatform (platform, check) ]
            )

        let gen = Gen.zip (Gen.elements (presetChecks |> List.map fst)) checkGen

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)
