namespace WoofWare.PosixKernel.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The machine's boot-image setters whose acceptance rule is a predicate over
/// a range, and which no fixture about what the setting *means* already holds
/// to that rule: each admits exactly the values the rule admits, and refuses
/// every other with the case that names why.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMachineSetterRefusals =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private platforms : SimulatedUnixPlatform list =
        [
            SimulatedUnixPlatform.linuxX64
            SimulatedUnixPlatform.linuxArm64
            SimulatedUnixPlatform.macOsArm64
        ]

    let private imageOn (platform : SimulatedUnixPlatform) : UnixBootImage<int, string> = UnixSystem.initial platform

    /// An int weighted towards zero and the type's ends, where a sign test
    /// goes wrong, as well as spread across the whole range.
    let private anyInt : Gen<int> =
        Gen.oneof
            [
                Gen.choose (-3, 3)
                Gen.elements [ Int32.MinValue ; Int32.MinValue + 1 ; Int32.MaxValue - 1 ; Int32.MaxValue ]
                ArbMap.defaults |> ArbMap.generate<int>
            ]

    let private anyInt64 : Gen<int64> =
        Gen.oneof
            [
                Gen.choose64 (-3L, 3L)
                Gen.elements [ Int64.MinValue ; Int64.MinValue + 1L ; Int64.MaxValue - 1L ; Int64.MaxValue ]
                ArbMap.defaults |> ArbMap.generate<int64>
            ]

    let private anyPort : Gen<uint16> =
        Gen.oneof
            [
                Gen.elements [ 0us ; 1us ; 2us ; UInt16.MaxValue - 1us ; UInt16.MaxValue ]
                ArbMap.defaults |> ArbMap.generate<uint16>
            ]

    // ------------------------------------------------------------ somaxconn

    [<Test>]
    let ``withSoMaxConn admits exactly the positive values, and refuses the rest as not positive`` () : unit =
        let property (platform : SimulatedUnixPlatform, value : int) : unit =
            match UnixBootImage.withSoMaxConn (Some value) (imageOn platform), value >= 1 with
            | Ok image, true ->
                (Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0) image).Machine.SoMaxConn
                |> shouldEqual value
            | Error refusal, false -> refusal |> shouldEqual (SoMaxConnRefusal.NotPositive value)
            | Ok _, false -> failwith $"%d{value} was admitted on %O{platform}"
            | Error refusal, true -> failwith $"%d{value} was refused on %O{platform}: %A{refusal}"

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen (Gen.zip (Gen.elements platforms) anyInt)) property)

    [<Test>]
    let ``withSoMaxConn's boundary is between 0 and 1`` () : unit =
        for platform in platforms do
            UnixBootImage.withSoMaxConn (Some 0) (imageOn platform)
            |> Result.map (fun _ -> ())
            |> shouldEqual (Error (SoMaxConnRefusal.NotPositive 0))

            (UnixBootImage.withSoMaxConn (Some 1) (imageOn platform)
             |> Configured.expectOk SoMaxConnRefusal.describe
             |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0))
                .Machine.SoMaxConn
            |> shouldEqual 1

    // ------------------------------------------------------ processor count

    [<Test>]
    let ``withProcessorCount admits exactly the positive counts, and refuses the rest as not positive`` () : unit =
        let property (platform : SimulatedUnixPlatform, count : int) : unit =
            match UnixBootImage.withProcessorCount count (imageOn platform), count >= 1 with
            | Ok image, true ->
                UnixMachineState.processorCount
                    (Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0) image).Machine
                |> shouldEqual count
            | Error refusal, false -> refusal |> shouldEqual (ProcessorCountRefusal.NotPositive count)
            | Ok _, false -> failwith $"%d{count} was admitted on %O{platform}"
            | Error refusal, true -> failwith $"%d{count} was refused on %O{platform}: %A{refusal}"

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen (Gen.zip (Gen.elements platforms) anyInt)) property)

    [<Test>]
    let ``withProcessorCount's boundary is between 0 and 1`` () : unit =
        for platform in platforms do
            UnixBootImage.withProcessorCount 0 (imageOn platform)
            |> Result.map (fun _ -> ())
            |> shouldEqual (Error (ProcessorCountRefusal.NotPositive 0))

            UnixBootImage.withProcessorCount 1 (imageOn platform)
            |> Configured.expectOk ProcessorCountRefusal.describe
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> fun system -> UnixMachineState.processorCount system.Machine
            |> shouldEqual 1

    // ------------------------------------------------ ephemeral port range

    [<Test>]
    let ``withEphemeralPortRange admits exactly the non-empty ranges that leave out port 0`` () : unit =
        let property (platform : SimulatedUnixPlatform, low : uint16, high : uint16) : unit =
            let expected : Result<unit, EphemeralPortRangeRefusal> =
                if low = 0us then
                    Error (EphemeralPortRangeRefusal.LowIsZero high)
                elif low > high then
                    Error (EphemeralPortRangeRefusal.Empty (low, high))
                else
                    Ok ()

            match UnixBootImage.withEphemeralPortRange (low, high) (imageOn platform) with
            | Ok image ->
                expected |> shouldEqual (Ok ())

                let machine =
                    (Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0) image).Machine

                machine.EphemeralPortRange |> shouldEqual (low, high)
                // Rewound into the range, so the first ephemeral port is its low end.
                machine.NextEphemeralPort |> shouldEqual low
            | Error refusal -> Error refusal |> shouldEqual expected

        let gen =
            Gen.oneof
                [
                    Gen.zip3 (Gen.elements platforms) anyPort anyPort
                    // Equal ends, the narrowest range there is, from both sides of
                    // the boundary.
                    gen {
                        let! platform = Gen.elements platforms
                        let! port = anyPort
                        return platform, port, port
                    }
                ]

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``withEphemeralPortRange's boundaries`` () : unit =
        let outcome (low : uint16, high : uint16) : Result<unit, EphemeralPortRangeRefusal> =
            UnixBootImage.withEphemeralPortRange (low, high) (imageOn SimulatedUnixPlatform.linuxX64)
            |> Result.map (fun _ -> ())

        outcome (0us, 0us)
        |> shouldEqual (Error (EphemeralPortRangeRefusal.LowIsZero 0us))

        outcome (0us, 10us)
        |> shouldEqual (Error (EphemeralPortRangeRefusal.LowIsZero 10us))

        outcome (2us, 1us)
        |> shouldEqual (Error (EphemeralPortRangeRefusal.Empty (2us, 1us)))

        outcome (1us, 1us) |> shouldEqual (Ok ())
        outcome (UInt16.MaxValue, UInt16.MaxValue) |> shouldEqual (Ok ())

    // ------------------------------------------------------------ pipe device

    [<Test>]
    let ``withPipeDevice admits a device of the flavour, and refuses the rest naming why`` () : unit =
        let property (platform : SimulatedUnixPlatform, device : int64) : unit =
            let flavour = SimulatedUnixPlatform.flavour platform

            let expected : Result<unit, PipeDeviceRefusal> =
                match flavour with
                | SimulatedUnixFlavour.Darwin when device <> 0L -> Error (PipeDeviceRefusal.DarwinReportsZero device)
                | _ when device < 0L -> Error (PipeDeviceRefusal.Negative device)
                | _ -> Ok ()

            match UnixBootImage.withPipeDevice (Some device) (imageOn platform) with
            | Ok image ->
                expected |> shouldEqual (Ok ())

                (Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0) image).Machine.PipeDevice
                |> shouldEqual device
            | Error refusal -> Error refusal |> shouldEqual expected

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen (Gen.zip (Gen.elements platforms) anyInt64)) property)

    [<Test>]
    let ``withPipeDevice's boundaries`` () : unit =
        let outcome (platform : SimulatedUnixPlatform) (device : int64) : Result<unit, PipeDeviceRefusal> =
            UnixBootImage.withPipeDevice (Some device) (imageOn platform)
            |> Result.map (fun _ -> ())

        outcome SimulatedUnixPlatform.linuxX64 -1L
        |> shouldEqual (Error (PipeDeviceRefusal.Negative -1L))

        outcome SimulatedUnixPlatform.linuxX64 0L |> shouldEqual (Ok ())

        // Darwin reports 0 for every pipe, so a negative device there is refused
        // for that, first.
        outcome SimulatedUnixPlatform.macOsArm64 -1L
        |> shouldEqual (Error (PipeDeviceRefusal.DarwinReportsZero -1L))

        outcome SimulatedUnixPlatform.macOsArm64 1L
        |> shouldEqual (Error (PipeDeviceRefusal.DarwinReportsZero 1L))

        outcome SimulatedUnixPlatform.macOsArm64 0L |> shouldEqual (Ok ())

        for platform in platforms do
            (UnixBootImage.withPipeDevice None (imageOn platform)
             |> Configured.expectOk PipeDeviceRefusal.describe
             |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0))
                .Machine.PipeDevice
            |> shouldEqual (UnixMachineState.defaultPipeDevice (SimulatedUnixPlatform.flavour platform))

    // -------------------------------------------------------------- describe

    /// Every case of every machine setter's refusal, described: the text a
    /// client composes its diagnostic from names the value refused, so a host
    /// reading it can find which of its settings to change.
    [<Test>]
    let ``each refusal's description names the value refused`` () : unit =
        let timestamp = UnixTimestamp.createOrFail "TestMachineSetterRefusals" -5L 123

        let rows : (string * string) list =
            [
                UserAddressLimitRefusal.describe (UserAddressLimitRefusal.NoUpFrontScreen SimulatedUnixFlavour.Darwin),
                "Darwin"
                UserAddressLimitRefusal.describe (
                    UserAddressLimitRefusal.NotObservedOn (0x1234UL, SimulatedUnixArchitecture.X64, None)
                ),
                "0x1234"
                UserAddressLimitRefusal.describe (
                    UserAddressLimitRefusal.NotObservedOn (
                        0x1234UL,
                        SimulatedUnixArchitecture.X64,
                        Some SimulatedUnixArchitecture.Arm64
                    )
                ),
                "Arm64"
                ProcessorCountRefusal.describe (ProcessorCountRefusal.NotPositive -7), "-7"
                EphemeralPortRangeRefusal.describe (EphemeralPortRangeRefusal.LowIsZero 77us), "0-77"
                EphemeralPortRangeRefusal.describe (EphemeralPortRangeRefusal.Empty (9us, 8us)), "9-8"
                BootTimeRefusal.describe (BootTimeRefusal.BeforeEpoch timestamp), string<UnixTimestamp> timestamp
                BootTimeRefusal.describe (BootTimeRefusal.PastMaxBootTime (timestamp, 42L)), "42"
                BootTimeRefusal.describe (BootTimeRefusal.FinerThanMicrosecond timestamp),
                string<UnixTimestamp> timestamp
                MountRefusal.describe (
                    MountRefusal.NotReportableUnder (EmulatedFileSystemType.Apfs, SimulatedUnixFlavour.Linux)
                ),
                "Apfs"
                ProtectedFilesRefusal.describe (
                    ProtectedFilesRefusal.NoSuchSysctls (
                        { ProtectedFiles.off with
                            Hardlinks = HardlinkProtection.NonOwnersNeedReadAndWrite
                        },
                        SimulatedUnixFlavour.Darwin
                    )
                ),
                "NonOwnersNeedReadAndWrite"
                PipeDeviceRefusal.describe (PipeDeviceRefusal.DarwinReportsZero 31L), "31"
                PipeDeviceRefusal.describe (PipeDeviceRefusal.Negative -31L), "-31"
                SoMaxConnRefusal.describe (SoMaxConnRefusal.NotPositive -9), "-9"
                TcpSendSpaceRefusal.describe (TcpSendSpaceRefusal.NotReadOn (SimulatedUnixFlavour.Linux, 4321)), "4321"
                TcpSendSpaceRefusal.describe (TcpSendSpaceRefusal.AboveSocketBufferMax (4321, 99)), "99"
                TcpSendSpaceRefusal.describe (TcpSendSpaceRefusal.BelowLoopbackSendPipe (4321, 98)), "98"
            ]

        for described, value in rows do
            described |> shouldContainText value
