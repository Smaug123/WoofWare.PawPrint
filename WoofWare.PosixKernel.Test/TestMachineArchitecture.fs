namespace WoofWare.PosixKernel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

[<TestFixture>]
module TestMachineArchitecture =

    [<Test>]
    let ``the presets name their machines`` () =
        SimulatedUnixPlatform.machineArchitecture SimulatedUnixPlatform.linuxX64
        |> shouldEqual MachineArchitecture.X86_64

        SimulatedUnixPlatform.machineArchitecture SimulatedUnixPlatform.macOsArm64
        |> shouldEqual MachineArchitecture.Arm64

    /// The machine is a function of the flavour alone: a Linux platform at any release is
    /// x86_64, so no platform can claim a Darwin release alongside an x86_64 machine.
    [<Test>]
    let ``the machine follows the flavour whatever the release`` () =
        for release in [ "5.10.0" ; "6.17.0-1022-azure" ] do
            SimulatedUnixPlatform.createOrFail "test" SimulatedUnixFlavour.Linux release
            |> SimulatedUnixPlatform.machineArchitecture
            |> shouldEqual MachineArchitecture.X86_64

        SimulatedUnixPlatform.createOrFail "test" SimulatedUnixFlavour.Darwin "24.6.0"
        |> SimulatedUnixPlatform.machineArchitecture
        |> shouldEqual MachineArchitecture.Arm64
