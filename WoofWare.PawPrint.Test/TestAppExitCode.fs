namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `AppExitCode.checkConsistent`: the App's exit code against how the kernel says the
/// simulated process ended.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestAppExitCode =

    let private flavours : SimulatedUnixFlavour list =
        [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]

    let private numberingOf (flavour : SimulatedUnixFlavour) : SignalNumbering =
        match flavour with
        | SimulatedUnixFlavour.Linux -> SignalNumbering.Linux
        | SimulatedUnixFlavour.Darwin -> SignalNumbering.Darwin

    [<Test>]
    let ``the whole latched exit code reads as whatever the kernel kept of it`` () : unit =
        let property (flavour : SimulatedUnixFlavour) (latched : int) : unit =
            let termination =
                ProcessTermination.Exited (ExitStatus.ofExitArgument flavour latched)

            AppExitCode.checkConsistent (numberingOf flavour) latched termination
            |> shouldEqual (Ok ())

            // Any other low byte disagrees.
            AppExitCode.checkConsistent (numberingOf flavour) (latched + 1) termination
            |> Result.isError
            |> shouldEqual true

        let gen = Gen.zip (Gen.elements flavours) (ArbMap.defaults |> ArbMap.generate<int>)

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 1000,
            Prop.forAll (Arb.fromGen gen) (fun (flavour, latched) -> property flavour latched)
        )

    [<Test>]
    let ``a death by signal reads as 128 plus the signal's own number, and as nothing else`` () : unit =
        for flavour in flavours do
            let numbering = numberingOf flavour

            for signo in 1 .. Signal.highestSignoUnder numbering do
                match Signal.ofRawSignoUnder numbering signo with
                | ValueNone -> ()
                | ValueSome signal ->
                    for coreDumped in [ false ; true ] do
                        let termination = ProcessTermination.Signaled (signal, coreDumped)

                        AppExitCode.checkConsistent numbering (128 + signo) termination
                        |> shouldEqual (Ok ())

                        AppExitCode.checkConsistent numbering signo termination
                        |> Result.isError
                        |> shouldEqual true

                        // An exit with the same code is not a death by the signal.
                        AppExitCode.checkConsistent
                            numbering
                            (128 + signo)
                            (ProcessTermination.Exited (ExitStatus.ofExitArgument flavour signo))
                        |> Result.isError
                        |> shouldEqual true
