namespace WoofWare.Pawprint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PawPrint.ExternImplementations
open WoofWare.PawPrint.Test

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFSharpPureCases =

    let private rid = RuntimeInformation.RuntimeIdentifier

    let private publishDir =
        Path.Combine (
            __SOURCE_DIRECTORY__,
            "..",
            "WoofWare.PawPrint.Test.FSharpPureCases",
            "bin",
            "Release",
            "net9.0",
            rid,
            "publish"
        )

    let private dllPath =
        Path.Combine (publishDir, "WoofWare.PawPrint.Test.FSharpPureCases.dll")

    let private loadImage () : byte array =
        if not (File.Exists dllPath) then
            Assert.Inconclusive "F# test cases not published; run dotnet publish first"

        File.ReadAllBytes dllPath

    let testCases : string list = [ "Placeholder" ; "BeqBranch" ]

    // PawPrint cannot yet allocate string argv (Program.allocateArgs is unimplemented),
    // so all F# test cases that require argv dispatch are unimplemented for now.
    let unimplemented : Set<string> = Set.ofList [ "Placeholder" ; "BeqBranch" ]

    let private runTest (testCaseName : string) : unit =
        let image = loadImage ()
        let messages, loggerFactory = LoggerFactory.makeTest ()

        let dotnetRuntimes =
            seq {
                yield publishDir
                yield! DotnetRuntime.SelectForDll (typeof<RunResult>.Assembly.Location)
            }
            |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        try
            let realResult = RealRuntime.executeWithRealRuntime [| testCaseName |] image

            let pawPrintResult =
                Program.run loggerFactory (Some dllPath) peImage dotnetRuntimes (MockEnv.make ()) [ testCaseName ]

            match realResult, pawPrintResult with
            | RealRuntimeResult.NormalExit exitCode, RunOutcome.NormalExit (terminalState, terminatingThread) ->
                exitCode |> shouldEqual 0

                let pawPrintExitCode =
                    match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                    | [] -> failwith "expected program to return a value, but it returned void"
                    | head :: _ ->
                        match head with
                        | EvalStackValue.Int32 i -> i
                        | ret -> failwith $"expected program to return an int, but it returned %O{ret}"

                pawPrintExitCode |> shouldEqual exitCode
            | RealRuntimeResult.UnhandledException _, RunOutcome.GuestUnhandledException _ -> ()
            | RealRuntimeResult.NormalExit exitCode, RunOutcome.GuestUnhandledException (_, _, exn) ->
                failwith
                    $"Real runtime exited normally with code %d{exitCode}, but PawPrint threw unhandled exception: %O{exn.ExceptionObject}"
            | RealRuntimeResult.UnhandledException realExn, RunOutcome.NormalExit (terminalState, terminatingThread) ->
                let pawPrintExitCode =
                    match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                    | [] -> None
                    | EvalStackValue.Int32 i :: _ -> Some i
                    | _ -> None

                failwith
                    $"Real runtime threw unhandled %s{realExn.GetType().Name}, but PawPrint exited normally (code: %O{pawPrintExitCode})"
        with _ ->
            for message in messages () do
                System.Console.Error.WriteLine $"{message}"

            reraise ()

    [<TestCaseSource(nameof testCases)>]
    let ``F# pure tests`` (testCaseName : string) =
        if unimplemented.Contains testCaseName then
            Assert.Inconclusive $"Test case '%s{testCaseName}' is not yet implemented in PawPrint"

        runTest testCaseName

    [<TestCaseSource(nameof unimplemented)>]
    let ``Unimplemented F# tests have correct real-runtime behaviour`` (testCaseName : string) =
        let image = loadImage ()

        match RealRuntime.executeWithRealRuntime [| testCaseName |] image with
        | RealRuntimeResult.NormalExit exitCode -> exitCode |> shouldEqual 0
        | RealRuntimeResult.UnhandledException exn ->
            failwith $"Real runtime threw unhandled %s{exn.GetType().Name} for %s{testCaseName}: %s{exn.Message}"
