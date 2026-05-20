namespace WoofWare.Pawprint.Test

open System.Collections.Immutable
open System.Diagnostics
open System.IO
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PawPrint.Test

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFSharpPureCases =

    let private rid = RuntimeInformation.RuntimeIdentifier

    let private projectDir =
        Path.Combine (__SOURCE_DIRECTORY__, "..", "WoofWare.PawPrint.Test.FSharpPureCases")

    let private projectFile =
        Path.Combine (projectDir, "WoofWare.PawPrint.Test.FSharpPureCases.fsproj")

    // Output goes to a path we control so the test isn't coupled to the project's TargetFramework.
    let private publishDir =
        Path.Combine (projectDir, "bin", "Release", "pawprint-test-publish", rid)

    let private dllPath =
        Path.Combine (publishDir, "WoofWare.PawPrint.Test.FSharpPureCases.dll")

    let private publishOnce : Lazy<unit> =
        lazy
            let psi = ProcessStartInfo "dotnet"
            psi.ArgumentList.Add "publish"
            psi.ArgumentList.Add "--configuration"
            psi.ArgumentList.Add "Release"
            psi.ArgumentList.Add "--self-contained"
            psi.ArgumentList.Add "--runtime"
            psi.ArgumentList.Add rid
            psi.ArgumentList.Add "--output"
            psi.ArgumentList.Add publishDir
            psi.ArgumentList.Add projectFile
            psi.RedirectStandardOutput <- true
            psi.RedirectStandardError <- true
            psi.UseShellExecute <- false

            use proc =
                Process.Start psi
                |> Option.ofObj
                |> Option.defaultWith (fun () -> failwith "Process.Start returned null for dotnet publish")

            // Read both pipes concurrently so a chatty child can't deadlock by filling
            // the stderr buffer while we're blocked draining stdout (or vice versa).
            let stdoutTask = proc.StandardOutput.ReadToEndAsync ()
            let stderrTask = proc.StandardError.ReadToEndAsync ()
            proc.WaitForExit ()
            let stdout = stdoutTask.GetAwaiter().GetResult ()
            let stderr = stderrTask.GetAwaiter().GetResult ()

            if proc.ExitCode <> 0 then
                failwith
                    $"dotnet publish failed with exit code %d{proc.ExitCode} for %s{projectFile}.\nSTDOUT:\n%s{stdout}\nSTDERR:\n%s{stderr}"

            if not (File.Exists dllPath) then
                failwith
                    $"dotnet publish completed but %s{dllPath} does not exist.\nSTDOUT:\n%s{stdout}\nSTDERR:\n%s{stderr}"

    let private loadImage () : byte array =
        publishOnce.Force ()
        File.ReadAllBytes dllPath

    let testCases : string list = [ "Placeholder" ; "CeqBranch" ]

    // PawPrint cannot yet allocate string argv (Program.allocateArgs is unimplemented),
    // so all F# test cases that require argv dispatch are unimplemented for now.
    let unimplemented : Set<string> = Set.ofList []

    // F# test cases that legitimately throw under both runtimes. Without this set, a test
    // that crashes both runtimes would silently pass — see TestPureCases.fs for the same
    // mechanism. Add a case here only when the throw is the intended observable.
    let expectsUnhandledException : Set<string> = Set.empty

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
                Program.run
                    loggerFactory
                    (Some dllPath)
                    peImage
                    dotnetRuntimes
                    (MockEnv.make ())
                    Map.empty
                    None
                    [ testCaseName ]

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
            | RealRuntimeResult.UnhandledException _, RunOutcome.GuestUnhandledException _ ->
                if not (expectsUnhandledException.Contains testCaseName) then
                    failwith
                        $"Both runtimes threw unhandled exceptions for %s{testCaseName}, but this test was not expected to throw. Add to expectsUnhandledException if intentional."
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
            | _, RunOutcome.FailFast _ ->
                failwith
                    "PawPrint called Environment.FailFast; the real runtime can't have done so or the test harness would be gone"
            | _, RunOutcome.ProcessExit _ ->
                failwith
                    "PawPrint called Environment.Exit; the real runtime can't have done so or the test harness would be gone"
            | _, RunOutcome.SignalTerminated _ ->
                failwith
                    "PawPrint terminated due to a signal; the real runtime can't have done so or the test harness would be gone"
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
