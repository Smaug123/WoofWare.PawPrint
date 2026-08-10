namespace WoofWare.Pawprint.Test

open System
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

    type private TestAssemblyMarker = class end

    let private rid = RuntimeInformation.RuntimeIdentifier

    // We cannot use __SOURCE_DIRECTORY__: when ContinuousIntegrationBuild is set
    // (Directory.Build.props turns it on whenever GITHUB_ACTION is non-empty), the
    // F# compiler remaps source paths to a deterministic `/_/` prefix that has no
    // relationship to the runtime filesystem. Instead, walk up from the test
    // assembly's on-disk location until we find WoofWare.PawPrint.slnx, then
    // resolve the sibling FSharpPureCases project from there.
    let private repoRoot : string =
        let testAssemblyDir =
            Path.GetDirectoryName typeof<TestAssemblyMarker>.Assembly.Location

        let rec walk (dir : string) : string =
            if String.IsNullOrEmpty dir then
                failwith
                    $"Could not locate WoofWare.PawPrint.slnx by walking up from %s{testAssemblyDir}; cannot determine F# test project directory."
            elif File.Exists (Path.Combine (dir, "WoofWare.PawPrint.slnx")) then
                dir
            else
                walk (Path.GetDirectoryName dir)

        walk testAssemblyDir

    let private projectDir =
        Path.Combine (repoRoot, "WoofWare.PawPrint.Test.FSharpPureCases")

    let private projectFile =
        Path.Combine (projectDir, "WoofWare.PawPrint.Test.FSharpPureCases.fsproj")

    // Output goes to a path we control so the test isn't coupled to the project's TargetFramework.
    let private publishDir =
        Path.Combine (projectDir, "bin", "Release", "pawprint-test-publish", rid)

    let private dllPath =
        Path.Combine (publishDir, "WoofWare.PawPrint.Test.FSharpPureCases.dll")

    // The real-runtime oracle runs the app out of process, so it needs the apphost rather than the
    // managed image: this is a self-contained publish, whose dependencies (FSharp.Core among them)
    // live beside the dll and are only resolvable from that directory.
    let private exePath =
        Path.Combine (
            publishDir,
            if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
                "WoofWare.PawPrint.Test.FSharpPureCases.exe"
            else
                "WoofWare.PawPrint.Test.FSharpPureCases"
        )

    let private publishOnce : Lazy<unit> =
        lazy
            if not (File.Exists projectFile) then
                failwith $"Cannot publish F# test cases: project file %s{projectFile} does not exist."

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

    let testCases : string list =
        [
            "Placeholder"
            "CeqBranch"
            "TailCall"
            "AbstractDispatch"
            "ByrefDispatch"
            "SprintfBasic"
            "UnionReflection"
        ]

    /// F# cases not expected to pass under PawPrint.
    ///
    /// A case named here is skipped as Inconclusive by `F# pure tests`, and only its *real-runtime*
    /// behaviour is checked. Nothing therefore detects a parked case that has started passing, so
    /// before recording that a case is blocked on a named primitive, un-park it and observe the
    /// failure: parking it is what stops the claim being checked.
    ///
    /// `UnionReflection` is parked on `MetadataImport::GetDefaultValue`: having learned a literal
    /// field's *type*, `MdFieldInfo.GetValue` goes on to read its constant out of the Constant
    /// table through `MdConstant.GetValue`, and that reaches an InternalCall with no handler
    /// ("Unimplemented native method (InternalCall): ... MetadataImport::GetDefaultValue").
    /// Observed by un-parking it and running: the real runtime exits 0, PawPrint throws out of
    /// `NativeDispatch.failUnimplemented`.
    ///
    /// Four earlier blockers are already gone: decoding each case's
    /// `CompilationMappingAttribute(SourceConstructFlags, ...)`, whose argument is an enum;
    /// enumerating the union's nested case types; `MetadataImport::GetSigOfFieldDef`; and the
    /// raw-blob path of `Signature_Init`, which the commit this comment sits in implements.
    let unimplemented : Set<string> = Set.ofList [ "UnionReflection" ]

    // F# test cases that legitimately throw under both runtimes. Without this set, a test
    // that crashes both runtimes would silently pass — see TestPureCases.fs for the same
    // mechanism. Add a case here only when the throw is the intended observable.
    let expectsUnhandledException : Set<string> = Set.empty

    // F# test cases whose successful exit code is not 0 — same mechanism as
    // `customExitCodes` in TestPureCases.fs. `AbstractDispatch` returns the sum computed by
    // its `Combine` call (see issue #693: 40 + 2 = 42) rather than a boolean success/failure
    // code, so a wrong dispatch is directly observable as a wrong number rather than being
    // laundered through an if/else into 0-or-1.
    let customExitCodes : Map<string, int> =
        [ "AbstractDispatch", 42 ; "ByrefDispatch", 42 ] |> Map.ofList

    let private runTest (testCaseName : string) : unit =
        let image = loadImage ()
        let messages, loggerFactory = LoggerFactory.makeTest ()

        let expectedExitCode =
            customExitCodes |> Map.tryFind testCaseName |> Option.defaultValue 0

        let dotnetRuntimes =
            seq {
                yield publishDir
                yield! DotnetRuntime.SelectForDll (typeof<RunResult>.Assembly.Location)
            }
            |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        try
            let realResult = RealRuntime.executePublishedApp [| testCaseName |] exePath

            let pawPrintResult =
                Program.run
                    loggerFactory
                    (Some dllPath)
                    peImage
                    { HostConfig.Default dotnetRuntimes with
                        Argv = [ testCaseName ]
                    }

            match realResult, pawPrintResult with
            | RealRuntimeResult.NormalExit exitCode, RunOutcome.NormalExit (terminalState, terminatingThread) ->
                exitCode |> shouldEqual expectedExitCode

                let pawPrintExitCode =
                    match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                    | [] -> failwith "expected program to return a value, but it returned void"
                    | head :: _ ->
                        match head with
                        | EvalStackValue.Int32 (Int32Source.Verbatim i) -> i
                        | ret -> failwith $"expected program to return an int, but it returned %O{ret}"

                pawPrintExitCode |> shouldEqual exitCode
            | RealRuntimeResult.UnhandledException _, RunOutcome.GuestUnhandledException _ ->
                if not (expectsUnhandledException.Contains testCaseName) then
                    failwith
                        $"Both runtimes threw unhandled exceptions for %s{testCaseName}, but this test was not expected to throw. Add to expectsUnhandledException if intentional."
            | RealRuntimeResult.NormalExit exitCode, RunOutcome.GuestUnhandledException (_, _, exn) ->
                failwith
                    $"Real runtime exited normally with code %d{exitCode}, but PawPrint threw unhandled exception: %O{exn.ExceptionObject}"
            | RealRuntimeResult.FailFast report, _ ->
                failwith
                    $"Real runtime called Environment.FailFast for %s{testCaseName}; this fixture does not exercise FailFast:\n%s{report}"
            | RealRuntimeResult.UnhandledException realExn, RunOutcome.NormalExit (terminalState, terminatingThread) ->
                let pawPrintExitCode =
                    match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                    | [] -> None
                    | EvalStackValue.Int32 (Int32Source.Verbatim i) :: _ -> Some i
                    | _ -> None

                failwith
                    $"Real runtime terminated with an unhandled exception, but PawPrint exited normally (code: %O{pawPrintExitCode}):\n%s{realExn}"
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

    /// The `TailCall` case only covers the `tail.` prefix if FSC actually emits it, which
    /// depends on the optimiser (Release + `--tailcalls+`) and on the exact shapes in
    /// TailCall.fs. Without this guard, a compiler change that stopped emitting `tail.`
    /// would leave `F# pure tests(TailCall)` silently passing while covering nothing.
    /// If this fails, re-inspect the IL (`dotnet run --project WoofWare.PawPrint.IlDump --
    /// <published dll> TailCall`) and reshape TailCall.fs until the prefix comes back.
    [<Test>]
    let ``TailCall case really does contain tail. prefixes`` () : unit =
        let image = loadImage ()
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)

        let assy = Assembly.read loggerFactory (Some dllPath) peImage

        let tailPrefixed =
            assy.TypeDefs.Values
            |> Seq.collect (fun typeInfo -> typeInfo.Methods)
            |> Seq.filter (fun methodInfo ->
                methodInfo.DeclaringType.Name = "TailCall"
                && match MethodInfo.tryIlBody methodInfo with
                   | None -> false
                   | Some instructions ->
                       instructions.Instructions
                       |> List.exists (fun (op, _offset) ->
                           match op with
                           | IlOp.Nullary NullaryIlOp.Tail -> true
                           | _ -> false
                       )
            )
            |> Seq.map (fun methodInfo -> methodInfo.Name)
            |> Set.ofSeq

        // `isEven`/`isOdd` are `tail. call`; `applyTail` is `tail. callvirt`. All three are
        // reached from `TailCall.main`, so the end-to-end case really executes the prefix.
        tailPrefixed |> shouldEqual (Set.ofList [ "isEven" ; "isOdd" ; "applyTail" ])

    /// The `AbstractDispatch` case only exercises issue #693 if FSC actually emits the
    /// abstract `Combine` declaration with zero Param-table rows despite its signature
    /// declaring one parameter. Without this guard, a compiler change that started emitting
    /// a Param row for the abstract declaration would leave `F# pure tests(AbstractDispatch)`
    /// silently passing while covering nothing — `Parameters.Length` would then agree with
    /// the true arity, and the bug this test exists to catch could regress unnoticed.
    /// If this fails, re-inspect the metadata (`dotnet run --project WoofWare.PawPrint.IlDump
    /// -- <published dll> Base Combine`) and reshape AbstractDispatch.fs until the shape
    /// comes back.
    [<Test>]
    let ``AbstractDispatch's abstract Combine really does have zero Param rows`` () : unit =
        let image = loadImage ()
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)

        let assy = Assembly.read loggerFactory (Some dllPath) peImage

        let combine =
            assy.TypeDefs.Values
            |> Seq.collect (fun typeInfo -> typeInfo.Methods)
            |> Seq.filter (fun methodInfo -> methodInfo.DeclaringType.Name = "Base" && methodInfo.Name = "Combine")
            |> Seq.toList
            |> function
                | [ m ] -> m
                | [] -> failwith "AbstractDispatch.Base::Combine not found in the published assembly"
                | ms -> failwith $"expected exactly one AbstractDispatch.Base::Combine, found %d{List.length ms}"

        (match combine.Body with
         | MethodBody.Abstract -> ()
         | other -> failwith $"expected AbstractDispatch.Base::Combine to be MethodBody.Abstract, got %O{other}")

        // Deliberately the Param *table*, not the arity: the whole point of this test is that the
        // two disagree for an F#-emitted abstract member.
        (MethodInfo.requireMetadata "test" combine).Parameters.IsEmpty
        |> shouldEqual true

        MethodInfo.arity combine |> shouldEqual 1

    /// Regression guard for issue #692, mirroring the `AbstractDispatch` guard above. #692's
    /// real-world trigger (FSharp.Core's `MapEnumerator`1::DoMoveNext(byref<T>)`) is an
    /// abstract method with a *byref* parameter; this asserts `ByrefDispatch.fs` reproduces
    /// that same zero-Param-rows-but-nonzero-arity shape, and specifically that the one
    /// parameter is a byref. Without this guard, a compiler change that started emitting a
    /// Param row for the abstract declaration (or stopped modelling the parameter as a byref)
    /// would leave `F# pure tests(ByrefDispatch)` silently passing while covering nothing.
    [<Test>]
    let ``ByrefDispatch's abstract Bump really does have zero Param rows and a byref parameter`` () : unit =
        let image = loadImage ()
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)

        let assy = Assembly.read loggerFactory (Some dllPath) peImage

        let bump =
            assy.TypeDefs.Values
            |> Seq.collect (fun typeInfo -> typeInfo.Methods)
            |> Seq.filter (fun methodInfo -> methodInfo.DeclaringType.Name = "Base" && methodInfo.Name = "Bump")
            |> Seq.toList
            |> function
                | [ m ] -> m
                | [] -> failwith "ByrefDispatch.Base::Bump not found in the published assembly"
                | ms -> failwith $"expected exactly one ByrefDispatch.Base::Bump, found %d{List.length ms}"

        (match bump.Body with
         | MethodBody.Abstract -> ()
         | other -> failwith $"expected ByrefDispatch.Base::Bump to be MethodBody.Abstract, got %O{other}")

        // As above: the Param table, not the arity.
        (MethodInfo.requireMetadata "test" bump).Parameters.IsEmpty |> shouldEqual true
        MethodInfo.arity bump |> shouldEqual 1

        (match bump.Signature.ParameterTypes.[0] with
         | TypeDefn.Byref _ -> ()
         | other -> failwith $"expected ByrefDispatch.Base::Bump's sole parameter to be a byref, got %O{other}")

    [<TestCaseSource(nameof unimplemented)>]
    let ``Unimplemented F# tests have correct real-runtime behaviour`` (testCaseName : string) =
        // This case never runs PawPrint, so it needs the publish rather than the image.
        publishOnce.Force ()

        let expectedExitCode =
            customExitCodes |> Map.tryFind testCaseName |> Option.defaultValue 0

        match RealRuntime.executePublishedApp [| testCaseName |] exePath with
        | RealRuntimeResult.NormalExit exitCode -> exitCode |> shouldEqual expectedExitCode
        | RealRuntimeResult.UnhandledException report ->
            failwith $"Real runtime terminated with an unhandled exception for %s{testCaseName}:\n%s{report}"
        | RealRuntimeResult.FailFast report ->
            failwith $"Real runtime called Environment.FailFast for %s{testCaseName}:\n%s{report}"
