namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open FsUnitTyped
open Microsoft.CodeAnalysis
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

type CrossAssemblySpec =
    {
        Name : string
        OutputKind : OutputKind
        References : string list
        Sources : string list
    }

[<RequireQualifiedAccess>]
module CrossAssemblySpec =
    let library (name : string) (references : string list) (sources : string list) : CrossAssemblySpec =
        {
            Name = name
            OutputKind = OutputKind.DynamicallyLinkedLibrary
            References = references
            Sources = sources
        }

    let entryPoint (name : string) (references : string list) (sources : string list) : CrossAssemblySpec =
        {
            Name = name
            OutputKind = OutputKind.ConsoleApplication
            References = references
            Sources = sources
        }

type CrossAssemblyEndToEndTestCase =
    {
        Assemblies : CrossAssemblySpec list
        EntryAssemblyName : string
        ExpectedReturnCode : int
    }

/// A requirement that `ForeignAssembly` occupies *different* `AssemblyRef` rows in
/// `DeclaringAssembly` than in `ExecutingAssembly`.
///
/// Tests for assembly-scope bugs need this. A `TypeRefResolutionScope.Assembly` is a row index into
/// one specific assembly's `AssemblyRef` table, so code that resolves it against the wrong assembly
/// only *observably* misbehaves when the two tables disagree about that row — and in practice they
/// frequently agree by coincidence, which silently reduces such a test to a tautology.
///
/// The only lever a test has over row numbering is the order in which the compiler first uses each
/// reference, which is a compiler implementation detail. Rather than assume it holds, state the
/// requirement and let the harness check it: if a future Roslyn reorders the table, the test fails
/// loudly instead of quietly ceasing to cover anything.
type AssemblyRefRowDivergence =
    {
        /// The assembly whose metadata scopes the signature under test.
        DeclaringAssembly : string
        /// The assembly whose IL contains the token under test.
        ExecutingAssembly : string
        /// The assembly that must land on different rows in the two above.
        ForeignAssembly : string
    }

[<RequireQualifiedAccess>]
module CrossAssemblyHarness =

    let private getExitCode (terminalState : IlMachineState) (terminatingThread : ThreadId) : int =
        match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
        | [] -> failwith "expected program to return an int, but it returned void"
        | head :: _ ->
            match head with
            | EvalStackValue.Int32 (Int32Source.Verbatim i) -> i
            | ret -> failwith $"expected program to return an int, but it returned %O{ret}"

    let compileAssemblies (assemblies : CrossAssemblySpec list) : Map<string, byte[]> =
        (Map.empty, assemblies)
        ||> List.fold (fun built spec ->
            let references =
                spec.References
                |> List.map (fun referenceName ->
                    match Map.tryFind referenceName built with
                    | Some bytes -> MetadataReference.CreateFromImage bytes :> MetadataReference
                    | None ->
                        failwithf
                            "Assembly %s references %s before it has been compiled; order assemblies topologically"
                            spec.Name
                            referenceName
                )

            let bytes = Roslyn.compileAssembly spec.Name spec.OutputKind references spec.Sources
            built |> Map.add spec.Name bytes
        )

    let private writeAssemblies (tempDir : string) (assemblies : Map<string, byte[]>) : unit =
        assemblies
        |> Map.iter (fun assemblyName bytes ->
            File.WriteAllBytes (Path.Combine (tempDir, assemblyName + ".dll"), bytes)
        )

    let private executeWithPawPrint (entryPath : string) (entryBytes : byte[]) : int =
        let assy = typeof<RunResult>.Assembly

        let messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "entry_assembly", entryPath ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimeDirs =
            seq {
                yield Path.GetDirectoryName entryPath
                yield! DotnetRuntime.SelectForDll assy.Location
            }
            |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (entryBytes)

        try
            let terminalState, terminatingThread =
                match Program.run loggerFactory (Some entryPath) peImage (HostConfig.Default dotnetRuntimeDirs) with
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith $"Guest threw unhandled exception: %O{exn.ExceptionObject}"
                | RunOutcome.FailFast (_, _, message) ->
                    let m = message |> Option.defaultValue "<no message>"
                    failwith $"Guest called Environment.FailFast: %s{m}"
                | RunOutcome.SignalTerminated (_, signal) -> failwith $"Guest was terminated by POSIX signal %O{signal}"
                | RunOutcome.NormalExit (state, thread) -> state, thread
                | RunOutcome.ProcessExit (state, thread) -> state, thread

            getExitCode terminalState terminatingThread
        with _ ->
            for message in messages () do
                Console.Error.WriteLine $"{message}"

            reraise ()

    /// Run the entry assembly on the real runtime, as its own process.
    ///
    /// The guest is a child process for the same reasons `RealRuntime` gives: in-process it shares
    /// every process-global with the test runner, so `Environment.Exit` or `FailFast` kills the run
    /// outright, and an escaped exception arrives as a `TargetInvocationException` whose message
    /// identifies nothing. Its sibling assemblies are already laid out in `entryPath`'s directory,
    /// which is exactly what the host needs to bind them.
    let private executeWithRealRuntime (entryPath : string) : int =
        match RealRuntime.executeAssemblyInPlace [||] entryPath with
        | RealRuntimeResult.NormalExit exitCode -> exitCode
        | RealRuntimeResult.UnhandledException report ->
            failwith $"Real runtime terminated with an unhandled exception:\n%s{report}"
        | RealRuntimeResult.FailFast report -> failwith $"Real runtime called Environment.FailFast:\n%s{report}"

    /// The `AssemblyRef` table's names in row order, and row order is precisely what
    /// a `TypeRefResolutionScope.Assembly` handle indexes into.
    let private assemblyRefNames (image : byte[]) : string list =
        use stream = new MemoryStream (image)
        use peReader = new PEReader (stream)
        let metadata = peReader.GetMetadataReader ()

        // Enumeration of `MetadataReader.AssemblyReferences` walks rows 1..N in order.
        metadata.AssemblyReferences
        |> Seq.map (fun handle -> metadata.GetString (metadata.GetAssemblyReference handle).Name)
        |> List.ofSeq

    let private imageOf (compiled : Map<string, byte[]>) (which : string) (assemblyName : string) : byte[] =
        compiled
        |> Map.tryFind assemblyName
        |> Option.defaultWith (fun () ->
            failwithf
                "AssemblyRef divergence check names %s as its %s, but it was not among the compiled assemblies: %s"
                assemblyName
                which
                (compiled |> Map.toSeq |> Seq.map fst |> String.concat ", ")
        )

    let private checkAssemblyRefDivergence
        (compiled : Map<string, byte[]>)
        (divergence : AssemblyRefRowDivergence)
        : unit
        =
        let declaringRefs =
            assemblyRefNames (imageOf compiled "DeclaringAssembly" divergence.DeclaringAssembly)

        let executingRefs =
            assemblyRefNames (imageOf compiled "ExecutingAssembly" divergence.ExecutingAssembly)

        let rowIn (which : string) (owner : string) (refs : string list) : int =
            match refs |> List.tryFindIndex (fun name -> name = divergence.ForeignAssembly) with
            | Some index -> index + 1
            | None ->
                failwithf
                    "AssemblyRef divergence check expected %s (the %s) to reference %s, but its AssemblyRef table is: %s"
                    owner
                    which
                    divergence.ForeignAssembly
                    (String.concat ", " refs)

        let declaringRow =
            rowIn "declaring assembly" divergence.DeclaringAssembly declaringRefs

        let executingRow =
            rowIn "executing assembly" divergence.ExecutingAssembly executingRefs

        if declaringRow = executingRow then
            failwithf
                "AssemblyRef rows for %s coincide (row %d in both %s and %s), so this test cannot distinguish a correctly-scoped type reference from one resolved against the wrong assembly. Force the rows apart (e.g. by having one assembly use a decoy reference first). Tables were:\n  %s: %s\n  %s: %s"
                divergence.ForeignAssembly
                declaringRow
                divergence.DeclaringAssembly
                divergence.ExecutingAssembly
                divergence.DeclaringAssembly
                (String.concat ", " declaringRefs)
                divergence.ExecutingAssembly
                (String.concat ", " executingRefs)

    let private runTestWith
        (divergences : AssemblyRefRowDivergence list)
        (case : CrossAssemblyEndToEndTestCase)
        : unit
        =
        // The oracle is a real process, and a process exit code carries 8 bits on Unix, whereas
        // PawPrint's is read as a full int32 off the evaluation stack. Reject an expectation the
        // oracle could never report before doing any work, so that a test author meets the limit
        // here rather than as a mystifying "expected 256, got 0" from the comparison below.
        if case.ExpectedReturnCode < 0 || case.ExpectedReturnCode > 255 then
            failwithf
                "Expected return code %d cannot be carried by a process exit code (0-255), so the real runtime cannot act as the oracle for it. Have the guest return a small discriminating value instead."
                case.ExpectedReturnCode

        let compiled = compileAssemblies case.Assemblies

        for divergence in divergences do
            checkAssemblyRefDivergence compiled divergence


        let entryBytes =
            compiled
            |> Map.tryFind case.EntryAssemblyName
            |> Option.defaultWith (fun () ->
                failwithf "Entry assembly %s was not among the compiled assemblies" case.EntryAssemblyName
            )

        let tempDir = Path.Combine (Path.GetTempPath (), Path.GetRandomFileName ())
        Directory.CreateDirectory tempDir |> ignore

        try
            writeAssemblies tempDir compiled

            let entryPath = Path.Combine (tempDir, case.EntryAssemblyName + ".dll")

            let realResult = executeWithRealRuntime entryPath
            realResult |> shouldEqual case.ExpectedReturnCode

            let pawPrintResult = executeWithPawPrint entryPath entryBytes
            pawPrintResult |> shouldEqual realResult
        finally
            try
                if Directory.Exists tempDir then
                    Directory.Delete (tempDir, true)
            with
            | :? IOException
            | :? UnauthorizedAccessException -> ()

    let runTest (case : CrossAssemblyEndToEndTestCase) : unit = runTestWith [] case

    /// As `runTest`, but first assert that each stated `AssemblyRefRowDivergence` actually holds of
    /// the compiled images. Use this for any test whose point is that a type reference is resolved
    /// against the right assembly; see `AssemblyRefRowDivergence` for why assuming it is not enough.
    let runTestRequiring (divergences : AssemblyRefRowDivergence list) (case : CrossAssemblyEndToEndTestCase) : unit =
        if List.isEmpty divergences then
            failwith
                "runTestRequiring was given no divergence requirements; use runTest if the test does not depend on AssemblyRef row layout."

        runTestWith divergences case
