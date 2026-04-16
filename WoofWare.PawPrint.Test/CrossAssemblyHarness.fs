namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.Loader
open FsUnitTyped
open Microsoft.CodeAnalysis
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PawPrint.ExternImplementations

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
        NativeImpls : NativeImpls
    }

[<RequireQualifiedAccess>]
module CrossAssemblyHarness =

    let private getExitCode (terminalState : IlMachineState) (terminatingThread : ThreadId) : int =
        match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
        | [] -> failwith "expected program to return an int, but it returned void"
        | head :: _ ->
            match head with
            | EvalStackValue.Int32 i -> i
            | ret -> failwith $"expected program to return an int, but it returned %O{ret}"

    let private compileAssemblies (assemblies : CrossAssemblySpec list) : Map<string, byte[]> =
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

    let private executeWithPawPrint (entryPath : string) (entryBytes : byte[]) (nativeImpls : NativeImpls) : int =
        let assy = typeof<RunResult>.Assembly
        let messages, loggerFactory = LoggerFactory.makeTest ()

        let dotnetRuntimeDirs =
            seq {
                yield Path.GetDirectoryName entryPath
                yield! DotnetRuntime.SelectForDll assy.Location
            }
            |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (entryBytes)

        try
            let terminalState, terminatingThread =
                Program.run loggerFactory (Some entryPath) peImage dotnetRuntimeDirs nativeImpls []

            getExitCode terminalState terminatingThread
        with _ ->
            for message in messages () do
                Console.Error.WriteLine $"{message}"

            reraise ()

    let private executeWithRealRuntime (entryPath : string) : int =
        let tempDir = Path.GetDirectoryName entryPath

        let loadContext =
            new AssemblyLoadContext ("CrossAssemblyEndToEnd", isCollectible = true)

        loadContext.add_Resolving (fun context assemblyName ->
            let candidate = Path.Combine (tempDir, assemblyName.Name + ".dll")

            if File.Exists candidate then
                context.LoadFromAssemblyPath candidate
            else
                null
        )

        try
            let entry : Reflection.Assembly = loadContext.LoadFromAssemblyPath entryPath
            let entryPoint : Reflection.MethodInfo = entry.EntryPoint
            let mainArgs : string[] = [||]
            let invokeArgs : obj[] = [| mainArgs :> obj |]
            let result : obj = entryPoint.Invoke ((null : obj), invokeArgs)
            unbox<int> result
        finally
            loadContext.Unload ()

    let runTest (case : CrossAssemblyEndToEndTestCase) : unit =
        let compiled = compileAssemblies case.Assemblies

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

            let pawPrintResult = executeWithPawPrint entryPath entryBytes case.NativeImpls
            pawPrintResult |> shouldEqual realResult
        finally
            try
                if Directory.Exists tempDir then
                    Directory.Delete (tempDir, true)
            with
            | :? IOException
            | :? UnauthorizedAccessException -> ()
