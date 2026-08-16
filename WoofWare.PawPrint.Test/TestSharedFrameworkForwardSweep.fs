namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Sweeps every type-forward declared by the .NET Framework compatibility facades in the real
/// pinned shared framework and asserts that resolving it lands somewhere findable by its
/// definition identity.
///
/// Those facades are where reference and definition identities actually disagree: they name their
/// implementation assemblies as `Version=0.0.0.0` (e.g. `mscorlib` forwards
/// `System.Security.AccessControl.FileSystemRights` to `System.IO.FileSystem.AccessControl`).
/// If the load context is ever keyed by reference identity, this fails in bulk rather than
/// waiting for a user report.
[<TestFixture>]
module TestSharedFrameworkForwardSweep =

    let private corelibPath : string = typeof<obj>.Assembly.Location
    let private runtimeDir : string = Path.GetDirectoryName corelibPath

    let private readAssembly (path : string) : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory path

    let private loader () : IAssemblyLoad =
        let _, loggerFactory = LoggerFactory.makeTest ()
        TypeResolution.directoryLoader loggerFactory [ runtimeDir ]

    /// The .NET Framework compatibility facades. These are the assemblies whose AssemblyReferences
    /// carry `Version=0.0.0.0`, and they are almost entirely type-forwards.
    let private facadeNames =
        [
            "mscorlib"
            "System"
            "System.Core"
            "System.Xml"
            "System.Xml.Linq"
            "System.Transactions"
            "System.Web"
        ]

    let facades : string list =
        facadeNames
        |> List.filter (fun n -> File.Exists (Path.Combine (runtimeDir, n + ".dll")))

    [<TestCaseSource(nameof facades)>]
    let ``every type forwarded by a facade resolves to a findable assembly`` (facadeName : string) : unit =
        let corelib = readAssembly corelibPath
        let facade = readAssembly (Path.Combine (runtimeDir, facadeName + ".dll"))
        let loadAssembly = loader ()

        let mutable loaded = LoadedAssemblies.ofAssemblies [ corelib ; facade ]
        let mutable resolvedCount = 0
        let failures = ResizeArray<string> ()

        // Only top-level forwards: nested ones are reached through their parent, which this
        // already covers, and `TryGetTopLevelExportedType` is the entry point resolution uses.
        //
        // Some facades forward to assemblies that are not part of the shared framework at all
        // (System.Transactions forwards two types to System.Security.Permissions, which ships as a
        // separate NuGet package). "The target is not installed" is a different fact from "we
        // loaded it and then could not find it", and only the latter is what this sweep is about.
        // The split is made structurally, on whether the target DLL exists, rather than by matching
        // exception text — a text filter here would hide the very regression we are looking for.
        let allForwards, skippedForMissingTarget =
            facade.ExportedTypes.Values
            |> Seq.choose (fun e ->
                match e.Data with
                | ExportedTypeData.ForwardsTo assyRefHandle ->
                    let target = facade.AssemblyReferences.[assyRefHandle]
                    Some (e, File.Exists (Path.Combine (runtimeDir, target.Name.Name + ".dll")))
                | ExportedTypeData.ParentExportedType _
                | ExportedTypeData.AssemblyFile _ -> None
            )
            |> Seq.toList
            |> List.partition snd
            |> fun (present, absent) -> present |> List.map fst, absent |> List.map fst

        if not skippedForMissingTarget.IsEmpty then
            TestContext.Progress.WriteLine
                $"%s{facadeName}: skipping %d{skippedForMissingTarget.Length} forwards whose target assembly is not present in the shared framework"

        let topLevelForwards = allForwards

        if topLevelForwards.IsEmpty then
            Assert.Ignore $"%s{facadeName} declares no top-level type forwards with a resolvable target"

        for exported in topLevelForwards do
            let ns = exported.Namespace |> Option.defaultValue "<global>"
            let describe = $"%s{ns}.%s{exported.Name} (forwarded by %s{facadeName})"

            try
                // Resolve exactly as production does, loading on demand.
                let rec go (assemblies : LoadedAssemblies) (fuel : int) =
                    if fuel <= 0 then
                        failwith "resolution did not converge"
                    else

                    match Assembly.resolveTypeFromExport facade assemblies ImmutableArray.Empty exported with
                    | TypeResolutionResult.FirstLoadAssy assyRef ->
                        let handle, referencedIn = assyRef.Handle
                        let assemblies, _ = loadAssembly.LoadAssembly assemblies referencedIn handle
                        go (LoadedAssemblies.assertReferenceBound describe assyRef assemblies) (fuel - 1)
                    | TypeResolutionResult.Resolved (targetAssembly, identity, _) ->
                        assemblies, targetAssembly, identity

                let next, targetAssembly, identity = go loaded 16
                loaded <- next
                resolvedCount <- resolvedCount + 1

                // The load context must be able to find the assembly the identity names, as
                // `ensureTypeDefinitionBaseAssembliesLoaded` and `concretizeTypeDefinition` do.
                if not (loaded.ContainsDefinition targetAssembly.Name) then
                    failures.Add
                        $"%s{describe}: resolved into %s{targetAssembly.Name.FullName}, which is not findable by its definition identity"
                elif loaded.TryByDefinitionName identity.AssemblyFullName |> Option.isNone then
                    failures.Add
                        $"%s{describe}: identity names %s{identity.AssemblyFullName}, which is not in the load context"
            with e ->
                // Every exception is a failure. Do not filter on message text:
                // `assertReferenceBound` says "still not bound", not "not loaded", so a text
                // filter hides the regression this sweep exists to catch.
                failures.Add $"%s{describe}: %s{e.GetType().Name}: %s{e.Message}"

        if failures.Count > 0 then
            let sample = failures |> Seq.truncate 10 |> String.concat "\n  "

            Assert.Fail
                $"%d{failures.Count} of %d{topLevelForwards.Length} forwards from %s{facadeName} failed identity lookup:\n  %s{sample}"

        // Every forward must have resolved, not merely "at least one".
        resolvedCount |> shouldEqual topLevelForwards.Length
