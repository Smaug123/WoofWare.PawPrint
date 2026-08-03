namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// Regression coverage for the invariant that concretizeMethod leaves every
/// ConcreteTypeHandle it emits (locals, parameters, return type) in a state
/// where CliType.zeroOf can safely walk its base-type chain — i.e. every
/// assembly reachable from a base-type TypeRef is already loaded.
///
/// The failure mode this guards is DumpedAssembly.getTypeRef crashing with
/// "seems pretty unlikely that we could have constructed this object without
/// loading its base type" when zeroOf → isValueType hits a TypeRef pointing at
/// an assembly (typically a facade like netstandard) that has not been force-
/// loaded yet.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestZeroOfBaseAssemblies =

    /// FSharp.Core built against netstandard2.1 references `netstandard` for
    /// BCL primitives (including System.ValueType, the base of every F#
    /// [<Struct>] type). Concretizing a struct from this DLL does not need
    /// netstandard, but zero-initialising an instance of that struct does.
    let private fsharpCoreNetstandard21Path : string =
        let nugetRoot =
            match Environment.GetEnvironmentVariable "NUGET_PACKAGES" with
            | null
            | "" -> Path.Combine (Environment.GetFolderPath Environment.SpecialFolder.UserProfile, ".nuget", "packages")
            | value -> value

        let fsharpCoreDir = Path.Combine (nugetRoot, "fsharp.core")

        if not (Directory.Exists fsharpCoreDir) then
            Assert.Ignore
                $"FSharp.Core nuget package not found under %s{fsharpCoreDir}; skipping netstandard2.1 base-chain regression."

        let candidate =
            Directory.EnumerateDirectories fsharpCoreDir
            |> Seq.choose (fun versionDir ->
                let dll = Path.Combine (versionDir, "lib", "netstandard2.1", "FSharp.Core.dll")

                if File.Exists dll then Some dll else None
            )
            |> Seq.tryHead

        match candidate with
        | Some dll -> dll
        | None ->
            Assert.Ignore
                "No FSharp.Core/*/lib/netstandard2.1/FSharp.Core.dll found; skipping netstandard2.1 base-chain regression."

            failwith "unreachable"

    let private corelibPath : string = typeof<obj>.Assembly.Location

    /// netstandard.dll and every BCL facade this test may need ship alongside
    /// System.Private.CoreLib in every Microsoft.NETCore.App runtime we test
    /// against; a directory-scoped IAssemblyLoad can resolve them on demand.
    let private runtimeDir : string = Path.GetDirectoryName corelibPath

    let private assertNetstandardAvailable () : unit =
        let candidate = Path.Combine (runtimeDir, "netstandard.dll")

        if not (File.Exists candidate) then
            Assert.Ignore $"netstandard.dll not found next to corelib at %s{candidate}"

    let private readAssembly (path : string) : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory path

    /// An IAssemblyLoad that resolves references by searching a set of
    /// directories on disk for a DLL matching the referenced assembly's
    /// simple name (`Foo` → `Foo.dll`). All BCL/facade assemblies live next
    /// to System.Private.CoreLib, so a single directory lookup suffices for
    /// this test's needs.
    type private OnDemandAssemblyLoad (searchDirs : string list) =
        interface IAssemblyLoad with
            member _.LoadAssembly
                (loadedAssemblies : ImmutableDictionary<string, DumpedAssembly>)
                (referencedIn : AssemblyName)
                (handle : AssemblyReferenceHandle)
                : ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly
                =
                let referencedInAssembly =
                    match loadedAssemblies.TryGetValue referencedIn.FullName with
                    | false, _ -> failwithf "Missing loaded assembly %s" referencedIn.FullName
                    | true, assy -> assy

                let assemblyRef = referencedInAssembly.AssemblyReferences.[handle]

                match loadedAssemblies.TryGetValue assemblyRef.Name.FullName with
                | true, assy -> loadedAssemblies, assy
                | false, _ ->
                    let path =
                        searchDirs
                        |> List.tryPick (fun dir ->
                            let candidate = Path.Combine (dir, assemblyRef.Name.Name + ".dll")
                            if File.Exists candidate then Some candidate else None
                        )
                        |> Option.defaultWith (fun () ->
                            failwithf "Test setup could not locate assembly %s on disk" assemblyRef.Name.FullName
                        )

                    let dumped = readAssembly path
                    loadedAssemblies.SetItem (dumped.Name.FullName, dumped), dumped

    /// Look up FSharpValueOption`1 in the netstandard2.1 build of FSharp.Core.
    let private getValueOptionTypeDef (fsharpCore : DumpedAssembly) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        fsharpCore.TryGetTopLevelTypeDef "Microsoft.FSharp.Core" "FSharpValueOption`1"
        |> Option.defaultWith (fun () ->
            failwith "Expected FSharpValueOption`1 in netstandard2.1 FSharp.Core; nuget layout may have changed"
        )

    [<Test>]
    let ``ensureBaseAssembliesLoadedForConcreteHandle loads facade assembly reachable via base-type TypeRef``
        ()
        : unit
        =
        let corelib = readAssembly corelibPath
        let fsharpCore = readAssembly fsharpCoreNetstandard21Path

        let baseTypes = Corelib.getBaseTypes corelib

        // Prime loadedAssemblies with corelib + FSharp.Core (netstandard2.1),
        // but NOT netstandard itself — that's the facade FSharp.Core's base
        // TypeRefs point at.
        let loaded : ImmutableDictionary<string, DumpedAssembly> =
            [ corelib ; fsharpCore ]
            |> Seq.map (fun a -> System.Collections.Generic.KeyValuePair (a.Name.FullName, a))
            |> ImmutableDictionary.CreateRange

        loaded.ContainsKey "netstandard, Version=2.1.0.0, Culture=neutral, PublicKeyToken=cc7b13ffcd2ddd51"
        |> shouldEqual false

        assertNetstandardAvailable ()
        let loadAssembly = OnDemandAssemblyLoad [ runtimeDir ]

        // Concretize FSharpValueOption<int>. Concretization itself does not walk
        // base types, so it should not trigger a netstandard load.
        let valueOptionTypeDef = getValueOptionTypeDef fsharpCore

        let concretizeCtx : TypeConcretization.ConcretizationContext<DumpedAssembly> =
            {
                ConcreteTypes = Corelib.concretizeAll loaded baseTypes AllConcreteTypes.Empty
                LoadedAssemblies = loaded
                BaseTypes = baseTypes
            }

        // FSharpValueOption`1 is [<Struct>], so its signature-kind is ValueType.
        // We hardcode this rather than calling DumpedAssembly.signatureTypeKind,
        // which itself would trigger the same base-chain walk we're trying to
        // exercise the fix for.
        let valueOptionInt : TypeDefn =
            let identity = valueOptionTypeDef.Identity

            TypeDefn.GenericInstantiation (
                TypeDefn.FromDefinition (identity, SignatureTypeKind.ValueType),
                ImmutableArray.CreateRange [ TypeDefn.PrimitiveType PrimitiveType.Int32 ]
            )

        let handle, concretizeCtx =
            TypeConcretization.concretizeType
                concretizeCtx
                (loadAssembly :> IAssemblyLoad)
                fsharpCore.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                valueOptionInt

        // Sanity: concretization itself did not need netstandard.
        let netstandardLoadedAfterConcretize =
            concretizeCtx.LoadedAssemblies.Keys
            |> Seq.exists (fun (n : string) -> n.StartsWith ("netstandard,", StringComparison.OrdinalIgnoreCase))

        netstandardLoadedAfterConcretize |> shouldEqual false

        // The bug: calling isValueType directly on the metadata now would crash
        // with "seems pretty unlikely...", because FSharpValueOption's base
        // TypeRef points at System.ValueType in netstandard (not yet loaded).
        let isValueTypeBeforeHelper =
            try
                DumpedAssembly.isValueType baseTypes concretizeCtx.LoadedAssemblies valueOptionTypeDef
                |> Ok
            with e ->
                Error e.Message

        match isValueTypeBeforeHelper with
        | Ok _ ->
            Assert.Fail
                "Expected isValueType to fail before running ensureBaseAssembliesLoadedForConcreteHandle; the test scenario no longer exercises the bug."
        | Error msg -> msg |> shouldContainText "seems pretty unlikely"

        // The fix: run the helper, then isValueType must succeed against the
        // returned assemblies dict.
        let visited = System.Collections.Generic.HashSet<ConcreteTypeHandle> ()

        let loadedAfterHelper, _ =
            Concretization.ensureBaseAssembliesLoadedForConcreteHandle
                (loadAssembly :> IAssemblyLoad)
                baseTypes
                visited
                concretizeCtx.LoadedAssemblies
                concretizeCtx.ConcreteTypes
                handle

        // netstandard is now loaded.
        loadedAfterHelper.Keys
        |> Seq.exists (fun (n : string) -> n.StartsWith ("netstandard,", StringComparison.OrdinalIgnoreCase))
        |> shouldEqual true

        // isValueType now succeeds — FSharpValueOption is a struct.
        DumpedAssembly.isValueType baseTypes loadedAfterHelper valueOptionTypeDef
        |> shouldEqual true

    [<Test>]
    let ``helper is idempotent and cycle-safe against repeated calls on the same handle`` () : unit =
        let corelib = readAssembly corelibPath
        let fsharpCore = readAssembly fsharpCoreNetstandard21Path
        let baseTypes = Corelib.getBaseTypes corelib

        let loaded : ImmutableDictionary<string, DumpedAssembly> =
            [ corelib ; fsharpCore ]
            |> Seq.map (fun a -> System.Collections.Generic.KeyValuePair (a.Name.FullName, a))
            |> ImmutableDictionary.CreateRange

        assertNetstandardAvailable ()
        let loadAssembly = OnDemandAssemblyLoad [ runtimeDir ]

        let concretizeCtx : TypeConcretization.ConcretizationContext<DumpedAssembly> =
            {
                ConcreteTypes = Corelib.concretizeAll loaded baseTypes AllConcreteTypes.Empty
                LoadedAssemblies = loaded
                BaseTypes = baseTypes
            }

        let valueOptionTypeDef = getValueOptionTypeDef fsharpCore

        let valueOptionInt : TypeDefn =
            TypeDefn.GenericInstantiation (
                TypeDefn.FromDefinition (valueOptionTypeDef.Identity, SignatureTypeKind.ValueType),
                ImmutableArray.CreateRange [ TypeDefn.PrimitiveType PrimitiveType.Int32 ]
            )

        let handle, concretizeCtx =
            TypeConcretization.concretizeType
                concretizeCtx
                (loadAssembly :> IAssemblyLoad)
                fsharpCore.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                valueOptionInt

        // Two back-to-back calls (fresh visited each time) must both return
        // assembly dicts containing netstandard, and the second must not throw
        // or attempt any redundant work that would break with a "already loaded"
        // condition. Sharing visited across a single call is required only to
        // bound work within one call; between calls, the dict itself is the
        // idempotence guarantee.
        let firstRun, firstCtx =
            Concretization.ensureBaseAssembliesLoadedForConcreteHandle
                (loadAssembly :> IAssemblyLoad)
                baseTypes
                (System.Collections.Generic.HashSet ())
                concretizeCtx.LoadedAssemblies
                concretizeCtx.ConcreteTypes
                handle

        let secondRun, _ =
            Concretization.ensureBaseAssembliesLoadedForConcreteHandle
                (loadAssembly :> IAssemblyLoad)
                baseTypes
                (System.Collections.Generic.HashSet ())
                firstRun
                firstCtx
                handle

        secondRun.Count |> shouldEqual firstRun.Count

    [<Test>]
    let ``helper recurses into instance fields of value types`` () : unit =
        // Nested-struct regression: the outer struct's own base chain resolves
        // against System.Runtime (already loaded via corelib references), so
        // loading the outer's base chain does NOT drag in netstandard. netstandard
        // becomes necessary only via the field walk into FSharpValueOption`1's
        // base chain. Without that recursion, zeroOf would still crash when it
        // descended into the field.
        let corelib = readAssembly corelibPath
        let fsharpCore = readAssembly fsharpCoreNetstandard21Path
        let baseTypes = Corelib.getBaseTypes corelib
        assertNetstandardAvailable ()

        // Build a C# library `struct Outer { FSharpValueOption<int> Inner; }`
        // that references the netstandard2.1 FSharp.Core we already loaded.
        let outerAssemblyBytes : byte[] =
            let fsharpCoreRef =
                MetadataReference.CreateFromFile fsharpCoreNetstandard21Path :> MetadataReference

            let source =
                """
public struct Outer
{
    public Microsoft.FSharp.Core.FSharpValueOption<int> Inner;
}
"""

            Roslyn.compileAssembly
                "NestedStructRegression"
                OutputKind.DynamicallyLinkedLibrary
                [ fsharpCoreRef ]
                [ source ]

        let outerAssembly : DumpedAssembly =
            let _, loggerFactory = LoggerFactory.makeTest ()
            use stream = new MemoryStream (outerAssemblyBytes)
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None stream

        let loadAssembly = OnDemandAssemblyLoad [ runtimeDir ]

        // Prime with corelib + FSharp.Core + the wrapper. Deliberately leave
        // netstandard out; the wrapper's own base chain doesn't need it.
        let loaded : ImmutableDictionary<string, DumpedAssembly> =
            [ corelib ; fsharpCore ; outerAssembly ]
            |> Seq.map (fun a -> System.Collections.Generic.KeyValuePair (a.Name.FullName, a))
            |> ImmutableDictionary.CreateRange

        let outerTypeDef =
            outerAssembly.TryGetTopLevelTypeDef "" "Outer"
            |> Option.defaultWith (fun () -> failwith "Failed to find compiled Outer struct in the test assembly")

        let concretizeCtx : TypeConcretization.ConcretizationContext<DumpedAssembly> =
            {
                ConcreteTypes = Corelib.concretizeAll loaded baseTypes AllConcreteTypes.Empty
                LoadedAssemblies = loaded
                BaseTypes = baseTypes
            }

        let outerHandle, concretizeCtx =
            TypeConcretization.concretizeType
                concretizeCtx
                (loadAssembly :> IAssemblyLoad)
                outerAssembly.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (outerTypeDef.Identity, SignatureTypeKind.ValueType))

        // Sanity: Outer's own base-chain load does not need netstandard.
        // We measure the state before running the helper, by explicitly
        // loading only the outer's base chain and confirming netstandard is
        // still absent.
        let assembliesAfterOuterBaseChain =
            Concretization.ensureTypeDefinitionBaseAssembliesLoaded
                (loadAssembly :> IAssemblyLoad)
                concretizeCtx.LoadedAssemblies
                outerAssembly.Name
                outerTypeDef.TypeDefHandle

        assembliesAfterOuterBaseChain.Keys
        |> Seq.exists (fun (n : string) -> n.StartsWith ("netstandard,", StringComparison.OrdinalIgnoreCase))
        |> shouldEqual false

        // Now run the full helper. It must descend into Outer's instance
        // field (FSharpValueOption<int>) and, through that field's base chain,
        // finally load netstandard.
        let loadedAfterHelper, _ =
            Concretization.ensureBaseAssembliesLoadedForConcreteHandle
                (loadAssembly :> IAssemblyLoad)
                baseTypes
                (System.Collections.Generic.HashSet ())
                assembliesAfterOuterBaseChain
                concretizeCtx.ConcreteTypes
                outerHandle

        loadedAfterHelper.Keys
        |> Seq.exists (fun (n : string) -> n.StartsWith ("netstandard,", StringComparison.OrdinalIgnoreCase))
        |> shouldEqual true

        // And isValueType on the nested field type succeeds too.
        let valueOptionTypeDef = getValueOptionTypeDef fsharpCore

        DumpedAssembly.isValueType baseTypes loadedAfterHelper valueOptionTypeDef
        |> shouldEqual true
