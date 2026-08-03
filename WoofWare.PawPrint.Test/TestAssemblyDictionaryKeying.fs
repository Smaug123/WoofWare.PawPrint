namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The loaded-assemblies dictionary is populated by two different keying conventions.
///
///   * `IlMachineState.WithLoadedAssembly` (and `IlMachineState.initial`) key by the
///     assembly's own *AssemblyDefinition* identity.
///   * `TypeResolution.loadAssembly` keys by the *AssemblyReference* that triggered the
///     load: `assemblies.SetItem (assemblyRef.Name.FullName, assy)`.
///
/// Every consumer, however, looks an assembly up by its definition identity:
/// `Concretization.ensureTypeDefinitionBaseAssembliesLoaded` indexes
/// `assemblies.[assemblyName.FullName]`, where `assemblyName` is a `DumpedAssembly.Name`
/// or a `ConcreteType.Assembly`, both of which come from
/// `ResolvedTypeIdentity.ofTypeDefinition` and are therefore definition identities.
///
/// A reference identity need not equal the referent's definition identity. In the .NET 10
/// shared framework the .NET Framework compatibility facades (mscorlib, System,
/// System.Core, System.Xml, ...) reference their implementation assemblies with
/// `Version=0.0.0.0`; six assemblies there — including
/// `System.IO.FileSystem.AccessControl` — are referenced *only* that way, so they can
/// never be found again once loaded.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestAssemblyDictionaryKeying =

    let private corelibPath : string = typeof<obj>.Assembly.Location
    let private runtimeDir : string = Path.GetDirectoryName corelibPath

    let private readAssembly (path : string) : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory path

    let private requireFile (name : string) : string =
        let candidate = Path.Combine (runtimeDir, name)

        if not (File.Exists candidate) then
            Assert.Ignore $"%s{name} not found next to corelib at %s{candidate}"

        candidate

    /// Byte-for-byte the keying production uses: `TypeResolution.loadAssembly` inserts the
    /// freshly-read assembly under the FullName taken from the AssemblyReference that
    /// caused the load, *not* under the assembly's own definition name.
    type private ProductionKeyedAssemblyLoad (searchDirs : string list) =
        interface IAssemblyLoad with
            member _.LoadAssembly
                (loadedAssemblies : ImmutableDictionary<string, DumpedAssembly>)
                (referencedIn : AssemblyName)
                (handle : AssemblyReferenceHandle)
                : ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly
                =
                let referencedInAssembly =
                    match loadedAssemblies.TryGetValue referencedIn.FullName with
                    | false, _ -> failwithf $"Missing loaded assembly %s{referencedIn.FullName}"
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
                            failwithf $"Test setup could not locate assembly %s{assemblyRef.Name.FullName} on disk"
                        )

                    let dumped = readAssembly path
                    // The production keying: the *reference's* FullName, not `dumped.Name.FullName`.
                    loadedAssemblies.SetItem (assemblyRef.Name.FullName, dumped), dumped

    /// Resolve a top-level type by name out of `fromAssembly`, loading referenced
    /// assemblies on demand exactly as production does.
    let private resolveWithLoads
        (loadAssembly : IAssemblyLoad)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (fromAssembly : DumpedAssembly)
        (ns : string)
        (name : string)
        : ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly * ResolvedTypeIdentity
        =
        let rec go
            (assemblies : ImmutableDictionary<string, DumpedAssembly>)
            : ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly * ResolvedTypeIdentity
            =
            match Assembly.resolveTopLevelTypeFromName fromAssembly assemblies (Some ns) name ImmutableArray.Empty with
            | TypeResolutionResult.Resolved (assy, identity, _) -> assemblies, assy, identity
            | TypeResolutionResult.FirstLoadAssy assyRef ->
                let handle, referencedIn = assyRef.Handle
                let assemblies, _ = loadAssembly.LoadAssembly assemblies referencedIn handle
                go assemblies

        go assemblies

    /// Set up the reported scenario: `mscorlib` forwards
    /// `System.Security.AccessControl.FileSystemRights` to
    /// `System.IO.FileSystem.AccessControl` through an AssemblyReference whose Version is
    /// 0.0.0.0, while the real DLL's AssemblyDefinition Version is the shared framework's.
    /// No other assembly in the shared framework references it at all, so this is the only
    /// way the interpreter can ever come to load it.
    let private loadForwardedAccessControlType
        ()
        : IAssemblyLoad *
          ImmutableDictionary<string, DumpedAssembly> *
          DumpedAssembly *
          DumpedAssembly *
          ResolvedTypeIdentity
        =
        let mscorlibPath = requireFile "mscorlib.dll"
        requireFile "System.IO.FileSystem.AccessControl.dll" |> ignore

        let corelib = readAssembly corelibPath
        let mscorlib = readAssembly mscorlibPath

        let loaded : ImmutableDictionary<string, DumpedAssembly> =
            [ corelib ; mscorlib ]
            |> Seq.map (fun a -> System.Collections.Generic.KeyValuePair (a.Name.FullName, a))
            |> ImmutableDictionary.CreateRange

        let loadAssembly = ProductionKeyedAssemblyLoad [ runtimeDir ] :> IAssemblyLoad

        let loaded, targetAssembly, identity =
            resolveWithLoads loadAssembly loaded mscorlib "System.Security.AccessControl" "FileSystemRights"

        // Sanity: we really did land in the forwarded-to assembly, via a reference whose
        // identity differs from that assembly's own definition identity.
        targetAssembly.Name.Name |> shouldEqual "System.IO.FileSystem.AccessControl"

        let referenceKey =
            loaded.Keys
            |> Seq.filter (fun k -> k.StartsWith ("System.IO.FileSystem.AccessControl,", StringComparison.Ordinal))
            |> Seq.exactlyOne

        referenceKey |> shouldNotEqual targetAssembly.Name.FullName
        identity.AssemblyFullName |> shouldEqual targetAssembly.Name.FullName

        loadAssembly, loaded, corelib, targetAssembly, identity

    /// An assembly that has been loaded must be findable by its definition identity,
    /// which is the only identity `ConcreteType.Assembly`, `DumpedAssembly.Name` and
    /// `ResolvedTypeIdentity.AssemblyFullName` ever carry.
    [<Test>]
    let ``a loaded assembly is keyed by its definition identity`` () : unit =
        let _, loaded, _, targetAssembly, _ = loadForwardedAccessControlType ()

        loaded.ContainsKey targetAssembly.Name.FullName |> shouldEqual true

    [<Test>]
    let ``ensureTypeDefinitionBaseAssembliesLoaded succeeds for a type reached through a forwarding facade`` () : unit =
        let loadAssembly, loaded, _, targetAssembly, identity =
            loadForwardedAccessControlType ()

        Concretization.ensureTypeDefinitionBaseAssembliesLoaded
            loadAssembly
            loaded
            targetAssembly.Name
            identity.TypeDefinition.Get
        |> ignore

    [<Test>]
    let ``concretizeTypeDefinition succeeds for a type reached through a forwarding facade`` () : unit =
        let _, loaded, corelib, _, identity = loadForwardedAccessControlType ()

        let baseTypes = Corelib.getBaseTypes corelib

        let ctx : TypeConcretization.ConcretizationContext<DumpedAssembly> =
            {
                ConcreteTypes = Corelib.concretizeAll loaded baseTypes AllConcreteTypes.Empty
                LoadedAssemblies = loaded
                BaseTypes = baseTypes
            }

        TypeConcretization.concretizeTypeDefinition ctx identity |> ignore
