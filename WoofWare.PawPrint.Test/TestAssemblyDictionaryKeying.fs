namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// An assembly has two identities and they are not interchangeable: the *definition* identity it
/// declares for itself, and the *reference* identity by which some other assembly names it.
/// Everything downstream of type resolution — `DumpedAssembly.Name`, `ConcreteType.Assembly`,
/// `ResolvedTypeIdentity.AssemblyFullName` — carries a definition identity, so an assembly must be
/// findable by that identity no matter which reference caused it to be loaded.
///
/// This used to fail: the load context was keyed by whichever AssemblyReference triggered the
/// load. In the .NET shared framework the .NET Framework compatibility facades reference their
/// implementation assemblies with `Version=0.0.0.0`, and six assemblies there — including
/// `System.IO.FileSystem.AccessControl` — are referenced *only* that way, so they could never be
/// found again once loaded.
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

    /// The production loader. Tests must not hand-roll an `IAssemblyLoad`: this bug was invisible
    /// to the suite for exactly as long as the test fakes keyed their dictionaries differently
    /// from production.
    let private loader () : IAssemblyLoad =
        let _, loggerFactory = LoggerFactory.makeTest ()
        TypeResolution.directoryLoader loggerFactory [ runtimeDir ]

    /// Resolve a top-level type by name out of `fromAssembly`, loading referenced assemblies on
    /// demand through the production loader.
    let private resolveWithLoads
        (loadAssembly : IAssemblyLoad)
        (assemblies : LoadedAssemblies)
        (fromAssembly : DumpedAssembly)
        (ns : string)
        (name : string)
        : LoadedAssemblies * DumpedAssembly * ResolvedTypeIdentity
        =
        let rec go (assemblies : LoadedAssemblies) : LoadedAssemblies * DumpedAssembly * ResolvedTypeIdentity =
            match Assembly.resolveTopLevelTypeFromName fromAssembly assemblies (Some ns) name ImmutableArray.Empty with
            | TypeResolutionResult.Resolved (assy, identity, _) -> assemblies, assy, identity
            | TypeResolutionResult.FirstLoadAssy assyRef ->
                let handle, referencedIn = assyRef.Handle
                let assemblies, _ = loadAssembly.LoadAssembly assemblies referencedIn handle

                // Same guard production uses: a load that leaves the reference unbound would make
                // this retry spin forever, so fail with a diagnosis instead of hanging.
                go (LoadedAssemblies.assertReferenceBound $"%s{ns}.%s{name}" assyRef assemblies)

        go assemblies

    /// The one reference in `mscorlib` that names `System.IO.FileSystem.AccessControl`.
    let private forwardingReference (mscorlib : DumpedAssembly) : AssemblyReference =
        mscorlib.AssemblyReferences.Values
        |> Seq.filter (fun r -> r.Name.Name = "System.IO.FileSystem.AccessControl")
        |> Seq.exactlyOne

    /// The reported scenario: `mscorlib` forwards `System.Security.AccessControl.FileSystemRights`
    /// to `System.IO.FileSystem.AccessControl` through an AssemblyReference whose Version is
    /// 0.0.0.0, while the real DLL's AssemblyDefinition Version is the shared framework's. No other
    /// assembly in the shared framework references it at all, so this is the only way the
    /// interpreter can ever come to load it.
    let private loadForwardedAccessControlType
        ()
        : LoadedAssemblies * DumpedAssembly * DumpedAssembly * DumpedAssembly * ResolvedTypeIdentity
        =
        let mscorlibPath = requireFile "mscorlib.dll"
        requireFile "System.IO.FileSystem.AccessControl.dll" |> ignore

        let corelib = readAssembly corelibPath
        let mscorlib = readAssembly mscorlibPath
        let loaded = LoadedAssemblies.ofAssemblies [ corelib ; mscorlib ]

        let loaded, targetAssembly, identity =
            resolveWithLoads (loader ()) loaded mscorlib "System.Security.AccessControl" "FileSystemRights"

        // Sanity: we really did land in the forwarded-to assembly, and this really is the
        // ref-identity-differs-from-def-identity case the fix is about.
        targetAssembly.Name.Name |> shouldEqual "System.IO.FileSystem.AccessControl"

        (forwardingReference mscorlib).Name.FullName
        |> shouldNotEqual targetAssembly.Name.FullName

        identity.AssemblyFullName |> shouldEqual targetAssembly.Name.FullName

        loaded, corelib, mscorlib, targetAssembly, identity

    /// The invariant the whole design rests on: whatever route got an assembly into the load
    /// context, it is findable by the only identity its consumers ever hold.
    [<Test>]
    let ``a loaded assembly is findable by its definition identity`` () : unit =
        let loaded, _, _, targetAssembly, _ = loadForwardedAccessControlType ()

        loaded.ContainsDefinition targetAssembly.Name |> shouldEqual true
        loaded.TryByDefinition targetAssembly.Name |> Option.isSome |> shouldEqual true

    /// ...and the reference that got us there still resolves, so we do not go back to disk.
    [<Test>]
    let ``the reference that triggered the load still resolves to the same assembly`` () : unit =
        let loaded, _, mscorlib, targetAssembly, _ = loadForwardedAccessControlType ()

        match loaded.TryResolveReference (forwardingReference mscorlib) with
        | None -> Assert.Fail "Expected the recorded binding to resolve the forwarding reference"
        | Some resolved -> resolved.Name.FullName |> shouldEqual targetAssembly.Name.FullName

    [<Test>]
    let ``ensureTypeDefinitionBaseAssembliesLoaded succeeds for a type reached through a forwarding facade`` () : unit =
        let loaded, _, _, targetAssembly, identity = loadForwardedAccessControlType ()

        Concretization.ensureTypeDefinitionBaseAssembliesLoaded
            (loader ())
            loaded
            targetAssembly.Name
            identity.TypeDefinition.Get
        |> ignore

    [<Test>]
    let ``concretizeTypeDefinition succeeds for a type reached through a forwarding facade`` () : unit =
        let loaded, corelib, _, _, identity = loadForwardedAccessControlType ()

        let baseTypes = Corelib.getBaseTypes corelib

        let ctx : TypeConcretization.ConcretizationContext<DumpedAssembly> =
            {
                ConcreteTypes = Corelib.concretizeAll loaded baseTypes AllConcreteTypes.Empty
                LoadedAssemblies = loaded
                BaseTypes = baseTypes
            }

        TypeConcretization.concretizeTypeDefinition ctx identity |> ignore

    /// A reference whose identity differs from every loaded assembly's definition identity must
    /// NOT resolve — otherwise the binder would be inventing bindings it has not made.
    [<Test>]
    let ``a mismatched reference does not resolve against an unbound load context`` () : unit =
        let mscorlibPath = requireFile "mscorlib.dll"
        let corelib = readAssembly corelibPath
        let mscorlib = readAssembly mscorlibPath
        let loaded = LoadedAssemblies.ofAssemblies [ corelib ; mscorlib ]

        let reference = forwardingReference mscorlib

        loaded.TryResolveReference reference |> Option.isSome |> shouldEqual false

    /// A reference whose identity happens to equal an already-loaded assembly's definition
    /// identity must resolve to it without going to disk — the CLR's exact-identity match. This is
    /// how an assembly registered directly (the entry assembly, or a fixture that exists only in
    /// memory, never written to a runtime dir) is found the first time something references it.
    [<Test>]
    let ``an exact identity match resolves with no recorded binding`` () : unit =
        let mscorlibPath = requireFile "mscorlib.dll"
        let mscorlib = readAssembly mscorlibPath
        let reference = forwardingReference mscorlib

        // Stand in for "an assembly registered directly whose definition identity is exactly what
        // some reference names": load the referent and register it under the reference's identity.
        let target = readAssembly (requireFile "System.IO.FileSystem.AccessControl.dll")

        let loaded =
            (LoadedAssemblies.empty.WithBoundReference reference target |> fst).WithLoadedAssembly target

        // Now build a fresh context holding only an assembly whose definition identity equals the
        // reference's identity, with no binding recorded at all.
        let bindingFree = LoadedAssemblies.ofAssemblies [ target ]
        bindingFree.TryResolveReference reference |> Option.isSome |> shouldEqual false

        // ...whereas the recorded binding does resolve.
        loaded.TryResolveReference reference |> Option.isSome |> shouldEqual true
