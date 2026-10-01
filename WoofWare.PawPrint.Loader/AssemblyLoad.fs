namespace WoofWare.PawPrint

open System.Reflection
open System.Reflection.Metadata

/// Why a loader declined to bind an AssemblyReference.
type AssemblyLoadFailure =
    /// Nowhere the loader is willing to look supplies this assembly.
    | NoSuchAssembly of WoofWare.PawPrint.AssemblyReference

    /// The reference is not in the load context, and this loader is not permitted to read files
    /// to get it. Always a bug in the caller's reasoning about what has been loaded, never a
    /// fact about the world.
    | LoadingNotPermitted of WoofWare.PawPrint.AssemblyReference

    override this.ToString () : string =
        match this with
        | AssemblyLoadFailure.NoSuchAssembly reference ->
            $"Could not find a readable DLL in any runtime dir with name %s{reference.Name.Name}.dll"
        | AssemblyLoadFailure.LoadingNotPermitted reference ->
            let referencedIn = snd reference.Handle

            $"Assembly %s{reference.FullName}, referenced by %s{referencedIn.FullName}, is not loaded, and this context is not permitted to load it."

    /// The reference that did not bind, whichever way it failed to.
    member this.Reference : WoofWare.PawPrint.AssemblyReference =
        match this with
        | AssemblyLoadFailure.NoSuchAssembly reference
        | AssemblyLoadFailure.LoadingNotPermitted reference -> reference

/// <summary>
/// Why a type's base-type chain could not be walked to its end.
/// </summary>
/// <remarks>
/// The two are different facts and the real runtime reports them differently, so a caller that
/// surfaces them to a guest must not collapse them: an unbindable assembly is a
/// <c>FileNotFoundException</c> that <c>RuntimeAssembly.GetTypeCore</c> catches when it was told
/// not to throw, whereas a base type absent from an assembly that did bind is a
/// <c>TypeLoadException</c> that escapes that catch and reaches the guest either way (measured
/// against .NET 10, at both <c>throwOnError</c> values).
/// </remarks>
type BaseChainFailure =
    /// A reference in the chain names an assembly the loader would not bind.
    | LoadFailed of AssemblyLoadFailure

    /// Every assembly in the chain bound, and one of them does not declare a base type the
    /// metadata says it does.
    | BaseTypeAbsent of TypeResolutionMiss

    override this.ToString () : string =
        match this with
        | BaseChainFailure.LoadFailed failure -> string<AssemblyLoadFailure> failure
        | BaseChainFailure.BaseTypeAbsent miss -> $"base type is not declared where the metadata says: %O{miss}"

type IAssemblyLoad =
    /// <param name="referencedIn">
    /// The <em>definition</em> identity of the assembly whose AssemblyReference table
    /// <c>handle</c> indexes. AssemblyReferenceHandles are only meaningful relative to the
    /// assembly that declares them.
    /// </param>
    abstract TryLoadAssembly :
        loadedAssemblies : LoadedAssemblies ->
        referencedIn : AssemblyName ->
        handle : AssemblyReferenceHandle ->
            Result<LoadedAssemblies * DumpedAssembly, AssemblyLoadFailure>

[<RequireQualifiedAccess>]
module IAssemblyLoad =
    /// <summary>
    /// Bind an AssemblyReference, terminating if it does not bind. This is what most callers want:
    /// they are walking metadata that has to be there, and have nowhere to put a failure.
    /// </summary>
    let load
        (loader : IAssemblyLoad)
        (loadedAssemblies : LoadedAssemblies)
        (referencedIn : AssemblyName)
        (handle : AssemblyReferenceHandle)
        : LoadedAssemblies * DumpedAssembly
        =
        match loader.TryLoadAssembly loadedAssemblies referencedIn handle with
        | Ok loaded -> loaded
        | Error failure -> failwith (string<AssemblyLoadFailure> failure)

    /// <summary>
    /// An <c>IAssemblyLoad</c> which refuses to go to disk: it binds an AssemblyReference only if
    /// the load context already holds the assembly. Use it where everything that could possibly
    /// be needed has provably been loaded already, so that a miss is a bug rather than a cue to
    /// read a file.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The proof must be evident *at the call site* — typically because every type reachable from
    /// the inputs lives in an assembly you are holding, as in <c>Corelib.concretizeAll</c>, which
    /// touches only corelib types. Do not use it to encode "some earlier sweep primed this": that
    /// is a claim about the whole interpreter, it cannot be checked here, and it is exactly the
    /// claim that rotted in issue #868, where <c>CliType.zeroOf</c> asserted it and a struct's
    /// field type turned out to live in an assembly the guest never named.
    /// </para>
    /// <para>
    /// The remaining uses that do rest on an upstream sweep are the handful of layout helpers
    /// which return a bare value with nowhere to put an updated load context or concrete-type
    /// registry (<c>MethodState.Empty</c>, <c>IlMachineManagedByref.zeroForConcreteType</c>,
    /// <c>ManagedPointerByteView.arrayElementSize</c>). Each says so at its call site. They keep
    /// this loader on purpose: failing loudly beats silently re-reading an assembly and
    /// discarding the handles minted from it.
    /// </para>
    /// </remarks>
    let alreadyLoadedOnly : IAssemblyLoad =
        { new IAssemblyLoad with
            member _.TryLoadAssembly loaded referencedIn handle =
                let targetRef = loaded.[referencedIn].AssemblyReferences.[handle]

                match loaded.TryResolveReference targetRef with
                | Some target -> Ok (loaded, target)
                | None -> AssemblyLoadFailure.LoadingNotPermitted targetRef |> Error
        }
