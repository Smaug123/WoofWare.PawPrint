namespace WoofWare.PawPrint

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Collections.Immutable
open System.Diagnostics
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core

/// <summary>
/// The assemblies the interpreter has loaded, together with the record of which
/// AssemblyReferences have already been bound to which of them. This is the CLR's split
/// between a *binder* (which reference identity resolves to which assembly) and a *load
/// context* (which assembly has which identity).
/// </summary>
/// <remarks>
/// Two kinds of identity are in play, and they are NOT interchangeable:
///
/// <para>
/// A <em>definition</em> identity is an assembly's own <c>AssemblyDefinition</c> FullName. It is
/// what <c>DumpedAssembly.DefinitionFullName</c>, <c>ConcreteType.AssemblyFullName</c> and
/// <c>ResolvedTypeIdentity.AssemblyFullName</c> all carry, and it is the load context's key.
/// </para>
///
/// <para>
/// A <em>reference</em> identity is the FullName recorded in an <c>AssemblyReference</c> row of
/// some other assembly. It need not equal the definition identity of whatever it binds to:
/// in the .NET shared framework the .NET Framework compatibility facades (mscorlib, System,
/// System.Core, ...) reference their implementation assemblies with <c>Version=0.0.0.0</c>,
/// and any assembly built for an older target framework references the BCL by the version
/// that framework shipped rather than the version on disk.
/// </para>
///
/// Consequently the only way to ask about a reference identity is <c>TryResolveReference</c>,
/// which takes an <c>AssemblyReference</c> — you cannot reach the binder with a bare
/// <c>AssemblyName</c>, and every other member consumes a definition identity.
/// </remarks>
[<NoEquality ; NoComparison>]
type LoadedAssemblies =
    private
        {
            /// AssemblyDefinition FullName -> the assembly bearing that identity.
            ByDefinition : ImmutableDictionary<string, DumpedAssembly>
            /// AssemblyReference FullName -> the AssemblyDefinition FullName it bound to.
            Bindings : ImmutableDictionary<string, string>
            /// The keys of <c>ByDefinition</c>, in the order each was first registered.
            LoadOrder : ImmutableArray<string>
        }

    /// <summary>
    /// Every definition identity currently loaded. For diagnostics only — see
    /// <c>DefinitionNamesInLoadOrder</c> for anything a guest can observe.
    /// </summary>
    /// <remarks>
    /// This is the backing dictionary's own enumeration order, which is a hash order over
    /// string hash codes — and those are randomised per process, so the same identities
    /// registered in the same order enumerate differently between runs. Measured: five
    /// assembly display names inserted identically into an <c>ImmutableDictionary</c> came
    /// back in three different orders across three processes. Nothing reproducible may rest
    /// on it.
    /// </remarks>
    member this.DefinitionNames : string seq = this.ByDefinition.Keys

    /// <summary>
    /// Every definition identity currently loaded, in the order each was first registered.
    /// </summary>
    /// <remarks>
    /// A function of the run rather than of the set, so it is reproducible under a fixed
    /// scheduler seed and varies with the interleaving exactly as a real runtime's load order
    /// does. Registering an identity already held does not move it, so first registration wins
    /// just as it does for the instance itself.
    /// </remarks>
    member this.DefinitionNamesInLoadOrder : ImmutableArray<string> = this.LoadOrder

    /// Look an assembly up by its definition identity.
    member this.TryByDefinitionName (fullName : string) : DumpedAssembly option =
        match this.ByDefinition.TryGetValue fullName with
        | false, _ -> None
        | true, v -> Some v

    /// Look an assembly up by its definition identity.
    member this.TryByDefinition (name : AssemblyName) : DumpedAssembly option = this.TryByDefinitionName name.FullName

    /// Look an assembly up by its definition identity, failing loudly if it is not loaded.
    member this.ByDefinitionName (fullName : string) : DumpedAssembly =
        match this.ByDefinition.TryGetValue fullName with
        | true, v -> v
        | false, _ ->
            failwithf
                "Assembly %s is not loaded. Loaded assemblies: %s"
                fullName
                (this.ByDefinition.Keys |> Seq.sort |> String.concat " ; ")

    /// Look an assembly up by its definition identity, failing loudly if it is not loaded.
    member this.Item
        with get (name : AssemblyName) : DumpedAssembly = this.ByDefinitionName name.FullName

    /// True if an assembly with this definition identity is loaded.
    member this.ContainsDefinition (name : AssemblyName) : bool =
        this.ByDefinition.ContainsKey name.FullName

    /// <summary>
    /// Resolve an AssemblyReference to the assembly it names, if we have already bound it.
    /// </summary>
    /// <remarks>
    /// A reference with no recorded binding may still name an assembly we hold, if its
    /// reference identity happens to equal that assembly's definition identity — the CLR's
    /// exact-identity match. That is how an assembly registered directly (the entry assembly,
    /// or a fixture that exists only in memory) is found the first time some other assembly
    /// references it; without this fallback we would go to disk for an assembly we already
    /// have, and fail outright for one that was never written to disk.
    /// </remarks>
    member this.TryResolveReference (reference : WoofWare.PawPrint.AssemblyReference) : DumpedAssembly option =
        let refFullName = reference.FullName

        match this.Bindings.TryGetValue refFullName with
        | true, definitionName -> this.TryByDefinitionName definitionName
        | false, _ -> this.TryByDefinitionName refFullName

    /// <summary>
    /// The canonical instance for <paramref name="assy"/>'s definition identity: whatever we
    /// already hold under that identity, or <paramref name="assy"/> itself if we hold nothing.
    /// </summary>
    /// <remarks>
    /// Every route into the load context goes through here, so that none of them can register a
    /// second, conflicting build under an identity already spoken for. Two distinct assemblies
    /// claiming one definition identity means silently picking one of two different sets of
    /// metadata to resolve and execute against; there is no safe choice, so crash.
    ///
    /// Sameness is decided by comparing metadata content, not by trusting any identifier the image
    /// carries — see <c>DumpedAssembly.HasSameContentAs</c>.
    /// </remarks>
    member private this.Canonicalise (assy : DumpedAssembly) (describeRequester : unit -> string) : DumpedAssembly =
        match this.ByDefinition.TryGetValue assy.DefinitionFullName with
        | false, _ -> assy
        | true, existing ->
            // The content comparison only runs when two non-reference-equal instances
            // collide, which is rare.
            if not (existing.HasSameContentAs assy) then
                failwithf
                    "Two different assemblies both claim definition identity %s (module version IDs %O and %O, and their metadata differs). Refusing to guess which one %s refers to."
                    assy.DefinitionFullName
                    existing.ModuleVersionId
                    assy.ModuleVersionId
                    (describeRequester ())

            existing

    /// <summary>
    /// <c>LoadOrder</c> with <paramref name="definitionName"/> appended if it is new, unchanged
    /// if it is not.
    /// </summary>
    /// <remarks>
    /// Reads <c>this.ByDefinition</c>, so every caller must evaluate it against the load context
    /// as it stood <em>before</em> the corresponding <c>SetItem</c> — otherwise a re-registration
    /// of an identity already held would append a duplicate.
    /// </remarks>
    member private this.LoadOrderWith (definitionName : string) : ImmutableArray<string> =
        if this.ByDefinition.ContainsKey definitionName then
            this.LoadOrder
        else
            this.LoadOrder.Add definitionName

    /// <summary>
    /// Register an assembly under its own definition identity. Idempotent for the same build: if
    /// that identity is already loaded, the existing instance wins. Registering a *different*
    /// build under an identity we already hold is an error — see <c>Canonicalise</c>.
    /// </summary>
    member this.WithLoadedAssembly (assy : DumpedAssembly) : LoadedAssemblies =
        let canonical = this.Canonicalise assy (fun () -> "this direct registration")

        if Object.ReferenceEquals (canonical, assy) then
            { this with
                ByDefinition = this.ByDefinition.SetItem (assy.DefinitionFullName, assy)
                LoadOrder = this.LoadOrderWith assy.DefinitionFullName
            }
        else
            this

    /// <summary>
    /// Record that <paramref name="reference"/> binds to <paramref name="assy"/>, registering
    /// the assembly under its own definition identity if we do not already hold it. Returns the
    /// canonical instance for that definition identity, which is the previously-loaded one if
    /// there was one — so exactly one <c>DumpedAssembly</c> exists per definition identity.
    /// </summary>
    member this.WithBoundReference
        (reference : WoofWare.PawPrint.AssemblyReference)
        (assy : DumpedAssembly)
        : LoadedAssemblies * DumpedAssembly
        =
        let definitionName = assy.DefinitionFullName
        let canonical = this.Canonicalise assy (fun () -> reference.FullName)

        let result =
            {
                ByDefinition = this.ByDefinition.SetItem (definitionName, canonical)
                Bindings = this.Bindings.SetItem (reference.FullName, definitionName)
                LoadOrder = this.LoadOrderWith definitionName
            }

        result, canonical

[<RequireQualifiedAccess>]
module LoadedAssemblies =
    let empty : LoadedAssemblies =
        {
            ByDefinition = ImmutableDictionary.Empty
            Bindings = ImmutableDictionary.Empty
            LoadOrder = ImmutableArray<string>.Empty
        }

    /// Build a load context from assemblies indexed by their own definition identities, with no
    /// reference bindings recorded yet.
    let ofAssemblies (assemblies : DumpedAssembly seq) : LoadedAssemblies =
        assemblies
        |> Seq.fold (fun (acc : LoadedAssemblies) assy -> acc.WithLoadedAssembly assy) empty

    /// <summary>
    /// Assert that the load prompted by a <c>TypeResolutionResult.FirstLoadAssy</c> achieved what
    /// the caller is about to retry on: the reference now resolves.
    /// </summary>
    /// <remarks>
    /// Every resolution loop responds to <c>FirstLoadAssy</c> by loading and then re-running the
    /// identical resolution. If the load leaves the reference still unbound, that re-run takes the
    /// identical branch and the loop spins forever. The only way to leave a reference unbound
    /// after loading it is to have mis-filed it, so the crash names the real fault.
    /// </remarks>
    let assertReferenceBound
        (context : string)
        (reference : WoofWare.PawPrint.AssemblyReference)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies
        =
        match assemblies.TryResolveReference reference with
        | Some _ -> assemblies
        | None ->
            failwithf
                "While resolving %s: loaded %s (referenced by %s), but it is still not bound afterwards. Retrying would loop forever; the load context has probably filed it under the wrong identity."
                context
                reference.FullName
                (snd reference.Handle).FullName

/// <summary>
/// A name that resolution looked for and the searched assembly's metadata does not contain.
/// </summary>
/// <remarks>
/// This is "the metadata does not say", not "PawPrint cannot say": a shape PawPrint has not
/// implemented (a multi-module <c>AssemblyFile</c> export, a <c>ModuleRef</c> scope) still
/// crashes, because answering "absent" for one would be a lie. The distinction matters because
/// a caller may legitimately have to report absence rather than terminate —
/// <c>Assembly.GetType(name, throwOnError: false)</c> answers <c>null</c> for a forwarder whose
/// target assembly does not declare the type.
/// </remarks>
type TypeResolutionMiss =
    /// No <c>TypeDef</c> and no top-level <c>ExportedType</c> row of this name, in this assembly.
    | TopLevelTypeAbsent of searchedIn : string * ns : string option * name : string

    /// This assembly declares the given type, but nothing of this name nested inside it.
    | NestedTypeAbsent of searchedIn : string * declaringType : string * name : string

    override this.ToString () : string =
        match this with
        | TypeResolutionMiss.TopLevelTypeAbsent (searchedIn, ns, name) ->
            let ns = ns |> Option.defaultValue "<global>"
            $"top-level type %s{ns}.%s{name} is not declared in %s{searchedIn}"
        | TypeResolutionMiss.NestedTypeAbsent (searchedIn, declaringType, name) ->
            $"no type named %s{name} is nested inside %s{declaringType} in %s{searchedIn}"

type TypeResolutionResult =
    | FirstLoadAssy of WoofWare.PawPrint.AssemblyReference
    | Resolved of DumpedAssembly * ResolvedTypeIdentity * TypeInfo<TypeDefn, TypeDefn>

    /// The metadata searched does not contain the name asked for. Callers that have no way to
    /// report absence should <c>failwith</c> the miss's own description.
    | NotFound of TypeResolutionMiss

    override this.ToString () : string =
        match this with
        | TypeResolutionResult.FirstLoadAssy a -> $"FirstLoadAssy(%s{a.FullName})"
        | TypeResolutionResult.Resolved (assy, identity, ty) ->
            $"Resolved(%s{assy.DefinitionFullName}: %O{identity} {string<TypeInfo<TypeDefn, TypeDefn>> ty})"
        | TypeResolutionResult.NotFound miss -> $"NotFound(%O{miss})"
