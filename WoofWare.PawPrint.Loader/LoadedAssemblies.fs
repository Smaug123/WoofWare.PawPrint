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
/// What the load context holds for a dynamic assembly beyond its <c>DumpedAssembly</c>: the emit scope
/// that assembly's image is written from, which of its types the runtime has created, and which loaded
/// assembly each of its <c>AssemblyRef</c> rows was emitted for.
/// </summary>
[<NoEquality ; NoComparison>]
type private DynamicAssemblyContent =
    {
        Scope : EmitScope
        Created : ImmutableHashSet<TypeDefRid>
        /// The definition identity of the assembly each recorded row names, as CoreCLR's module keeps
        /// it in its rid map (<c>ForceStoreAssemblyRef</c>).
        AssemblyRefTargets : ImmutableDictionary<AssemblyRefRid, string>
    }

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
            /// The keys of <c>ByDefinition</c> that name a dynamic assembly: one created at run time
            /// by <c>AssemblyBuilder</c> rather than read from an image on disk. Each one's
            /// <c>DumpedAssembly</c> is the image of its scope.
            Dynamic : ImmutableDictionary<string, DynamicAssemblyContent>
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
    /// True if the loaded assembly with this definition identity is a dynamic one, registered by
    /// <c>WithDynamicAssembly</c>. What <c>Assembly.IsDynamic</c> reports.
    /// </summary>
    member this.IsDynamic (fullName : string) : bool = this.Dynamic.ContainsKey fullName

    /// <summary>
    /// The emit scope of the dynamic assembly with this definition identity, from which its
    /// <c>DumpedAssembly</c> was written; <c>None</c> if no dynamic assembly has that identity.
    /// </summary>
    member this.TryDynamicScope (fullName : string) : EmitScope option =
        match this.Dynamic.TryGetValue fullName with
        | true, content -> Some content.Scope
        | false, _ -> None

    /// <summary>
    /// True if the runtime has created this type of the dynamic assembly with this definition
    /// identity (<c>WithCreatedType</c>).
    /// </summary>
    member this.IsCreatedType (fullName : string) (typeDef : TypeDefRid) : bool =
        match this.Dynamic.TryGetValue fullName with
        | true, content -> content.Created.Contains typeDef
        | false, _ -> false

    /// <summary>
    /// True if <paramref name="identity"/> is a type of a dynamic assembly that the runtime has not
    /// created (<c>WithCreatedType</c>). CoreCLR's class loader does not load such a type: it raises
    /// the <c>TypeResolve</c> event instead.
    /// </summary>
    member this.IsUncreatedDynamicType (identity : ResolvedTypeIdentity) : bool =
        match this.Dynamic.TryGetValue identity.AssemblyFullName with
        | false, _ -> false
        | true, content ->
            let rid = MetadataTokens.GetRowNumber identity.TypeDefinition.Get

            match EmitScope.tryTypeDefRid rid content.Scope with
            | Some typeDef -> not (content.Created.Contains typeDef)
            | None ->
                failwith
                    $"BUG: TypeDef %d{rid} of the dynamic assembly %s{identity.AssemblyFullName} is not in its scope, from which its image was written"

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
    ///
    /// A dynamic assembly is never found this way. CoreCLR's binder does not consult dynamic
    /// assemblies, so a reference that names one is bound, if at all, to an image on disk.
    ///
    /// A row of a dynamic assembly that its emitter recorded (<c>WithAssemblyRefTarget</c>) resolves
    /// to the assembly it was emitted for, before any of that: CoreCLR's module looks a reference up
    /// in its rid map before it asks the binder.
    /// </remarks>
    member this.TryResolveReference (reference : WoofWare.PawPrint.AssemblyReference) : DumpedAssembly option =
        let recorded =
            match this.Dynamic.TryGetValue reference.ReferencingAssemblyFullName with
            | false, _ -> None
            | true, content ->
                let row =
                    MetadataTokens.GetRowNumber (
                        AssemblyReferenceHandle.op_Implicit (fst reference.Handle) : EntityHandle
                    )

                match EmitScope.tryAssemblyRefRid row content.Scope with
                | None ->
                    failwith
                        $"TryResolveReference: %s{reference.ReferencingAssemblyFullName} has no AssemblyRef row %d{row} in its scope, but its image holds a reference there"
                | Some row ->
                    match content.AssemblyRefTargets.TryGetValue row with
                    | true, target -> Some target
                    | false, _ -> None

        match recorded with
        | Some target -> Some this.ByDefinition.[target]
        | None ->

        let refFullName = reference.FullName

        match this.Bindings.TryGetValue refFullName with
        | true, definitionName -> this.TryByDefinitionName definitionName
        | false, _ ->
            if this.Dynamic.ContainsKey refFullName then
                None
            else
                this.TryByDefinitionName refFullName

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

        if this.Dynamic.ContainsKey definitionName then
            failwithf
                "Refusing to bind reference %s to the dynamic assembly %s: CoreCLR's binder never binds a reference to a dynamic assembly."
                reference.FullName
                definitionName

        let canonical = this.Canonicalise assy (fun () -> reference.FullName)

        let result =
            { this with
                ByDefinition = this.ByDefinition.SetItem (definitionName, canonical)
                Bindings = this.Bindings.SetItem (reference.FullName, definitionName)
                LoadOrder = this.LoadOrderWith definitionName
            }

        result, canonical

    /// The assembly an emit scope's image holds.
    static member private ReadScope (loggerFactory : ILoggerFactory) (scope : EmitScope) : DumpedAssembly =
        use image = new MemoryStream (EmitScopeImage.write scope)
        Assembly.read loggerFactory None image

    /// What the load context holds for the dynamic assembly with this identity, crashing if it holds
    /// none: the caller has mistaken some other assembly for one the runtime emitted.
    member private this.DynamicContent (operation : string) (fullName : string) : DynamicAssemblyContent =
        match this.Dynamic.TryGetValue fullName with
        | true, content -> content
        | false, _ -> failwith $"%s{operation}: %s{fullName} is not a dynamic assembly, so it has no emit scope"

    /// <summary>
    /// Register a dynamic assembly: one created at run time, whose identity no image on disk
    /// supplied. Its <c>DumpedAssembly</c> is read from the image of <paramref name="scope"/>, which
    /// must define the <c>Assembly</c> row. Such an assembly is found by its definition identity like
    /// any other, and reported by <c>IsDynamic</c>, but no assembly reference ever binds to it.
    /// </summary>
    /// <returns>
    /// The load context and the dynamic assembly; or an error naming the assembly already loaded under
    /// the scope's definition identity, if there is one. CoreCLR admits a dynamic assembly whose
    /// identity another assembly already has, but here that would make the two one assembly.
    /// </returns>
    member this.WithDynamicAssembly
        (loggerFactory : ILoggerFactory)
        (scope : EmitScope)
        : Result<LoadedAssemblies * DumpedAssembly, DumpedAssembly>
        =
        if (EmitScope.assembly scope).IsNone then
            invalidArg (nameof scope) "a dynamic assembly's scope must define the Assembly row, which names it"

        let assy = LoadedAssemblies.ReadScope loggerFactory scope
        let definitionName = assy.DefinitionFullName

        match this.ByDefinition.TryGetValue definitionName with
        | true, existing -> Error existing
        | false, _ ->
            let content : DynamicAssemblyContent =
                {
                    Scope = scope
                    Created = ImmutableHashSet.Empty
                    AssemblyRefTargets = ImmutableDictionary.Empty
                }

            ({ this with
                ByDefinition = this.ByDefinition.Add (definitionName, assy)
                LoadOrder = this.LoadOrder.Add definitionName
                Dynamic = this.Dynamic.Add (definitionName, content)
             },
             assy)
            |> Ok

    /// <summary>
    /// Replace the dynamic assembly with this definition identity by the image of
    /// <paramref name="scope"/>, under the same identity and in the same place in the load order.
    /// </summary>
    /// <remarks>
    /// Crashes unless <paramref name="scope"/> extends the assembly's current scope
    /// (<c>EmitScopeExtension.violations</c>), given the types created so far: every handle into the
    /// old image then means the same in the new one, and every created type reads back the same, so
    /// nothing the runtime has already read from the old image is invalidated. A scope that does not
    /// extend it is a fault in whatever built the scope.
    /// </remarks>
    /// <returns>The load context and the replacement assembly.</returns>
    member this.WithEmitScope
        (loggerFactory : ILoggerFactory)
        (fullName : string)
        (scope : EmitScope)
        : LoadedAssemblies * DumpedAssembly
        =
        let content = this.DynamicContent "WithEmitScope" fullName

        match EmitScopeExtension.violations content.Created content.Scope scope with
        | [] -> ()
        | violations ->
            let described = violations |> List.map string |> String.concat "; "

            failwith
                $"Refusing to replace the dynamic assembly %s{fullName} by the image of a scope that does not extend its own: %s{described}"

        let assy = LoadedAssemblies.ReadScope loggerFactory scope

        // The `Assembly` row is unchanged, so the identity is too.
        if assy.DefinitionFullName <> fullName then
            failwith
                $"The image of an extension of %s{fullName}'s scope is named %s{assy.DefinitionFullName}; an unchanged Assembly row must name the same assembly"

        { this with
            ByDefinition = this.ByDefinition.SetItem (fullName, assy)
            Dynamic =
                this.Dynamic.SetItem (
                    fullName,
                    { content with
                        Scope = scope
                    }
                )
        },
        assy

    /// <summary>
    /// Record that the runtime has created this type of the dynamic assembly with this definition
    /// identity, as CoreCLR's <c>TypeBuilder_TermCreateClass</c> does: from now on its members are
    /// fixed, and <c>WithEmitScope</c> refuses a scope that changes them.
    /// </summary>
    /// <remarks>Crashes if the type is not in the assembly's scope, or is already created.</remarks>
    member this.WithCreatedType (fullName : string) (typeDef : TypeDefRid) : LoadedAssemblies =
        let content = this.DynamicContent "WithCreatedType" fullName

        if typeDef.Rid > (EmitScope.typeDefs content.Scope).Count then
            failwith
                $"Refusing to create TypeDef %d{typeDef.Rid} of %s{fullName}: it is not in the scope, which has %d{(EmitScope.typeDefs content.Scope).Count} types"

        if content.Created.Contains typeDef then
            failwith $"Refusing to create TypeDef %d{typeDef.Rid} of %s{fullName}: it is already created"

        { this with
            Dynamic =
                this.Dynamic.SetItem (
                    fullName,
                    { content with
                        Created = content.Created.Add typeDef
                    }
                )
        }

    /// <summary>
    /// Record that the <c>AssemblyRef</c> row <paramref name="assemblyRef"/> of the dynamic assembly with
    /// this definition identity was emitted for <paramref name="target"/>, as CoreCLR's
    /// <c>ModuleBuilder_GetTypeRef</c> records it in the module's rid map. From then on
    /// <c>TryResolveReference</c> resolves that row of that assembly to <paramref name="target"/>, a
    /// dynamic assembly included, whatever the binder would do with its name.
    /// </summary>
    /// <remarks>
    /// A row already recorded keeps its first target, as CoreCLR's does: the emitter can hand one row to
    /// two assemblies, since it does not compare the build and revision of <c>mscorlib</c>. Crashes
    /// unless <paramref name="target"/> is the loaded assembly with its identity and
    /// <paramref name="assemblyRef"/> is a row that <c>EmitScopeReferences.defineAssemblyRef</c> could
    /// have returned for a reference to <paramref name="target"/> (<c>isAssemblyRefFor</c>).
    /// </remarks>
    member this.WithAssemblyRefTarget
        (fullName : string)
        (assemblyRef : AssemblyRefRid)
        (target : DumpedAssembly)
        : LoadedAssemblies
        =
        let operation = "WithAssemblyRefTarget"
        let content = this.DynamicContent operation fullName

        match this.ByDefinition.TryGetValue target.DefinitionFullName with
        | true, loaded when Object.ReferenceEquals (loaded, target) -> ()
        | _ ->
            failwith
                $"%s{operation}: %s{target.DefinitionFullName} is not the loaded assembly with that identity, so no row of %s{fullName} can have been emitted for it"

        let request = AssemblyRefRequest.ofDefinition (target.PeReader.GetMetadataReader ())

        if not (EmitScopeReferences.isAssemblyRefFor request assemblyRef content.Scope) then
            failwith
                $"%s{operation}: AssemblyRef row %d{assemblyRef.Rid} of %s{fullName} is not a row its emitter would find or add for a reference to %s{target.DefinitionFullName}"

        if content.AssemblyRefTargets.ContainsKey assemblyRef then
            this
        else
            { this with
                Dynamic =
                    this.Dynamic.SetItem (
                        fullName,
                        { content with
                            AssemblyRefTargets = content.AssemblyRefTargets.Add (assemblyRef, target.DefinitionFullName)
                        }
                    )
            }

[<RequireQualifiedAccess>]
module LoadedAssemblies =
    let empty : LoadedAssemblies =
        {
            ByDefinition = ImmutableDictionary.Empty
            Bindings = ImmutableDictionary.Empty
            LoadOrder = ImmutableArray<string>.Empty
            Dynamic = ImmutableDictionary.Empty
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

    /// The search reached a shape PawPrint does not implement resolving.
    | Unmodelled of UnmodelledBinding

    override this.ToString () : string =
        match this with
        | TypeResolutionResult.FirstLoadAssy a -> $"FirstLoadAssy(%s{a.FullName})"
        | TypeResolutionResult.Resolved (assy, identity, ty) ->
            $"Resolved(%s{assy.DefinitionFullName}: %O{identity} {string<TypeInfo<TypeDefn, TypeDefn>> ty})"
        | TypeResolutionResult.NotFound miss -> $"NotFound(%O{miss})"
        | TypeResolutionResult.Unmodelled unmodelled -> $"Unmodelled(%s{unmodelled.Diagnostic})"
