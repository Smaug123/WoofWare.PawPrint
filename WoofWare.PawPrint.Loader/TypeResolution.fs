namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

/// <summary>
/// What following an assembly's type forwarders to the type they name produced.
/// </summary>
/// <remarks>
/// The two failure cases are distinct facts about the world, and callers that report them to a
/// guest must tell them apart: a reference nobody can bind is what the real runtime reports as
/// <c>FileNotFoundException</c>, whereas a chain that binds every assembly and still finds no such
/// type is a plain absence, which <c>Assembly.GetType</c> reports as <c>null</c> (and
/// <c>TypeNameResolver</c> turns into <c>TypeLoadException</c> when the caller asked it to throw).
/// </remarks>
type ExportedTypeResolution =
    /// The chain ended at a type definition, in the assembly that declares it.
    | Forwarded of DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>

    /// A reference in the chain names an assembly no runtime directory supplies.
    | AssemblyUnavailable of WoofWare.PawPrint.AssemblyReference

    /// Every assembly in the chain bound, and none of them declares the type.
    | TypeAbsent of TypeResolutionMiss

    /// The chain arrived at the type, and a type in *its* base chain is not declared where the
    /// metadata says it is. Distinct from <c>TypeAbsent</c> because the real runtime answers the
    /// two differently: this one reaches a guest as a <c>TypeLoadException</c> whether or not the
    /// caller asked to be thrown at, whereas an absent type is simply not found.
    | BaseTypeAbsent of TypeResolutionMiss

    /// The chain reached a shape PawPrint does not implement following.
    | Unmodelled of UnmodelledBinding

/// <summary>
/// Where following an assembly's type forwarders arrived, before anything the arrived-at type
/// depends on has been loaded.
/// </summary>
[<RequireQualifiedAccess>]
type ExportChainArrival =
    /// The chain ended at a type definition, in the assembly that declares it; nothing that type
    /// depends on has been loaded yet.
    | Arrived of DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>

    /// As <see cref="ExportedTypeResolution.AssemblyUnavailable" />.
    | AssemblyUnavailable of WoofWare.PawPrint.AssemblyReference

    /// As <see cref="ExportedTypeResolution.TypeAbsent" />.
    | TypeAbsent of TypeResolutionMiss

    /// As <see cref="ExportedTypeResolution.Unmodelled" />.
    | Unmodelled of UnmodelledBinding

/// <summary>
/// The definition a type reference names, found without loading any type.
/// </summary>
/// <remarks>
/// The two failure cases are distinct, as for <see cref="ExportedTypeResolution" />: binding a
/// reference to an assembly nobody supplies is what the real runtime reports as
/// <c>FileNotFoundException</c>, and a type missing from an assembly that binds as
/// <c>TypeLoadException</c>.
/// </remarks>
[<RequireQualifiedAccess>]
type TypeReferenceIdentity =
    /// The reference names this definition.
    | Resolved of ResolvedTypeIdentity

    /// A reference on the way names an assembly no runtime directory supplies.
    | AssemblyUnavailable of WoofWare.PawPrint.AssemblyReference

    /// Every assembly on the way bound, and the last one does not declare the name.
    | TypeAbsent of TypeResolutionMiss

    /// The way reached a shape PawPrint does not implement resolving.
    | Unmodelled of UnmodelledBinding

/// <summary>
/// Why a type that a guest's metadata names could not be bound and loaded.
/// </summary>
/// <remarks>
/// The first three are distinct facts the real runtime reports differently, as for
/// <see cref="ExportedTypeResolution" />: an assembly nobody supplies is a
/// <c>FileNotFoundException</c>, a name its assembly does not declare a <c>TypeLoadException</c>, and
/// a base type missing from where its derived type's metadata says a <c>TypeLoadException</c> too, but
/// one a non-throwing lookup does not catch.
/// </remarks>
[<RequireQualifiedAccess>]
type TypeBindingFailure =
    /// A reference on the way names an assembly no runtime directory supplies.
    | AssemblyUnavailable of WoofWare.PawPrint.AssemblyReference
    /// Every assembly on the way bound, and the last one does not declare the name.
    | TypeAbsent of TypeResolutionMiss
    /// The type was found, and a type in its base chain is not declared where the metadata says it
    /// is.
    | BaseTypeAbsent of TypeResolutionMiss
    /// The way reached a shape PawPrint does not implement binding.
    | Unmodelled of UnmodelledBinding

/// Functions on <see cref="TypeBindingFailure" />.
[<RequireQualifiedAccess>]
module TypeBindingFailure =

    /// What a base chain that could not be walked to its end means for binding the type it belongs
    /// to. A load the loader was not permitted to make is a bug in the caller, not a fact about the
    /// guest's metadata, so it terminates.
    let ofBaseChain (failure : BaseChainFailure) : TypeBindingFailure =
        match failure with
        | BaseChainFailure.LoadFailed (AssemblyLoadFailure.NoSuchAssembly reference) ->
            TypeBindingFailure.AssemblyUnavailable reference
        | BaseChainFailure.LoadFailed (AssemblyLoadFailure.LoadingNotPermitted _) ->
            failwith (string<BaseChainFailure> failure)
        | BaseChainFailure.BaseTypeAbsent miss -> TypeBindingFailure.BaseTypeAbsent miss
        | BaseChainFailure.Unmodelled unmodelled -> TypeBindingFailure.Unmodelled unmodelled

    /// The failure, described for a run that stops on it.
    let describe (failure : TypeBindingFailure) : string =
        match failure with
        | TypeBindingFailure.AssemblyUnavailable reference ->
            $"Could not find a readable DLL in any runtime dir with name %s{reference.Name.Name}.dll"
        | TypeBindingFailure.TypeAbsent miss -> string<TypeResolutionMiss> miss
        | TypeBindingFailure.BaseTypeAbsent miss -> $"base type is not declared where the metadata says: %O{miss}"
        | TypeBindingFailure.Unmodelled unmodelled -> unmodelled.Diagnostic

/// Functions for resolving type metadata (TypeRefs, TypeDefs, TypeSpecs) to concrete TypeInfo values.
/// Operates on the loaded-assemblies dictionary directly, without requiring IlMachineState.
[<RequireQualifiedAccess>]
module TypeResolution =

    /// <summary>
    /// Bind an AssemblyReference to an assembly, loading it from the runtime dirs if this is the
    /// first time we have needed it. Returns the updated load context, the assembly, and the
    /// assembly's own *definition* identity — which is in general NOT the reference's identity,
    /// so callers must not assume the returned name is what they asked for.
    /// </summary>
    /// <remarks>
    /// Binding is by simple name: we look for <c>&lt;SimpleName&gt;.dll</c>, ignoring case, in each
    /// runtime dir in turn (<see cref="AssemblyProbe.tryReadFromRuntimeDirs"/>). Version, culture
    /// and public key token in the reference are therefore not honoured, which is exactly why the
    /// reference's identity so often disagrees with the definition identity of what it binds to
    /// (the .NET Framework compatibility facades reference implementation assemblies as
    /// <c>Version=0.0.0.0</c>).
    /// </remarks>
    let internal tryLoadAssembly
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (referencedInAssembly : DumpedAssembly)
        (r : AssemblyReferenceHandle)
        (assemblies : LoadedAssemblies)
        : (LoadedAssemblies * DumpedAssembly * AssemblyName) option
        =
        let assemblyRef = referencedInAssembly.AssemblyReferences.[r]

        match assemblies.TryResolveReference assemblyRef with
        | Some v -> Some (assemblies, v, v.Name)
        | None ->
            let assy =
                AssemblyProbe.tryReadFromRuntimeDirs loggerFactory dotnetRuntimeDirs assemblyRef.Name.Name

            match assy with
            | None -> None
            | Some assy ->
                // Record both the assembly (under its own definition identity) and the binding
                // that got us here, so the next probe with this reference identity is a hit.
                let assemblies, canonical = assemblies.WithBoundReference assemblyRef assy
                Some (assemblies, canonical, canonical.Name)

    /// <summary>
    /// The interpreter's assembly loader: binds AssemblyReferences by simple name against
    /// <paramref name="dotnetRuntimeDirs"/>, in order.
    /// </summary>
    /// <remarks>
    /// This is the one loader. Tests must use it too rather than hand-rolling an
    /// <c>IAssemblyLoad</c>: test fakes that key their dictionaries differently from production
    /// hide reference-binding bugs from the suite.
    /// </remarks>
    let directoryLoader (loggerFactory : ILoggerFactory) (dotnetRuntimeDirs : string seq) : IAssemblyLoad =
        { new IAssemblyLoad with
            member _.TryLoadAssembly loaded referencedIn ref =
                match tryLoadAssembly loggerFactory dotnetRuntimeDirs loaded.[referencedIn] ref loaded with
                | Some (assemblies, targetAssy, _name) -> Ok (assemblies, targetAssy)
                | None ->
                    AssemblyLoadFailure.NoSuchAssembly loaded.[referencedIn].AssemblyReferences.[ref]
                    |> Error
        }

    /// <summary>
    /// Discharge the precondition of the pure base-chain walks, for a type we are about to hand
    /// to a caller: load every assembly reachable from <paramref name="ty"/>'s base-type chain.
    /// </summary>
    /// <remarks>
    /// <para>
    /// <c>LoadedTypeInfo.isValueType</c>, <c>signatureTypeKind</c> and <c>typeInfoToTypeDefn</c>
    /// walk a type's base-type chain given only a <c>LoadedAssemblies</c>. They cannot load, so
    /// they fail hard ("seems pretty unlikely that we could have constructed this object without
    /// loading its base type") whenever a link in that chain is a TypeRef scoped to an assembly
    /// nobody has loaded yet. Those functions are called from roughly a hundred places across the
    /// interpreter, almost none of which hold the load capability, so the precondition has to be
    /// established here — at the one layer that can both resolve a type and read a file.
    /// </para>
    /// <para>
    /// This also matches the real CLR, which cannot build a MethodTable for a type without first
    /// loading its base type, and so has the same closure property for free.
    /// </para>
    /// </remarks>
    let internal tryPrimeBaseChain
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (assemblies : LoadedAssemblies)
        (definedIn : DumpedAssembly)
        (ty : WoofWare.PawPrint.TypeInfo<'generic, TypeDefn>)
        : LoadedAssemblies * BaseChainFailure option
        =
        BaseChainLoading.tryEnsureTypeDefinitionBaseAssembliesLoaded
            (directoryLoader loggerFactory dotnetRuntimeDirs)
            assemblies
            definedIn
            ty.TypeDefHandle

    let private primeBaseChain
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (assemblies : LoadedAssemblies)
        (definedIn : DumpedAssembly)
        (ty : WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>)
        : LoadedAssemblies * Result<DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>, TypeBindingFailure>
        =
        match tryPrimeBaseChain loggerFactory dotnetRuntimeDirs assemblies definedIn ty with
        | assemblies, None -> assemblies, Ok (definedIn, ty)
        | assemblies, Some failure -> assemblies, Error (TypeBindingFailure.ofBaseChain failure)

    /// Bind `loadFirst`, which a resolution of `what` asked for, then carry on in the context that
    /// binds it.
    let private loadingFirst
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (what : string)
        (loadFirst : WoofWare.PawPrint.AssemblyReference)
        (assemblies : LoadedAssemblies)
        : Result<LoadedAssemblies, TypeBindingFailure>
        =
        match
            tryLoadAssembly
                loggerFactory
                dotnetRuntimeDirs
                assemblies.[snd loadFirst.Handle]
                (fst loadFirst.Handle)
                assemblies
        with
        | None -> Error (TypeBindingFailure.AssemblyUnavailable loadFirst)
        | Some (assemblies, _, _) -> Ok (LoadedAssemblies.assertReferenceBound what loadFirst assemblies)

    /// Each of `items` put through `step` in order, threading the load context and stopping at the
    /// first that fails.
    let private traverseLoaded
        (step : LoadedAssemblies -> 'a -> LoadedAssemblies * Result<'b, TypeBindingFailure>)
        (assemblies : LoadedAssemblies)
        (items : 'a seq)
        : LoadedAssemblies * Result<ImmutableArray<'b>, TypeBindingFailure>
        =
        let builder = ImmutableArray.CreateBuilder<'b> ()
        let mutable assemblies = assemblies
        let mutable failure = None
        use items = items.GetEnumerator ()

        while failure.IsNone && items.MoveNext () do
            match step assemblies items.Current with
            | loaded, Ok item ->
                assemblies <- loaded
                builder.Add item
            | loaded, Error failed ->
                assemblies <- loaded
                failure <- Some failed

        match failure with
        | Some failed -> assemblies, Error failed
        | None -> assemblies, Ok (builder.ToImmutable ())

    /// The top-level type `ns`.`name` that `assy` declares or forwards, its base chain primed.
    let rec internal resolveTopLevelTypeFromName
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (ns : string option)
        (name : string)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * Result<DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>, TypeBindingFailure>
        =
        match LoadedTypeResolution.resolveTopLevelTypeFromName assy assemblies ns name genericArgs with
        | TypeResolutionResult.Resolved (assy, _, typeDef) ->
            primeBaseChain loggerFactory dotnetRuntimeDirs assemblies assy typeDef
        | TypeResolutionResult.NotFound miss -> assemblies, Error (TypeBindingFailure.TypeAbsent miss)
        | TypeResolutionResult.Unmodelled unmodelled -> assemblies, Error (TypeBindingFailure.Unmodelled unmodelled)
        | TypeResolutionResult.FirstLoadAssy loadFirst ->
            match loadingFirst loggerFactory dotnetRuntimeDirs $"top-level type %s{name}" loadFirst assemblies with
            | Error failure -> assemblies, Error failure
            | Ok assemblies ->
                resolveTopLevelTypeFromName loggerFactory dotnetRuntimeDirs ns name genericArgs assy assemblies

    /// <summary>
    /// Follow <paramref name="ty" />'s forwarder chain to the assembly that declares the type,
    /// loading the chain's assemblies but nothing the arrived-at type itself depends on.
    /// </summary>
    /// <remarks>
    /// The arrived-at type is not yet fit to hand over: pass it to
    /// <see cref="tryPrimeExportArrival"/> first. The two are separate so a caller can inspect where
    /// the chain arrived before a failure to load the type's dependencies hides it.
    ///
    /// The returned <c>LoadedAssemblies</c> carries every load the walk did manage, in every
    /// outcome: a chain that binds two assemblies and then fails on a third must not discard the
    /// two.
    /// </remarks>
    let rec internal tryWalkExportChain
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (fromAssembly : DumpedAssembly)
        (ty : WoofWare.PawPrint.ExportedType)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * ExportChainArrival
        =
        match LoadedTypeResolution.resolveTypeFromExport fromAssembly assemblies genericArgs ty with
        | TypeResolutionResult.Resolved (assy, _, typeDef) -> assemblies, ExportChainArrival.Arrived (assy, typeDef)
        | TypeResolutionResult.NotFound miss -> assemblies, ExportChainArrival.TypeAbsent miss
        | TypeResolutionResult.Unmodelled unmodelled -> assemblies, ExportChainArrival.Unmodelled unmodelled
        | TypeResolutionResult.FirstLoadAssy loadFirst ->
            match
                tryLoadAssembly
                    loggerFactory
                    dotnetRuntimeDirs
                    assemblies.[snd loadFirst.Handle]
                    (fst loadFirst.Handle)
                    assemblies
            with
            | None -> assemblies, ExportChainArrival.AssemblyUnavailable loadFirst
            | Some (assemblies, _, _) ->

            let assemblies =
                LoadedAssemblies.assertReferenceBound $"exported type %s{ty.Name}" loadFirst assemblies

            tryWalkExportChain loggerFactory dotnetRuntimeDirs fromAssembly ty genericArgs assemblies

    /// <summary>
    /// Load what a type that <see cref="tryWalkExportChain"/> arrived at depends on, so it can be
    /// handed over; the answer is never <c>TypeAbsent</c>.
    /// </summary>
    /// <remarks>
    /// Whatever loading managed is kept in every outcome, because a guest can enumerate what is
    /// loaded.
    /// </remarks>
    let internal tryPrimeExportArrival
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (definedIn : DumpedAssembly)
        (typeDef : WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * ExportedTypeResolution
        =
        // The pure walks a caller will run on the type cannot load, so its base chain has to be
        // primed first — and priming can itself need an assembly nobody supplies. The real runtime
        // reports that the same way it reports a missing forwarder target, because it is the same
        // fact: a type this one depends on could not be loaded.
        match tryPrimeBaseChain loggerFactory dotnetRuntimeDirs assemblies definedIn typeDef with
        | assemblies, None -> assemblies, ExportedTypeResolution.Forwarded (definedIn, typeDef)
        | assemblies, Some (BaseChainFailure.LoadFailed (AssemblyLoadFailure.NoSuchAssembly reference)) ->
            assemblies, ExportedTypeResolution.AssemblyUnavailable reference
        | assemblies, Some (BaseChainFailure.BaseTypeAbsent miss) ->
            assemblies, ExportedTypeResolution.BaseTypeAbsent miss
        | assemblies, Some (BaseChainFailure.Unmodelled unmodelled) ->
            assemblies, ExportedTypeResolution.Unmodelled unmodelled
        | _, Some (BaseChainFailure.LoadFailed (AssemblyLoadFailure.LoadingNotPermitted _) as failure) ->
            // Unreachable: the loader used here reads files. A caller's mistaken belief about
            // what is already loaded is a bug in us, not a fact to report onwards.
            failwith (string<BaseChainFailure> failure)

    /// <summary>
    /// Follow <paramref name="ty" />'s forwarder chain to the assembly that declares the type,
    /// loading assemblies along the way, and report what happened rather than terminating on a
    /// chain that does not arrive anywhere.
    /// </summary>
    /// <remarks>
    /// <see cref="tryWalkExportChain"/> then <see cref="tryPrimeExportArrival"/>. The returned
    /// <c>LoadedAssemblies</c> carries every load either managed, in every outcome.
    /// </remarks>
    let internal tryResolveTypeFromExport
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (fromAssembly : DumpedAssembly)
        (ty : WoofWare.PawPrint.ExportedType)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * ExportedTypeResolution
        =
        match tryWalkExportChain loggerFactory dotnetRuntimeDirs fromAssembly ty genericArgs assemblies with
        | assemblies, ExportChainArrival.Arrived (definedIn, typeDef) ->
            tryPrimeExportArrival loggerFactory dotnetRuntimeDirs definedIn typeDef assemblies
        | assemblies, ExportChainArrival.AssemblyUnavailable reference ->
            assemblies, ExportedTypeResolution.AssemblyUnavailable reference
        | assemblies, ExportChainArrival.TypeAbsent miss -> assemblies, ExportedTypeResolution.TypeAbsent miss
        | assemblies, ExportChainArrival.Unmodelled unmodelled ->
            assemblies, ExportedTypeResolution.Unmodelled unmodelled

    /// The type a forwarder chain arrived at, or why it arrived at none.
    let exportedType
        (resolution : ExportedTypeResolution)
        : Result<DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>, TypeBindingFailure>
        =
        match resolution with
        | ExportedTypeResolution.Forwarded (assy, typeDef) -> Ok (assy, typeDef)
        | ExportedTypeResolution.AssemblyUnavailable reference ->
            Error (TypeBindingFailure.AssemblyUnavailable reference)
        | ExportedTypeResolution.TypeAbsent miss -> Error (TypeBindingFailure.TypeAbsent miss)
        | ExportedTypeResolution.BaseTypeAbsent miss -> Error (TypeBindingFailure.BaseTypeAbsent miss)
        | ExportedTypeResolution.Unmodelled unmodelled -> Error (TypeBindingFailure.Unmodelled unmodelled)

    /// A failure to follow the forwarder `ty` of `fromAssembly`, described for a run that stops on
    /// it.
    let describeExportFailure
        (ty : WoofWare.PawPrint.ExportedType)
        (fromAssembly : DumpedAssembly)
        (failure : TypeBindingFailure)
        : string
        =
        match failure with
        | TypeBindingFailure.TypeAbsent miss ->
            sprintf "Type forwarder %s from %s does not arrive: %O" ty.Name fromAssembly.DefinitionFullName miss
        | TypeBindingFailure.BaseTypeAbsent miss ->
            sprintf
                "Type forwarder %s from %s arrives at a type whose base chain is broken: %O"
                ty.Name
                fromAssembly.DefinitionFullName
                miss
        | TypeBindingFailure.AssemblyUnavailable _
        | TypeBindingFailure.Unmodelled _ -> TypeBindingFailure.describe failure

    /// The type `target`, a reference of `referencedInAssembly`, names, its base chain primed.
    let rec internal resolveTypeFromRef
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (referencedInAssembly : DumpedAssembly)
        (target : TypeRef)
        (typeGenericArgs : ImmutableArray<TypeDefn>)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * Result<DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>, TypeBindingFailure>
        =
        match LoadedTypeResolution.resolveTypeRef assemblies referencedInAssembly typeGenericArgs target with
        | TypeResolutionResult.Resolved (assy, _, typeDef) ->
            primeBaseChain loggerFactory dotnetRuntimeDirs assemblies assy typeDef
        | TypeResolutionResult.NotFound miss -> assemblies, Error (TypeBindingFailure.TypeAbsent miss)
        | TypeResolutionResult.Unmodelled unmodelled -> assemblies, Error (TypeBindingFailure.Unmodelled unmodelled)
        | TypeResolutionResult.FirstLoadAssy loadFirst ->
            match
                loadingFirst loggerFactory dotnetRuntimeDirs $"type reference %s{target.Name}" loadFirst assemblies
            with
            | Error failure -> assemblies, Error failure
            | Ok assemblies ->
                resolveTypeFromRef
                    loggerFactory
                    dotnetRuntimeDirs
                    referencedInAssembly
                    target
                    typeGenericArgs
                    assemblies

    /// A failure to bind `target`, a type reference of `referencedInAssembly`, described for a run
    /// that stops on it.
    let describeReferenceFailure
        (target : TypeRef)
        (referencedInAssembly : DumpedAssembly)
        (failure : TypeBindingFailure)
        : string
        =
        match failure with
        | TypeBindingFailure.TypeAbsent miss ->
            sprintf
                "Type reference %s from %s does not resolve: %O"
                target.Name
                referencedInAssembly.DefinitionFullName
                miss
        | TypeBindingFailure.AssemblyUnavailable _
        | TypeBindingFailure.BaseTypeAbsent _
        | TypeBindingFailure.Unmodelled _ -> TypeBindingFailure.describe failure

    /// <summary>
    /// Resolve a TypeRef to the identity of the TypeDef it names, following AssemblyRefs and type
    /// forwarders, but <i>without</i> priming the resolved type's base chain.
    /// </summary>
    /// <remarks>
    /// The result therefore does *not* satisfy the base-chain closure invariant described on
    /// <c>primeBaseChain</c>, which is why this hands back an identity rather than a
    /// <c>TypeInfo</c>: the pure walks that invariant exists for have nothing to run against.
    ///
    /// Prefer this wherever the question is only *which* type a reference names. Priming a base
    /// chain loads every assembly reachable from it, so a caller that never looks at the base type
    /// would otherwise make an unrelated assembly's availability decide whether it succeeds.
    /// CoreCLR draws the same line: <c>ClassLoader::ResolveTokenToTypeDefThrowing</c>, which its
    /// signature comparison uses, reads metadata and loads no types.
    /// </remarks>
    let rec tryResolveTypeRefIdentity
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (referencedInAssembly : DumpedAssembly)
        (target : TypeRef)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * TypeReferenceIdentity
        =
        match LoadedTypeResolution.resolveTypeRef assemblies referencedInAssembly ImmutableArray.Empty target with
        | TypeResolutionResult.Resolved (_, identity, _) -> assemblies, TypeReferenceIdentity.Resolved identity
        | TypeResolutionResult.NotFound miss -> assemblies, TypeReferenceIdentity.TypeAbsent miss
        | TypeResolutionResult.Unmodelled unmodelled -> assemblies, TypeReferenceIdentity.Unmodelled unmodelled
        | TypeResolutionResult.FirstLoadAssy loadFirst ->
            let referencing = assemblies.[snd loadFirst.Handle]

            match tryLoadAssembly loggerFactory dotnetRuntimeDirs referencing (fst loadFirst.Handle) assemblies with
            | None ->
                assemblies,
                TypeReferenceIdentity.AssemblyUnavailable referencing.AssemblyReferences.[fst loadFirst.Handle]
            | Some (assemblies, _, _) ->
                let assemblies =
                    LoadedAssemblies.assertReferenceBound $"type reference %s{target.Name}" loadFirst assemblies

                tryResolveTypeRefIdentity loggerFactory dotnetRuntimeDirs referencedInAssembly target assemblies

    let internal resolveType
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (ty : TypeReferenceHandle)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * Result<DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>, TypeBindingFailure>
        =
        let target = assy.TypeRefs.[ty]
        resolveTypeFromRef loggerFactory dotnetRuntimeDirs assy target genericArgs assemblies

    /// <summary>
    /// One generic parameter whose argument we are part-way through expanding.
    /// </summary>
    /// <remarks>
    /// Substitution here replaces a parameter with its argument and then keeps walking that
    /// argument under the <i>same</i> environment, so that an argument which itself mentions a
    /// parameter gets expanded in turn. That terminates exactly when the "parameter i's argument
    /// mentions parameter j" relation is acyclic; a cyclic environment (the degenerate case being
    /// <c>!0 := !0</c>, the interesting one <c>!0 := List&lt;!0&gt;</c>) otherwise re-expands for
    /// ever. A value of this type records one link of the expansion chain currently in flight, so
    /// that revisiting a parameter can be reported instead of recursed into.
    /// </remarks>
    [<RequireQualifiedAccess>]
    type private ExpandingParam =
        /// The type-generic parameter at this index, i.e. `!n`.
        | Type of int
        /// The method-generic parameter at this index, i.e. `!!n`.
        | Method of int

        override this.ToString () : string =
            match this with
            | ExpandingParam.Type i -> $"!%d{i}"
            | ExpandingParam.Method i -> $"!!%d{i}"

    /// <summary>
    /// <paramref name="definition" /> instantiated at <paramref name="args" />, the arguments an
    /// instantiation names for it.
    /// </summary>
    /// <remarks>
    /// There must be exactly as many arguments as the definition declares generic parameters (for
    /// a nested type, counting those it inherits from its declaring types). Otherwise CoreCLR's
    /// class loader throws <c>TypeLoadException</c> (<c>IDS_CLASSLOAD_TYPEWRONGNUMGENERICARGS</c>),
    /// which is unmodelled here.
    /// </remarks>
    let private instantiateDefinition
        (args : ImmutableArray<TypeDefn>)
        (definition : WoofWare.PawPrint.TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : Result<WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>, TypeBindingFailure>
        =
        if args.IsEmpty || args.Length <> definition.Generics.Length then
            {
                Diagnostic =
                    $"Type %s{definition.Namespace}.%s{definition.Name} in %s{definition.AssemblyFullName} declares %d{definition.Generics.Length} generic parameter(s), but an instantiation names %d{args.Length} argument(s); CoreCLR refuses that with TypeLoadException, which is not modelled"
            }
            |> TypeBindingFailure.Unmodelled
            |> Error
        else
            Ok (TypeInfo.applyGenericArgs args definition)

    /// <summary>
    /// The head of an instantiation instantiated at <paramref name="args" />, the instantiation's
    /// arguments with any parameters already substituted, without the base-chain priming
    /// <c>resolveTypeFromDefn</c> promises.
    /// </summary>
    /// <remarks>
    /// Metadata can only spell the head as the TypeDef or TypeRef of a generic definition, and any
    /// other head is unmodelled. <paramref name="assy" /> is the assembly a TypeRef head is a
    /// reference of.
    /// </remarks>
    let private instantiateHead
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (head : TypeDefn)
        (args : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * Result<DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>, TypeBindingFailure>
        =
        let instantiateIn (definedIn : DumpedAssembly) (handle : TypeDefinitionHandle) (assemblies : LoadedAssemblies) =
            match instantiateDefinition args definedIn.TypeDefs.[handle] with
            | Ok instantiated -> assemblies, Ok (definedIn, instantiated)
            | Error failure -> assemblies, Error failure

        match head with
        | TypeDefn.FromDefinition (identity, _typeKind) ->
            instantiateIn (assemblies.ByDefinitionName identity.AssemblyFullName) identity.TypeDefinition.Get assemblies
        | TypeDefn.FromReference (ref, _typeKind) ->
            match resolveTypeFromRef loggerFactory dotnetRuntimeDirs assy ref ImmutableArray.Empty assemblies with
            | assemblies, Error failure -> assemblies, Error failure
            | assemblies, Ok (resolvedIn, resolved) -> instantiateIn resolvedIn resolved.TypeDefHandle assemblies
        | head ->
            assemblies,
            Error (
                TypeBindingFailure.Unmodelled
                    {
                        Diagnostic =
                            $"An instantiation's head %O{head} is not a generic type definition named by a TypeDef or TypeRef, which is the only head a signature can spell"
                    }
            )

    /// A cyclic environment is not something metadata can express: environments are built by the
    /// interpreter, and every live path builds them out of `concreteHandleToTypeDefn`, which emits
    /// closed types only. So this is an invariant violation in the caller, not bad guest input —
    /// hence a hard failure rather than a guest exception. It exists because the alternative is a
    /// stack overflow, which .NET cannot catch, report, or contain.
    let private failCyclicEnvironment (param : ExpandingParam) (arg : TypeDefn) (expanding : Set<ExpandingParam>) : 'a =
        let chain = expanding |> Seq.map string |> String.concat ", "

        failwith
            $"TypeResolution: generic environment is cyclic. Expanding generic parameter %O{param} requires expanding %O{param} again (already expanding: %s{chain}); its argument is %O{arg}. A generic environment must be well-founded: an argument may not mention, whether directly or through the other arguments, the parameter it is bound to. Callers should be passing closed generic arguments."

    /// The argument the environment binds <paramref name="param" /> to. Guest metadata can name a
    /// parameter the environment does not hold (`List`1<!0>` read from a non-generic method, say),
    /// and CoreCLR's `SigPointer::GetTypeVariableThrowing` refuses that with
    /// <c>BadImageFormatException</c>, which here is unmodelled.
    let private argumentOf
        (typeGenericArgs : ImmutableArray<TypeDefn>)
        (methodGenericArgs : ImmutableArray<TypeDefn>)
        (param : ExpandingParam)
        : Result<TypeDefn, TypeBindingFailure>
        =
        let environment, index =
            match param with
            | ExpandingParam.Type index -> typeGenericArgs, index
            | ExpandingParam.Method index -> methodGenericArgs, index

        if 0 <= index && index < environment.Length then
            Ok environment.[index]
        else
            {
                Diagnostic =
                    $"The generic parameter %O{param} is out of range of an environment of %d{environment.Length} argument(s); CoreCLR refuses that with BadImageFormatException, which is not modelled"
            }
            |> TypeBindingFailure.Unmodelled
            |> Error

    /// <summary>
    /// What <c>substituteGenericsInTypeDefn</c> is being asked, within one fixed generic
    /// environment: a type to substitute into, and the expansion chain in flight while doing it.
    /// </summary>
    /// <remarks>
    /// <para>
    /// <c>Ty</c> is compared and hashed <i>by reference</i>, not structurally. Reference equality
    /// implies structural equality, so the memo can only miss, never lie. A <c>TypeDefn</c> is a
    /// DAG, and structural hashing of one costs its size <i>as a tree</i>: for
    /// <c>nest(d) := Dictionary&lt;nest(d-1), nest(d-1)&gt;</c> (<c>d</c> nodes as a DAG,
    /// 2^<c>d</c> as a tree), hashing the key structurally would cost as much as the traversal it
    /// is trying to avoid. Reference keying is O(1) and hits exactly on the sharing that makes
    /// the DAG small.
    /// </para>
    /// <para>
    /// <c>Expanding</c> is part of the key because it decides whether a parameter is a cycle or an
    /// ordinary expansion: the same type under a different expansion chain is a different
    /// question. It is compared structurally, which is cheap — it holds one entry per parameter
    /// expansion currently in flight.
    /// </para>
    /// <para>
    /// <c>DefinedIn</c> is the assembly a <c>FromReference</c> is resolved relative to, so it too
    /// changes the answer. Every path in this recursive group currently threads one unchanged for
    /// as long as a table lives, but nothing enforces that, and getting it wrong would hand back
    /// a type resolved against the wrong assembly rather than fail. It is compared by reference,
    /// so at worst it costs a miss.
    /// </para>
    /// </remarks>
    [<Struct ; CustomEquality ; NoComparison>]
    type private SubstitutionKey =
        {
            Ty : TypeDefn
            Expanding : Set<ExpandingParam>
            DefinedIn : DumpedAssembly
        }

        override this.Equals (other : obj) : bool =
            match other with
            | :? SubstitutionKey as other -> (this :> System.IEquatable<SubstitutionKey>).Equals other
            | _ -> false

        override this.GetHashCode () : int =
            // Both by reference: `DumpedAssembly` is a record, so structural hashing would walk a
            // whole parsed assembly, and `TypeDefn` is a DAG whose structural hash costs its size
            // as a tree — the very traversal this table exists to avoid.
            (System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode this.Ty * 397)
            ^^^ (System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode this.DefinedIn * 31)
            ^^^ hash this.Expanding

        interface System.IEquatable<SubstitutionKey> with
            member this.Equals (other : SubstitutionKey) : bool =
                System.Object.ReferenceEquals (this.Ty, other.Ty)
                && System.Object.ReferenceEquals (this.DefinedIn, other.DefinedIn)
                && this.Expanding = other.Expanding

    /// <summary>
    /// Memoised answers of <c>substituteGenericsInTypeDefn</c> for one generic environment.
    /// </summary>
    /// <remarks>
    /// <para>
    /// A table is valid only for the environment (<c>typeGenericArgs</c>,
    /// <c>methodGenericArgs</c>) it was created against, which is why the environment is not part
    /// of <c>SubstitutionKey</c>: one top-level <c>resolveTypeFromDefn</c> call threads its
    /// environment unchanged throughout, so the table travels with it. Nothing checks that, which
    /// is why nothing else about the question is left out of the key; see
    /// <c>SubstitutionKey</c>.
    /// </para>
    /// <para>
    /// Substitution also has the side effect of loading assemblies, and a memo hit skips it. That
    /// is sound because a table never outlives one top-level <c>resolveTypeFromDefn</c> call, and
    /// within one such call the <c>LoadedAssemblies</c> is threaded strictly forward: whatever a
    /// cached answer loaded when it was computed is, by construction, already loaded in every
    /// <c>LoadedAssemblies</c> the hit can be serving.
    /// </para>
    /// </remarks>
    type private SubstitutionMemo = System.Collections.Generic.Dictionary<SubstitutionKey, TypeDefn>

    /// Substitute generic parameters in a TypeDefn while preserving the structure of
    /// constructed types (arrays, pointers, byrefs): e.g. OneDimensionalArrayLowerBoundZero
    /// is preserved rather than being collapsed to System.Array. "Leaf" types
    /// (FromReference, FromDefinition, PrimitiveType) round-trip losslessly.
    ///
    /// <c>expanding</c> is the chain of parameter expansions currently in flight; see
    /// <c>ExpandingParam</c>. <c>memo</c> must have been created for this environment; see
    /// <c>SubstitutionMemo</c>.
    let rec private substituteGenericsInTypeDefn
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ty : TypeDefn)
        (typeGenericArgs : ImmutableArray<TypeDefn>)
        (methodGenericArgs : ImmutableArray<TypeDefn>)
        (expanding : Set<ExpandingParam>)
        (memo : SubstitutionMemo)
        (assy : DumpedAssembly)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * Result<TypeDefn, TypeBindingFailure>
        =
        let key =
            {
                SubstitutionKey.Ty = ty
                SubstitutionKey.Expanding = expanding
                SubstitutionKey.DefinedIn = assy
            }

        match memo.TryGetValue key with
        | true, cached -> assemblies, Ok cached
        | false, _ ->
            match
                substituteGenericsInTypeDefnUncached
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    ty
                    typeGenericArgs
                    methodGenericArgs
                    expanding
                    memo
                    assy
                    assemblies
            with
            | assemblies, Error failure -> assemblies, Error failure
            | assemblies, Ok substituted ->
                memo.[key] <- substituted
                assemblies, Ok substituted

    /// The body of <c>substituteGenericsInTypeDefn</c>, without the memo consulted or populated.
    /// Only <c>substituteGenericsInTypeDefn</c> may call this.
    and private substituteGenericsInTypeDefnUncached
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ty : TypeDefn)
        (typeGenericArgs : ImmutableArray<TypeDefn>)
        (methodGenericArgs : ImmutableArray<TypeDefn>)
        (expanding : Set<ExpandingParam>)
        (memo : SubstitutionMemo)
        (assy : DumpedAssembly)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * Result<TypeDefn, TypeBindingFailure>
        =
        let substitute (expanding : Set<ExpandingParam>) (assemblies : LoadedAssemblies) (ty : TypeDefn) =
            substituteGenericsInTypeDefn
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                ty
                typeGenericArgs
                methodGenericArgs
                expanding
                memo
                assy
                assemblies

        // The element substituted, then wrapped back up as it was.
        let wrapped (wrap : TypeDefn -> TypeDefn) (elementType : TypeDefn) =
            match substitute expanding assemblies elementType with
            | assemblies, Ok resolved -> assemblies, Ok (wrap resolved)
            | assemblies, Error failure -> assemblies, Error failure

        // `ty` resolved in the environment, read back as the TypeDefn naming the result.
        let preserved (ty : TypeDefn) (assemblies : LoadedAssemblies) =
            match
                resolveTypeFromDefnTracked
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    ty
                    typeGenericArgs
                    methodGenericArgs
                    expanding
                    memo
                    assy
                    assemblies
            with
            | assemblies, Error failure -> assemblies, Error failure
            | assemblies, Ok (_, resolvedInfo) ->
                assemblies, Ok (LoadedTypeInfo.typeInfoToTypeDefn baseClassTypes assemblies resolvedInfo)

        // A parameter replaced by its argument, which is substituted in turn.
        let expand (link : ExpandingParam) =
            match argumentOf typeGenericArgs methodGenericArgs link with
            | Error failure -> assemblies, Error failure
            | Ok arg ->
                if Set.contains link expanding then
                    failCyclicEnvironment link arg expanding

                substitute (Set.add link expanding) assemblies arg

        match ty with
        | TypeDefn.GenericTypeParameter idx -> expand (ExpandingParam.Type idx)
        | TypeDefn.GenericMethodParameter idx -> expand (ExpandingParam.Method idx)
        | TypeDefn.OneDimensionalArrayLowerBoundZero elementType ->
            wrapped TypeDefn.OneDimensionalArrayLowerBoundZero elementType
        | TypeDefn.Array (elementType, rank) -> wrapped (fun resolved -> TypeDefn.Array (resolved, rank)) elementType
        | TypeDefn.Pointer elementType -> wrapped TypeDefn.Pointer elementType
        | TypeDefn.Byref elementType -> wrapped TypeDefn.Byref elementType
        | TypeDefn.GenericInstantiation (generic, args) ->
            match traverseLoaded (substitute expanding) assemblies args with
            | assemblies, Error failure -> assemblies, Error failure
            | assemblies, Ok args' ->
                match instantiateHead loggerFactory dotnetRuntimeDirs generic args' assy assemblies with
                | assemblies, Error failure -> assemblies, Error failure
                | assemblies, Ok (resolvedIn, instantiated) ->
                    match primeBaseChain loggerFactory dotnetRuntimeDirs assemblies resolvedIn instantiated with
                    | assemblies, Error failure -> assemblies, Error failure
                    | assemblies, Ok (_, primed) ->
                        assemblies, Ok (LoadedTypeInfo.typeInfoToTypeDefn baseClassTypes assemblies primed)
        | TypeDefn.FromReference _
        | TypeDefn.FromDefinition _
        | TypeDefn.PrimitiveType _
        | TypeDefn.Void ->
            match preserved ty assemblies with
            | assemblies, Ok (TypeDefn.GenericInstantiation (head, _)) ->
                // A generic definition spelled bare names itself, open (see `resolveTypeFromDefn`),
                // and an open definition is not a type an instantiation can be at. CoreCLR's
                // `SigPointer::GetTypeHandleThrowing` loads such an argument with
                // `FailIfUninstDefOrRef`, and so throws `TypeLoadException`, whatever the
                // environment.
                assemblies,
                Error (
                    TypeBindingFailure.Unmodelled
                        {
                            Diagnostic =
                                $"An instantiation's argument %O{ty} names the generic definition %O{head} without instantiating it; CoreCLR refuses that with TypeLoadException, which is not modelled"
                        }
                )
            | resolved -> resolved
        | other -> preserved other assemblies

    /// The body of <c>resolveTypeFromDefn</c>, carrying the chain of parameter expansions
    /// currently in flight. The public entry point starts that chain empty; everything inside
    /// this recursive group must thread it, because the cycle it guards against runs through
    /// here.
    and private resolveTypeFromDefnTracked
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ty : TypeDefn)
        (typeGenericArgs : ImmutableArray<TypeDefn>)
        (methodGenericArgs : ImmutableArray<TypeDefn>)
        (expanding : Set<ExpandingParam>)
        (memo : SubstitutionMemo)
        (assy : DumpedAssembly)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * Result<DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>, TypeBindingFailure>
        =
        match
            resolveTypeFromDefnUnprimed
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                ty
                typeGenericArgs
                methodGenericArgs
                expanding
                memo
                assy
                assemblies
        with
        | assemblies, Error failure -> assemblies, Error failure
        | assemblies, Ok (resolvedIn, resolved) ->
            primeBaseChain loggerFactory dotnetRuntimeDirs assemblies resolvedIn resolved

    /// The body of <c>resolveTypeFromDefn</c>, without the base-chain priming its contract
    /// promises. Only <c>resolveTypeFromDefnTracked</c> may call this.
    and private resolveTypeFromDefnUnprimed
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ty : TypeDefn)
        (typeGenericArgs : ImmutableArray<TypeDefn>)
        (methodGenericArgs : ImmutableArray<TypeDefn>)
        (expanding : Set<ExpandingParam>)
        (memo : SubstitutionMemo)
        (assy : DumpedAssembly)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * Result<DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>, TypeBindingFailure>
        =
        let unmodelled (diagnostic : string) =
            assemblies,
            Error (
                TypeBindingFailure.Unmodelled
                    {
                        Diagnostic = diagnostic
                    }
            )

        // A parameter resolved as its argument.
        let resolveParameter (link : ExpandingParam) =
            match argumentOf typeGenericArgs methodGenericArgs link with
            | Error failure -> assemblies, Error failure
            | Ok arg ->
                if Set.contains link expanding then
                    failCyclicEnvironment link arg expanding

                // TODO: this assembly is probably wrong?
                resolveTypeFromDefnTracked
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    arg
                    typeGenericArgs
                    methodGenericArgs
                    (Set.add link expanding)
                    memo
                    assy
                    assemblies

        match ty with
        | TypeDefn.GenericInstantiation (generic, args) ->
            let substitute (assemblies : LoadedAssemblies) (arg : TypeDefn) =
                substituteGenericsInTypeDefn
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    arg
                    typeGenericArgs
                    methodGenericArgs
                    expanding
                    memo
                    assy
                    assemblies

            match traverseLoaded substitute assemblies args with
            | assemblies, Error failure -> assemblies, Error failure
            | assemblies, Ok args' -> instantiateHead loggerFactory dotnetRuntimeDirs generic args' assy assemblies
        | TypeDefn.FromDefinition (identity, _typeKind) ->
            let definedIn = assemblies.ByDefinitionName identity.AssemblyFullName

            assemblies,
            Ok (
                definedIn,
                TypeInfo.applyGenericArgs ImmutableArray.Empty definedIn.TypeDefs.[identity.TypeDefinition.Get]
            )
        | TypeDefn.FromReference (ref, _typeKind) ->
            resolveTypeFromRef loggerFactory dotnetRuntimeDirs assy ref ImmutableArray.Empty assemblies
        | TypeDefn.PrimitiveType prim ->
            let primitive =
                match prim with
                | PrimitiveType.Boolean -> Some baseClassTypes.Boolean
                | PrimitiveType.Char -> Some baseClassTypes.Char
                | PrimitiveType.SByte -> Some baseClassTypes.SByte
                | PrimitiveType.Byte -> Some baseClassTypes.Byte
                | PrimitiveType.Int16 -> Some baseClassTypes.Int16
                | PrimitiveType.UInt16 -> Some baseClassTypes.UInt16
                | PrimitiveType.Int32 -> Some baseClassTypes.Int32
                | PrimitiveType.UInt32 -> Some baseClassTypes.UInt32
                | PrimitiveType.Int64 -> Some baseClassTypes.Int64
                | PrimitiveType.UInt64 -> Some baseClassTypes.UInt64
                | PrimitiveType.Single -> Some baseClassTypes.Single
                | PrimitiveType.Double -> Some baseClassTypes.Double
                | PrimitiveType.String -> Some baseClassTypes.String
                | PrimitiveType.TypedReference -> None
                | PrimitiveType.IntPtr -> Some baseClassTypes.IntPtr
                | PrimitiveType.UIntPtr -> Some baseClassTypes.UIntPtr
                | PrimitiveType.Object -> Some baseClassTypes.Object

            match primitive with
            | None -> unmodelled "TODO: resolveTypeFromDefn of System.TypedReference is not implemented"
            | Some ty ->
                let ty =
                    ty |> TypeInfo.mapGeneric (fun _ -> failwith "none of these types are generic")

                assemblies, Ok (baseClassTypes.Corelib, ty)
        // `ELEMENT_TYPE_VOID` in a type position, as a TypeSpec may spell it, names CoreLib's
        // `System.Void` as the primitive element types name theirs.
        | TypeDefn.Void ->
            let ty =
                baseClassTypes.Void
                |> TypeInfo.mapGeneric (fun _ -> failwith "System.Void is not generic")

            assemblies, Ok (baseClassTypes.Corelib, ty)
        | TypeDefn.GenericTypeParameter param -> resolveParameter (ExpandingParam.Type param)
        | TypeDefn.GenericMethodParameter param -> resolveParameter (ExpandingParam.Method param)
        | TypeDefn.OneDimensionalArrayLowerBoundZero _
        | TypeDefn.Array _ ->
            // This is lossy: we return System.Array's TypeInfo, discarding the element type.
            // Callers that need precise array type identity (e.g. Ldtoken) should use
            // concretizeType directly instead of going through this function.
            let arrayTy =
                baseClassTypes.Array
                |> TypeInfo.mapGeneric (fun _ -> failwith "System.Array is not generic")

            assemblies, Ok (baseClassTypes.Corelib, arrayTy)
        | TypeDefn.Pointer _
        | TypeDefn.Byref _
        | TypeDefn.Pinned _ ->
            unmodelled
                $"TODO: resolveTypeFromDefn cannot faithfully represent pointer/byref/pinned types as TypeInfo. Caller should handle these wrapper types before calling resolveTypeFromDefn. Got: {ty}"
        | s -> unmodelled $"TODO: resolveTypeFromDefn unimplemented for {s}"

    /// <summary>
    /// Resolve a TypeDefn to the metadata of the type it names, loading assemblies as required.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The returned TypeInfo satisfies the base-chain closure invariant described on
    /// <c>primeBaseChain</c>: every assembly reachable from its base-type chain is loaded in the
    /// returned load context, so the caller may run the pure walks (<c>isValueType</c>,
    /// <c>signatureTypeKind</c>, <c>typeInfoToTypeDefn</c>) over it.
    /// </para>
    /// <para>
    /// A bare <c>FromDefinition</c> or <c>FromReference</c> names the definition itself, so a
    /// generic one comes back open, at its own parameters, whatever the environment. An
    /// instantiation names exactly as many arguments as its definition declares parameters, none
    /// of them a generic definition spelled bare; CoreCLR refuses either with
    /// <c>TypeLoadException</c>, and here either is unmodelled. So is a generic parameter that
    /// <paramref name="typeGenericArgs" /> or <paramref name="methodGenericArgs" /> does not hold,
    /// which CoreCLR refuses with <c>BadImageFormatException</c>.
    /// </para>
    /// <para>
    /// <paramref name="typeGenericArgs" /> and <paramref name="methodGenericArgs" /> must be
    /// well-founded: no argument may mention, whether directly or through the other arguments,
    /// the parameter it is bound to. Passing closed arguments — which is what every path through
    /// <c>concreteHandleToTypeDefn</c> produces — satisfies this trivially. A cyclic environment
    /// is diagnosed and raised on rather than recursed into; see <c>ExpandingParam</c>.
    /// </para>
    /// <para>
    /// The cost is the size of <paramref name="ty" /> as a <i>DAG</i>, not as a tree: a subterm
    /// physically shared between several argument positions is substituted once and the answer
    /// reused, and the answer shares in the same way. A type like
    /// <c>nest(d) := Dictionary&lt;nest(d-1), nest(d-1)&gt;</c> is <c>d</c> nodes shared and
    /// 2^<c>d</c> unshared, so callers that build types by repeated instantiation should hand the
    /// same object to each position that wants the same type, rather than an equal copy. Doing
    /// otherwise costs more but cannot change the answer; see <c>SubstitutionMemo</c>.
    /// </para>
    /// </remarks>
    let resolveTypeFromDefn
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ty : TypeDefn)
        (typeGenericArgs : ImmutableArray<TypeDefn>)
        (methodGenericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * Result<DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>, TypeBindingFailure>
        =
        resolveTypeFromDefnTracked
            loggerFactory
            dotnetRuntimeDirs
            baseClassTypes
            ty
            typeGenericArgs
            methodGenericArgs
            Set.empty
            (SubstitutionMemo ())
            assy
            assemblies

    /// <c>resolveTypeFromDefn</c> of the signature of <paramref name="ty" />, a TypeSpec of
    /// <paramref name="assy" />.
    let resolveTypeFromSpec
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ty : TypeSpecificationHandle)
        (assy : DumpedAssembly)
        (typeGenericArgs : TypeDefn ImmutableArray)
        (methodGenericArgs : TypeDefn ImmutableArray)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * Result<DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>, TypeBindingFailure>
        =
        let sign = assy.TypeSpecs.[ty].Signature

        resolveTypeFromDefn
            loggerFactory
            dotnetRuntimeDirs
            baseClassTypes
            sign
            typeGenericArgs
            methodGenericArgs
            assy
            assemblies
