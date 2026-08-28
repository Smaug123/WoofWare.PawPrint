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

/// Functions for resolving type metadata (TypeRefs, TypeDefs, TypeSpecs) to concrete TypeInfo values.
/// Operates on the loaded-assemblies dictionary directly, without requiring IlMachineState.
[<RequireQualifiedAccess>]
module TypeResolution =

    type private Dummy = class end

    /// <summary>
    /// Bind an AssemblyReference to an assembly, loading it from the runtime dirs if this is the
    /// first time we have needed it. Returns the updated load context, the assembly, and the
    /// assembly's own *definition* identity — which is in general NOT the reference's identity,
    /// so callers must not assume the returned name is what they asked for.
    /// </summary>
    /// <remarks>
    /// Binding is by simple name: we look for <c>&lt;SimpleName&gt;.dll</c> in each runtime dir in
    /// turn. Version, culture and public key token in the reference are therefore not honoured,
    /// which is exactly why the reference's identity so often disagrees with the definition
    /// identity of what it binds to (the .NET Framework compatibility facades reference
    /// implementation assemblies as <c>Version=0.0.0.0</c>).
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
            let assemblyName = assemblyRef.Name
            let logger = loggerFactory.CreateLogger typeof<Dummy>.DeclaringType

            // `tryPick`, not `choose |> tryHead`: the first hit is the binding, so every
            // later dir must go unread. Reading them anyway is not merely wasted parsing
            // (though it is that too, and a runtime dir list often holds a whole second
            // framework) — it lets a directory we were never going to bind against fail
            // the load, because anything but FileNotFoundException escapes.
            let assy =
                dotnetRuntimeDirs
                |> Seq.tryPick (fun dir ->
                    let file = Path.Combine (dir, assemblyName.Name + ".dll")

                    try
                        logger.LogInformation ("Loading assembly from file {AssemblyFileLoadPath}", file)
                        Assembly.readFile loggerFactory file |> Some
                    with :? FileNotFoundException ->
                        None
                )

            match assy with
            | None -> None
            | Some assy ->
                // Record both the assembly (under its own definition identity) and the binding
                // that got us here, so the next probe with this reference identity is a hit.
                let assemblies, canonical = assemblies.WithBoundReference assemblyRef assy
                Some (assemblies, canonical, canonical.Name)

    /// <summary>
    /// As <see cref="tryLoadAssembly"/>, for the majority of callers that have no way to report a
    /// failed bind and so must terminate on one.
    /// </summary>
    let internal loadAssembly
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (referencedInAssembly : DumpedAssembly)
        (r : AssemblyReferenceHandle)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * DumpedAssembly * AssemblyName
        =
        match tryLoadAssembly loggerFactory dotnetRuntimeDirs referencedInAssembly r assemblies with
        | Some loaded -> loaded
        | None ->
            let assemblyName = referencedInAssembly.AssemblyReferences.[r].Name
            failwith $"Could not find a readable DLL in any runtime dir with name %s{assemblyName.Name}.dll"

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
    /// <c>DumpedAssembly.isValueType</c>, <c>signatureTypeKind</c> and <c>typeInfoToTypeDefn</c>
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
    let private tryPrimeBaseChain
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (assemblies : LoadedAssemblies)
        (definedIn : DumpedAssembly)
        (ty : WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>)
        : LoadedAssemblies * BaseChainFailure option
        =
        Concretization.tryEnsureTypeDefinitionBaseAssembliesLoaded
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
        : LoadedAssemblies
        =
        match tryPrimeBaseChain loggerFactory dotnetRuntimeDirs assemblies definedIn ty with
        | assemblies, None -> assemblies
        | _, Some failure -> failwith (string<BaseChainFailure> failure)

    let rec internal resolveTopLevelTypeFromName
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (ns : string option)
        (name : string)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        match Assembly.resolveTopLevelTypeFromName assy assemblies ns name genericArgs with
        | TypeResolutionResult.Resolved (assy, _, typeDef) ->
            primeBaseChain loggerFactory dotnetRuntimeDirs assemblies assy typeDef, assy, typeDef
        | TypeResolutionResult.NotFound miss -> failwithf "Top-level type resolution failed: %O" miss
        | TypeResolutionResult.FirstLoadAssy loadFirst ->
            let assemblies, _, _ =
                loadAssembly
                    loggerFactory
                    dotnetRuntimeDirs
                    assemblies.[snd loadFirst.Handle]
                    (fst loadFirst.Handle)
                    assemblies

            let assemblies =
                LoadedAssemblies.assertReferenceBound $"top-level type %s{name}" loadFirst assemblies

            resolveTopLevelTypeFromName loggerFactory dotnetRuntimeDirs ns name genericArgs assy assemblies

    /// <summary>
    /// Follow <paramref name="ty" />'s forwarder chain to the assembly that declares the type,
    /// loading assemblies along the way, and report what happened rather than terminating on a
    /// chain that does not arrive anywhere.
    /// </summary>
    /// <remarks>
    /// The returned <c>LoadedAssemblies</c> carries every load the walk did manage, in every
    /// outcome: a chain that binds two assemblies and then fails on a third must not discard the
    /// two.
    /// </remarks>
    let rec internal tryResolveTypeFromExport
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (fromAssembly : DumpedAssembly)
        (ty : WoofWare.PawPrint.ExportedType)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * ExportedTypeResolution
        =
        match Assembly.resolveTypeFromExport fromAssembly assemblies genericArgs ty with
        | TypeResolutionResult.Resolved (assy, _, typeDef) ->
            // Arriving at the type is not enough to hand it over: the pure walks a caller will run
            // on it cannot load, so its base chain has to be primed first — and priming can itself
            // need an assembly nobody supplies. The real runtime reports that the same way it
            // reports a missing forwarder target, because it is the same fact: a type this one
            // depends on could not be loaded.
            // Whatever priming managed is kept in every arm below: a chain that loads two
            // assemblies and then fails on a third must not lose the two, because a guest can
            // enumerate what is loaded.
            match tryPrimeBaseChain loggerFactory dotnetRuntimeDirs assemblies assy typeDef with
            | assemblies, None -> assemblies, ExportedTypeResolution.Forwarded (assy, typeDef)
            | assemblies, Some (BaseChainFailure.LoadFailed (AssemblyLoadFailure.NoSuchAssembly reference)) ->
                assemblies, ExportedTypeResolution.AssemblyUnavailable reference
            | assemblies, Some (BaseChainFailure.BaseTypeAbsent miss) ->
                assemblies, ExportedTypeResolution.BaseTypeAbsent miss
            | _, Some (BaseChainFailure.LoadFailed (AssemblyLoadFailure.LoadingNotPermitted _) as failure) ->
                // Unreachable: the loader used here reads files. A caller's mistaken belief about
                // what is already loaded is a bug in us, not a fact to report onwards.
                failwith (string<BaseChainFailure> failure)
        | TypeResolutionResult.NotFound miss -> assemblies, ExportedTypeResolution.TypeAbsent miss
        | TypeResolutionResult.FirstLoadAssy loadFirst ->
            match
                tryLoadAssembly
                    loggerFactory
                    dotnetRuntimeDirs
                    assemblies.[snd loadFirst.Handle]
                    (fst loadFirst.Handle)
                    assemblies
            with
            | None -> assemblies, ExportedTypeResolution.AssemblyUnavailable loadFirst
            | Some (assemblies, _, _) ->

            let assemblies =
                LoadedAssemblies.assertReferenceBound $"exported type %s{ty.Name}" loadFirst assemblies

            tryResolveTypeFromExport loggerFactory dotnetRuntimeDirs fromAssembly ty genericArgs assemblies

    /// <summary>
    /// As <see cref="tryResolveTypeFromExport"/>, for callers that have no way to report a chain
    /// which does not arrive anywhere and so must terminate on one.
    /// </summary>
    let internal resolveTypeFromExport
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (fromAssembly : DumpedAssembly)
        (ty : WoofWare.PawPrint.ExportedType)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        match tryResolveTypeFromExport loggerFactory dotnetRuntimeDirs fromAssembly ty genericArgs assemblies with
        | assemblies, ExportedTypeResolution.Forwarded (assy, typeDef) -> assemblies, assy, typeDef
        | _, ExportedTypeResolution.AssemblyUnavailable reference ->
            failwith $"Could not find a readable DLL in any runtime dir with name %s{reference.Name.Name}.dll"
        | _, ExportedTypeResolution.TypeAbsent miss ->
            failwithf "Type forwarder %s from %s does not arrive: %O" ty.Name fromAssembly.Name.FullName miss
        | _, ExportedTypeResolution.BaseTypeAbsent miss ->
            failwithf
                "Type forwarder %s from %s arrives at a type whose base chain is broken: %O"
                ty.Name
                fromAssembly.Name.FullName
                miss

    let rec internal resolveTypeFromRef
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (referencedInAssembly : DumpedAssembly)
        (target : TypeRef)
        (typeGenericArgs : ImmutableArray<TypeDefn>)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        match Assembly.resolveTypeRef assemblies referencedInAssembly typeGenericArgs target with
        | TypeResolutionResult.Resolved (assy, _, typeDef) ->
            primeBaseChain loggerFactory dotnetRuntimeDirs assemblies assy typeDef, assy, typeDef
        | TypeResolutionResult.NotFound miss ->
            failwithf
                "Type reference %s from %s does not resolve: %O"
                target.Name
                referencedInAssembly.Name.FullName
                miss
        | TypeResolutionResult.FirstLoadAssy loadFirst ->
            let assemblies, _, _ =
                loadAssembly
                    loggerFactory
                    dotnetRuntimeDirs
                    assemblies.[snd loadFirst.Handle]
                    (fst loadFirst.Handle)
                    assemblies

            let assemblies =
                LoadedAssemblies.assertReferenceBound $"type reference %s{target.Name}" loadFirst assemblies

            resolveTypeFromRef loggerFactory dotnetRuntimeDirs referencedInAssembly target typeGenericArgs assemblies

    let internal resolveType
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (ty : TypeReferenceHandle)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
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

    /// A cyclic environment is not something metadata can express: environments are built by the
    /// interpreter, and every live path builds them out of `concreteHandleToTypeDefn`, which emits
    /// closed types only. So this is an invariant violation in the caller, not bad guest input —
    /// hence a hard failure rather than a guest exception. It exists because the alternative is a
    /// stack overflow, which .NET cannot catch, report, or contain.
    let private failCyclicEnvironment (param : ExpandingParam) (arg : TypeDefn) (expanding : Set<ExpandingParam>) : 'a =
        let chain = expanding |> Seq.map string |> String.concat ", "

        failwith
            $"TypeResolution: generic environment is cyclic. Expanding generic parameter %O{param} requires expanding %O{param} again (already expanding: %s{chain}); its argument is %O{arg}. A generic environment must be well-founded: an argument may not mention, whether directly or through the other arguments, the parameter it is bound to. Callers should be passing closed generic arguments."

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
    /// of <c>SubstitutionKey</c>: a fresh table is made at the one place the environment changes,
    /// namely the head resolution of a <c>GenericInstantiation</c> in
    /// <c>resolveTypeFromDefnUnprimed</c>. Everywhere else the environment is threaded unchanged,
    /// so the table travels with it. Nothing checks that, which is why nothing else about the
    /// question is left out of the key; see <c>SubstitutionKey</c>.
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
        : LoadedAssemblies * TypeDefn
        =
        let key =
            {
                SubstitutionKey.Ty = ty
                SubstitutionKey.Expanding = expanding
                SubstitutionKey.DefinedIn = assy
            }

        match memo.TryGetValue key with
        | true, cached -> assemblies, cached
        | false, _ ->
            let assemblies, substituted =
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

            memo.[key] <- substituted
            assemblies, substituted

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
        : LoadedAssemblies * TypeDefn
        =
        match ty with
        | TypeDefn.GenericTypeParameter idx ->
            let link = ExpandingParam.Type idx

            if Set.contains link expanding then
                failCyclicEnvironment link typeGenericArgs.[idx] expanding

            substituteGenericsInTypeDefn
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                typeGenericArgs.[idx]
                typeGenericArgs
                methodGenericArgs
                (Set.add link expanding)
                memo
                assy
                assemblies
        | TypeDefn.GenericMethodParameter idx ->
            let link = ExpandingParam.Method idx

            if Set.contains link expanding then
                failCyclicEnvironment link methodGenericArgs.[idx] expanding

            substituteGenericsInTypeDefn
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                methodGenericArgs.[idx]
                typeGenericArgs
                methodGenericArgs
                (Set.add link expanding)
                memo
                assy
                assemblies
        | TypeDefn.OneDimensionalArrayLowerBoundZero elementType ->
            let assemblies, resolved =
                substituteGenericsInTypeDefn
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    elementType
                    typeGenericArgs
                    methodGenericArgs
                    expanding
                    memo
                    assy
                    assemblies

            assemblies, TypeDefn.OneDimensionalArrayLowerBoundZero resolved
        | TypeDefn.Array (elementType, rank) ->
            let assemblies, resolved =
                substituteGenericsInTypeDefn
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    elementType
                    typeGenericArgs
                    methodGenericArgs
                    expanding
                    memo
                    assy
                    assemblies

            assemblies, TypeDefn.Array (resolved, rank)
        | TypeDefn.Pointer elementType ->
            let assemblies, resolved =
                substituteGenericsInTypeDefn
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    elementType
                    typeGenericArgs
                    methodGenericArgs
                    expanding
                    memo
                    assy
                    assemblies

            assemblies, TypeDefn.Pointer resolved
        | TypeDefn.Byref elementType ->
            let assemblies, resolved =
                substituteGenericsInTypeDefn
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    elementType
                    typeGenericArgs
                    methodGenericArgs
                    expanding
                    memo
                    assy
                    assemblies

            assemblies, TypeDefn.Byref resolved
        | TypeDefn.GenericInstantiation (generic, args) ->
            // Substitute the arguments, then resolve the head *under those arguments* and convert
            // back with typeInfoToTypeDefn. Resolving the head canonicalises it (a TypeRef
            // becomes the TypeDef it binds to, in the assembly it actually binds to) and loads
            // whatever assembly that names; the structural recursion above keeps arrays,
            // pointers and byrefs in the arguments intact rather than collapsing them.
            //
            // We resolve `generic` directly rather than re-entering resolveTypeFromDefn on the
            // reassembled `GenericInstantiation (generic, args')`. Re-entering would substitute
            // `args'` a second time; that cannot change the answer (substitution is idempotent on
            // its own output, and `args'` is closed whenever the environment is well-founded,
            // this function's stated precondition), but it re-traverses and re-allocates the
            // whole argument subtree at every nesting level, and the fresh nodes defeat the
            // reference-keyed memo, leaving the blowup exponential.
            let builder = ImmutableArray.CreateBuilder args.Length

            let assemblies =
                (assemblies, args)
                ||> Seq.fold (fun assemblies arg ->
                    let assemblies, resolved =
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

                    builder.Add resolved
                    assemblies
                )

            let args' = builder.ToImmutable ()

            // `args'` replaces `typeGenericArgs` as the environment the head is resolved in, so the
            // memo — which is only valid for one environment — must not travel with it.
            let assemblies, _assy, resolvedInfo =
                resolveTypeFromDefnTracked
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    generic
                    args'
                    methodGenericArgs
                    expanding
                    (SubstitutionMemo ())
                    assy
                    assemblies

            let preserved =
                DumpedAssembly.typeInfoToTypeDefn baseClassTypes assemblies resolvedInfo

            assemblies, preserved
        | TypeDefn.FromReference _
        | TypeDefn.FromDefinition _
        | TypeDefn.PrimitiveType _
        | TypeDefn.Void ->
            // Leaf types: resolve for side effects (assembly loading) and convert back.
            // The round-trip through TypeInfo is lossless for these cases.
            let assemblies, _assy, resolvedInfo =
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

            let preserved =
                DumpedAssembly.typeInfoToTypeDefn baseClassTypes assemblies resolvedInfo

            match preserved with
            | TypeDefn.GenericInstantiation (head, injected) ->
                // `ty` named a type *definition*, and that definition turned out to be generic —
                // a shape no signature can spell, and which the resolver reads as "this
                // definition, instantiated at the ambient environment". So resolution has just
                // put arguments into our answer that it took straight out of `typeGenericArgs`
                // by sequence number, and substitution has not been applied to any of them.
                //
                // The environment is only required to be well-founded, not closed, so those raw
                // entries can perfectly well mention parameters: under `!0 := !1`, `!1 := int`
                // this is the difference between answering `List<int>` and answering `List<!1>`,
                // an open type that then cannot be resolved again on its own. Expanding parameter
                // `k` under this environment is exactly what substituting `GenericTypeParameter k`
                // does — cycle guard, memo and all — so ask for that rather than reimplementing
                // it, and the answer is closed.
                //
                // The instantiation case above needs no such fix-up, and must not be given one:
                // the environment it resolves its head under is `args'`, which is this function's
                // own output and so already closed by the same induction. Substituting it again
                // is precisely the redundant second pass, and it is exponential.
                let builder = ImmutableArray.CreateBuilder injected.Length

                let assemblies =
                    (assemblies, Seq.init injected.Length id)
                    ||> Seq.fold (fun assemblies k ->
                        let assemblies, substituted =
                            substituteGenericsInTypeDefn
                                loggerFactory
                                dotnetRuntimeDirs
                                baseClassTypes
                                (TypeDefn.GenericTypeParameter k)
                                typeGenericArgs
                                methodGenericArgs
                                expanding
                                memo
                                assy
                                assemblies

                        builder.Add substituted
                        assemblies
                    )

                assemblies, TypeDefn.GenericInstantiation (head, builder.ToImmutable ())
            | preserved -> assemblies, preserved
        | other ->
            // For any other TypeDefn variant, resolve and convert back.
            let assemblies, _assy, resolvedInfo =
                resolveTypeFromDefnTracked
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    other
                    typeGenericArgs
                    methodGenericArgs
                    expanding
                    memo
                    assy
                    assemblies

            let preserved =
                DumpedAssembly.typeInfoToTypeDefn baseClassTypes assemblies resolvedInfo

            assemblies, preserved

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
        : LoadedAssemblies * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        let assemblies, resolvedIn, resolved =
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

        primeBaseChain loggerFactory dotnetRuntimeDirs assemblies resolvedIn resolved, resolvedIn, resolved

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
        : LoadedAssemblies * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        match ty with
        | TypeDefn.GenericInstantiation (generic, args) ->
            let builder = ImmutableArray.CreateBuilder args.Length

            let assemblies =
                (assemblies, args)
                ||> Seq.fold (fun assemblies arg ->
                    let assemblies, preservedArg =
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

                    builder.Add preservedArg

                    assemblies
                )

            let args' = builder.ToImmutable ()

            // `args'` replaces `typeGenericArgs` as the environment `generic` is resolved in, so
            // in principle the chain's type-parameter links now name parameters of an environment
            // we have left. We carry them anyway: `generic` is the head of an instantiation, which
            // metadata can only ever spell as a TypeDef or TypeRef, and resolving either consults
            // no parameter and so reaches no guard. Clearing the links would only change the
            // answer for a parameter-headed instantiation — a shape no assembly can contain, and
            // one whose environment is cyclic in any case.
            //
            // The memo, by contrast, must *not* be carried: it is keyed on the type alone, and is
            // therefore only meaningful for the environment it was built against, which we have
            // just left.
            resolveTypeFromDefnTracked
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                generic
                args'
                methodGenericArgs
                expanding
                (SubstitutionMemo ())
                assy
                assemblies
        | TypeDefn.FromDefinition (identity, _typeKind) ->
            let assy = assemblies.ByDefinitionName identity.AssemblyFullName

            let defn =
                assy.TypeDefs.[identity.TypeDefinition.Get]
                |> TypeInfo.mapGeneric (fun (param, _) -> typeGenericArgs.[param.SequenceNumber])

            assemblies, assy, defn
        | TypeDefn.FromReference (ref, _typeKind) ->
            let assemblies, assy, ty =
                resolveTypeFromRef loggerFactory dotnetRuntimeDirs assy ref typeGenericArgs assemblies

            assemblies, assy, ty
        | TypeDefn.PrimitiveType prim ->
            let ty =
                match prim with
                | PrimitiveType.Boolean -> baseClassTypes.Boolean
                | PrimitiveType.Char -> baseClassTypes.Char
                | PrimitiveType.SByte -> baseClassTypes.SByte
                | PrimitiveType.Byte -> baseClassTypes.Byte
                | PrimitiveType.Int16 -> baseClassTypes.Int16
                | PrimitiveType.UInt16 -> baseClassTypes.UInt16
                | PrimitiveType.Int32 -> baseClassTypes.Int32
                | PrimitiveType.UInt32 -> baseClassTypes.UInt32
                | PrimitiveType.Int64 -> baseClassTypes.Int64
                | PrimitiveType.UInt64 -> baseClassTypes.UInt64
                | PrimitiveType.Single -> baseClassTypes.Single
                | PrimitiveType.Double -> baseClassTypes.Double
                | PrimitiveType.String -> baseClassTypes.String
                | PrimitiveType.TypedReference -> failwith "todo"
                | PrimitiveType.IntPtr -> baseClassTypes.IntPtr
                | PrimitiveType.UIntPtr -> baseClassTypes.UIntPtr
                | PrimitiveType.Object -> baseClassTypes.Object
                |> TypeInfo.mapGeneric (fun _ -> failwith "none of these types are generic")

            assemblies, baseClassTypes.Corelib, ty
        | TypeDefn.GenericTypeParameter param ->
            let arg = typeGenericArgs.[param]
            let link = ExpandingParam.Type param

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
        | TypeDefn.GenericMethodParameter param ->
            let arg = methodGenericArgs.[param]
            let link = ExpandingParam.Method param

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
        | TypeDefn.OneDimensionalArrayLowerBoundZero _
        | TypeDefn.Array _ ->
            // This is lossy: we return System.Array's TypeInfo, discarding the element type.
            // Callers that need precise array type identity (e.g. Ldtoken) should use
            // concretizeType directly instead of going through this function.
            let arrayTy =
                baseClassTypes.Array
                |> TypeInfo.mapGeneric (fun _ -> failwith "System.Array is not generic")

            assemblies, baseClassTypes.Corelib, arrayTy
        | TypeDefn.Pointer _
        | TypeDefn.Byref _
        | TypeDefn.Pinned _ ->
            failwith
                $"TODO: resolveTypeFromDefn cannot faithfully represent pointer/byref/pinned types as TypeInfo. Caller should handle these wrapper types before calling resolveTypeFromDefn. Got: {ty}"
        | s -> failwith $"TODO: resolveTypeFromDefn unimplemented for {s}"

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
        : LoadedAssemblies * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
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

    let resolveTypeFromSpec
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ty : TypeSpecificationHandle)
        (assy : DumpedAssembly)
        (typeGenericArgs : TypeDefn ImmutableArray)
        (methodGenericArgs : TypeDefn ImmutableArray)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
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
