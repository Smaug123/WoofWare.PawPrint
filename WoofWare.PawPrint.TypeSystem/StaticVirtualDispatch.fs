namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging

/// Which method a `constrained.` call of a static virtual interface method runs, given the type the
/// prefix names, as CoreCLR's `MethodTable::ResolveVirtualStaticMethod` (methodtable.cpp) decides
/// it for such a call, variance allowed.
///
/// A static virtual has no slot, so none of virtual dispatch's tables apply, and nothing matches it
/// by name and signature: it is implemented only by a MethodImpl naming it, or by its own body. In
/// order:
///
/// 1. each type from the constrained type up its base chain, asked for a MethodImpl naming the
///    method on exactly the call's interface instantiation, and then, if that interface is
///    variant, on the first variance-compatible instantiation in the type's interface map;
/// 2. the most specific default body among the constrained type and its interfaces: the method's
///    own body on its interface, or a MethodImpl on a more specific interface. Exactly the call's
///    instantiation is looked for first, and then any variance-compatible one, whose body runs as
///    that instantiation's (`FindDefaultInterfaceImplementation`). Two equally specific bodies for
///    exactly the call's instantiation are a conflict, but of two for variance-compatible ones the
///    first runs. The search meets interfaces in order (the constrained type itself if it is an
///    interface, then each type's interface map from the constrained type up its base chain), and
///    a more specific body takes the place of the first one it displaces.
///
/// CoreCLR falls back to the method's own body after that, which no type admitting the interface
/// reaches: the search in 2 finds that body wherever the interface or a compatible instantiation of
/// it is in the type's interface map.
///
/// Whatever loads an assembly or registers a concrete type on the way returns the state it leaves
/// behind; `dotnetRuntimeDirs` is where the loader looks for an assembly not yet loaded.
[<RequireQualifiedAccess>]
module StaticVirtualDispatch =

    let private operation = "static virtual dispatch"

    /// A default body `FindDefaultInterfaceImplementation` considers, and the interface supplying it.
    type private Candidate =
        {
            Interface : ConcreteTypeHandle
            Body : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        }

    /// What the default-body search finds.
    [<RequireQualifiedAccess>]
    type private DefaultBody =
        /// The body the call runs: the only most specific one, or in the variant pass the first of
        /// several.
        | Unique of Candidate
        /// Several equally specific bodies for exactly the call's instantiation, which the call
        /// throws `AmbiguousImplementationException` for.
        | Conflict of Candidate list
        | NotFound

    let private isVariant (state : TypeSystemState) (handle : ConcreteTypeHandle) : bool =
        match TypeSystemState.tryGetConcreteTypeInfo state handle with
        | Some (_, typeInfo) -> typeInfo.Generics |> Seq.exists (fun (_, metadata) -> metadata.Variance.IsSome)
        | None -> false

    let private sameDefinition (state : TypeSystemState) (a : ConcreteTypeHandle) (b : ConcreteTypeHandle) : bool =
        match TypeSystemState.tryGetConcreteTypeInfo state a, TypeSystemState.tryGetConcreteTypeInfo state b with
        | Some (a, _), Some (b, _) -> a.Identity = b.Identity
        | _ -> false

    /// The definition of the interface method `method` instantiates.
    let private definitionOf
        (state : TypeSystemState)
        (method : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        match method.TryMetadata with
        | Some facts -> state.LoadedAssembly(method.DeclaringAssemblyFullName).Value.Methods.[facts.Handle]
        | None -> failwith $"%s{operation}: %s{method.Name} is synthesised, so it is not a static virtual"

    /// The body of the first MethodImpl on `level`, in row order, that implements `method` on an
    /// interface instantiation `accepts` admits (`MethodTable::TryResolveVirtualStaticMethodOnThisType`).
    let private implementationOnType
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (level : ConcreteTypeHandle)
        (method : SlotIdentity)
        (accepts : TypeSystemState -> ConcreteTypeHandle -> TypeSystemState * bool)
        (state : TypeSystemState)
        : TypeSystemState *
          WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> option
        =
        match TypeSystemState.tryGetConcreteTypeInfo state level with
        | None -> state, None
        | Some (levelTy, levelTypeInfo) ->

        let owner = ConcreteMethodTable.ownerOfDefinition operation state levelTy.Identity

        let assembly, _ =
            ConcreteMethodTable.definitionMetadata operation state levelTy.Identity

        let rows =
            levelTypeInfo.MethodImpls
            |> Seq.sortBy (fun (KeyValue (handle, _)) ->
                MetadataToken.toInt (MetadataToken.MethodImplementation handle)
            )
            |> Seq.map (fun (KeyValue (_, impl)) -> impl)
            |> List.ofSeq

        let rec first (state : TypeSystemState) (rows : MethodImplParsed list) =
            match rows with
            | [] -> state, None
            | row :: rows ->
                let state, declaration =
                    ConcreteInterfaceDispatch.interfaceMethodImplDeclaration
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        operation
                        true
                        state
                        levelTy
                        owner.Description
                        owner.Substitution
                        assembly
                        row.Declaration

                match declaration with
                | Some (declaredOn, _, declared) when declared = method ->
                    match accepts state declaredOn with
                    | state, true ->
                        let state, body =
                            ConcreteInterfaceDispatch.methodImplBody
                                loggerFactory
                                dotnetRuntimeDirs
                                baseClassTypes
                                operation
                                state
                                owner
                                levelTypeInfo
                                assembly
                                row.Body

                        state, Some body
                    | state, false -> first state rows
                | _ -> first state rows

        first state rows

    /// Whether `from` casts to `target`, which for two interfaces is identity or variance.
    let private castsTo
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (target : ConcreteTypeHandle)
        (state : TypeSystemState)
        (from : ConcreteTypeHandle)
        : TypeSystemState * bool
        =
        TypeAssignability.isConcreteTypeAssignableTo loggerFactory dotnetRuntimeDirs baseClassTypes state from target

    /// What the interface `candidate` supplies for `method` of the instantiation `target`, if it
    /// supplies anything (`TryGetCandidateImplementation`): on `target` itself, the method's own body;
    /// on a variance-compatible instantiation of it, where variance is allowed, that body as that
    /// instantiation's; on a more specific interface, a MethodImpl of its own naming the method.
    let private candidateOn
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allowVariance : bool)
        (target : ConcreteTypeHandle)
        (method : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (candidate : ConcreteTypeHandle)
        (state : TypeSystemState)
        : TypeSystemState * Candidate option
        =
        let own () =
            if method.Body.IsAbstract then
                None
            else
                Some
                    {
                        Interface = candidate
                        Body = definitionOf state method
                    }

        if candidate = target then
            state, own ()
        else

        match castsTo loggerFactory dotnetRuntimeDirs baseClassTypes target state candidate with
        | state, false -> state, None
        | state, true ->

        if sameDefinition state candidate target then
            state, (if allowVariance then own () else None)
        else

        let accepts (state : TypeSystemState) (declaredOn : ConcreteTypeHandle) =
            if declaredOn = target then
                state, true
            elif allowVariance && sameDefinition state declaredOn target then
                castsTo loggerFactory dotnetRuntimeDirs baseClassTypes target state declaredOn
            else
                state, false

        let state, body =
            implementationOnType
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                candidate
                (method.DeclaringAssemblyFullName, method.IdentityKey)
                accepts
                state

        state,
        body
        |> Option.map (fun body ->
            {
                Interface = candidate
                Body = body
            }
        )

    /// Add `current` to `candidates`, keeping only the most specific of those that cast to one
    /// another (`FindDefaultInterfaceImplementation`'s insertion loop, transcribed). `None` marks a
    /// candidate a more specific one displaced.
    let private insertCandidate
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allowVariance : bool)
        (current : Candidate)
        (state : TypeSystemState)
        (candidates : Candidate option list)
        : TypeSystemState * Candidate option list
        =
        // Walks the candidates in order, rebuilding the list: `seenMoreSpecific` says whether
        // `current` has already displaced one, and `insert` whether it still needs a place.
        let rec go
            (state : TypeSystemState)
            (seenMoreSpecific : bool)
            (insert : bool)
            (before : Candidate option list)
            (after : Candidate option list)
            =
            match after with
            | [] -> state, List.rev before, insert
            | None :: rest -> go state seenMoreSpecific insert (None :: before) rest
            | Some existing :: rest ->
                if existing.Interface = current.Interface then
                    // A duplicate: nothing more to do.
                    state, List.rev before @ after, false
                elif allowVariance && sameDefinition state existing.Interface current.Interface then
                    // Variant instantiations of one interface tie.
                    go state seenMoreSpecific insert (Some existing :: before) rest
                else

                match
                    castsTo loggerFactory dotnetRuntimeDirs baseClassTypes existing.Interface state current.Interface
                with
                | state, true ->
                    // `current` is more specific: it takes the first such place, and empties the rest.
                    let replaced = if seenMoreSpecific then None else Some current
                    go state true false (replaced :: before) rest
                | state, false ->

                match
                    castsTo loggerFactory dotnetRuntimeDirs baseClassTypes current.Interface state existing.Interface
                with
                | state, true ->
                    // `existing` already stands for `current`.
                    state, List.rev before @ after, false
                | state, false -> go state seenMoreSpecific insert (Some existing :: before) rest

        let state, candidates, insert = go state false true [] candidates

        state, (if insert then candidates @ [ Some current ] else candidates)

    /// The most specific default body among `constrained` and the interfaces of every type on its
    /// chain (`MethodTable::FindDefaultInterfaceImplementation`). Each level contributes only the
    /// interfaces its parent's map does not already hold.
    let private defaultBody
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allowVariance : bool)
        (constrained : ConcreteTypeHandle)
        (target : ConcreteTypeHandle)
        (method : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : TypeSystemState)
        : TypeSystemState * DefaultBody
        =
        let consider (state : TypeSystemState, candidates : Candidate option list) (on : ConcreteTypeHandle) =
            match candidateOn loggerFactory dotnetRuntimeDirs baseClassTypes allowVariance target method on state with
            | state, None -> state, candidates
            | state, Some current ->
                insertCandidate loggerFactory dotnetRuntimeDirs baseClassTypes allowVariance current state candidates

        // The constrained type itself, when it is an interface.
        let state, candidates =
            match TypeSystemState.tryGetConcreteTypeInfo state constrained with
            | Some (_, typeInfo) when typeInfo.IsInterface -> consider (state, []) constrained
            | _ -> state, []

        let rec levels (state : TypeSystemState, candidates : Candidate option list) (level : ConcreteTypeHandle) =
            let state, map =
                ConcreteInterfaceDispatch.interfaceMapOf
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    operation
                    state
                    level

            let state, candidates =
                ((state, candidates), map |> List.filter (fun entry -> not entry.ImplementedByParent))
                ||> List.fold (fun acc entry -> consider acc entry.Interface)

            match
                TypeSystemState.resolveBaseConcreteType loggerFactory dotnetRuntimeDirs baseClassTypes state level
            with
            | state, None -> state, candidates
            | state, Some parent -> levels (state, candidates) parent

        let state, candidates = levels (state, candidates) constrained

        // The survivors are distinct interfaces, so more than one is a conflict. The search for a
        // static method finds that conflict in the variant pass too, but `ResolveVirtualStaticMethod`
        // then runs the first survivor anyway, reporting a variant match as unique.
        match List.choose id candidates with
        | [] -> state, DefaultBody.NotFound
        | [ only ] -> state, DefaultBody.Unique only
        | first :: _ when allowVariance -> state, DefaultBody.Unique first
        | all -> state, DefaultBody.Conflict all

    /// The method a `constrained.` call of the static virtual `method` runs when the prefix names
    /// `constrained`. `method` is the interface method as the call instantiates it, so its declaring
    /// type is the interface instantiation the call names.
    ///
    /// `Ambiguous` where no MethodImpl on the chain implements the method and two default bodies
    /// for exactly the call's instantiation are equally specific, which the call throws
    /// `AmbiguousImplementationException` for; `NotOverridden` where nothing implements it, which
    /// for a type CoreCLR loads and a call it compiles cannot happen. `constrained` must be a type
    /// with a TypeDef row.
    let resolve
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (constrained : ConcreteTypeHandle)
        (method : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : TypeSystemState)
        : TypeSystemState * VirtualImplementation
        =
        if not method.IsStatic then
            failwith $"%s{operation}: %s{method.Name} is not static"

        let target =
            match
                AllConcreteTypes.findExistingConcreteType
                    state.ConcreteTypes
                    method.RequiredDeclaringType.Identity
                    method.DeclaringTypeGenerics
            with
            | Some target -> target
            | None ->
                failwith
                    $"%s{operation}: the interface declaring %s{method.Name}, which has been instantiated, is not registered"

        if (TypeSystemState.tryGetConcreteTypeInfo state constrained).IsNone then
            failwith $"%s{operation}: the constrained type %O{constrained} has no TypeDef row"

        let slot : SlotIdentity = method.DeclaringAssemblyFullName, method.IdentityKey

        let found (state : TypeSystemState) (on : ConcreteTypeHandle) body =
            let typeGenerics =
                match TypeSystemState.tryGetConcreteTypeInfo state on with
                | Some (ty, _) -> ty.Generics
                | None -> ImmutableArray.Empty

            VirtualImplementation.Found
                {
                    Definition = body
                    TypeGenerics = typeGenerics
                    MethodGenerics = method.Generics
                }

        // 1. A MethodImpl on a type of the chain, exactly and then, for a variant interface, on the
        // first variance-compatible entry of the type's interface map.
        let rec onChain (state : TypeSystemState) (level : ConcreteTypeHandle) =
            let exactly (state : TypeSystemState) (declaredOn : ConcreteTypeHandle) = state, declaredOn = target

            match implementationOnType loggerFactory dotnetRuntimeDirs baseClassTypes level slot exactly state with
            | state, Some body -> state, Some (found state level body)
            | state, None ->

            let state, variant =
                if not (isVariant state target) then
                    state, None
                else

                let state, map =
                    ConcreteInterfaceDispatch.interfaceMapOf
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        operation
                        state
                        level

                let rec entries (state : TypeSystemState) (map : ConcreteInterfaceDispatch.InterfaceMapEntry list) =
                    match map with
                    | [] -> state, None
                    | entry :: rest when entry.Interface = target || not (sameDefinition state entry.Interface target) ->
                        entries state rest
                    | entry :: rest ->
                        match castsTo loggerFactory dotnetRuntimeDirs baseClassTypes target state entry.Interface with
                        | state, false -> entries state rest
                        | state, true ->
                            let onEntry (state : TypeSystemState) (declaredOn : ConcreteTypeHandle) =
                                state, declaredOn = entry.Interface

                            match
                                implementationOnType
                                    loggerFactory
                                    dotnetRuntimeDirs
                                    baseClassTypes
                                    level
                                    slot
                                    onEntry
                                    state
                            with
                            | state, Some body -> state, Some (found state level body)
                            | state, None -> entries state rest

                entries state map

            match variant with
            | Some _ -> state, variant
            | None ->

            match
                TypeSystemState.resolveBaseConcreteType loggerFactory dotnetRuntimeDirs baseClassTypes state level
            with
            | state, None -> state, None
            | state, Some parent -> onChain state parent

        match onChain state constrained with
        | state, Some implementation -> state, implementation
        | state, None ->

        // 2. A default body, exactly and then allowing variance.
        let fromDefault (state : TypeSystemState) (allowVariance : bool) =
            match
                defaultBody loggerFactory dotnetRuntimeDirs baseClassTypes allowVariance constrained target method state
            with
            | state, DefaultBody.Unique candidate -> state, Some (found state candidate.Interface candidate.Body)
            | state, DefaultBody.Conflict candidates ->
                state, Some (VirtualImplementation.Ambiguous (candidates |> List.map _.Body))
            | state, DefaultBody.NotFound -> state, None

        match fromDefault state false with
        | state, Some implementation -> state, implementation
        | state, None ->

        match fromDefault state true with
        | state, Some implementation -> state, implementation
        | state, None -> state, VirtualImplementation.NotOverridden

    /// Resolve a `constrained.`-prefixed reference to a static abstract interface member down to
    /// the implementation the constrained type supplies, returning it alongside its declaring
    /// type's handle.
    ///
    /// Shared by `constrained. call` and `constrained. ldftn`, which pick their target the same
    /// way: CoreCLR routes both through `getCallInfo` with the constrained token, and the switch
    /// there is `pConstrainedResolvedToken != NULL && pMD->IsInterface() && pMD->IsStatic()`
    /// (`jitinterface.cpp`, `getCallInfo`). That test is computed before anything branches on
    /// `CORINFO_CALLINFO_LDFTN`, so the *method chosen* cannot differ between the two opcodes;
    /// what differs afterwards is only what the caller does with it.
    ///
    /// `opName` names the prefixed instruction (`constrained.call` / `Ldftn`), so a failure says
    /// which one hit it rather than always blaming `call`.
    ///
    /// The instance-receiver forms of the prefix (`CORINFO_DEREF_THIS` / `CORINFO_BOX_THIS`) are
    /// not implemented: Roslyn emits `constrained.` before `ldftn` only for static
    /// abstract interface members, and before `call`/`callvirt` the instance cases are handled by
    /// `executeCallvirt`'s own transformation. Anything else fails loudly here rather than being
    /// guessed at.
    let resolveConstrainedStaticInterfaceMethod
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (opName : string)
        (constrainedTypeHandle : ConcreteTypeHandle)
        (methodToCall : WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>)
        (concretizedMethod : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : TypeSystemState)
        : TypeSystemState *
          WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> *
          ConcreteTypeHandle
        =
        let methodDeclAssy =
            state._LoadedAssemblies.ByDefinitionName methodToCall.DeclaringAssemblyFullName

        let methodDeclType =
            methodDeclAssy.TypeDefs.[methodToCall.RequiredDeclaringType.Definition.Get]

        if not methodToCall.IsStatic || not methodDeclType.IsInterface then
            failwith
                $"%s{opName}: expected a static interface method, got %s{MethodOwner.describe methodToCall.Owner}::%s{methodToCall.Name}"

        match constrainedTypeHandle with
        | ConcreteTypeHandle.Concrete _ ->
            // Registration is checked eagerly, and separately from rendering: an unregistered
            // handle would otherwise surface as a confusing resolution failure below rather than
            // as the bookkeeping error it is.
            if (AllConcreteTypes.lookup constrainedTypeHandle state.ConcreteTypes).IsNone then
                failwith $"%s{opName}: constrained type handle %O{constrainedTypeHandle} is not registered"
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            failwith
                $"%s{opName}: static interface dispatch for non-concrete constrained type %O{constrainedTypeHandle} is not implemented"

        let state, implementation =
            resolve loggerFactory dotnetRuntimeDirs baseClassTypes constrainedTypeHandle concretizedMethod state

        match implementation with
        | VirtualImplementation.NotOverridden ->
            let constrained =
                AllConcreteTypes.describe state._LoadedAssemblies state.ConcreteTypes constrainedTypeHandle

            failwith $"%s{opName}: could not find static implementation of %s{methodToCall.Name} on %s{constrained}"
        | VirtualImplementation.Ambiguous candidates ->
            candidates
            |> List.map (fun m -> $"%s{MethodOwner.describe m.Owner}::%s{m.Name}")
            |> String.concat ", "
            // TODO: throw guest System.Runtime.AmbiguousImplementationException here.
            |> failwithf
                "%s: multiple most-specific default interface implementations of %s: %s"
                opName
                methodToCall.Name
        | VirtualImplementation.Unmodelled reason -> failwith $"%s{opName}: %s{reason}"
        | VirtualImplementation.Found implementation when not implementation.Definition.IsStatic ->
            failwith
                $"%s{opName}: resolved non-static implementation %s{MethodOwner.describe implementation.Definition.Owner}::%s{implementation.Definition.Name}"
        | VirtualImplementation.Found implementation ->
            MethodConcretisation.concretizeMethodWithAllGenerics
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                implementation.TypeGenerics
                implementation.Definition
                implementation.MethodGenerics
                state
