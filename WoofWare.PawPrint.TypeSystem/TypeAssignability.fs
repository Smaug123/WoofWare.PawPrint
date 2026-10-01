namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging

/// Whether a value of one concrete type can be stored where another is expected: CoreCLR's
/// `CanCastTo` over closed types, with interfaces, variance and the array rules.
[<RequireQualifiedAccess>]
module TypeAssignability =
    /// `isConcreteTypeAssignableTo`, as asked from inside a variance comparison that is already
    /// comparing the pairs in `visited` further up the same path: CoreCLR's `TypeHandlePairList`.
    /// A variance comparison that comes back to one of those pairs answers false, exactly as
    /// `CanCastByVarianceToInterfaceOrDelegate` does, which is what makes an expansive hierarchy
    /// such as `class C : IIn<IIn<C>>` terminate.
    let rec isConcreteTypeAssignableToVisiting
        (visited : Set<ConcreteTypeHandle * ConcreteTypeHandle>)
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (objType : ConcreteTypeHandle)
        (targetType : ConcreteTypeHandle)
        : TypeSystemState * bool
        =
        if objType = targetType then
            state, true
        else

        let isReferenceTypeHandle =
            TypeSystemState.isReferenceTypeHandle baseClassTypes "isConcreteTypeAssignableTo"

        let arrayShape (handle : ConcreteTypeHandle) : (ConcreteTypeHandle * int option) option =
            match handle with
            | ConcreteTypeHandle.OneDimArrayZero element -> Some (element, None)
            | ConcreteTypeHandle.Array (element, rank) -> Some (element, Some rank)
            | ConcreteTypeHandle.Concrete _
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> None

        let rec checkInterfaces (state : TypeSystemState) (current : ConcreteTypeHandle) : TypeSystemState * bool =
            match TypeSystemState.tryGetConcreteTypeInfo state current with
            | None ->
                // This node has no metadata-declared interfaces. The caller decides whether to walk its base.
                state, false
            | Some (ct, typeInfo) ->
                let assy = state._LoadedAssemblies.ByDefinitionName ct.Identity.AssemblyFullName

                ((state, false), typeInfo.ImplementedInterfaces)
                ||> Seq.fold (fun (state, found) impl ->
                    if found then
                        state, true
                    else
                        let implAssy =
                            match state.LoadedAssembly impl.RelativeToAssembly.FullName with
                            | Some a -> a
                            | None ->
                                // Assembly not yet loaded; use the assembly we already have since
                                // RelativeToAssembly is set to the assembly containing the type definition.
                                assy

                        let state, implTypeDefn, implResolvedAssy =
                            TypeSystemState.resolveTypeMetadataToken
                                loggerFactory
                                dotnetRuntimeDirs
                                baseClassTypes
                                state
                                implAssy
                                impl.InterfaceHandle

                        let state, implHandle =
                            TypeSystemState.concretizeType
                                loggerFactory
                                dotnetRuntimeDirs
                                baseClassTypes
                                state
                                implResolvedAssy.DefinitionFullName
                                ct.Generics
                                ImmutableArray.Empty
                                implTypeDefn

                        // Check exact match, then recurse into the interface's own parent interfaces.
                        walk state implHandle
                )

        and walkBase (state : TypeSystemState) (current : ConcreteTypeHandle) : TypeSystemState * bool =
            match current with
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> state, false
            | ConcreteTypeHandle.Concrete _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ ->
                let state, baseType =
                    TypeSystemState.resolveBaseConcreteType loggerFactory dotnetRuntimeDirs baseClassTypes state current

                match baseType with
                | None ->
                    // Every reference type (including interfaces) is assignable to System.Object.
                    match targetType with
                    | ConcreteActivePatterns.ConcreteObj state.ConcreteTypes -> state, true
                    | _ -> state, false
                | Some parent -> walk state parent

        and walk (state : TypeSystemState) (current : ConcreteTypeHandle) : TypeSystemState * bool =
            if current = targetType then
                state, true
            else

            match TypeSystemState.tryGetConcreteTypeInfo state current with
            | None -> walkBase state current
            | Some (currentCt, _) ->
                // Same TypeDef but different instantiations is the variance hook
                // (ECMA-335 §I.8.7.2 / CoreCLR
                // `CanCastByVarianceToInterfaceOrDelegate`). Classes are invariant
                // by spec, so when none of the parameters declare variance the
                // answer is definitively false. Interfaces and delegates can
                // declare `+`/`-` on each parameter; per-parameter assignability
                // resolves the cast.
                let sameDefnDifferentGenerics =
                    match AllConcreteTypes.lookup targetType state.ConcreteTypes with
                    | Some targetCt when
                        currentCt.Identity = targetCt.Identity
                        && currentCt.Generics <> targetCt.Generics
                        ->
                        Some targetCt
                    | _ -> None

                match sameDefnDifferentGenerics with
                | Some targetCt ->
                    let targetAssy =
                        state._LoadedAssemblies.ByDefinitionName targetCt.Identity.AssemblyFullName

                    let targetTypeInfo = targetAssy.TypeDefs.[targetCt.Identity.TypeDefinition.Get]

                    let hasVariantGenericParams =
                        targetTypeInfo.Generics
                        |> Seq.exists (fun (_, metadata) -> metadata.Variance.IsSome)

                    if not hasVariantGenericParams then
                        // All generic parameters are invariant; same definition + different generics = not assignable.
                        state, false
                    elif Set.contains (current, targetType) visited then
                        state, false
                    else
                        checkVariantGenericArgs
                            (Set.add (current, targetType) visited)
                            state
                            currentCt
                            targetCt
                            targetTypeInfo
                | None ->
                    let state, interfaceMatch = checkInterfaces state current

                    if interfaceMatch then
                        state, true
                    else
                        walkBase state current

        // ECMA-335 §I.8.7 / CoreCLR `MethodTable::CanCastByVarianceToInterfaceOrDelegate`:
        // when two generic instantiations share the same TypeDef and the
        // definition declares variance on at least one parameter, the cast
        // reduces to a per-parameter check.
        //   - Identical arguments are always accepted.
        //   - Covariant (`out`) parameter: `fromArg` must be a reference type
        //     and reference-assignable to `toArg`. (CoreCLR's `IsBoxedAndCanCastTo`
        //     rejects value-typed `fromArg` regardless of the declared variance —
        //     boxing changes identity, and the variance walk assumes the
        //     argument is in its boxed form.)
        //   - Contravariant (`in`) parameter: `toArg` must be a reference type
        //     and reference-assignable to `fromArg`.
        //   - Invariant parameter: arguments must be identical, so a difference
        //     here short-circuits to `false`.
        // Recursion into `isConcreteTypeAssignableTo` for the per-argument check
        // is necessary because variance composes (e.g. `Func<Func<Derived>>` ⊑
        // `Func<Func<Base>>` for the nested covariant `out` parameter).
        and checkVariantGenericArgs
            (visited : Set<ConcreteTypeHandle * ConcreteTypeHandle>)
            (state : TypeSystemState)
            (currentCt : ConcreteType<ConcreteTypeHandle>)
            (targetCt : ConcreteType<ConcreteTypeHandle>)
            (targetTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
            : TypeSystemState * bool
            =
            let rec loop (state : TypeSystemState) (i : int) : TypeSystemState * bool =
                if i >= currentCt.Generics.Length then
                    state, true
                else
                    let fromArg = currentCt.Generics.[i]
                    let toArg = targetCt.Generics.[i]

                    if fromArg = toArg then
                        loop state (i + 1)
                    else
                        let _, paramMetadata = targetTypeInfo.Generics.[i]

                        let state, argOk =
                            match paramMetadata.Variance with
                            | None ->
                                // Invariant parameter with non-identical arguments.
                                state, false
                            | Some GenericVariance.Covariant ->
                                if not (isReferenceTypeHandle state fromArg) then
                                    state, false
                                else
                                    isConcreteTypeAssignableToVisiting
                                        visited
                                        loggerFactory
                                        dotnetRuntimeDirs
                                        baseClassTypes
                                        state
                                        fromArg
                                        toArg
                            | Some GenericVariance.Contravariant ->
                                if not (isReferenceTypeHandle state toArg) then
                                    state, false
                                else
                                    isConcreteTypeAssignableToVisiting
                                        visited
                                        loggerFactory
                                        dotnetRuntimeDirs
                                        baseClassTypes
                                        state
                                        toArg
                                        fromArg

                        if argOk then loop state (i + 1) else state, false

            loop state 0

        // ECMA-335 III.8.7 / CoreCLR `GetNormalizedIntegralArrayElementType`:
        // signed and unsigned primitive integers of equal width are interchangeable
        // as array element types (`int[]` ↔ `uint[]`, `short[]` ↔ `ushort[]`, etc.).
        // Returns `Some normalizedIdentity` when `handle` is one of those primitive
        // integers; otherwise `None`. Floating-point, Boolean, and Char have no
        // normalization partners.
        let normalizedPrimitiveIntegerIdentity (handle : ConcreteTypeHandle) : ResolvedTypeIdentity option =
            match TypeSystemState.tryGetConcreteTypeInfo state handle with
            | Some (ct, _) when ct.Generics.IsEmpty ->
                let id = ct.Identity

                if id = baseClassTypes.SByte.Identity || id = baseClassTypes.Byte.Identity then
                    Some baseClassTypes.SByte.Identity
                elif id = baseClassTypes.Int16.Identity || id = baseClassTypes.UInt16.Identity then
                    Some baseClassTypes.Int16.Identity
                elif id = baseClassTypes.Int32.Identity || id = baseClassTypes.UInt32.Identity then
                    Some baseClassTypes.Int32.Identity
                elif id = baseClassTypes.Int64.Identity || id = baseClassTypes.UInt64.Identity then
                    Some baseClassTypes.Int64.Identity
                elif id = baseClassTypes.IntPtr.Identity || id = baseClassTypes.UIntPtr.Identity then
                    Some baseClassTypes.IntPtr.Identity
                else
                    None
            | _ -> None

        // ECMA-335 III.4.3 / CoreCLR `CanCastParam`: for value-typed array elements the
        // assignment-compatibility relation reduces to "the normalised integer identity
        // of each element matches". The normalised identity of a primitive integer is
        // the signed canonical (see `normalizedPrimitiveIntegerIdentity`); the normalised
        // identity of an enum is the normalised identity of its underlying integer.
        // Anything else (`float`, `double`, `bool`, `char`, non-integer struct) has no
        // normalised identity. Returns `None` when the input has no equivalence partner;
        // returns `Some id` otherwise.
        let valueElementNormalisedIdentity
            (state : TypeSystemState)
            (handle : ConcreteTypeHandle)
            : TypeSystemState * ResolvedTypeIdentity option
            =
            let state, isEnum =
                TypeSystemState.isEnumValueType loggerFactory dotnetRuntimeDirs baseClassTypes state handle

            if isEnum then
                match
                    TypeSystemState.enumUnderlyingHandle loggerFactory dotnetRuntimeDirs baseClassTypes state handle
                with
                | None -> state, None
                | Some (state, underlying) -> state, normalizedPrimitiveIntegerIdentity underlying
            else
                state, normalizedPrimitiveIntegerIdentity handle

        // ECMA-335 III.4.3 / CoreCLR `TypeDesc::CanCastParam`: element-compatibility
        // for parameterised array slots (whether array-to-array or SZ-array-to-
        // implicit-generic-interface) reduces to one of three cases.
        //   1. Identical elements — always compatible.
        //   2. Both reference-typed — recursive assignability (covariance).
        //   3. Both value-typed — same normalised integer identity, applying both
        //      ECMA-335 III.8.7 primitive-width equivalence and enum-underlying-
        //      type equivalence (see `valueElementNormalisedIdentity`).
        // Anything else (ref/value mismatch, non-integer value types, generic
        // type variables) answers definitively false.
        let elementCovariantlyCompatible
            (state : TypeSystemState)
            (objElement : ConcreteTypeHandle)
            (targetElement : ConcreteTypeHandle)
            : TypeSystemState * bool
            =
            if objElement = targetElement then
                state, true
            else
                let objIsRef = isReferenceTypeHandle state objElement
                let targetIsRef = isReferenceTypeHandle state targetElement

                if objIsRef && targetIsRef then
                    isConcreteTypeAssignableToVisiting
                        visited
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        state
                        objElement
                        targetElement
                elif objIsRef <> targetIsRef then
                    state, false
                else
                    let state, objNormalised = valueElementNormalisedIdentity state objElement
                    let state, targetNormalised = valueElementNormalisedIdentity state targetElement

                    match objNormalised, targetNormalised with
                    | Some a, Some b when a = b -> state, true
                    | _, _ -> state, false

        let checkArraySpecificRules
            (state : TypeSystemState)
            (objType : ConcreteTypeHandle)
            (targetType : ConcreteTypeHandle)
            : TypeSystemState * bool option
            =
            match arrayShape objType, arrayShape targetType with
            | Some (objElement, objShape), Some (targetElement, targetShape) ->
                // CoreCLR `MethodTable::ArrayIsInstanceOf` (`methodtable.cpp`): an SZ-array
                // target admits only an SZ-array source, and any other array target compares
                // ranks, where an SZ array's rank is 1. So `int[]` is an `int[*]` (the rank-1
                // ELEMENT_TYPE_ARRAY), but `int[*]` is not an `int[]`.
                let ranksAgree =
                    match objShape, targetShape with
                    | None, None -> true
                    | None, Some targetRank -> targetRank = 1
                    | Some _, None -> false
                    | Some objRank, Some targetRank -> objRank = targetRank

                if not ranksAgree then
                    state, Some false
                else
                    let state, compatible = elementCovariantlyCompatible state objElement targetElement
                    state, Some compatible
            | Some _, None -> state, None
            | None, _ -> failwith $"checkArraySpecificRules called with non-array source %O{objType}"

        // CoreCLR `MethodTable::ArraySupportsBizarreInterface` /
        // `IsImplicitInterfaceOfSZArray` (`src/coreclr/vm/array.cpp`): an
        // SZ-array `T[]` implicitly implements the five generic interfaces
        // `IList<U>`, `ICollection<U>`, `IEnumerable<U>`, `IReadOnlyList<U>`,
        // and `IReadOnlyCollection<U>` whenever `T` is element-compatible
        // with `U` under the CoreCLR `CanCastParam` rule (recursive
        // reference covariance for ref elements; normalised-integer
        // equivalence for value elements). The carve-out applies even for
        // the invariant interfaces (`IList<U>`, `ICollection<U>`).
        //
        // Multi-dim arrays do NOT participate in this carve-out, and other
        // generic interfaces (anything that isn't one of the five) are
        // never implicitly implemented by arrays. Returns `None` when the
        // pair does not fit the carve-out, leaving the caller to default
        // to `false`.
        let tryCheckSzArrayImplicitInterface
            (state : TypeSystemState)
            (objType : ConcreteTypeHandle)
            (targetType : ConcreteTypeHandle)
            : (TypeSystemState * bool) option
            =
            match objType with
            | ConcreteTypeHandle.OneDimArrayZero objElement ->
                match TypeSystemState.tryGetConcreteTypeInfo state targetType with
                | Some (targetCt, _) when targetCt.Generics.Length = 1 ->
                    if baseClassTypes.IsImplicitInterfaceOfSzArray targetCt.Identity then
                        let targetElement = targetCt.Generics.[0]
                        let state, compatible = elementCovariantlyCompatible state objElement targetElement
                        Some (state, compatible)
                    else
                        None
                | _ -> None
            | ConcreteTypeHandle.Array _
            | ConcreteTypeHandle.Concrete _
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> None

        match objType with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            let state, assignable = walk state objType

            if assignable then
                state, assignable
            else
                match checkArraySpecificRules state objType targetType with
                | state, Some assignable -> state, assignable
                | state, None ->
                    match tryCheckSzArrayImplicitInterface state objType targetType with
                    | Some result -> result
                    | None ->
                        // The remaining structural shapes — multi-dim arrays
                        // against any generic interface, or SZ-arrays against
                        // a generic interface that isn't one of the five
                        // implicit ones — are definitively not assignable.
                        // CoreCLR's `ArraySupportsBizarreInterface` agrees.
                        state, false
        | ConcreteTypeHandle.Concrete _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> walk state objType

    /// Check whether the concrete type `objType` is assignable to `targetType`.
    /// Walks the base type chain and checks implemented interfaces at each level.
    /// Returns true if objType = targetType, or targetType is a base class of objType,
    /// or targetType is an interface implemented by objType or any of its base classes.
    let isConcreteTypeAssignableTo
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (objType : ConcreteTypeHandle)
        (targetType : ConcreteTypeHandle)
        : TypeSystemState * bool
        =
        isConcreteTypeAssignableToVisiting
            Set.empty
            loggerFactory
            dotnetRuntimeDirs
            baseClassTypes
            state
            objType
            targetType
