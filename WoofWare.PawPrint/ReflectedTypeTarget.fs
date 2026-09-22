namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging

module ReflectedTypeTarget =
    /// Instantiate `genericDefinition` with `genericArguments`, producing a fresh
    /// closed `ConcreteTypeHandle`. Mirrors CoreCLR's `Instantiate(...)` step:
    /// canonicalise to the open generic definition first, then re-instantiate.
    let instantiateOpenGenericTypeDefinition
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (genericDefinition : ResolvedTypeIdentity)
        (genericArguments : ConcreteTypeHandle list)
        : ConcreteTypeHandle * IlMachineState
        =
        let assembly =
            state.LoadedAssembly genericDefinition.AssemblyFullName
            |> Option.defaultWith (fun () ->
                failwith
                    $"%s{operation}: assembly for open generic type definition is not loaded: %s{genericDefinition.AssemblyFullName}"
            )

        let typeInfo = assembly.TypeDefs.[genericDefinition.TypeDefinition.Get]

        if typeInfo.Generics.Length <> genericArguments.Length then
            failwith
                $"%s{operation}: generic arity mismatch for %s{typeInfo.Namespace}.%s{typeInfo.Name}; definition has %i{typeInfo.Generics.Length} parameters, but call supplied %i{genericArguments.Length} arguments"

        let signatureTypeKind =
            DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies typeInfo

        let genericDefn = TypeDefn.FromDefinition (genericDefinition, signatureTypeKind)

        let genericArgDefns =
            genericArguments
            |> List.map (fun handle ->
                Concretization.concreteHandleToTypeDefn
                    baseClassTypes
                    handle
                    state.ConcreteTypes
                    state._LoadedAssemblies
            )
            |> ImmutableArray.CreateRange

        let state, instantiatedHandle =
            IlMachineTypeResolution.concretizeType
                loggerFactory
                baseClassTypes
                state
                genericDefinition.AssemblyFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.GenericInstantiation (genericDefn, genericArgDefns))

        instantiatedHandle, state

    /// What one axis of a reflected type's generic environment -- the type variables, or the
    /// method variables -- denotes.
    ///
    /// Two cases and not a per-variable mixture, because a mixture cannot arise: an axis comes
    /// either from an instantiation, whose every argument is a runtime type, or from a definition
    /// read as itself, whose every argument is that definition's own variable. Binding *some*
    /// arguments of an axis is `MakeGenericMethod` with an open argument, which
    /// `MethodHandle.MethodGenerics` cannot express either (see
    /// `sourcesPure/MakeGenericMethodOpenArgument.cs`).
    ///
    /// The distinction is not cosmetic. `Bound` keeps the concrete handles, which is what lets an
    /// element mentioning only this axis be *concretized* rather than walked -- and it must be,
    /// since `RuntimeTypeHandleTarget.openConstructed` refuses an all-closed argument list.
    [<RequireQualifiedAccess>]
    type ReflectionVariableBinding =
        /// The owner is an instantiation: the `i`th variable denotes this runtime type.
        | Bound of ImmutableArray<ConcreteTypeHandle>
        /// The owner is a definition read as itself: the `i`th variable denotes this target,
        /// which is the very object reflection hands the guest for it.
        | Formal of ImmutableArray<RuntimeTypeHandleTarget>

    [<RequireQualifiedAccess>]
    module ReflectionVariableBinding =
        /// The axis as targets, which is what a structural walk substitutes.
        let targets (binding : ReflectionVariableBinding) : ImmutableArray<RuntimeTypeHandleTarget> =
            match binding with
            | ReflectionVariableBinding.Bound handles ->
                handles |> Seq.map RuntimeTypeHandleTarget.Closed |> ImmutableArray.CreateRange
            | ReflectionVariableBinding.Formal targets -> targets

        /// The axis as a substitution `concretizeType` can apply, where it has one.
        ///
        /// `Formal` answers empty rather than failing: it is a legitimate argument on the closed
        /// path precisely when the element mentions no variable of this axis, and then the vector
        /// is never indexed. `reflectedTypeTarget` is what enforces that precondition.
        let substitution (binding : ReflectionVariableBinding) : ImmutableArray<ConcreteTypeHandle> =
            match binding with
            | ReflectionVariableBinding.Bound handles -> handles
            | ReflectionVariableBinding.Formal _ -> ImmutableArray.Empty

    /// The generic environment a *reflected* type is read in: what each ECMA-335 `!i` and `!!i`
    /// denotes, as something reflection can hand the guest.
    ///
    /// One entry per generic parameter the owner declares, so an index outside an array is
    /// malformed metadata rather than a missing entry. `MethodVariables` is an empty `Formal`
    /// where the owner is a type rather than a method, which makes every `!!i` there a failure --
    /// ECMA-335 §II.10.1.7 scopes a type parameter's constraints to the type, and a method that
    /// declares no generic parameters cannot spell one in its own signature.
    type ReflectionTypeEnvironment =
        {
            TypeVariables : ReflectionVariableBinding
            MethodVariables : ReflectionVariableBinding
        }

    /// Which axes a signature element mentions: whether any `!i` and whether any `!!i` appears
    /// anywhere inside it.
    ///
    /// Both answers come from one walk rather than from two predicates, because the caller uses
    /// them together to decide whether the element can be concretized, and two walks that had to
    /// agree by discipline could drift.
    [<Struct>]
    type private MentionedAxes =
        {
            TypeVariable : bool
            MethodVariable : bool
        }

    module private MentionedAxes =
        let none : MentionedAxes =
            {
                TypeVariable = false
                MethodVariable = false
            }

        let combine (a : MentionedAxes) (b : MentionedAxes) : MentionedAxes =
            {
                TypeVariable = a.TypeVariable || b.TypeVariable
                MethodVariable = a.MethodVariable || b.MethodVariable
            }

    let rec private mentionedAxes (ty : TypeDefn) : MentionedAxes =
        match ty with
        | TypeDefn.GenericTypeParameter _ ->
            { MentionedAxes.none with
                TypeVariable = true
            }
        | TypeDefn.GenericMethodParameter _ ->
            { MentionedAxes.none with
                MethodVariable = true
            }
        | TypeDefn.Array (element, _)
        | TypeDefn.Pinned element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.OneDimensionalArrayLowerBoundZero element -> mentionedAxes element
        | TypeDefn.Modified m -> MentionedAxes.combine (mentionedAxes m.Unmodified) (mentionedAxes m.Modifier)
        | TypeDefn.GenericInstantiation (generic, args) ->
            (mentionedAxes generic, args)
            ||> Seq.fold (fun acc arg -> MentionedAxes.combine acc (mentionedAxes arg))
        | TypeDefn.FunctionPointer signature ->
            let fromReturn =
                match signature.ReturnType with
                | MethodReturnType.Void -> MentionedAxes.none
                | MethodReturnType.Returns ret -> mentionedAxes ret

            (fromReturn, signature.ParameterTypes)
            ||> List.fold (fun acc parameter -> MentionedAxes.combine acc (mentionedAxes parameter))
        | TypeDefn.PrimitiveType _
        | TypeDefn.FromReference _
        | TypeDefn.FromDefinition _
        | TypeDefn.Void -> MentionedAxes.none

    /// The canonical identity of the definition at the head of a generic instantiation.
    ///
    /// Identity, not spelling: the same definition reached via `FromDefinition` and via a
    /// `FromReference` in some other assembly must produce one `ResolvedTypeIdentity`, because
    /// `TypeHandleRegistry` keys guest `Type` object identity on the resulting target.
    let private resolveDefinitionIdentity
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assembly : DumpedAssembly)
        (state : IlMachineState)
        (genericDef : TypeDefn)
        : IlMachineState * ResolvedTypeIdentity
        =
        match genericDef with
        | TypeDefn.FromDefinition (identity, _) -> state, identity
        | _ ->
            let state, _, resolved =
                IlMachineTypeResolution.resolveTypeFromDefn
                    loggerFactory
                    baseClassTypes
                    genericDef
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    assembly
                    state

            state, resolved.Identity

    /// The type reflection surfaces for one signature element, read in an environment whose
    /// variables need not denote runtime types -- a generic parameter's constraints, or the
    /// signature of a method whose declaring type is a generic definition rather than an
    /// instantiation.
    ///
    /// <paramref name="assembly"/> is the one whose token space <paramref name="ty"/> is spelled
    /// in; <paramref name="ownerDescription"/> names what carries the element, for diagnostics.
    ///
    /// An array, pointer, byref or function pointer *over* a variable comes back as a
    /// <c>Composite</c> or <c>FunctionPointer</c> target, with the variable beneath the shape as
    /// real .NET reflects it.
    let rec reflectedTypeTarget
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (ownerDescription : string)
        (assembly : DumpedAssembly)
        (environment : ReflectionTypeEnvironment)
        (state : IlMachineState)
        (ty : TypeDefn)
        : IlMachineState * RuntimeTypeHandleTarget
        =
        // Every variable this element mentions denotes a runtime type, so the element does too --
        // whether it mentions none at all, or only variables of an axis that is `Bound`.
        //
        // This has to be decided *before* the structural walk rather than after it, because a
        // walk that resolved every argument closed could not put the answer back together:
        // `RuntimeTypeHandleTarget.openConstructed` refuses an all-closed argument list, since
        // such a type belongs in `AllConcreteTypes` as a `Closed` handle, which the walk cannot
        // mint. `List<!0>` under a closed `Box<int>` is exactly that shape, and it shares a
        // signature with `!!0` whenever the declaring type is an instantiation and the method is
        // a generic method definition.
        let mentioned = mentionedAxes ty

        let axisIsClosed (mentions : bool) (binding : ReflectionVariableBinding) : bool =
            not mentions
            || (
                match binding with
                | ReflectionVariableBinding.Bound _ -> true
                | ReflectionVariableBinding.Formal _ -> false
            )

        if
            axisIsClosed mentioned.TypeVariable environment.TypeVariables
            && axisIsClosed mentioned.MethodVariable environment.MethodVariables
        then
            // A `Formal` axis contributes an empty substitution, which is sound precisely because
            // the test above has established that this element mentions no variable of it.
            let state, handle =
                IlMachineTypeResolution.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    assembly.DefinitionFullName
                    (ReflectionVariableBinding.substitution environment.TypeVariables)
                    (ReflectionVariableBinding.substitution environment.MethodVariables)
                    ty

            state, RuntimeTypeHandleTarget.Closed handle
        else

        // Reached only for an axis the test above found `Formal`, since a `Bound` one takes the
        // closed path; but indexed generally, so the arm stays correct if that ever changes.
        let typeVariables = ReflectionVariableBinding.targets environment.TypeVariables
        let methodVariables = ReflectionVariableBinding.targets environment.MethodVariables

        match ty with
        | TypeDefn.GenericTypeParameter index ->
            if index < 0 || index >= typeVariables.Length then
                failwith
                    $"%s{operation}: %s{ownerDescription} names type-generic parameter !%d{index}, but its generic environment supplies %d{typeVariables.Length} type argument(s)"

            state, typeVariables.[index]
        | TypeDefn.GenericMethodParameter index ->
            if index < 0 || index >= methodVariables.Length then
                failwith
                    $"%s{operation}: %s{ownerDescription} names method-generic parameter !!%d{index}, but its generic environment supplies %d{methodVariables.Length} method argument(s)"

            state, methodVariables.[index]
        | TypeDefn.Modified modified ->
            // Reflection reports the unmodified type, and `concretizeType` strips custom modifiers
            // on the closed path too (TypeConcretisation.fs), so the two agree.
            reflectedTypeTarget
                loggerFactory
                baseClassTypes
                operation
                ownerDescription
                assembly
                environment
                state
                modified.Unmodified
        | TypeDefn.GenericInstantiation (genericDef, args) ->
            let state, definition =
                resolveDefinitionIdentity loggerFactory baseClassTypes assembly state genericDef

            let state, argumentTargets =
                ((state, []), args)
                ||> Seq.fold (fun (state, acc) arg ->
                    let state, target =
                        reflectedTypeTarget
                            loggerFactory
                            baseClassTypes
                            operation
                            ownerDescription
                            assembly
                            environment
                            state
                            arg

                    state, target :: acc
                )

            // `openConstructed` is what keeps this canonical: it collapses the typical
            // instantiation -- the CRTP `where T : ISelf<T>`, and a definition naming itself in its
            // own method signatures -- back to the bare definition, exactly as CoreCLR's class
            // loader does, so the guest sees one `Type` object rather than two.
            state, RuntimeTypeHandleTarget.openConstructed definition (List.rev argumentTargets)
        | TypeDefn.Byref element
        | TypeDefn.Pointer element
        | TypeDefn.OneDimensionalArrayLowerBoundZero element
        | TypeDefn.Array (element, _) ->
            let shape =
                match ty with
                | TypeDefn.Byref _ -> CompositeShape.Byref
                | TypeDefn.Pointer _ -> CompositeShape.Pointer
                | TypeDefn.OneDimensionalArrayLowerBoundZero _ -> CompositeShape.OneDimArrayZero
                | TypeDefn.Array (_, rank) -> CompositeShape.Array rank
                | _ -> failwith "unreachable: the enclosing arm matched one of the four shapes"

            let state, elementTarget =
                reflectedTypeTarget
                    loggerFactory
                    baseClassTypes
                    operation
                    ownerDescription
                    assembly
                    environment
                    state
                    element

            // `composite` collapses a closed element back into the closed shape; the axis test
            // above means the element here mentions a formal, but the collapse is what keeps this
            // canonical if that ever changes.
            state, RuntimeTypeHandleTarget.composite shape elementTarget
        | TypeDefn.FunctionPointer signature ->
            let state, signature =
                TypeMethodSignature.map
                    state
                    (fun state ty ->
                        reflectedTypeTarget
                            loggerFactory
                            baseClassTypes
                            operation
                            ownerDescription
                            assembly
                            environment
                            state
                            ty
                    )
                    signature

            state, RuntimeTypeHandleTarget.functionPointer signature
        | TypeDefn.PrimitiveType _
        | TypeDefn.FromReference _
        | TypeDefn.FromDefinition _
        | TypeDefn.Void ->
            failwith
                $"unreachable: %s{operation}: %s{ownerDescription} names %O{ty}, which mentions no generic variable and so should have taken the closed path above"
        | TypeDefn.Pinned _ ->
            failwith
                $"%s{operation}: %s{ownerDescription} names %O{ty}, but a pinned type is a local-variable constraint (ECMA-335 II.23.2.9) and cannot appear in a signature reflection reads"
