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
    /// An axis comes either from an instantiation, whose every argument is a runtime type, or from
    /// something open: a definition read as itself, whose every argument is that definition's own
    /// variable, or an open construction such as the `Base&lt;int, T&gt;` a definition may extend,
    /// whose arguments mix the two.
    ///
    /// The distinction is not cosmetic. `Bound` keeps the concrete handles, which is what lets an
    /// element mentioning only this axis be *concretized* in one step rather than walked.
    [<RequireQualifiedAccess>]
    type ReflectionVariableBinding =
        /// The owner is an instantiation: the `i`th variable denotes this runtime type.
        | Bound of ImmutableArray<ConcreteTypeHandle>
        /// The owner is open -- a definition read as itself, or an open construction: the `i`th
        /// variable denotes this target, closed or not, which is the very object reflection hands
        /// the guest for it.
        | Open of ImmutableArray<RuntimeTypeHandleTarget>

    [<RequireQualifiedAccess>]
    module ReflectionVariableBinding =
        /// The axis as targets, which is what a structural walk substitutes.
        let targets (binding : ReflectionVariableBinding) : ImmutableArray<RuntimeTypeHandleTarget> =
            match binding with
            | ReflectionVariableBinding.Bound handles ->
                handles |> Seq.map RuntimeTypeHandleTarget.Closed |> ImmutableArray.CreateRange
            | ReflectionVariableBinding.Open targets -> targets

        /// The axis as a substitution `concretizeType` can apply, where it has one.
        ///
        /// `Open` answers empty rather than failing: it is a legitimate argument on the closed
        /// path precisely when the element mentions no variable of this axis, and then the vector
        /// is never indexed. `reflectedTypeTarget` is what enforces that precondition.
        let substitution (binding : ReflectionVariableBinding) : ImmutableArray<ConcreteTypeHandle> =
            match binding with
            | ReflectionVariableBinding.Bound handles -> handles
            | ReflectionVariableBinding.Open _ -> ImmutableArray.Empty

    /// The generic environment a *reflected* type is read in: what each ECMA-335 `!i` and `!!i`
    /// denotes, as something reflection can hand the guest.
    ///
    /// One entry per generic parameter the owner declares, so an index outside an array is
    /// malformed metadata rather than a missing entry. `MethodVariables` is an empty `Open`
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
    /// variables need not denote runtime types -- a generic parameter's constraints, the
    /// signature of a method whose declaring type is a generic definition rather than an
    /// instantiation, or the extends clause of a definition or of an open construction.
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
        // whether it mentions none at all, or only variables of an axis that is `Bound` -- and it
        // is concretized whole. The structural walk below is reached only for an element that
        // mentions a variable of an `Open` axis; each piece of it that does not (the `int` in
        // `Dictionary<!0, int>`) comes back through here and is concretized.
        let mentioned = mentionedAxes ty

        let axisIsClosed (mentions : bool) (binding : ReflectionVariableBinding) : bool =
            not mentions
            || (
                match binding with
                | ReflectionVariableBinding.Bound _ -> true
                | ReflectionVariableBinding.Open _ -> false
            )

        if
            axisIsClosed mentioned.TypeVariable environment.TypeVariables
            && axisIsClosed mentioned.MethodVariable environment.MethodVariables
        then
            // An `Open` axis contributes an empty substitution, which is sound precisely because
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

        // Reached only for an axis the test above found `Open`, since a `Bound` one takes the
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

            let argumentTargets = List.rev argumentTargets

            let closedArguments =
                argumentTargets
                |> List.choose (fun target ->
                    match target with
                    | RuntimeTypeHandleTarget.Closed handle -> Some handle
                    | _ -> None
                )

            if closedArguments.Length = argumentTargets.Length then
                // Every variable mentioned here named a closed argument of an `Open` axis --
                // `Base<!0>` read under `Derived<int, T>` -- so this is a closed type, which
                // `openConstructed` refuses to spell and `AllConcreteTypes` must hold instead.
                let handle, state =
                    instantiateOpenGenericTypeDefinition
                        loggerFactory
                        baseClassTypes
                        operation
                        state
                        definition
                        closedArguments

                state, RuntimeTypeHandleTarget.Closed handle
            else
                // `openConstructed` is what keeps this canonical: it collapses the typical
                // instantiation -- the CRTP `where T : ISelf<T>`, and a definition naming itself in
                // its own method signatures -- back to the bare definition, exactly as CoreCLR's
                // class loader does, so the guest sees one `Type` object rather than two.
                state, RuntimeTypeHandleTarget.openConstructed definition argumentTargets
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

    /// Render a method for a diagnostic: its declaring type and name, plus its MethodDef token, so
    /// that overloads sharing a name stay distinguishable.
    let private describeMethodDefinition
        (assembly : DumpedAssembly)
        (declaringTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (handle : System.Reflection.Metadata.MethodDefinitionHandle)
        (methodInfo : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        : string
        =
        let token =
            let handle : System.Reflection.Metadata.EntityHandle =
                System.Reflection.Metadata.MethodDefinitionHandle.op_Implicit handle

            System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken handle

        let declaringTypeName =
            TypeInfo.fullName (fun h -> assembly.TypeDefs.[h]) declaringTypeInfo

        $"%s{declaringTypeName}::%s{methodInfo.Name} (MethodDef 0x%08x{token})"

    /// A generic parameter's declaration, as reflection reads it: its metadata, and the type each
    /// of its GenericParamConstraint rows names, in row order. The rows are those
    /// `GenericParamMetadata.Constraints` holds, so the synthetic `System.ValueType` row Roslyn
    /// emits beside `where T : struct` is absent; `NativeRuntimeTypeHelpers.genericParameterConstraintTargets`
    /// is what reports it as reflection does.
    ///
    /// <paramref name="target"/> must name a generic parameter, of a type or of a method.
    ///
    /// A constraint that mentions a type variable cannot be concretised, so it comes back as a
    /// parameter target (<c>where T2 : T1</c>) or as an open constructed type whose arguments are
    /// themselves targets (<c>where T : IComparable&lt;T&gt;</c>), recursively.
    let declaredConstraintTargets
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (target : RuntimeTypeHandleTarget)
        : IlMachineState * GenericParamMetadata * RuntimeTypeHandleTarget list
        =
        let declaringType =
            match target with
            | RuntimeTypeHandleTarget.GenericParameter (declaringType, _)
            | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, _, _) -> declaringType
            | RuntimeTypeHandleTarget.Closed _
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _
            | RuntimeTypeHandleTarget.OpenConstructed _
            | RuntimeTypeHandleTarget.DynamicMethodsClass _
            | RuntimeTypeHandleTarget.Composite _
            | RuntimeTypeHandleTarget.FunctionPointer _ ->
                failwith
                    $"%s{operation}: genericParameterConstraintTargets requires a generic-parameter target, got %O{target}"

        let assembly =
            state.LoadedAssembly declaringType.AssemblyFullName
            |> Option.defaultWith (fun () ->
                failwith
                    $"%s{operation}: assembly for the declaring type of %O{target} is not loaded: %s{declaringType.AssemblyFullName}"
            )

        let declaringTypeInfo = assembly.TypeDefs.[declaringType.TypeDefinition.Get]

        let declaringTypeName =
            TypeInfo.fullName (fun h -> assembly.TypeDefs.[h]) declaringTypeInfo

        // `!!n` inside a constraint signature names the owning *method*'s n-th formal, so it can be
        // given a target only under a method owner; the vector is empty for a type owner.
        // ECMA-335 §II.10.1.7 scopes a type parameter's constraints to the type, so no signature
        // the metadata model permits spells `!!n` there, and an empty vector stays a loud failure.
        let ownerDescription, parameterMetadata, methodVariables =
            match target with
            | RuntimeTypeHandleTarget.GenericParameter (_, position) ->
                if position < 0 || position >= declaringTypeInfo.Generics.Length then
                    failwith
                        $"%s{operation}: generic parameter position %d{position} is out of range for %s{declaringTypeName}, which declares %d{declaringTypeInfo.Generics.Length} parameter(s)"

                let description = $"type-generic parameter #%d{position} of %s{declaringTypeName}"

                description, snd declaringTypeInfo.Generics.[position], ImmutableArray.Empty
            | RuntimeTypeHandleTarget.MethodGenericParameter (_, declaringMethod, position) ->
                let methodInfo = assembly.Methods.[declaringMethod.Get]

                let methodDescription =
                    describeMethodDefinition assembly declaringTypeInfo declaringMethod.Get methodInfo

                // The MethodDef row is read out of the *declaring type's* assembly, and a
                // constraint's `!n` is resolved against that same type's formals. A target pairing
                // a method with a type that does not declare it would therefore answer about some
                // other method's parameter list rather than fail.
                match methodInfo.TryDeclaringType with
                | Some owner when owner.Identity = declaringType -> ()
                | Some owner ->
                    failwith
                        $"%s{operation}: %s{methodDescription} is declared on %O{owner.Identity}, but %O{target} names %s{declaringTypeName} as its declaring type"
                | None ->
                    failwith
                        $"%s{operation}: %s{methodDescription} has no declaring type, so it cannot be a method of %s{declaringTypeName} as %O{target} claims"

                if position < 0 || position >= methodInfo.Generics.Length then
                    failwith
                        $"%s{operation}: method-generic parameter position %d{position} is out of range for %s{methodDescription}, which declares %d{methodInfo.Generics.Length} parameter(s)"

                let description = $"method-generic parameter #%d{position} of %s{methodDescription}"

                let methodVariables =
                    Seq.init
                        methodInfo.Generics.Length
                        (fun index ->
                            RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, index)
                        )
                    |> ImmutableArray.CreateRange

                description, snd methodInfo.Generics.[position], methodVariables
            | RuntimeTypeHandleTarget.Closed _
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _
            | RuntimeTypeHandleTarget.OpenConstructed _
            | RuntimeTypeHandleTarget.DynamicMethodsClass _
            | RuntimeTypeHandleTarget.Composite _
            | RuntimeTypeHandleTarget.FunctionPointer _ ->
                failwith
                    $"logic error: %s{operation}: %O{target} is not a generic-parameter target, which binding `declaringType` above has already refused"

        // Both axes are `Open`: a constraint is read against the declaring owner's own
        // variables, never against an instantiation of them, so nothing here can take the closed
        // path on account of the environment. An all-closed constraint like `where T : List<int>`
        // still does, by mentioning no variable at all.
        let environment =
            {
                ReflectionTypeEnvironment.TypeVariables =
                    Seq.init
                        declaringTypeInfo.Generics.Length
                        (fun index -> RuntimeTypeHandleTarget.GenericParameter (declaringType, index))
                    |> ImmutableArray.CreateRange
                    |> ReflectionVariableBinding.Open
                ReflectionTypeEnvironment.MethodVariables = methodVariables |> ReflectionVariableBinding.Open
            }

        let constraintTarget (state : IlMachineState) (ty : TypeDefn) : IlMachineState * RuntimeTypeHandleTarget =
            reflectedTypeTarget
                loggerFactory
                baseClassTypes
                operation
                $"a constraint on %s{ownerDescription}"
                assembly
                environment
                state
                ty

        // No variance validation happens here. CoreCLR's `TypeVarTypeDesc::LoadConstraints` runs
        // `EEClass::CheckVarianceInSig` over each TypeSpec constraint of a method declared on a
        // variant type, and throws TypeLoadException on violation; PawPrint validates variance
        // nowhere, and C# rejects the violating shape, so only hand-written IL could tell.
        // A constraint mentioning no type variable -- `where T : List<int>` -- is an ordinary closed
        // type; one that mentions a variable (`where T2 : T1`, `where T : IComparable<T>`) comes
        // back as a parameter target or an open construction over one. The walk decides which.
        let baseTargets, state =
            ((List.empty, state), parameterMetadata.Constraints)
            ||> Seq.fold (fun (acc, state) ty ->
                let state, target = constraintTarget state ty
                target :: acc, state
            )

        let baseTargets = List.rev baseTargets

        state, parameterMetadata, baseTargets
