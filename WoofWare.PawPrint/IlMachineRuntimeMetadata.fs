namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module IlMachineRuntimeMetadata =
    /// Returns the type handle and an allocated System.RuntimeType.
    let getOrAllocateType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (defn : RuntimeTypeHandleTarget)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let state, runtimeType =
            TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofDefinitionInAssembly
                    baseClassTypes.Corelib.DefinitionFullName
                    baseClassTypes.RuntimeType.TypeDefHandle,
                SignatureTypeKind.Class
            )
            |> IlMachineTypeResolution.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty

        let result, reg, state =
            TypeHandleRegistry.getOrAllocate
                state.ConcreteTypes
                baseClassTypes
                state
                (fun fields state -> IlMachineThreadState.allocateManagedObject runtimeType fields state)
                defn
                state.TypeHandles

        let state =
            { state with
                TypeHandles = reg
            }

        result, state

    /// Returns a System.RuntimeFieldHandle for the given field, observed on
    /// `declaringType`. The caller is responsible for supplying the correct
    /// instantiation context: in CoreCLR, `typeof(G<int>).GetField(...).FieldHandle`
    /// and `typeof(G<>).GetField(...).FieldHandle` are observably different — each
    /// carries its own declaring `RuntimeTypeHandle` — so this helper preserves the
    /// distinction by keying on the full target. Type-parameter targets are rejected
    /// by the registry because they cannot own a field.
    ///
    /// `fieldHandle` indexes the tables of the assembly that *defines* the field, which the
    /// registry derives from `declaringType` rather than taking as an argument.
    let getOrAllocateField
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (declaringType : RuntimeTypeHandleTarget)
        (fieldHandle : FieldDefinitionHandle)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let state, runtimeFieldInfoStub =
            TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofDefinitionInAssembly
                    baseClassTypes.Corelib.DefinitionFullName
                    baseClassTypes.RuntimeFieldInfoStub.TypeDefHandle,
                SignatureTypeKind.Class
            )
            |> IlMachineTypeResolution.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty

        let result, reg, state =
            FieldHandleRegistry.getOrAllocate
                baseClassTypes
                state.ConcreteTypes
                state
                (fun fields state -> IlMachineThreadState.allocateManagedObject runtimeFieldInfoStub fields state)
                declaringType
                fieldHandle
                state.FieldHandles

        let state =
            { state with
                FieldHandles = reg
            }

        result, state

    /// Returns a System.RuntimeMethodHandle.
    let getOrAllocateMethod
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (method : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let state, runtimeMethodInfoStub =
            TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofDefinitionInAssembly
                    baseClassTypes.Corelib.DefinitionFullName
                    baseClassTypes.RuntimeMethodInfoStub.TypeDefHandle,
                SignatureTypeKind.Class
            )
            |> IlMachineTypeResolution.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty

        let result, reg, state =
            MethodHandleRegistry.getOrAllocate
                baseClassTypes
                state.ConcreteTypes
                state
                (fun fields state -> IlMachineThreadState.allocateManagedObject runtimeMethodInfoStub fields state)
                method
                state.MethodHandles

        let state =
            { state with
                MethodHandles = reg
            }

        result, state

    let evalStackValueToObjectRef
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (value : EvalStackValue)
        : ManagedHeapAddress option
        =
        match value with
        | EvalStackValue.NullObjectRef -> None
        | EvalStackValue.ObjectRef addr -> Some addr
        | EvalStackValue.ManagedPointer src ->
            match IlMachineManagedByref.readManagedByref baseClassTypes state src with
            | CliType.ObjectRef addr -> addr
            | other -> failwith $"expected object reference, got {other}"
        | other -> failwith $"expected object reference, got {other}"

    let lookupTypeDefn
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (activeAssy : DumpedAssembly)
        (typeDef : TypeDefinitionHandle)
        : IlMachineState * TypeDefn
        =
        let defn = activeAssy.TypeDefs.[typeDef]
        state, DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies defn

    /// Resolve a `TypeReference` token to the type it names.
    ///
    /// No generic context is taken, and none may be: a `TypeReference` row names a type and carries
    /// no type arguments, so there is nothing for a caller to instantiate it with. `resolveTypeRef`
    /// substitutes whatever it is handed into the *referenced type's own* formal parameters,
    /// positionally (`Assembly.applyGenericArgs`), so passing the executing frame's generics binds
    /// them into an unrelated type's slots whenever the arities happen to line up: `ldtoken List`1`
    /// from a frame on `Holder<string>` came back as `List<string>`.
    ///
    /// A caller that does have arguments for the type is looking at a `TypeSpecification`, whose
    /// signature spells them out and which resolves by a different route. Callers that need the
    /// frame's context apply it downstream, when concretizing the `TypeDefn` this returns.
    let lookupTypeRef
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (activeAssy : DumpedAssembly)
        (ref : TypeReferenceHandle)
        : IlMachineState * TypeDefn * DumpedAssembly
        =
        let ref = activeAssy.TypeRefs.[ref]

        let state, assy, resolved =
            IlMachineTypeResolution.resolveTypeFromRef loggerFactory activeAssy ref ImmutableArray.Empty state

        state, DumpedAssembly.typeInfoToTypeDefn baseClassTypes state._LoadedAssemblies resolved, assy

    /// Resolve a BaseTypeInfo to the assembly and TypeDefn of the base type.
    let resolveBaseTypeInfo
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentAssembly : DumpedAssembly)
        (baseTypeInfo : BaseTypeInfo)
        : IlMachineState * DumpedAssembly * TypeDefn
        =
        match baseTypeInfo with
        | BaseTypeInfo.TypeDef handle ->
            let typeInfo = currentAssembly.TypeDefs.[handle]

            let typeDefn =
                DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies typeInfo

            state, currentAssembly, typeDefn
        | BaseTypeInfo.TypeRef handle ->
            let state, assy, resolved =
                IlMachineTypeResolution.resolveTypeFromRef
                    loggerFactory
                    currentAssembly
                    (currentAssembly.TypeRefs.[handle])
                    ImmutableArray.Empty
                    state

            let typeDefn =
                DumpedAssembly.typeInfoToTypeDefn baseClassTypes state._LoadedAssemblies resolved

            state, assy, typeDefn
        | BaseTypeInfo.TypeSpec handle ->
            let signature = currentAssembly.TypeSpecs.[handle].Signature
            state, currentAssembly, signature

    /// Given a ConcreteTypeHandle, resolve and return its base type as a ConcreteTypeHandle.
    /// Returns None for types without a base type (System.Object).
    let resolveBaseConcreteType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : IlMachineState * ConcreteTypeHandle option
        =
        match concreteType with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            // Structural array handles keep their own runtime identity; their base type is System.Array.
            let state, arrayHandle =
                DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.Array
                |> IlMachineTypeResolution.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    baseClassTypes.Corelib.DefinitionFullName
                    ImmutableArray.Empty
                    ImmutableArray.Empty

            state, Some arrayHandle
        | ConcreteTypeHandle.FunctionPointer _ ->
            failwith
                $"TODO: resolveBaseConcreteType: function pointer types (%O{concreteType}) not yet supported; the runtime base type is System.ValueType but the lookup path needs adjusting"
        | ConcreteTypeHandle.Concrete _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _ ->

            match AllConcreteTypes.lookup concreteType state.ConcreteTypes with
            | None -> failwith $"ConcreteTypeHandle {concreteType} not found in AllConcreteTypes"
            | Some ct ->
                let assy = state._LoadedAssemblies.ByDefinitionName ct.Identity.AssemblyFullName
                let typeInfo = assy.TypeDefs.[ct.Identity.TypeDefinition.Get]

                match typeInfo.BaseType with
                | None -> state, None
                | Some baseTypeInfo ->
                    let state, baseAssy, baseTypeDefn =
                        resolveBaseTypeInfo loggerFactory baseClassTypes state assy baseTypeInfo

                    let state, baseHandle =
                        IlMachineTypeResolution.concretizeType
                            loggerFactory
                            baseClassTypes
                            state
                            baseAssy.DefinitionFullName
                            ct.Generics
                            ImmutableArray.Empty
                            baseTypeDefn

                    state, Some baseHandle

    /// True iff `ty` references any `GenericTypeParameter` / `GenericMethodParameter`. The
    /// open-generic source cast walk uses this to decide whether a base/interface edge can be
    /// materialised to a closed `ConcreteTypeHandle` (when false) or only stripped to its
    /// definition identity for continued identity-walking (when true). A method generic
    /// parameter has no legitimate appearance in a type definition's base or interfaces,
    /// but treating it as unbound is the safe default.
    let rec containsAnyGenericParameter (ty : TypeDefn) : bool =
        match ty with
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _ -> true
        | TypeDefn.Array (element, _)
        | TypeDefn.Pinned element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.OneDimensionalArrayLowerBoundZero element -> containsAnyGenericParameter element
        | TypeDefn.Modified m ->
            containsAnyGenericParameter m.Unmodified
            || containsAnyGenericParameter m.Modifier
        | TypeDefn.GenericInstantiation (generic, args) ->
            containsAnyGenericParameter generic
            || (args |> Seq.exists containsAnyGenericParameter)
        | TypeDefn.FunctionPointer signature ->
            let returnContains =
                match signature.ReturnType with
                | MethodReturnType.Void -> false
                | MethodReturnType.Returns ret -> containsAnyGenericParameter ret

            returnContains
            || (signature.ParameterTypes |> List.exists containsAnyGenericParameter)
        | TypeDefn.PrimitiveType _
        | TypeDefn.FromReference _
        | TypeDefn.FromDefinition _
        | TypeDefn.Void -> false

    /// Whether CoreCLR shares code over `System.__Canon` for an instantiation with this type
    /// argument: `ClassLoader::CanonicalizeGenericArg` (generics.cpp:27) replaces a reference type,
    /// arrays included, by `__Canon`, and a value type by its canonical MethodTable. So a value type
    /// is shared exactly when one of its own type arguments is -- `ValueTuple<string>` shares with
    /// `ValueTuple<object>` -- and a non-generic one never is.
    ///
    /// A byref, pointer or function pointer is not a valid type argument, and CoreCLR asserts that
    /// none reaches the canonicalisation; one is refused rather than classified.
    let rec isSharedTypeArgument
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (describe : string)
        (argument : ConcreteTypeHandle)
        : bool
        =
        match argument with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ -> true
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            failwith
                $"TODO: %s{describe} is instantiated with %s{AllConcreteTypes.describe state._LoadedAssemblies state.ConcreteTypes argument}, which is not a valid type argument; PawPrint does not model how CoreCLR refuses it"
        | ConcreteTypeHandle.Concrete _ ->

        match AllConcreteTypes.tryTypeInfo state._LoadedAssemblies state.ConcreteTypes argument with
        | None ->
            failwith $"BUG: %s{describe} is instantiated with the handle %O{argument}, which names no registered type"
        | Some (concrete, typeInfo) ->
            if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo then
                concrete.Generics
                |> Seq.exists (isSharedTypeArgument baseClassTypes state describe)
            else
                true

    /// Given a `RuntimeTypeHandleTarget`, resolve and return its parent's
    /// `RuntimeTypeHandleTarget`. Returns `None` only at `System.Object`.
    ///
    /// `Closed` defers to `resolveBaseConcreteType` and rewraps as `Closed`.
    ///
    /// `OpenGenericTypeDefinition` is the typical instantiation `G<T>`, and `OpenConstructed` an
    /// instantiation with at least one argument open. Either one's parent is its definition's
    /// extends clause with that instantiation's arguments substituted in, exactly as CoreCLR loads
    /// the parent of any instantiation: `class G<T> : Base<T>` makes the parent of `G<>` the open
    /// construction `Base<T>` over `G`'s own `T` (reflection reports it with
    /// `IsGenericTypeDefinition` false), a base mentioning no parameter (`Base<int>`) is the closed
    /// type, and so is `Base<!0>` read under `G<int, U>`.
    ///
    /// `GenericParameter` and `MethodGenericParameter` are TypeDescs in
    /// CoreCLR and carry no MethodTable; asking for their parent is a bug.
    let resolveBaseRuntimeTypeHandleTarget
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (target : RuntimeTypeHandleTarget)
        : IlMachineState * RuntimeTypeHandleTarget option
        =
        match target with
        // `CreateMinimalMethodTable` calls `SetParentMethodTable(NULL)` (methodtable.cpp:701), so
        // the dynamic-methods class has no base type at all -- not even `object`. Its own comment
        // observes that the global type is built the same way.
        | RuntimeTypeHandleTarget.DynamicMethodsClass _ -> state, None
        | RuntimeTypeHandleTarget.Closed handle ->
            let state, parent =
                resolveBaseConcreteType loggerFactory baseClassTypes state handle

            state, parent |> Option.map RuntimeTypeHandleTarget.Closed
        // An array's base type is System.Array whatever its element, exactly as
        // `resolveBaseConcreteType` answers for a closed array.
        | RuntimeTypeHandleTarget.Composite ((CompositeShape.OneDimArrayZero | CompositeShape.Array _), _) ->
            let state, arrayHandle =
                DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.Array
                |> IlMachineTypeResolution.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    baseClassTypes.Corelib.DefinitionFullName
                    ImmutableArray.Empty
                    ImmutableArray.Empty

            state, Some (RuntimeTypeHandleTarget.Closed arrayHandle)
        | RuntimeTypeHandleTarget.Composite ((CompositeShape.Byref | CompositeShape.Pointer), _)
        | RuntimeTypeHandleTarget.FunctionPointer _ ->
            RuntimeTypeHandleTarget.refuseComposite "resolveBaseRuntimeTypeHandleTarget" target
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity
        | RuntimeTypeHandleTarget.OpenConstructed (identity, _) ->
            let assy =
                match state.LoadedAssembly identity.AssemblyFullName with
                | Some assembly -> assembly
                | None ->
                    failwith
                        $"resolveBaseRuntimeTypeHandleTarget: assembly %s{identity.AssemblyFullName} not loaded for open generic typedef %O{identity.TypeDefinition.Get}"

            let typeInfo = assy.TypeDefs.[identity.TypeDefinition.Get]

            match typeInfo.BaseType with
            | None -> state, None
            | Some baseTypeInfo ->
                let state, baseAssy, baseTypeDefn =
                    resolveBaseTypeInfo loggerFactory baseClassTypes state assy baseTypeInfo

                // What each `!i` of the extends clause denotes. The typical instantiation's
                // arguments are the definition's own variables, the very targets reflection hands
                // the guest for `typeof(G<>).GetGenericArguments()`.
                let typeArguments =
                    match target with
                    | RuntimeTypeHandleTarget.OpenConstructed (_, arguments) -> ImmutableArray.CreateRange arguments
                    | _ ->
                        Seq.init
                            typeInfo.Generics.Length
                            (fun index -> RuntimeTypeHandleTarget.GenericParameter (identity, index))
                        |> ImmutableArray.CreateRange

                let environment =
                    {
                        ReflectedTypeTarget.ReflectionTypeEnvironment.TypeVariables =
                            ReflectedTypeTarget.ReflectionVariableBinding.Open typeArguments
                        ReflectedTypeTarget.ReflectionTypeEnvironment.MethodVariables =
                            ReflectedTypeTarget.ReflectionVariableBinding.Open ImmutableArray.Empty
                    }

                let state, parent =
                    ReflectedTypeTarget.reflectedTypeTarget
                        loggerFactory
                        baseClassTypes
                        "resolveBaseRuntimeTypeHandleTarget"
                        $"the extends clause of %O{target}"
                        baseAssy
                        environment
                        state
                        baseTypeDefn

                state, Some parent
        | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
            failwith
                $"resolveBaseRuntimeTypeHandleTarget: refused for generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get}: TypeDescs have no MethodTable in CoreCLR"
        | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
            failwith
                $"resolveBaseRuntimeTypeHandleTarget: refused for method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}: TypeDescs have no MethodTable in CoreCLR"

    /// Collect the whole base chain as layout levels, base first: each entry carries the fields
    /// *that* type declares, together with the facts that govern how they are placed.
    ///
    /// The chain rather than a flat list is what makes layout faithful. CoreCLR lays each type out
    /// separately, starting a derived type's own fields at the parent's instance size
    /// (`HandleAutoLayout`, methodtablebuilder.cpp:8283-8296), and the per-level `Pack`, declared
    /// kind and declared `Size` all feed that -- none of which survives flattening (issue #994).
    let rec collectInstanceFieldChain
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : IlMachineState * TypeLayoutLevel list
        =
        let ct =
            AllConcreteTypes.lookup concreteType state.ConcreteTypes
            |> Option.defaultWith (fun () ->
                failwith
                    $"collectInstanceFieldChain: ConcreteTypeHandle %O{concreteType} not found in AllConcreteTypes"
            )

        let assy = state._LoadedAssemblies.ByDefinitionName ct.Identity.AssemblyFullName
        let typeInfo = assy.TypeDefs.[ct.Identity.TypeDefinition.Get]

        // Get this type's own instance fields
        let state, ownFields =
            let instanceFields =
                typeInfo.Fields
                |> List.filter (fun field -> not (field.Attributes.HasFlag FieldAttributes.Static))

            ((state, []), instanceFields)
            ||> List.fold (fun (state, fields) field ->
                let state, zero, fieldTypeHandle =
                    IlMachineTypeResolution.cliTypeZeroOf
                        loggerFactory
                        baseClassTypes
                        assy
                        field.Signature
                        ct.Generics
                        ImmutableArray.Empty
                        state

                let cliField : CliField =
                    {
                        Id = FieldId.metadata concreteType field.Handle field.Name
                        Name = field.Name
                        Contents = zero
                        Offset = field.Offset
                        Type = fieldTypeHandle
                        MarshallingDescriptor = field.MarshallingDescriptor
                    }

                state, cliField :: fields
            )

        // An `[InlineArray(N)]` type's storage is N repeats of its one declared field; see
        // `InlineArrayStorage.expand`. This site is reached for real by `newobj` on a struct with a
        // constructor, which is exactly how CoreLib builds `TwoObjects` for `SR.Format`.
        //
        // Unlike the other expansion sites, this one also walks *reference* types — for which the
        // attribute is inert; see `InlineArrayStorage.effectiveLength`.
        let ownFields =
            List.rev ownFields
            |> InlineArrayStorage.expand
                (fun () -> $"%s{typeInfo.Namespace}.%s{typeInfo.Name}")
                typeInfo.Layout
                (InlineArrayStorage.effectiveLength
                    (DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo)
                    typeInfo.InlineArrayLength)

        // `hasNonTrivialParent` (methodtablebuilder.cpp:8132) treats a type deriving directly from
        // `System.Object` or `System.ValueType` as having no parent at all. Marked rather than
        // dropped, so that the chain still describes `System.Object` itself.
        //
        // `System.Enum` is absent: it is a *reference* type deriving from `ValueType`
        // and declaring no instance fields, so CoreCLR treats it as an ordinary zero-sized parent
        // -- which is exactly why an enum's `value__` still lands at offset 0.
        let isTrivialParent =
            ct.Identity = baseClassTypes.Object.Identity
            || ct.Identity = baseClassTypes.ValueType.Identity

        let level : TypeLayoutLevel =
            {
                Declared = concreteType
                Facts = DeclaredTypeFacts.ofTypeInfo baseClassTypes state._LoadedAssemblies typeInfo
                OwnFields = ownFields
                IsTrivialParent = isTrivialParent
            }

        // Recurse into base type
        let state, baseHandle =
            resolveBaseConcreteType loggerFactory baseClassTypes state concreteType

        match baseHandle with
        | None -> state, [ level ]
        | Some parentHandle ->
            let state, baseLevels =
                collectInstanceFieldChain loggerFactory baseClassTypes state parentHandle

            state, baseLevels @ [ level ]

    /// The whole chain's fields as one list, base first. For callers that want to know *which*
    /// fields an object has rather than where they sit; anything laying storage out wants
    /// `collectInstanceFieldChain`, because the flat list cannot say where one type's fields end.
    let collectAllInstanceFields
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : IlMachineState * CliField list
        =
        let state, chain =
            collectInstanceFieldChain loggerFactory baseClassTypes state concreteType

        state, chain |> List.collect _.OwnFields

    /// Build the field-block storage for a heap instance of `concreteType`: its whole base chain,
    /// laid out per-declaring-type. The one way to build an object's storage.
    let buildInstanceStorage
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : IlMachineState * CliValueType
        =
        let state, chain =
            collectInstanceFieldChain loggerFactory baseClassTypes state concreteType

        state, CliValueType.OfFieldChain baseClassTypes state.ConcreteTypes concreteType chain

    /// Allocate a zeroed heap instance of <paramref name="concreteType"/> and return its address.
    /// No constructor runs, and no class initialiser either: callers that need
    /// the type initialised must arrange that themselves.
    ///
    /// For a value type this is the *boxed* representation, structurally identical to what
    /// `Box` writes for `default(T)`: both route the type's own non-static fields through
    /// `InlineArrayStorage.expand` and the same layout pass, and the base chain contributes
    /// nothing for a value type -- `ValueType` and `Object` declare no instance fields, and both
    /// are `IsTrivialParent`, so a struct's chain is one level starting at offset 0.
    /// `TestBoxedAllocationParity` pins that parity.
    let allocateUninitialisedInstance
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (concreteType : ConcreteTypeHandle)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let state, fields =
            buildInstanceStorage loggerFactory baseClassTypes state concreteType

        IlMachineThreadState.allocateManagedObject concreteType fields state

    /// Allocate a new System.String managed object on the heap with the given contents.
    /// Does NOT intern the string: every call returns a fresh heap object.  The Ldstr opcode
    /// wraps this with its own interning cache (see UnaryStringTokenIlOp); runtime-generated
    /// strings (stack traces, type names, etc.) call this directly.
    let allocateManagedString
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (contents : string)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        // String type is:
        // https://github.com/dotnet/runtime/blob/f0168ee80ba9aca18a7e7140b2bb436defda623c/src/libraries/System.Private.CoreLib/src/System/String.cs#L26
        let stringInstanceFields =
            baseClassTypes.String.Fields
            |> List.choose (fun field ->
                if int (field.Attributes &&& FieldAttributes.Static) = 0 then
                    Some (field.Name, field.Signature)
                else
                    None
            )
            |> List.sortBy fst

        if
            stringInstanceFields
            <> [
                ("_firstChar", TypeDefn.PrimitiveType PrimitiveType.Char)
                ("_stringLength", TypeDefn.PrimitiveType PrimitiveType.Int32)
            ]
        then
            failwith $"unexpectedly don't know how to initialise a string: got fields %O{stringInstanceFields}"

        let dataAddr, state = IlMachineThreadState.allocateStringData contents.Length state
        let state = IlMachineThreadState.setStringData dataAddr contents state

        let state, stringType =
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.String
            |> IlMachineTypeResolution.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty

        let fields =
            // `_firstChar` is omitted: its canonical storage is
            // `StringArrayData[dataOffset]`, and `RuntimeFieldProjection` synthesises
            // ldfld/ldflda/stfld access against that side-table. Materialising a
            // separate field cell would create a second source of truth for the
            // same char: a `stfld _firstChar` (e.g. CoreLib's `String.CreateFromChar`'s
            // `result._firstChar = c`) would bypass `setStringChar` and leave the
            // byte view at NUL.
            let stringLengthField =
                FieldIdentity.requiredOwnInstanceField baseClassTypes.String "_stringLength"

            [
                FieldIdentity.cliField
                    stringType
                    stringLengthField
                    (CliType.Numeric (CliNumericType.Int32 contents.Length))
                    (AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Int32)
            ]
            |> CliValueType.OfFields
                baseClassTypes
                state.ConcreteTypes
                stringType
                (DeclaredTypeFacts.ofTypeInfo baseClassTypes state._LoadedAssemblies baseClassTypes.String)

        let addr, state = IlMachineThreadState.allocateManagedObject stringType fields state

        let state =
            { state with
                ManagedHeap =
                    state.ManagedHeap
                    |> ManagedHeap.recordStringContents addr contents
                    |> ManagedHeap.recordStringDataOffset addr dataAddr
            }

        addr, state

    /// Return the address of the canonical empty managed string, allocating it lazily
    /// on first request. This is the single shared instance that backs both `ldstr ""`
    /// and `ldsfld System.String::Empty`, satisfying the CLR's invariant that
    /// `ReferenceEquals(string.Empty, "")` holds.
    let internCanonicalEmptyString
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        match state.InternedStrings.TryGetValue "" with
        | true, addr -> addr, state
        | false, _ ->
            let addr, state = allocateManagedString loggerFactory baseClassTypes "" state

            addr,
            { state with
                InternedStrings = state.InternedStrings.Add ("", addr)
            }

    /// Allocate a sentinel 2-entry CastCache backing array shaped to match what the native
    /// EE's `CastCache::Initialize` writes into `CastHelpers::s_table` at startup. The
    /// array stays a forever-empty sentinel: managed `CastCache.TryGet` reads `version == 0`
    /// on every probe and returns `CastResult.MaybeCast`, so callers fall through to the
    /// slow path that PawPrint's type system already handles. `CastCache.TrySet` would
    /// take the `TableMask == 1` early-return on the sentinel and never mutate it, but
    /// in any case PawPrint never invokes the instance `TrySet` (CoreCLR only writes to
    /// the cache from native code).
    let internCastCacheSentinelTable
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let state, int32Handle =
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.Int32
            |> IlMachineTypeResolution.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty

        let arrayTypeHandle = ConcreteTypeHandle.OneDimArrayZero int32Handle

        let zeroInt () : CliType =
            CliType.Numeric (CliNumericType.Int32 0)

        // Layout under managed `CastCache.CreateCastCache(2)` on a 64-bit guest: the `int32[]`
        // length is `(size + 1) * sizeof(CastCacheEntry) / 4` = `3 * 24 / 4` = 18. The ints
        // beyond the auxiliary header are zero-initialised — indices 3..5 are the unused tail
        // of the aux-slot `CastCacheEntry`, and indices 6..17 are entries 0 and 1 (zero
        // `_version` triggers the immediate `break` in `TryGet`).
        let addr, state =
            IlMachineThreadState.allocateArray arrayTypeHandle zeroInt 18 state

        // Auxiliary header: hashShift = 63 (LeadingZeroCount((nuint)1) on 64-bit),
        // tableMask = size - 1 = 1, victimCounter = 0 (already zero, written for clarity).
        // These live at element indices 0, 1, 2 because `CastCache.TableData` resolves
        // `GetRawData(table) + sizeof(nint)` to the first int element — `GetRawData` on
        // arrays returns a pointer at `RawArrayData.Length`, so the 8-byte skip walks past
        // `Length` + 64-bit padding and lands at element 0. The `HashShift`/`TableMask`/
        // `VictimCounter` accessors in `CastCache.cs:113-130` therefore index from `array[0]`.
        // PawPrint targets 64-bit guests exclusively, so 63 is hard-coded; it bounds the
        // initial `KeyToBucket` index to {0, 1}, keeping `Element(tableData, k)` inside the
        // `int[18]` table.
        let state =
            state
            |> IlMachineThreadState.setArrayValue addr (CliType.Numeric (CliNumericType.Int32 63)) 0
            |> IlMachineThreadState.setArrayValue addr (CliType.Numeric (CliNumericType.Int32 1)) 1
            |> IlMachineThreadState.setArrayValue addr (CliType.Numeric (CliNumericType.Int32 0)) 2

        addr, state

    let private concreteTypeFullName (state : IlMachineState) (ty : ConcreteType<ConcreteTypeHandle>) : string =
        match state.LoadedAssembly ty.AssemblyFullName with
        | Some assy -> Assembly.fullName assy ty.Identity
        | None when String.IsNullOrEmpty ty.Namespace -> ty.Name
        | None -> $"{ty.Namespace}.{ty.Name}"

    /// `Type.Name` for the BCL primitive types — CoreCLR's stack-trace rendering emits these
    /// rather than the IL keyword forms (e.g. `"Int32"`, not `"int32"`).
    let private primitiveBclName (pt : PrimitiveType) : string =
        match pt with
        | PrimitiveType.Boolean -> "Boolean"
        | PrimitiveType.Char -> "Char"
        | PrimitiveType.SByte -> "SByte"
        | PrimitiveType.Byte -> "Byte"
        | PrimitiveType.Int16 -> "Int16"
        | PrimitiveType.UInt16 -> "UInt16"
        | PrimitiveType.Int32 -> "Int32"
        | PrimitiveType.UInt32 -> "UInt32"
        | PrimitiveType.Int64 -> "Int64"
        | PrimitiveType.UInt64 -> "UInt64"
        | PrimitiveType.Single -> "Single"
        | PrimitiveType.Double -> "Double"
        | PrimitiveType.String -> "String"
        | PrimitiveType.TypedReference -> "TypedReference"
        | PrimitiveType.IntPtr -> "IntPtr"
        | PrimitiveType.UIntPtr -> "UIntPtr"
        | PrimitiveType.Object -> "Object"

    /// Render a parameter's type using the CLR's stack-trace convention: just `Type.Name`.
    /// `Type.Name` for a constructed generic such as `List<int>` is `"List`1"` — the
    /// instantiation is NOT appended (verified against `typeof(List<int>).Name` in CoreCLR),
    /// so this differs from full reflection name rendering (cf. `NativeRuntimeType.fs`
    /// `concreteTypeHandleName`, which appends `[args]` under FormatNamespace/Assembly).
    /// Array, pointer, and byref wrappers do show up in `Type.Name`, so we render those.
    ///
    /// Generic-method and generic-type parameter references resolve to the parameter's
    /// declared name (e.g. `TC`, `TM`) via the supplied name arrays. This mirrors
    /// CoreCLR — stack frames captured for shared-generic JITted code keep the formal
    /// parameter names rather than the call-site substitution, so a call to
    /// `Container<int>.Throw<string>(int, string)` renders as `Throw[TM](TC c, TM m)`.
    let rec private renderTypeDefnForStackFrame
        (state : IlMachineState)
        (typeGenericNames : string array)
        (methodGenericNames : string array)
        (ty : TypeDefn)
        : string
        =
        let recurse = renderTypeDefnForStackFrame state typeGenericNames methodGenericNames

        match ty with
        | TypeDefn.PrimitiveType pt -> primitiveBclName pt
        | TypeDefn.Void -> "Void"
        | TypeDefn.Byref inner -> recurse inner + "&"
        | TypeDefn.Pointer inner -> recurse inner + "*"
        | TypeDefn.OneDimensionalArrayLowerBoundZero inner -> recurse inner + "[]"
        | TypeDefn.Array (inner, rank) ->
            let dims = if rank <= 1 then "*" else System.String (',', rank - 1)
            recurse inner + "[" + dims + "]"
        | TypeDefn.Pinned inner -> recurse inner
        // Modified types: render the type the modifier is attached to, so optional/required
        // custom modifiers (e.g. the `modreq InAttribute` C# emits on an `in` parameter of a
        // virtual method) don't leak into the printed name.
        | TypeDefn.Modified m -> recurse m.Unmodified
        // CLR `Type.Name` on `List<int>` is `"List`1"`; the instantiation is dropped.
        | TypeDefn.GenericInstantiation (generic, _args) -> recurse generic
        | TypeDefn.GenericTypeParameter index ->
            if index >= 0 && index < typeGenericNames.Length then
                typeGenericNames.[index]
            else
                // The signature referenced a type-generic position the declaring type
                // doesn't declare — bad metadata. Render a debuggable placeholder rather
                // than crash the stack-trace path.
                $"!{index}"
        | TypeDefn.GenericMethodParameter index ->
            if index >= 0 && index < methodGenericNames.Length then
                methodGenericNames.[index]
            else
                $"!!{index}"
        | TypeDefn.FromReference (typeRef, _) -> typeRef.Name
        | TypeDefn.FromDefinition (identity, _) ->
            match state.LoadedAssembly identity.AssemblyFullName with
            | None -> "<unresolved>"
            | Some assy ->
                match assy.TypeDefs.TryGetValue identity.TypeDefinition.Get with
                | true, ti -> ti.Name
                | false, _ -> "<unresolved>"
        // CoreCLR's TypeString::AppendType emits the empty string for FnPtr when FormatNamespace
        // is unset; stack-trace parameter rendering uses the no-namespace form, so match that.
        | TypeDefn.FunctionPointer _ -> ""

    let private renderExceptionStackFrame
        (state : IlMachineState)
        (frame : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : string
        =
        // A method minted by `Reflection.Emit` has no declaring type, and real .NET renders its
        // frame with no type name at all: measured, a `DynamicMethod` called "Thrower" appears as
        // `at Thrower(Int32)` where an ordinary method appears as `at Ns.Type.M(Int32)`. So the
        // separating dot goes too, rather than leaving a leading `.` for a name that is not there.
        let qualifier =
            match frame.Method.TryDeclaringType with
            | Some declaringType -> $"%s{concreteTypeFullName state declaringType}."
            | None -> ""

        // The method's defining assembly is the assembly that contains its declaring type;
        // both the type-level and the method-level generic-parameter names live in there.
        let declaringAssembly = state.LoadedAssembly frame.Method.DeclaringAssemblyFullName

        let typeGenericNames : string array =
            match declaringAssembly, frame.Method.TryDeclaringType with
            | None, _
            | _, None -> Array.empty
            | Some assy, Some declaringType ->
                match assy.TypeDefs.TryGetValue declaringType.Definition.Get with
                | true, ti -> ti.Generics |> Seq.map (fun (gp, _) -> gp.Name) |> Seq.toArray
                | false, _ -> Array.empty

        let methodGenericNames : string array =
            match declaringAssembly with
            | None -> Array.empty
            | Some assy ->
                match frame.Method.TryMetadata with
                | None -> Array.empty
                | Some facts ->

                match assy.Methods.TryGetValue facts.Handle with
                | true, m -> m.Generics |> Seq.map (fun (gp, _) -> gp.Name) |> Seq.toArray
                | false, _ -> Array.empty

        // CoreCLR renders the method's generic argument list as `[T1,T2]` (comma-separated,
        // no space) between the method name and the parameter list. Non-generic methods get
        // no `[...]` suffix at all.
        let methodGenericsText =
            if methodGenericNames.Length = 0 then
                ""
            else
                "[" + (methodGenericNames |> String.concat ",") + "]"

        // Metadata Parameters skip SequenceNumber=0 (`this` / ref return), so signature index `i`
        // pairs with the parameter whose SequenceNumber is `i + 1` regardless of static-ness.
        // A synthesised method has no Param rows at all, so its frame renders positionally.
        let parameterByPosition =
            match frame.Method.TryMetadata with
            | None -> Map.empty
            | Some facts -> facts.Parameters |> Seq.map (fun p -> p.SequenceNumber, p.Name) |> Map.ofSeq

        // Walk the raw (TypeDefn) signature rather than the concretized one so
        // `GenericTypeParameter`/`GenericMethodParameter` references survive to render
        // as their formal names (`TC`, `TM`, etc.).
        // A synthesised method has no raw signature to walk. CoreCLR's own IL stubs do appear in
        // stack traces, so render the frame rather than refusing — just without parameter detail,
        // which is the only part that needs the metadata form.
        let paramText =
            match frame.Method.TryMetadata with
            | None -> "…"
            | Some facts ->

            facts.RawSignature.ParameterTypes
            |> List.mapi (fun i ty ->
                let typeStr =
                    renderTypeDefnForStackFrame state typeGenericNames methodGenericNames ty

                match Map.tryFind (i + 1) parameterByPosition with
                | Some name when not (String.IsNullOrEmpty name) -> $"%s{typeStr} %s{name}"
                | _ -> typeStr
            )
            |> String.concat ", "

        $"   at %s{qualifier}%s{frame.Method.Name}%s{methodGenericsText}(%s{paramText})"

    /// CoreLib's `SR.Exception_EndStackTraceFromPreviousThrow` (Strings.resx:2291), emitted by
    /// `StackTrace.ToString` after a frame whose `IsLastFrameFromForeignExceptionStackTrace` is
    /// set (StackTrace.cs:365). Hard-coded for the same reason as
    /// `NativeException.messageForKind`'s strings: PawPrint has no resource pipeline, and this is
    /// the invariant (non-localised) value.
    [<Literal>]
    let private foreignStackTraceBoundary =
        "--- End of stack trace from previous location ---"

    let private renderExceptionStackTrace
        (state : IlMachineState)
        (stackTrace : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> list)
        : string
        =
        stackTrace
        |> List.collect (fun frame ->
            let rendered = renderExceptionStackFrame state frame

            if frame.IsLastFrameFromForeignExceptionStackTrace then
                // Unconditionally, where `StackTrace.ToString` suppresses the annotation on an
                // async state machine's frame. That, and the rest of that method's display
                // policy, is a documented rendering gap; see docs/divergences.md.
                [ rendered ; foreignStackTraceBoundary ]
            else
                [ rendered ]
        )
        |> String.concat Environment.NewLine

    /// Write one field on an already-allocated exception object.
    ///
    /// `ExceptionDispatching.allocateRuntimeException` only zero-initialises the object and does
    /// not run any constructor, so runtime-synthesised exceptions otherwise carry a null `_message`
    /// and `Exception.Message` falls back to the generic "Exception of type X was thrown" string,
    /// and a `TypeLoadException` reports an empty `TypeName`. Where the CLR would have passed the
    /// value to a constructor overload, call this so a guest that catches the exception sees what
    /// it would really see. The declaring type of the field is part of the case, so a
    /// `TypeLoadException` field on an exception of another type fails loudly.
    let setRuntimeExceptionField
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (exceptionAddr : ManagedHeapAddress)
        (field : RuntimeExceptionField)
        (state : IlMachineState)
        : IlMachineState
        =
        let declaringType, fieldName, value =
            match field with
            | RuntimeExceptionField.Message message -> baseClassTypes.Exception, "_message", Choice1Of2 message
            | RuntimeExceptionField.TypeLoadClassName className ->
                baseClassTypes.TypeLoadException, "_className", Choice1Of2 className
            | RuntimeExceptionField.TypeLoadAssemblyName assemblyName ->
                baseClassTypes.TypeLoadException, "_assemblyName", Choice1Of2 assemblyName
            | RuntimeExceptionField.TypeLoadResourceId resourceId ->
                baseClassTypes.TypeLoadException, "_resourceId", Choice2Of2 resourceId

        match
            ManagedHeap.tryGet exceptionAddr state.ManagedHeap,
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes declaringType.Identity
        with
        | Some _, Some declaringTypeHandle ->
            let value, state =
                match value with
                | Choice1Of2 (str : string) ->
                    let valueAddr, state = allocateManagedString loggerFactory baseClassTypes str state
                    CliType.ObjectRef (Some valueAddr), state
                | Choice2Of2 (i : int) -> CliType.Numeric (CliNumericType.Int32 i), state

            let fieldId =
                FieldIdentity.requiredOwnInstanceField declaringType fieldName
                |> FieldIdentity.fieldId declaringTypeHandle

            IlMachineThreadState.setInstanceFieldById exceptionAddr fieldId value state
        // Mirrors `setExceptionStackTraceString`: skeletal states in low-level dispatch tests may
        // lack either piece, and there is nothing to project into in that case.
        | None, _
        | _, None -> state

    /// Project PawPrint's structured exception trace into the managed `System.Exception`
    /// object so guest code observing `Exception.StackTrace` sees a non-null trace string.
    let setExceptionStackTraceString
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (exceptionAddr : ManagedHeapAddress)
        (stackTrace : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> list)
        (state : IlMachineState)
        : IlMachineState
        =
        match stackTrace with
        | [] -> state
        | _ :: _ ->
            // Low-level dispatch tests sometimes use synthetic exception addresses in skeletal states.
            // Full guest execution has both pieces, so only then can we project into the managed object.
            match
                ManagedHeap.tryGet exceptionAddr state.ManagedHeap,
                AllConcreteTypes.findExistingNonGenericConcreteType
                    state.ConcreteTypes
                    baseClassTypes.Exception.Identity
            with
            | Some _, Some exceptionHandle ->
                let trace = renderExceptionStackTrace state stackTrace

                let traceAddr, state =
                    allocateManagedString loggerFactory baseClassTypes trace state

                let stackTraceStringField =
                    FieldIdentity.requiredOwnInstanceField baseClassTypes.Exception "_stackTraceString"
                    |> FieldIdentity.fieldId exceptionHandle

                IlMachineThreadState.setInstanceFieldById
                    exceptionAddr
                    stackTraceStringField
                    (CliType.ObjectRef (Some traceAddr))
                    state
            | None, _
            | _, None -> state

    /// The concrete field id of `System.Exception._stackTrace`, given the concrete handle of
    /// `System.Exception` itself. The field is declared on `Exception`, so it must be resolved
    /// against that type and not against whatever derived exception the object actually is.
    let private stackTraceFieldId
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (exceptionHandle : ConcreteTypeHandle)
        : FieldId
        =
        FieldIdentity.requiredOwnInstanceField baseClassTypes.Exception "_stackTrace"
        |> FieldIdentity.fieldId exceptionHandle

    /// Read the frozen-stack-trace token out of `exceptionAddr`'s `_stackTrace`. `None` means the
    /// exception has never been thrown, which is a legitimate state and not an error.
    ///
    /// Fails if the field holds a token PawPrint did not mint. The invariant is not that
    /// `recordThrownStackTrace` is the only writer — guest IL writes `_stackTrace` too, since
    /// `Exception.RestoreDispatchState` (Exception.CoreCLR.cs:140) assigns it from a captured
    /// `DispatchState` — but that every non-null value ever written is a token minted here
    /// earlier. That holds because the only source of a non-null value the guest can obtain is
    /// `ExceptionNative_GetFrozenStackTrace`, which hands back this same field, and because
    /// `FrozenStackTraces` is never pruned, so a token stays decodable for the rest of the run.
    /// Anything else means a second minter has appeared and a later decode would misread it.
    let frozenStackTraceToken
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (exceptionAddr : ManagedHeapAddress)
        (state : IlMachineState)
        : ManagedHeapAddress option
        =
        let exceptionObj =
            match ManagedHeap.tryGet exceptionAddr state.ManagedHeap with
            | Some obj -> obj
            | None ->
                failwith
                    $"frozenStackTraceToken: exception @ %O{exceptionAddr} is not a non-array heap object; this is an interpreter bug"

        match
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes baseClassTypes.Exception.Identity
        with
        | None ->
            failwith
                "frozenStackTraceToken: System.Exception has no concrete type handle, but an exception object exists on the heap; this is an interpreter bug"
        | Some exceptionHandle ->

        match
            AllocatedNonArrayObject.DereferenceFieldById (stackTraceFieldId baseClassTypes exceptionHandle) exceptionObj
        with
        | CliType.ObjectRef None -> None
        | CliType.ObjectRef (Some tokenAddr) ->
            if state.FrozenStackTraces |> Map.containsKey tokenAddr then
                Some tokenAddr
            else
                failwith
                    $"frozenStackTraceToken: exception @ %O{exceptionAddr} holds a _stackTrace token @ %O{tokenAddr} that is not registered in FrozenStackTraces; this is an interpreter bug"
        | other -> failwith $"frozenStackTraceToken: expected ObjectRef in Exception._stackTrace, got %O{other}"

    /// The frames behind `exceptionAddr`'s `_stackTrace` token: the trace as of its last dispatch.
    /// Empty means the exception has never been thrown, which is a legitimate state — an
    /// `ExceptionDispatchInfo` may be captured from an unthrown exception.
    ///
    /// This is the read side of `recordThrownStackTrace`, and the two must agree about what a
    /// token means; `frozenStackTraceToken` is what enforces that the token is one PawPrint minted.
    let frozenStackTraceFrames
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (exceptionAddr : ManagedHeapAddress)
        (state : IlMachineState)
        : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> list
        =
        match frozenStackTraceToken baseClassTypes exceptionAddr state with
        | None -> []
        | Some token ->
            match state.FrozenStackTraces |> Map.tryFind token with
            | Some frames -> frames
            | None ->
                // `frozenStackTraceToken` has already established that the token is registered, so
                // this is unreachable; stated rather than defaulted to `[]` because silently
                // answering "never thrown" would turn a bookkeeping bug into a lost stack trace.
                failwith
                    $"frozenStackTraceFrames: token %O{token} for exception @ %O{exceptionAddr} vanished from FrozenStackTraces between lookups; this is an interpreter bug"

    /// Record that `exceptionAddr` has been thrown, with `stackTrace` as the frames behind it:
    /// mint a fresh token object, register `token -> frames` in `IlMachineState.FrozenStackTraces`,
    /// and store the token in the exception's `_stackTrace`.
    ///
    /// A separate function from `setExceptionStackTraceString` rather than folded
    /// into it: that one is a pure string projection and is also called from
    /// `IlMachineStateExecution`, where a literal empty list means "I have nothing to say about
    /// this cached exception"; this one is a claim about dispatch, and only the four
    /// dispatch-conclusion sites in `ExceptionDispatching` make it.
    ///
    /// `_stackTrace` is what CoreLib's `Exception.HasBeenThrown` tests, so without this an
    /// exception PawPrint has dispatched still claims never to have been thrown, and
    /// `Exception.Source` silently answers null where the real runtime names the assembly.
    /// See `FrozenStackTraces` for why the token is opaque.
    ///
    /// An empty frame list mints no token. `HasBeenThrown`
    /// being true is what sends `Exception.StackTrace` down `GetStackTrace()` and into the
    /// structured decoder, so a token with no frames would promise an answer PawPrint cannot
    /// give and turn a readable `null` trace into a crash at the unimplemented
    /// `StackTrace_GetStackFramesInternal`.
    ///
    /// `ExceptionDispatching.applyFrameWraps` seeds each wrapper it synthesises with the
    /// frame raising it, so a synthesised wrapper arrives here with a frame; the empty-list
    /// guard is about this function's contract rather than about that caller.
    let recordThrownStackTrace
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (exceptionAddr : ManagedHeapAddress)
        (stackTrace : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> list)
        (state : IlMachineState)
        : IlMachineState
        =
        match stackTrace with
        | [] -> state
        | _ :: _ ->

        // Mirrors `setExceptionStackTraceString`: skeletal states in low-level dispatch tests may
        // lack either piece, and there is nothing to project into in that case.
        match
            ManagedHeap.tryGet exceptionAddr state.ManagedHeap,
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes baseClassTypes.Exception.Identity
        with
        | Some _, Some exceptionHandle ->
            let state, sbyteHandle =
                DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.SByte
                |> IlMachineTypeResolution.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    baseClassTypes.Corelib.DefinitionFullName
                    ImmutableArray.Empty
                    ImmutableArray.Empty

            // Zero-length: the token carries no data of its own, only identity. CoreCLR's real
            // array is an `I1Array`, hence `sbyte[]` rather than `byte[]`.
            let tokenAddr, state =
                IlMachineThreadState.allocateArray
                    (ConcreteTypeHandle.OneDimArrayZero sbyteHandle)
                    (fun () -> CliType.Numeric (CliNumericType.Int8 0y))
                    0
                    state

            if state.FrozenStackTraces |> Map.containsKey tokenAddr then
                failwith
                    $"recordThrownStackTrace: freshly allocated token %O{tokenAddr} is already registered in FrozenStackTraces; heap addresses must never be reused. This is an interpreter bug."

            let state =
                { state with
                    FrozenStackTraces = state.FrozenStackTraces |> Map.add tokenAddr stackTrace
                }

            IlMachineThreadState.setInstanceFieldById
                exceptionAddr
                (stackTraceFieldId baseClassTypes exceptionHandle)
                (CliType.ObjectRef (Some tokenAddr))
                state
        | None, _
        | _, None -> state

    /// Return the managed `System.Threading.Thread` heap object corresponding to the given guest
    /// thread, allocating it on first request and caching the address thereafter so that repeated
    /// calls yield reference-identical objects. Populates only the fields whose zero-initialised
    /// defaults would observably diverge from the CLR: `_managedThreadId` (ThreadId 0 is
    /// hardcoded to managed ID 1; others consume `NextManagedThreadId`), `_priority` (CLR
    /// exposes `ThreadPriority.Normal = 2`, not zero-valued `Lowest`), and
    /// `_DONT_USE_InternalThread` (non-zero sentinel so `GetNativeHandle()` doesn't throw).
    /// The Thread constructor is NOT run; other fields remain zero-initialised.
    let getOrAllocateManagedThreadObject
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (threadId : ThreadId)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        match state.ManagedThreadObjects.TryFind threadId with
        | Some addr -> addr, state
        | None ->

        let threadTypeInfo =
            baseClassTypes.Corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) ->
                if v.Namespace = "System.Threading" && v.Name = "Thread" then
                    Some v
                else
                    None
            )
            |> Seq.exactlyOne

        let state, threadTypeHandle =
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies threadTypeInfo
            |> IlMachineTypeResolution.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty

        let state, fields =
            buildInstanceStorage loggerFactory baseClassTypes state threadTypeHandle

        let addr, state =
            IlMachineThreadState.allocateManagedObject threadTypeHandle fields state

        // The main thread (ThreadId 0) always gets managed ID 1 — the CLR assigns it at
        // startup, before user code runs.  Other scheduler-created threads consume the shared
        // counter so IDs remain globally unique.
        let managedThreadId, state =
            let (ThreadId idx) = threadId

            if idx = 0 then
                1, state
            else
                let id = state.NextManagedThreadId

                id,
                { state with
                    NextManagedThreadId = id + 1
                }

        let threadPriorityNormal = 2
        let (ManagedHeapAddress addrInt) = addr

        let managedThreadIdField =
            FieldIdentity.requiredOwnInstanceField threadTypeInfo "_managedThreadId"
            |> FieldIdentity.fieldId threadTypeHandle

        let priorityField =
            FieldIdentity.requiredOwnInstanceField threadTypeInfo "_priority"
            |> FieldIdentity.fieldId threadTypeHandle

        let internalThreadField =
            FieldIdentity.requiredOwnInstanceField threadTypeInfo "_DONT_USE_InternalThread"
            |> FieldIdentity.fieldId threadTypeHandle

        let updatedObj =
            ManagedHeap.get addr state.ManagedHeap
            |> AllocatedNonArrayObject.SetFieldById
                managedThreadIdField
                (CliType.Numeric (CliNumericType.Int32 managedThreadId))
            |> AllocatedNonArrayObject.SetFieldById
                priorityField
                (CliType.Numeric (CliNumericType.Int32 threadPriorityNormal))
            |> AllocatedNonArrayObject.SetFieldById
                internalThreadField
                (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim (int64 addrInt))))

        let state =
            { state with
                ManagedHeap = ManagedHeap.set addr updatedObj state.ManagedHeap
                ManagedThreadObjects = state.ManagedThreadObjects |> Map.add threadId addr
            }

        addr, state

    /// Return the CLR-visible managed thread ID for the current guest thread.
    /// This is distinct from PawPrint's scheduler ThreadId.
    let getCurrentManagedThreadId (threadId : ThreadId) (state : IlMachineState) : int =
        match state.ManagedThreadObjects.TryFind threadId with
        | Some addr ->
            let threadObj = ManagedHeap.get addr state.ManagedHeap

            let threadConcreteType =
                AllConcreteTypes.lookup threadObj.ConcreteType state.ConcreteTypes
                |> Option.defaultWith (fun () ->
                    failwith
                        $"Environment.CurrentManagedThreadId: Thread object has unknown concrete type %O{threadObj.ConcreteType}"
                )

            let threadAssembly =
                state._LoadedAssemblies.ByDefinitionName threadConcreteType.Identity.AssemblyFullName

            let threadTypeInfo =
                threadAssembly.TypeDefs.[threadConcreteType.Identity.TypeDefinition.Get]

            let managedThreadIdField =
                FieldIdentity.requiredOwnInstanceField threadTypeInfo "_managedThreadId"
                |> FieldIdentity.fieldId threadObj.ConcreteType

            match AllocatedNonArrayObject.DereferenceFieldById managedThreadIdField threadObj with
            | CliType.Numeric (CliNumericType.Int32 id) -> id
            | other ->
                failwith
                    $"Environment.CurrentManagedThreadId: Thread object for ThreadId %O{threadId} has non-int32 _managedThreadId field %O{other}"
        | None ->
            match threadId with
            | ThreadId.ThreadId 0 -> 1
            | ThreadId.ThreadId _ ->
                failwith
                    $"Environment.CurrentManagedThreadId: non-main ThreadId %O{threadId} has no managed Thread object"

    /// Synthesize a TypeInitializationException wrapping the given inner exception object.
    /// Allocates the exception on the heap with zero-initialized fields (constructor is NOT run).
    /// Sets the _innerException, _typeName, and _HResult fields on the TIE to match what the
    /// TypeInitializationException(string, Exception) ctor would have done.
    /// Returns the heap address, the ConcreteTypeHandle, and the updated state.
    let synthesizeTypeInitializationException
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (typeFullName : string)
        (innerExceptionAddr : ManagedHeapAddress)
        (state : IlMachineState)
        : ManagedHeapAddress * ConcreteTypeHandle * IlMachineState
        =
        let tieTypeInfo = baseClassTypes.TypeInitializationException

        let stk =
            DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies tieTypeInfo

        let state, tieHandle =
            IlMachineTypeResolution.concretizeType
                loggerFactory
                baseClassTypes
                state
                tieTypeInfo.AssemblyFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (tieTypeInfo.Identity, stk))

        let state, fields =
            buildInstanceStorage loggerFactory baseClassTypes state tieHandle

        let addr, state = IlMachineThreadState.allocateManagedObject tieHandle fields state

        let typeNameAddr, state =
            allocateManagedString loggerFactory baseClassTypes typeFullName state

        // Set _innerException, _typeName and _HResult on the allocated TIE, matching what the
        // TypeInitializationException(string, Exception) ctor would have done.
        // See CLR's EEException::CreateThrowable:
        // https://github.com/dotnet/dotnet/blob/10060d128e3f470e77265f8490f5e4f72dae738e/src/runtime/src/coreclr/vm/clrex.cpp#L972-L1019
        let heapObj = ManagedHeap.get addr state.ManagedHeap

        let exceptionHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Exception

        let innerExceptionField =
            FieldIdentity.requiredOwnInstanceField baseClassTypes.Exception "_innerException"
            |> FieldIdentity.fieldId exceptionHandle

        let typeNameField =
            FieldIdentity.requiredOwnInstanceField tieTypeInfo "_typeName"
            |> FieldIdentity.fieldId tieHandle

        let hresultField =
            FieldIdentity.requiredOwnInstanceField baseClassTypes.Exception "_HResult"
            |> FieldIdentity.fieldId exceptionHandle

        let heapObj =
            heapObj
            |> AllocatedNonArrayObject.SetFieldById innerExceptionField (CliType.ObjectRef (Some innerExceptionAddr))
            |> AllocatedNonArrayObject.SetFieldById typeNameField (CliType.ObjectRef (Some typeNameAddr))
            |> AllocatedNonArrayObject.SetFieldById
                hresultField
                (CliType.Numeric (CliNumericType.Int32 (ExceptionHResults.lookup "System.TypeInitializationException")))

        let state =
            { state with
                ManagedHeap = ManagedHeap.set addr heapObj state.ManagedHeap
            }

        addr, tieHandle, state

    /// Synthesize a TargetInvocationException wrapping the given inner exception object.
    /// Allocates the exception on the heap with zero-initialized fields (constructor is NOT run).
    /// Sets the _innerException, _message and _HResult fields on the base Exception to match what
    /// `new TargetInvocationException(inner)` would have done in CoreCLR (whose base ctor sets
    /// `_message` to the SR.Arg_TargetInvocationException string). Returns the heap address, the
    /// ConcreteTypeHandle, and the updated state.
    /// See https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Reflection/TargetInvocationException.cs#L13-L17
    let synthesizeTargetInvocationException
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (innerExceptionAddr : ManagedHeapAddress)
        (state : IlMachineState)
        : ManagedHeapAddress * ConcreteTypeHandle * IlMachineState
        =
        let tieTypeInfo = baseClassTypes.TargetInvocationException

        let stk =
            DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies tieTypeInfo

        let state, tieHandle =
            IlMachineTypeResolution.concretizeType
                loggerFactory
                baseClassTypes
                state
                tieTypeInfo.AssemblyFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (tieTypeInfo.Identity, stk))

        let state, fields =
            buildInstanceStorage loggerFactory baseClassTypes state tieHandle

        let addr, state = IlMachineThreadState.allocateManagedObject tieHandle fields state

        // CoreCLR's TargetInvocationException(Exception) ctor calls
        //     base(SR.Arg_TargetInvocationException, inner)
        // which sets `_message` to the canonical string below. Bypassing the ctor would leave
        // `_message` null and divert `Message` / `ToString()` to the
        // "Exception of type '...' was thrown." fallback in Exception.Message, so allocate and
        // store the message explicitly.
        let messageAddr, state =
            allocateManagedString
                loggerFactory
                baseClassTypes
                "Exception has been thrown by the target of an invocation."
                state

        let heapObj = ManagedHeap.get addr state.ManagedHeap

        let exceptionHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Exception

        let innerExceptionField =
            FieldIdentity.requiredOwnInstanceField baseClassTypes.Exception "_innerException"
            |> FieldIdentity.fieldId exceptionHandle

        let messageField =
            FieldIdentity.requiredOwnInstanceField baseClassTypes.Exception "_message"
            |> FieldIdentity.fieldId exceptionHandle

        let hresultField =
            FieldIdentity.requiredOwnInstanceField baseClassTypes.Exception "_HResult"
            |> FieldIdentity.fieldId exceptionHandle

        let heapObj =
            heapObj
            |> AllocatedNonArrayObject.SetFieldById innerExceptionField (CliType.ObjectRef (Some innerExceptionAddr))
            |> AllocatedNonArrayObject.SetFieldById messageField (CliType.ObjectRef (Some messageAddr))
            |> AllocatedNonArrayObject.SetFieldById
                hresultField
                (CliType.Numeric (
                    CliNumericType.Int32 (ExceptionHResults.lookup "System.Reflection.TargetInvocationException")
                ))

        let state =
            { state with
                ManagedHeap = ManagedHeap.set addr heapObj state.ManagedHeap
            }

        addr, tieHandle, state

    /// Resolve a MetadataToken (TypeDefinition, TypeReference, or TypeSpecification) to a TypeDefn,
    /// together with the assembly the type was resolved in.
    ///
    /// Takes no generic context, for the reason `lookupTypeRef` gives: none of the three token
    /// kinds carries one. A `TypeSpecification`'s signature is returned verbatim, `!0` and all,
    /// for the caller to concretize against whatever context it means.
    let resolveTypeMetadataToken
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (activeAssy : DumpedAssembly)
        (token : MetadataToken)
        : IlMachineState * TypeDefn * DumpedAssembly
        =
        match token with
        | MetadataToken.TypeDefinition h ->
            let state, ty = lookupTypeDefn baseClassTypes state activeAssy h
            state, ty, activeAssy
        | MetadataToken.TypeReference ref -> lookupTypeRef loggerFactory baseClassTypes state activeAssy ref
        | MetadataToken.TypeSpecification spec -> state, activeAssy.TypeSpecs.[spec].Signature, activeAssy
        | m -> failwith $"unexpected type metadata token {m}"

    /// Get the metadata row directly represented by this concrete handle.
    /// Structural arrays, byrefs, and pointers have no direct TypeDef row; callers that are walking
    /// inheritance should ask for their base type explicitly.
    let tryGetConcreteTypeInfo
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : (ConcreteType<ConcreteTypeHandle> * TypeInfo<GenericParamFromMetadata, TypeDefn>) option
        =
        // Deliberately not just `AllConcreteTypes.tryTypeInfo`: this distinguishes the two
        // reasons that returns `None`. A structural handle is an ordinary answer of "no nominal
        // type here", but a `Concrete` handle with no row is a broken invariant and is raised.
        match concreteType with
        | ConcreteTypeHandle.Concrete _ ->
            match AllConcreteTypes.tryTypeInfo state._LoadedAssemblies state.ConcreteTypes concreteType with
            | None -> failwith $"ConcreteTypeHandle {concreteType} not found in AllConcreteTypes"
            | resolved -> resolved
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> None

    /// Returns true if `handle` is a CLR enum value type — a nominal type whose immediate runtime
    /// base is `System.Enum`.
    let isEnumValueType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : IlMachineState * bool
        =
        match handle with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> state, false
        | ConcreteTypeHandle.Concrete _ ->
            let state, baseHandle =
                resolveBaseConcreteType loggerFactory baseClassTypes state handle

            match baseHandle with
            | None -> state, false
            | Some bh ->
                match AllConcreteTypes.lookup bh state.ConcreteTypes with
                | Some baseTy -> state, baseTy.Identity = baseClassTypes.Enum.Identity
                | None -> state, false

    /// For an enum `ConcreteTypeHandle`, return the `ConcreteTypeHandle` of its underlying integer
    /// type by concretising the signature of its sole instance field (`value__`, the CLR-reserved
    /// name for the integer slot of an enum; ECMA-335 §II.14.3). Returns `None` if `handle` is not
    /// an enum, has no TypeDef row, or — defensively — has a malformed Fields list. The caller is
    /// expected to have first verified enum-ness via `isEnumValueType`; this helper does the
    /// metadata read.
    let enumUnderlyingHandle
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : (IlMachineState * ConcreteTypeHandle) option
        =
        match tryGetConcreteTypeInfo state handle with
        | None -> None
        | Some (ct, typeInfo) ->
            let instanceFields =
                typeInfo.Fields
                |> List.filter (fun f -> not (f.Attributes.HasFlag FieldAttributes.Static))

            match instanceFields with
            | [ valueField ] when valueField.Name = "value__" ->
                let assy = state._LoadedAssemblies.ByDefinitionName ct.Identity.AssemblyFullName

                let state, underlying =
                    IlMachineTypeResolution.concretizeType
                        loggerFactory
                        baseClassTypes
                        state
                        assy.DefinitionFullName
                        ct.Generics
                        ImmutableArray.Empty
                        valueField.Signature

                Some (state, underlying)
            | _ -> None

    /// CoreCLR `MethodTable::GetPrimitiveCorElementType`, restricted to the question `unbox` asks
    /// of it: which primitive `CorElementType` does this handle report, if it is in the
    /// primitive-value-type category at all?
    ///
    /// The category is CoreCLR's `enum_flag_Category_PrimitiveValueType`, which *includes enums*
    /// (see the `// Enum is included` remarks in RuntimeHelpers.CoreCLR.cs); an enum reports the
    /// element type of its underlying integer. Everything else — user structs, `Nullable\`1`,
    /// `System.Decimal`, reference types, the structural handles — answers `None`.
    ///
    /// The identity is returned *exactly*: `Int32` and `UInt32` are different answers, as are
    /// `Char`/`UInt16`, `Boolean`/`Byte` and `IntPtr`/`Int64`. This is narrower than
    /// both ECMA-335's verification types and the array-element rule below, each of which collapses
    /// signedness — see `unboxPermitted` for why we make that distinction.
    let primitiveElementIdentity
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : IlMachineState * ResolvedTypeIdentity option
        =
        // The built-in primitives, each its own distinct answer. Not a normalisation table: the
        // pairs that other CLR rules collapse (Int32/UInt32, Char/UInt16, Boolean/Byte,
        // IntPtr/UIntPtr) are listed separately precisely so they stay distinct here.
        // Takes `state` explicitly rather than capturing it: `enumUnderlyingHandle` concretises the
        // underlying type, so the enum branch below must consult the state it returns, not the one
        // this function was entered with.
        let builtInPrimitiveIdentity
            (state : IlMachineState)
            (handle : ConcreteTypeHandle)
            : ResolvedTypeIdentity option
            =
            match tryGetConcreteTypeInfo state handle with
            | None -> None
            | Some (ct, _) when not ct.Generics.IsEmpty -> None
            | Some (ct, _) ->
                let id = ct.Identity

                let isPrimitive =
                    [
                        baseClassTypes.Boolean
                        baseClassTypes.Char
                        baseClassTypes.SByte
                        baseClassTypes.Byte
                        baseClassTypes.Int16
                        baseClassTypes.UInt16
                        baseClassTypes.Int32
                        baseClassTypes.UInt32
                        baseClassTypes.Int64
                        baseClassTypes.UInt64
                        baseClassTypes.IntPtr
                        baseClassTypes.UIntPtr
                        baseClassTypes.Single
                        baseClassTypes.Double
                    ]
                    |> List.exists (fun ty -> ty.Identity = id)

                if isPrimitive then
                    Some id
                // CoreCLR puts three CoreLib handle structs in the primitive category too,
                // by name, reporting ELEMENT_TYPE_I — the same element type as `IntPtr`
                // (MethodTableBuilder, the `g_RuntimeMethodHandleInternalName` /
                // `g_RuntimeFieldHandleInternalName` / `g_RuntimeArgumentHandleName` arms of
                // `SetInternalCorElementType`). So `unbox.any IntPtr` on one of them is legal.
                // PawPrint already flattens the two `*HandleInternal` structs to a
                // runtime-pointer NativeInt, so they can be honoured exactly.
                //
                // `RuntimeArgumentHandle` is absent: PawPrint has no
                // `PrimitiveLikeKind` for it, so it is not stored flattened and answering
                // `Some` here would license an unbox this interpreter cannot materialise.
                // It stays unclassified, which costs an InvalidCastException in a case only
                // `__arglist` IL can reach.
                else if
                    id = baseClassTypes.RuntimeMethodHandleInternal.Identity
                    || id = baseClassTypes.RuntimeFieldHandleInternal.Identity
                then
                    Some baseClassTypes.IntPtr.Identity
                else
                    None

        match handle with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> state, None
        | ConcreteTypeHandle.Concrete _ ->
            match builtInPrimitiveIdentity state handle with
            | Some id -> state, Some id
            | None ->
                // An enum is in the primitive category too, reporting the element type of its
                // underlying integer. That underlying must itself be a built-in primitive — the
                // CLR type loader will not admit an enum whose `value__` is anything else — so
                // this resolves in exactly one step rather than recursing.
                let state, isEnum = isEnumValueType loggerFactory baseClassTypes state handle

                if not isEnum then
                    state, None
                else
                    match enumUnderlyingHandle loggerFactory baseClassTypes state handle with
                    | None -> state, None
                    | Some (state, underlying) -> state, builtInPrimitiveIdentity state underlying

    /// Does PawPrint store values of `handle` in the flattened form that the eval stack expects for
    /// a bare primitive?
    ///
    /// True for the built-in primitives themselves, and for enums over the fixed-width integers.
    /// False for enums over `bool`, `char` or a native int: ECMA-335 II.14.3 permits those and the
    /// CLR does load them (C# cannot declare one, but Reflection.Emit can), yet
    /// `CliValueType.EnumUnderlyingIsFlattenable` answers false for them, so their storage stays
    /// a wrapped `CliValueType`.
    let private unboxMaterialisesFlattened
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : IlMachineState * bool
        =
        let state, isEnum = isEnumValueType loggerFactory baseClassTypes state handle

        if not isEnum then
            // The caller has already established a primitive element identity for `handle`, so it
            // is either a built-in primitive (flattened by definition) or one of the two
            // `*HandleInternal` structs, which `PrimitiveLikeKind.FlattenToRuntimePointer` flattens.
            state, true
        else
            match enumUnderlyingHandle loggerFactory baseClassTypes state handle with
            | None -> state, false
            | Some (state, underlying) ->
                let state, underlyingId =
                    primitiveElementIdentity loggerFactory baseClassTypes state underlying

                match underlyingId with
                | None -> state, false
                | Some underlyingId ->
                    [
                        baseClassTypes.SByte
                        baseClassTypes.Byte
                        baseClassTypes.Int16
                        baseClassTypes.UInt16
                        baseClassTypes.Int32
                        baseClassTypes.UInt32
                        baseClassTypes.Int64
                        baseClassTypes.UInt64
                    ]
                    |> List.exists (fun ty -> ty.Identity = underlyingId)
                    |> fun flattenable -> state, flattenable

    /// CoreCLR `CastHelpers.Unbox_Helper`: `unbox` and the value-typed form of `unbox.any` accept a
    /// boxed operand when the two handles are identical, or when both types are in the primitive
    /// category and report the *same* primitive element type. That second clause is what lets a
    /// boxed enum unbox to its underlying integer and back.
    ///
    /// It is narrower than it first looks:
    ///   - ECMA-335's verification types collapse signedness (`int32` and `uint32` share one), but
    ///     this does not: `(uint)(object)1` raises InvalidCastException on a real runtime;
    ///   - the array-element rule (`CanCastParam`, via `valueElementNormalisedIdentity` below)
    ///     *does* collapse signedness, which is why `(uint[])(object)new int[1]` succeeds while the
    ///     scalar cast fails. Do not reach for that helper here — the two rules differ.
    ///
    /// `Nullable\`1` never reaches this predicate: it matches its argument by exact equivalence
    /// (`Nullable::IsNullableForTypeHelper`), so a boxed enum is not a `T?` of its underlying type.
    let unboxPermitted
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (boxedType : ConcreteTypeHandle)
        (targetType : ConcreteTypeHandle)
        : IlMachineState * bool
        =
        if boxedType = targetType then
            state, true
        else

        let state, boxedPrimitive =
            primitiveElementIdentity loggerFactory baseClassTypes state boxedType

        match boxedPrimitive with
        | None -> state, false
        | Some boxedPrimitive ->
            let state, targetPrimitive =
                primitiveElementIdentity loggerFactory baseClassTypes state targetType

            match targetPrimitive with
            | None -> state, false
            | Some targetPrimitive ->
                if boxedPrimitive <> targetPrimitive then
                    state, false
                else
                    // About to license the relaxation, which pairs two *different* handles. Both
                    // sides must be ones PawPrint stores in flattened form, and for different
                    // reasons:
                    //   - the boxed side drives materialisation, so an unflattened one would push a
                    //     wrapped `UserDefinedValueType` where the next instruction expects a bare
                    //     stack primitive;
                    //   - the target side is the slot the value lands in, and `toCliTypeCoerced`
                    //     rejects a bare primitive into a value-type slot unless that slot is
                    //     primitive-like (see the `failwith` in its `CliType.ValueType` arm), so an
                    //     unflattened target would abort on the following `stloc`/`stfld` instead.
                    // The identity case never reaches here, so this only ever rejects
                    // mixed pairs. Fail loudly rather than answering `false`, which would raise
                    // InvalidCastException where a real runtime would succeed.
                    let state, boxedFlattened =
                        unboxMaterialisesFlattened loggerFactory baseClassTypes state boxedType

                    let state, targetFlattened =
                        unboxMaterialisesFlattened loggerFactory baseClassTypes state targetType

                    if boxedFlattened && targetFlattened then
                        state, true
                    else
                        let offender = if boxedFlattened then targetType else boxedType

                        failwith
                            $"unbox of %O{boxedType} to %O{targetType}: CoreCLR permits this (both report the same primitive element type), but PawPrint does not store %O{offender} in flattened form — see CliValueType.EnumUnderlyingIsFlattenable, which covers only enums over the fixed-width integers, not over bool/char/native int"

    /// Does this handle denote a reference type (as opposed to a value type)?
    ///
    /// The structural handles answer without any metadata: arrays of every rank are reference
    /// types, while byrefs, pointers and function pointers are not (they are neither, strictly,
    /// but every caller asks this question to decide whether reference-type rules — covariance,
    /// array-store checks, atomic reference exchange — apply, and for those the answer is "no").
    /// Nominal handles defer to the TypeDef row.
    ///
    /// `context` names the caller in the diagnostic raised when a nominal handle has no TypeDef
    /// row, which would be a bug in whatever produced the handle.
    let isReferenceTypeHandle
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (context : string)
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : bool
        =
        match handle with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ -> true
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> false
        | ConcreteTypeHandle.Concrete _ ->
            match tryGetConcreteTypeInfo state handle with
            | Some (_, typeInfo) -> DumpedAssembly.isReferenceType baseClassTypes state._LoadedAssemblies typeInfo
            | None -> failwith $"%s{context}: concrete type handle %O{handle} has no TypeDef row"

    let requiredOwnInstanceFieldId
        (state : IlMachineState)
        (declaringType : ConcreteTypeHandle)
        (fieldName : string)
        : FieldId
        =
        match tryGetConcreteTypeInfo state declaringType with
        | Some (_, typeInfo) ->
            FieldIdentity.requiredOwnInstanceField typeInfo fieldName
            |> FieldIdentity.fieldId declaringType
        | None ->
            failwith
                $"requiredOwnInstanceFieldId: %O{declaringType} has no TypeDef row; cannot resolve field '%s{fieldName}'"

    /// The field `IRuntimeFieldInfo.Value` reads, on the non-array heap object at `addr`, which
    /// must implement `IRuntimeFieldInfo`. For a `RuntimeFieldInfoStub` that is read according to
    /// the stub's classified `RuntimeFieldInfoStubLayout`, and is the `RuntimeFieldHandleInternal`
    /// the stub names; for an `RtFieldInfo` it is its own `m_fieldHandle`, a bare `IntPtr`. Either
    /// holds a field-registry id or a zero pointer.
    let runtimeFieldInfoValue
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (addr : ManagedHeapAddress)
        (state : IlMachineState)
        : CliType
        =
        let heapObj = ManagedHeap.get addr state.ManagedHeap

        if RuntimeFieldInfoStubLayout.isStub baseClassTypes state.ConcreteTypes heapObj.ConcreteType then
            RuntimeFieldInfoStubLayout.value baseClassTypes state.ConcreteTypes heapObj.ConcreteType heapObj.Contents
        else
            let field = requiredOwnInstanceFieldId state heapObj.ConcreteType "m_fieldHandle"

            AllocatedNonArrayObject.DereferenceFieldById field heapObj

    /// CoreCLR's `Nullable::IsNullableForType` (`coreclr/vm/object.cpp:1516`): is
    /// `nullableCandidate` the type `System.Nullable`1[T]` for a `T` equivalent to `boxed`?
    ///
    /// This is the rule that makes *object* castability disagree with *type* castability: a
    /// boxed `T` "is" a `Nullable<T>` because the two share a boxed representation, even though
    /// `T` is not assignable to `Nullable<T>` structurally. CoreCLR checks it first and
    /// deliberately never caches the answer (`jithelpers.cpp:401-406`). The reflection cast
    /// path reaches the same rule through `CanCastToWorker(nullableCast: true)`.
    ///
    /// CoreCLR compares the instantiation argument with `TypeHandle::IsEquivalentTo`, which
    /// degenerates to handle equality unless `FEATURE_TYPEEQUIVALENCE` is on. That feature is
    /// Windows-only, so handle equality is exact for every CoreLib flavour PawPrint runs.
    let isNullableForType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (nullableCandidate : ConcreteTypeHandle)
        (boxed : ConcreteTypeHandle)
        : bool
        =
        match nullableCandidate with
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ -> false
        | ConcreteTypeHandle.Concrete _ ->
            match AllConcreteTypes.lookup nullableCandidate state.ConcreteTypes with
            | Some candidate when
                InternalTypeKind.kind baseClassTypes candidate = InternalTypeKind.Nullable
                && candidate.Generics.Length = 1
                ->
                candidate.Generics.[0] = boxed
            | _ -> false

    /// `isConcreteTypeAssignableTo`, as asked from inside a variance comparison that is already
    /// comparing the pairs in `visited` further up the same path: CoreCLR's `TypeHandlePairList`.
    /// A variance comparison that comes back to one of those pairs answers false, exactly as
    /// `CanCastByVarianceToInterfaceOrDelegate` does, which is what makes an expansive hierarchy
    /// such as `class C : IIn<IIn<C>>` terminate.
    let rec isConcreteTypeAssignableToVisiting
        (visited : Set<ConcreteTypeHandle * ConcreteTypeHandle>)
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (objType : ConcreteTypeHandle)
        (targetType : ConcreteTypeHandle)
        : IlMachineState * bool
        =
        if objType = targetType then
            state, true
        else

        let isReferenceTypeHandle =
            isReferenceTypeHandle baseClassTypes "isConcreteTypeAssignableTo"

        let arrayShape (handle : ConcreteTypeHandle) : (ConcreteTypeHandle * int option) option =
            match handle with
            | ConcreteTypeHandle.OneDimArrayZero element -> Some (element, None)
            | ConcreteTypeHandle.Array (element, rank) -> Some (element, Some rank)
            | ConcreteTypeHandle.Concrete _
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> None

        let rec checkInterfaces (state : IlMachineState) (current : ConcreteTypeHandle) : IlMachineState * bool =
            match tryGetConcreteTypeInfo state current with
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
                            resolveTypeMetadataToken loggerFactory baseClassTypes state implAssy impl.InterfaceHandle

                        let state, implHandle =
                            IlMachineTypeResolution.concretizeType
                                loggerFactory
                                baseClassTypes
                                state
                                implResolvedAssy.DefinitionFullName
                                ct.Generics
                                ImmutableArray.Empty
                                implTypeDefn

                        // Check exact match, then recurse into the interface's own parent interfaces.
                        walk state implHandle
                )

        and walkBase (state : IlMachineState) (current : ConcreteTypeHandle) : IlMachineState * bool =
            match current with
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> state, false
            | ConcreteTypeHandle.Concrete _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ ->
                let state, baseType =
                    resolveBaseConcreteType loggerFactory baseClassTypes state current

                match baseType with
                | None ->
                    // Every reference type (including interfaces) is assignable to System.Object.
                    match targetType with
                    | ConcreteActivePatterns.ConcreteObj state.ConcreteTypes -> state, true
                    | _ -> state, false
                | Some parent -> walk state parent

        and walk (state : IlMachineState) (current : ConcreteTypeHandle) : IlMachineState * bool =
            if current = targetType then
                state, true
            else

            match tryGetConcreteTypeInfo state current with
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
            (state : IlMachineState)
            (currentCt : ConcreteType<ConcreteTypeHandle>)
            (targetCt : ConcreteType<ConcreteTypeHandle>)
            (targetTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
            : IlMachineState * bool
            =
            let rec loop (state : IlMachineState) (i : int) : IlMachineState * bool =
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
            match tryGetConcreteTypeInfo state handle with
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
            (state : IlMachineState)
            (handle : ConcreteTypeHandle)
            : IlMachineState * ResolvedTypeIdentity option
            =
            let state, isEnum = isEnumValueType loggerFactory baseClassTypes state handle

            if isEnum then
                match enumUnderlyingHandle loggerFactory baseClassTypes state handle with
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
            (state : IlMachineState)
            (objElement : ConcreteTypeHandle)
            (targetElement : ConcreteTypeHandle)
            : IlMachineState * bool
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
            (state : IlMachineState)
            (objType : ConcreteTypeHandle)
            (targetType : ConcreteTypeHandle)
            : IlMachineState * bool option
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
            (state : IlMachineState)
            (objType : ConcreteTypeHandle)
            (targetType : ConcreteTypeHandle)
            : (IlMachineState * bool) option
            =
            match objType with
            | ConcreteTypeHandle.OneDimArrayZero objElement ->
                match tryGetConcreteTypeInfo state targetType with
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
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (objType : ConcreteTypeHandle)
        (targetType : ConcreteTypeHandle)
        : IlMachineState * bool
        =
        isConcreteTypeAssignableToVisiting Set.empty loggerFactory baseClassTypes state objType targetType

    /// The definition a MethodTable-backed nominal target instantiates, and its instantiation as
    /// targets: a closed type's own arguments, a definition's own variables (the typical
    /// instantiation), or an open construction's mix of the two. `None` for every other shape,
    /// none of which CoreCLR gives an instantiation that `CanCastByVarianceToInterfaceOrDelegate`
    /// could compare.
    let private nominalInstantiation
        (state : IlMachineState)
        (target : RuntimeTypeHandleTarget)
        : (ResolvedTypeIdentity * RuntimeTypeHandleTarget list) option
        =
        match target with
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete _ as handle) ->
            match tryGetConcreteTypeInfo state handle with
            | Some (concreteType, _) ->
                Some (
                    concreteType.Identity,
                    concreteType.Generics |> Seq.map RuntimeTypeHandleTarget.Closed |> List.ofSeq
                )
            | None -> failwith $"logic error: tryGetConcreteTypeInfo refused the Concrete handle %O{handle}"
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            let typeInfo =
                state._LoadedAssemblies
                    .ByDefinitionName(identity.AssemblyFullName)
                    .TypeDefs.[identity.TypeDefinition.Get]

            Some (
                identity,
                List.init
                    typeInfo.Generics.Length
                    (fun index -> RuntimeTypeHandleTarget.GenericParameter (identity, index))
            )
        | RuntimeTypeHandleTarget.OpenConstructed (identity, arguments) -> Some (identity, arguments)
        | RuntimeTypeHandleTarget.Closed _
        | RuntimeTypeHandleTarget.DynamicMethodsClass _
        | RuntimeTypeHandleTarget.GenericParameter _
        | RuntimeTypeHandleTarget.MethodGenericParameter _
        | RuntimeTypeHandleTarget.Composite _
        | RuntimeTypeHandleTarget.FunctionPointer _ -> None

    let private typeInfoOfIdentity
        (state : IlMachineState)
        (identity : ResolvedTypeIdentity)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        state._LoadedAssemblies.ByDefinitionName(identity.AssemblyFullName).TypeDefs.[identity.TypeDefinition.Get]

    /// CoreCLR's `CorTypeInfo::IsObjRef` of a target's element type, for every target that is not a
    /// type variable: whether values of it are object references. A type variable's answer is
    /// `TypeVarTypeDesc::ConstrainedAsObjRef`, which needs its constraints, so it is refused here.
    let private isObjRefTarget
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (target : RuntimeTypeHandleTarget)
        : bool
        =
        match target with
        | RuntimeTypeHandleTarget.Closed handle -> isReferenceTypeHandle baseClassTypes "isObjRefTarget" state handle
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity
        | RuntimeTypeHandleTarget.OpenConstructed (identity, _) ->
            DumpedAssembly.isReferenceType baseClassTypes state._LoadedAssemblies (typeInfoOfIdentity state identity)
        | RuntimeTypeHandleTarget.DynamicMethodsClass _ -> true
        | RuntimeTypeHandleTarget.Composite ((CompositeShape.OneDimArrayZero | CompositeShape.Array _), _) -> true
        | RuntimeTypeHandleTarget.Composite ((CompositeShape.Byref | CompositeShape.Pointer), _)
        | RuntimeTypeHandleTarget.FunctionPointer _ -> false
        | RuntimeTypeHandleTarget.GenericParameter _
        | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
            failwith $"logic error: isObjRefTarget asked about the type variable %O{target}"

    let private isInterfaceTarget (state : IlMachineState) (target : RuntimeTypeHandleTarget) : bool =
        match nominalInstantiation state target with
        | Some (identity, _) -> (typeInfoOfIdentity state identity).IsInterface
        | None -> false

    /// Whether `target` is the closed type `typeInfo` names, which must be a non-generic type.
    let private isClosedNonGeneric
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (target : RuntimeTypeHandleTarget)
        : bool
        =
        match target with
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete _ as handle) ->
            match tryGetConcreteTypeInfo state handle with
            | Some (concreteType, _) -> concreteType.Identity = typeInfo.Identity
            | None -> false
        | _ -> false

    /// The interfaces a nominal MethodTable-backed target declares directly, each read under that
    /// target's own instantiation, so an open construction's interfaces mention its arguments.
    let private declaredInterfaceTargets
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (target : RuntimeTypeHandleTarget)
        : IlMachineState * RuntimeTypeHandleTarget list
        =
        let typeVariables =
            match target with
            | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete _ as handle) ->
                match tryGetConcreteTypeInfo state handle with
                | Some (concreteType, _) -> ReflectedTypeTarget.ReflectionVariableBinding.Bound concreteType.Generics
                | None -> failwith $"logic error: tryGetConcreteTypeInfo refused the Concrete handle %O{handle}"
            | _ ->
                match nominalInstantiation state target with
                | Some (_, arguments) ->
                    ReflectedTypeTarget.ReflectionVariableBinding.Open (ImmutableArray.CreateRange arguments)
                | None -> failwith $"declaredInterfaceTargets: %O{target} is not a nominal MethodTable-backed type"

        let identity =
            match nominalInstantiation state target with
            | Some (identity, _) -> identity
            | None -> failwith $"declaredInterfaceTargets: %O{target} is not a nominal MethodTable-backed type"

        let assy = state._LoadedAssemblies.ByDefinitionName identity.AssemblyFullName
        let typeInfo = typeInfoOfIdentity state identity

        let environment =
            {
                ReflectedTypeTarget.ReflectionTypeEnvironment.TypeVariables = typeVariables
                ReflectedTypeTarget.ReflectionTypeEnvironment.MethodVariables =
                    ReflectedTypeTarget.ReflectionVariableBinding.Open ImmutableArray.Empty
            }

        let state, interfaces =
            ((state, []), typeInfo.ImplementedInterfaces)
            ||> Seq.fold (fun (state, acc) impl ->
                let implAssy =
                    match state.LoadedAssembly impl.RelativeToAssembly.FullName with
                    | Some a -> a
                    | None -> assy

                let state, implTypeDefn, implResolvedAssy =
                    resolveTypeMetadataToken loggerFactory baseClassTypes state implAssy impl.InterfaceHandle

                let state, implTarget =
                    ReflectedTypeTarget.reflectedTypeTarget
                        loggerFactory
                        baseClassTypes
                        "declaredInterfaceTargets"
                        $"an interface implemented by %O{target}"
                        implResolvedAssy
                        environment
                        state
                        implTypeDefn

                state, implTarget :: acc
            )

        state, List.rev interfaces

    /// CoreCLR's `TypeHandle::CanCastTo` over the full `RuntimeTypeHandleTarget` DU: whether a
    /// value of type `source` can be treated as one of type `target`, where either may mention
    /// type variables. This is the relation both the `TypeHandle_CanCastTo_NoCacheLookup` QCall
    /// and generic-constraint validation (`TypeVarTypeDesc::SatisfiesConstraints`) ask.
    ///
    /// Two closed types are answered by `isConcreteTypeAssignableTo`. Otherwise the rules are
    /// CoreCLR's, case by case:
    /// - a type variable source casts to `System.Object`, to `System.ValueType` iff it carries
    ///   the `struct` constraint, and to anything one of its declared constraints casts to
    ///   (`TypeDesc::CanCastTo`);
    /// - nothing but the variable itself casts to a type variable;
    /// - a MethodTable-backed source walks its parent chain (for a class target) or its interface
    ///   map (for an interface target), accepting an exact match or, where the target has
    ///   variant parameters, a same-definition match that `CanCastByVarianceToInterfaceOrDelegate`
    ///   accepts. A variant argument compares by `IsBoxedAndCanCastTo`, under which a type
    ///   variable counts as a reference only when `ConstrainedAsObjRef`;
    /// - an array source casts to an open generic interface only through the implicit `T[]`
    ///   interfaces (`ArraySupportsBizarreInterface`).
    ///
    /// A composite (array, byref, pointer or function pointer) over a type variable, on either
    /// side, is refused loudly.
    let isRuntimeTypeHandleTargetAssignableTo
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (source : RuntimeTypeHandleTarget)
        (target : RuntimeTypeHandleTarget)
        : IlMachineState * bool
        =
        let isObject (state : IlMachineState) (target : RuntimeTypeHandleTarget) : bool =
            isClosedNonGeneric state baseClassTypes.Object target

        // A type variable's metadata (for its flag-style constraints) and its declared
        // constraints as targets.
        let typeVariableFacts
            (state : IlMachineState)
            (variable : RuntimeTypeHandleTarget)
            : IlMachineState * GenericParamMetadata * RuntimeTypeHandleTarget list
            =
            ReflectedTypeTarget.declaredConstraintTargets
                loggerFactory
                baseClassTypes
                "isRuntimeTypeHandleTargetAssignableTo"
                state
                variable

        let isTypeVariable (target : RuntimeTypeHandleTarget) : bool =
            match target with
            | RuntimeTypeHandleTarget.GenericParameter _
            | RuntimeTypeHandleTarget.MethodGenericParameter _ -> true
            | _ -> false

        // `TypeVarTypeDesc::ConstrainedAsObjRefHelper` (typedesc.cpp:1036): some declared
        // constraint is a class other than Object, ValueType and Enum, or is itself a variable so
        // constrained. The `class` flag is deliberately not consulted here: it does not propagate
        // through a variable-to-variable constraint.
        let rec constrainedAsObjRefByConstraints
            (state : IlMachineState)
            (variable : RuntimeTypeHandleTarget)
            : IlMachineState * bool
            =
            let state, _, constraints = typeVariableFacts state variable

            ((state, false), constraints)
            ||> List.fold (fun (state, found) constraintTarget ->
                if found then
                    state, true
                elif isTypeVariable constraintTarget then
                    constrainedAsObjRefByConstraints state constraintTarget
                elif
                    not (isInterfaceTarget state constraintTarget)
                    && isObjRefTarget baseClassTypes state constraintTarget
                    && not (isObject state constraintTarget)
                    && not (isClosedNonGeneric state baseClassTypes.ValueType constraintTarget)
                    && not (isClosedNonGeneric state baseClassTypes.Enum constraintTarget)
                then
                    state, true
                else
                    state, false
            )

        // `TypeVarTypeDesc::ConstrainedAsObjRef` (typedesc.cpp:1006).
        let constrainedAsObjRef (state : IlMachineState) (variable : RuntimeTypeHandleTarget) : IlMachineState * bool =
            let state, metadata, _ = typeVariableFacts state variable

            if metadata.Constraint = Some GenericConstraint.Reference then
                state, true
            else
                constrainedAsObjRefByConstraints state variable

        // `where T : U, U : T` makes the constraint walk below loop, and CoreCLR refuses such a
        // declaration when it loads the type (`TypeVarTypeDesc::LoadConstraints` rejects circular
        // constraints), so no well-formed program reaches it.
        let requireAcyclicVariableConstraints
            (state : IlMachineState)
            (variable : RuntimeTypeHandleTarget)
            : IlMachineState
            =
            let rec walk
                (state : IlMachineState)
                (path : RuntimeTypeHandleTarget list)
                (current : RuntimeTypeHandleTarget)
                =
                if List.contains current path then
                    failwith
                        $"isRuntimeTypeHandleTargetAssignableTo: the type-variable constraints of %O{variable} form a cycle through %O{current}, which CoreCLR refuses at type load"

                let state, _, constraints = typeVariableFacts state current

                (state, constraints |> List.filter isTypeVariable)
                ||> List.fold (fun state next -> walk state (current :: path) next)

            walk state [] variable

        // `visited` is CoreCLR's `TypeHandlePairList`: the (source, target) pairs already being
        // compared further up this path. Revisiting one through variance answers false, exactly
        // as `CanCastByVarianceToInterfaceOrDelegate` does, which is what makes an expansive
        // hierarchy such as `class C : IIn<IIn<C>>` terminate.
        let rec canCast
            (visited : Set<RuntimeTypeHandleTarget * RuntimeTypeHandleTarget>)
            (state : IlMachineState)
            (source : RuntimeTypeHandleTarget)
            (target : RuntimeTypeHandleTarget)
            : IlMachineState * bool
            =
            if source = target then
                state, true
            else

            match source, target with
            // The dynamic-methods class is assignable to nothing but itself, and nothing but itself
            // is assignable to it: `CreateMinimalMethodTable` gives it no parent and no interfaces,
            // so even the "everything is assignable to System.Object" rule does not apply to it.
            | RuntimeTypeHandleTarget.DynamicMethodsClass _, _
            | _, RuntimeTypeHandleTarget.DynamicMethodsClass _ -> state, false
            | RuntimeTypeHandleTarget.Closed s, RuntimeTypeHandleTarget.Closed t ->
                let closedVisited =
                    visited
                    |> Set.toSeq
                    |> Seq.choose (fun pair ->
                        match pair with
                        | RuntimeTypeHandleTarget.Closed a, RuntimeTypeHandleTarget.Closed b -> Some (a, b)
                        | _ -> None
                    )
                    |> Set.ofSeq

                isConcreteTypeAssignableToVisiting closedVisited loggerFactory baseClassTypes state s t
            | (RuntimeTypeHandleTarget.GenericParameter _ | RuntimeTypeHandleTarget.MethodGenericParameter _), _ ->
                // `TypeDesc::CanCastTo` (typedesc.cpp:322). The ValueType arm reads only the
                // `struct` flag, not the declared constraints, so `where T : Enum` does not make
                // `T` castable to ValueType here.
                if isObject state target then
                    state, true
                elif isClosedNonGeneric state baseClassTypes.ValueType target then
                    let state, metadata, _ = typeVariableFacts state source
                    state, metadata.Constraint = Some GenericConstraint.NonNullableValue
                else
                    // Revisiting a (variable, target) pair is legal here: `G<T> where T : IIn<IIn<T>>`
                    // against `class C : IIn<IIn<C>>` comes back to it through variance, and that
                    // recursion ends at `canCastByVariance`'s pair check, as CoreCLR's does. What
                    // would not end is a cycle of variables constraining each other directly,
                    // which CoreCLR refuses at type load, so that is refused here instead.
                    let state = requireAcyclicVariableConstraints state source
                    let state, _, constraints = typeVariableFacts state source

                    ((state, false), constraints)
                    ||> List.fold (fun (state, found) constraintTarget ->
                        if found then
                            state, true
                        else
                            canCast visited state constraintTarget target
                    )
            // `TypeHandle::CanCastTo` (typehandle.cpp:608): a MethodTable never casts to a TypeDesc,
            // and a closed byref, pointer or function pointer is a TypeDesc of a different kind from
            // a type variable, which `TypeDesc::CanCastTo` refuses too.
            | _, (RuntimeTypeHandleTarget.GenericParameter _ | RuntimeTypeHandleTarget.MethodGenericParameter _) ->
                state, false
            | (RuntimeTypeHandleTarget.Composite _ | RuntimeTypeHandleTarget.FunctionPointer _ as composite), _
            | _, (RuntimeTypeHandleTarget.Composite _ | RuntimeTypeHandleTarget.FunctionPointer _ as composite) ->
                RuntimeTypeHandleTarget.refuseComposite "isRuntimeTypeHandleTargetAssignableTo" composite
            // A TypeDesc never casts to a MethodTable (`TypeDesc::CanCastTo`'s last arm).
            | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Byref _ | ConcreteTypeHandle.Pointer _ | ConcreteTypeHandle.FunctionPointer _),
              _ -> state, false
            | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.OneDimArrayZero element), _ when
                isInterfaceTarget state target
                ->
                // `ArraySupportsBizarreInterface` (methodtable.cpp:1433): an szarray is one of its
                // implicit generic interfaces over an element `CanCastParam` accepts. Its
                // primitive-width rule relates two closed primitives, so it cannot fire against
                // an open target's argument; what is left is identity and `IsBoxedAndCanCastTo`.
                match nominalInstantiation state target with
                | Some (identity, [ targetElement ]) when baseClassTypes.IsImplicitInterfaceOfSzArray identity ->
                    let sourceElement = RuntimeTypeHandleTarget.Closed element

                    if sourceElement = targetElement then
                        state, true
                    else
                        isBoxedAndCanCastTo visited state sourceElement targetElement
                | _ -> state, false
            // Any other array against an open target: an array is a class, so it matches an open
            // class only through its parent chain (System.Array, System.Object), which is closed;
            // and it is an interface only through the implicit ones above.
            | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.OneDimArrayZero _ | ConcreteTypeHandle.Array _), _ ->
                state, false
            | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete _), _
            | (RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ | RuntimeTypeHandleTarget.OpenConstructed _), _ ->
                methodTableCanCast visited state source target

        // `TypeHandle::IsBoxedAndCanCastTo` (typehandle.cpp:546), the comparison a variant
        // argument is held to: `from` must be an object reference, which for a type variable means
        // `ConstrainedAsObjRef`, and must then cast to `to`.
        and isBoxedAndCanCastTo
            (visited : Set<RuntimeTypeHandleTarget * RuntimeTypeHandleTarget>)
            (state : IlMachineState)
            (from : RuntimeTypeHandleTarget)
            (``to`` : RuntimeTypeHandleTarget)
            : IlMachineState * bool
            =
            let state, isObjRef =
                if isTypeVariable from then
                    constrainedAsObjRef state from
                else
                    state, isObjRefTarget baseClassTypes state from

            if isObjRef then
                canCast visited state from ``to``
            else
                state, false

        // `MethodTable::CanCastByVarianceToInterfaceOrDelegate` (methodtable.cpp:1242), where
        // `target` is known to be a variant interface or delegate.
        and canCastByVariance
            (visited : Set<RuntimeTypeHandleTarget * RuntimeTypeHandleTarget>)
            (state : IlMachineState)
            (candidate : RuntimeTypeHandleTarget)
            (target : RuntimeTypeHandleTarget)
            : IlMachineState * bool
            =
            match nominalInstantiation state candidate, nominalInstantiation state target with
            | Some (candidateIdentity, candidateArgs), Some (targetIdentity, targetArgs) when
                candidateIdentity = targetIdentity
                && not (Set.contains (candidate, target) visited)
                ->
                let visited = Set.add (candidate, target) visited
                let targetInfo = typeInfoOfIdentity state targetIdentity

                ((state, true), List.zip candidateArgs targetArgs |> List.indexed)
                ||> List.fold (fun (state, ok) (index, (candidateArg, targetArg)) ->
                    if not ok then
                        state, false
                    elif candidateArg = targetArg then
                        state, true
                    else
                        let _, parameterMetadata = targetInfo.Generics.[index]

                        match parameterMetadata.Variance with
                        | None -> state, false
                        | Some GenericVariance.Covariant -> isBoxedAndCanCastTo visited state candidateArg targetArg
                        | Some GenericVariance.Contravariant ->
                            isBoxedAndCanCastTo visited state targetArg candidateArg
                )
            | _ -> state, false

        // `MethodTable::CanCastTo` (methodtable.cpp:1385) for a nominal source that is not an
        // array, against a MethodTable-backed target, at least one of the two being open.
        and methodTableCanCast
            (visited : Set<RuntimeTypeHandleTarget * RuntimeTypeHandleTarget>)
            (state : IlMachineState)
            (source : RuntimeTypeHandleTarget)
            (target : RuntimeTypeHandleTarget)
            : IlMachineState * bool
            =
            match target with
            | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.OneDimArrayZero _ | ConcreteTypeHandle.Array _) ->
                state, false
            | _ when isObject state target ->
                // Every reference type (including an interface) and every boxed value type casts to
                // System.Object, which is also where `isConcreteTypeAssignableTo` ends its walk.
                state, true
            | _ ->

            let targetIdentity =
                match nominalInstantiation state target with
                | Some (identity, _) -> identity
                | None -> failwith $"logic error: methodTableCanCast reached with the non-nominal target %O{target}"

            let targetInfo = typeInfoOfIdentity state targetIdentity

            let targetIsVariant =
                targetInfo.Generics
                |> Seq.exists (fun (_, metadata) -> metadata.Variance.IsSome)

            let matches (state : IlMachineState) (candidate : RuntimeTypeHandleTarget) : IlMachineState * bool =
                if candidate = target then
                    state, true
                elif targetIsVariant then
                    canCastByVariance visited state candidate target
                else
                    state, false

            let rec anyMatch (state : IlMachineState) (candidates : RuntimeTypeHandleTarget list) =
                match candidates with
                | [] -> state, false
                | candidate :: rest ->
                    let state, found = matches state candidate
                    if found then state, true else anyMatch state rest

            // `source` and its parents, most derived first.
            let rec parentChain
                (state : IlMachineState)
                (acc : RuntimeTypeHandleTarget list)
                (current : RuntimeTypeHandleTarget)
                : IlMachineState * RuntimeTypeHandleTarget list
                =
                let state, parent =
                    resolveBaseRuntimeTypeHandleTarget loggerFactory baseClassTypes state current

                match parent with
                | None -> state, List.rev (current :: acc)
                | Some parent -> parentChain state (current :: acc) parent

            let state, chain = parentChain state [] source

            if targetInfo.IsInterface then
                // CoreCLR's interface map: every interface of every type in the chain, closed
                // under the interfaces' own interfaces (`ExpandApproxInterface` recurses; Roslyn
                // happens to list the closure on every type anyway, but ECMA-335 does not require
                // it). The source itself stays a candidate, for an interface source.
                let rec closeOver
                    (state : IlMachineState)
                    (seen : Set<RuntimeTypeHandleTarget>)
                    (pending : RuntimeTypeHandleTarget list)
                    : IlMachineState * Set<RuntimeTypeHandleTarget>
                    =
                    match pending with
                    | [] -> state, seen
                    | next :: rest ->
                        if Set.contains next seen then
                            closeOver state seen rest
                        else
                            let state, declared =
                                declaredInterfaceTargets loggerFactory baseClassTypes state next

                            closeOver state (Set.add next seen) (declared @ rest)

                let state, fromChain =
                    ((state, []), chain)
                    ||> List.fold (fun (state, acc) current ->
                        let state, declared =
                            declaredInterfaceTargets loggerFactory baseClassTypes state current

                        state, acc @ declared
                    )

                let state, sourceMatches = matches state source

                if sourceMatches then
                    state, true
                else

                let targetIsSpecialMarker =
                    match target with
                    | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ -> targetIsVariant
                    | _ -> false

                let state, interfaceMap = closeOver state Set.empty fromChain
                let state, scanMatches = anyMatch state (Set.toList interfaceMap)

                if not scanMatches then
                    state, false
                elif
                    targetIsSpecialMarker
                    && (isInterfaceTarget state source
                        || not (isObjRefTarget baseClassTypes state source))
                then
                    // `CanCastToInterface` (methodtable.cpp:1228): a variant interface's typical
                    // instantiation is also the "special marker" CoreCLR compresses a value type's
                    // or interface's interface map with, so it refuses to scan such a map for one
                    // unless `MayHaveOpenInterfacesInInterfaceMap` is set. Measured:
                    // `typeof(IIn<>).IsAssignableFrom` is true of a class implementing `IIn<object>`
                    // and false of a struct or interface doing the same.
                    match source with
                    | RuntimeTypeHandleTarget.Closed _ ->
                        // The flag is set on a value type or interface only when it is shared by
                        // generic instantiations, which PawPrint's are not, or when a closed entry
                        // of its map would itself be a marker, which no closed instantiation is.
                        state, false
                    | _ ->
                        // A scan that finds nothing answers false whatever the flag says; one that
                        // finds a match is answered by the flag alone.
                        failwith
                            $"TODO: isRuntimeTypeHandleTargetAssignableTo: the open value type or interface %O{source} has an interface that casts to the variant interface definition %O{target}, but whether CoreCLR scans for it depends on its MayHaveOpenInterfacesInInterfaceMap flag, which PawPrint does not model for open types"
                else
                    state, true
            else
                anyMatch state chain

        canCast Set.empty state source target
