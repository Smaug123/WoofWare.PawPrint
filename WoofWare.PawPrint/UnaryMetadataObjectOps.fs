namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module internal UnaryMetadataObjectOps =
    /// ECMA-335 III.4.3 (`castclass`) and III.4.33 (`unbox.any` whose type token denotes a
    /// reference type) specify identical behaviour once the operand and the target type are in
    /// hand: a null operand passes through; an operand whose runtime type is assignable to the
    /// target passes through unchanged; anything else raises InvalidCastException.
    ///
    /// `opName` appears only in the diagnostic for eval-stack shapes we do not model.
    let private castToReferenceType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (opName : string)
        (thread : ThreadId)
        (targetConcreteType : ConcreteTypeHandle)
        (actualObj : EvalStackValue)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        match actualObj with
        | EvalStackValue.NullObjectRef ->
            // Per ECMA-335 III.4.3: null ref is always valid for a cast to a reference type.
            let state =
                state
                |> IlMachineState.pushToEvalStack' EvalStackValue.NullObjectRef thread
                |> IlMachineState.advanceProgramCounter thread

            state, WhatWeDid.Executed
        | EvalStackValue.ObjectRef addr ->
            // `getObjectConcreteType` consults both the array and the non-array side of the heap,
            // so array operands need no special-casing here; `isConcreteTypeAssignableTo` already
            // understands array handles (rank, element covariance, the SZ-array implicit generic
            // interfaces, and the `System.Array` base chain).
            let objConcreteType = ManagedHeap.getObjectConcreteType addr state.ManagedHeap

            let state, isAssignable =
                IlMachineState.isConcreteTypeAssignableTo
                    loggerFactory
                    baseClassTypes
                    state
                    objConcreteType
                    targetConcreteType

            if isAssignable then
                let state =
                    state
                    |> IlMachineState.pushToEvalStack' actualObj thread
                    |> IlMachineState.advanceProgramCounter thread

                state, WhatWeDid.Executed
            else
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    baseClassTypes
                    baseClassTypes.InvalidCastException
                    thread
                    state
        | other -> failwith $"%s{opName}: unexpected eval stack value {other}"

    let executeCastclass (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let actualObj, state = IlMachineState.popEvalStack thread state

        let state, targetType, _targetAssy =
            IlMachineState.resolveTypeMetadataToken
                loggerFactory
                baseClassTypes
                state
                activeAssy
                ImmutableArray.Empty
                metadataToken

        let state, targetConcreteType =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                activeAssy.Name
                currentMethod.DeclaringType.Generics
                currentMethod.Generics
                targetType

        castToReferenceType loggerFactory baseClassTypes "Castclass" thread targetConcreteType actualObj state

    /// Implements `newobj T[<rank>]::.ctor(int32, ..., int32)` — the runtime-synthesized constructor
    /// for a multi-dimensional array of element type `elementType`. Pops `rank` Int32 lengths off
    /// the eval stack (top-of-stack is the rightmost argument), allocates a zero-initialised
    /// row-major buffer via `IlMachineState.allocateMultiDimArray`, and pushes the resulting
    /// object reference. ECMA-335 II.14.2 also defines a `2*rank`-parameter form for non-zero
    /// lower bounds; that is not yet implemented (C# never emits it).
    let private executeMultiDimArrayNewobj
        (ctx : UnaryMetadataIlOpContext)
        (state : IlMachineState)
        (elementType : TypeDefn)
        (rank : int)
        (signature : MemberSignature)
        : IlMachineState * WhatWeDid
        =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        // ECMA-335 II.14.2 / CoreCLR: a rank-1 ELEMENT_TYPE_ARRAY constructor
        // (`newobj instance void T[0...]::.ctor(int32)`) morphs at runtime to an
        // SZARRAY (`T[]`) — the resulting object's type identity is the SZARRAY,
        // not a rank-1 MdArray, which is observable through GetType, casts and
        // assignability. We don't yet implement that morphing, so reject the
        // rank-1 constructor form rather than silently producing a
        // `ConcreteTypeHandle.Array(_, 1)` with the wrong runtime type. C# never
        // emits this form, so this path is exercised only by hand-rolled IL.
        if rank = 1 then
            failwith
                "TODO: rank-1 ELEMENT_TYPE_ARRAY newobj should morph to SZARRAY (OneDimArrayZero) per CoreCLR semantics; not yet implemented"

        let methodSig =
            match signature with
            | MemberSignature.Method m -> m
            | MemberSignature.Field _ ->
                failwith
                    $"BUG: multi-dim array newobj for rank %d{rank} had a field signature; expected method signature"

        let paramCount = methodSig.ParameterTypes.Length

        if paramCount <> rank then
            failwith
                $"TODO: multi-dim array newobj for rank %d{rank} has %d{paramCount} parameters; only the zero-lower-bound form (%d{rank} Int32 lengths) is implemented"

        for paramTy in methodSig.ParameterTypes do
            match paramTy with
            | TypeDefn.PrimitiveType PrimitiveType.Int32 -> ()
            | other ->
                failwith
                    $"TODO: multi-dim array newobj for rank %d{rank} has non-Int32 parameter type %O{other}; only Int32 lengths are supported"

        // Pop `rank` Int32 lengths off the eval stack. The top of stack is the rightmost
        // argument (i.e. dimension index rank-1), so fill the array right-to-left.
        let lengths = Array.zeroCreate<int> rank
        let mutable s = state

        for i = rank - 1 downto 0 do
            let v, s' = IlMachineState.popEvalStack thread s

            match v with
            | EvalStackValue.Int32 (Int32Source.Verbatim n) ->
                lengths.[i] <- n
                s <- s'
            | other ->
                failwith $"unexpectedly popped non-Int32 value %O{other} as multi-dim array length at dimension %d{i}"

        let dimensionLengths = lengths |> ImmutableArray.CreateRange
        let state = s

        let typeGenerics = currentMethod.DeclaringType.Generics
        let methodGenerics = currentMethod.Generics

        let state, zeroOfType, elementHandle =
            IlMachineState.cliTypeZeroOf
                loggerFactory
                baseClassTypes
                activeAssy
                elementType
                typeGenerics
                methodGenerics
                state

        let arrayType = ConcreteTypeHandle.Array (elementHandle, rank)

        let alloc, state =
            IlMachineState.allocateMultiDimArray arrayType (fun () -> zeroOfType) dimensionLengths state

        let state =
            state
            |> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some alloc)) thread
            |> IlMachineState.advanceProgramCounter thread

        state, WhatWeDid.Executed

    let executeNewobj (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let thread = ctx.Thread
        let logger = ctx.Logger

        let heapValueByref (addr : ManagedHeapAddress) : ManagedPointerSource =
            ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, [])

        // Multi-dimensional array constructors are runtime-synthesized (ECMA-335 II.14.2): the
        // metadata token is a MemberReference whose parent is a TypeSpec of TypeDefn.Array.
        // There's no managed body to resolve, so detect that shape up front and route to the
        // multi-dim allocation path. szarrays still go through `newarr`, not `newobj`, so we
        // don't need to handle TypeDefn.OneDimensionalArrayLowerBoundZero here.
        let multiDimSpec =
            match metadataToken with
            | MemberReference mrHandle ->
                match activeAssy.Members.TryGetValue mrHandle with
                | true, memberRef ->
                    match memberRef.Parent with
                    | MetadataToken.TypeSpecification specHandle ->
                        match activeAssy.TypeSpecs.TryGetValue specHandle with
                        | true, ts ->
                            match ts.Signature with
                            | TypeDefn.Array (elt, rank) -> Some (elt, rank, memberRef.Signature)
                            | _ -> None
                        | false, _ -> None
                    | _ -> None
                | false, _ -> None
            | _ -> None

        match multiDimSpec with
        | Some (elementType, rank, sig0) -> executeMultiDimArrayNewobj ctx state elementType rank sig0
        | None ->

        let state, ctor, typeArgsFromMetadata =
            match metadataToken with
            | MethodDef md ->
                let method = activeAssy.Methods.[md]

                state, MethodInfo.mapTypeGenerics (fun _ -> failwith "non-generic method") method, None
            | MemberReference mr ->
                let state, _, method, extractedTypeArgs =
                    IlMachineState.resolveMember
                        loggerFactory
                        baseClassTypes
                        thread
                        activeAssy
                        ImmutableArray.Empty
                        mr
                        state

                match method with
                | Choice1Of2 mr -> state, mr, Some extractedTypeArgs
                | Choice2Of2 _field -> failwith "unexpectedly NewObj found a constructor which is a field"
            | x -> failwith $"Unexpected metadata token for constructor: %O{x}"

        let state, concretizedCtor, declaringTypeHandle =
            ExecutionConcretization.concretizeMethodForExecution
                loggerFactory
                baseClassTypes
                thread
                ctor
                None
                typeArgsFromMetadata
                state

        let state, init =
            IlMachineStateExecution.ensureTypeInitialised loggerFactory baseClassTypes thread declaringTypeHandle state

        match init with
        // Park this thread on the other thread's in-progress cctor, exactly as `call`/`callvirt`
        // (UnaryMetadataCallOps) and the static-field ops (UnaryMetadataFieldOps) already do.
        // Nothing has been popped yet — the constructor's arguments are consumed later, inside
        // `callMethod` — and the program counter has not been advanced, so when the scheduler
        // wakes us we simply re-execute this `newobj` from the top. `ensureTypeInitialised`
        // returns the state unmodified on this path, and the work done above it (assembly
        // loading, member resolution, concretization) is idempotent cache population, so the
        // retry observes no partial effect of this attempt. Unlike the call ops there is no
        // `constrained.` prefix to reinstate, because `newobj` cannot carry one.
        | WhatWeDid.BlockedOnClassInit blockedBy -> state, WhatWeDid.BlockedOnClassInit blockedBy
        | WhatWeDid.SuspendedForClassInit -> state, WhatWeDid.SuspendedForClassInit
        | WhatWeDid.SuspendedForManagedCall ->
            failwith "logic error: ensureTypeInitialised cannot suspend for an arbitrary managed call"
        | WhatWeDid.ThrowingTypeInitializationException -> state, WhatWeDid.ThrowingTypeInitializationException
        | WhatWeDid.VoluntaryYield _ -> failwith "logic error: ensureTypeInitialised cannot produce a VoluntaryYield"
        | WhatWeDid.Executed ->

        let ctorAssembly = state.LoadedAssembly ctor.DeclaringType.Assembly |> Option.get
        let ctorType = ctorAssembly.TypeDefs.[ctor.DeclaringType.Definition.Get]

        do
            logger.LogDebug (
                "Creating object of type {ConstructorAssembly}.{ConstructorType}",
                ctorAssembly.Name.Name,
                ctorType.Name
            )

        // The CLI's variable-size-object case: types whose instance size depends on the
        // constructor arguments, which CoreCLR flags `CORINFO_FLG_VAROBJSIZE` (set whenever
        // the MethodTable `HasComponentSize` — see `vm/jitinterface.cpp`). The runtime cannot
        // allocate before the constructor runs, so it allocates nothing and passes no `this`
        // (`jit/importer.cpp`, CEE_NEWOBJ: "At present this can only be String",
        // `newObjThisPtr = nullptr`; `interpreter/compiler.cpp`, `doCallInsteadOfNew = true`).
        //
        // Arrays are the CLI's only other variable-size case and never reach here:
        // multi-dimensional array constructors were diverted to `executeMultiDimArrayNewobj`
        // above, and szarrays go through `newarr` rather than `newobj`. So, exactly as CoreCLR
        // asserts, this is System.String and nothing else.
        //
        // Every `System.String` constructor is declared `extern` with
        // `MethodImplOptions.InternalCall` and has an empty body; the *implementation* is the
        // sibling managed static `String.Ctor` of the same parameter signature, returning
        // `string`. CoreCLR wires the two together in `vm/ecall.cpp`
        // (`PopulateManagedStringConstructors`), which walks the nine `METHOD__STRING__CTORF_*`
        // binder entries and dynamically assigns each `Ctor` method's own compiled code as the
        // ctor's FCall implementation. So a `newobj` on String really does execute CoreLib IL —
        // `Ctor`'s — and we reproduce that by redirecting the call here rather than
        // hand-implementing each overload at the native boundary.
        //
        // The stack shapes line up exactly: `newobj` has pushed the N constructor arguments and
        // no `this`, which is precisely what a static N-ary `Ctor` pops, and `Ctor`'s `string`
        // return value is pushed to the caller by the ordinary `NotConstructing` return path —
        // which is what `newobj` must leave behind.
        if TypeInfo.NominallyEqual ctorType baseClassTypes.String then
            let ctorImplementation =
                ctorType.Methods
                |> List.filter (fun candidate ->
                    candidate.Name = "Ctor"
                    && candidate.IsStatic
                    && candidate.RawSignature.ParameterTypes = ctor.RawSignature.ParameterTypes
                )

            let describedSignature : string =
                ctor.RawSignature.ParameterTypes |> List.map string |> String.concat ", "

            let ctorImplementation =
                match ctorImplementation with
                | [ single ] -> single
                | [] ->
                    failwith
                        $"newobj on System.String::.ctor(%s{describedSignature}) found no matching static String.Ctor to redirect to. CoreCLR implements every string constructor as its same-signature `Ctor` sibling (vm/ecall.cpp, PopulateManagedStringConstructors); a missing one means this CoreLib declares a constructor overload we do not know about."
                | _ :: _ :: _ ->
                    failwith
                        $"newobj on System.String::.ctor(%s{describedSignature}) found several matching static String.Ctor overloads; the parameter signature should identify exactly one."

            match ctorImplementation.RawSignature.ReturnType with
            | MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.String) -> ()
            | other ->
                failwith
                    $"String.Ctor selected for newobj returns %O{other}; every String.Ctor overload must return String, because its return value is what newobj pushes."

            // String is non-generic, so there are no type generics to substitute, and no
            // `Ctor` overload is itself generic.
            let state, concretizedCtorImplementation, _ =
                ExecutionConcretization.concretizeMethodWithTypeGenerics
                    loggerFactory
                    baseClassTypes
                    ImmutableArray.Empty
                    ctorImplementation
                    None
                    ctorAssembly.Name
                    ImmutableArray.Empty
                    state

            let threadState = state.ThreadState.[thread]

            IlMachineStateExecution.callMethod
                loggerFactory
                baseClassTypes
                None
                ConstructionState.NotConstructing
                false
                false
                true
                concretizedCtorImplementation.Generics
                concretizedCtorImplementation
                thread
                threadState
                None
                ConstructedObjectDisposition.PushToCaller
                false // wrapExceptionInTargetInvocation
                state,
            WhatWeDid.Executed
        else

        let state, allFields =
            IlMachineState.collectAllInstanceFields loggerFactory baseClassTypes state declaringTypeHandle

        let fields =
            CliValueType.OfFields
                baseClassTypes
                state.ConcreteTypes
                declaringTypeHandle
                ctorType.Layout
                (CharSetMetadata.ofTypeAttributes ctorType.TypeAttributes)
                allFields

        // Note: this is a bit unorthodox for value types, which *aren't* heap-allocated.
        // We'll perform their construction on the heap, though, to keep the interface
        // of Newobj uniform.
        // On completion of the constructor, we'll copy the value back off the heap,
        // and put it on the eval stack directly.
        let allocatedAddr, state =
            let ty =
                AllConcreteTypes.findExistingConcreteType
                    state.ConcreteTypes
                    concretizedCtor.DeclaringType.Identity
                    concretizedCtor.DeclaringType.Generics
                |> Option.get

            IlMachineState.allocateManagedObject ty fields state

        let state =
            if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies ctorType then
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer (heapValueByref allocatedAddr)) thread
            else
                state
                |> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some allocatedAddr)) thread

        let threadState = state.ThreadState.[thread]

        IlMachineStateExecution.callMethod
            loggerFactory
            baseClassTypes
            None
            (ConstructionState.Constructing allocatedAddr)
            false
            false
            true
            concretizedCtor.Generics
            concretizedCtor
            thread
            threadState
            None
            ConstructedObjectDisposition.PushToCaller
            false // wrapExceptionInTargetInvocation
            state,
        WhatWeDid.Executed

    let executeBox (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let state, ty, assy =
            match metadataToken with
            | MetadataToken.TypeDefinition h ->
                let state, ty = IlMachineState.lookupTypeDefn baseClassTypes state activeAssy h
                state, ty, activeAssy
            | MetadataToken.TypeReference ref ->
                IlMachineState.lookupTypeRef
                    loggerFactory
                    baseClassTypes
                    state
                    activeAssy
                    currentMethod.DeclaringType.Generics
                    ref
            | MetadataToken.TypeSpecification spec -> state, activeAssy.TypeSpecs.[spec].Signature, activeAssy
            | _ -> failwith $"unexpected token {metadataToken} in Box"

        let state, typeHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                assy.Name
                currentMethod.DeclaringType.Generics
                currentMethod.Generics
                ty

        let toBox, state = state |> IlMachineState.popEvalStack thread

        // ECMA-335 III.4.1: structural reference-type tokens (szarrays and multi-dim arrays)
        // make `box` a no-op — the value already on the stack is a reference. Byref and
        // pointer tokens are unverifiable for `box`. FunctionPointer is similarly not boxable.
        match typeHandle with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            state
            |> IlMachineState.pushToEvalStack' toBox thread
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed
        | ConcreteTypeHandle.Byref _ ->
            failwithf "Box: byref types cannot be boxed (unverifiable IL); typeHandle=%O" typeHandle
        | ConcreteTypeHandle.Pointer _ ->
            failwithf "Box: pointer types cannot be boxed (unverifiable IL); typeHandle=%O" typeHandle
        | ConcreteTypeHandle.FunctionPointer _ ->
            failwithf "TODO: Box of function pointer type not implemented; typeHandle=%O" typeHandle
        | ConcreteTypeHandle.Concrete _ ->

        let targetType =
            AllConcreteTypes.lookup typeHandle state.ConcreteTypes |> Option.get

        let defn =
            state._LoadedAssemblies.[targetType.Assembly].TypeDefs.[targetType.Definition.Get]

        let isNullable =
            InternalTypeKind.kind baseClassTypes targetType = InternalTypeKind.Nullable

        let toPush, state =
            if isNullable then
                // Nullable<T> boxing: null when !HasValue, box underlying T when HasValue.
                match toBox with
                | EvalStackValue.UserDefinedValueType cvt ->
                    let hasValueField =
                        IlMachineState.requiredOwnInstanceFieldId state cvt.Declared "hasValue"

                    let hasValue = CliValueType.DereferenceFieldById hasValueField cvt

                    match hasValue with
                    | CliType.Bool 0uy ->
                        // Nullable with HasValue=false boxes to null.
                        EvalStackValue.NullObjectRef, state
                    | CliType.Bool _ ->
                        // Nullable with HasValue=true: box the underlying value as T.
                        let underlyingTypeHandle = targetType.Generics.[0]

                        let valueField =
                            IlMachineState.requiredOwnInstanceFieldId state cvt.Declared "value"

                        let value = CliValueType.DereferenceFieldById valueField cvt

                        let cvt, state =
                            match value with
                            | CliType.ValueType existingCvt ->
                                // Multi-field struct: use the stored CliValueType directly.
                                existingCvt, state
                            | _ ->
                                // Primitive or single-field: reconstruct from type definition.
                                let underlyingConcreteType =
                                    AllConcreteTypes.lookup underlyingTypeHandle state.ConcreteTypes |> Option.get

                                let underlyingDefn =
                                    state._LoadedAssemblies.[underlyingConcreteType.Assembly].TypeDefs
                                        .[underlyingConcreteType.Definition.Get]

                                let underlyingInstanceFields =
                                    underlyingDefn.Fields
                                    |> List.filter (fun field -> not (field.Attributes.HasFlag FieldAttributes.Static))

                                let underlyingAssembly = state._LoadedAssemblies.[underlyingConcreteType.Assembly]

                                let valueAsEval = EvalStackValue.ofCliType value

                                let state, fieldValues =
                                    ((state, []), underlyingInstanceFields)
                                    ||> List.fold (fun (state, acc) field ->
                                        let state, fieldZero, fieldTypeHandle =
                                            IlMachineState.cliTypeZeroOf
                                                loggerFactory
                                                baseClassTypes
                                                underlyingAssembly
                                                field.Signature
                                                underlyingConcreteType.Generics
                                                ImmutableArray.Empty
                                                state

                                        let coerced = EvalStackValue.toCliTypeCoerced fieldZero valueAsEval

                                        let cliField : CliField =
                                            {
                                                Id = FieldId.metadata underlyingTypeHandle field.Handle field.Name
                                                Name = field.Name
                                                Contents = coerced
                                                Offset = field.Offset
                                                Type = fieldTypeHandle
                                                MarshallingDescriptor = field.MarshallingDescriptor
                                            }

                                        state, cliField :: acc
                                    )

                                List.rev fieldValues
                                // Not reachable for an inline array — an N>1 inline array is never
                                // primitive-like, so it always arrives here as
                                // `UserDefinedValueType` and takes the branch above — but routed
                                // through the expansion anyway so the invariant is enforced by the
                                // one shared helper rather than assumed at each site.
                                |> InlineArrayStorage.expand
                                    (fun () -> $"%s{underlyingDefn.Namespace}.%s{underlyingDefn.Name}")
                                    underlyingDefn.Layout
                                    (InlineArrayStorage.effectiveLength
                                        (DumpedAssembly.isValueType
                                            baseClassTypes
                                            state._LoadedAssemblies
                                            underlyingDefn)
                                        underlyingDefn.InlineArrayLength)
                                |> CliValueType.OfFields
                                    baseClassTypes
                                    state.ConcreteTypes
                                    underlyingTypeHandle
                                    underlyingDefn.Layout
                                    (CharSetMetadata.ofTypeAttributes underlyingDefn.TypeAttributes),
                                state

                        let addr, state =
                            IlMachineState.allocateManagedObject underlyingTypeHandle cvt state

                        EvalStackValue.ObjectRef addr, state
                    | other -> failwith $"Box Nullable: expected Bool for hasValue field, got %O{other}"
                | _ -> failwith $"Box Nullable: expected UserDefinedValueType on eval stack, got %O{toBox}"
            elif DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies defn then
                // Boxing a value type: wrap it in a heap object and push an ObjectRef
                let cvt, state =
                    match toBox with
                    | EvalStackValue.UserDefinedValueType cvt ->
                        // Already have the CliValueType with the right field structure
                        cvt, state
                    | _ ->
                        // Primitive value on the eval stack (Int32, Int64, Float, etc.)
                        // Construct a CliValueType from the type definition's instance fields
                        let targetAssembly = state._LoadedAssemblies.[targetType.Assembly]

                        let instanceFields =
                            defn.Fields
                            |> List.filter (fun field -> not (field.Attributes.HasFlag FieldAttributes.Static))

                        let state, fieldValues =
                            ((state, []), instanceFields)
                            ||> List.fold (fun (state, acc) field ->
                                let state, fieldZero, fieldTypeHandle =
                                    IlMachineState.cliTypeZeroOf
                                        loggerFactory
                                        baseClassTypes
                                        targetAssembly
                                        field.Signature
                                        targetType.Generics
                                        ImmutableArray.Empty
                                        state

                                let coerced = EvalStackValue.toCliTypeCoerced fieldZero toBox

                                let cliField : CliField =
                                    {
                                        Id = FieldId.metadata typeHandle field.Handle field.Name
                                        Name = field.Name
                                        Contents = coerced
                                        Offset = field.Offset
                                        Type = fieldTypeHandle
                                        MarshallingDescriptor = field.MarshallingDescriptor
                                    }

                                state, cliField :: acc
                            )

                        let cvt =
                            List.rev fieldValues
                            // As above: unreachable for an inline array, but routed through the
                            // shared expansion rather than relying on that being true here.
                            |> InlineArrayStorage.expand
                                (fun () -> $"%s{defn.Namespace}.%s{defn.Name}")
                                defn.Layout
                                (InlineArrayStorage.effectiveLength
                                    (DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies defn)
                                    defn.InlineArrayLength)
                            |> CliValueType.OfFields
                                baseClassTypes
                                state.ConcreteTypes
                                typeHandle
                                defn.Layout
                                (CharSetMetadata.ofTypeAttributes defn.TypeAttributes)

                        cvt, state

                let addr, state = IlMachineState.allocateManagedObject typeHandle cvt state

                EvalStackValue.ObjectRef addr, state
            else
                // Reference type: box is a no-op, value passes through unchanged
                toBox, state

        state
        |> IlMachineState.pushToEvalStack' toPush thread
        |> IlMachineState.advanceProgramCounter thread
        |> Tuple.withRight WhatWeDid.Executed

    let executeIsinst (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let actualObj, state = IlMachineState.popEvalStack thread state

        let state, targetType, _targetAssy =
            IlMachineState.resolveTypeMetadataToken
                loggerFactory
                baseClassTypes
                state
                activeAssy
                ImmutableArray.Empty
                metadataToken

        let state, targetConcreteType =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                activeAssy.Name
                currentMethod.DeclaringType.Generics
                currentMethod.Generics
                targetType

        let isinstCheck
            (state : IlMachineState)
            (objConcreteType : ConcreteTypeHandle)
            (successValue : EvalStackValue)
            : IlMachineState * EvalStackValue
            =
            let state, result =
                IlMachineState.isConcreteTypeAssignableTo
                    loggerFactory
                    baseClassTypes
                    state
                    objConcreteType
                    targetConcreteType

            if result then
                state, successValue
            else
                state, EvalStackValue.NullObjectRef

        let state, returnObj =
            match actualObj with
            | EvalStackValue.NullObjectRef ->
                // null IsInstance check always succeeds and results in a null reference
                state, EvalStackValue.NullObjectRef
            | EvalStackValue.ObjectRef addr ->
                let concreteType = ManagedHeap.getObjectConcreteType addr state.ManagedHeap
                isinstCheck state concreteType actualObj
            | EvalStackValue.ManagedPointer src ->
                match IlMachineState.readManagedByref baseClassTypes state src with
                | CliType.ObjectRef None -> state, EvalStackValue.NullObjectRef
                | CliType.ObjectRef (Some addr) ->
                    let concreteType = ManagedHeap.getObjectConcreteType addr state.ManagedHeap
                    isinstCheck state concreteType (EvalStackValue.ObjectRef addr)
                | other -> failwith $"TODO: Isinst on managed pointer to non-object-ref {other}"
            | esv -> failwith $"TODO: Isinst on {esv}"

        let state =
            state
            |> IlMachineState.pushToEvalStack' returnObj thread
            |> IlMachineState.advanceProgramCounter thread

        state, WhatWeDid.Executed

    /// The CLI value logically held by a boxed object whose runtime type is `handle`; the inverse
    /// of the shape `executeBox` writes. Callers must already have established that
    /// `contents.Declared = handle` — both `executeBox` paths guarantee it, by constructing the
    /// heap object's contents with `CliValueType.OfFields ... handle`.
    ///
    /// Three shapes come back out, matching the three `executeBox` writes:
    ///   - primitive-like (IntPtr, RuntimeTypeHandle, an enum, ...): keep it wrapped, since the
    ///     push path flattens it via the `PrimitiveLikeKind` invariant;
    ///   - a genuine multi-field value type: keep it wrapped;
    ///   - a bare primitive (Int32, Float64, ...), which `box` stored in a synthetic single-field
    ///     struct: read field 0 back by offset and size. `box` guarantees that shape, so this is a
    ///     nominal dereference rather than a structural guess.
    /// `Some zero` exactly when `executeBox` stored a *bare* primitive inside a synthetic
    /// single-field struct, `zero` being the zero of that primitive (whose size is the field's
    /// extent). `None` when the boxed storage is the value type's own fields — either because it
    /// is primitive-like (IntPtr, RuntimeTypeHandle, an enum, ...) and stays wrapped, or because
    /// it is a genuine value type.
    ///
    /// This distinction is what separates "a byref to the box addresses the value directly" from
    /// "it addresses a wrapper around the value", so both `unboxedContents` and `executeUnbox`
    /// hang off it.
    let private barePrimitiveBoxShape
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (handle : ConcreteTypeHandle)
        (contents : CliValueType)
        (state : IlMachineState)
        : CliType option * IlMachineState
        =
        if contents.PrimitiveLikeKind.IsSome then
            None, state
        else
            let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes handle

            match zero with
            | CliType.ValueType _ -> None, state
            | bare -> Some bare, state

    let private unboxedContents
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (handle : ConcreteTypeHandle)
        (contents : CliValueType)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let shape, state = barePrimitiveBoxShape baseClassTypes handle contents state

        match shape with
        | None -> CliType.ValueType contents, state
        | Some zero ->
            let size = (CliType.SizeOf zero).Size
            CliValueType.DereferenceFieldAt 0 size contents, state

    /// The outcome of the type test that ECMA-335 III.4.32 (`unbox`) and the value-type arm of
    /// III.4.33 (`unbox.any`) share; CoreCLR routes both through `CastHelpers.Unbox_Helper`.
    [<RequireQualifiedAccess>]
    type private UnboxTypeTest =
        /// The operand is a boxed value whose type the token accepts. Materialise from
        /// `boxed.ConcreteType` rather than from the token's handle: under the enum/underlying
        /// relaxation in `unboxPermitted` the two differ, and `Contents` was built with the former.
        | Accepted of addr : ManagedHeapAddress * boxed : AllocatedNonArrayObject
        /// The operand is null. `unbox` and the non-Nullable arm of `unbox.any` both raise
        /// NullReferenceException; only the `Nullable<T>` arm of `unbox.any` accepts null, and it
        /// never reaches this test.
        | NullOperand
        /// InvalidCastException: the operand is not a boxed value type the token accepts.
        | WrongType

    /// Shared by `unbox` and the value-type arm of `unbox.any`, so the two cannot drift apart on
    /// which operands they accept. `opName` appears only in diagnostics for shapes we do not model.
    let private unboxTypeTest
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (opName : string)
        (targetConcreteTypeHandle : ConcreteTypeHandle)
        (actualObj : EvalStackValue)
        (state : IlMachineState)
        : IlMachineState * UnboxTypeTest
        =
        match actualObj with
        | EvalStackValue.NullObjectRef -> state, UnboxTypeTest.NullOperand
        | EvalStackValue.ObjectRef addr ->
            let boxedOpt =
                match state.ManagedHeap.NonArrayObjects.TryGetValue addr with
                | true, v -> Some v
                | false, _ ->
                    match state.ManagedHeap.Arrays.TryGetValue addr with
                    // An array is never a boxed value type, so per the CLR this is an ordinary
                    // type mismatch rather than an interpreter abort.
                    | true, _ -> None
                    | false, _ -> failwith $"%s{opName}: could not find managed object with address {addr}"

            match boxedOpt with
            | None -> state, UnboxTypeTest.WrongType
            | Some boxed ->
                // Handle identity, or same-primitive-element-type per CoreCLR
                // `CastHelpers.Unbox_Helper` — the clause that lets a boxed enum unbox to its
                // underlying integer and back. Not assignability, and narrower than ECMA-335's
                // verification types: see `unboxPermitted`.
                let state, permitted =
                    IlMachineState.unboxPermitted
                        loggerFactory
                        baseClassTypes
                        state
                        boxed.ConcreteType
                        targetConcreteTypeHandle

                if permitted then
                    state, UnboxTypeTest.Accepted (addr, boxed)
                else
                    state, UnboxTypeTest.WrongType
        | other -> failwith $"%s{opName}: unexpected eval stack value {other}"

    let executeUnboxAny (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        // ECMA-335 III.4.33
        let actualObj, state = IlMachineState.popEvalStack thread state

        let state, targetType, _targetAssy =
            IlMachineState.resolveTypeMetadataToken
                loggerFactory
                baseClassTypes
                state
                activeAssy
                ImmutableArray.Empty
                metadataToken

        let state, targetConcreteTypeHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                activeAssy.Name
                currentMethod.DeclaringType.Generics
                currentMethod.Generics
                targetType

        // The type token need not denote a nominal type: `unbox.any !!T` with `T = int[]`
        // concretizes to a structural array handle, which by design has no row in
        // `AllConcreteTypes` and no TypeDef to interrogate. Dispatch on the shape of the handle
        // before touching any metadata.
        match targetConcreteTypeHandle with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            // Array types are reference types, and are never `Nullable<T>`, so III.4.33 reduces
            // to castclass.
            castToReferenceType
                loggerFactory
                baseClassTypes
                "Unbox_Any (reference-type target)"
                thread
                targetConcreteTypeHandle
                actualObj
                state
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            // ECMA-335 III.4.33 requires `typeTok` to denote a boxable type, and none of these
            // are; nor can any of them be a generic argument, so `unbox.any !!T` cannot reach
            // here either. Metadata that gets here would be rejected by the real runtime too.
            failwith
                $"Unbox_Any: type token denotes byref/pointer/function-pointer type %O{targetConcreteTypeHandle}, which is not a boxable type as ECMA-335 III.4.33 requires; this is invalid IL"
        | ConcreteTypeHandle.Concrete _ ->

        let targetConcreteType =
            AllConcreteTypes.lookup targetConcreteTypeHandle state.ConcreteTypes
            |> Option.get

        let targetDefn =
            state._LoadedAssemblies.[targetConcreteType.Assembly].TypeDefs.[targetConcreteType.Definition.Get]

        let isNullable =
            InternalTypeKind.kind baseClassTypes targetConcreteType = InternalTypeKind.Nullable

        let isValueType =
            DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies targetDefn

        if isNullable then
            // ECMA-335 III.4.33 / CoreCLR `Nullable::UnBox` (src/coreclr/vm/object.cpp). `box` of a
            // `Nullable<T>` never produces a boxed Nullable — it yields null, or a boxed `T` — so
            // unboxing has to reconstruct the Nullable from those two forms:
            //   - a null operand yields a zeroed Nullable (`hasValue = false`). This is the one
            //     value-typed `unbox.any` target that accepts null instead of raising
            //     NullReferenceException;
            //   - a boxed `T` yields `hasValue = true` with that value;
            //   - anything else is an InvalidCastException.
            // The match against `T` is exact equivalence, not assignability
            // (`Nullable::IsNullableForTypeHelper` compares against `GetInstantiation()[0]`).
            if targetConcreteType.Generics.Length <> 1 then
                failwith
                    $"Unbox_Any: %O{targetConcreteTypeHandle} classified as System.Nullable`1 but has %d{targetConcreteType.Generics.Length} generic arguments, expected exactly 1"

            let underlyingHandle = targetConcreteType.Generics.[0]

            // Built from the zero rather than hand-rolled, so the layout, field ids and offsets
            // match every other way a `Nullable<T>` comes into existence.
            let nullableZero, state =
                IlMachineState.cliTypeZeroOfHandle state baseClassTypes targetConcreteTypeHandle

            let zeroCvt =
                match nullableZero with
                | CliType.ValueType cvt -> cvt
                | other ->
                    failwith
                        $"Unbox_Any: zero of Nullable`1 %O{targetConcreteTypeHandle} was %O{other}, expected a value type"

            match actualObj with
            | EvalStackValue.NullObjectRef ->
                state
                |> IlMachineState.pushToEvalStack nullableZero thread
                |> IlMachineState.advanceProgramCounter thread
                |> Tuple.withRight WhatWeDid.Executed
            | EvalStackValue.ObjectRef addr ->
                let boxedOpt =
                    match state.ManagedHeap.NonArrayObjects.TryGetValue addr with
                    | true, v -> Some v
                    | false, _ ->
                        match state.ManagedHeap.Arrays.TryGetValue addr with
                        // An array can never be a boxed T for any T that Nullable admits.
                        | true, _ -> None
                        | false, _ -> failwith $"Unbox_Any: could not find managed object with address {addr}"

                match boxedOpt with
                | Some boxed when boxed.ConcreteType = underlyingHandle ->
                    let value, state =
                        unboxedContents baseClassTypes underlyingHandle boxed.Contents state

                    // No coercion needed: `unboxedContents` decides its shape from
                    // `cliTypeZeroOfHandle underlyingHandle`, which is the same computation that
                    // produced the zero of the `value` field we are overwriting.
                    let hasValueField =
                        IlMachineState.requiredOwnInstanceFieldId state zeroCvt.Declared "hasValue"

                    let valueField =
                        IlMachineState.requiredOwnInstanceFieldId state zeroCvt.Declared "value"

                    let result =
                        zeroCvt
                        |> CliValueType.WithFieldSetById hasValueField (CliType.ofBool true)
                        |> CliValueType.WithFieldSetById valueField value

                    state
                    |> IlMachineState.pushToEvalStack (CliType.ValueType result) thread
                    |> IlMachineState.advanceProgramCounter thread
                    |> Tuple.withRight WhatWeDid.Executed
                | Some boxed when boxed.ConcreteType = targetConcreteTypeHandle ->
                    // CoreCLR has a "for safety's sake" arm here that copies a genuinely boxed
                    // `Nullable<T>` straight through. Nothing in this interpreter can produce one:
                    // `executeBox` never boxes a Nullable as itself, and the only other boxing path
                    // (the `constrained.` callvirt fallback in UnaryMetadataCallOps) requires the
                    // method to be unresolvable on the value type and declared on
                    // Object/ValueType/Enum — whereas `Nullable<T>` overrides all three, and by an
                    // explicit CoreLib invariant ("Do NOT add any interfaces to Nullable!",
                    // Nullable.cs) implements no interfaces to dispatch through either.
                    // So this is unreachable rather than merely untested; fail loudly if that
                    // assumption ever stops holding, instead of silently answering
                    // InvalidCastException like the arm below.
                    failwith
                        $"Unbox_Any: operand at %O{addr} is a boxed Nullable`1 (%O{targetConcreteTypeHandle}), which no PawPrint boxing path can create; CoreCLR's Nullable::UnBox copies it through, but that arm is deliberately unmodelled here"
                | Some _
                | None ->
                    IlMachineStateExecution.raiseRuntimeException
                        loggerFactory
                        baseClassTypes
                        baseClassTypes.InvalidCastException
                        thread
                        state
            | other -> failwith $"Unbox_Any (Nullable`1 target): unexpected eval stack value {other}"
        elif not isValueType then
            // Reference-type target: behave exactly like castclass.
            castToReferenceType
                loggerFactory
                baseClassTypes
                "Unbox_Any (reference-type target)"
                thread
                targetConcreteTypeHandle
                actualObj
                state
        else
            // Value-type target, non-Nullable.
            let state, typeTest =
                unboxTypeTest
                    loggerFactory
                    baseClassTypes
                    "Unbox_Any (value-type target)"
                    targetConcreteTypeHandle
                    actualObj
                    state

            match typeTest with
            | UnboxTypeTest.NullOperand ->
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    baseClassTypes
                    baseClassTypes.NullReferenceException
                    thread
                    state
            | UnboxTypeTest.WrongType ->
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    baseClassTypes
                    baseClassTypes.InvalidCastException
                    thread
                    state
            | UnboxTypeTest.Accepted (_addr, boxed) ->
                // Materialise using the *boxed object's* handle, not the target's: that is the
                // handle its `Contents` were built with, which is the precondition
                // `unboxedContents` documents. Under the enum relaxation the two can differ,
                // and it is the push/store path that reconciles the result with the target.
                let toPush, state =
                    unboxedContents baseClassTypes boxed.ConcreteType boxed.Contents state

                state
                |> IlMachineState.pushToEvalStack toPush thread
                |> IlMachineState.advanceProgramCounter thread
                |> Tuple.withRight WhatWeDid.Executed

    /// ECMA-335 III.4.32 (`unbox`). The type test is shared with the value-type arm of
    /// `unbox.any` — CoreCLR routes both through `CastHelpers.Unbox_Helper` — but the result
    /// differs: `unbox` pushes a managed pointer *into* the boxed object rather than a copy of
    /// its contents, so a `stobj`/`stfld` through the result is visible through the box.
    ///
    /// The `Nullable<T>` target is deliberately unimplemented; see the `failwith` below.
    let executeUnbox (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let actualObj, state = IlMachineState.popEvalStack thread state

        let state, targetType, _targetAssy =
            IlMachineState.resolveTypeMetadataToken
                loggerFactory
                baseClassTypes
                state
                activeAssy
                ImmutableArray.Empty
                metadataToken

        let state, targetConcreteTypeHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                activeAssy.Name
                currentMethod.DeclaringType.Generics
                currentMethod.Generics
                targetType

        // Unlike `unbox.any`, whose token may denote any boxable type, III.4.32 requires a value
        // type. None of the structural handle shapes is one — arrays are reference types, and
        // byrefs/pointers/function pointers are not boxable at all — so metadata that gets here
        // is invalid IL that the real runtime would reject too. Dispatch on the shape before
        // touching metadata, since these handles have no row in `AllConcreteTypes`.
        match targetConcreteTypeHandle with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            failwith
                $"Unbox: type token denotes %O{targetConcreteTypeHandle}, which is not a value type as ECMA-335 III.4.32 requires; this is invalid IL"
        | ConcreteTypeHandle.Concrete _ ->

        let targetConcreteType =
            AllConcreteTypes.lookup targetConcreteTypeHandle state.ConcreteTypes
            |> Option.get

        let targetDefn =
            state._LoadedAssemblies.[targetConcreteType.Assembly].TypeDefs.[targetConcreteType.Definition.Get]

        // `Nullable<T>` is a value type, so test for it before the general value-type check.
        if InternalTypeKind.kind baseClassTypes targetConcreteType = InternalTypeKind.Nullable then
            // `box` of a `Nullable<T>` yields null or a boxed `T`, so there is no `Nullable<T>` in
            // the heap for a pointer to point *into*. CoreCLR resolves that by materialising a
            // fresh `Nullable<T>` into a JIT temp and pushing the temp's address
            // (jit/importer.cpp, `CEE_UNBOX` with `CORINFO_HELP_UNBOX_NULLABLE`), which the JIT
            // itself flags as non-compliant with ECMA-335: the result aliases a copy, so writes
            // through it are lost. Modelling that needs a storage location for the temp, which
            // this interpreter has no notion of at this point; rather than guess at one, refuse
            // loudly. Roslyn never emits this shape — it compiles `(T?) o` to
            // `unbox.any; stloc; ldloca` — so reaching this is a signal that some other IL
            // producer needs the temp modelled properly.
            failwith
                $"TODO: Unbox with a System.Nullable`1 type token (%O{targetConcreteTypeHandle}) is unimplemented; CoreCLR would push the address of a materialised copy rather than a pointer into the box"

        if not (DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies targetDefn) then
            failwith
                $"Unbox: type token denotes reference type %O{targetConcreteTypeHandle}, but ECMA-335 III.4.32 requires a value type; this is invalid IL"

        let state, typeTest =
            unboxTypeTest loggerFactory baseClassTypes "Unbox" targetConcreteTypeHandle actualObj state

        match typeTest with
        | UnboxTypeTest.NullOperand ->
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                baseClassTypes.NullReferenceException
                thread
                state
        | UnboxTypeTest.WrongType ->
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                baseClassTypes.InvalidCastException
                thread
                state
        | UnboxTypeTest.Accepted (addr, boxed) ->
            // `HeapValue` denotes the whole boxed value (see `CellAwareMemOps`), so the aliasing
            // III.4.32 requires falls out: reads and writes through this pointer go to the box
            // itself, not to a copy.
            //
            // What it does *not* carry is a static type. Every consumer — `ldind`, `ldobj`,
            // `ldfld`, `ldflda`, `stobj`, `stfld` — resolves the pointee from whatever storage it
            // finds at the root, which is right exactly when the box holds the target type's own
            // fields. The two shapes below break that, and no projection list fixes them: a
            // trailing byte view satisfies `ldind`/`ldobj` (they take the typed byte-view read for
            // such a pointer) but then hides the storage's real fields from `ldfld`, and omitting
            // it does the reverse. Serving them properly needs a byref that carries a static type
            // — a change to the pointer representation itself, not to this instruction — so refuse
            // loudly here instead of handing back a pointer that is wrong for half its consumers.
            //
            // Nothing exercises either today: C# emits `unbox` only for a field read, so only ever
            // for a genuine value type, and the single bare `unbox` in all of
            // System.Private.CoreLib (`System.Index.Equals`, covered by UnboxFieldAccess.cs) is
            // one of those.
            let barePrimitive, state =
                barePrimitiveBoxShape baseClassTypes boxed.ConcreteType boxed.Contents state

            match barePrimitive with
            | Some _ ->
                failwith
                    $"TODO: Unbox of a boxed bare primitive (%O{boxed.ConcreteType}) is unimplemented; `box` stores it inside a synthetic single-field struct, so a byref to the box addresses that wrapper rather than the value, and `HeapValue` cannot express the distinction"
            | None ->

            if boxed.ConcreteType <> targetConcreteTypeHandle then
                // `unboxPermitted` also accepts same-primitive-element-type pairs, so the box's
                // type and the token's can differ: a boxed enum unboxed as its underlying integer,
                // or as another enum over the same integer.
                failwith
                    $"TODO: Unbox under the enum/underlying relaxation (box holds %O{boxed.ConcreteType}, token says %O{targetConcreteTypeHandle}) is unimplemented; the byref would have to present the box's storage as the token's type while leaving the box's own runtime type intact"

            let ptr =
                CliType.RuntimePointer (
                    CliRuntimePointer.Managed (ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, []))
                )

            state
            |> IlMachineState.pushToEvalStack ptr thread
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed
