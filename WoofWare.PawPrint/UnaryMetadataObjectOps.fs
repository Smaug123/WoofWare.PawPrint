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
            | EvalStackValue.Int32 n ->
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
        | WhatWeDid.BlockedOnClassInit state -> failwith "TODO: another thread is running the initialiser"
        | WhatWeDid.SuspendedForClassInit -> state, WhatWeDid.SuspendedForClassInit
        | WhatWeDid.SuspendedForManagedCall ->
            failwith "logic error: ensureTypeInitialised cannot suspend for an arbitrary managed call"
        | WhatWeDid.ThrowingTypeInitializationException -> state, WhatWeDid.ThrowingTypeInitializationException
        | WhatWeDid.VoluntaryYield -> failwith "logic error: ensureTypeInitialised cannot produce a VoluntaryYield"
        | WhatWeDid.Executed ->

        let ctorAssembly = state.LoadedAssembly ctor.DeclaringType.Assembly |> Option.get
        let ctorType = ctorAssembly.TypeDefs.[ctor.DeclaringType.Definition.Get]

        do
            logger.LogDebug (
                "Creating object of type {ConstructorAssembly}.{ConstructorType}",
                ctorAssembly.Name.Name,
                ctorType.Name
            )

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
            (Some allocatedAddr)
            false
            false
            true
            concretizedCtor.Generics
            concretizedCtor
            thread
            threadState
            None
            false
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
            state._LoadedAssemblies.[targetType.Assembly.FullName].TypeDefs.[targetType.Definition.Get]

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
                                    state._LoadedAssemblies.[underlyingConcreteType.Assembly.FullName].TypeDefs
                                        .[underlyingConcreteType.Definition.Get]

                                let underlyingInstanceFields =
                                    underlyingDefn.Fields
                                    |> List.filter (fun field -> not (field.Attributes.HasFlag FieldAttributes.Static))

                                let underlyingAssembly =
                                    state._LoadedAssemblies.[underlyingConcreteType.Assembly.FullName]

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
                        let targetAssembly = state._LoadedAssemblies.[targetType.Assembly.FullName]

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
            state._LoadedAssemblies.[targetConcreteType.Assembly.FullName].TypeDefs.[targetConcreteType.Definition.Get]

        let isNullable =
            InternalTypeKind.kind baseClassTypes targetConcreteType = InternalTypeKind.Nullable

        let isValueType =
            DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies targetDefn

        if isNullable then
            failwith "TODO: Unbox_Any for Nullable<T> unimplemented"
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
            match actualObj with
            | EvalStackValue.NullObjectRef ->
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    baseClassTypes
                    baseClassTypes.NullReferenceException
                    thread
                    state
            | EvalStackValue.ObjectRef addr ->
                let boxedOpt =
                    match state.ManagedHeap.NonArrayObjects.TryGetValue addr with
                    | true, v -> Some v
                    | false, _ ->
                        match state.ManagedHeap.Arrays.TryGetValue addr with
                        | true, _ ->
                            // Array object with non-array value-type target: wrong type, per CLR this
                            // is an InvalidCastException, not an interpreter abort.
                            None
                        | false, _ -> failwith $"Unbox_Any: could not find managed object with address {addr}"

                match boxedOpt with
                | None ->
                    IlMachineStateExecution.raiseRuntimeException
                        loggerFactory
                        baseClassTypes
                        baseClassTypes.InvalidCastException
                        thread
                        state
                | Some boxed ->

                // Exact-type match per ECMA-335 III.4.33, not assignability.
                // TODO: relax to underlying-type equivalence so a boxed enum can be unboxed to its
                // underlying integral type (spec's "same type-verifier type"). Needs a generic-method
                // test to exercise; not in scope for this PR.
                if boxed.ConcreteType = targetConcreteTypeHandle then
                    // Push the boxed value back onto the eval stack. The push path
                    // (EvalStackValue.ofCliType) handles primitive-like value types
                    // (IntPtr, RuntimeTypeHandle, enums, ...) via the flatten invariant,
                    // and leaves genuine user-defined value types as UserDefinedValueType.
                    //
                    // For primitive targets (Int32, Float64, etc.) the Box path stored
                    // the value in a single-field struct (e.g. { value__ = Int32 42 }).
                    // The zero of such types is a non-ValueType CliType, so we detect
                    // them here and extract the inner field before pushing.
                    let toPush, state =
                        if boxed.Contents.PrimitiveLikeKind.IsSome then
                            // Primitive-like (incl. enum): ofCliType will flatten on push.
                            CliType.ValueType boxed.Contents, state
                        else
                            let targetZero, state =
                                IlMachineState.cliTypeZeroOfHandle state baseClassTypes targetConcreteTypeHandle

                            match targetZero with
                            | CliType.ValueType _ ->
                                // Genuine user-defined value type: keep wrapped.
                                CliType.ValueType boxed.Contents, state
                            | _ ->
                                // Primitive target: Box stored the value in a single instance
                                // field at offset 0 whose Size matches the primitive. Read it
                                // back by offset/size — the Box path guarantees the shape, so
                                // this is a nominal dereference, not a structural guess.
                                let size = (CliType.SizeOf targetZero).Size

                                CliValueType.DereferenceFieldAt 0 size boxed.Contents, state

                    state
                    |> IlMachineState.pushToEvalStack toPush thread
                    |> IlMachineState.advanceProgramCounter thread
                    |> Tuple.withRight WhatWeDid.Executed
                else
                    IlMachineStateExecution.raiseRuntimeException
                        loggerFactory
                        baseClassTypes
                        baseClassTypes.InvalidCastException
                        thread
                        state
            | other -> failwith $"Unbox_Any (value-type target): unexpected eval stack value {other}"
