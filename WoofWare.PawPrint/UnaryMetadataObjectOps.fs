namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module internal UnaryMetadataObjectOps =
    let executeCastclass (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let actualObj, state = IlMachineThreadState.popEvalStack thread state

        let state, targetType, _targetAssy =
            IlMachineRuntimeMetadata.resolveTypeMetadataToken
                loggerFactory
                baseClassTypes
                state
                activeAssy
                ImmutableArray.Empty
                metadataToken

        let state, targetConcreteType =
            IlMachineTypeResolution.concretizeType
                loggerFactory
                baseClassTypes
                state
                activeAssy.Name
                currentMethod.DeclaringType.Generics
                currentMethod.Generics
                targetType

        match actualObj with
        | EvalStackValue.NullObjectRef ->
            // Per ECMA-335 III.4.3: null ref is always valid for castclass on reference types.
            let state =
                state
                |> IlMachineThreadState.pushToEvalStack' EvalStackValue.NullObjectRef thread
                |> IlMachineThreadState.advanceProgramCounter thread

            state, WhatWeDid.Executed
        | EvalStackValue.ObjectRef addr ->
            let objConcreteType = ManagedHeap.getObjectConcreteType addr state.ManagedHeap

            let state, isAssignable =
                IlMachineRuntimeMetadata.isConcreteTypeAssignableTo
                    loggerFactory
                    baseClassTypes
                    state
                    objConcreteType
                    targetConcreteType

            if isAssignable then
                let state =
                    state
                    |> IlMachineThreadState.pushToEvalStack' actualObj thread
                    |> IlMachineThreadState.advanceProgramCounter thread

                state, WhatWeDid.Executed
            else
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    baseClassTypes
                    baseClassTypes.InvalidCastException
                    thread
                    state
        | other -> failwith $"Castclass: unexpected eval stack value {other}"

    let executeNewobj (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let thread = ctx.Thread
        let logger = ctx.Logger

        let heapValueByref (addr : ManagedHeapAddress) : ManagedPointerSource =
            ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, [])

        let state, ctor, typeArgsFromMetadata =
            match metadataToken with
            | MethodDef md ->
                let method = activeAssy.Methods.[md]

                state, MethodInfo.mapTypeGenerics (fun _ -> failwith "non-generic method") method, None
            | MemberReference mr ->
                let state, _, method, extractedTypeArgs =
                    IlMachineMemberResolution.resolveMember
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
        | WhatWeDid.ThrowingTypeInitializationException -> state, WhatWeDid.ThrowingTypeInitializationException
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
            IlMachineRuntimeMetadata.collectAllInstanceFields loggerFactory baseClassTypes state declaringTypeHandle

        let fields =
            CliValueType.OfFields baseClassTypes state.ConcreteTypes declaringTypeHandle ctorType.Layout allFields

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

            IlMachineThreadState.allocateManagedObject ty fields state

        let state =
            if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies ctorType then
                state
                |> IlMachineThreadState.pushToEvalStack'
                    (EvalStackValue.ManagedPointer (heapValueByref allocatedAddr))
                    thread
            else
                state
                |> IlMachineThreadState.pushToEvalStack (CliType.ObjectRef (Some allocatedAddr)) thread

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
                let state, ty =
                    IlMachineRuntimeMetadata.lookupTypeDefn baseClassTypes state activeAssy h

                state, ty, activeAssy
            | MetadataToken.TypeReference ref ->
                IlMachineRuntimeMetadata.lookupTypeRef
                    loggerFactory
                    baseClassTypes
                    state
                    activeAssy
                    currentMethod.DeclaringType.Generics
                    ref
            | MetadataToken.TypeSpecification spec -> state, activeAssy.TypeSpecs.[spec].Signature, activeAssy
            | _ -> failwith $"unexpected token {metadataToken} in Box"

        let state, typeHandle =
            IlMachineTypeResolution.concretizeType
                loggerFactory
                baseClassTypes
                state
                assy.Name
                currentMethod.DeclaringType.Generics
                currentMethod.Generics
                ty

        let toBox, state = state |> IlMachineThreadState.popEvalStack thread

        let targetType =
            AllConcreteTypes.lookup typeHandle state.ConcreteTypes |> Option.get

        let defn =
            state._LoadedAssemblies.[targetType.Assembly.FullName].TypeDefs.[targetType.Definition.Get]

        let isNullable =
            targetType.Namespace = "System"
            && targetType.Name = "Nullable`1"
            && targetType.Assembly.FullName = baseClassTypes.Corelib.Name.FullName

        let toPush, state =
            if isNullable then
                // Nullable<T> boxing: null when !HasValue, box underlying T when HasValue.
                match toBox with
                | EvalStackValue.UserDefinedValueType cvt ->
                    let hasValueField =
                        IlMachineRuntimeMetadata.requiredOwnInstanceFieldId state cvt.Declared "hasValue"

                    let hasValue = CliValueType.DereferenceFieldById hasValueField cvt

                    match hasValue with
                    | CliType.Bool 0uy ->
                        // Nullable with HasValue=false boxes to null.
                        EvalStackValue.NullObjectRef, state
                    | CliType.Bool _ ->
                        // Nullable with HasValue=true: box the underlying value as T.
                        let underlyingTypeHandle = targetType.Generics.[0]

                        let valueField =
                            IlMachineRuntimeMetadata.requiredOwnInstanceFieldId state cvt.Declared "value"

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
                                            IlMachineTypeResolution.cliTypeZeroOf
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
                                            }

                                        state, cliField :: acc
                                    )

                                List.rev fieldValues
                                |> CliValueType.OfFields
                                    baseClassTypes
                                    state.ConcreteTypes
                                    underlyingTypeHandle
                                    underlyingDefn.Layout,
                                state

                        let addr, state =
                            IlMachineThreadState.allocateManagedObject underlyingTypeHandle cvt state

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
                                    IlMachineTypeResolution.cliTypeZeroOf
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
                                    }

                                state, cliField :: acc
                            )

                        let cvt =
                            List.rev fieldValues
                            |> CliValueType.OfFields baseClassTypes state.ConcreteTypes typeHandle defn.Layout

                        cvt, state

                let addr, state = IlMachineThreadState.allocateManagedObject typeHandle cvt state

                EvalStackValue.ObjectRef addr, state
            else
                // Reference type: box is a no-op, value passes through unchanged
                toBox, state

        state
        |> IlMachineThreadState.pushToEvalStack' toPush thread
        |> IlMachineThreadState.advanceProgramCounter thread
        |> Tuple.withRight WhatWeDid.Executed

    let executeIsinst (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let actualObj, state = IlMachineThreadState.popEvalStack thread state

        let state, targetType, _targetAssy =
            IlMachineRuntimeMetadata.resolveTypeMetadataToken
                loggerFactory
                baseClassTypes
                state
                activeAssy
                ImmutableArray.Empty
                metadataToken

        let state, targetConcreteType =
            IlMachineTypeResolution.concretizeType
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
                IlMachineRuntimeMetadata.isConcreteTypeAssignableTo
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
                match IlMachineManagedByref.readManagedByref state src with
                | CliType.ObjectRef None -> state, EvalStackValue.NullObjectRef
                | CliType.ObjectRef (Some addr) ->
                    let concreteType = ManagedHeap.getObjectConcreteType addr state.ManagedHeap
                    isinstCheck state concreteType (EvalStackValue.ObjectRef addr)
                | other -> failwith $"TODO: Isinst on managed pointer to non-object-ref {other}"
            | esv -> failwith $"TODO: Isinst on {esv}"

        let state =
            state
            |> IlMachineThreadState.pushToEvalStack' returnObj thread
            |> IlMachineThreadState.advanceProgramCounter thread

        state, WhatWeDid.Executed

    let executeUnboxAny (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        // ECMA-335 III.4.33
        let actualObj, state = IlMachineThreadState.popEvalStack thread state

        let state, targetType, _targetAssy =
            IlMachineRuntimeMetadata.resolveTypeMetadataToken
                loggerFactory
                baseClassTypes
                state
                activeAssy
                ImmutableArray.Empty
                metadataToken

        let state, targetConcreteTypeHandle =
            IlMachineTypeResolution.concretizeType
                loggerFactory
                baseClassTypes
                state
                activeAssy.Name
                currentMethod.DeclaringType.Generics
                currentMethod.Generics
                targetType

        let targetConcreteType =
            AllConcreteTypes.lookup targetConcreteTypeHandle state.ConcreteTypes
            |> Option.get

        let targetDefn =
            state._LoadedAssemblies.[targetConcreteType.Assembly.FullName].TypeDefs.[targetConcreteType.Definition.Get]

        let isNullable =
            targetConcreteType.Namespace = "System"
            && targetConcreteType.Name = "Nullable`1"
            && targetConcreteType.Assembly.FullName = baseClassTypes.Corelib.Name.FullName

        let isValueType =
            DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies targetDefn

        if isNullable then
            failwith "TODO: Unbox_Any for Nullable<T> unimplemented"
        elif not isValueType then
            // Reference-type target: behave exactly like castclass.
            // TODO: factor the shared castclass/unbox.any reference-type logic into a helper.
            match actualObj with
            | EvalStackValue.NullObjectRef ->
                state
                |> IlMachineThreadState.pushToEvalStack' EvalStackValue.NullObjectRef thread
                |> IlMachineThreadState.advanceProgramCounter thread
                |> Tuple.withRight WhatWeDid.Executed
            | EvalStackValue.ObjectRef addr ->
                let objConcreteType =
                    match state.ManagedHeap.NonArrayObjects.TryGetValue addr with
                    | true, v -> v.ConcreteType
                    | false, _ ->
                        match state.ManagedHeap.Arrays.TryGetValue addr with
                        | true, _v -> failwith "TODO: Unbox_Any on array objects (reference-type target)"
                        | false, _ -> failwith $"Unbox_Any: could not find managed object with address {addr}"

                let state, isAssignable =
                    IlMachineRuntimeMetadata.isConcreteTypeAssignableTo
                        loggerFactory
                        baseClassTypes
                        state
                        objConcreteType
                        targetConcreteTypeHandle

                if isAssignable then
                    state
                    |> IlMachineThreadState.pushToEvalStack' actualObj thread
                    |> IlMachineThreadState.advanceProgramCounter thread
                    |> Tuple.withRight WhatWeDid.Executed
                else
                    IlMachineStateExecution.raiseRuntimeException
                        loggerFactory
                        baseClassTypes
                        baseClassTypes.InvalidCastException
                        thread
                        state
            | other -> failwith $"Unbox_Any (reference-type target): unexpected eval stack value {other}"
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
                                IlMachineTypeResolution.cliTypeZeroOfHandle
                                    state
                                    baseClassTypes
                                    targetConcreteTypeHandle

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
                    |> IlMachineThreadState.pushToEvalStack toPush thread
                    |> IlMachineThreadState.advanceProgramCounter thread
                    |> Tuple.withRight WhatWeDid.Executed
                else
                    IlMachineStateExecution.raiseRuntimeException
                        loggerFactory
                        baseClassTypes
                        baseClassTypes.InvalidCastException
                        thread
                        state
            | other -> failwith $"Unbox_Any (value-type target): unexpected eval stack value {other}"
