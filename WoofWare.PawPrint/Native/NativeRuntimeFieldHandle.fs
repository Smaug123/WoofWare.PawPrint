namespace WoofWare.PawPrint

open System.Reflection
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module NativeRuntimeFieldHandle =
    let internal fieldHandleOfRuntimeFieldHandleInternal
        (operation : string)
        (state : IlMachineState)
        (arg : CliType)
        : FieldHandle option
        =
        match NativeCall.fieldHandleIdOfRuntimeFieldHandleInternal operation arg with
        | None -> None
        | Some fieldHandleId ->
            match FieldHandleRegistry.resolveFieldFromId fieldHandleId state.FieldHandles with
            | Some fieldHandle -> Some fieldHandle
            | None -> failwith $"%s{operation}: field-registry handle %d{fieldHandleId} is not allocated"

    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "System.Private.CoreLib",
          "System",
          "RuntimeFieldHandle",
          "GetUtf8NameInternal",
          [ CorelibType state.ConcreteTypes ("System", "RuntimeFieldHandleInternal", generics) ],
          MethodReturnType.Returns (ConcretePointer (ConcreteVoid state.ConcreteTypes)) when generics.IsEmpty ->
            // CoreCLR's RuntimeFieldHandle::GetUtf8NameInternal (runtimehandles.cpp:2167)
            // is an FCall that dereferences a FieldDesc* and reads the field's UTF-8 name
            // from the metadata string heap. The managed wrapper RuntimeFieldHandle.GetUtf8Name
            // (RuntimeHandles.cs:1501) wraps the result in MdUtf8String, which strlens the
            // pointer to discover the byte length. PawPrint materialises the field's metadata
            // name as a freshly-allocated null-terminated UTF-8 byte[] and returns a byref to
            // it; the managed strlen path then walks the array as expected. Mirrors the
            // RuntimeMethodHandle.GetUtf8NameInternal handler.
            let operation = "RuntimeFieldHandle.GetUtf8NameInternal"

            let fieldHandle =
                // FCall asserts non-null; surface a null handle loudly here, matching the
                // sibling RuntimeFieldHandle.GetAttributes precedent below.
                fieldHandleOfRuntimeFieldHandleInternal operation state instruction.Arguments.[0]
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: null field handle")

            let _, fieldInfo = FieldRvaData.fieldForHandle operation fieldHandle state

            let namePtr, state =
                NativeCall.allocateNullTerminatedUtf8 ctx.BaseClassTypes fieldInfo.Name state

            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer namePtr) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeFieldHandle",
          "GetAttributes",
          [ CorelibType state.ConcreteTypes ("System", "RuntimeFieldHandleInternal", generics) ],
          MethodReturnType.Returns (CorelibType state.ConcreteTypes ("System.Reflection", "FieldAttributes", retGenerics)) when
            generics.IsEmpty && retGenerics.IsEmpty
            ->
            let operation = "RuntimeFieldHandle.GetAttributes"

            let fieldHandle =
                // CoreCLR exposes this as a raw FieldDesc* FCall; null handles fault here,
                // unlike QCalls such as GetRVAFieldInfo which return success/failure.
                fieldHandleOfRuntimeFieldHandleInternal operation state instruction.Arguments.[0]
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: null field handle")

            let _, fieldInfo = FieldRvaData.fieldForHandle operation fieldHandle state

            let state =
                IlMachineState.pushToEvalStack
                    (CliType.Numeric (CliNumericType.Int32 (int32 fieldInfo.Attributes)))
                    ctx.Thread
                    state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeFieldHandle",
          "GetApproxDeclaringMethodTable",
          [ CorelibType state.ConcreteTypes ("System", "RuntimeFieldHandleInternal", generics) ],
          MethodReturnType.Returns (ConcretePointer (CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                                                                      "MethodTable",
                                                                                      methodTableGenerics))) when
            generics.IsEmpty && methodTableGenerics.IsEmpty
            ->
            // CoreCLR's RuntimeFieldHandle::GetApproxDeclaringMethodTable
            // (runtimehandles.cpp:2192) is an FCall returning
            // pField->GetApproxEnclosingMethodTable() — the canonical MethodTable for
            // the field's declaring type. Under shared-generic codegen the canonical
            // form is the open instantiation. With PawPrint's per-canonical
            // FieldHandle model, the stored DeclaringType is `Closed` for non-generic
            // declaring types and `OpenGenericTypeDefinition` for generic ones.
            // `NativeIntSource.MethodTablePtr` carries the full `RuntimeTypeHandleTarget`,
            // so the open-generic case surfaces directly.
            let operation = "RuntimeFieldHandle.GetApproxDeclaringMethodTable"

            let fieldHandle =
                // CoreCLR asserts !field.IsNullHandle() at the managed caller; fault
                // loudly here, matching the sibling GetAttributes precedent above.
                fieldHandleOfRuntimeFieldHandleInternal operation state instruction.Arguments.[0]
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: null field handle")

            let declaringTypeHandle = fieldHandle.GetDeclaringTypeHandle ()

            let state =
                IlMachineState.pushToEvalStack'
                    (EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr declaringTypeHandle))
                    ctx.Thread
                    state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeFieldHandle",
          "IsFastPathSupported",
          [ CorelibType state.ConcreteTypes ("System.Reflection", "RtFieldInfo", generics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) when generics.IsEmpty ->
            // The question this predicate asks is "may you ask me for this field's raw offset or
            // address?": its only two consumers, `RuntimeFieldHandle.GetInstanceFieldOffset` and
            // `GetStaticFieldAddress`, each open with
            // `_ASSERTE(IsFastPathSupportedHelper(pFieldDesc))` (reflectioninvocation.cpp:1093,
            // 1115), and its only managed caller uses it for nothing else
            // (`FieldAccessor.Initialize`, FieldAccessor.cs:36). CoreCLR's three disjuncts —
            // thread-static, EnC-new, collectible static (reflectioninvocation.cpp:1060) — are its
            // own three cases where no stable raw location exists.
            //
            // PawPrint has none for *any* field: a `ManagedHeap` object is a map from `FieldId` to
            // `CliType` rather than a byte image, and static storage is keyed the same way, so
            // there is no offset or address to hand out. Answering `false` keeps the accessor in
            // `FieldAccessorType.SlowPath`, where every get and set goes through the
            // `RuntimeFieldHandle_GetValue`/`_SetValue` QCalls below.
            //
            // This becomes `true` — alongside implementations of those two offset FCalls — if
            // PawPrint ever byte-addresses heap objects; nothing else here would have to move.
            let state = IlMachineState.pushToEvalStack (CliType.ofBool false) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | _ -> None

    /// The declaring type the QCalls below reason about: the one the field-handle registry
    /// recorded when the guest asked for the handle, rather than the `m_declaringType` view the
    /// managed caller also passes. The registry's target *is* the field's identity — a closed
    /// instantiation and its open definition allocate distinct ids — so binding the cctor check
    /// and the storage key to it cannot drift from the field we are about to write.
    let private declaringTypeOfFieldHandle (operation : string) (fieldHandle : FieldHandle) : ConcreteTypeHandle =
        match fieldHandle.GetDeclaringTypeHandle () with
        | RuntimeTypeHandleTarget.Closed handle -> handle
        | notClosed ->
            // `FieldAccessor`'s constructor routes a declaring type with generic parameters to
            // `FieldAccessorType.NoInvoke` (FieldAccessor.cs:22-27), which throws before any QCall
            // runs, so no guest reaches this through `FieldInfo.SetValue`.
            failwith
                $"%s{operation}: declaring type %O{notClosed} is not closed; CoreCLR raises NotSupportedException for a declaring type shared by generic instantiations (invokeutil.cpp:777), and the managed caller refuses one containing generic parameters before reaching here"

    /// The value to store in a field of type `fieldTypeHandle`, given the `object?` the managed
    /// caller boxed it into. Mirrors the split in `InvokeUtil::SetValidField`
    /// (invokeutil.cpp:820-960) between the reference-typed arms, which copy the `OBJECTREF`
    /// straight through, and the rest, which read the payload out of the box.
    let private valueToStore
        (ctx : NativeCallContext)
        (operation : string)
        (fieldTypeHandle : ConcreteTypeHandle)
        (value : ManagedHeapAddress option)
        (state : IlMachineState)
        : IlMachineState * CliType
        =
        let baseClassTypes = ctx.BaseClassTypes

        if IlMachineState.isReferenceTypeHandle baseClassTypes operation state fieldTypeHandle then
            state, CliType.ObjectRef value
        else

        let valueAddr =
            match value with
            | Some addr -> addr
            | None ->
                // Unreachable from the only managed caller: `RuntimeType.CheckValue` replaces a
                // null destined for a value-type field with a default box before the QCall runs
                // (`AllocateValueType(this, value: null)`, RuntimeType.cs:1013). CoreCLR's
                // corresponding `InitValueClass` arm (invokeutil.cpp:955) is therefore dead here,
                // so refuse rather than ship an arm nothing can provoke.
                failwith
                    $"%s{operation}: null value for the value-typed field type %O{fieldTypeHandle}; the managed caller is expected to have boxed a default instead"

        let boxed =
            match ManagedHeap.tryGet valueAddr state.ManagedHeap with
            | Some boxed -> boxed
            | None ->
                failwith
                    $"%s{operation}: value for field type %O{fieldTypeHandle} is not a boxed value type (it is an array or is not on the non-array heap)"

        // The relaxation this consults is load-bearing rather than defensive. Managed `CheckValue`
        // converts only when the source and destination `CorElementType`s *differ*
        // (`TryChangeTypeSpecial`, RuntimeType.CoreCLR.cs:3789), so a boxed `int` written into an
        // `enum : int` field — and the reverse, and a same-underlying sibling enum — all arrive
        // here unconverted. That is exactly the enum/underlying clause `unboxPermitted` models for
        // the `unbox` opcode.
        let state, permitted =
            IlMachineState.unboxPermitted ctx.LoggerFactory baseClassTypes state boxed.ConcreteType fieldTypeHandle

        if not permitted then
            failwith
                $"%s{operation}: cannot store a value boxed as %O{boxed.ConcreteType} into a field of type %O{fieldTypeHandle}"

        // Materialise from the *boxed object's* handle, which is what its `Contents` were built
        // with; under the relaxation above the two handles differ, and the coercion below is what
        // reconciles the result with the field's cell.
        let contents, state =
            BoxedValue.contents baseClassTypes boxed.ConcreteType boxed.Contents state

        let zero, state =
            IlMachineState.cliTypeZeroOfHandle state baseClassTypes fieldTypeHandle

        state, EvalStackValue.toCliTypeCoerced zero (EvalStackValue.ofCliType contents)

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "RuntimeFieldHandle_SetValue",
          "System.Private.CoreLib",
          "System",
          "RuntimeFieldHandle",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "ObjectHandleOnStack", instanceGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "ObjectHandleOnStack", valueGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallTypeHandle", fieldTypeGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallTypeHandle", declaringTypeGenerics)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ],
          MethodReturnType.Void when
            instanceGenerics.IsEmpty
            && valueGenerics.IsEmpty
            && fieldTypeGenerics.IsEmpty
            && declaringTypeGenerics.IsEmpty
            ->
            // CoreCLR's `RuntimeFieldHandle_SetValue` (reflectioninvocation.cpp:46), which is
            // `InvokeUtil::SetValidField` (invokeutil.cpp:742) once the arguments are unpacked.
            // This is the primitive under every `FieldInfo.SetValue`: `FieldAccessor` reaches its
            // address-based fast path only when `IsFastPathSupported` says a raw offset exists,
            // and PawPrint answers `false` there, so *all* reflective field writes land here.
            let operation = "RuntimeFieldHandle_SetValue"

            let fieldHandle =
                fieldHandleOfRuntimeFieldHandleInternal operation state instruction.Arguments.[0]
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: null field handle")

            let _declaringAssy, fieldInfo =
                FieldRvaData.fieldForHandle operation fieldHandle state

            let declaringTypeHandle = declaringTypeOfFieldHandle operation fieldHandle

            let isClassInitializedPtr =
                NativeCall.managedPointerOfPointerArgument operation "pIsClassInitialized" instruction.Arguments.[5]

            let incomingIsClassInitialized =
                // `[MarshalAs(UnmanagedType.Bool)] ref bool` reaches us as a four-byte cell.
                match IlMachineState.readManagedByref ctx.BaseClassTypes state isClassInitializedPtr with
                | CliType.Numeric (CliNumericType.Int32 i) -> i <> 0
                | other -> failwith $"%s{operation}: expected Int32 in pIsClassInitialized, got %O{other}"

            // As an *input* the flag means "the caller has already established the class is
            // initialised, so skip the check"; `FieldAccessor`'s permanent `SlowPath` state passes
            // `true` (FieldAccessor.cs:329) and its first-call state passes `false` (:301).
            // CoreCLR guards the cctor run on exactly this (invokeutil.cpp:785).
            let classInitOutcome : Choice<IlMachineState, NativeHandlerResult> =
                if incomingIsClassInitialized then
                    Choice1Of2 state
                else
                    match TypeInitTable.tryGet declaringTypeHandle state.TypeInitTable with
                    | Some (TypeInitState.Failed _) ->
                        // Refusing here rather than delegating is deliberate. CoreCLR catches a
                        // failing initialiser and throws a *fresh* `TargetInvocationException`
                        // wrapping the `TypeInitializationException` (`CreateTargetExcept`,
                        // invokeutil.cpp:803) — unlike `ReflectionInvocation_RunClassConstructor`,
                        // which lets it through unwrapped. `ensureTypeInitialised` dispatches the
                        // cached exception itself, so once it has returned there is nothing left
                        // to wrap; catching the already-failed state is the only interception
                        // point we have. See docs/divergences.md for the sibling case — an
                        // initialiser that fails *during* this call — which is not interceptable
                        // at all today.
                        failwith
                            $"TODO: %s{operation} on a field of %O{declaringTypeHandle}, whose class initialiser has already failed; CoreCLR wraps the cached TypeInitializationException in a TargetInvocationException, which PawPrint cannot yet construct from here"
                    | _ ->

                    // If the initialiser has to run, it is pushed as a frame and this native
                    // frame stays on the stack; when it returns we are re-entered and
                    // `ensureTypeInitialised` answers `Executed`.
                    let state, whatWeDid =
                        IlMachineStateExecution.ensureTypeInitialised
                            ctx.LoggerFactory
                            ctx.BaseClassTypes
                            ctx.Thread
                            declaringTypeHandle
                            state

                    match NativeHandlerResult.tryEarlyReturn (state, whatWeDid) with
                    | Some earlyReturn -> Choice2Of2 earlyReturn
                    | None -> Choice1Of2 state

            match classInitOutcome with
            | Choice2Of2 result -> Some result
            | Choice1Of2 state ->

            // Whether the *initialiser has completed*, which is what CoreCLR reports
            // (`pDeclMT->IsClassInited()`, invokeutil.cpp:791) and what the init-only gate below
            // reads (:811). `InProgress` is deliberately not "initialised": that is what makes
            // reflectively setting a static readonly field from inside its own declaring type's
            // initialiser legal, exactly as on real .NET.
            let classIsInitialised =
                match TypeInitTable.tryGet declaringTypeHandle state.TypeInitTable with
                | Some TypeInitState.Initialized -> true
                | Some (TypeInitState.InProgress _)
                | Some (TypeInitState.Failed _)
                | None -> false

            let isStatic = fieldInfo.Attributes.HasFlag FieldAttributes.Static

            if
                isStatic
                && classIsInitialised
                && fieldInfo.Attributes.HasFlag FieldAttributes.InitOnly
            then
                // invokeutil.cpp:809-818. This gate is load-bearing rather than decorative:
                // managed `VerifyInitOnly` skips its own check while the accessor is in
                // `SlowPathUntilClassInitialized` (FieldAccessor.cs:386) and delegates it here.
                // CoreCLR attaches a message naming the field and its class; the default one is
                // used here, so guests should test the exception type rather than its text.
                NativeHandlerResult.raiseException ctx.BaseClassTypes.FieldAccessException state
                |> Some
            else

            let fieldTypeHandle =
                NativeCall.qCallTypeHandleToConcreteTypeHandle
                    operation
                    state
                    (EvalStackValue.ofCliType instruction.Arguments.[3])

            let readObjectHandle (argName : string) (index : int) : ManagedHeapAddress option =
                let ptr =
                    NativeCall.objectHandleOnStackTarget operation state argName instruction.Arguments.[index]

                // An `ObjectHandleOnStack` names a slot holding an object reference, so this wants
                // the object-aware reader rather than the byte-view one.
                match IlMachineState.readManagedByref ctx.BaseClassTypes state ptr with
                | CliType.ObjectRef addr -> addr
                | other ->
                    failwith $"%s{operation}: expected ObjectRef in %s{argName} ObjectHandleOnStack, got %O{other}"

            let value = readObjectHandle "value" 2

            let state, toStore = valueToStore ctx operation fieldTypeHandle value state

            let state =
                if isStatic then
                    if fieldInfo.HasFieldRVA then
                        // An RVA-backed static's storage is the PE image's own bytes: CoreCLR
                        // writes it through `GetCurrentStaticAddress`, which for such a field is
                        // `Module::GetRvaField` (invokeutil.cpp:947, reflectioninvocation.cpp:1117).
                        // PawPrint keeps that data in a `PeByteRange` that `IlMachineState.setStatic`
                        // does not touch, and `ldsflda` reads *only* that range
                        // (`peByteRangeForFieldRva`, UnaryMetadataFieldOps.fs) — so writing to
                        // ordinary static storage here would succeed and then be invisible to
                        // every address-based read.
                        //
                        // The same read/write asymmetry already exists between `stsfld` and
                        // `ldsflda`, but no C# program emits `stsfld` against one of these fields:
                        // Roslyn only ever `ldsflda`s the `<PrivateImplementationDetails>` blobs it
                        // generates. Reflection is a new route to the shape, so refuse rather than
                        // extend the silent mismatch to it.
                        failwith
                            $"TODO: %s{operation} on the RVA-backed static field %s{fieldInfo.Name}; its storage is the PE byte range that `ldsflda` reads, which `setStatic` does not update"

                    // A `[ThreadStatic]` field goes to the storing thread's own slot, as `stsfld`
                    // does; that is also the one field kind for which CoreCLR itself answers
                    // `IsFastPathSupported = false`, so both runtimes reach this by the same route.
                    IlMachineState.setStatic
                        (StaticOwner.forField ctx.Thread fieldInfo)
                        declaringTypeHandle
                        (ComparableFieldDefinitionHandle.Make fieldInfo.Handle)
                        toStore
                        state
                else

                let target =
                    match readObjectHandle "instance" 1 with
                    | Some addr -> addr
                    | None ->
                        // Managed `VerifyTarget` (FieldAccessor.cs:351) has already thrown
                        // `TargetException` for a null instance, so this cannot arrive.
                        failwith
                            $"%s{operation}: null instance for the instance field %s{fieldInfo.Name}; the managed caller checks this before the QCall"

                let fieldId = FieldId.metadata declaringTypeHandle fieldInfo.Handle fieldInfo.Name

                match
                    RuntimeFieldProjection.tryProjectFieldStore
                        ctx.BaseClassTypes
                        fieldInfo
                        target
                        toStore
                        state.ManagedHeap
                with
                | Some heap ->
                    { state with
                        ManagedHeap = heap
                    }
                | None ->
                    { state with
                        ManagedHeap = ManagedHeap.setFieldById target fieldId toStore state.ManagedHeap
                    }

            let state =
                if incomingIsClassInitialized then
                    // CoreCLR writes this cell only inside its `if (*pIsClassInitialized == FALSE)`
                    // block (invokeutil.cpp:785-794), leaving the caller's `true` alone otherwise.
                    // Recomputing it would answer "not initialised" for a type the caller vouched
                    // for but which has no `TypeInitTable` entry of ours — a worse answer than the
                    // one it supplied, and not one it asked us to revisit.
                    state
                else
                    IlMachineState.writeManagedByrefWithBase
                        ctx.BaseClassTypes
                        state
                        isClassInitializedPtr
                        (CliType.Numeric (CliNumericType.Int32 (if classIsInitialised then 1 else 0)))

            NativeHandlerResult.completed state |> Some
        | "RuntimeFieldHandle_GetRVAFieldInfo",
          "System.Private.CoreLib",
          "System",
          "RuntimeFieldHandle",
          [ CorelibType state.ConcreteTypes ("System", "RuntimeFieldHandleInternal", generics)
            ConcretePointer (ConcretePointer (ConcreteVoid state.ConcreteTypes))
            ConcretePointer (ConcreteUInt32 state.ConcreteTypes) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when generics.IsEmpty ->
            let operation = "RuntimeFieldHandle_GetRVAFieldInfo"

            let addressOut =
                NativeCall.managedPointerOfPointerArgument operation "address out pointer" instruction.Arguments.[1]

            let sizeOut =
                NativeCall.managedPointerOfPointerArgument operation "size out pointer" instruction.Arguments.[2]

            let state =
                match NativeCall.fieldHandleIdOfRuntimeFieldHandleInternal operation instruction.Arguments.[0] with
                | None ->
                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread
                | Some fieldHandleId ->
                    match FieldHandleRegistry.resolveFieldFromId fieldHandleId state.FieldHandles with
                    | None ->
                        state
                        |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread
                    | Some fieldHandle ->
                        let state, peByteRange =
                            FieldRvaData.tryGet ctx.LoggerFactory ctx.BaseClassTypes operation fieldHandle state

                        match peByteRange with
                        | None ->
                            state
                            |> IlMachineState.pushToEvalStack'
                                (EvalStackValue.Int32 (Int32Source.Verbatim 0))
                                ctx.Thread
                        | Some peByteRange ->
                            let state, dataPtr =
                                IlMachineState.peByteRangePointer ctx.LoggerFactory ctx.BaseClassTypes peByteRange state

                            let state =
                                IlMachineState.writeManagedByrefWithBase
                                    ctx.BaseClassTypes
                                    state
                                    addressOut
                                    (CliType.RuntimePointer (CliRuntimePointer.Managed dataPtr))

                            let state =
                                IlMachineState.writeManagedByrefWithBase
                                    ctx.BaseClassTypes
                                    state
                                    sizeOut
                                    (NativeCall.cliUInt32 (uint32 peByteRange.Size))

                            state
                            |> IlMachineState.pushToEvalStack'
                                (EvalStackValue.Int32 (Int32Source.Verbatim 1))
                                ctx.Thread

            NativeHandlerResult.completed state |> Some
        | _ -> None
