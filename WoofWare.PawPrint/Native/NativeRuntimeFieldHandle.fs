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

    /// The token CoreCLR's `RuntimeFieldHandle::GetToken` FCall (runtimehandles.cpp:2205)
    /// returns: `pField->GetMemberDef()`, which reads a value stored on the FieldDesc at
    /// construction rather than looking anything up.
    ///
    /// This is a function of the FieldDef row identity alone. In particular the declaring type
    /// the handle was minted against does not affect it: CoreCLR keeps a generic type's
    /// FieldDescs on the canonical (`__Canon`) MethodTable, so every instantiation shares them.
    /// Measured on real .NET: `Gen&lt;&gt;.Value`, `Gen&lt;int&gt;.Value` and
    /// `Gen&lt;string&gt;.Value` all report one token, even though PawPrint gives those three
    /// handles distinct registry ids.
    ///
    /// There is no nil answer to give. CoreCLR's assertion allows `mdFieldDefNil` for a FieldDesc
    /// that no metadata row names, but `FieldHandle` carries a `ComparableFieldDefinitionHandle`,
    /// so PawPrint cannot be holding such a field.
    let fieldDefinitionToken (handle : FieldHandle) : int32 =
        let definitionHandle : System.Reflection.Metadata.EntityHandle =
            System.Reflection.Metadata.FieldDefinitionHandle.op_Implicit (handle.GetFieldDefinitionHandle().Get)

        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken definitionHandle

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
          "GetToken",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // `RtFieldInfo.MetadataToken` (RtFieldInfo.cs:62) is this call and nothing else, and
            // it is what `CustomAttribute.GetCustomAttributes` keys the CustomAttribute table on
            // when it looks for a field's attributes.
            let operation = "RuntimeFieldHandle.GetToken"

            let fieldHandle =
                // The FCall's PRECONDITION is `pField != NULL`; a null handle faults there, so
                // fault loudly here too, as the GetAttributes sibling above does.
                fieldHandleOfRuntimeFieldHandleInternal operation state instruction.Arguments.[0]
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: null field handle")

            let state =
                IlMachineState.pushToEvalStack
                    (CliType.Numeric (CliNumericType.Int32 (fieldDefinitionToken fieldHandle)))
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
          "AcquiresContextFromThis",
          [ CorelibType state.ConcreteTypes ("System", "RuntimeFieldHandleInternal", generics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) when generics.IsEmpty ->
            // CoreCLR's RuntimeFieldHandle::AcquiresContextFromThis (runtimehandles.cpp:250) answers
            // `pField->IsSharedByGenericInstantiations()`: true for an *instance* field whose
            // FieldDesc belongs to a canonical `__Canon` MethodTable (field.h:398), which is when the
            // field's approximate declaring type is not its exact one and the exact type has to be
            // recovered from the `this` object. Its two managed callers exist only to compensate for
            // that: `MemberInfoCache.AddField` (RuntimeType.CoreCLR.cs:311) switches from an exact
            // comparison of the declaring type against the reflected type to a canonical one, and
            // `RuntimeType.GetFieldInfo` (RuntimeType.CoreCLR.cs:1988) lets a caller-supplied
            // declaring type through when it merely canonicalises alike.
            //
            // PawPrint shares no field descriptions between instantiations: a `FieldHandle` records
            // the exact `RuntimeTypeHandleTarget` the guest asked about, and `GetApproxDeclaringType`
            // above hands that exact type back. So the answer is `false` for every handle the registry
            // mints, and both callers then take their exact-comparison arm, which is the right one for
            // an exact type. The one observable consequence is the same one the method-handle
            // registry already has, recorded in docs/divergences.md ("A field handle is
            // per-instantiation ..."): CoreCLR accepts `GetFieldFromHandle` on a handle from one
            // reference-type instantiation together with a *different* reference-type instantiation
            // of the same definition, because the two canonicalise alike, and PawPrint rejects it with
            // the `ArgumentException` the exact comparison produces.
            let operation = "RuntimeFieldHandle.AcquiresContextFromThis"

            let fieldHandle =
                // CoreCLR's PRECONDITION(CheckPointer(pField)) — a null handle is a caller bug, and the
                // sibling arms above fault the same way.
                fieldHandleOfRuntimeFieldHandleInternal operation state instruction.Arguments.[0]
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: null field handle")

            let acquiresContextFromThis =
                match fieldHandle.GetDeclaringTypeHandle () with
                | RuntimeTypeHandleTarget.Closed _
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ -> false
                | RuntimeTypeHandleTarget.GenericParameter _
                | RuntimeTypeHandleTarget.MethodGenericParameter _
                | RuntimeTypeHandleTarget.OpenConstructed _
                | RuntimeTypeHandleTarget.DynamicMethodsClass _
                | RuntimeTypeHandleTarget.Composite _
                | RuntimeTypeHandleTarget.FunctionPointer _ as other ->
                    // `FieldHandleRegistry.getOrAllocate` refuses these, so no handle can carry one.
                    failwith
                        $"BUG: %s{operation}: field-registry handle has declaring type %O{other}, which FieldHandleRegistry.getOrAllocate is supposed to have refused"

            let state =
                IlMachineState.pushToEvalStack (CliType.ofBool acquiresContextFromThis) ctx.Thread state

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
    /// and the storage key to it cannot drift from the field we are about to read or write.
    let private declaringTypeOfFieldHandle (operation : string) (fieldHandle : FieldHandle) : ConcreteTypeHandle =
        match fieldHandle.GetDeclaringTypeHandle () with
        | RuntimeTypeHandleTarget.Closed handle -> handle
        | notClosed ->
            // `FieldAccessor`'s constructor routes a declaring type with generic parameters to
            // `FieldAccessorType.NoInvoke` (FieldAccessor.cs:22-27), which throws before any QCall
            // runs, so no guest reaches this through `FieldInfo.GetValue` or `SetValue`.
            failwith
                $"%s{operation}: declaring type %O{notClosed} is not closed; CoreCLR raises NotSupportedException for a declaring type shared by generic instantiations (invokeutil.cpp:777), and the managed caller refuses one containing generic parameters before reaching here"

    /// The `bool` a `[MarshalAs(UnmanagedType.Bool)] ref bool` argument addresses, which reaches
    /// us as a four-byte cell.
    let private readIsClassInitialized
        (ctx : NativeCallContext)
        (operation : string)
        (isClassInitializedPtr : ManagedPointerSource)
        (state : IlMachineState)
        : bool
        =
        match IlMachineState.readManagedByref ctx.BaseClassTypes state isClassInitializedPtr with
        | CliType.Numeric (CliNumericType.Int32 i) -> i <> 0
        | other -> failwith $"%s{operation}: expected Int32 in pIsClassInitialized, got %O{other}"

    /// The object an `ObjectHandleOnStack` argument addresses.
    let private objectHandleOnStackContents
        (ctx : NativeCallContext)
        (operation : string)
        (argName : string)
        (arg : CliType)
        (state : IlMachineState)
        : ManagedHeapAddress option
        =
        let ptr = NativeCall.objectHandleOnStackTarget operation state argName arg

        // An `ObjectHandleOnStack` names a slot holding an object reference, so this wants the
        // object-aware reader rather than the byte-view one.
        match IlMachineState.readManagedByref ctx.BaseClassTypes state ptr with
        | CliType.ObjectRef addr -> addr
        | other -> failwith $"%s{operation}: expected ObjectRef in %s{argName} ObjectHandleOnStack, got %O{other}"

    /// Run the declaring type's class initialiser if the caller has not vouched that it has run.
    /// `Choice2Of2` is the handler's early return: the initialiser has been pushed as a frame (or
    /// is blocked on another thread's run of it), and the handler will be re-entered from the top
    /// once it completes.
    ///
    /// As an *input* the flag means "the caller has already established the class is
    /// initialised, so skip the check"; `FieldAccessor`'s permanent `SlowPath` state passes
    /// `true` (FieldAccessor.cs:193, :329) and its first-call state passes `false` (:177, :301).
    /// CoreCLR guards the cctor run on exactly this (invokeutil.cpp:785, :1010).
    let private ensureDeclaringClassInitialised
        (ctx : NativeCallContext)
        (operation : string)
        (declaringTypeHandle : ConcreteTypeHandle)
        (incomingIsClassInitialized : bool)
        (state : IlMachineState)
        : Choice<IlMachineState, NativeHandlerResult>
        =
        if incomingIsClassInitialized then
            Choice1Of2 state
        else
            match TypeInitTable.tryGet declaringTypeHandle state.TypeInitTable with
            | Some (TypeInitState.Failed _) ->
                // Refusing here rather than delegating is deliberate. CoreCLR catches a
                // failing initialiser and throws a *fresh* `TargetInvocationException`
                // wrapping the `TypeInitializationException` (`CreateTargetExcept`,
                // invokeutil.cpp:803, :1028) — unlike `ReflectionInvocation_RunClassConstructor`,
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

            match NativeHandlerResult.tryEarlyReturn ctx.Thread (state, whatWeDid) with
            | Some earlyReturn -> Choice2Of2 earlyReturn
            | None -> Choice1Of2 state

    /// Whether the *initialiser has completed*, which is what CoreCLR reports
    /// (`pDeclMT->IsClassInited()`, invokeutil.cpp:791, :1016) and what the write side's init-only
    /// gate reads (:811). `InProgress` is deliberately not "initialised": that is what makes
    /// reflectively setting a static readonly field from inside its own declaring type's
    /// initialiser legal, exactly as on real .NET.
    let private declaringClassIsInitialised (declaringTypeHandle : ConcreteTypeHandle) (state : IlMachineState) : bool =
        match TypeInitTable.tryGet declaringTypeHandle state.TypeInitTable with
        | Some TypeInitState.Initialized -> true
        | Some (TypeInitState.InProgress _)
        | Some (TypeInitState.Failed _)
        | None -> false

    /// The out-parameter half of `pIsClassInitialized`.
    let private writeBackIsClassInitialized
        (ctx : NativeCallContext)
        (isClassInitializedPtr : ManagedPointerSource)
        (incomingIsClassInitialized : bool)
        (classIsInitialised : bool)
        (state : IlMachineState)
        : IlMachineState
        =
        if incomingIsClassInitialized then
            // CoreCLR writes this cell only inside its `if (*pIsClassInitialized == FALSE)`
            // block (invokeutil.cpp:785-794, :1010-1019), leaving the caller's `true` alone
            // otherwise. Recomputing it would answer "not initialised" for a type the caller
            // vouched for but which has no `TypeInitTable` entry of ours — a worse answer than
            // the one it supplied, and not one it asked us to revisit.
            state
        else
            IlMachineState.writeManagedByrefWithBase
                ctx.BaseClassTypes
                state
                isClassInitializedPtr
                (CliType.Numeric (CliNumericType.Int32 (if classIsInitialised then 1 else 0)))

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

    /// The `object?` to hand back for the cell of a field of type `fieldTypeHandle`. Mirrors the
    /// switch in `InvokeUtil::GetFieldValue` (invokeutil.cpp:1035-1137): the reference-typed arms
    /// answer the `OBJECTREF` itself, and every value-typed arm allocates a box of the *field's*
    /// type and copies the cell into it — so an enum field answers a boxed enum, not a boxed
    /// integer — with `Nullable::NormalizeBox` then turning a `Nullable<T>` box into null or a
    /// boxed `T`. `Boxing.boxValue` is that whole rule, shared with the `box` opcode.
    let private valueToReturn
        (ctx : NativeCallContext)
        (operation : string)
        (fieldTypeHandle : ConcreteTypeHandle)
        (cell : CliType)
        (state : IlMachineState)
        : IlMachineState * CliType
        =
        match fieldTypeHandle with
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            // CoreCLR's `ELEMENT_TYPE_PTR` arm answers a `System.Reflection.Pointer` around the raw
            // address, and its `ELEMENT_TYPE_FNPTR` arm a boxed `IntPtr` holding it (measured:
            // `typeof(P).GetField("Ptr").GetValue(p).GetType()` is `System.Reflection.Pointer`).
            // PawPrint's pointer cell carries provenance rather than an address to wrap, and the
            // `box` opcode refuses a pointer type for the same reason (`executeBox`).
            failwith
                $"TODO: %s{operation} on a field of pointer type %O{fieldTypeHandle}; CoreCLR answers a System.Reflection.Pointer (or a boxed IntPtr for a function pointer) around the raw address, which PawPrint's provenance-tracked pointer cannot supply"
        | ConcreteTypeHandle.Byref _ ->
            // A `ref` field lives only in a ref struct, which cannot be boxed, so no instance can
            // reach `FieldInfo.GetValue` with one.
            failwith
                $"BUG: %s{operation} on a field of byref type %O{fieldTypeHandle}; only a ref struct declares one, and a ref struct instance cannot be handed to reflection"
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.Concrete _ ->

        if IlMachineState.isReferenceTypeHandle ctx.BaseClassTypes operation state fieldTypeHandle then
            match cell with
            | CliType.ObjectRef _ -> state, cell
            | other ->
                failwith
                    $"BUG: %s{operation}: the cell of a field of reference type %O{fieldTypeHandle} holds %O{other} rather than an object reference"
        else

        let boxed, state =
            Boxing.boxValue ctx.LoggerFactory ctx.BaseClassTypes fieldTypeHandle (EvalStackValue.ofCliType cell) state

        match boxed with
        | EvalStackValue.ObjectRef addr -> state, CliType.ObjectRef (Some addr)
        | EvalStackValue.NullObjectRef -> state, CliType.ObjectRef None
        | other ->
            failwith
                $"BUG: %s{operation}: boxing the cell of a field of type %O{fieldTypeHandle} produced %O{other}, expected an object reference"

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
        | "RuntimeFieldHandle_GetValue",
          "System.Private.CoreLib",
          "System",
          "RuntimeFieldHandle",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "ObjectHandleOnStack", instanceGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallTypeHandle", fieldTypeGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallTypeHandle", declaringTypeGenerics)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "ObjectHandleOnStack", resultGenerics) ],
          MethodReturnType.Void when
            instanceGenerics.IsEmpty
            && fieldTypeGenerics.IsEmpty
            && declaringTypeGenerics.IsEmpty
            && resultGenerics.IsEmpty
            ->
            // CoreCLR's `RuntimeFieldHandle_GetValue` (reflectioninvocation.cpp:28), which is
            // `InvokeUtil::GetFieldValue` (invokeutil.cpp:972) once the arguments are unpacked.
            // This is the primitive under every `FieldInfo.GetValue`: `FieldAccessor` reaches its
            // address-based fast paths only when `IsFastPathSupported` says a raw offset exists,
            // and PawPrint answers `false` there, so *all* reflective field reads land here.
            let operation = "RuntimeFieldHandle_GetValue"

            let fieldHandle =
                fieldHandleOfRuntimeFieldHandleInternal operation state instruction.Arguments.[0]
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: null field handle")

            let _declaringAssy, fieldInfo =
                FieldRvaData.fieldForHandle operation fieldHandle state

            let declaringTypeHandle = declaringTypeOfFieldHandle operation fieldHandle

            let isClassInitializedPtr =
                NativeCall.managedPointerOfPointerArgument operation "pIsClassInitialized" instruction.Arguments.[4]

            let incomingIsClassInitialized =
                readIsClassInitialized ctx operation isClassInitializedPtr state

            match
                ensureDeclaringClassInitialised ctx operation declaringTypeHandle incomingIsClassInitialized state
            with
            | Choice2Of2 result -> Some result
            | Choice1Of2 state ->

            let classIsInitialised = declaringClassIsInitialised declaringTypeHandle state

            let fieldTypeHandle =
                NativeCall.qCallTypeHandleToConcreteTypeHandle
                    operation
                    state
                    (EvalStackValue.ofCliType instruction.Arguments.[2])

            let isStatic = fieldInfo.Attributes.HasFlag FieldAttributes.Static

            let cell, state =
                if isStatic then
                    if fieldInfo.HasFieldRVA then
                        // An RVA-backed static's storage is the PE image's own bytes, which
                        // `ldsflda` reads (`peByteRangeForFieldRva`, UnaryMetadataFieldOps.fs)
                        // while `ldsfld` reads the ordinary static slot no store has touched. The
                        // write side refuses this shape for the same asymmetry; answering here
                        // would mean picking one of the two readings the opcodes disagree on.
                        failwith
                            $"TODO: %s{operation} on the RVA-backed static field %s{fieldInfo.Name}; its storage is the PE byte range that `ldsflda` reads, which `getStatic` does not see"

                    // A `[ThreadStatic]` field reads the calling thread's own slot, as `ldsfld`
                    // does; that is also the one field kind for which CoreCLR itself answers
                    // `IsFastPathSupported = false`, so both runtimes reach this by the same route.
                    match
                        IlMachineState.getStatic
                            (StaticOwner.forField ctx.Thread fieldInfo)
                            declaringTypeHandle
                            (ComparableFieldDefinitionHandle.Make fieldInfo.Handle)
                            state
                    with
                    | Some cell -> cell, state
                    | None ->
                        // Storage no `stsfld` has touched holds the field type's zero, which is
                        // what `ldsfld` answers (and stores back; a read need not).
                        IlMachineState.cliTypeZeroOfHandle state ctx.BaseClassTypes fieldTypeHandle
                else

                let target =
                    match objectHandleOnStackContents ctx operation "instance" instruction.Arguments.[1] state with
                    | Some addr -> addr
                    | None ->
                        // Managed `VerifyTarget` (FieldAccessor.cs:351) has already thrown
                        // `TargetException` for a null instance, so this cannot arrive.
                        failwith
                            $"%s{operation}: null instance for the instance field %s{fieldInfo.Name}; the managed caller checks this before the QCall"

                let fieldId = FieldId.metadata declaringTypeHandle fieldInfo.Handle fieldInfo.Name

                // The same two-step read as `ldfld` on an object reference: a field whose
                // canonical storage lies outside the field map (`String._firstChar`,
                // `RawArrayData::Length`) is projected, and everything else is looked up. A
                // boxed-struct target is an ordinary non-array object keyed by the same
                // `FieldId`s, so `FieldInfo.GetValue(boxedStruct)` reads out of the box.
                match RuntimeFieldProjection.tryProjectFieldLoad ctx.BaseClassTypes fieldInfo target state with
                | Some cell -> cell, state
                | None ->
                    AllocatedNonArrayObject.DereferenceFieldById fieldId (ManagedHeap.get target state.ManagedHeap),
                    state

            let state, result = valueToReturn ctx operation fieldTypeHandle cell state

            let resultPtr =
                NativeCall.objectHandleOnStackTarget operation state "result" instruction.Arguments.[5]

            let state =
                IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state resultPtr result

            let state =
                writeBackIsClassInitialized
                    ctx
                    isClassInitializedPtr
                    incomingIsClassInitialized
                    classIsInitialised
                    state

            NativeHandlerResult.completed state |> Some
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
                readIsClassInitialized ctx operation isClassInitializedPtr state

            match
                ensureDeclaringClassInitialised ctx operation declaringTypeHandle incomingIsClassInitialized state
            with
            | Choice2Of2 result -> Some result
            | Choice1Of2 state ->

            let classIsInitialised = declaringClassIsInitialised declaringTypeHandle state

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

            let value =
                objectHandleOnStackContents ctx operation "value" instruction.Arguments.[2] state

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
                    match objectHandleOnStackContents ctx operation "instance" instruction.Arguments.[1] state with
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
                writeBackIsClassInitialized
                    ctx
                    isClassInitializedPtr
                    incomingIsClassInitialized
                    classIsInitialised
                    state

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
