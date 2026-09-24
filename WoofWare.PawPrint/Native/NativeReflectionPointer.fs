namespace WoofWare.PawPrint

/// The `System.Reflection.Pointer` object through which reflection carries an unmanaged pointer
/// in an `object`, and the rule by which any value of pointer or function-pointer type becomes an
/// `object`. Both reflection primitives that hand such a value back to managed code — the
/// `RuntimeMethodHandle_InvokeMethod` QCall for a return value and `RuntimeFieldHandle_GetValue`
/// for a field — follow the one rule.
[<RequireQualifiedAccess>]
module internal NativeReflectionPointer =

    /// CoreCLR's `InvokeUtil::CreatePointer` (invokeutil.cpp:58): a fresh `System.Reflection.Pointer`
    /// whose `_ptr` is `value` and whose `_ptrType` is the `RuntimeType` of `pointerType` itself,
    /// not of its pointee. That is the shape `Pointer.Box` produces, and `_ptrType` is what
    /// `RuntimeType.TryChangeTypeSpecial` reads back when the `Pointer` is later passed as an
    /// argument. CoreCLR writes both fields directly rather than running `Pointer`'s constructor,
    /// and so does this.
    let private createPointer
        (ctx : NativeCallContext)
        (pointerType : ConcreteTypeHandle)
        (value : EvalStackValue)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        match pointerType with
        | ConcreteTypeHandle.Pointer _ -> ()
        | other -> failwith $"createPointer: %O{other} is not an unmanaged pointer type"

        let state, _, pointerClass =
            NativeRuntimeTypeHelpers.concretizeNonGenericCorelibType
                ctx.LoggerFactory
                ctx.BaseClassTypes
                state
                "System.Reflection"
                "Pointer"

        let addr, state =
            IlMachineState.allocateUninitialisedInstance ctx.LoggerFactory ctx.BaseClassTypes pointerClass state

        let ptrTypeObject, state =
            IlMachineState.getOrAllocateType
                ctx.LoggerFactory
                ctx.BaseClassTypes
                (RuntimeTypeHandleTarget.Closed pointerType)
                state

        // Coerce against the field's own zero so the stored cell has the `void*` field's shape
        // while keeping whatever provenance `value` carries.
        let ptrField = IlMachineState.requiredOwnInstanceFieldId state pointerClass "_ptr"

        let ptrZero =
            ManagedHeap.get addr state.ManagedHeap
            |> AllocatedNonArrayObject.DereferenceFieldById ptrField

        let state =
            state
            |> IlMachineState.setInstanceFieldById addr ptrField (EvalStackValue.toCliTypeCoerced ptrZero value)
            |> IlMachineState.setOwnInstanceField addr "_ptrType" (CliType.ObjectRef (Some ptrTypeObject))

        addr, state

    /// The object reflection hands back for `value`, a value of the unmanaged pointer or
    /// function-pointer type `ty`: a `System.Reflection.Pointer` recording `ty` for a pointer,
    /// even a null one, and a boxed `IntPtr` carrying the value for a function pointer. This is
    /// the split both `InvokeUtil::CreateObjectAfterInvoke` (invokeutil.cpp:555, :570) and
    /// `InvokeUtil::GetFieldValue` (invokeutil.cpp:1111, :1125) make. The result keeps whatever
    /// provenance `value` carries, so it still dereferences to the same storage once unwrapped.
    let toObject
        (ctx : NativeCallContext)
        (ty : ConcreteTypeHandle)
        (value : EvalStackValue)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        match ty with
        | ConcreteTypeHandle.Pointer _ -> createPointer ctx ty value state
        | ConcreteTypeHandle.FunctionPointer _ ->
            let intPtr =
                AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes ctx.BaseClassTypes.IntPtr

            Boxing.boxValueType ctx.LoggerFactory ctx.BaseClassTypes intPtr value state
        | ConcreteTypeHandle.Concrete _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            failwith
                $"NativeReflectionPointer.toObject: %O{ty} is neither an unmanaged pointer nor a function pointer type"

    /// CoreCLR's `InvokeUtil::GetPointerValue` (invokeutil.cpp:99): the `_ptr` of the
    /// `System.Reflection.Pointer` at `addr`, with whatever provenance it carries, or `None` if
    /// the object at `addr` is not a `System.Reflection.Pointer`.
    let tryPointerValue (state : IlMachineState) (addr : ManagedHeapAddress) : CliType option =
        match ManagedHeap.tryGet addr state.ManagedHeap with
        | None -> None
        | Some obj ->
            match obj.ConcreteType with
            | CorelibType state.ConcreteTypes ("System.Reflection", "Pointer", generics) when generics.IsEmpty ->
                let ptrField =
                    IlMachineState.requiredOwnInstanceFieldId state obj.ConcreteType "_ptr"

                AllocatedNonArrayObject.DereferenceFieldById ptrField obj |> Some
            | _ -> None
