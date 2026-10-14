namespace WoofWare.PawPrint

/// How a multicast delegate's invocation list holds its elements, which its `DelegateLayout`
/// decides.
[<RequireQualifiedAccess>]
type InvocationListElements =
    /// An `object[]` whose elements are the delegates themselves.
    | Delegates
    /// A `Delegate.Wrapper[]`, each element of which names its delegate in the field `value`.
    | Wrappers of value : FieldInfo<GenericParamFromMetadata, TypeDefn>

/// What a delegate's fields beyond its binding say, read according to its `DelegateLayout`: the
/// rows of CoreCLR's delegate kinds table (comdelegate.cpp:2857-2867) that the binding fields
/// cannot tell apart.
[<RequireQualifiedAccess>]
type DelegateInvocations =
    /// The count is zero, so the binding fields say everything: a closed delegate, or an open one
    /// whose target needs no virtual call stub.
    | BindingOnly
    /// A multicast delegate, which invokes the first `count` elements of the array `list`.
    | Multicast of list : ManagedHeapAddress * count : int64 * elements : InvocationListElements
    /// An `InvocationListAndCount` open virtual delegate, whose `_invocationCount` is its target's
    /// `MethodDesc*`, the method-registry id `registryId`, beside the virtual call stub over that
    /// target in `_methodPtrAux`.
    | OpenVirtual of registryId : int64
    /// A `HelperObjectAndExtraData` single-cast delegate whose `_extraData` records its target's
    /// `MethodDesc*`, the method-registry id `registryId`: written when it is bound, or cached by
    /// `COMDelegate::GetMethodDesc`, which answers it without consulting the binding fields.
    | RecordedMethodDesc of registryId : int64
    /// A row PawPrint builds no delegate of: a wrapper delegate, an unmanaged function pointer
    /// delegate, or an inner open virtual delegate of a wrapper. `description` names what the
    /// fields hold.
    | Unbuilt of description : string

[<RequireQualifiedAccess>]
module DelegateInvocations =

    /// The native int that `field`, one of a `DelegateLayout`'s fields, holds in `delegateObject`.
    let readNativeInt
        (operation : string)
        (allConcreteTypes : AllConcreteTypes)
        (field : FieldInfo<GenericParamFromMetadata, TypeDefn>)
        (delegateObject : AllocatedNonArrayObject)
        : NativeIntSource
        =
        // These fields are typed `IntPtr`/`nint` (primitive-like); unwrap to the inner NativeInt.
        match
            AllocatedNonArrayObject.DereferenceFieldById (DelegateLayout.fieldId allConcreteTypes field) delegateObject
            |> CliType.unwrapPrimitiveLikeDeep
        with
        | CliType.Numeric (CliNumericType.NativeInt src) -> src
        // `NewMulticastDelegate` stores `GetInvokeMethod()`'s answer, a method-registry pointer,
        // straight into `_methodPtrAux`.
        | CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle id) -> NativeIntSource.MethodHandlePtr id
        | other -> failwith $"%s{operation}: expected %s{field.Name} to be a native int, got %O{other}"

    /// The object reference that `field`, one of a `DelegateLayout`'s fields, holds in
    /// `delegateObject`.
    let readObjectRef
        (operation : string)
        (allConcreteTypes : AllConcreteTypes)
        (field : FieldInfo<GenericParamFromMetadata, TypeDefn>)
        (delegateObject : AllocatedNonArrayObject)
        : ManagedHeapAddress option
        =
        match
            AllocatedNonArrayObject.DereferenceFieldById (DelegateLayout.fieldId allConcreteTypes field) delegateObject
            |> CliType.unwrapPrimitiveLikeDeep
        with
        | CliType.ObjectRef target -> target
        | other -> failwith $"%s{operation}: expected %s{field.Name} to be an object reference, got %O{other}"

    /// What the fields beyond its binding say of the delegate at `delegateAddr` on `heap`, read
    /// according to `baseClassTypes`' CoreLib's `DelegateLayout`. Refuses a CoreLib with no known
    /// layout, and a delegate whose fields hold values of the wrong kinds.
    let read
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allConcreteTypes : AllConcreteTypes)
        (heap : ManagedHeap)
        (delegateAddr : ManagedHeapAddress)
        : DelegateInvocations
        =
        let delegateObject = ManagedHeap.get delegateAddr heap

        // `NewMulticastDelegate` stores an `int`, so a multicast delegate's count is always one.
        let verbatimCount (field : FieldInfo<GenericParamFromMetadata, TypeDefn>) (count : NativeIntSource) : int64 =
            match count with
            | NativeIntSource.Verbatim count -> count
            | other -> failwith $"%s{operation}: expected %s{field.Name} to be a verbatim native int, got %O{other}"

        let unbuilt
            (countField : FieldInfo<GenericParamFromMetadata, TypeDefn>)
            (count : NativeIntSource)
            (listField : FieldInfo<GenericParamFromMetadata, TypeDefn>)
            (list : ManagedHeapAddress option)
            : DelegateInvocations
            =
            DelegateInvocations.Unbuilt $"%s{countField.Name} %O{count} and %s{listField.Name} %O{list}"

        match DelegateLayout.require baseClassTypes with
        | DelegateLayout.InvocationListAndCount (_, invocations) ->
            let count =
                readNativeInt operation allConcreteTypes invocations.InvocationCount delegateObject

            if NativeIntSource.isZero count then
                DelegateInvocations.BindingOnly
            else

            let list =
                readObjectRef operation allConcreteTypes invocations.InvocationList delegateObject

            match list, count with
            | Some list, _ when ManagedHeap.isArray list heap ->
                DelegateInvocations.Multicast (
                    list,
                    verbatimCount invocations.InvocationCount count,
                    InvocationListElements.Delegates
                )
            | None, NativeIntSource.MethodHandlePtr registryId -> DelegateInvocations.OpenVirtual registryId
            | _ -> unbuilt invocations.InvocationCount count invocations.InvocationList list
        | DelegateLayout.HelperObjectAndExtraData (_, helper) ->
            let extraData =
                readNativeInt operation allConcreteTypes helper.ExtraData delegateObject

            if NativeIntSource.isZero extraData then
                DelegateInvocations.BindingOnly
            else

            let helperObject =
                readObjectRef operation allConcreteTypes helper.HelperObject delegateObject

            // `HasSingleTarget`: `_helperObject` is an invocation list exactly when it is a
            // `Wrapper[]`, because it also caches `Delegate.Method` and keeps a collectible
            // delegate's loader allocator alive.
            let isWrapperArray (addr : ManagedHeapAddress) : bool =
                match
                    AllConcreteTypes.findExistingNonGenericConcreteType
                        allConcreteTypes
                        helper.WrapperValue.DeclaringType.Identity
                with
                // No `Wrapper` was ever concretised, so nothing on the heap is a `Wrapper[]`.
                | None -> false
                | Some wrapper ->
                    ManagedHeap.isArray addr heap
                    && ManagedHeap.getObjectConcreteType addr heap = ConcreteTypeHandle.OneDimArrayZero wrapper

            // The delegate kinds table (comdelegate.cpp:2569) gives `_extraData` as the count of a
            // multicast delegate, -1 for an unmanaged function pointer delegate, and otherwise
            // the target's `MethodDesc*`, whatever the binding fields hold.
            match helperObject, extraData with
            | Some list, _ when isWrapperArray list ->
                DelegateInvocations.Multicast (
                    list,
                    verbatimCount helper.ExtraData extraData,
                    InvocationListElements.Wrappers helper.WrapperValue
                )
            | _, NativeIntSource.MethodHandlePtr registryId -> DelegateInvocations.RecordedMethodDesc registryId
            | _ -> unbuilt helper.ExtraData extraData helper.HelperObject helperObject

    /// The delegate that element `index` of the multicast invocation list `list` on `heap` names,
    /// its elements held as `elements` says.
    let element
        (operation : string)
        (allConcreteTypes : AllConcreteTypes)
        (elements : InvocationListElements)
        (list : ManagedHeapAddress)
        (index : int)
        (heap : ManagedHeap)
        : ManagedHeapAddress
        =
        let refuse (held : CliType) : ManagedHeapAddress =
            failwith
                $"%s{operation}: expected element %d{index} of the invocation list to name a delegate, got %O{held}"

        let held = ManagedHeap.getArrayValue list index heap

        match elements with
        | InvocationListElements.Delegates ->
            match held with
            | CliType.ObjectRef (Some element) -> element
            | other -> refuse other
        | InvocationListElements.Wrappers value ->
            match held with
            | CliType.ValueType wrapper ->
                match CliValueType.DereferenceFieldById (DelegateLayout.fieldId allConcreteTypes value) wrapper with
                | CliType.ObjectRef (Some element) -> element
                | other -> refuse other
            | other -> refuse other
