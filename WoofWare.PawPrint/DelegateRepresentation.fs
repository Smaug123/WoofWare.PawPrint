namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

/// <summary>
/// How a single-cast delegate is bound, in the terms of CoreCLR's delegate kinds table
/// (comdelegate.cpp:2857-2867). Which one applies is decided by arity alone: a delegate is
/// <c>Open</c> when its <c>Invoke</c> supplies every argument the target takes, and
/// <c>Closed</c> when it supplies one fewer.
/// </summary>
[<RequireQualifiedAccess>]
type DelegateBinding =
    /// Kinds 1 and 4: `_target` is the bound first argument (the receiver of an instance target,
    /// or the first parameter of a static one, and possibly null in the latter case), and
    /// `_methodPtr` names the target.
    | Closed of target : ManagedHeapAddress option * methodPtr : FunctionPointerTarget
    /// Kinds 2, 3 and 6: `_target` is the delegate itself, `_methodPtr` is the shuffle thunk, and
    /// `_methodPtrAux` holds `aux` — the target, or for kind 3 a
    /// <see cref="FunctionPointerTarget.VirtualCallStub"/> over it, in which case `_invocationCount`
    /// also names the target method.
    | Open of aux : FunctionPointerTarget

/// What an open delegate over a given method stores in `_methodPtrAux`, or that it cannot be built.
[<RequireQualifiedAccess>]
type OpenDelegateAux =
    /// The delegate can be built, with this in `_methodPtrAux`.
    | Aux of FunctionPointerTarget
    /// The target needs a virtual call stub but has a generic instantiation of its own, which
    /// `GetVirtualCallStub` refuses by raising `NotSupportedException` (comdelegate.cpp:979-982).
    /// Measured: real .NET raises it from `CreateDelegate` even with `throwOnBindFailure: false`.
    | GenericVirtualUnsupported

/// What invoking a single-cast delegate does, read back off its fields.
[<RequireQualifiedAccess>]
type DelegateInvocation =
    /// Call `methodPtr` with `target` prepended to `Invoke`'s arguments. Kinds 1 and 4, and also a
    /// multicast delegate, whose `_target` is itself and whose `_methodPtr` is its invoke stub.
    | ThroughMethodPtr of target : ManagedHeapAddress option * methodPtr : FunctionPointerTarget
    /// `_methodPtr` is the shuffle thunk: call `aux` with `Invoke`'s arguments and nothing
    /// prepended. `aux` is either the target itself or a
    /// <see cref="FunctionPointerTarget.VirtualCallStub"/>.
    | ThroughShuffleThunk of aux : FunctionPointerTarget

/// <summary>
/// The fields of <c>System.Delegate</c> and <c>System.MulticastDelegate</c> that say what a
/// delegate is bound to, written and read with the meaning CoreCLR gives them.
/// </summary>
/// <remarks>
/// CoreLib's own managed code reads these fields — <c>Delegate.Target</c>, <c>Equals</c>,
/// <c>GetHashCode</c> and <c>GetMethodImpl</c> all branch on whether <c>_methodPtrAux</c> is zero —
/// so their contents are guest-observable, and every delegate PawPrint builds is laid out as the
/// kinds table says. The two stubs CoreCLR generates for an open delegate are
/// <see cref="FunctionPointerTarget.OpenDelegateShuffleThunk"/> and
/// <see cref="FunctionPointerTarget.VirtualCallStub"/>; `AbstractMachine.dispatchDelegateInvoke`
/// performs what each would do, without pushing a frame for either, which is also how CoreCLR's
/// stack walk reports them.
/// </remarks>
[<RequireQualifiedAccess>]
module DelegateRepresentation =

    let private functionPointer (target : FunctionPointerTarget) : CliType =
        CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FunctionPointer target))

    /// <summary>
    /// What an open delegate over <paramref name="method" /> stores in <c>_methodPtrAux</c>:
    /// <c>COMDelegate::BindToMethod</c>'s rule (comdelegate.cpp:1236-1245), which uses a virtual
    /// call stub for every virtual target except one declared on a value type. A value type
    /// cannot be derived from, so its virtuals need no dispatch, and the stub could not dispatch
    /// on the unboxed receiver an open delegate over one is handed anyway.
    /// </summary>
    /// <remarks>
    /// <c>IsVirtual</c>, not <c>DispatchesVirtually</c>: CoreCLR takes the stub path for a
    /// <c>final</c> virtual too, where it resolves to the method itself, and for a static
    /// abstract interface method, where invoking it raises. Only the latter is observable, and
    /// only because of this choice. The delegate constructor's rule differs for statics; see
    /// <c>construct</c>.
    /// </remarks>
    let openAux
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (method : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (declaringType : ConcreteTypeHandle)
        (state : IlMachineState)
        : IlMachineState * OpenDelegateAux
        =
        if
            not method.IsVirtual
            || not (IlMachineState.isReferenceTypeHandle baseClassTypes operation state declaringType)
        then
            state, OpenDelegateAux.Aux (FunctionPointerTarget.Managed method)
        elif not method.Generics.IsEmpty then
            state, OpenDelegateAux.GenericVirtualUnsupported
        else

        // `GetTokenFromOwnerAndSlot(TypeHandle(pExactMethodType), pMD->GetSlot())`.
        let state, slotTable =
            VirtualSlotLayout.slotTableOfClosed loggerFactory baseClassTypes operation state declaringType

        let slot =
            slotTable
            |> MethodTableLayout.slotIndexInTable (method.DeclaringAssemblyFullName, method.IdentityKey)
            |> Option.defaultWith (fun () ->
                failwith
                    $"%s{operation}: %s{method.Name} occupies no slot in its declaring type %s{MethodOwner.describe method.Owner}"
            )

        let isInterface =
            match IlMachineState.tryGetConcreteTypeInfo state declaringType with
            | Some (_, typeInfo) -> typeInfo.IsInterface
            | None -> failwith $"%s{operation}: declaring type %O{declaringType} has no TypeDef row"

        let token =
            if isInterface then
                VirtualDispatchToken.InterfaceSlot (declaringType, slot)
            else
                VirtualDispatchToken.ClassSlot slot

        state, OpenDelegateAux.Aux (FunctionPointerTarget.VirtualCallStub (token, method))

    /// Write `binding` into the delegate at `delegateAddr`. Every field the binding determines is
    /// written, so this is correct on a freshly allocated delegate and needs no prior state.
    let write
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (delegateAddr : ManagedHeapAddress)
        (binding : DelegateBinding)
        (state : IlMachineState)
        : IlMachineState
        =
        let layout = DelegateLayout.require baseClassTypes
        let zero = CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))

        let target, methodPtr, aux, state, invocationCount =
            match binding with
            | DelegateBinding.Closed (target, methodPtr) -> target, methodPtr, zero, state, zero
            | DelegateBinding.Open aux ->
                let state, invocationCount =
                    match aux with
                    | FunctionPointerTarget.VirtualCallStub (_, method) ->
                        // `SetInvocationCount((INT_PTR)(void *)pTargetMethod)`: the `MethodDesc*`,
                        // which is what `COMDelegate::GetMethodDesc` reads the target back from.
                        let registryId, registry =
                            MethodHandleRegistry.getOrAllocateConcreteId state.ConcreteTypes method state.MethodHandles

                        { state with
                            MethodHandles = registry
                        },
                        NativeIntSource.MethodHandlePtr registryId
                    | FunctionPointerTarget.Managed _
                    | FunctionPointerTarget.Dynamic _ -> state, NativeIntSource.Verbatim 0L
                    | FunctionPointerTarget.RuntimeAllocator
                    | FunctionPointerTarget.OpenDelegateShuffleThunk
                    | FunctionPointerTarget.UnboxingStub _ ->
                        failwith $"logic error: an open delegate's _methodPtrAux cannot be %O{aux}"

                Some delegateAddr,
                FunctionPointerTarget.OpenDelegateShuffleThunk,
                functionPointer aux,
                state,
                CliType.Numeric (CliNumericType.NativeInt invocationCount)

        let set
            (field : FieldInfo<GenericParamFromMetadata, TypeDefn>)
            (value : CliType)
            (heap : ManagedHeap)
            : ManagedHeap
            =
            ManagedHeap.setFieldById delegateAddr (DelegateLayout.fieldId state.ConcreteTypes field) value heap

        let heap =
            match layout with
            | DelegateLayout.InvocationListAndCount (fields, invocations) ->
                state.ManagedHeap
                |> set fields.Target (CliType.ObjectRef target)
                |> set fields.MethodPtr (functionPointer methodPtr)
                |> set fields.MethodPtrAux aux
                |> set invocations.InvocationCount invocationCount

        { state with
            ManagedHeap = heap
        }

    let private nativeIntField
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

    let private objectRefField
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

    let private functionPointerField
        (operation : string)
        (allConcreteTypes : AllConcreteTypes)
        (field : FieldInfo<GenericParamFromMetadata, TypeDefn>)
        (delegateObject : AllocatedNonArrayObject)
        : FunctionPointerTarget
        =
        match nativeIntField operation allConcreteTypes field delegateObject with
        | NativeIntSource.FunctionPointer target -> target
        | other -> failwith $"%s{operation}: expected %s{field.Name} to hold a function pointer, got %O{other}"

    /// What invoking the delegate at `delegateAddr` calls: CoreCLR's `Invoke` stub, which calls
    /// `_methodPtr` with `_target` as its first argument, with the shuffle thunk's own behaviour
    /// folded in when that is what `_methodPtr` names.
    let invocationOf
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (delegateAddr : ManagedHeapAddress)
        (state : IlMachineState)
        : DelegateInvocation
        =
        let fields = DelegateLayout.require baseClassTypes |> DelegateLayout.binding
        let delegateObject = ManagedHeap.get delegateAddr state.ManagedHeap

        match functionPointerField operation state.ConcreteTypes fields.MethodPtr delegateObject with
        | FunctionPointerTarget.OpenDelegateShuffleThunk ->
            match functionPointerField operation state.ConcreteTypes fields.MethodPtrAux delegateObject with
            | FunctionPointerTarget.OpenDelegateShuffleThunk
            | FunctionPointerTarget.RuntimeAllocator as aux ->
                failwith $"%s{operation}: an open delegate's _methodPtrAux names %O{aux}, which is not a call target"
            | FunctionPointerTarget.UnboxingStub _ as aux ->
                // `COMDelegate::BindToMethod` swaps an unboxing stub for the unboxed entry point
                // before storing it, since an open delegate passes a value-type receiver by byref.
                failwith
                    $"%s{operation}: an open delegate's _methodPtrAux names %O{aux}, which an open delegate never holds"
            | aux -> DelegateInvocation.ThroughShuffleThunk aux
        | methodPtr ->
            let target =
                objectRefField operation state.ConcreteTypes fields.Target delegateObject

            DelegateInvocation.ThroughMethodPtr (target, methodPtr)

    /// <summary>
    /// The registry id of the method the delegate at <paramref name="delegateAddr" /> is bound
    /// to: <c>COMDelegate::GetMethodDesc</c> (comdelegate.cpp:1815), which is what both
    /// <c>Delegate_FindMethodHandle</c> and <c>Delegate_InternalEqualMethodHandles</c> consult.
    /// A registry id is PawPrint's <c>MethodDesc*</c>: two delegates are bound to the same method
    /// exactly when this answers the same id for both.
    /// </summary>
    /// <remarks>
    /// For a multicast delegate this is its type's <c>Invoke</c>, as it is in CoreCLR; managed code
    /// answers <c>Delegate.Method</c> for one from its last element instead, and never asks.
    /// </remarks>
    let methodDescOf
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (delegateAddr : ManagedHeapAddress)
        (state : IlMachineState)
        : IlMachineState * int64
        =
        let layout = DelegateLayout.require baseClassTypes
        let fields = DelegateLayout.binding layout
        let delegateObject = ManagedHeap.get delegateAddr state.ManagedHeap

        let idOfTarget (state : IlMachineState) (fieldName : string) (target : FunctionPointerTarget) =
            match target with
            | FunctionPointerTarget.Managed method ->
                let registryId, registry =
                    MethodHandleRegistry.getOrAllocateConcreteId state.ConcreteTypes method state.MethodHandles

                { state with
                    MethodHandles = registry
                },
                registryId
            | FunctionPointerTarget.Dynamic handle -> state, handle.GetRegistryId ()
            | FunctionPointerTarget.RuntimeAllocator
            | FunctionPointerTarget.OpenDelegateShuffleThunk
            | FunctionPointerTarget.VirtualCallStub _ ->
                failwith $"%s{operation}: the delegate's %s{fieldName} names %O{target}, which is not a method"
            | FunctionPointerTarget.UnboxingStub _ ->
                // CoreCLR's closed delegates over a value-type method do hold one, but PawPrint's
                // delegate constructor stores the method itself, so nothing writes one here.
                failwith
                    $"%s{operation}: the delegate's %s{fieldName} names %O{target}, which no PawPrint delegate binding produces"

        let methodPtrAux =
            nativeIntField operation state.ConcreteTypes fields.MethodPtrAux delegateObject

        // The rows of CoreCLR's table that the invocation fields decide, or `None` for a delegate
        // whose binding fields alone say what it is bound to.
        let fromInvocationFields : (IlMachineState * int64) option =
            match layout with
            | DelegateLayout.InvocationListAndCount (_, invocations) ->
                let invocationCount =
                    nativeIntField operation state.ConcreteTypes invocations.InvocationCount delegateObject

                if NativeIntSource.isZero invocationCount then
                    None
                else

                let invocationList =
                    objectRefField operation state.ConcreteTypes invocations.InvocationList delegateObject

                match invocationList, invocationCount with
                | Some list, _ when ManagedHeap.isArray list state.ManagedHeap ->
                    // A multicast delegate: `FindDelegateInvokeMethod`.
                    let state, invoke =
                        MulticastDelegateStub.invokeMethodOf
                            loggerFactory
                            baseClassTypes
                            operation
                            (ManagedHeap.getObjectConcreteType delegateAddr state.ManagedHeap)
                            state

                    let registryId, registry =
                        MethodHandleRegistry.getOrAllocateConcreteId state.ConcreteTypes invoke state.MethodHandles

                    Some (
                        { state with
                            MethodHandles = registry
                        },
                        registryId
                    )
                | None, NativeIntSource.MethodHandlePtr registryId ->
                    // An open virtual delegate: `GetMethodDescForOpenVirtualDelegate` reads the
                    // `MethodDesc*` out of `_invocationCount`. `_methodPtrAux` holds the stub over
                    // that same method, and the two are written together by `write`.
                    match methodPtrAux with
                    | NativeIntSource.FunctionPointer (FunctionPointerTarget.VirtualCallStub (_, method)) ->
                        let state, stubId =
                            idOfTarget state fields.MethodPtrAux.Name (FunctionPointerTarget.Managed method)

                        if stubId <> registryId then
                            failwith
                                $"%s{operation}: an open virtual delegate's _invocationCount names method %d{registryId} but its virtual call stub dispatches method %d{stubId}"

                        Some (state, registryId)
                    | other ->
                        failwith
                            $"%s{operation}: _invocationCount names method %d{registryId}, but _methodPtrAux is %O{other} rather than a virtual call stub"
                | _ ->
                    // The remaining shapes of CoreCLR's table with a count: a wrapper delegate
                    // (`_invocationList` is the inner delegate), an unmanaged function pointer
                    // delegate (`_invocationCount == -1`), and an inner open virtual delegate of a
                    // wrapper. PawPrint builds none of them.
                    failwith
                        $"TODO: %s{operation} was handed a delegate with _invocationCount %O{invocationCount} and _invocationList %O{invocationList}, which is a wrapper or unmanaged-function-pointer delegate; PawPrint builds neither"

        match fromInvocationFields with
        | Some answer -> answer
        | None ->

        if not (NativeIntSource.isZero methodPtrAux) then
            match methodPtrAux with
            | NativeIntSource.FunctionPointer target -> idOfTarget state fields.MethodPtrAux.Name target
            | other -> failwith $"%s{operation}: expected _methodPtrAux to hold a function pointer, got %O{other}"
        else
            functionPointerField operation state.ConcreteTypes fields.MethodPtr delegateObject
            |> idOfTarget state fields.MethodPtr.Name

    /// <summary>
    /// The delegate constructor every delegate type's <c>.ctor(object, IntPtr)</c> runs:
    /// <c>Delegate_Construct</c> (comdelegate.cpp:1665), which is also what the JIT's choice
    /// among <c>MulticastDelegate</c>'s <c>Ctor*</c> helpers computes. When CoreCLR refuses the
    /// delegate, returns the exception for the caller to raise, with its message if that is not the
    /// parameterless constructor's: <c>ArgumentException(Arg_DlgtNullInst)</c> for a delegate
    /// closed over a null receiver of an instance method, and <c>NotSupportedException</c> for one
    /// open over a generic virtual method.
    /// </summary>
    let construct
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (instruction : MethodState)
        (state : IlMachineState)
        : Result<IlMachineState, TypeInfo<GenericParamFromMetadata, TypeDefn> * string option>
        =
        let operation = "delegate constructor"

        let constructing =
            match instruction.Arguments.[0] with
            | CliType.ObjectRef (Some target) -> target
            | other -> failwith $"%s{operation}: expected the delegate under construction, got %O{other}"

        let target =
            match instruction.Arguments.[1] with
            | CliType.ObjectRef target -> target
            | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null) -> None
            | other -> failwith $"%s{operation}: unexpected target argument %O{other}"

        let methodPtr =
            match instruction.Arguments.[2] |> CliType.unwrapPrimitiveLikeDeep with
            | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FunctionPointer target)) -> target
            | other -> failwith $"%s{operation}: expected a function pointer, got %O{other}"

        let method = FunctionPointerTarget.requireManaged operation methodPtr

        let state, invoke =
            MulticastDelegateStub.invokeMethodOf
                loggerFactory
                baseClassTypes
                operation
                (ManagedHeap.getObjectConcreteType constructing state.ManagedHeap)
                state

        let methodArgCount = MethodInfo.arity method + (if method.IsStatic then 0 else 1)

        let invokeArgCount = MethodInfo.arity invoke

        if methodArgCount = invokeArgCount then
            // `Delegate_Construct`'s rule is `openAux`'s with statics excluded
            // (`!pMeth->IsStatic() && pMeth->IsVirtual()`, comdelegate.cpp:1736). A static
            // virtual reaches here only through `constrained. ldftn`, which has already resolved
            // it to an implementation, so its body is the one to call.
            let state, aux =
                if method.IsStatic then
                    state, OpenDelegateAux.Aux (FunctionPointerTarget.Managed method)
                else
                    let declaringType =
                        AllConcreteTypes.findExistingConcreteType
                            state.ConcreteTypes
                            method.RequiredDeclaringType.Identity
                            method.DeclaringTypeGenerics
                        |> Option.defaultWith (fun () ->
                            failwith
                                $"%s{operation}: declaring type %s{MethodOwner.describe method.Owner} is not registered in AllConcreteTypes"
                        )

                    openAux loggerFactory baseClassTypes operation method declaringType state

            match aux with
            | OpenDelegateAux.Aux aux -> write baseClassTypes constructing (DelegateBinding.Open aux) state |> Ok
            | OpenDelegateAux.GenericVirtualUnsupported -> Error (baseClassTypes.NotSupportedException, None)
        elif methodArgCount = invokeArgCount + 1 then
            if not method.IsStatic && target.IsNone then
                Error (baseClassTypes.ArgumentException, Some "Delegate to an instance method cannot have null 'this'.")
            else
                write baseClassTypes constructing (DelegateBinding.Closed (target, methodPtr)) state
                |> Ok
        else
            failwith
                $"%s{operation}: %s{method.Name} takes %d{methodArgCount} argument(s) but the delegate's Invoke takes %d{invokeArgCount}; the target must take the same number or one more"
