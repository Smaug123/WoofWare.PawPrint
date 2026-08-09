namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging

/// CoreCLR's `reflectioninvocation.cpp` QCalls that *call* a managed method, as opposed to merely
/// describing one. Today that is `RuntimeMethodHandle_InvokeMethod`, the primitive underneath
/// every `MethodBase.Invoke`.
[<RequireQualifiedAccess>]
module internal NativeReflectionInvocation =

    /// The target method of an in-flight `InvokeMethod`, recovered from the `Signature` object the
    /// QCall was handed. Recovered identically on first entry and on re-entry: everything here is
    /// a function of the QCall's own arguments, so nothing has to be smuggled across the managed
    /// call through the eval stack.
    type private InvokeTarget =
        {
            Method : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
            /// The type that declares `Method`, as CoreCLR's `pSig->GetDeclaringType()`.
            DeclaringType : ConcreteTypeHandle
        }

    /// Decode the `Signature` object behind a `pSig` ObjectHandleOnStack, and concretize the method
    /// it describes.
    ///
    /// The method comes from the Signature's `_pMethod` (a `RuntimeMethodHandleInternal`), i.e. from
    /// the *identity* CoreCLR's `pSig->GetMethod()` also uses. Its parameter and return types then
    /// come from PawPrint's own parsed signature rather than from a second read of the `_arguments`
    /// / `_returnTypeORfieldType` reflection objects: CoreCLR reads those (`SignatureNative`
    /// accessors, runtimehandles.h:345), but under PawPrint they are a *view* that
    /// `Signature_Init` derived from this same `MethodInfo.Signature`, and binding a call against a
    /// view rather than an identity is what lets the two drift apart silently.
    ///
    /// Caveat, which CoreCLR does not share: CoreCLR snapshots the return TypeHandle before the
    /// call, whereas we re-derive it afterwards. A guest that reflectively overwrote
    /// `Signature._pMethod` from inside the target method would change our post-call classification
    /// where CoreCLR's would be fixed.
    let private resolveTarget
        (ctx : NativeCallContext)
        (operation : string)
        (state : IlMachineState)
        : IlMachineState * InvokeTarget
        =
        let sigPtr =
            NativeCall.objectHandleOnStackTarget operation state "pSig" ctx.Instruction.Arguments.[2]

        // ObjectHandleOnStack carries a managed byref to a slot holding an object reference, so
        // this needs the object-aware reader rather than the byte-view variant.
        let signatureAddr =
            match IlMachineState.readManagedByref ctx.BaseClassTypes state sigPtr with
            | CliType.ObjectRef (Some addr) -> addr
            | CliType.ObjectRef None -> failwith $"%s{operation}: pSig ObjectHandleOnStack held a null Signature"
            | other -> failwith $"%s{operation}: expected ObjectRef in pSig ObjectHandleOnStack, got %O{other}"

        let signatureObj = ManagedHeap.get signatureAddr state.ManagedHeap

        let pMethodField =
            IlMachineState.requiredOwnInstanceFieldId state signatureObj.ConcreteType "_pMethod"

        let pMethod = AllocatedNonArrayObject.DereferenceFieldById pMethodField signatureObj

        let identity =
            NativeRuntimeMethodHandle.resolveMetadataIdentityFromArg operation state pMethod

        let methodInfo =
            NativeRuntimeMethodHandle.methodInfoOfMetadataIdentity operation state identity

        let declaringTypeHandle = identity.GetDeclaringType ()

        let typeGenerics =
            match declaringTypeHandle with
            | ConcreteTypeHandle.Concrete _ ->
                match AllConcreteTypes.lookup declaringTypeHandle state.ConcreteTypes with
                | Some declaringType -> declaringType.Generics
                | None ->
                    failwith
                        $"%s{operation}: declaring type handle %O{declaringTypeHandle} was not concretized, so the target method cannot be resolved"
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ ->
                // The runtime-generated array methods (Get/Set/Address/.ctor) are the only members
                // of a structural type. CoreCLR resolves their signatures against
                // `GetClassOrArrayInstantiation`, which PawPrint does not model — it stores array
                // element types structurally in the handle rather than as a generic argument
                // vector. `Array_CreateInstance` is the supported route to those.
                failwith
                    $"TODO: %s{operation} on a method whose declaring type is the structural type %O{declaringTypeHandle}; CoreCLR resolves such a signature against GetClassOrArrayInstantiation, which PawPrint does not model"

        let methodGenerics = identity.GetMethodGenerics () |> ImmutableArray.CreateRange

        if methodInfo.Generics.Length <> methodGenerics.Length then
            failwith
                $"TODO: %s{operation} on generic method definition %s{methodInfo.Name}: it declares %d{methodInfo.Generics.Length} generic parameter(s) but the handle carries %d{methodGenerics.Length} generic argument(s); the managed reflection layer is expected to reject an uninstantiated generic method before the QCall"

        let state, concretized, _declaringTypeHandle =
            ExecutionConcretization.concretizeMethodWithAllGenerics
                ctx.LoggerFactory
                ctx.BaseClassTypes
                typeGenerics
                methodInfo
                methodGenerics
                state

        state,
        {
            Method = concretized
            DeclaringType = declaringTypeHandle
        }

    /// The byref at index `i` of the `void** args` buffer.
    ///
    /// `MethodBaseInvoker` builds this buffer in two structurally different places: as the address
    /// of a `StackAllocatedByRefs` struct local for up to four arguments
    /// (`InvokeDirectByRefWithFewArgs`, MethodBaseInvoker.cs:162), and as an offset into a
    /// `stackalloc IntPtr[3 * argCount]` block beyond that (`InvokeWithManyArgs`,
    /// MethodBaseInvoker.cs:236). Both are plain byte cursors once the guest has done
    /// `(IntPtr*)&byrefs + i`, so stride the cursor by pointer-widths and let the byref model
    /// resolve what that lands on — the same rule `sourcesPure/StructLocalPointerArithmetic.cs`
    /// pins for `(T*)&local + i`.
    ///
    /// Only `index = 0` is reachable today, and the stride below is therefore unexercised: a target
    /// taking two or more arguments never reaches this QCall, because writing `args[1]` into the
    /// buffer fails first in the guest (a byte-view write over pointer-containing struct storage).
    /// `sourcesPure/ReflectionInvokeMethodMultipleArguments.cs` is parked on exactly that and will
    /// cover the stride when it un-parks.
    let private argumentByrefSlot
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (buffer : ManagedPointerSource)
        (index : int)
        : ManagedPointerSource
        =
        if index = 0 then
            buffer
        else
            ManagedPointerByteView.addByteOffsetToByteView
                baseClassTypes
                state
                (index * NativeRuntimeTypeHelpers.nativeIntSize)
                buffer

    /// Read the value the caller placed behind `args[index]`, as the *signature's* parameter type.
    ///
    /// `args[i]` is a `ByReference`, whose sole field is a `ref byte`: the caller type-erases every
    /// argument through `Unsafe.As<T, byte>` on the way in (`ByReference.Create<T>`), so the byref
    /// that arrives here always carries a trailing `ReinterpretAs System.Byte`. Re-imposing a type
    /// on it is therefore not an optimisation but the whole operation, and it is exactly what
    /// CoreCLR does: `InvokeUtil::CopyArg(th, args[i], &argDest)` takes the width and shape from
    /// `th`, the signature's argument TypeHandle, never from the pointee.
    ///
    /// The two kinds of byref the caller can have built are distinguished by that same type
    /// (`_invokerArgFlags[i] & InvokerArgFlags.IsValueType`, MethodBaseInvoker.cs:167): a value-type
    /// parameter's byref addresses the *payload* of a box, and a reference-type parameter's byref
    /// addresses an `object?` slot. `CopyArg` splits on it too, and copies a reference-type argument
    /// as a bare `OBJECTREF` without regard to its exact type — so `System.Object` is the honest
    /// view to re-impose there, and it is also the only one available, since an array-typed
    /// parameter has no nominal `ConcreteType` to name as a reinterpret target.
    let private readArgument
        (ctx : NativeCallContext)
        (operation : string)
        (state : IlMachineState)
        (buffer : ManagedPointerSource)
        (index : int)
        (parameterType : ConcreteTypeHandle)
        : CliType
        =
        let slot = argumentByrefSlot ctx.BaseClassTypes state buffer index

        let byref =
            IlMachineState.readManagedByref ctx.BaseClassTypes state slot
            |> NativeCall.managedPointerOfPointerArgument operation $"args[%d{index}]"

        if byref = ManagedPointerSource.Null then
            failwith
                $"%s{operation}: args[%d{index}] was a null byref; the managed argument-marshalling layer is expected to have materialised every argument before the QCall"

        let viewType =
            if NativeRuntimeTypeHelpers.argumentIsValueType ctx.BaseClassTypes state parameterType then
                parameterType
            else
                AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes ctx.BaseClassTypes.Object

        let viewConcreteType =
            AllConcreteTypes.lookup viewType state.ConcreteTypes
            |> Option.defaultWith (fun () ->
                failwith
                    $"%s{operation}: view type %O{viewType} for args[%d{index}] is not registered in AllConcreteTypes"
            )

        ManagedPointerSource.reinterpretAs viewConcreteType byref
        |> IlMachineState.readManagedByref ctx.BaseClassTypes state

    /// Reject the invocation shapes CoreCLR handles but this does not, naming the triggering
    /// condition rather than diverging quietly.
    let private rejectUnsupportedShapes
        (ctx : NativeCallContext)
        (operation : string)
        (state : IlMachineState)
        (target : InvokeTarget)
        : unit
        =
        let describe () =
            $"%s{target.Method.DeclaringType.Namespace}.%s{target.Method.DeclaringType.Name}::%s{target.Method.Name}"

        if
            target.Method.Signature.Header.Get.CallingConvention = System.Reflection.Metadata.SignatureCallingConvention.VarArgs
        then
            failwith $"TODO: %s{operation} on the vararg method %s{describe ()}"

        if
            not target.Method.IsStatic
            && NativeRuntimeTypeHelpers.argumentIsValueType ctx.BaseClassTypes state target.DeclaringType
        then
            // CoreCLR unboxes the target into a `this` pointer here, with a distinct branch for an
            // unboxing stub and another that re-boxes a `Nullable<T>` receiver
            // (reflectioninvocation.cpp:492-494). Neither is modelled, and passing the box itself
            // as the receiver would mutate the wrong storage.
            failwith
                $"TODO: %s{operation} on %s{describe ()}, an instance method of a value type; CoreCLR unboxes the target to form `this`"

        let rejectParameterShape (index : int) (parameterType : ConcreteTypeHandle) : unit =
            match parameterType with
            | ConcreteTypeHandle.Byref _ ->
                // A `ref`/`out` parameter needs the callee's writes propagated back out through the
                // caller's byref, which is `MethodBaseInvoker.CopyBack`'s half of the contract.
                failwith
                    $"TODO: %s{operation} on %s{describe ()}: parameter %d{index} is a byref, whose copy-back semantics are not modelled"
            | _ ->

            if NativeRuntimeTypeHelpers.argumentIsNullable ctx.BaseClassTypes state parameterType then
                // `CheckValue` converts the incoming object to a *true* boxed `Nullable<T>` before
                // the byref is formed, so unmarshalling one means reading a `Nullable<T>` out of a
                // box that is itself a `Nullable<T>` — the one shape PawPrint's boxing deliberately
                // never produces.
                failwith
                    $"TODO: %s{operation} on %s{describe ()}: parameter %d{index} is a Nullable<T>, which arrives as a true boxed Nullable rather than as a boxed T"

        target.Method.Signature.ParameterTypes |> List.iteri rejectParameterShape

        match target.Method.Signature.ReturnType with
        | MethodReturnType.Void -> ()
        | MethodReturnType.Returns returnType ->
            match returnType with
            | ConcreteTypeHandle.Byref _ ->
                // CoreCLR dereferences a byref return and boxes the target
                // (reflectioninvocation.cpp:657-669), throwing NullReferenceException for a null
                // one. PawPrint's call path returns the byref itself.
                failwith
                    $"TODO: %s{operation} on %s{describe ()}: a byref return must be dereferenced and boxed before it leaves the QCall"
            | _ ->

            if NativeRuntimeTypeHelpers.argumentIsNullable ctx.BaseClassTypes state returnType then
                // `Nullable::NormalizeBox` boxes a `Nullable<T>` return as a `T`, or as null.
                failwith
                    $"TODO: %s{operation} on %s{describe ()}: a Nullable<T> return must be normalised by Nullable::NormalizeBox before it leaves the QCall"

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "RuntimeMethodHandle_InvokeMethod",
          "System.Private.CoreLib",
          "System",
          "RuntimeMethodHandle",
          "InvokeMethod",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              targetGenerics)
            ConcretePointer (ConcretePointer (ConcreteVoid state.ConcreteTypes))
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              sigGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "", "BOOL", boolGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              resultGenerics) ],
          MethodReturnType.Void when
            targetGenerics.IsEmpty
            && sigGenerics.IsEmpty
            && boolGenerics.IsEmpty
            && resultGenerics.IsEmpty
            ->
            // CoreCLR's `RuntimeMethodHandle_InvokeMethod` (reflectioninvocation.cpp:311): unmarshal
            // the caller's array of byrefs against the Signature, call the target, and hand the
            // result back boxed through the `result` ObjectHandleOnStack. This is the primitive
            // under `MethodBase.Invoke`; the argument coercion, the target-type check and the
            // `TargetInvocationException` wrapping all live in managed code above it, and PawPrint
            // interprets that managed code like any other.
            let operation = "RuntimeMethodHandle.InvokeMethod"

            if instruction.Arguments.Length <> 5 then
                failwith $"%s{operation}: expected five native arguments, got %d{instruction.Arguments.Length}"

            let isConstructor =
                NativeCall.int32Argument operation instruction.Arguments.[3] <> 0

            if isConstructor then
                // The constructor path allocates the instance itself (with separate branches for
                // array constructors and for variable-sized objects like String) and then calls the
                // ctor against it. `Activator.CreateInstance` reaches PawPrint through
                // `RuntimeTypeHandle_GetActivationInfo` instead, so nothing needs this yet.
                failwith
                    $"TODO: %s{operation} with isConstructor=true; ConstructorInfo.Invoke allocates its own instance and is not implemented"

            let resultPtr =
                NativeCall.objectHandleOnStackTarget operation state "result" instruction.Arguments.[4]

            match state.ThreadState.[ctx.Thread].MethodState.EvaluationStack.Values with
            | [] ->
                // First entry.
                let state, target = resolveTarget ctx operation state
                rejectUnsupportedShapes ctx operation state target

                // CoreCLR's `pMeth->EnsureActive()`. Suspending here is safe to be re-entered
                // through: the eval stack is still empty, so the handler re-runs from the top, and
                // everything above this point is idempotent (concretization memoises into
                // append-only registries rather than being free of state change).
                let state, typeInit =
                    IlMachineStateExecution.ensureTypeInitialised
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        ctx.Thread
                        target.DeclaringType
                        state

                match typeInit with
                | WhatWeDid.SuspendedForClassInit -> NativeHandlerResult.suspendedForClassInit state |> Some
                | WhatWeDid.BlockedOnClassInit blockedBy ->
                    NativeHandlerResult.blockedOnClassInit blockedBy state |> Some
                | WhatWeDid.ThrowingTypeInitializationException ->
                    NativeHandlerResult.throwingTypeInitializationException state |> Some
                | WhatWeDid.SuspendedForManagedCall ->
                    failwith
                        $"logic error: %s{operation}: ensureTypeInitialised cannot suspend for an arbitrary managed call"
                | WhatWeDid.VoluntaryYield ->
                    failwith $"logic error: %s{operation}: ensureTypeInitialised cannot produce a VoluntaryYield"
                | WhatWeDid.Executed ->

                let thisValue =
                    if target.Method.IsStatic then
                        None
                    else
                        let targetPtr =
                            NativeCall.objectHandleOnStackTarget operation state "target" instruction.Arguments.[0]

                        match IlMachineState.readManagedByref ctx.BaseClassTypes state targetPtr with
                        | CliType.ObjectRef (Some addr) -> Some (CliType.ObjectRef (Some addr))
                        | CliType.ObjectRef None ->
                            // `MethodBaseInvoker.ValidateInvokeTarget` throws TargetException for a
                            // null target on an instance method before we are reached, so this is a
                            // guest that bypassed it rather than an ordinary null.
                            failwith
                                $"%s{operation}: null target for the instance method %s{target.Method.DeclaringType.Namespace}.%s{target.Method.DeclaringType.Name}::%s{target.Method.Name}; the managed layer is expected to have thrown TargetException"
                        | other ->
                            failwith $"%s{operation}: expected ObjectRef in target ObjectHandleOnStack, got %O{other}"

                let parameterTypes = target.Method.Signature.ParameterTypes

                let state, arguments =
                    if List.isEmpty parameterTypes then
                        // `InvokeWithNoArgs` passes `refArguments: null`, so there is no buffer to
                        // read: a nullary method must not touch the pointer at all.
                        state, []
                    else

                    let buffer =
                        NativeCall.managedPointerOfPointerArgument operation "args" instruction.Arguments.[1]

                    if buffer = ManagedPointerSource.Null then
                        failwith
                            $"%s{operation}: args buffer was null for a method taking %d{List.length parameterTypes} argument(s)"

                    state,
                    (parameterTypes
                     |> List.mapi (fun index parameterType ->
                         readArgument ctx operation state buffer index parameterType
                     ))

                // The re-entry marker. Its value is never read — only its presence distinguishes
                // resumption from first entry — but it must be pushed *first*, because `callMethod`
                // pops exactly `this` plus the arguments, leaving it as the bottom of this frame's
                // eval stack for the resumption branch above to find beneath any return value.
                let state = IlMachineState.pushToEvalStack (CliType.ObjectRef None) ctx.Thread state

                let state =
                    match thisValue with
                    | None -> state
                    | Some v -> IlMachineState.pushToEvalStack v ctx.Thread state

                let state =
                    (state, arguments)
                    ||> List.fold (fun state arg -> IlMachineState.pushToEvalStack arg ctx.Thread state)

                let threadState = state.ThreadState.[ctx.Thread]

                // performInterfaceResolution = true: CoreCLR takes
                // `GetSingleCallableAddrOfVirtualizedCode` for a vtable method
                // (reflectioninvocation.cpp:419), so invoking a virtual method through a base
                // class's MethodInfo runs the derived override.
                // advanceProgramCounterOfCaller = false: the native QCall frame has no IL.
                // wrapExceptionInTargetInvocation = false: `MethodBaseInvoker` wraps in managed
                // code (MethodBaseInvoker.cs:176), so wrapping here too would nest two
                // TargetInvocationExceptions.
                let state, commitment =
                    IlMachineStateExecution.callMethodWithCommitment
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        None
                        ConstructionState.NotConstructing
                        true
                        false
                        false
                        target.Method.Generics
                        target.Method
                        ctx.Thread
                        threadState
                        None
                        ConstructedObjectDisposition.PushToCaller
                        false
                        state

                match commitment with
                | IlMachineStateExecution.CallCommitment.Committed ->
                    NativeHandlerResult.pushedManagedCallee state |> Some
                | IlMachineStateExecution.CallCommitment.SuspendedForClassInit
                | IlMachineStateExecution.CallCommitment.Raised ->
                    // Both arms leave this frame re-enterable with the marker pushed but no callee
                    // frame beneath it, which the resumption branch would misread as "the target
                    // returned". They are reachable only for an `[Intrinsic]` target serviced
                    // inside `callMethodWithCommitment` (IlMachineStateExecution.fs:1909, :1961);
                    // fail loudly rather than silently answering with the marker.
                    failwith
                        $"TODO: %s{operation} on %s{target.Method.DeclaringType.Namespace}.%s{target.Method.DeclaringType.Name}::%s{target.Method.Name}: the call did not commit (%O{commitment}), which the re-entry protocol cannot represent"
            | stack ->
                // Resumption: the target has returned. `EvalStack.Values` is top-first, so a
                // non-void return sits *above* the marker.
                let state, target = resolveTarget ctx operation state

                let returned, state =
                    match target.Method.Signature.ReturnType with
                    | MethodReturnType.Void ->
                        match stack with
                        | [ _marker ] -> ()
                        | _ ->
                            failwith
                                $"%s{operation}: expected only the re-entry marker on the eval stack after a void-returning target, got %d{stack.Length} value(s): %A{stack}"

                        let _marker, state = IlMachineState.popEvalStack ctx.Thread state
                        CliType.ObjectRef None, state
                    | MethodReturnType.Returns returnType ->

                    match stack with
                    | [ _returnValue ; _marker ] -> ()
                    | _ ->
                        failwith
                            $"%s{operation}: expected a return value above the re-entry marker on the eval stack, got %d{stack.Length} value(s): %A{stack}"

                    let returnValue, state = IlMachineState.popEvalStack ctx.Thread state
                    let _marker, state = IlMachineState.popEvalStack ctx.Thread state

                    if NativeRuntimeTypeHelpers.argumentIsValueType ctx.BaseClassTypes state returnType then
                        // `InvokeUtil::CreateObjectAfterInvoke` (reflectioninvocation.cpp:678): the
                        // QCall's contract is to hand back a boxed value.
                        let addr, state =
                            UnaryMetadataObjectOps.boxValueType
                                ctx.LoggerFactory
                                ctx.BaseClassTypes
                                returnType
                                returnValue
                                state

                        CliType.ObjectRef (Some addr), state
                    else
                        match returnValue with
                        | EvalStackValue.ObjectRef addr -> CliType.ObjectRef (Some addr), state
                        | EvalStackValue.NullObjectRef -> CliType.ObjectRef None, state
                        | other ->
                            failwith
                                $"%s{operation}: expected an object reference from the reference-typed return of %s{target.Method.DeclaringType.Namespace}.%s{target.Method.DeclaringType.Name}::%s{target.Method.Name}, got %O{other}"

                let state =
                    IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state resultPtr returned

                NativeHandlerResult.completed state |> Some
        | _ -> None
