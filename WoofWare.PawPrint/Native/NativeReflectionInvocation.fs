namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

/// CoreCLR's `reflectioninvocation.cpp` QCalls, other than the `RuntimeTypeHandle_*` family that
/// `NativeRuntimeTypeQCall` serves. Today that is `RuntimeMethodHandle_InvokeMethod`, the primitive
/// underneath every `MethodBase.Invoke`, and `ReflectionInvocation_GetBoxInfo`, which describes to
/// managed code how to box a value of a given type.
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

    /// Read `Signature._pMethod` and return the method-registry id it names.
    ///
    /// This is the one and only read of mutable heap state that decides *which* method the
    /// invocation binds, and it happens exactly once, on first entry, before anything that could run
    /// guest code. CoreCLR likewise reads `pMeth` once, at QCall entry
    /// (reflectioninvocation.cpp:337), and never again.
    ///
    /// The *id* is what gets snapshotted onto the eval stack rather than the resolved method,
    /// because a registry id is a plain int64 that a `NativeIntSource.MethodHandlePtr` can carry
    /// there, and `MethodHandleRegistry` is append-only — so resolving the same id later cannot
    /// yield a different method, however the guest may since have rewritten `_pMethod`.
    let private readMethodHandleId (ctx : NativeCallContext) (operation : string) (state : IlMachineState) : int64 =
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

        AllocatedNonArrayObject.DereferenceFieldById pMethodField signatureObj
        |> NativeCall.methodHandleIdOfRuntimeMethodHandleInternal operation
        |> Option.defaultWith (fun () ->
            failwith $"%s{operation}: null RuntimeMethodHandleInternal in Signature._pMethod"
        )

    /// Concretize the method a snapshotted method-registry id names.
    ///
    /// The id comes from the Signature's `_pMethod`, i.e. from the *identity* CoreCLR's
    /// `pSig->GetMethod()` also uses. The method's parameter and return types then come from
    /// PawPrint's own parsed signature rather than from a second read of the `_arguments` /
    /// `_returnTypeORfieldType` reflection objects: CoreCLR reads those (`SignatureNative`
    /// accessors, runtimehandles.h:345), but under PawPrint they are a *view* that `Signature_Init`
    /// derived from this same `MethodInfo.Signature`, and binding a call against a view rather than
    /// an identity is what lets the two drift apart silently.
    ///
    /// Taking the id as a parameter rather than reading `_pMethod` itself is what makes this safe to
    /// call more than once: every call in a given invocation resolves the same id.
    let private resolveTarget
        (ctx : NativeCallContext)
        (operation : string)
        (methodHandleId : int64)
        (state : IlMachineState)
        : IlMachineState * InvokeTarget
        =
        let identity =
            match MethodHandleRegistry.resolveMethodFromId methodHandleId state.MethodHandles with
            | Some (MethodHandle.FromMetadata identity) -> identity
            | Some (MethodHandle.FromDynamic dynamicHandle) ->
                // Legal in CoreCLR: invoking a `DynamicMethod` runs the IL its `DynamicResolver`
                // hands back. PawPrint mints the method in `ModuleHandle_GetDynamicMethod` but has
                // no interpretable body for it yet, so there is nothing to invoke.
                failwith
                    $"TODO: %s{operation} was asked to invoke %O{dynamicHandle}, a Reflection.Emit method; PawPrint cannot yet execute a method with no metadata body"
            | None ->
                failwith $"%s{operation}: method-registry id %d{methodHandleId} did not resolve to a known MethodHandle"

        let methodInfo =
            NativeRuntimeMethodHandle.methodInfoOfMetadataIdentity operation state identity

        let declaringTypeHandle =
            NativeRuntimeMethodHandle.requireClosedDeclaringType operation identity

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
    /// `sourcesPure/ReflectionInvokeMethodMultipleArguments.cs` covers both buffer shapes.
    let private argumentByrefSlot
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (buffer : ManagedPointerSource)
        (index : int)
        : ManagedPointerSource
        =
        // The stride *establishes* the byte view rather than assuming one: the four-argument
        // buffer arrives as the bare address of the struct local (`p + 0` is `p`,
        // BinaryArithmetic.fs:347), so there is no cursor there to advance. Anchoring one is
        // exactly what the guest's own `(IntPtr*)&byrefs + i` does for `i > 0`, and appending
        // to an existing cursor — which the `stackalloc` buffer does carry — accumulates rather
        // than restarts, so one call serves both.
        if index = 0 then
            buffer
        else

        let byteType =
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes baseClassTypes.Byte.Identity
            |> Option.bind (fun handle -> AllConcreteTypes.lookup handle state.ConcreteTypes)
            |> Option.defaultWith (fun () ->
                failwith "argumentByrefSlot: System.Byte is not concretized, so no byte cursor can be built"
            )

        ManagedPointerByteView.addByteOffset state byteType (index * NativeRuntimeTypeHelpers.nativeIntSize) buffer

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
        (byReferenceZero : CliType)
        (buffer : ManagedPointerSource)
        (index : int)
        (parameterType : ConcreteTypeHandle)
        : CliType
        =
        let slot = argumentByrefSlot ctx.BaseClassTypes state buffer index

        // Read the slot *as a `ByReference`*, which is what the caller stored there, rather than as
        // whatever the pointer happens to describe. Neither spelling of the slot describes it:
        // index 0 is the bare address of a four-element `StackAllocatedByRefs` local, whose whole
        // value is thirty-two bytes, and index 1 and up is that address plus a `System.Byte`
        // cursor, whose view is one byte. The width belongs to the buffer's element type, which is
        // something this boundary knows and the pointer does not.
        let byref =
            IlMachineState.readManagedByrefAs ctx.BaseClassTypes state byReferenceZero slot
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

    /// True iff `callMethodWithCommitment` would service this method *inline* through
    /// `Intrinsics.call` / its `Activator.CreateInstance<T>` special case, rather than by pushing an
    /// IL frame for it.
    ///
    /// This mirrors the `isIntrinsic && not (Intrinsics.isSafeIntrinsic intrinsicKey)` condition at
    /// `IlMachineStateExecution.fs:1659`, specialised to our situation: we have no separate call
    /// site, so the method-level and type-level `[Intrinsic]` checks — which that code deliberately
    /// keys on the post-resolution method and on the call site's static type respectively — both
    /// look at the one method we are about to invoke. The two can only disagree if virtual
    /// resolution changes the method underneath us, which reflection cannot reach today (see
    /// `sourcesPure/ReflectionInvokeVirtualMethod.cs`); and if they ever did, the result would be
    /// the same loud crash this predicate exists to pre-empt, not a wrong answer.
    let private isInlineServicedIntrinsic
        (state : IlMachineState)
        (method : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : bool
        =
        let declaringAssembly =
            state.LoadedAssembly method.DeclaringAssembly
            |> Option.defaultWith (fun () ->
                failwith
                    $"RuntimeMethodHandle.InvokeMethod: declaring assembly for %O{method} is not loaded: %O{method.DeclaringAssembly}"
            )

        let getMemberRefParentType (handle : MemberReferenceHandle) : TypeRef =
            match declaringAssembly.Members.[handle].Parent with
            | MetadataToken.TypeReference r -> declaringAssembly.TypeRefs.[r]
            | other -> failwith $"RuntimeMethodHandle.InvokeMethod: unexpected MemberReference parent %O{other}"

        let declaringType =
            declaringAssembly.TypeDefs.[method.RequiredDeclaringType.Definition.Get]

        // `[Intrinsic]` on an abstract body is a call-site inlining hint with no IL behind it, and
        // is not serviced inline; the same suppression as `isAbstractBody` / `callSiteBodyIsAbstract`.
        let isAbstractBody =
            match method.Body with
            | MethodBody.Abstract -> true
            | _ -> false

        let isIntrinsic =
            not isAbstractBody
            && (MethodInfo.isJITIntrinsic getMemberRefParentType declaringAssembly.Methods method
                || MethodInfo.hasIntrinsicAttribute
                    getMemberRefParentType
                    declaringAssembly.Methods
                    declaringType.Attributes)

        isIntrinsic
        && not (Intrinsics.isSafeIntrinsic (Intrinsics.methodKey state method))

    /// Reject the invocation shapes CoreCLR handles but this does not, naming the triggering
    /// condition rather than diverging quietly.
    let private rejectUnsupportedShapes
        (ctx : NativeCallContext)
        (operation : string)
        (state : IlMachineState)
        (isConstructor : bool)
        (target : InvokeTarget)
        : unit
        =
        let describe () =
            $"%s{MethodOwner.describe target.Method.Owner}::%s{target.Method.Name}"

        if
            target.Method.Signature.Header.Get.CallingConvention = System.Reflection.Metadata.SignatureCallingConvention.VarArgs
        then
            failwith $"TODO: %s{operation} on the vararg method %s{describe ()}"

        if isInlineServicedIntrinsic state target.Method then
            // An intrinsic serviced inside `callMethodWithCommitment` never pushes a frame: it
            // computes its result and then advances the *caller's* program counter, because for an
            // ordinary `call` opcode the caller is the frame whose instruction has now finished.
            // Here the caller is this native QCall frame, which has no IL body, so that advance
            // aborts the interpreter. It also reports `CallCommitment.Committed`, so the commitment
            // check after the call cannot catch it either. Honouring
            // `advanceProgramCounterOfCaller = false` on that path would mean touching all ~70
            // advance sites across `Intrinsics.fs` and `IntrinsicHelpers.fs`, which is its own
            // change; reject here instead.
            failwith
                $"TODO: %s{operation} on %s{describe ()}, which PawPrint services as a JIT intrinsic; the intrinsic dispatcher advances the caller's program counter, and this QCall's frame has no IL to advance"

        match target.Method.Body with
        | MethodBody.RuntimeProvided RuntimeBehaviour.DelegateCtor ->
            // On CoreCLR a delegate constructor is not runtime-provided at all: the compiler-emitted
            // ctor calls managed `Delegate.DelegateConstruct` (Delegate.CoreCLR.cs:469), whose own
            // comment says "via reflection you can pass in just about any value for the method", and
            // which therefore screens `IntPtr.Zero` into an `ArgumentNullException` before QCalling
            // `Delegate_Construct` — which in turn decides open versus closed binding by comparing
            // the target method's argument count against `Invoke`'s, builds a shuffle thunk for the
            // open case, and throws `Arg_DlgtNullInst` for a null instance on a closed one
            // (comdelegate.cpp:1712-1756). PawPrint models the ctor as `RuntimeBehaviour.DelegateCtor`
            // and writes `_target`/`_methodPtr` verbatim, which is exact for a `newobj` whose
            // arguments the compiler produced with `ldftn`, and wrong for arguments a guest chose.
            // Measured: `ConstructorInfo.Invoke(new object[] { null, IntPtr.Zero })` on a delegate
            // type gives `TargetInvocationException(ArgumentNullException)` on .NET 10 and a
            // successfully constructed delegate here.
            failwith
                $"TODO: %s{operation} on %s{describe ()}, a delegate constructor; PawPrint services it by writing the target and method pointer straight into the instance, which skips the validation CoreCLR's managed Delegate.DelegateConstruct and its Delegate_Construct QCall perform on arguments reflection let the guest choose"
        | _ -> ()

        if isConstructor then
            // `MethodBaseInvoker` passes `isConstructor: obj is null`
            // (`MethodBaseInvoker.CoreCLR.cs`, `InterpretedInvoke_Constructor`), so the flag means
            // "allocate the instance yourself", not "the target is a constructor". Managed code
            // never asks for an allocation on behalf of a `.cctor`: the allocating overload of
            // `ConstructorInfo.Invoke` throws `MemberAccessException` through `ThrowNoInvokeException`
            // (RuntimeConstructorInfo.cs:88-92, reached via the `NoConstructorInvoke` flag it sets at
            // :29), and the overload taking an instance diverts to `InvokeClassConstructor`
            // (:120-126). A static target under this flag is therefore a guest that bypassed the
            // managed layer.
            if target.Method.IsStatic then
                failwith
                    $"%s{operation}: isConstructor=true for the static method %s{describe ()}; the managed layer routes a class constructor to RuntimeType.InvokeClassConstructor rather than asking this QCall to allocate an instance for it"

            match NativeRuntimeTypeHelpers.nominalTypeInfoOfArgument state target.DeclaringType with
            | Some declaringTypeInfo when TypeInfo.NominallyEqual declaringTypeInfo ctx.BaseClassTypes.String ->
                // CoreCLR's `fCtorOfVariableSizedObject` (reflectioninvocation.cpp:370): a
                // MethodTable with a component size allocates itself, so the QCall allocates
                // nothing, passes no `this`, and takes the *ctor's return value* as the object
                // (:616). Arrays are the CLI's only other variable-sized shape and `resolveTarget`
                // already refuses their structural declaring type, so this is String and nothing
                // else. `executeNewobj` models it by redirecting to the same-signature static
                // `String.Ctor` sibling; wiring that in here means a different result shape from
                // every other constructor, so it is its own change.
                failwith
                    $"TODO: %s{operation} with isConstructor=true on %s{describe ()}, a constructor of System.String; a variable-sized object allocates itself, so the QCall must take the constructor's return value as the result rather than pre-allocating a `this`"
            | _ -> ()

            if NativeRuntimeTypeHelpers.argumentIsNullable ctx.BaseClassTypes state target.DeclaringType then
                // CoreCLR constructs into a *true* boxed `Nullable<T>` and then
                // `Nullable::NormalizeBox`es the result down to a boxed `T` or to null
                // (reflectioninvocation.cpp:620). PawPrint's boxing deliberately never produces a
                // box whose type is `Nullable<T>`, so there is no such buffer to construct into.
                failwith
                    $"TODO: %s{operation} with isConstructor=true on %s{describe ()}, a constructor of Nullable<T>; CoreCLR constructs into a true boxed Nullable and then normalises it to a boxed T or null, and PawPrint has no true boxed Nullable to construct into"

        // A value-type receiver the *caller* supplied needs no handling here: CoreCLR forms `this`
        // as `gc.target->UnBox()` (reflectioninvocation.cpp:502), a pointer into the payload of the
        // box the caller passed, and `callMethodWithCommitment` already converts an `ObjectRef`
        // receiver for a value-type method into exactly that — `Byref (ByrefRoot.HeapValue addr, [])`
        // (IlMachineStateExecution.fs:2074). So a mutating struct method writes through to the
        // caller's box, as it does on CoreCLR.
        //
        // Its two sibling branches do need rejecting, because both form a *different* `this`.
        // None of this applies under `isConstructor`, where the receiver is the QCall's own fresh
        // allocation rather than the caller's box; that case is guarded above.
        if
            not isConstructor
            && not target.Method.IsStatic
            && NativeRuntimeTypeHelpers.argumentIsValueType ctx.BaseClassTypes state target.DeclaringType
        then
            if NativeRuntimeTypeHelpers.argumentIsNullable ctx.BaseClassTypes state target.DeclaringType then
                // CoreCLR allocates a fresh box and `Nullable::UnBox`es the target into it, then uses
                // *that* buffer as `this` (reflectioninvocation.cpp:494-499) — so unlike every other
                // struct, a mutation is not visible through the caller's box. Unboxing in place here
                // would write somewhere CoreCLR does not.
                failwith
                    $"TODO: %s{operation} on %s{describe ()}, an instance method of Nullable<T>; CoreCLR unboxes into a freshly allocated true boxed Nullable rather than through the caller's box"

            if target.Method.IsVirtual then
                // For a virtual struct method CoreCLR may hold the *unboxing stub*, whose `this` is
                // the boxed object itself rather than its payload (reflectioninvocation.cpp:492).
                // PawPrint models no such stub, so we cannot tell which of the two rules applies, and
                // guessing would silently address the wrong storage. Unreachable today — `GetMethod`
                // on a type declaring a virtual method stops at `RuntimeMethodHandle.GetSlot` first
                // (see `sourcesPure/ReflectionInvokeVirtualMethod.cs`) — so this guard exists to keep
                // that from becoming a silent divergence when `GetSlot` lands.
                failwith
                    $"TODO: %s{operation} on %s{describe ()}, a virtual instance method of a value type; CoreCLR may hold an unboxing stub here, whose `this` is the box rather than its payload, and PawPrint does not model unboxing stubs"

        let rejectParameterShape (index : int) (parameterType : ConcreteTypeHandle) : unit =
            match parameterType with
            | ConcreteTypeHandle.Byref _ ->
                // A `ref`/`out` parameter needs the callee's writes propagated back out through the
                // caller's byref, which is `MethodBaseInvoker.CopyBack`'s half of the contract.
                failwith
                    $"TODO: %s{operation} on %s{describe ()}: parameter %d{index} is a byref, whose copy-back semantics are not modelled"
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ ->
                // `InvokerArgFlags.IsValueType` is set for a pointer parameter
                // (`MethodInvokerCommon.Initialize`), so the caller's byref addresses the payload of
                // a boxed `IntPtr` rather than an `object?` slot — reachable with a plain `null`
                // argument, which `CheckValue` converts to `IntPtr.Zero`. `argumentIsValueType` says
                // false for a structural pointer handle, so `readArgument` would take its
                // reference-type branch and misread the payload. Reject here instead: the read
                // needs a pointer-width payload path of its own, and a `System.Reflection.Pointer`
                // argument needs unwrapping besides.
                failwith
                    $"TODO: %s{operation} on %s{describe ()}: parameter %d{index} is a pointer or function pointer, whose argument buffer entry addresses a boxed IntPtr payload rather than an object slot"
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
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ ->
                // The callee leaves a native int on the eval stack, not an object reference, so the
                // reference-return branch below could not take it. CoreCLR does not hand the raw
                // value back either: `InvokeUtil::CreateObjectAfterInvoke` wraps an
                // `ELEMENT_TYPE_PTR` return in a `System.Reflection.Pointer` (which also carries the
                // pointed-to Type, so `Pointer.Unbox` and `GetPointerType` work), and boxes a
                // function pointer as an `IntPtr`. Constructing a `Pointer` is its own piece of
                // work; reject rather than invent a representation.
                failwith
                    $"TODO: %s{operation} on %s{describe ()}: a pointer return must be boxed as System.Reflection.Pointer, and a function-pointer return as IntPtr, before it leaves the QCall"
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

            // "Allocate the instance yourself", rather than "the target is a constructor":
            // `MethodBaseInvoker.CoreCLR.cs` passes `isConstructor: obj is null`, so running a
            // constructor against an instance the guest already has
            // (`RuntimeConstructorInfo.Invoke(obj, ...)`) arrives here as `false` and is served by
            // the ordinary instance-method path below.
            let isConstructor =
                NativeCall.int32Argument operation instruction.Arguments.[3] <> 0

            let resultPtr =
                NativeCall.objectHandleOnStackTarget operation state "result" instruction.Arguments.[4]

            // The tail shared by the two pre-call phases: initialise the declaring type, and
            // once it is initialised, issue the call. Reached directly on first entry, and
            // again on each re-entry while a `.cctor` this invocation triggered is still
            // running.
            let issueCall (target : InvokeTarget) (state : IlMachineState) : NativeHandlerResult option =
                // CoreCLR's `pMeth->EnsureActive()`. This can suspend to run a `.cctor`, which is
                // guest code: the `sourcesPure/ReflectionInvokeMethod.cs` class-init case really does
                // take that path. Re-entry therefore arrives with the snapshot marker on the stack
                // and resolves from the id it carries, so nothing the `.cctor` does can change which
                // method this goes on to invoke.
                //
                // Under `isConstructor` CoreCLR gets its class initialisation from somewhere else —
                // the JIT puts it in the instance constructor's prologue, which is why the QCall
                // does not ask for one (RuntimeConstructorInfo.cs:140). Running it here instead
                // makes it happen one frame earlier, and the difference does not reach the guest:
                // measured on .NET 10, reflectively constructing a type whose `.cctor` throws gives
                // a `TargetInvocationException` wrapping a `TypeInitializationException` wrapping the
                // original on both runtimes, and neither `TypeInitializationException`'s stack trace
                // names the instance constructor. `sourcesPure/ReflectionInvokeConstructor.cs` pins
                // that shape.
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
                | WhatWeDid.VoluntaryYield _ ->
                    failwith $"logic error: %s{operation}: ensureTypeInitialised cannot produce a VoluntaryYield"
                | WhatWeDid.Executed ->

                // CoreCLR's `gc.retVal = pMT->Allocate()` (reflectioninvocation.cpp:373).
                // `allocateUninitialisedInstance` is that allocation, including for a value type,
                // where it produces the boxed representation `box` itself writes — which is what
                // lets the same address serve both as the receiver and as the QCall's result.
                //
                // Deliberately *after* `ensureTypeInitialised` rather than before it as CoreCLR
                // allocates: a `.cctor` suspension re-enters this handler, and allocating first
                // would allocate a fresh instance on every re-entry. Nothing can observe the
                // difference, because an object allocated before a `.cctor` that then throws is
                // unreachable on either runtime.
                let constructedInstance, state =
                    if isConstructor then
                        let addr, state =
                            IlMachineState.allocateUninitialisedInstance
                                ctx.LoggerFactory
                                ctx.BaseClassTypes
                                target.DeclaringType
                                state

                        Some addr, state
                    else
                        None, state

                let thisValue =
                    match constructedInstance with
                    | Some addr ->
                        // Both of CoreCLR's constructor receiver branches
                        // (reflectioninvocation.cpp:466-475) come out of this one push:
                        // `callMethodWithCommitment`'s `NotConstructing` receiver path converts an
                        // `ObjectRef` into `Byref (ByrefRoot.HeapValue addr, [])` exactly when the
                        // declaring type is a value type (IlMachineStateExecution.fs:2074), which
                        // is `gc.retVal->GetData()`, and leaves it alone otherwise.
                        Some (CliType.ObjectRef (Some addr))
                    | None ->

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
                                $"%s{operation}: null target for the instance method %s{MethodOwner.describe target.Method.Owner}::%s{target.Method.Name}; the managed layer is expected to have thrown TargetException"
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

                    // Every slot of the buffer is a `System.ByReference`, so its zero is materialised
                    // once here rather than per argument — and here rather than inside
                    // `readArgument`, because materialising it can register concrete types and that
                    // has to reach the state the call goes on to use.
                    let byReferenceZero, state =
                        match ctx.BaseClassTypes.ByReference with
                        | None ->
                            failwith
                                $"%s{operation}: this corelib declares no System.ByReference, but MethodBaseInvoker builds its argument buffer out of them"
                        | Some byReference ->
                            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes byReference
                            |> IlMachineState.cliTypeZeroOfHandle state ctx.BaseClassTypes

                    state,
                    (parameterTypes
                     |> List.mapi (fun index parameterType ->
                         readArgument ctx operation state byReferenceZero buffer index parameterType
                     ))

                // The re-entry marker. It must be pushed *first*, because `callMethod` pops exactly
                // `this` plus the arguments, leaving it as the bottom of this frame's eval stack for
                // the resumption branch below to find beneath any return value.
                //
                // Its *presence* distinguishes resumption from first entry, and its *shape* carries
                // what the QCall must answer with: under `isConstructor` the instance just
                // allocated, which is the one thing in this branch that the handler created rather
                // than derived from the QCall's arguments and so cannot be recovered later;
                // otherwise how the return value must be classified — a null reference for a void
                // return, and a handle to the return type for a value one. That is a snapshot taken
                // before the call —
                // CoreCLR reads `retTH` once, before `CallDescrWorkerWithHandler`
                // (reflectioninvocation.cpp:439), and re-deriving it on resumption would instead make
                // the classification a function of the `Signature` object as the *callee left it*.
                // Nothing else in this branch is recoverable-but-mutable, so this is the whole of
                // what has to be snapshotted.
                //
                // CoreCLR distinguishes the void case by the return TypeHandle being `System.Void`'s
                // rather than by a separate marker shape. Two shapes are used here so the resumption
                // match is exhaustive over the encoding and needs no handle lookup to interpret.
                let state =
                    // Swap the snapshot marker in place: the method-registry id it carried has done
                    // its job (nothing after this point can suspend before the call, so the identity
                    // can no longer be disturbed), and the slot is now needed for the return type.
                    // Swapping rather than pushing a second marker is what keeps "class init still
                    // pending" and "called, returned void" — both one slot deep — apart.
                    let _idMarker, state = IlMachineState.popEvalStack ctx.Thread state

                    let marker =
                        match constructedInstance with
                        | Some addr -> CliType.ObjectRef (Some addr)
                        | None ->

                        match target.Method.Signature.ReturnType with
                        | MethodReturnType.Void -> CliType.ObjectRef None
                        | MethodReturnType.Returns returnType ->
                            CliType.RuntimePointer (
                                CliRuntimePointer.TypeHandlePtr (RuntimeTypeHandleTarget.Closed returnType)
                            )

                    IlMachineState.pushToEvalStack marker ctx.Thread state

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
                        ReturnValueDisposition.PushToCaller
                        false
                        state

                match commitment with
                | IlMachineStateExecution.CallCommitment.Committed ->
                    NativeHandlerResult.pushedManagedCallee state |> Some
                | IlMachineStateExecution.CallCommitment.Raised ->
                    // Leaves this frame re-enterable with the marker pushed but no callee frame
                    // beneath it, which the resumption branch would misread as "the target
                    // returned". Reachable only for an `[Intrinsic]` target serviced inside
                    // `callMethodWithCommitment`; fail loudly rather than silently answering with
                    // the marker.
                    failwith
                        $"TODO: %s{operation} on %s{MethodOwner.describe target.Method.Owner}::%s{target.Method.Name}: the call raised instead of committing, which the re-entry protocol cannot represent"

            // The re-entry protocol. This native frame's eval stack *is* the state machine, and its
            // five shapes are told apart by depth, plus — where several shapes share a depth — by
            // the marker's `EvalStackValue` case:
            //
            //   []                                     first entry
            //   [MethodHandlePtr id]                   target snapshotted; a `.cctor` is running
            //   [NullObjectRef]                        called; the target returned void
            //   [ObjectRef addr]                       called; `addr` is the constructed instance
            //   [ret; TypeHandlePtr returnType]        called; the target returned a value
            //
            // The marker occupies one slot throughout and is swapped as the phase advances, which is
            // what keeps the one-slot shapes apart. A constructor cannot add a return value of its
            // own on top of its marker: its signature returns void, so the frame it left behind
            // pushed nothing.
            match state.ThreadState.[ctx.Thread].MethodState.EvaluationStack.Values with
            | [] ->
                // First entry: read `Signature._pMethod` exactly once, and snapshot the id it names
                // before anything that could run guest code.
                let methodHandleId = readMethodHandleId ctx operation state
                let state, target = resolveTarget ctx operation methodHandleId state
                rejectUnsupportedShapes ctx operation state isConstructor target

                let state =
                    IlMachineState.pushToEvalStack
                        (CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle methodHandleId))
                        ctx.Thread
                        state

                issueCall target state
            | [ EvalStackValue.NativeInt (NativeIntSource.MethodHandlePtr methodHandleId) ] ->
                // Re-entered while a `.cctor` this invocation triggered runs. Resolve from the
                // snapshotted id rather than re-reading `_pMethod`, which the `.cctor` may have
                // changed; `MethodHandleRegistry` is append-only, so the id still names what it did.
                let state, target = resolveTarget ctx operation methodHandleId state
                issueCall target state
            | stack ->
                // Resumption: the target has returned. `EvalStack.Values` is top-first, so a
                // non-void return sits *above* the marker.
                //
                // The `Signature` is deliberately not re-read here; everything needed came from the
                // marker the first-entry branch pushed. See there for why.
                let returned, state =
                    match stack with
                    | [ EvalStackValue.NullObjectRef ] ->
                        // Void marker: the target returned nothing, and `MethodBase.Invoke` answers
                        // null.
                        let _marker, state = IlMachineState.popEvalStack ctx.Thread state
                        CliType.ObjectRef None, state
                    | [ EvalStackValue.ObjectRef constructed ] ->
                        // Constructor marker: the instance this handler allocated, now constructed
                        // in place. CoreCLR hands back the same `gc.retVal` it allocated before the
                        // call (reflectioninvocation.cpp:687), and for a value type that object is
                        // the box.
                        let _marker, state = IlMachineState.popEvalStack ctx.Thread state
                        CliType.ObjectRef (Some constructed), state
                    | [ returnValue
                        EvalStackValue.NativeInt (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed returnType)) ] ->
                        let _returnValue, state = IlMachineState.popEvalStack ctx.Thread state
                        let _marker, state = IlMachineState.popEvalStack ctx.Thread state

                        if NativeRuntimeTypeHelpers.argumentIsValueType ctx.BaseClassTypes state returnType then
                            // `InvokeUtil::CreateObjectAfterInvoke` (reflectioninvocation.cpp:678):
                            // the QCall's contract is to hand back a boxed value.
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
                                // Re-read `_pMethod` and re-derive the target purely to name it in
                                // the message. This is the one place the snapshot is deliberately
                                // bypassed: we are aborting anyway, so a stale name cannot affect
                                // any result, and naming the method beats naming its return type.
                                let methodHandleId = readMethodHandleId ctx operation state
                                let _state, target = resolveTarget ctx operation methodHandleId state

                                failwith
                                    $"%s{operation}: expected an object reference from the reference-typed return of %s{MethodOwner.describe target.Method.Owner}::%s{target.Method.Name}, got %O{other}"
                    | _ ->
                        failwith
                            $"%s{operation}: expected a re-entry marker on the eval stack, optionally beneath one return value, got %d{stack.Length} value(s): %A{stack}"

                let state =
                    IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state resultPtr returned

                NativeHandlerResult.completed state |> Some
        | "ReflectionInvocation_GetBoxInfo",
          "System.Private.CoreLib",
          "",
          "BoxCache",
          "GetBoxInfo",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              typeHandleGenerics)
            ConcretePointer (ConcreteFunctionPointer _)
            ConcretePointer (ConcretePointer (ConcreteVoid state.ConcreteTypes))
            ConcretePointer (ConcreteInt32 state.ConcreteTypes)
            ConcretePointer (ConcreteUInt32 state.ConcreteTypes) ],
          MethodReturnType.Void when typeHandleGenerics.IsEmpty ->
            // CoreCLR: `ReflectionInvocation_GetBoxInfo`, reflectioninvocation.cpp:1909. Describes
            // how `RuntimeType.BoxCache` should box a value of this type by `calli`: an allocator
            // plus the MethodTable to hand it, where the payload starts inside the source, and how
            // many bytes of it to copy. Like its `GetActivationInfo` sibling it runs no
            // constructor and no class initialiser: CoreCLR's trailing `EnsureInstanceActive()`
            // raises the *load level* of the modules owning the type, its ancestors and its
            // generic arguments (`MethodTable_EnsureInstanceActiveHelper`, methodtable.cpp:7658),
            // and touches no static state. PawPrint models no load levels, so there is nothing
            // here to reproduce.
            //
            // Argument 0 is a `QCallTypeHandle` by value; the other four are raw out-pointers to
            // locals in the managed shim (RuntimeType.BoxCache.cs:116-124).
            let operation = "ReflectionInvocation.GetBoxInfo"

            if instruction.Arguments.Length <> 5 then
                failwith $"%s{operation}: expected five native arguments, got %d{instruction.Arguments.Length}"

            let target =
                NativeCall.qCallTypeHandleToRuntimeTypeHandleTarget
                    operation
                    state
                    (EvalStackValue.ofCliType instruction.Arguments.[0])

            let outAllocator =
                NativeCall.managedPointerOfPointerArgument operation "ppfnAllocator" instruction.Arguments.[1]

            let outAllocatorFirstArg =
                NativeCall.managedPointerOfPointerArgument operation "pvAllocatorFirstArg" instruction.Arguments.[2]

            let outValueOffset =
                NativeCall.managedPointerOfPointerArgument operation "pValueOffset" instruction.Arguments.[3]

            let outValueSize =
                NativeCall.managedPointerOfPointerArgument operation "pValueSize" instruction.Arguments.[4]

            let state, info = BoxInfo.classify ctx.BaseClassTypes operation target state

            match info with
            | BoxInfo.Rejected rejection ->
                // Nothing is written on the throwing path, matching CoreCLR: `BEGIN_QCALL`
                // unwinds past every assignment, so the shim's locals keep the `default` the
                // managed wrapper gave them.
                let exnType =
                    match rejection with
                    | BoxRejection.Void -> ctx.BaseClassTypes.ArgumentException
                    | BoxRejection.ByRefLike -> ctx.BaseClassTypes.NotSupportedException

                NativeHandlerResult.raiseException exnType state |> Some
            | BoxInfo.Describes description ->
                let write ptr value state =
                    IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state ptr value

                state
                // The same helper `RuntimeTypeHandle_GetActivationInfo` hands back, and the same
                // reason for the shape: `NativeIntSource.FunctionPointer` is what
                // `executeAllocatorCalli` recognises when the guest calls through the slot.
                |> write
                    outAllocator
                    (CliType.Numeric (
                        CliNumericType.NativeInt (
                            NativeIntSource.FunctionPointer FunctionPointerTarget.RuntimeAllocator
                        )
                    ))
                // `pvAllocatorFirstArg` addresses a `void*` slot which the guest copies into
                // `BoxCache`'s own `void*` field, so a `RuntimePointer` rather than a `NativeInt`:
                // a `NativeInt`-shaped value there would force that copy down the byte-image path,
                // which a pointer cell has no byte image for.
                |> write
                    outAllocatorFirstArg
                    (CliType.RuntimePointer (
                        CliRuntimePointer.MethodTablePtr (RuntimeTypeHandleTarget.Closed description.MethodTable)
                    ))
                |> write outValueOffset (CliType.Numeric (CliNumericType.Int32 description.ValueOffset))
                // PawPrint carries CLI uint32 values as Int32 while preserving the low 32 bits;
                // see PrimitiveType.UInt32 and `MethodTableProjection.uint32Field`.
                |> write outValueSize (CliType.Numeric (CliNumericType.Int32 (int32 description.ValueSize)))
                |> NativeHandlerResult.completed
                |> Some
        | _ -> None
