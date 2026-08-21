namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging

open NativeRuntimeTypeHelpers

[<RequireQualifiedAccess>]
module NativeRuntimeTypeQCall =
    let tryExecute (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
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
        | "RuntimeTypeHandle_ConstructName",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "ConstructName",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallTypeHandle", qCallGenerics)
            CorelibType state.ConcreteTypes ("System", "TypeNameFormatFlags", flagsGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "StringHandleOnStack",
                                             stringHandleGenerics) ],
          MethodReturnType.Void when qCallGenerics.IsEmpty && flagsGenerics.IsEmpty && stringHandleGenerics.IsEmpty ->
            let operation = "RuntimeTypeHandle.ConstructName"
            let qCallHandle = instruction.Arguments.[0] |> EvalStackValue.ofCliType

            let typeHandleTarget =
                NativeCall.qCallTypeHandleToRuntimeTypeHandleTarget operation state qCallHandle

            let flags =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 flags) -> flags
                | other -> failwith $"%s{operation}: expected TypeNameFormatFlags as Int32, got %O{other}"

            let retString =
                NativeCall.stringHandleOnStackTarget operation state "retString" instruction.Arguments.[2]

            let name = runtimeTypeHandleName operation state flags typeHandleTarget

            let nameAddr, state =
                IlMachineState.allocateManagedString ctx.LoggerFactory ctx.BaseClassTypes name state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retString
                    (CliType.ObjectRef (Some nameAddr))

            NativeHandlerResult.completed state |> Some
        | "TypeHandle_GetCorElementType",
          "System.Private.CoreLib",
          "System.Runtime.CompilerServices",
          "TypeHandle",
          _,
          [ ConcretePointer (ConcreteVoid state.ConcreteTypes) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "TypeHandle.GetCorElementType"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            let typeHandleArg = instruction.Arguments.[0] |> EvalStackValue.ofCliType

            let target =
                NativeCall.runtimeTypeHandleTargetOfEvalStackValue operation typeHandleArg

            let elementType = corElementType operation ctx.BaseClassTypes state target

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 elementType)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "TypeHandle_CanCastTo_NoCacheLookup",
          "System.Private.CoreLib",
          "System.Runtime.CompilerServices",
          "TypeHandle",
          _,
          [ ConcretePointer (ConcreteVoid state.ConcreteTypes) ; ConcretePointer (ConcreteVoid state.ConcreteTypes) ],
          returnType ->
            // The managed wrapper short-circuits identity, the "ref-type → TypeDesc" rejection,
            // and the reflection-only Nullable<T> ⇆ T rule *before* invoking this QCall. By the
            // time we get here, the only remaining job is the uncached cast walk that PawPrint's
            // existing oracle already implements for the IL `castclass`/`isinst`/`stelem.ref`
            // opcodes. We delegate to it directly.
            let operation = "TypeHandle.CanCastTo_NoCacheLookup"

            match returnType with
            | MethodReturnType.Returns (CorelibType state.ConcreteTypes ("", "BOOL", boolGenerics)) when
                boolGenerics.IsEmpty
                ->
                ()
            | other -> failwith $"%s{operation}: unexpected QCall stub return type %O{other}"

            if instruction.Arguments.Length <> 2 then
                failwith $"%s{operation}: expected two native arguments, got %d{instruction.Arguments.Length}"

            let fromTarget =
                NativeCall.runtimeTypeHandleTargetOfEvalStackValue
                    operation
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let toTarget =
                NativeCall.runtimeTypeHandleTargetOfEvalStackValue
                    operation
                    (instruction.Arguments.[1] |> EvalStackValue.ofCliType)

            let state, isAssignable =
                IlMachineState.isRuntimeTypeHandleTargetAssignableTo
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    state
                    fromTarget
                    toTarget

            // Interop.BOOL is int-backed with FALSE = 0 and TRUE = 1, so a raw Int32 is the
            // correct representation on the eval stack.
            let state =
                let ret = if isAssignable then 1 else 0
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 ret)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "MethodTable_CanCompareBitsOrUseFastGetHashCode",
          "System.Private.CoreLib",
          "System",
          "ValueType",
          _,
          [ ConcretePointer (CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                                              "MethodTable",
                                                              methodTableGenerics)) ],
          returnType when methodTableGenerics.IsEmpty ->
            let operation = "MethodTable_CanCompareBitsOrUseFastGetHashCode"

            match returnType with
            | MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean)
            | MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) -> ()
            | other -> failwith $"%s{operation}: unexpected QCall stub return type %O{other}"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            let methodTableArg = instruction.Arguments.[0] |> EvalStackValue.ofCliType
            let methodTableFor = NativeCall.methodTableOfEvalStackValue operation methodTableArg

            let state, canCompare =
                canCompareBitsOrUseFastGetHashCode ctx.LoggerFactory ctx.BaseClassTypes ctx.Thread methodTableFor state

            let state =
                let ret = if canCompare then 1 else 0

                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 ret)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "RuntimeTypeHandle_IsCollectible",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "IsCollectible",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallTypeHandle", qCallGenerics) ],
          MethodReturnType.Returns (CorelibType state.ConcreteTypes ("", "BOOL", boolGenerics)) when
            qCallGenerics.IsEmpty && boolGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.IsCollectible"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            // CoreCLR is `pTypeHandle.AsTypeHandle().GetLoaderAllocator()->IsCollectible()`
            // (runtimehandles.cpp:1094). The target is decoded rather than ignored so that a
            // malformed handle fails here rather than being reported as non-collectible; the
            // decoded value is not consulted, because with one loader allocator the answer cannot
            // depend on it. Deliberately no walk from the target to an assembly: it would have to
            // handle every structural and synthetic shape -- `int[]`, `int&`, a function pointer,
            // the dynamic-methods class -- and could fail on one, where the answer cannot.
            instruction.Arguments.[0]
            |> EvalStackValue.ofCliType
            |> NativeCall.qCallTypeHandleToRuntimeTypeHandleTarget operation state
            |> ignore<RuntimeTypeHandleTarget>

            // Interop.BOOL is int-backed with FALSE = 0 and TRUE = 1.
            let state =
                let ret =
                    if LoaderAllocator.isCollectible LoaderAllocator.Global then
                        1
                    else
                        0

                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 ret)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "RuntimeTypeHandle_Instantiate",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "Instantiate",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallTypeHandle", qCallGenerics)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "ObjectHandleOnStack",
                                             objectHandleGenerics) ],
          MethodReturnType.Void when qCallGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            let operation = "RuntimeTypeHandle.Instantiate"

            if instruction.Arguments.Length <> 4 then
                failwith $"%s{operation}: expected four native arguments, got %d{instruction.Arguments.Length}"

            let typeHandleTarget =
                NativeCall.qCallTypeHandleToRuntimeTypeHandleTarget
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let instantiationPointer =
                NativeCall.managedPointerOfPointerArgument operation "pInst" instruction.Arguments.[1]

            let genericArgumentCount =
                NativeCall.int32Argument operation instruction.Arguments.[2]

            if genericArgumentCount < 0 then
                failwith $"%s{operation}: numGenericArgs must be non-negative, got %d{genericArgumentCount}"

            let retType =
                NativeCall.objectHandleOnStackTarget operation state "type" instruction.Arguments.[3]

            let genericArguments =
                [
                    for index in 0 .. genericArgumentCount - 1 ->
                        readTypeHandleInstantiationElement ctx.BaseClassTypes operation state instantiationPointer index
                ]

            // Validate the parameter list's constraints before instantiating: the special-constraint
            // flags (NotNullableValueTypeConstraint / ReferenceTypeConstraint /
            // DefaultConstructorConstraint), the `allows ref struct` rejection, and the base-type
            // and interface requirements from the GenericParamConstraint table.
            //
            // Pointers, byrefs and `void` never reach here: the managed `RuntimeType.MakeGenericType`
            // screens them out (`SanityCheckGenericArguments` /
            // `ThrowIfTypeNeverValidGenericArgument`) before the QCall, and PawPrint interprets that
            // BCL code like any other.
            let state, constraintViolation =
                match openGenericTypeInfoForValidation state typeHandleTarget with
                | None -> state, None
                | Some typeInfo ->
                    validateConstraints ctx.LoggerFactory ctx.BaseClassTypes state typeInfo genericArguments

            match constraintViolation with
            | Some _message ->
                NativeHandlerResult.raiseException ctx.BaseClassTypes.ArgumentException state
                |> Some
            | None ->

            let instantiatedHandle, state =
                instantiateGenericRuntimeTypeTarget
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    operation
                    state
                    typeHandleTarget
                    genericArguments

            let runtimeTypeAddr, state =
                IlMachineState.getOrAllocateType
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    (RuntimeTypeHandleTarget.Closed instantiatedHandle)
                    state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retType
                    (CliType.ObjectRef (Some runtimeTypeAddr))

            NativeHandlerResult.completed state |> Some
        | "RuntimeTypeHandle_GetInstantiation",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetInstantiation",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallTypeHandle", qCallGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "ObjectHandleOnStack",
                                             objectHandleGenerics)
            _ ],
          MethodReturnType.Void when qCallGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            let operation = "RuntimeTypeHandle.GetInstantiation"
            let qCallHandle = instruction.Arguments.[0] |> EvalStackValue.ofCliType

            let typeHandleTarget =
                NativeCall.qCallTypeHandleToRuntimeTypeHandleTarget operation state qCallHandle

            let retTypes =
                NativeCall.objectHandleOnStackTarget operation state "retTypes" instruction.Arguments.[1]

            // Interop.BOOL is an int32-backed enum. TRUE selects RuntimeType[]; FALSE selects Type[].
            let asRuntimeTypeArray =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[2] with
                | CliType.Numeric (CliNumericType.Int32 i) -> i <> 0
                | other -> failwith $"%s{operation}: expected Interop.BOOL as Int32, got %O{other}"

            let genericArgumentTargets : RuntimeTypeHandleTarget list =
                match typeHandleTarget with
                // The arguments are already targets, and they are already canonical, so the
                // `Type` objects handed back are the very ones the registry holds — which is
                // what makes `typeof(Box<>).GetGenericArguments()[0]` reference-equal to the
                // argument of its own `IComparable<T>` constraint.
                | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
                    RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
                | RuntimeTypeHandleTarget.OpenConstructed (_, arguments) -> arguments
                | RuntimeTypeHandleTarget.Closed handle ->
                    match handle with
                    | ConcreteTypeHandle.Concrete _ ->
                        let concreteType =
                            AllConcreteTypes.lookup handle state.ConcreteTypes
                            |> Option.defaultWith (fun () ->
                                failwith $"%s{operation}: concrete type handle was not registered: %O{handle}"
                            )

                        concreteType.Generics |> Seq.map RuntimeTypeHandleTarget.Closed |> List.ofSeq
                    | ConcreteTypeHandle.Byref _
                    | ConcreteTypeHandle.Pointer _
                    | ConcreteTypeHandle.FunctionPointer _
                    | ConcreteTypeHandle.OneDimArrayZero _
                    | ConcreteTypeHandle.Array _ ->
                        // Real .NET strips array/byref/pointer wrappers via GetRootElementType
                        // before reaching this QCall, but be defensive: these wrappers carry
                        // no generic instantiation of their own.
                        []
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                    // Real .NET returns Type[] { typeof(T), ... } where each T is a generic
                    // type parameter. We surface each parameter as a RuntimeType backed by a
                    // GenericParameter target.
                    let assembly =
                        state.LoadedAssembly identity.AssemblyFullName
                        |> Option.defaultWith (fun () ->
                            failwith
                                $"%s{operation}: assembly for open generic type definition is not loaded: %s{identity.AssemblyFullName}"
                        )

                    let typeInfo = assembly.TypeDefs.[identity.TypeDefinition.Get]

                    if typeInfo.Generics.IsEmpty then
                        failwith
                            $"%s{operation}: open generic type definition %O{identity} declares no generic parameters"

                    List.init
                        typeInfo.Generics.Length
                        (fun position -> RuntimeTypeHandleTarget.GenericParameter (identity, position))
                | RuntimeTypeHandleTarget.GenericParameter _
                | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
                    // GetInstantiation on a generic parameter T returns Type.EmptyTypes in CoreCLR,
                    // because a parameter has no instantiation of its own.
                    []

            // `copyRuntimeTypeHandles` leaves the caller's local null when the list is empty,
            // matching native CopyRuntimeTypeHandles for 0 args; RuntimeType.GetGenericArguments
            // handles that null via `?? EmptyTypes`.
            let state =
                copyRuntimeTypeHandles
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    state
                    asRuntimeTypeArray
                    retTypes
                    genericArgumentTargets

            NativeHandlerResult.completed state |> Some
        | "RuntimeTypeHandle_GetGenericTypeDefinition",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetGenericTypeDefinition",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallTypeHandle", qCallGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "ObjectHandleOnStack",
                                             objectHandleGenerics) ],
          MethodReturnType.Void when qCallGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            let operation = "RuntimeTypeHandle.GetGenericTypeDefinition"

            if instruction.Arguments.Length <> 2 then
                failwith $"%s{operation}: expected two native arguments, got %d{instruction.Arguments.Length}"

            let typeHandleTarget =
                NativeCall.qCallTypeHandleToRuntimeTypeHandleTarget
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let retType =
                NativeCall.objectHandleOnStackTarget operation state "retType" instruction.Arguments.[1]

            // CoreCLR (runtimehandles.cpp:1122) reloads the TypeDef named by the MethodTable's
            // (module, GetCl()) pair, i.e. the uninstantiated definition. A `ConcreteType`'s
            // `Identity` is exactly that pair, so the whole operation is the projection from a
            // closed instantiation to its open definition.
            //
            // Only one target shape can legitimately arrive here. `RuntimeType
            // .GetGenericTypeDefinition` (RuntimeType.CoreCLR.cs:3557) throws
            // InvalidOperationException unless `IsGenericType`, and
            // `RuntimeTypeCache.GetGenericTypeDefinition` (RuntimeType.CoreCLR.cs:1639) returns
            // the receiver without calling the QCall at all when `IsGenericTypeDefinition`. Both
            // flags are read off the projected MethodTable, and `MethodTableProjection
            // .genericsFlags` reports GenericInst only for a nominal concrete type with a
            // non-empty instantiation. Every other shape is a BCL contract violation: fail
            // loudly rather than mint an `OpenGenericTypeDefinition` for a type with no generic
            // parameters, which would break the invariant asserted in
            // `MethodTableProjection.targetContainsGenericVariables`.
            let definitionTarget =
                match typeHandleTarget with
                // `openConstructed` has already collapsed the typical instantiation to
                // `OpenGenericTypeDefinition`, so anything still spelled `OpenConstructed` is a
                // genuine instantiation whose definition is exactly this.
                | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
                    RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
                | RuntimeTypeHandleTarget.OpenConstructed (definition, _) ->
                    RuntimeTypeHandleTarget.OpenGenericTypeDefinition definition
                | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete _ as handle) ->
                    let concreteType =
                        AllConcreteTypes.lookup handle state.ConcreteTypes
                        |> Option.defaultWith (fun () ->
                            failwith $"%s{operation}: concrete type handle was not registered: %O{handle}"
                        )

                    if concreteType.Generics.IsEmpty then
                        failwith
                            $"%s{operation}: BCL contract violation: QCall reached for non-generic target %O{typeHandleTarget}; the managed wrapper throws InvalidOperationException unless IsGenericType"

                    RuntimeTypeHandleTarget.OpenGenericTypeDefinition concreteType.Identity
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ ->
                    failwith
                        $"%s{operation}: BCL contract violation: QCall reached for open generic type definition %O{typeHandleTarget}; RuntimeTypeCache.GetGenericTypeDefinition returns the receiver unchanged when IsGenericTypeDefinition"
                | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.OneDimArrayZero _)
                | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Array _)
                | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Byref _)
                | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Pointer _)
                | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.FunctionPointer _)
                | RuntimeTypeHandleTarget.GenericParameter _
                | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
                    failwith
                        $"%s{operation}: BCL contract violation: QCall reached for target %O{typeHandleTarget}, which reports IsGenericType=false; the managed wrapper throws InvalidOperationException without invoking this QCall"

            // The type-handle registry is keyed on the whole `RuntimeTypeHandleTarget`, so this
            // is the very same `RuntimeType` object that `ldtoken Foo<>` yields — which is what
            // makes `x.GetGenericTypeDefinition() == typeof(Foo<>)` (reference equality for
            // RuntimeType operands, Type.cs:703) answer true.
            let runtimeTypeAddr, state =
                IlMachineState.getOrAllocateType ctx.LoggerFactory ctx.BaseClassTypes definitionTarget state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retType
                    (CliType.ObjectRef (Some runtimeTypeAddr))

            NativeHandlerResult.completed state |> Some
        | "RuntimeTypeHandle_GetConstraints",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetConstraints",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallTypeHandle", qCallGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "ObjectHandleOnStack",
                                             objectHandleGenerics) ],
          MethodReturnType.Void when qCallGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            let operation = "RuntimeTypeHandle.GetConstraints"
            let qCallHandle = instruction.Arguments.[0] |> EvalStackValue.ofCliType

            let typeHandleTarget =
                NativeCall.qCallTypeHandleToRuntimeTypeHandleTarget operation state qCallHandle

            let retTypes =
                NativeCall.objectHandleOnStackTarget operation state "retTypes" instruction.Arguments.[1]

            match typeHandleTarget with
            | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
                RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
            | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
                failwith
                    $"TODO: open constructed types are not handled at Native/NativeRuntimeTypeQCall.fs:%s{__LINE__}; got %O{openConstructed}"
            | RuntimeTypeHandleTarget.Closed _
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ ->
                // CoreCLR's QCall throws ArgumentException for non-generic-variable arguments,
                // but the only managed caller (RuntimeType.GetGenericParameterConstraints) gates
                // on IsGenericParameter, so we should never reach this branch in practice. Fail
                // loudly rather than silently writing Type.EmptyTypes, which would mask a bug.
                failwith $"%s{operation}: expected a generic-parameter type handle, got %O{typeHandleTarget}"
            | RuntimeTypeHandleTarget.GenericParameter _
            | RuntimeTypeHandleTarget.MethodGenericParameter _ ->

            let state, constraintTargets =
                genericParameterConstraintTargets ctx.LoggerFactory ctx.BaseClassTypes operation state typeHandleTarget

            // CopyRuntimeTypeHandles allocates Type[] (CLASS__TYPE), not RuntimeType[], and writes
            // NULL rather than a zero-length array when there are no constraints; the managed
            // wrapper launders that null through `?? Type.EmptyTypes`.
            let state =
                copyRuntimeTypeHandles ctx.LoggerFactory ctx.BaseClassTypes state false retTypes constraintTargets

            NativeHandlerResult.completed state |> Some
        | "RuntimeTypeHandle_CreateInstanceForAnotherGenericParameter",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "CreateInstanceForAnotherGenericParameter",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallTypeHandle", qCallGenerics)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "ObjectHandleOnStack",
                                             objectHandleGenerics) ],
          MethodReturnType.Void when qCallGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            let operation = "RuntimeTypeHandle.CreateInstanceForAnotherGenericParameter"

            if instruction.Arguments.Length <> 4 then
                failwith $"%s{operation}: expected four native arguments, got %d{instruction.Arguments.Length}"

            let outHandle =
                NativeCall.objectHandleOnStackTarget operation state "instantiatedObject" instruction.Arguments.[3]

            // The handler runs in two phases connected by `WhatWeDid.SuspendedForManagedCall`:
            //   1. First entry — eval stack empty: instantiate, ensure cctor, allocate, push the
            //      allocated address as a re-entry marker beneath `this`, then push `this` and
            //      hand off to the default ctor via `callMethod`. We return SuspendedForManagedCall
            //      so the dispatch loop runs the ctor before re-entering us.
            //   2. Re-entry — eval stack holds the marker: pop it and write to OutHandle. Per
            //      CoreCLR's reflectioninvocation.cpp, OutHandle is set only after the ctor
            //      returns successfully; if the ctor throws, exception dispatch unwinds past us
            //      and the caller's pre-zeroed `instantiatedObject` local stays null.
            //
            // The cctor case is independent: ensureTypeInitialised may suspend with
            // SuspendedForClassInit on the first phase; the eval stack stays empty across that
            // suspension, so when we re-enter we re-run phase 1 and ensureTypeInitialised
            // returns Executed the second time.
            match instruction.EvaluationStack.Values with
            | [ marker ] ->
                let addr =
                    match marker with
                    | EvalStackValue.ObjectRef a -> a
                    | other ->
                        failwith
                            $"%s{operation}: expected re-entry marker (object ref to allocated instance) on eval stack, got %O{other}"

                let _, state = IlMachineState.popEvalStack ctx.Thread state

                let state =
                    IlMachineState.writeManagedByrefWithBase
                        ctx.BaseClassTypes
                        state
                        outHandle
                        (CliType.ObjectRef (Some addr))

                NativeHandlerResult.completed state |> Some
            | [] ->
                let typeHandleTarget =
                    NativeCall.qCallTypeHandleToRuntimeTypeHandleTarget
                        operation
                        state
                        (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

                let pInstArray =
                    NativeCall.managedPointerOfPointerArgument operation "pTypeHandles" instruction.Arguments.[1]

                let cInstArray = NativeCall.int32Argument operation instruction.Arguments.[2]

                if cInstArray < 0 then
                    failwith $"%s{operation}: cTypeHandles must be non-negative, got %d{cInstArray}"

                let genericArguments =
                    [
                        for index in 0 .. cInstArray - 1 ->
                            readTypeHandleInstantiationElement ctx.BaseClassTypes operation state pInstArray index
                    ]

                let instantiatedHandle, state =
                    instantiateGenericRuntimeTypeTarget
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        operation
                        state
                        typeHandleTarget
                        genericArguments

                let state, typeInit =
                    IlMachineStateExecution.ensureTypeInitialised
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        ctx.Thread
                        instantiatedHandle
                        state

                match typeInit with
                | WhatWeDid.SuspendedForClassInit -> NativeHandlerResult.suspendedForClassInit state |> Some
                | WhatWeDid.BlockedOnClassInit blockedBy ->
                    NativeHandlerResult.blockedOnClassInit blockedBy state |> Some
                | WhatWeDid.ThrowingTypeInitializationException ->
                    NativeHandlerResult.throwingTypeInitializationException state |> Some
                | WhatWeDid.Aborted fatal -> NativeHandlerResult.aborted ctx.Thread fatal state |> Some
                | WhatWeDid.SuspendedForManagedCall ->
                    failwith "logic error: ensureTypeInitialised cannot suspend for an arbitrary managed call"
                | WhatWeDid.VoluntaryYield _ ->
                    failwith "logic error: ensureTypeInitialised cannot produce a VoluntaryYield"
                | WhatWeDid.Executed ->

                let concreteType =
                    AllConcreteTypes.lookup instantiatedHandle state.ConcreteTypes
                    |> Option.defaultWith (fun () ->
                        failwith $"%s{operation}: instantiated handle was not registered: %O{instantiatedHandle}"
                    )

                let assembly =
                    state.LoadedAssembly concreteType.AssemblyFullName
                    |> Option.defaultWith (fun () ->
                        failwith $"%s{operation}: assembly is not loaded: %s{concreteType.AssemblyFullName}"
                    )

                let typeInfo = assembly.TypeDefs.[concreteType.Definition.Get]

                if DumpedAssembly.isValueType ctx.BaseClassTypes state._LoadedAssemblies typeInfo then
                    // CoreCLR's QCall asserts !pVMT->IsByRefLike() and routes value types
                    // away from this path elsewhere; the only documented consumer
                    // (ArraySortHelper) instantiates reference types. If a value-type ever
                    // reaches us, calling the parameterless ctor with `this`-as-ObjectRef
                    // would silently box the receiver, so reject it explicitly.
                    failwith $"TODO: %s{operation} for value type %s{typeInfo.Namespace}.%s{typeInfo.Name}"

                let objectAddr, state =
                    allocateManagedObjectOfConcreteType
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        state
                        typeInfo
                        instantiatedHandle

                let ctor =
                    typeInfo.Methods
                    |> List.tryFind (fun m -> m.Name = ".ctor" && not m.IsStatic && MethodInfo.arity m = 0)
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"%s{operation}: no parameterless .ctor found on %s{typeInfo.Namespace}.%s{typeInfo.Name}"
                    )

                let state, concretizedCtor, _declaringTypeHandle =
                    ExecutionConcretization.concretizeMethodWithAllGenerics
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        concreteType.Generics
                        ctor
                        ImmutableArray.Empty
                        state

                // Push the allocated address as the re-entry marker. `callMethod` pops
                // only the ctor's `this` (which we push next), leaving the marker visible
                // to the re-entry branch above when the ctor returns.
                let state =
                    IlMachineState.pushToEvalStack (CliType.ObjectRef (Some objectAddr)) ctx.Thread state

                let state =
                    IlMachineState.pushToEvalStack (CliType.ObjectRef (Some objectAddr)) ctx.Thread state

                let threadState = state.ThreadState.[ctx.Thread]

                // wasConstructing = None: we're calling the ctor as a regular instance
                // method, not Newobj. We don't want returnStackFrame to push the
                // constructed value back — the marker is already there for us.
                // advanceProgramCounterOfCaller = false: the native frame has no IL.
                let state =
                    IlMachineStateExecution.callMethod
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        None
                        ConstructionState.NotConstructing
                        false
                        false
                        false
                        concretizedCtor.Generics
                        concretizedCtor
                        ctx.Thread
                        threadState
                        None
                        ReturnValueDisposition.PushToCaller
                        false // wrapExceptionInTargetInvocation
                        state

                NativeHandlerResult.pushedManagedCallee state |> Some
            | other ->
                failwith
                    $"%s{operation}: expected at most one re-entry marker on the eval stack, got %d{other.Length} value(s): %A{other}"
        | "RuntimeTypeHandle_GetDeclaringTypeHandle",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetDeclaringTypeHandle",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr) ->
            let operation = "RuntimeTypeHandle.GetDeclaringTypeHandle"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            let typeHandleArg = instruction.Arguments.[0] |> EvalStackValue.ofCliType

            let target =
                NativeCall.runtimeTypeHandleTargetOfEvalStackValue operation typeHandleArg

            // The managed wrapper RuntimeTypeHandle.GetDeclaringType filters TypeDesc handles
            // (generic parameters via the GetDeclaringTypeHandleForGenericVariable QCall, and
            // Byref/Pointer/FunctionPointer via the early `IsTypeDesc` exit returning null) so
            // this QCall only ever receives non-TypeDesc handles. Mirror CoreCLR's
            // `_ASSERTE(!typeHandle.IsTypeDesc())` by making contract violations loud rather
            // than silently returning a wrong answer if future BCL changes route a TypeDesc
            // through here.
            let typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn> option =
                match target with
                // The declaring type of an instantiation is the declaring type of its
                // definition — `IComparable<T>` is nested exactly where `IComparable<>` is.
                | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
                    RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity
                | RuntimeTypeHandleTarget.OpenConstructed (identity, _) ->
                    let assembly =
                        state.LoadedAssembly identity.AssemblyFullName
                        |> Option.defaultWith (fun () ->
                            failwith
                                $"%s{operation}: assembly for open generic type definition is not loaded: %s{identity.AssemblyFullName}"
                        )

                    Some assembly.TypeDefs.[identity.TypeDefinition.Get]
                | RuntimeTypeHandleTarget.Closed typeHandle ->
                    match typeHandle with
                    | ConcreteTypeHandle.Concrete _ ->
                        let concreteType =
                            AllConcreteTypes.lookup typeHandle state.ConcreteTypes
                            |> Option.defaultWith (fun () ->
                                failwith $"%s{operation}: concrete type handle was not registered: %O{typeHandle}"
                            )

                        let assembly =
                            state.LoadedAssembly concreteType.AssemblyFullName
                            |> Option.defaultWith (fun () ->
                                failwith
                                    $"%s{operation}: assembly for concrete type is not loaded: %s{concreteType.AssemblyFullName}"
                            )

                        Some assembly.TypeDefs.[concreteType.Definition.Get]
                    | ConcreteTypeHandle.OneDimArrayZero _
                    | ConcreteTypeHandle.Array _ ->
                        // Arrays have no DeclaringType. PawPrint reports IsTypeDesc=false for
                        // arrays (matching modern CoreCLR, where arrays are MethodTables rather
                        // than ArrayTypeDescs), so the BCL wrapper routes them here; return null.
                        None
                    | ConcreteTypeHandle.Byref _
                    | ConcreteTypeHandle.Pointer _
                    | ConcreteTypeHandle.FunctionPointer _ ->
                        failwith
                            $"%s{operation}: BCL contract violation: QCall reached for TypeDesc target %O{target}; the managed wrapper should have returned null without invoking this QCall"
                | RuntimeTypeHandleTarget.GenericParameter _
                | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
                    failwith
                        $"%s{operation}: BCL contract violation: QCall reached for generic-parameter target %O{target}; the managed wrapper should have routed this to RuntimeTypeHandle_GetDeclaringTypeHandleForGenericVariable"

            let declaringTarget, state =
                match typeInfo with
                | None -> None, state
                | Some typeInfo ->
                    declaringTypeHandleTargetForTypeInfo ctx.LoggerFactory ctx.BaseClassTypes state typeInfo

            let returnSource : NativeIntSource =
                match declaringTarget with
                | None -> NativeIntSource.Verbatim 0L
                | Some t -> NativeIntSource.TypeHandlePtr t

            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt returnSource) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "RuntimeTypeHandle_GetDeclaringTypeHandleForGenericVariable",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetDeclaringTypeHandleForGenericVariable",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr) ->
            let operation = "RuntimeTypeHandle.GetDeclaringTypeHandleForGenericVariable"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            let typeHandleArg = instruction.Arguments.[0] |> EvalStackValue.ofCliType

            let target =
                NativeCall.runtimeTypeHandleTargetOfEvalStackValue operation typeHandleArg

            // The managed wrapper RuntimeTypeHandle.GetDeclaringType only routes here when
            // typeHandle.IsTypeDesc and the CorElementType is ELEMENT_TYPE_VAR/MVAR — i.e.
            // exactly when the underlying target is a generic parameter. Mirror CoreCLR's
            // `_ASSERTE(typeHandle.IsGenericVariable())` and fail loudly on any other
            // shape rather than silently returning a wrong answer.
            let declaringTarget, state =
                match target with
                | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
                    RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
                | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
                    failwith
                        $"TODO: open constructed types are not handled at Native/NativeRuntimeTypeQCall.fs:%s{__LINE__}; got %O{openConstructed}"
                | RuntimeTypeHandleTarget.GenericParameter (declaringType, _) ->
                    // The owning type of a type-generic parameter is always generic
                    // (the parameter could not exist otherwise), so the declaring
                    // RuntimeType is the OpenGenericTypeDefinition. The type-handle
                    // registry keys structurally, so this is reference-equal to the
                    // RuntimeType allocated for `typeof(T<>)`.
                    RuntimeTypeHandleTarget.OpenGenericTypeDefinition declaringType, state
                | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, _, _) ->
                    // CoreCLR returns the owning method's MethodTable, which corresponds
                    // to the canonical (open) type that declares the method. For non-
                    // generic declaring types we must fall back to a Closed handle: an
                    // OpenGenericTypeDefinition target would incorrectly report
                    // IsGenericType=true. This mirrors `declaringRuntimeType` above.
                    let assembly =
                        state.LoadedAssembly declaringType.AssemblyFullName
                        |> Option.defaultWith (fun () ->
                            failwith
                                $"%s{operation}: assembly for method-generic-parameter declaring type is not loaded: %s{declaringType.AssemblyFullName}"
                        )

                    let typeInfo = assembly.TypeDefs.[declaringType.TypeDefinition.Get]

                    if typeInfo.Generics.IsEmpty then
                        let stk =
                            DumpedAssembly.signatureTypeKind ctx.BaseClassTypes state._LoadedAssemblies typeInfo

                        let state, typeHandle =
                            IlMachineState.concretizeType
                                ctx.LoggerFactory
                                ctx.BaseClassTypes
                                state
                                typeInfo.AssemblyFullName
                                ImmutableArray.Empty
                                ImmutableArray.Empty
                                (TypeDefn.FromDefinition (typeInfo.Identity, stk))

                        RuntimeTypeHandleTarget.Closed typeHandle, state
                    else
                        RuntimeTypeHandleTarget.OpenGenericTypeDefinition declaringType, state
                | RuntimeTypeHandleTarget.Closed _
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ ->
                    failwith
                        $"%s{operation}: BCL contract violation: QCall reached for non-generic-variable target %O{target}; the managed wrapper should have routed this to RuntimeTypeHandle_GetDeclaringTypeHandle"

            let state =
                IlMachineState.pushToEvalStack'
                    (EvalStackValue.NativeInt (NativeIntSource.TypeHandlePtr declaringTarget))
                    ctx.Thread
                    state

            NativeHandlerResult.completed state |> Some
        | "RuntimeTypeHandle_GetDeclaringMethodForGenericParameter",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetDeclaringMethodForGenericParameter",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallTypeHandle", qCallGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "ObjectHandleOnStack",
                                             objectHandleGenerics) ],
          MethodReturnType.Void when qCallGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            let operation = "RuntimeTypeHandle.GetDeclaringMethodForGenericParameter"

            if instruction.Arguments.Length <> 2 then
                failwith $"%s{operation}: expected two native arguments, got %d{instruction.Arguments.Length}"

            let qCallHandle = instruction.Arguments.[0] |> EvalStackValue.ofCliType

            let typeHandleTarget =
                NativeCall.qCallTypeHandleToRuntimeTypeHandleTarget operation state qCallHandle

            // Validate the ObjectHandleOnStack argument shape eagerly so a malformed
            // BCL call fails here rather than later. We don't write through the byref
            // on the type-level branch: the managed wrapper initialises its local
            // `IRuntimeMethodInfo? method = null` before the QCall, and CoreCLR's
            // implementation in runtimehandles.cpp:885 leaves `result` untouched when
            // the generic variable's def-token is not an mdtMethodDef. Returning
            // without writing therefore yields the same null the managed wrapper
            // reads back.
            let _ =
                NativeCall.objectHandleOnStackTarget operation state "result" instruction.Arguments.[1]

            match typeHandleTarget with
            | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
                RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
            | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
                failwith
                    $"TODO: open constructed types are not handled at Native/NativeRuntimeTypeQCall.fs:%s{__LINE__}; got %O{openConstructed}"
            | RuntimeTypeHandleTarget.GenericParameter _ ->
                // Type-level generic parameter: CoreCLR's `defToken` is mdtTypeDef,
                // so the early-exit branch leaves `result` null. Mirror that.
                NativeHandlerResult.completed state |> Some
            | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
                failwith
                    $"TODO: %s{operation} for method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}; need to allocate and return an IRuntimeMethodInfo for the declaring method (same gap as the RuntimeTypeHandle.GetDeclaringMethod InternalCall)"
            | RuntimeTypeHandleTarget.Closed _
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ ->
                failwith
                    $"%s{operation}: BCL contract violation: QCall reached for non-generic-variable target %O{typeHandleTarget}; the managed wrapper guards with Debug.Assert(IsGenericVariable(type))"
        | "ModuleHandle_ResolveType",
          "System.Private.CoreLib",
          "System",
          "ModuleHandle",
          "ResolveType",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallModule", qCallModuleGenerics)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "ObjectHandleOnStack",
                                             objectHandleGenerics) ],
          MethodReturnType.Void when qCallModuleGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            let operation = "ModuleHandle.ResolveType"

            if instruction.Arguments.Length <> 7 then
                failwith $"%s{operation}: expected seven native arguments, got %d{instruction.Arguments.Length}"

            let assemblyFullName =
                NativeCall.qCallModuleToAssemblyFullName
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let typeToken = NativeCall.int32Argument operation instruction.Arguments.[1]

            let typeInstArgsPtr =
                NativeCall.managedPointerOfPointerArgument operation "typeInstArgs" instruction.Arguments.[2]

            let typeInstCount = NativeCall.int32Argument operation instruction.Arguments.[3]

            if typeInstCount < 0 then
                failwith $"%s{operation}: typeInstCount must be non-negative, got %d{typeInstCount}"

            let methodInstArgsPtr =
                NativeCall.managedPointerOfPointerArgument operation "methodInstArgs" instruction.Arguments.[4]

            let methodInstCount = NativeCall.int32Argument operation instruction.Arguments.[5]

            if methodInstCount < 0 then
                failwith $"%s{operation}: methodInstCount must be non-negative, got %d{methodInstCount}"

            let retType =
                NativeCall.objectHandleOnStackTarget operation state "type" instruction.Arguments.[6]

            let assembly =
                state.LoadedAssembly assemblyFullName
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: module's assembly %s{assemblyFullName} is not loaded"
                )

            // CoreCLR allows the caller to pass declaring-type / declaring-method generic
            // argument arrays as substitution context for tokens that reference generic
            // parameters (typically TypeSpecs); these arrays may also be supplied for tokens
            // that don't need them, in which case they are simply unused. Decode them up
            // front so we never reject a call whose token doesn't actually consume them.
            let typeInstantiation =
                ImmutableArray.CreateRange (
                    seq {
                        for index in 0 .. typeInstCount - 1 ->
                            readTypeHandleInstantiationElement ctx.BaseClassTypes operation state typeInstArgsPtr index
                    }
                )

            let methodInstantiation =
                ImmutableArray.CreateRange (
                    seq {
                        for index in 0 .. methodInstCount - 1 ->
                            readTypeHandleInstantiationElement
                                ctx.BaseClassTypes
                                operation
                                state
                                methodInstArgsPtr
                                index
                    }
                )

            // A token naming a row this module does not have. CoreCLR's
            // `ClassLoader::LoadTypeDefOrRefOrSpecThrowing` throws for one, and the managed
            // wrapper `ModuleHandle.ResolveTypeHandle` catches *that* and asks
            // `MetadataImport.IsValidToken` whether the token was the problem; when it says no,
            // the guest gets an `ArgumentOutOfRangeException` naming `typeToken`
            // (RuntimeHandles.cs:1851-1857). So the exception raised here is not the one the guest
            // sees, and deliberately so: only CoreLib can attach that `paramName`.
            //
            // Screened with the same predicate the `IsValidToken` FCall answers from, rather than a
            // separate `ContainsKey` test per arm. The two must agree — if this raised while
            // `IsValidToken` called the token valid, the managed wrapper would rethrow and the
            // guest would see an `ArgumentException` instead — and sharing the predicate is what
            // makes that structural rather than a coincidence of two tests over the same data.
            if not (NativeMetadataImport.isValidToken operation assembly typeToken) then
                NativeHandlerResult.raiseException ctx.BaseClassTypes.BadImageFormatException state
                |> Some
            else

            // The C# wrapper validates the token kind (TypeDef/TypeSpec/TypeRef, and not the
            // global TypeDef token) before reaching this QCall, so any other kind here is a
            // contract violation rather than user error.
            let state, target =
                match MetadataToken.ofInt typeToken with
                | MetadataToken.TypeDefinition h ->
                    let state, typeDefn =
                        IlMachineState.lookupTypeDefn ctx.BaseClassTypes state assembly h

                    IlMachineState.runtimeTypeHandleTargetForTypeToken
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        assembly
                        true
                        typeInstantiation
                        methodInstantiation
                        typeDefn
                        state
                | MetadataToken.TypeReference h ->
                    // Resolve the TypeRef itself with no caller-supplied generic context: the
                    // referenced type's own definition must not be substituted via the caller's
                    // type/method instantiation. Caller context is reserved for TypeSpec generic
                    // substitution, applied below by runtimeTypeHandleTargetForTypeToken.
                    let state, typeDefn, declaringAssembly =
                        IlMachineState.lookupTypeRef
                            ctx.LoggerFactory
                            ctx.BaseClassTypes
                            state
                            assembly
                            ImmutableArray.Empty
                            h

                    IlMachineState.runtimeTypeHandleTargetForTypeToken
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        declaringAssembly
                        true
                        typeInstantiation
                        methodInstantiation
                        typeDefn
                        state
                | MetadataToken.TypeSpecification h ->
                    // Mirror executeLdtoken: feed the raw signature directly with
                    // allowOpenGenericDefinition=false. TypeSpecs already encode their
                    // structure, including any generic instantiations.
                    let typeDefn = assembly.TypeSpecs.[h].Signature

                    IlMachineState.runtimeTypeHandleTargetForTypeToken
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        assembly
                        false
                        typeInstantiation
                        methodInstantiation
                        typeDefn
                        state
                | other ->
                    failwith
                        $"%s{operation}: unexpected metadata token kind %O{other} from token 0x%08x{typeToken}; the managed wrapper should only forward TypeDef/TypeSpec/TypeRef"

            let runtimeTypeAddr, state =
                IlMachineState.getOrAllocateType ctx.LoggerFactory ctx.BaseClassTypes target state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retType
                    (CliType.ObjectRef (Some runtimeTypeAddr))

            NativeHandlerResult.completed state |> Some
        | "ModuleHandle_ResolveMethod",
          "System.Private.CoreLib",
          "System",
          "ModuleHandle",
          "ResolveMethod",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallModule", qCallModuleGenerics)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (CorelibType state.ConcreteTypes ("System",
                                                                     "RuntimeMethodHandleInternal",
                                                                     returnGenerics)) when
            qCallModuleGenerics.IsEmpty && returnGenerics.IsEmpty
            ->
            let operation = "ModuleHandle.ResolveMethod"

            if instruction.Arguments.Length <> 6 then
                failwith $"%s{operation}: expected six native arguments, got %d{instruction.Arguments.Length}"

            let assemblyFullName =
                NativeCall.qCallModuleToAssemblyFullName
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let methodToken = NativeCall.int32Argument operation instruction.Arguments.[1]

            let typeInstArgsPtr =
                NativeCall.managedPointerOfPointerArgument operation "typeInstArgs" instruction.Arguments.[2]

            let typeInstCount = NativeCall.int32Argument operation instruction.Arguments.[3]

            if typeInstCount < 0 then
                failwith $"%s{operation}: typeInstCount must be non-negative, got %d{typeInstCount}"

            let methodInstArgsPtr =
                NativeCall.managedPointerOfPointerArgument operation "methodInstArgs" instruction.Arguments.[4]

            let methodInstCount = NativeCall.int32Argument operation instruction.Arguments.[5]

            if methodInstCount < 0 then
                failwith $"%s{operation}: methodInstCount must be non-negative, got %d{methodInstCount}"

            let assembly =
                state.LoadedAssembly assemblyFullName
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: module's assembly %s{assemblyFullName} is not loaded"
                )

            // Decode the caller-supplied substitution context up front. Mirrors CoreCLR's
            // SigTypeContext(Instantiation(typeArgs, ...), Instantiation(methodArgs, ...)): the
            // arrays are used to substitute any GenericTypeParameter / GenericMethodParameter
            // references the token's signatures contain, and may be empty for tokens that don't
            // need them.
            let typeInstantiation =
                ImmutableArray.CreateRange (
                    seq {
                        for index in 0 .. typeInstCount - 1 ->
                            readTypeHandleInstantiationElement ctx.BaseClassTypes operation state typeInstArgsPtr index
                    }
                )

            let methodInstantiation =
                ImmutableArray.CreateRange (
                    seq {
                        for index in 0 .. methodInstCount - 1 ->
                            readTypeHandleInstantiationElement
                                ctx.BaseClassTypes
                                operation
                                state
                                methodInstArgsPtr
                                index
                    }
                )

            let state, concretizedMethod =
                match MetadataToken.ofInt methodToken with
                | MetadataToken.MethodDef h ->
                    // CoreCLR's ModuleHandle.ResolveMethod returns the metadata definition for
                    // a MethodDef token without consulting the caller-supplied
                    // type/method-instantiation arrays:
                    //   MemberLoader::GetMethodDescFromMemberDefOrRefOrSpec
                    //     -> GetMethodDescFromMethodDef (no SigTypeContext parameter)
                    // (memberload.cpp). So `ResolveMethodHandle(token)` for a method like
                    // `Generic<T>.M` returns the open `Generic<T>.M`, even if the caller
                    // supplied `typeInst = [string]`. Our registry only stores fully concretised
                    // methods; faithfully representing the open form is not yet supported.
                    let method = assembly.Methods.[h]

                    if method.DeclaringTypeGenerics.Length > 0 then
                        failwith
                            $"TODO: %s{operation}: MethodDef token 0x%08x{methodToken} declared on generic type %s{MethodOwner.describe method.Owner} (%d{method.DeclaringTypeGenerics.Length} type generic parameter(s)); CoreCLR returns the open metadata definition without consuming the caller's typeInstantiation, but the MethodHandle registry only supports fully concretised methods."

                    if method.Generics.Length > 0 then
                        failwith
                            $"TODO: %s{operation}: MethodDef token 0x%08x{methodToken} resolves to generic method %s{method.Name} (%d{method.Generics.Length} method generic parameter(s)); CoreCLR returns the open metadata definition without consuming the caller's methodInstantiation, but the MethodHandle registry only supports fully concretised methods."

                    let methodMapped =
                        method
                        |> MethodInfo.mapTypeGenerics (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)

                    // Pass empty instantiation arrays: for MethodDef tokens, CoreCLR does not
                    // consume the caller's type/method instantiation context, and after the
                    // guards above the method has no generic parameters to substitute anyway.
                    let state, concretized, _ =
                        ExecutionConcretization.concretizeMethodWithAllGenerics
                            ctx.LoggerFactory
                            ctx.BaseClassTypes
                            ImmutableArray.Empty
                            methodMapped
                            ImmutableArray.Empty
                            state

                    state, concretized
                | MetadataToken.MemberReference h ->
                    // Surface typeInstantiation/methodInstantiation as TypeDefn arrays so the
                    // MemberRef resolver can substitute any GenericTypeParameter /
                    // GenericMethodParameter appearing in the TypeSpec parent or member
                    // signature.
                    let typeGenericsAsTypeDefn =
                        typeInstantiation
                        |> Seq.map (fun handle ->
                            Concretization.concreteHandleToTypeDefn
                                ctx.BaseClassTypes
                                handle
                                state.ConcreteTypes
                                state._LoadedAssemblies
                        )
                        |> ImmutableArray.CreateRange

                    let methodGenericsAsTypeDefn =
                        methodInstantiation
                        |> Seq.map (fun handle ->
                            Concretization.concreteHandleToTypeDefn
                                ctx.BaseClassTypes
                                handle
                                state.ConcreteTypes
                                state._LoadedAssemblies
                        )
                        |> ImmutableArray.CreateRange

                    let state, _, resolved, extractedTypeArgs =
                        IlMachineState.resolveMemberWithGenerics
                            ctx.LoggerFactory
                            ctx.BaseClassTypes
                            ctx.Thread
                            assembly
                            typeGenericsAsTypeDefn
                            methodGenericsAsTypeDefn
                            h
                            state

                    let method =
                        match resolved with
                        | Choice1Of2 m -> m
                        | Choice2Of2 _field ->
                            failwith
                                $"%s{operation}: MemberRef token 0x%08x{methodToken} resolved to a field, but ResolveMethod expects a method"

                    // `extractedTypeArgs` are the TypeDefn args of the parent TypeSpec, already
                    // substituted via the caller-supplied type/method instantiation context
                    // above. They are therefore closed and can be concretized with an empty
                    // substitution context.
                    let state, declaringTypeGenerics =
                        ((state, ImmutableArray.CreateBuilder ()), extractedTypeArgs)
                        ||> Seq.fold (fun (state, acc) ty ->
                            let state, handle =
                                IlMachineState.concretizeType
                                    ctx.LoggerFactory
                                    ctx.BaseClassTypes
                                    state
                                    method.DeclaringAssemblyFullName
                                    ImmutableArray.Empty
                                    ImmutableArray.Empty
                                    ty

                            acc.Add handle
                            state, acc
                        )
                        |> Tuple.rmap (fun b -> b.ToImmutable ())

                    let state, concretized, _ =
                        ExecutionConcretization.concretizeMethodWithAllGenerics
                            ctx.LoggerFactory
                            ctx.BaseClassTypes
                            declaringTypeGenerics
                            method
                            methodInstantiation
                            state

                    state, concretized
                | MetadataToken.MethodSpecification _ ->
                    // MethodSpec encodes its method-generic instantiation in the spec itself
                    // rather than via the caller-supplied methodInstantiation buffer, so this
                    // case needs a separate concretization path. Leave unimplemented until a
                    // test exercises it.
                    failwith $"TODO: %s{operation} does not yet handle MethodSpec tokens (token 0x%08x{methodToken})"
                | other ->
                    failwith
                        $"%s{operation}: unexpected metadata token kind %O{other} from token 0x%08x{methodToken}; the managed wrapper should only forward MethodDef/MemberReference/MethodSpec"

            let handleValue, reg =
                MethodHandleRegistry.getOrAllocateConcreteInternalHandle
                    ctx.BaseClassTypes
                    state.ConcreteTypes
                    concretizedMethod
                    state.MethodHandles

            let state =
                { state with
                    MethodHandles = reg
                }

            let state =
                IlMachineState.pushToEvalStack (CliType.ValueType handleValue) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "RuntimeTypeHandle_GetFields",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          _,
          [ ConcretePointer (CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                                              "MethodTable",
                                                              methodTableGenerics))
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ],
          returnType when methodTableGenerics.IsEmpty ->
            let operation = "RuntimeTypeHandle.GetFields"

            // The QCall returns Interop.BOOL — an int32-backed enum nested inside the
            // top-level static class `Interop`. PawPrint sees nested types with an empty
            // namespace and the bare name, so accept that shape and reject anything else
            // explicitly so a future BCL refactor surfaces a clear error. The managed
            // wrapper unpacks Span<IntPtr> into a pinned ptr[intptr] before invoking the
            // QCall stub, so we see three raw pointers here, not a Span value type.
            match returnType with
            | MethodReturnType.Returns (CorelibType state.ConcreteTypes ("", "BOOL", boolGenerics)) when
                boolGenerics.IsEmpty
                ->
                ()
            | other -> failwith $"%s{operation}: unexpected QCall stub return type %O{other}"

            if instruction.Arguments.Length <> 3 then
                failwith $"%s{operation}: expected three native arguments, got %d{instruction.Arguments.Length}"

            // Arg 0: MethodTable* — already a closed RuntimeTypeHandleTarget. The .NET 10
            // wrapper short-circuits TypeDesc cases (byref/pointer/generic param) at the
            // managed level via `typeHandle.IsTypeDesc`, so we expect Closed Concrete /
            // arrays / OpenGenericTypeDefinition here.
            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfEvalStackValue
                    operation
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            // Arg 1: ptr[intptr] — pointer to the first IntPtr slot of the caller's
            // buffer. The wrapper produces this by pinning Span<IntPtr>.GetPinnableReference
            // and Conv_U-ing the resulting byref.
            let resultBuffer =
                NativeCall.managedPointerOfPointerArgument operation "result" instruction.Arguments.[1]

            // Arg 2: ptr[int32] — pointer to the usedCount slot. The wrapper initialises
            // it to `buffer.Length` so we read the input capacity from this slot, then
            // overwrite it with the actual count on return regardless of whether the
            // caller's buffer was big enough.
            let countPtr =
                NativeCall.managedPointerOfPointerArgument operation "usedCount" instruction.Arguments.[2]

            let capacity = int32AtPointer operation ctx.BaseClassTypes state countPtr

            let state, fieldHandleIds =
                match typeHandleTarget with
                | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
                    RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
                | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
                    failwith
                        $"TODO: open constructed types are not handled at Native/NativeRuntimeTypeQCall.fs:%s{__LINE__}; got %O{openConstructed}"
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                    // CoreCLR's RuntimeTypeHandle::GetFields walks the canonical
                    // (open-generic) MethodTable's FieldDescs. The resulting handles
                    // carry `OpenGenericTypeDefinition` as the declaring target —
                    // observably distinct from the closed-instantiation handles a
                    // `typeof(G<int>)` walk would produce.
                    walkFieldsOfTypeDefinition
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        operation
                        identity.AssemblyFullName
                        identity.TypeDefinition.Get
                        (RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity)
                        state
                | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
                    // The wrapper asserts !IsGenericVariable(type) before reaching us, so
                    // PawPrint should never see this case. Surface it loudly if it does.
                    failwith
                        $"%s{operation}: generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get} reached the QCall; the managed wrapper should have asserted before this point"
                | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
                    failwith
                        $"%s{operation}: method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get} reached the QCall; the managed wrapper should have asserted before this point"
                | RuntimeTypeHandleTarget.Closed typeHandle ->
                    walkClosedTypeHandleFields ctx.LoggerFactory ctx.BaseClassTypes operation typeHandle state

            let count = List.length fieldHandleIds

            let state =
                if count > capacity then
                    writeInt32AtPointer ctx.BaseClassTypes state countPtr count
                else
                    let state =
                        ((state, 0), fieldHandleIds)
                        ||> List.fold (fun (state, index) fieldHandleId ->
                            writeFieldHandleElement operation ctx.BaseClassTypes state resultBuffer index fieldHandleId,
                            index + 1
                        )
                        |> fst

                    writeInt32AtPointer ctx.BaseClassTypes state countPtr count

            // Push Interop.BOOL.TRUE / FALSE — represented as Int32 1 / 0. Real CoreCLR
            // returns FALSE when the caller's buffer was too small (so the managed
            // wrapper can resize and retry); we mirror that contract.
            let result = if count <= capacity then 1 else 0

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 result)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "RuntimeTypeHandle_GetInterfaces",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          _,
          [ ConcretePointer (CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                                              "MethodTable",
                                                              methodTableGenerics))
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "ObjectHandleOnStack",
                                             objectHandleGenerics) ],
          MethodReturnType.Void when methodTableGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            // CoreCLR's RuntimeTypeHandle_GetInterfaces (runtimehandles.cpp:518) walks
            // MethodTable::IterateInterfaceMap, allocates a fresh PTRARRAYREF of length
            // pMT->GetNumInterfaces() with element type RuntimeType, populates each slot
            // from `it.GetInterface(pMT)->GetManagedClassObject()`, and writes the array
            // through the ObjectHandleOnStack. If ifaceCount == 0 the QCall returns
            // without writing, leaving the caller's empty-array local intact (the managed
            // wrapper RuntimeHandles.cs:559 initialises `result` to `[]`).
            //
            // The managed wrapper short-circuits TypeDesc cases (byref/pointer/generic
            // parameter) by returning `[]` before reaching the QCall, so we expect a
            // closed MethodTable here.
            let operation = "RuntimeTypeHandle.GetInterfaces"

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfEvalStackValue
                    operation
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let retArray =
                NativeCall.objectHandleOnStackTarget operation state "result" instruction.Arguments.[1]

            match typeHandleTarget with
            | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
                RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
            | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
                failwith
                    $"TODO: open constructed types are not handled at Native/NativeRuntimeTypeQCall.fs:%s{__LINE__}; got %O{openConstructed}"
            | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Byref _ as handle)
            | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Pointer _ as handle)
            | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.FunctionPointer _ as handle) ->
                failwith
                    $"%s{operation}: byref/pointer/function-pointer handle %O{handle} reached the QCall; the managed wrapper should have short-circuited IsTypeDesc to `[]`"
            | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
                failwith
                    $"%s{operation}: generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get} reached the QCall; the managed wrapper short-circuits IsTypeDesc to `[]` before this point"
            | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
                failwith
                    $"%s{operation}: method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get} reached the QCall; the managed wrapper short-circuits IsTypeDesc to `[]` before this point"
            // An array's MethodTable interface map is *inherited verbatim* from
            // `System.Array` — CoreCLR copies it row for row in `CreateArrayMethodTable`
            // ("Because of array method table persisting, we need to copy the map",
            // `src/coreclr/vm/array.cpp:410-424`), so `GetNumInterfaces()` for `int[]` is
            // `System.Array`'s six and this QCall returns exactly those.
            //
            // The five implicit generic interfaces of an SZ array (`IList<T>` and friends)
            // are deliberately *not* added here. They are appended in managed code by
            // `RuntimeTypeCache.PopulateInterfaces` (`RuntimeType.CoreCLR.cs:1043-1055`),
            // which PawPrint runs from the guest's own corelib. Synthesising them here would
            // double-count them — `PopulateInterfaces` appends unconditionally, with no
            // dedup against what we return — and would make our `MethodTable` projection
            // disagree with the map we hand back.
            //
            // So array handles simply fall through to the ordinary walk below: they
            // contribute no interfaces of their own, and `resolveBaseConcreteType` already
            // knows that an array's base type is `System.Array`, whose closure the walk then
            // collects.
            | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete _)
            | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.OneDimArrayZero _)
            | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Array _)
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ ->

            // CoreCLR's MethodTable interface map enumerates ALL implemented interfaces:
            // the type's direct ImplementedInterfaces rows, every interface those interfaces
            // transitively extend, and the same closure for every base class up the chain.
            // Mirror that here so e.g. `class D : B` where `B : IDisposable` reports
            // `IDisposable`, and a class implementing `IList<T>` also reports `ICollection<T>`,
            // `IEnumerable<T>`, `IEnumerable`. Concretization always uses the *owning* type's
            // generics: when walking B<int>'s interfaces we resolve under `[int]`, regardless
            // of the derived class's own generic instantiation.
            //
            // For an `OpenGenericTypeDefinition` (CoreCLR's canonical MethodTable
            // `G<__Canon>`), the interface map is also canonical: each row whose
            // metadata references `G`'s generic parameters becomes a `bound shared`
            // MT (e.g. `class G<T> : IList<T>` → slot `IList<__Canon>` →
            // `OpenGenericTypeDefinition IList`). We do not yet have a representation
            // for that shape, so the open-generic walker only handles rows that are
            // fully closed in metadata (no generic parameter references at all);
            // anything else fails loudly.
            let rec collectInterfaces
                (state : IlMachineState, seen : Set<RuntimeTypeHandleTarget>, ordered : RuntimeTypeHandleTarget list)
                (current : RuntimeTypeHandleTarget)
                : IlMachineState * Set<RuntimeTypeHandleTarget> * RuntimeTypeHandleTarget list
                =
                let state, seen, ordered =
                    match current with
                    // Structural handles carry no interface rows of their own. For arrays
                    // that is CoreCLR's own rule (the map is inherited from `System.Array`,
                    // reached by the base-type walk below); byrefs, pointers and function
                    // pointers are TypeDescs, which have no MethodTable and so no map at
                    // all. Either way the walk continues to the parent rather than stopping.
                    | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
                        RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
                    | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
                        failwith
                            $"TODO: open constructed types are not handled at Native/NativeRuntimeTypeQCall.fs:%s{__LINE__}; got %O{openConstructed}"
                    | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.OneDimArrayZero _)
                    | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Array _)
                    | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Byref _)
                    | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Pointer _)
                    | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.FunctionPointer _) -> state, seen, ordered
                    | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete _ as currentHandle) ->
                        match IlMachineState.tryGetConcreteTypeInfo state currentHandle with
                        // A `Concrete` handle names a type with a TypeDef row, so its type
                        // info must be registered; a miss is a bug in whoever produced the
                        // handle, not a type that legitimately has no interfaces.
                        | None -> failwith $"%s{operation}: concrete type handle was not registered: %O{currentHandle}"
                        | Some (currentCt, currentTypeInfo) ->
                            let currentAssy =
                                state.LoadedAssembly currentCt.Identity.AssemblyFullName
                                |> Option.defaultWith (fun () ->
                                    failwith
                                        $"%s{operation}: owning assembly %s{currentCt.Identity.AssemblyFullName} not loaded"
                                )

                            ((state, seen, ordered), currentTypeInfo.ImplementedInterfaces)
                            ||> Seq.fold (fun (state, seen, ordered) impl ->
                                let implAssy =
                                    state.LoadedAssembly impl.RelativeToAssembly.FullName
                                    |> Option.defaultValue currentAssy

                                let state, implTypeDefn, implResolvedAssy =
                                    IlMachineState.resolveTypeMetadataToken
                                        ctx.LoggerFactory
                                        ctx.BaseClassTypes
                                        state
                                        implAssy
                                        currentCt.Generics
                                        impl.InterfaceHandle

                                let state, implHandle =
                                    IlMachineState.concretizeType
                                        ctx.LoggerFactory
                                        ctx.BaseClassTypes
                                        state
                                        implResolvedAssy.DefinitionFullName
                                        currentCt.Generics
                                        ImmutableArray.Empty
                                        implTypeDefn

                                let implTarget = RuntimeTypeHandleTarget.Closed implHandle

                                if Set.contains implTarget seen then
                                    state, seen, ordered
                                else
                                    let seen = Set.add implTarget seen
                                    let ordered = implTarget :: ordered
                                    // Recurse into the interface's own transitive interface set.
                                    collectInterfaces (state, seen, ordered) implTarget
                            )
                    | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                        let currentAssy =
                            state.LoadedAssembly identity.AssemblyFullName
                            |> Option.defaultWith (fun () ->
                                failwith
                                    $"%s{operation}: assembly %s{identity.AssemblyFullName} not loaded for open generic typedef %O{identity.TypeDefinition.Get}"
                            )

                        let currentTypeInfo = currentAssy.TypeDefs.[identity.TypeDefinition.Get]

                        ((state, seen, ordered), currentTypeInfo.ImplementedInterfaces)
                        ||> Seq.fold (fun (state, seen, ordered) impl ->
                            let implAssy =
                                state.LoadedAssembly impl.RelativeToAssembly.FullName
                                |> Option.defaultValue currentAssy

                            let state, implTypeDefn, implResolvedAssy =
                                IlMachineState.resolveTypeMetadataToken
                                    ctx.LoggerFactory
                                    ctx.BaseClassTypes
                                    state
                                    implAssy
                                    ImmutableArray.Empty
                                    impl.InterfaceHandle

                            if IlMachineState.containsAnyGenericParameter implTypeDefn then
                                failwith
                                    $"TODO: %s{operation} for open generic typedef %O{identity.TypeDefinition.Get} in %s{identity.AssemblyFullName}: interface row resolves to %O{implTypeDefn}, which references generic parameters (bound-shared interface MT); only fully-closed interface rows are supported today"
                            else
                                let state, implHandle =
                                    IlMachineState.concretizeType
                                        ctx.LoggerFactory
                                        ctx.BaseClassTypes
                                        state
                                        implResolvedAssy.DefinitionFullName
                                        ImmutableArray.Empty
                                        ImmutableArray.Empty
                                        implTypeDefn

                                let implTarget = RuntimeTypeHandleTarget.Closed implHandle

                                if Set.contains implTarget seen then
                                    state, seen, ordered
                                else
                                    let seen = Set.add implTarget seen
                                    let ordered = implTarget :: ordered
                                    collectInterfaces (state, seen, ordered) implTarget
                        )
                    | RuntimeTypeHandleTarget.GenericParameter _
                    | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
                        // The top-level dispatch above rejects these; the only way one
                        // could reach the recursive walker is if a base-walk produced
                        // a generic-parameter target, but `resolveBaseRuntimeTypeHandleTarget`
                        // refuses for those shapes. Fail loudly if we ever get here.
                        failwith
                            $"%s{operation}: generic-parameter target %O{current} reached collectInterfaces; TypeDescs have no interface map"

                // Walk up the base type chain.
                let state, baseTarget =
                    IlMachineState.resolveBaseRuntimeTypeHandleTarget ctx.LoggerFactory ctx.BaseClassTypes state current

                match baseTarget with
                | None -> state, seen, ordered
                | Some baseTarget -> collectInterfaces (state, seen, ordered) baseTarget

            let state, _, interfaceTargetsReversed =
                collectInterfaces (state, Set.empty, []) typeHandleTarget

            let interfaceTargets = List.rev interfaceTargetsReversed

            if List.isEmpty interfaceTargets then
                // Mirror CoreCLR: skip the allocation and leave the caller's `[]` local intact.
                NativeHandlerResult.completed state |> Some
            else
                let state, _, runtimeTypeElementHandle =
                    concretizeNonGenericCorelibType ctx.LoggerFactory ctx.BaseClassTypes state "System" "RuntimeType"

                let arrayAddr, state =
                    IlMachineState.allocateArray
                        (ConcreteTypeHandle.OneDimArrayZero runtimeTypeElementHandle)
                        (fun () -> CliType.ObjectRef None)
                        (List.length interfaceTargets)
                        state

                let state =
                    ((state, 0), interfaceTargets)
                    ||> List.fold (fun (state, index) ifaceTarget ->
                        let runtimeTypeAddr, state =
                            IlMachineState.getOrAllocateType ctx.LoggerFactory ctx.BaseClassTypes ifaceTarget state

                        let state =
                            IlMachineState.setArrayValue
                                arrayAddr
                                (CliType.ObjectRef (Some runtimeTypeAddr))
                                index
                                state

                        state, index + 1
                    )
                    |> fst

                let state =
                    IlMachineState.writeManagedByrefWithBase
                        ctx.BaseClassTypes
                        state
                        retArray
                        (CliType.ObjectRef (Some arrayAddr))

                NativeHandlerResult.completed state |> Some
        | "RuntimeTypeHandle_InternalAllocNoChecks",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "InternalAllocNoChecks",
          [ ConcretePointer (CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                                              "MethodTable",
                                                              methodTableGenerics))
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "ObjectHandleOnStack",
                                             objectHandleGenerics) ],
          MethodReturnType.Void when methodTableGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            // CoreCLR: `RuntimeTypeHandle_InternalAllocNoChecks`, reflectioninvocation.cpp:134,
            // which is `pMT->AllocateNoChecks()`. This is the slow half of
            // `RuntimeTypeHandle.InternalAllocNoChecks` (RuntimeHandles.cs:304); PawPrint's fast
            // half always declines, so this is where every such allocation lands. See
            // `RuntimeTypeHandle.InternalAllocNoChecks_FastPath` in Native/NativeRuntimeTypeFCall.fs
            // for why.
            //
            // "NoChecks" is what separates this from
            // `RuntimeTypeHandle_InternalAlloc`: it runs no class initialiser and performs no
            // activation or instantiability check, because its callers already know the type is
            // initialised and allocatable (`MethodTable::AllocateNoChecks`, methodtable.h:2701,
            // "can only be used if ... IsClassInited() are known to be true"). Do not "tidy" this
            // into initialising the type the way the sibling
            // `RuntimeTypeHandle_CreateInstanceForAnotherGenericParameter` does;
            // `TestInternalAllocNoChecks` pins that a type carrying a `.cctor` allocates here
            // without that `.cctor` running.
            let operation = "RuntimeTypeHandle.InternalAllocNoChecks"

            if instruction.Arguments.Length <> 2 then
                failwith $"%s{operation}: expected two native arguments, got %d{instruction.Arguments.Length}"

            let typeHandle =
                NativeCall.methodTableOfEvalStackValue operation (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            // PawPrint never puts a `Nullable<T>` on the heap: `box`/`unbox` special-case it
            // before allocating (UnaryMetadataObjectOps), so a heap object carrying a Nullable
            // MethodTable is a shape no reader here is prepared for. Creating one would be silent
            // corruption; refusing is a loud failure. CoreCLR needs no such guard — its readers
            // cope with a raw-layout box — so this is PawPrint's invariant, not upstream
            // behaviour.
            //
            // Unlike the same refusal in the `calli` allocation helper
            // (UnaryMetadataCallOps.executeAllocationHelperCall), this is a guard rather than a
            // proof. That helper can enumerate its two producers and show neither can pair it
            // with a Nullable. Here, one of the three BCL callers *deliberately* can:
            // `AsyncHelpers.AllocContinuationResultBox` (AsyncHelpers.CoreCLR.cs:198) exists
            // precisely "to store structs without changing layout, including nullables", so a
            // runtime-async method returning an object-containing `Nullable<T>` passes exactly
            // this shape. It is unreachable today because PawPrint models no runtime-async at all
            // — that method is called from JIT-generated code (`corelib.h`,
            // `ALLOC_CONTINUATION_RESULT_BOX`), never from IL a guest can execute — and until
            // that changes there is no way to exercise, or therefore to test, a raw-layout
            // Nullable box. When runtime-async
            // arrives, this failure is what will fire, and the fix is a heap representation for a
            // layout-preserving Nullable box, not a wider predicate here. The other two callers
            // avoid the shape for good: `MulticastDelegate.NewMulticastDelegate` passes a
            // delegate's MethodTable, and `RuntimeHelpers.Box` routes Nullables to
            // `CastHelpers.Box_Nullable`, which substitutes the underlying `T`.
            //
            // The invariant properly belongs at the chokepoint
            // (`IlMachineState.allocateUninitialisedInstance`), which would subsume both copies.
            match AllConcreteTypes.lookup typeHandle state.ConcreteTypes with
            | Some ct when InternalTypeKind.kind ctx.BaseClassTypes ct = InternalTypeKind.Nullable ->
                failwith
                    $"%s{operation}: refusing to allocate a Nullable<T> (%O{typeHandle}) on the heap; PawPrint boxes the underlying value instead, so no reader can interpret such an object. CoreCLR does allow this, for runtime-async continuation result boxes, which preserve a nullable's layout — if that is what you are hitting, PawPrint needs a layout-preserving boxed-Nullable representation"
            | _ -> ()

            let result =
                NativeCall.objectHandleOnStackTarget operation state "result" instruction.Arguments.[1]

            let addr, state =
                IlMachineState.allocateUninitialisedInstance ctx.LoggerFactory ctx.BaseClassTypes typeHandle state

            let state =
                IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state result (CliType.ObjectRef (Some addr))

            NativeHandlerResult.completed state |> Some
        | "RuntimeTypeHandle_InternalAlloc",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "InternalAlloc",
          [ ConcretePointer (CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                                              "MethodTable",
                                                              methodTableGenerics))
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "ObjectHandleOnStack",
                                             objectHandleGenerics) ],
          MethodReturnType.Void when methodTableGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            // CoreCLR: `RuntimeTypeHandle_InternalAlloc`, reflectioninvocation.cpp:119, which is
            // `pMT->Allocate()`. The checked counterpart of `RuntimeTypeHandle_InternalAllocNoChecks`
            // above: same allocation, but `MethodTable::Allocate` (methodtable.cpp:4056) first
            // runs `EnsureInstanceActive` and, for a type with precise-init cctors, a class
            // initialiser.
            //
            // Two managed callers, and they want different things:
            //
            //  * `Delegate.InternalAlloc` (Delegate.CoreCLR.cs:435), reached from all four
            //    `Delegate.CreateDelegate` overloads and from `CreateDelegateInternal`. It
            //    allocates the delegate object; a separate `Delegate_BindToMethodName`/
            //    `Delegate_BindToMethodInfo` QCall then binds it to a target. This is the caller
            //    that works.
            //  * `RuntimeMethodHandle.ReboxToNullable` (RuntimeHandles.cs:1200), reached from
            //    `RuntimeType.CheckValue` whenever reflection has to coerce an argument to a
            //    `Nullable<T>` parameter — `MethodInfo.Invoke` on a method taking `int?`, say. It
            //    passes a *nullable's* MethodTable and then `Unbox_Nullable`s into the raw data of
            //    what comes back. This is the caller that is refused below.
            let operation = "RuntimeTypeHandle.InternalAlloc"

            if instruction.Arguments.Length <> 2 then
                failwith $"%s{operation}: expected two native arguments, got %d{instruction.Arguments.Length}"

            let typeHandle =
                NativeCall.methodTableOfEvalStackValue operation (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let concreteType =
                AllConcreteTypes.lookup typeHandle state.ConcreteTypes
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: MethodTable handle was not registered: %O{typeHandle}"
                )

            let assembly =
                state.LoadedAssembly concreteType.AssemblyFullName
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: assembly is not loaded: %s{concreteType.AssemblyFullName}"
                )

            let typeInfo = assembly.TypeDefs.[concreteType.Definition.Get]

            // The `ReboxToNullable` caller lands here, and PawPrint cannot serve it. A boxed
            // `Nullable<T>` in PawPrint is a box of the *underlying* `T`: `box`/`unbox`
            // special-case nullables before allocating (UnaryMetadataObjectOps), so nothing on
            // the heap ever carries a nullable MethodTable and no reader is prepared for one.
            // `ReboxToNullable` needs precisely the opposite — an object whose raw data has the
            // nullable's own layout, so that `CastHelpers.Unbox_Nullable` can write the has-value
            // flag and the payload into it. Producing an ordinary box of `T` here would satisfy
            // the allocation and then be silently corrupted by that write.
            //
            // So this is a real gap, not an unreachable guard: a guest calling `MethodInfo.Invoke`
            // on a method with a `Nullable<T>` parameter reaches it. The fix is a layout-preserving
            // boxed-Nullable heap representation, which is the same thing the "NoChecks" arm above
            // says it needs for `AsyncHelpers.AllocContinuationResultBox`; one representation would
            // serve both. Refusing loudly beats corrupting.
            match InternalTypeKind.kind ctx.BaseClassTypes concreteType with
            | InternalTypeKind.Nullable ->
                failwith
                    $"TODO: %s{operation} was asked to allocate the Nullable %s{typeInfo.Namespace}.%s{typeInfo.Name}, as RuntimeMethodHandle.ReboxToNullable does when reflection coerces an argument to a Nullable<T> parameter; PawPrint boxes the underlying value instead, so there is no object here whose raw data Unbox_Nullable could write the nullable's layout into"
            | _ -> ()

            if DumpedAssembly.isValueType ctx.BaseClassTypes state._LoadedAssemblies typeInfo then
                // Any other value type: `pMT->Allocate()` produces a box, and PawPrint's heap has
                // no representation for one that arrived here rather than through `box`. Neither
                // caller can produce this — `Delegate.InternalAlloc` asserts its argument derives
                // from `MulticastDelegate`, and `ReboxToNullable` asserts its argument is a
                // nullable — so this is the guard that catches a *third* caller appearing, rather
                // than a gap with a known consumer.
                failwith
                    $"TODO: %s{operation} was asked to allocate the value type %s{typeInfo.Namespace}.%s{typeInfo.Name}; neither Delegate.InternalAlloc nor RuntimeMethodHandle.ReboxToNullable can pass one, and PawPrint has no boxed representation for an object allocated here"

            // `MethodTable::Allocate` initialises *as if constructing*
            // (`CheckRunClassInitAsIfConstructingThrowing`, methodtable.cpp:4034), which is a
            // stronger rule than the one that governs ordinary static access: it walks the whole
            // parent chain, running each non-`beforefieldinit` ancestor's `.cctor`. This is the
            // only place in PawPrint where that rule applies, which is why the walk is open-coded
            // here rather than folded into `ensureTypeInitialised` — `loadClass` deliberately does
            // *not* initialise base types (IlMachineStateExecution.fs), and it is right not to:
            // the CLR does not run a base initialiser just because a derived type's did.
            //
            // PawPrint runs every ancestor's initialiser rather than only the non-`beforefieldinit`
            // ones, so it is uniformly more eager than CoreCLR here rather than modelling the
            // `beforefieldinit` predicate. That matches the convention `newobj` already follows and
            // `docs/divergences.md` records, and ECMA-335 II.10.5.3.2 permits an eager schedule.
            // The difference needs a `beforefieldinit` ancestor that has a `.cctor` *and* an
            // allocation through this entry point; neither caller can arrange one, since the
            // delegate hierarchy carries no static constructor at all.
            //
            // The suspension needs no re-entry marker. Nothing has been written to the result
            // handle and no managed call is outstanding, so re-entry re-runs this arm from the top;
            // ancestors already initialised answer `Executed`, so the walk resumes where it left
            // off and terminates.
            let rec initialiseWithAncestors
                (ty : ConcreteTypeHandle)
                (state : IlMachineState)
                : IlMachineState * WhatWeDid
                =
                let state, typeInit =
                    IlMachineStateExecution.ensureTypeInitialised
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        ctx.Thread
                        ty
                        state

                match typeInit with
                | WhatWeDid.Executed ->
                    let state, baseType =
                        IlMachineState.resolveBaseConcreteType ctx.LoggerFactory ctx.BaseClassTypes state ty

                    match baseType with
                    | None -> state, WhatWeDid.Executed
                    | Some baseType -> initialiseWithAncestors baseType state
                | other -> state, other

            let state, typeInit = initialiseWithAncestors typeHandle state

            match typeInit with
            | WhatWeDid.SuspendedForClassInit -> NativeHandlerResult.suspendedForClassInit state |> Some
            | WhatWeDid.BlockedOnClassInit blockedBy -> NativeHandlerResult.blockedOnClassInit blockedBy state |> Some
            | WhatWeDid.ThrowingTypeInitializationException ->
                NativeHandlerResult.throwingTypeInitializationException state |> Some
            | WhatWeDid.Aborted fatal -> NativeHandlerResult.aborted ctx.Thread fatal state |> Some
            | WhatWeDid.SuspendedForManagedCall ->
                failwith
                    $"logic error: %s{operation}: ensureTypeInitialised cannot suspend for an arbitrary managed call"
            | WhatWeDid.VoluntaryYield _ ->
                failwith $"logic error: %s{operation}: ensureTypeInitialised cannot produce a VoluntaryYield"
            | WhatWeDid.Executed ->

            let result =
                NativeCall.objectHandleOnStackTarget operation state "result" instruction.Arguments.[1]

            let addr, state =
                IlMachineState.allocateUninitialisedInstance ctx.LoggerFactory ctx.BaseClassTypes typeHandle state

            let state =
                IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state result (CliType.ObjectRef (Some addr))

            NativeHandlerResult.completed state |> Some
        | "RuntimeTypeHandle_GetActivationInfo",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetActivationInfo",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "ObjectHandleOnStack",
                                             objectHandleGenerics)
            ConcretePointer (ConcreteFunctionPointer _)
            ConcretePointer (ConcretePointer (ConcreteVoid state.ConcreteTypes))
            ConcretePointer (ConcreteFunctionPointer _)
            ConcretePointer (ConcreteFunctionPointer _)
            ConcretePointer (CorelibType state.ConcreteTypes ("", "BOOL", boolGenerics)) ],
          MethodReturnType.Void when objectHandleGenerics.IsEmpty && boolGenerics.IsEmpty ->
            // CoreCLR: `RuntimeTypeHandle_GetActivationInfo`, reflectioninvocation.cpp. Describes
            // how `RuntimeType.ActivatorCache` should activate a type via `calli`: an allocator
            // plus its first argument, a boxed-receiver constructor, an unboxed-receiver
            // constructor, and whether that constructor is public. It runs no constructor and,
            // deliberately, no class initialiser.
            //
            // Only argument 0 is an `ObjectHandleOnStack`; the other five are raw out-pointers
            // to locals in the managed shim (RuntimeHandles.cs).
            let operation = "RuntimeTypeHandle.GetActivationInfo"

            if instruction.Arguments.Length <> 6 then
                failwith $"%s{operation}: expected six native arguments, got %d{instruction.Arguments.Length}"

            let runtimeTypePtr =
                NativeCall.objectHandleOnStackTarget operation state "pRuntimeType" instruction.Arguments.[0]

            let outAllocator =
                NativeCall.managedPointerOfPointerArgument operation "ppfnAllocator" instruction.Arguments.[1]

            let outAllocatorFirstArg =
                NativeCall.managedPointerOfPointerArgument operation "pvAllocatorFirstArg" instruction.Arguments.[2]

            let outRefCtor =
                NativeCall.managedPointerOfPointerArgument operation "ppfnRefCtor" instruction.Arguments.[3]

            let outValueCtor =
                NativeCall.managedPointerOfPointerArgument operation "ppfnValueCtor" instruction.Arguments.[4]

            let outCtorIsPublic =
                NativeCall.managedPointerOfPointerArgument operation "pfCtorIsPublic" instruction.Arguments.[5]

            let target =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef
                    operation
                    state
                    (IlMachineState.readManagedByref ctx.BaseClassTypes state runtimeTypePtr
                     |> EvalStackValue.ofCliType)

            let state, activation =
                ActivationInfo.classify ctx.LoggerFactory ctx.BaseClassTypes operation target state

            // A null pointer must be written as something `NativeIntSource.isZero` agrees is
            // zero, never as a `FunctionPointer`: `ActivatorCache` decides whether to substitute
            // its no-op stubs by comparing each pointer against null, and a `FunctionPointer`
            // never compares equal to zero. `RuntimePointer` rather than a `NativeInt` because
            // these out-params address pointer-typed slots, whose own zero is this shape.
            let nullPointer =
                CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null)

            let allocatorPointer =
                CliType.Numeric (
                    CliNumericType.NativeInt (NativeIntSource.FunctionPointer FunctionPointerTarget.RuntimeAllocator)
                )

            // `Interop.BOOL` is int32-backed (`FALSE = 0`, `TRUE = 1`).
            let boolValue (b : bool) : CliType =
                CliType.Numeric (CliNumericType.Int32 (if b then 1 else 0))

            let writeAll
                (allocator : CliType)
                (allocatorFirstArg : CliType)
                (refCtor : CliType)
                (valueCtor : CliType)
                (ctorIsPublic : bool)
                (state : IlMachineState)
                : IlMachineState
                =
                let write ptr value state =
                    IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state ptr value

                state
                |> write outAllocator allocator
                |> write outAllocatorFirstArg allocatorFirstArg
                |> write outRefCtor refCtor
                |> write outValueCtor valueCtor
                |> write outCtorIsPublic (boolValue ctorIsPublic)

            // `pvAllocatorFirstArg` addresses a `void*` slot, so the MethodTable pointer goes in
            // as a `RuntimePointer`; the guest copies that slot into `ActivatorCache`'s own
            // `void*` field, and a `NativeInt`-shaped value there would force that copy down the
            // byte-image path, which a pointer cell has no byte image for.
            let methodTablePointer (handle : ConcreteTypeHandle) : CliType =
                CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr (RuntimeTypeHandleTarget.Closed handle))

            match activation with
            | ActivationInfo.Rejected rejection ->
                let exnType =
                    match rejection with
                    | ActivationRejection.Delegate -> ctx.BaseClassTypes.ArgumentException
                    | ActivationRejection.UnsupportedShape _
                    | ActivationRejection.VariableLength
                    | ActivationRejection.Interface
                    | ActivationRejection.AbstractClass
                    | ActivationRejection.NoDefaultConstructor -> ctx.BaseClassTypes.MissingMethodException

                // Nothing is written on the throwing path, matching CoreCLR: the shim's locals
                // stay at the zero the managed wrapper gave them.
                NativeHandlerResult.raiseException exnType state |> Some
            | ActivationInfo.Nullable ->
                // CoreCLR returns a null allocator for Nullable<T> and ActivatorCache
                // substitutes its own null-returning stub, which is how
                // `Activator.CreateInstance(typeof(int?))` comes back null. "No ctor call
                // needed" is reported as public.
                writeAll nullPointer nullPointer nullPointer nullPointer true state
                |> NativeHandlerResult.completed
                |> Some
            | ActivationInfo.ValueTypeWithoutConstructor methodTable ->
                // A boxed `default(T)` needs no constructor call at all, so both ctor pointers
                // are null and ActivatorCache installs its no-op stubs.
                writeAll allocatorPointer (methodTablePointer methodTable) nullPointer nullPointer true state
                |> NativeHandlerResult.completed
                |> Some
            | ActivationInfo.WithConstructor (methodTable, ctor, isPublic, isValueType) ->
                let declaringType =
                    AllConcreteTypes.lookup methodTable state.ConcreteTypes
                    |> Option.defaultWith (fun () ->
                        failwith $"%s{operation}: ConcreteTypeHandle %O{methodTable} not found in AllConcreteTypes"
                    )

                if isValueType then
                    // CoreCLR hands back the ctor's *boxed* entry point in `ppfnRefCtor` and its
                    // unboxed entry point in `ppfnValueCtor` — the same MethodDesc reached two
                    // ways. PawPrint's function pointers carry no entry-point flavour, and
                    // `CreateInstanceDefaultCtor` calls the boxed one, so we would have to invoke
                    // a value-type instance method with an ObjectRef receiver. Coercing that into
                    // a byref `this` risks constructing into a copy of the box's payload and
                    // silently discarding the result, so refuse instead.
                    let typeInfo =
                        state._LoadedAssemblies
                            .ByDefinitionName(declaringType.Identity.AssemblyFullName)
                            .TypeDefs.[declaringType.Identity.TypeDefinition.Get]

                    failwith
                        $"TODO: %s{operation} for value type %s{typeInfo.Namespace}.%s{typeInfo.Name}, which declares an explicit parameterless constructor; CoreCLR returns that ctor's boxed entry point, which PawPrint's function-pointer representation cannot express"

                let state, concretizedCtor, _declaringTypeHandle =
                    ExecutionConcretization.concretizeMethodWithAllGenerics
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        declaringType.Generics
                        ctor
                        ImmutableArray.Empty
                        state

                let refCtorPointer =
                    CliType.Numeric (
                        CliNumericType.NativeInt (
                            NativeIntSource.FunctionPointer (FunctionPointerTarget.Managed concretizedCtor)
                        )
                    )

                // A reference type has no value ctor: CoreCLR asserts `*ppfnValueCtor == NULL`
                // for one.
                writeAll allocatorPointer (methodTablePointer methodTable) refCtorPointer nullPointer isPublic state
                |> NativeHandlerResult.completed
                |> Some
        | _ -> None
