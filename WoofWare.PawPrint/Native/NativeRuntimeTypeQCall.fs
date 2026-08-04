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
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              qCallGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "TypeNameFormatFlags", flagsGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
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
            | MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                          "",
                                                                          "BOOL",
                                                                          boolGenerics)) when boolGenerics.IsEmpty -> ()
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
          [ ConcretePointer (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                               "System.Runtime.CompilerServices",
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
        | "RuntimeTypeHandle_Instantiate",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "Instantiate",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              qCallGenerics)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
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

            // Stage B2: validate the special-constraint flags
            // (NotNullableValueTypeConstraint / ReferenceTypeConstraint /
            // DefaultConstructorConstraint) before instantiating. Base-type and
            // interface (`Constraints` array) requirements are not yet validated;
            // those will land in Stage B3.
            let constraintViolation =
                openGenericTypeInfoForValidation state typeHandleTarget
                |> Option.bind (fun typeInfo ->
                    validateSpecialConstraints ctx.BaseClassTypes state typeInfo genericArguments
                )

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
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              qCallGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
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

            let genericArgumentTargets : ImmutableArray<RuntimeTypeHandleTarget> =
                match typeHandleTarget with
                | RuntimeTypeHandleTarget.Closed handle ->
                    match handle with
                    | ConcreteTypeHandle.Concrete _ ->
                        let concreteType =
                            AllConcreteTypes.lookup handle state.ConcreteTypes
                            |> Option.defaultWith (fun () ->
                                failwith $"%s{operation}: concrete type handle was not registered: %O{handle}"
                            )

                        concreteType.Generics
                        |> Seq.map RuntimeTypeHandleTarget.Closed
                        |> ImmutableArray.CreateRange
                    | ConcreteTypeHandle.Byref _
                    | ConcreteTypeHandle.Pointer _
                    | ConcreteTypeHandle.FunctionPointer _
                    | ConcreteTypeHandle.OneDimArrayZero _
                    | ConcreteTypeHandle.Array _ ->
                        // Real .NET strips array/byref/pointer wrappers via GetRootElementType
                        // before reaching this QCall, but be defensive: these wrappers carry
                        // no generic instantiation of their own.
                        ImmutableArray.Empty
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                    // Real .NET returns Type[] { typeof(T), ... } where each T is a generic
                    // type parameter. We surface each parameter as a RuntimeType backed by a
                    // GenericParameter target.
                    let assembly =
                        state.LoadedAssembly identity.Assembly
                        |> Option.defaultWith (fun () ->
                            failwith
                                $"%s{operation}: assembly for open generic type definition is not loaded: %s{identity.AssemblyFullName}"
                        )

                    let typeInfo = assembly.TypeDefs.[identity.TypeDefinition.Get]

                    if typeInfo.Generics.IsEmpty then
                        failwith
                            $"%s{operation}: open generic type definition %O{identity} declares no generic parameters"

                    Seq.init
                        typeInfo.Generics.Length
                        (fun position -> RuntimeTypeHandleTarget.GenericParameter (identity, position))
                    |> ImmutableArray.CreateRange
                | RuntimeTypeHandleTarget.GenericParameter _
                | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
                    // GetInstantiation on a generic parameter T returns Type.EmptyTypes in CoreCLR,
                    // because a parameter has no instantiation of its own.
                    ImmutableArray.Empty

            // Empty: leave the caller's local null. RuntimeType.GetGenericArguments handles
            // null via `?? EmptyTypes`, matching native CopyRuntimeTypeHandles for 0 args.
            if genericArgumentTargets.IsEmpty then
                NativeHandlerResult.completed state |> Some
            else
                let elementTypeName = if asRuntimeTypeArray then "RuntimeType" else "Type"

                let state, _, elementTypeHandle =
                    concretizeNonGenericCorelibType ctx.LoggerFactory ctx.BaseClassTypes state "System" elementTypeName

                let arrayAddr, state =
                    IlMachineState.allocateArray
                        (ConcreteTypeHandle.OneDimArrayZero elementTypeHandle)
                        (fun () -> CliType.ObjectRef None)
                        genericArgumentTargets.Length
                        state

                let state =
                    ((state, 0), genericArgumentTargets)
                    ||> Seq.fold (fun (state, index) target ->
                        let runtimeTypeAddr, state =
                            IlMachineState.getOrAllocateType ctx.LoggerFactory ctx.BaseClassTypes target state

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
                        retTypes
                        (CliType.ObjectRef (Some arrayAddr))

                NativeHandlerResult.completed state |> Some
        | "RuntimeTypeHandle_GetConstraints",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetConstraints",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              qCallGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
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
            | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
                let assembly =
                    state.LoadedAssembly declaringType.Assembly
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"%s{operation}: assembly for declaring type of generic parameter is not loaded: %s{declaringType.AssemblyFullName}"
                    )

                let typeInfo = assembly.TypeDefs.[declaringType.TypeDefinition.Get]

                if position < 0 || position >= typeInfo.Generics.Length then
                    failwith
                        $"%s{operation}: generic parameter position %d{position} is out of range for %O{declaringType.TypeDefinition.Get} (declares %d{typeInfo.Generics.Length} parameters)"

                let _, metadata = typeInfo.Generics.[position]

                // Detect constraints that *embed* a generic parameter inside a structural shape
                // (e.g. `where T : IEnumerable<T>` decoded as `GenericInstantiation(IEnumerable,
                // [GenericTypeParameter 0])`). Concretizing such a shape would require binding
                // parameters to parameter targets, which our concretization machinery doesn't
                // model. Detect up front and fail with a pointed TODO rather than letting
                // concretizeType raise IndexOutOfRangeException from deep in the resolver.
                let rec embedsTypeParameter (ty : TypeDefn) : bool =
                    match ty with
                    | TypeDefn.GenericTypeParameter _
                    | TypeDefn.GenericMethodParameter _ -> true
                    | TypeDefn.Array (element, _)
                    | TypeDefn.Pinned element
                    | TypeDefn.Pointer element
                    | TypeDefn.Byref element
                    | TypeDefn.OneDimensionalArrayLowerBoundZero element -> embedsTypeParameter element
                    | TypeDefn.Modified (original, modifier, _) ->
                        embedsTypeParameter original || embedsTypeParameter modifier
                    | TypeDefn.GenericInstantiation (generic, args) ->
                        embedsTypeParameter generic || (args |> Seq.exists embedsTypeParameter)
                    | TypeDefn.FunctionPointer signature ->
                        let returnContains =
                            match signature.ReturnType with
                            | MethodReturnType.Void -> false
                            | MethodReturnType.Returns ret -> embedsTypeParameter ret

                        returnContains || (signature.ParameterTypes |> List.exists embedsTypeParameter)
                    | TypeDefn.PrimitiveType _
                    | TypeDefn.FromReference _
                    | TypeDefn.FromDefinition _
                    | TypeDefn.Void -> false

                // Closed (non-parameter) constraints are concretized against the declaring
                // assembly with no generic context: a constraint like `where T : List<int>`
                // resolves to the closed type. Constraints that reference another type-generic
                // parameter (e.g. `where T2 : T1`) are surfaced as parameter targets directly,
                // because concretizeType cannot bind a parameter back to a parameter target.
                let baseTargets, state =
                    ((List.empty, state), metadata.Constraints)
                    ||> Seq.fold (fun (acc, state) ty ->
                        match ty with
                        | TypeDefn.GenericTypeParameter idx ->
                            let target = RuntimeTypeHandleTarget.GenericParameter (declaringType, idx)
                            target :: acc, state
                        | TypeDefn.GenericMethodParameter idx ->
                            failwith
                                $"%s{operation}: type-generic parameter #%d{position} of %O{declaringType.TypeDefinition.Get} declares a method-generic parameter constraint !!%d{idx}; impossible without a method context"
                        | _ when embedsTypeParameter ty ->
                            failwith
                                $"TODO: %s{operation}: constraint %O{ty} on type-generic parameter #%d{position} of %O{declaringType.TypeDefinition.Get} embeds a generic-parameter reference; concretization needs to bind parameters to parameter targets"
                        | _ ->
                            let state, handle =
                                IlMachineState.concretizeType
                                    ctx.LoggerFactory
                                    ctx.BaseClassTypes
                                    state
                                    assembly.Name
                                    ImmutableArray.Empty
                                    ImmutableArray.Empty
                                    ty

                            RuntimeTypeHandleTarget.Closed handle :: acc, state
                    )

                let baseTargets = List.rev baseTargets

                // GenericParameter.fs filters out the synthetic System.ValueType row that Roslyn
                // emits alongside the NotNullableValueTypeConstraint flag for `where T : struct`,
                // but only the TypeRef/TypeDef forms — a `where T : unmanaged` constraint encodes
                // ValueType as a TypeSpec wrapped in an `IsUnmanaged` modreq, which the filter
                // doesn't recognise. Append the synthetic row only when no existing entry already
                // resolves to System.ValueType, matching reflection's behaviour of returning
                // exactly one ValueType for both `struct` and `unmanaged` constraints.
                let constraintTargets, state =
                    match metadata.Constraint with
                    | Some GenericConstraint.NonNullableValue ->
                        let state, _, valueTypeHandle =
                            concretizeNonGenericCorelibType
                                ctx.LoggerFactory
                                ctx.BaseClassTypes
                                state
                                "System"
                                "ValueType"

                        let alreadyHasValueType =
                            baseTargets
                            |> List.exists (fun t ->
                                match t with
                                | RuntimeTypeHandleTarget.Closed h -> h = valueTypeHandle
                                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _
                                | RuntimeTypeHandleTarget.GenericParameter _
                                | RuntimeTypeHandleTarget.MethodGenericParameter _ -> false
                            )

                        if alreadyHasValueType then
                            baseTargets, state
                        else
                            baseTargets @ [ RuntimeTypeHandleTarget.Closed valueTypeHandle ], state
                    | Some GenericConstraint.Reference
                    | None -> baseTargets, state

                if List.isEmpty constraintTargets then
                    // CopyRuntimeTypeHandles writes NULL when count = 0; the managed wrapper turns
                    // the resulting null into Type.EmptyTypes via `?? EmptyTypes`. Leave the
                    // caller's local null untouched.
                    NativeHandlerResult.completed state |> Some
                else
                    // CopyRuntimeTypeHandles allocates Type[] (CLASS__TYPE) — not RuntimeType[].
                    let state, _, typeHandle =
                        concretizeNonGenericCorelibType ctx.LoggerFactory ctx.BaseClassTypes state "System" "Type"

                    let arrayAddr, state =
                        IlMachineState.allocateArray
                            (ConcreteTypeHandle.OneDimArrayZero typeHandle)
                            (fun () -> CliType.ObjectRef None)
                            (List.length constraintTargets)
                            state

                    let state =
                        ((state, 0), constraintTargets)
                        ||> List.fold (fun (state, index) target ->
                            let runtimeTypeAddr, state =
                                IlMachineState.getOrAllocateType ctx.LoggerFactory ctx.BaseClassTypes target state

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
                            retTypes
                            (CliType.ObjectRef (Some arrayAddr))

                    NativeHandlerResult.completed state |> Some

            | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
                failwith
                    $"TODO: %s{operation} for method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}"
            | RuntimeTypeHandleTarget.Closed _
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ ->
                // CoreCLR's QCall throws ArgumentException for non-generic-variable arguments,
                // but the only managed caller (RuntimeType.GetGenericParameterConstraints) gates
                // on IsGenericParameter, so we should never reach this branch in practice. Fail
                // loudly rather than silently writing Type.EmptyTypes, which would mask a bug.
                failwith $"%s{operation}: expected a generic-parameter type handle, got %O{typeHandleTarget}"
        | "RuntimeTypeHandle_CreateInstanceForAnotherGenericParameter",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "CreateInstanceForAnotherGenericParameter",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              qCallGenerics)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
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
                | WhatWeDid.SuspendedForManagedCall ->
                    failwith "logic error: ensureTypeInitialised cannot suspend for an arbitrary managed call"
                | WhatWeDid.VoluntaryYield ->
                    failwith "logic error: ensureTypeInitialised cannot produce a VoluntaryYield"
                | WhatWeDid.Executed ->

                let concreteType =
                    AllConcreteTypes.lookup instantiatedHandle state.ConcreteTypes
                    |> Option.defaultWith (fun () ->
                        failwith $"%s{operation}: instantiated handle was not registered: %O{instantiatedHandle}"
                    )

                let assembly =
                    state.LoadedAssembly concreteType.Assembly
                    |> Option.defaultWith (fun () ->
                        failwith $"%s{operation}: assembly is not loaded: %s{concreteType.Assembly.FullName}"
                    )

                let typeInfo = assembly.TypeDefs.[concreteType.Definition.Get]

                if DumpedAssembly.isValueType ctx.BaseClassTypes state._LoadedAssemblies typeInfo then
                    // CoreCLR's QCall asserts !pVMT->IsByRefLike() and routes value types
                    // away from this path elsewhere; the only documented consumer
                    // (ArraySortHelper) instantiates reference types. If a value-type ever
                    // reaches us, calling the parameterless ctor with `this`-as-ObjectRef
                    // would silently boxsem the receiver, so reject it explicitly.
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
                    |> List.tryFind (fun m -> m.Name = ".ctor" && not m.IsStatic && m.Parameters.IsEmpty)
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
                        ConstructedObjectDisposition.PushToCaller
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
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                    let assembly =
                        state.LoadedAssembly identity.Assembly
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
                            state.LoadedAssembly concreteType.Assembly
                            |> Option.defaultWith (fun () ->
                                failwith
                                    $"%s{operation}: assembly for concrete type is not loaded: %s{concreteType.Assembly.FullName}"
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
                        state.LoadedAssembly declaringType.Assembly
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
                                typeInfo.Assembly
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
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              qCallGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
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
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallModule",
                                              qCallModuleGenerics)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
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
                state.LoadedAssembly' assemblyFullName
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
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallModule",
                                              qCallModuleGenerics)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System",
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
                state.LoadedAssembly' assemblyFullName
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

                    if method.DeclaringType.Generics.Length > 0 then
                        failwith
                            $"TODO: %s{operation}: MethodDef token 0x%08x{methodToken} declared on generic type %s{method.DeclaringType.Namespace}.%s{method.DeclaringType.Name} (%d{method.DeclaringType.Generics.Length} type generic parameter(s)); CoreCLR returns the open metadata definition without consuming the caller's typeInstantiation, but the MethodHandle registry only supports fully concretised methods."

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
                            methodInstantiation
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
                                    method.DeclaringType.Assembly
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
          [ ConcretePointer (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                               "System.Runtime.CompilerServices",
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
            | MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                          "",
                                                                          "BOOL",
                                                                          boolGenerics)) when boolGenerics.IsEmpty -> ()
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
                        identity.Assembly
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
          [ ConcretePointer (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                               "System.Runtime.CompilerServices",
                                                               "MethodTable",
                                                               methodTableGenerics))
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
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
                                state.LoadedAssembly' currentCt.Identity.AssemblyFullName
                                |> Option.defaultWith (fun () ->
                                    failwith
                                        $"%s{operation}: owning assembly %s{currentCt.Identity.AssemblyFullName} not loaded"
                                )

                            ((state, seen, ordered), currentTypeInfo.ImplementedInterfaces)
                            ||> Seq.fold (fun (state, seen, ordered) impl ->
                                let implAssy =
                                    state.LoadedAssembly impl.RelativeToAssembly |> Option.defaultValue currentAssy

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
                                        implResolvedAssy.Name
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
                            state.LoadedAssembly identity.Assembly
                            |> Option.defaultWith (fun () ->
                                failwith
                                    $"%s{operation}: assembly %s{identity.AssemblyFullName} not loaded for open generic typedef %O{identity.TypeDefinition.Get}"
                            )

                        let currentTypeInfo = currentAssy.TypeDefs.[identity.TypeDefinition.Get]

                        ((state, seen, ordered), currentTypeInfo.ImplementedInterfaces)
                        ||> Seq.fold (fun (state, seen, ordered) impl ->
                            let implAssy =
                                state.LoadedAssembly impl.RelativeToAssembly |> Option.defaultValue currentAssy

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
                                        implResolvedAssy.Name
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
        | _ -> None
