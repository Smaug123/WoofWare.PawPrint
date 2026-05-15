namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module NativeRuntimeFieldHandle =
    let internal getFieldForFieldHandle
        (operation : string)
        (fieldHandle : FieldHandle)
        (state : IlMachineState)
        : DumpedAssembly * FieldInfo<GenericParamFromMetadata, TypeDefn>
        =
        let assemblyFullName = fieldHandle.GetAssemblyFullName ()

        let assembly =
            state.LoadedAssembly' assemblyFullName
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

        let fieldDefinitionHandle = fieldHandle.GetFieldDefinitionHandle().Get

        let fieldInfo =
            match assembly.Fields.TryGetValue fieldDefinitionHandle with
            | true, fieldInfo -> fieldInfo
            | false, _ -> failwith $"%s{operation}: field %O{fieldDefinitionHandle} not found in %s{assemblyFullName}"

        assembly, fieldInfo

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

    let private getPeByteRangeForFieldHandle
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (fieldHandle : FieldHandle)
        (state : IlMachineState)
        : IlMachineState * PeByteRangePointer option
        =
        let assembly, fieldInfo = getFieldForFieldHandle operation fieldHandle state

        let declaringTypeHandle = fieldHandle.GetDeclaringTypeHandle ()

        let typeGenerics =
            match AllConcreteTypes.lookup declaringTypeHandle state.ConcreteTypes with
            | Some declaringType -> declaringType.Generics
            | None ->
                failwith
                    $"%s{operation}: declaring type handle %O{declaringTypeHandle} was not concretized, so RVA field size cannot be computed"

        IlMachineState.peByteRangeForFieldRva loggerFactory baseClassTypes assembly fieldInfo typeGenerics state

    let tryExecute (ctx : NativeCallContext) : ExecutionResult option =
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
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeFieldHandleInternal", generics) ],
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

            let _, fieldInfo = getFieldForFieldHandle operation fieldHandle state

            let namePtr, state =
                NativeCall.allocateNullTerminatedUtf8 ctx.BaseClassTypes fieldInfo.Name state

            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer namePtr) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeFieldHandle",
          "GetAttributes",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeFieldHandleInternal", generics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System.Reflection",
                                                                      "FieldAttributes",
                                                                      retGenerics)) when
            generics.IsEmpty && retGenerics.IsEmpty
            ->
            let operation = "RuntimeFieldHandle.GetAttributes"

            let fieldHandle =
                // CoreCLR exposes this as a raw FieldDesc* FCall; null handles fault here,
                // unlike QCalls such as GetRVAFieldInfo which return success/failure.
                fieldHandleOfRuntimeFieldHandleInternal operation state instruction.Arguments.[0]
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: null field handle")

            let _, fieldInfo = getFieldForFieldHandle operation fieldHandle state

            let state =
                IlMachineState.pushToEvalStack
                    (CliType.Numeric (CliNumericType.Int32 (int32 fieldInfo.Attributes)))
                    ctx.Thread
                    state

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | _ -> None

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : ExecutionResult option =
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
        | "RuntimeFieldHandle_GetRVAFieldInfo",
          "System.Private.CoreLib",
          "System",
          "RuntimeFieldHandle",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeFieldHandleInternal", generics)
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
                | None -> state |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0) ctx.Thread
                | Some fieldHandleId ->
                    match FieldHandleRegistry.resolveFieldFromId fieldHandleId state.FieldHandles with
                    | None -> state |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0) ctx.Thread
                    | Some fieldHandle ->
                        let state, peByteRange =
                            getPeByteRangeForFieldHandle
                                ctx.LoggerFactory
                                ctx.BaseClassTypes
                                operation
                                fieldHandle
                                state

                        match peByteRange with
                        | None -> state |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0) ctx.Thread
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

                            state |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 1) ctx.Thread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | _ -> None
