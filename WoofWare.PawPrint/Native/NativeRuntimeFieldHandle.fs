namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module NativeRuntimeFieldHandle =
    let private fieldDefinitionToken (handle : System.Reflection.Metadata.FieldDefinitionHandle) : int32 =
        let handle : System.Reflection.Metadata.EntityHandle =
            System.Reflection.Metadata.FieldDefinitionHandle.op_Implicit handle

        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken handle

    let getFieldForFieldHandle
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

    let fieldHandleOfRuntimeFieldHandleInternal
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

    let private allocateNullTerminatedUtf8
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (value : string)
        (state : IlMachineState)
        : ManagedPointerSource * IlMachineState
        =
        let byteHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Byte

        let bytes = System.Text.Encoding.UTF8.GetBytes value
        let storage = Array.zeroCreate<byte> (bytes.Length + 1)
        Array.blit bytes 0 storage 0 bytes.Length

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero byteHandle)
                (fun () -> CliType.Numeric (CliNumericType.UInt8 0uy))
                storage.Length
                state

        let state =
            ((state, 0), storage)
            ||> Array.fold (fun (state, index) b ->
                IlMachineState.setArrayValue arrayAddr (CliType.Numeric (CliNumericType.UInt8 b)) index state,
                index + 1
            )
            |> fst

        ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), []), state

    let private getRvaDataForFieldHandle
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (fieldHandle : FieldHandle)
        (state : IlMachineState)
        : IlMachineState * RvaDataPointer option
        =
        let assembly, fieldInfo = getFieldForFieldHandle operation fieldHandle state

        let declaringTypeHandle = fieldHandle.GetDeclaringTypeHandle ()

        let typeGenerics =
            match AllConcreteTypes.lookup declaringTypeHandle state.ConcreteTypes with
            | Some declaringType -> declaringType.Generics
            | None ->
                failwith
                    $"%s{operation}: declaring type handle %O{declaringTypeHandle} was not concretized, so RVA field size cannot be computed"

        IlMachineState.rvaDataForField loggerFactory baseClassTypes assembly fieldInfo typeGenerics state

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
                fieldHandleOfRuntimeFieldHandleInternal operation state instruction.Arguments.[0]
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: null field handle")

            let _, fieldInfo = getFieldForFieldHandle operation fieldHandle state
            let attrs = int32 fieldInfo.Attributes

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 attrs)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeFieldHandle",
          "GetUtf8NameInternal",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeFieldHandleInternal", generics) ],
          MethodReturnType.Returns (ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)) when
            generics.IsEmpty
            ->
            let operation = "RuntimeFieldHandle.GetUtf8NameInternal"

            let fieldHandle =
                fieldHandleOfRuntimeFieldHandleInternal operation state instruction.Arguments.[0]
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: null field handle")

            let _, fieldInfo = getFieldForFieldHandle operation fieldHandle state

            let namePtr, state =
                allocateNullTerminatedUtf8 ctx.BaseClassTypes fieldInfo.Name state

            let state =
                // CoreCLR returns a byte* into runtime metadata. PawPrint has no immortal native
                // metadata block yet, so expose deterministic managed byte storage through the
                // runtime-pointer byte-view path used by the BCL caller.
                IlMachineState.pushToEvalStack
                    (CliType.RuntimePointer (CliRuntimePointer.Managed namePtr))
                    ctx.Thread
                    state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeFieldHandle",
          "GetApproxDeclaringMethodTable",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeFieldHandleInternal", generics) ],
          MethodReturnType.Returns (ConcretePointer (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                                       "System.Runtime.CompilerServices",
                                                                                       "MethodTable",
                                                                                       methodTableGenerics))) when
            generics.IsEmpty && methodTableGenerics.IsEmpty
            ->
            let operation = "RuntimeFieldHandle.GetApproxDeclaringMethodTable"

            let fieldHandle =
                fieldHandleOfRuntimeFieldHandleInternal operation state instruction.Arguments.[0]
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: null field handle")

            let declaringType = fieldHandle.GetDeclaringTypeHandle ()

            let state =
                IlMachineState.pushToEvalStack'
                    (EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr declaringType))
                    ctx.Thread
                    state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeFieldHandle",
          "GetToken",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeFieldHandleInternal", generics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when generics.IsEmpty ->
            let operation = "RuntimeFieldHandle.GetToken"

            let fieldHandle =
                fieldHandleOfRuntimeFieldHandleInternal operation state instruction.Arguments.[0]
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: null field handle")

            let token = fieldDefinitionToken (fieldHandle.GetFieldDefinitionHandle().Get)

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 token)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
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
                        let state, rvaData =
                            getRvaDataForFieldHandle ctx.LoggerFactory ctx.BaseClassTypes operation fieldHandle state

                        match rvaData with
                        | None -> state |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0) ctx.Thread
                        | Some rvaData ->
                            let state, dataPtr =
                                IlMachineState.rvaBytePointer ctx.LoggerFactory ctx.BaseClassTypes rvaData state

                            let state =
                                IlMachineState.writeManagedByref
                                    state
                                    addressOut
                                    (CliType.RuntimePointer (CliRuntimePointer.Managed dataPtr))

                            let state =
                                IlMachineState.writeManagedByref
                                    state
                                    sizeOut
                                    (NativeCall.cliUInt32 (uint32 rvaData.Size))

                            state |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 1) ctx.Thread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None
