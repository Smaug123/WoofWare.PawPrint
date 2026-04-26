namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module NativeRuntimeFieldHandle =
    let private getRvaDataForFieldHandle
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (fieldHandle : FieldHandle)
        (state : IlMachineState)
        : IlMachineState * RvaDataPointer option
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

        let declaringTypeHandle = fieldHandle.GetDeclaringTypeHandle ()

        let typeGenerics =
            match AllConcreteTypes.lookup declaringTypeHandle state.ConcreteTypes with
            | Some declaringType -> declaringType.Generics
            | None ->
                failwith
                    $"%s{operation}: declaring type handle %O{declaringTypeHandle} was not concretized, so RVA field size cannot be computed"

        IlMachineState.rvaDataForField loggerFactory baseClassTypes assembly fieldInfo typeGenerics state

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
