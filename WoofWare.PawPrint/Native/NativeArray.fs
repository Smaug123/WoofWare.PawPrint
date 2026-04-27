namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeArray =
    let private int32OfCliType (operation : string) (argName : string) (arg : CliType) : int =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.Int32 i) -> i
        | other -> failwith $"%s{operation}: expected %s{argName} as Int32, got %O{other}"

    let private readInt32Pointer
        (operation : string)
        (state : IlMachineState)
        (argName : string)
        (ptr : ManagedPointerSource)
        : int
        =
        match ptr with
        | ManagedPointerSource.Null -> failwith $"%s{operation}: expected non-null %s{argName} pointer"
        | ManagedPointerSource.Byref _ -> IlMachineState.readManagedByref state ptr |> int32OfCliType operation argName

    let private arrayTypeForCreateInstance
        (operation : string)
        (fromArrayType : bool)
        (rank : int)
        (target : RuntimeTypeHandleTarget)
        : ConcreteTypeHandle * ConcreteTypeHandle
        =
        match target with
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            failwith $"TODO: %s{operation} for open generic type definition %O{identity}"
        | RuntimeTypeHandleTarget.Closed typeHandle ->
            if fromArrayType then
                match typeHandle with
                | ConcreteTypeHandle.OneDimArrayZero element when rank = 1 -> typeHandle, element
                | ConcreteTypeHandle.Array (element, arrayRank) when rank = arrayRank ->
                    failwith
                        $"TODO: %s{operation} from multidimensional array type %O{typeHandle}; PawPrint array allocation currently models one length"
                | ConcreteTypeHandle.Array _ ->
                    failwith $"%s{operation}: requested rank %d{rank} does not match array type %O{typeHandle}"
                | other -> failwith $"%s{operation}: fromArrayType=true expected array RuntimeType, got %O{other}"
            else if rank = 1 then
                ConcreteTypeHandle.OneDimArrayZero typeHandle, typeHandle
            else
                failwith
                    $"TODO: %s{operation} for rank %d{rank}; PawPrint array allocation currently models rank-1 zero-lower-bound arrays"

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
        | "Array_CreateInstance",
          "System.Private.CoreLib",
          "System",
          "Array",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              qCallGenerics)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              objectHandleGenerics) ],
          MethodReturnType.Void when qCallGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            let operation = "Array.CreateInstance"

            if instruction.Arguments.Length <> 6 then
                failwith $"%s{operation}: expected six native arguments, got %d{instruction.Arguments.Length}"

            let typeHandle =
                NativeCall.qCallTypeHandleToRuntimeTypeHandleTarget
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let rank = int32OfCliType operation "rank" instruction.Arguments.[1]

            let lengths =
                NativeCall.managedPointerOfPointerArgument operation "lengths" instruction.Arguments.[2]

            let lowerBounds =
                NativeCall.managedPointerOfPointerArgument operation "lowerBounds" instruction.Arguments.[3]

            let fromArrayType =
                match int32OfCliType operation "fromArrayType" instruction.Arguments.[4] with
                | 0 -> false
                | _ -> true

            let retArray =
                NativeCall.objectHandleOnStackTarget operation state "retArray" instruction.Arguments.[5]

            let arrayLength = readInt32Pointer operation state "lengths[0]" lengths

            if arrayLength < 0 then
                failwith "TODO: Array.CreateInstance with negative length should throw ArgumentOutOfRangeException"

            match lowerBounds with
            | ManagedPointerSource.Null -> ()
            | ManagedPointerSource.Byref _ ->
                let lowerBound = readInt32Pointer operation state "lowerBounds[0]" lowerBounds

                if lowerBound <> 0 then
                    failwith
                        $"TODO: %s{operation} with non-zero lower bound %d{lowerBound}; PawPrint only models SZ arrays here"

            let arrayType, elementType =
                arrayTypeForCreateInstance operation fromArrayType rank typeHandle

            let zero, state =
                IlMachineState.cliTypeZeroOfHandle state ctx.BaseClassTypes elementType

            let arrayAddr, state =
                IlMachineState.allocateArray arrayType (fun () -> zero) arrayLength state

            let state =
                IlMachineState.writeManagedByref state retArray (CliType.ObjectRef (Some arrayAddr))

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None
