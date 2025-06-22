namespace WoofWare.PawPrint

#nowarn "42"

open Microsoft.Extensions.Logging

type private IArithmeticOperation =
    abstract Int32Int32 : int32 -> int32 -> int32
    abstract Int64Int64 : int64 -> int64 -> int64
    abstract FloatFloat : float -> float -> float
    abstract Name : string

[<RequireQualifiedAccess>]
module private ArithmeticOperation =
    let add =
        { new IArithmeticOperation with
            member _.Int32Int32 a b = (# "add" a b : int32 #)
            member _.Int64Int64 a b = (# "add" a b : int64 #)
            member _.FloatFloat a b = (# "add" a b : float #)
            member _.Name = "add"
        }

    let mul =
        { new IArithmeticOperation with
            member _.Int32Int32 a b = (# "mul" a b : int32 #)
            member _.Int64Int64 a b = (# "mul" a b : int64 #)
            member _.FloatFloat a b = (# "mul" a b : float #)
            member _.Name = "mul"
        }

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NullaryIlOp =
    type private LdindTargetType =
        | LdindI
        | LdindI1
        | LdindI2
        | LdindI4
        | LdindI8
        | LdindU1
        | LdindU2
        | LdindU4
        | LdindU8
        | LdindR4
        | LdindR8

    // Helper to get the target CliType for each Ldind variant
    let private getTargetLdindCliType (targetType : LdindTargetType) : CliType =
        match targetType with
        | LdindI -> CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
        | LdindI1 -> CliType.Numeric (CliNumericType.Int8 0y)
        | LdindI2 -> CliType.Numeric (CliNumericType.Int16 0s)
        | LdindI4 -> CliType.Numeric (CliNumericType.Int32 0)
        | LdindI8 -> CliType.Numeric (CliNumericType.Int64 0L)
        | LdindU1 -> CliType.Numeric (CliNumericType.UInt8 0uy)
        | LdindU2 -> CliType.Numeric (CliNumericType.UInt16 0us)
        | LdindU4 ->
            // This doesn't actually exist as a CLI type
            CliType.Numeric (CliNumericType.Int32 0)
        | LdindU8 ->
            // This doesn't actually exist as a CLI type
            CliType.Numeric (CliNumericType.Int64 0L)
        | LdindR4 -> CliType.Numeric (CliNumericType.Float32 0.0f)
        | LdindR8 -> CliType.Numeric (CliNumericType.Float64 0.0)

    /// Retrieve a value from a pointer
    let private loadFromPointerSource (state : IlMachineState) (src : ManagedPointerSource) : CliType =
        match src with
        | ManagedPointerSource.Null -> failwith "unexpected null pointer in Ldind operation"
        | ManagedPointerSource.Argument (sourceThread, methodFrame, whichVar) ->
            state.ThreadState.[sourceThread].MethodStates.[methodFrame].Arguments.[int<uint16> whichVar]
        | ManagedPointerSource.LocalVariable (sourceThread, methodFrame, whichVar) ->
            state.ThreadState.[sourceThread].MethodStates.[methodFrame].LocalVariables.[int<uint16> whichVar]
        | ManagedPointerSource.Heap managedHeapAddress -> failwith "TODO: Heap pointer dereferencing not implemented"

    // Unified Ldind implementation
    let private executeLdind
        (targetType : LdindTargetType)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : ExecutionResult
        =
        let popped, state = IlMachineState.popEvalStack currentThread state

        let loadedValue =
            match popped with
            | EvalStackValue.ManagedPointer src -> loadFromPointerSource state src
            | EvalStackValue.NativeInt nativeIntSource ->
                failwith $"TODO: Native int pointer dereferencing not implemented for {targetType}"
            | EvalStackValue.ObjectRef managedHeapAddress ->
                failwith "TODO: Object reference dereferencing not implemented"
            | other -> failwith $"Unexpected eval stack value for Ldind operation: {other}"

        let loadedValue = loadedValue |> EvalStackValue.ofCliType

        let targetCliType = getTargetLdindCliType targetType
        let coercedValue = EvalStackValue.toCliTypeCoerced targetCliType loadedValue

        let state =
            state
            |> IlMachineState.pushToEvalStack coercedValue currentThread
            |> IlMachineState.advanceProgramCounter currentThread

        (state, WhatWeDid.Executed) |> ExecutionResult.Stepped

    let private binaryArithmeticOperation
        (op : IArithmeticOperation)
        (currentThread : ThreadId)
        (state : IlMachineState)
        =
        let val1, state = IlMachineState.popEvalStack currentThread state
        let val2, state = IlMachineState.popEvalStack currentThread state
        // see table at https://learn.microsoft.com/en-us/dotnet/api/system.reflection.emit.opcodes.add?view=net-9.0
        let result =
            match val1, val2 with
            | EvalStackValue.Int32 val1, EvalStackValue.Int32 val2 ->
                (# "add" val1 val2 : int32 #) |> EvalStackValue.Int32
            | EvalStackValue.Int32 val1, EvalStackValue.NativeInt val2 -> failwith "" |> EvalStackValue.NativeInt
            | EvalStackValue.Int32 val1, EvalStackValue.ManagedPointer val2 ->
                failwith "" |> EvalStackValue.ManagedPointer
            | EvalStackValue.Int32 val1, EvalStackValue.ObjectRef val2 -> failwith "" |> EvalStackValue.ObjectRef
            | EvalStackValue.Int64 val1, EvalStackValue.Int64 val2 ->
                (# "add" val1 val2 : int64 #) |> EvalStackValue.Int64
            | EvalStackValue.NativeInt val1, EvalStackValue.Int32 val2 -> failwith "" |> EvalStackValue.NativeInt
            | EvalStackValue.NativeInt val1, EvalStackValue.NativeInt val2 -> failwith "" |> EvalStackValue.NativeInt
            | EvalStackValue.NativeInt val1, EvalStackValue.ManagedPointer val2 ->
                failwith "" |> EvalStackValue.ManagedPointer
            | EvalStackValue.NativeInt val1, EvalStackValue.ObjectRef val2 -> failwith "" |> EvalStackValue.ObjectRef
            | EvalStackValue.Float val1, EvalStackValue.Float val2 ->
                (# "add" val1 val2 : float #) |> EvalStackValue.Float
            | EvalStackValue.ManagedPointer val1, EvalStackValue.NativeInt val2 ->
                failwith "" |> EvalStackValue.ManagedPointer
            | EvalStackValue.ObjectRef val1, EvalStackValue.NativeInt val2 -> failwith "" |> EvalStackValue.ObjectRef
            | EvalStackValue.ManagedPointer val1, EvalStackValue.Int32 val2 ->
                failwith "" |> EvalStackValue.ManagedPointer
            | EvalStackValue.ObjectRef val1, EvalStackValue.Int32 val2 -> failwith "" |> EvalStackValue.ObjectRef
            | val1, val2 -> failwith $"invalid %s{op.Name} operation: {val1} and {val2}"

        result, state

    let private stind (varType : CliType) (currentThread : ThreadId) (state : IlMachineState) : IlMachineState =
        // TODO: throw NullReferenceException if unaligned target
        let valueToStore, state = IlMachineState.popEvalStack currentThread state
        let addr, state = IlMachineState.popEvalStack currentThread state

        match addr with
        | EvalStackValue.Int32 _
        | EvalStackValue.Int64 _
        | EvalStackValue.UserDefinedValueType _
        | EvalStackValue.Float _ -> failwith $"unexpectedly tried to store value {valueToStore} in a non-address {addr}"
        | EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
        | EvalStackValue.ManagedPointer src ->
            match src with
            | ManagedPointerSource.Null -> failwith "TODO: throw NullReferenceException"
            | ManagedPointerSource.Argument (sourceThread, methodFrame, whichVar) ->
                failwith "unexpected - can we really write to an argument?"
            | ManagedPointerSource.LocalVariable (sourceThread, methodFrame, whichVar) ->
                { state with
                    ThreadState =
                        state.ThreadState
                        |> Map.change
                            sourceThread
                            (fun state ->
                                match state with
                                | None -> failwith "tried to store in local variables of nonexistent stack frame"
                                | Some state ->
                                    let frame = state.MethodStates.[methodFrame]

                                    let frame =
                                        { frame with
                                            LocalVariables =
                                                frame.LocalVariables.SetItem (
                                                    int<uint16> whichVar,
                                                    EvalStackValue.toCliTypeCoerced varType valueToStore
                                                )
                                        }

                                    { state with
                                        MethodStates = state.MethodStates.SetItem (methodFrame, frame)
                                    }
                                    |> Some
                            )
                }
            | ManagedPointerSource.Heap managedHeapAddress -> failwith "todo"
        | EvalStackValue.ObjectRef managedHeapAddress -> failwith "todo"

    let internal execute
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentThread : ThreadId)
        (op : NullaryIlOp)
        : ExecutionResult
        =
        match op with
        | Nop ->
            (IlMachineState.advanceProgramCounter currentThread state, WhatWeDid.Executed)
            |> ExecutionResult.Stepped
        | LdArg0 ->
            state
            |> IlMachineState.loadArgument currentThread 0
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdArg1 ->
            state
            |> IlMachineState.loadArgument currentThread 1
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdArg2 ->
            state
            |> IlMachineState.loadArgument currentThread 2
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdArg3 ->
            state
            |> IlMachineState.loadArgument currentThread 3
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Ldloc_0 ->
            let localVar = state.ThreadState.[currentThread].MethodState.LocalVariables.[0]

            state
            |> IlMachineState.pushToEvalStack localVar currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Ldloc_1 ->
            let localVar = state.ThreadState.[currentThread].MethodState.LocalVariables.[1]

            state
            |> IlMachineState.pushToEvalStack localVar currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Ldloc_2 ->
            let localVar = state.ThreadState.[currentThread].MethodState.LocalVariables.[2]

            state
            |> IlMachineState.pushToEvalStack localVar currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Ldloc_3 ->
            let localVar = state.ThreadState.[currentThread].MethodState.LocalVariables.[3]

            state
            |> IlMachineState.pushToEvalStack localVar currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Pop ->
            IlMachineState.popEvalStack currentThread state
            |> snd
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Dup ->
            let topValue =
                match IlMachineState.peekEvalStack currentThread state with
                | None -> failwith "tried to Dup when nothing on top of stack"
                | Some v -> v

            state
            |> IlMachineState.pushToEvalStack' topValue currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Ret ->
            match IlMachineState.returnStackFrame loggerFactory corelib currentThread state with
            | None -> ExecutionResult.Terminated (state, currentThread)
            | Some state -> (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
        | LdcI4_0 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 0)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_1 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 1)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_2 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 2)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_3 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 3)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_4 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 4)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_5 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 5)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_6 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 6)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_7 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 7)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_8 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 8)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_m1 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 -1)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdNull ->
            let state =
                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.ManagedPointer ManagedPointerSource.Null)
                    currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
        | Ceq ->
            let var2, state = state |> IlMachineState.popEvalStack currentThread
            let var1, state = state |> IlMachineState.popEvalStack currentThread

            let comparisonResult =
                // Table III.4
                match var1, var2 with
                | EvalStackValue.Int32 var1, EvalStackValue.Int32 var2 -> if var1 = var2 then 1 else 0
                | EvalStackValue.Int32 var1, EvalStackValue.NativeInt var2 -> failwith "TODO: int32 CEQ nativeint"
                | EvalStackValue.Int32 _, _ -> failwith $"bad ceq: Int32 vs {var2}"
                | EvalStackValue.Int64 var1, EvalStackValue.Int64 var2 -> if var1 = var2 then 1 else 0
                | EvalStackValue.Int64 _, _ -> failwith $"bad ceq: Int64 vs {var2}"
                | EvalStackValue.Float var1, EvalStackValue.Float var2 -> failwith "TODO: float CEQ float"
                | EvalStackValue.Float _, _ -> failwith $"bad ceq: Float vs {var2}"
                | EvalStackValue.NativeInt var1, EvalStackValue.NativeInt var2 ->
                    match var1, var2 with
                    | NativeIntSource.FunctionPointer f1, NativeIntSource.FunctionPointer f2 ->
                        if f1 = f2 then
                            1
                        else
                            failwith $"TODO(CEQ): nativeint vs nativeint, {f1} vs {f2}"
                    | NativeIntSource.TypeHandlePtr f1, NativeIntSource.TypeHandlePtr f2 -> if f1 = f2 then 1 else 0
                    | NativeIntSource.Verbatim f1, NativeIntSource.Verbatim f2 -> if f1 = f2 then 1 else 0
                    | NativeIntSource.ManagedPointer f1, NativeIntSource.ManagedPointer f2 -> if f1 = f2 then 1 else 0
                    | _, _ -> failwith $"TODO (CEQ): nativeint vs nativeint, {var1} vs {var2}"
                | EvalStackValue.NativeInt var1, EvalStackValue.Int32 var2 -> failwith $"TODO (CEQ): nativeint vs int32"
                | EvalStackValue.NativeInt var1, EvalStackValue.ManagedPointer var2 ->
                    failwith $"TODO (CEQ): nativeint vs managed pointer"
                | EvalStackValue.NativeInt _, _ -> failwith $"bad ceq: NativeInt vs {var2}"
                | EvalStackValue.ObjectRef var1, EvalStackValue.ObjectRef var2 -> if var1 = var2 then 1 else 0
                | EvalStackValue.ObjectRef _, _ -> failwith $"bad ceq: ObjectRef vs {var2}"
                | EvalStackValue.ManagedPointer var1, EvalStackValue.ManagedPointer var2 -> if var1 = var2 then 1 else 0
                | EvalStackValue.ManagedPointer var1, EvalStackValue.NativeInt var2 ->
                    failwith $"TODO (CEQ): managed pointer vs nativeint"
                | EvalStackValue.ManagedPointer _, _ -> failwith $"bad ceq: ManagedPointer vs {var2}"
                | EvalStackValue.UserDefinedValueType _, _ -> failwith $"bad ceq: UserDefinedValueType vs {var2}"

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 comparisonResult) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Cgt -> failwith "TODO: Cgt unimplemented"
        | Cgt_un -> failwith "TODO: Cgt_un unimplemented"
        | Clt ->
            let var2, state = state |> IlMachineState.popEvalStack currentThread
            let var1, state = state |> IlMachineState.popEvalStack currentThread

            let comparisonResult =
                match var1, var2 with
                | EvalStackValue.Int64 var1, EvalStackValue.Int64 var2 -> if var1 < var2 then 1 else 0
                | EvalStackValue.Float var1, EvalStackValue.Float var2 ->
                    failwith "TODO: Clt float comparison unimplemented"
                | EvalStackValue.ObjectRef var1, EvalStackValue.ObjectRef var2 ->
                    failwith $"Clt instruction invalid for comparing object refs, {var1} vs {var2}"
                | EvalStackValue.ObjectRef var1, other -> failwith $"invalid comparison, ref %O{var1} vs %O{other}"
                | other, EvalStackValue.ObjectRef var2 -> failwith $"invalid comparison, %O{other} vs ref %O{var2}"
                | EvalStackValue.Float i, other -> failwith $"invalid comparison, float %f{i} vs %O{other}"
                | other, EvalStackValue.Float i -> failwith $"invalid comparison, %O{other} vs float %f{i}"
                | EvalStackValue.Int64 i, other -> failwith $"invalid comparison, int64 %i{i} vs %O{other}"
                | other, EvalStackValue.Int64 i -> failwith $"invalid comparison, %O{other} vs int64 %i{i}"
                | EvalStackValue.Int32 var1, EvalStackValue.Int32 var2 -> if var1 < var2 then 1 else 0
                | EvalStackValue.Int32 var1, EvalStackValue.NativeInt var2 ->
                    failwith "TODO: Clt Int32 vs NativeInt comparison unimplemented"
                | EvalStackValue.Int32 i, other -> failwith $"invalid comparison, int32 %i{i} vs %O{other}"
                | EvalStackValue.NativeInt var1, EvalStackValue.Int32 var2 ->
                    failwith "TODO: Clt NativeInt vs Int32 comparison unimplemented"
                | other, EvalStackValue.Int32 var2 -> failwith $"invalid comparison, {other} vs int32 {var2}"
                | EvalStackValue.NativeInt var1, EvalStackValue.NativeInt var2 ->
                    if NativeIntSource.isLess var1 var2 then 1 else 0
                | EvalStackValue.NativeInt var1, other -> failwith $"invalid comparison, nativeint {var1} vs %O{other}"
                | EvalStackValue.ManagedPointer managedPointerSource, NativeInt int64 ->
                    failwith "TODO: Clt ManagedPointer vs NativeInt comparison unimplemented"
                | EvalStackValue.ManagedPointer managedPointerSource, ManagedPointer pointerSource ->
                    failwith "TODO: Clt ManagedPointer vs ManagedPointer comparison unimplemented"
                | EvalStackValue.ManagedPointer managedPointerSource, UserDefinedValueType _ ->
                    failwith "TODO: Clt ManagedPointer vs UserDefinedValueType comparison unimplemented"
                | EvalStackValue.UserDefinedValueType _, NativeInt int64 ->
                    failwith "TODO: Clt UserDefinedValueType vs NativeInt comparison unimplemented"
                | EvalStackValue.UserDefinedValueType _, ManagedPointer managedPointerSource ->
                    failwith "TODO: Clt UserDefinedValueType vs ManagedPointer comparison unimplemented"
                | EvalStackValue.UserDefinedValueType _, UserDefinedValueType _ ->
                    failwith "TODO: Clt UserDefinedValueType vs UserDefinedValueType comparison unimplemented"

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 comparisonResult) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Clt_un -> failwith "TODO: Clt_un unimplemented"
        | Stloc_0 ->
            state
            |> IlMachineState.popFromStackToLocalVariable currentThread 0
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Stloc_1 ->
            state
            |> IlMachineState.popFromStackToLocalVariable currentThread 1
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Stloc_2 ->
            state
            |> IlMachineState.popFromStackToLocalVariable currentThread 2
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Stloc_3 ->
            state
            |> IlMachineState.popFromStackToLocalVariable currentThread 3
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Sub -> failwith "TODO: Sub unimplemented"
        | Sub_ovf -> failwith "TODO: Sub_ovf unimplemented"
        | Sub_ovf_un -> failwith "TODO: Sub_ovf_un unimplemented"
        | Add ->
            let result, state =
                binaryArithmeticOperation ArithmeticOperation.add currentThread state

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Add_ovf -> failwith "TODO: Add_ovf unimplemented"
        | Add_ovf_un -> failwith "TODO: Add_ovf_un unimplemented"
        | Mul ->
            let result, state =
                binaryArithmeticOperation ArithmeticOperation.mul currentThread state

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Mul_ovf -> failwith "TODO: Mul_ovf unimplemented"
        | Mul_ovf_un -> failwith "TODO: Mul_ovf_un unimplemented"
        | Div -> failwith "TODO: Div unimplemented"
        | Div_un -> failwith "TODO: Div_un unimplemented"
        | Shr -> failwith "TODO: Shr unimplemented"
        | Shr_un -> failwith "TODO: Shr_un unimplemented"
        | Shl -> failwith "TODO: Shl unimplemented"
        | And -> failwith "TODO: And unimplemented"
        | Or -> failwith "TODO: Or unimplemented"
        | Xor -> failwith "TODO: Xor unimplemented"
        | Conv_I ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let converted = EvalStackValue.toNativeInt popped

            let state =
                match converted with
                | None -> failwith "TODO: Conv_I conversion failure unimplemented"
                | Some conv ->
                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt conv) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
        | Conv_I1 -> failwith "TODO: Conv_I1 unimplemented"
        | Conv_I2 -> failwith "TODO: Conv_I2 unimplemented"
        | Conv_I4 ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let converted = EvalStackValue.convToInt32 popped

            let state =
                match converted with
                | None -> failwith "TODO: Conv_I4 conversion failure unimplemented"
                | Some conv ->
                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 conv) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
        | Conv_I8 ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let converted = EvalStackValue.convToInt64 popped

            let state =
                match converted with
                | None -> failwith "TODO: Conv_I8 conversion failure unimplemented"
                | Some conv ->
                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.Int64 conv) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
        | Conv_R4 -> failwith "TODO: Conv_R4 unimplemented"
        | Conv_R8 -> failwith "TODO: Conv_R8 unimplemented"
        | Conv_U ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let converted = EvalStackValue.toUnsignedNativeInt popped

            let state =
                match converted with
                | None -> failwith "TODO: Conv_U conversion failure unimplemented"
                | Some conv ->
                    // > If overflow occurs when converting one integer type to another, the high-order bits are silently truncated.
                    let conv =
                        match conv with
                        | UnsignedNativeIntSource.Verbatim conv ->
                            if conv > uint64 System.Int64.MaxValue then
                                (conv % uint64 System.Int64.MaxValue) |> int64 |> NativeIntSource.Verbatim
                            else
                                int64 conv |> NativeIntSource.Verbatim
                        | UnsignedNativeIntSource.FromManagedPointer ptr -> NativeIntSource.ManagedPointer ptr

                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt conv) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
        | Conv_U1 -> failwith "TODO: Conv_U1 unimplemented"
        | Conv_U2 -> failwith "TODO: Conv_U2 unimplemented"
        | Conv_U4 -> failwith "TODO: Conv_U4 unimplemented"
        | Conv_U8 ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let converted = EvalStackValue.convToUInt64 popped

            let state =
                match converted with
                | None -> failwith "TODO: Conv_U8 conversion failure unimplemented"
                | Some conv ->
                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.Int64 conv) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
        | LdLen ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            let popped =
                match popped with
                | EvalStackValue.ManagedPointer ManagedPointerSource.Null -> failwith "TODO: throw NRE"
                | EvalStackValue.ManagedPointer (ManagedPointerSource.Heap addr) -> addr
                | _ -> failwith $"can't get len of {popped}"

            let popped = state.ManagedHeap.Arrays.[popped]

            IlMachineState.pushToEvalStack' (EvalStackValue.Int32 popped.Length) currentThread state
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Endfilter -> failwith "TODO: Endfilter unimplemented"
        | Endfinally ->
            let threadState = state.ThreadState.[currentThread]
            let currentMethodState = threadState.MethodStates.[threadState.ActiveMethodState]

            match currentMethodState.ExceptionContinuation with
            | None ->
                // Not in a finally block, just advance PC
                state
                |> IlMachineState.advanceProgramCounter currentThread
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.Stepped
            | Some (ExceptionContinuation.ResumeAfterFinally targetPC) ->
                // Resume at the leave target
                let newMethodState =
                    currentMethodState
                    |> MethodState.setProgramCounter targetPC
                    |> MethodState.clearExceptionContinuation

                let newThreadState =
                    { threadState with
                        MethodStates = threadState.MethodStates.SetItem (threadState.ActiveMethodState, newMethodState)
                    }

                { state with
                    ThreadState = state.ThreadState |> Map.add currentThread newThreadState
                }
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.Stepped
            | Some (ExceptionContinuation.PropagatingException exn) ->
                // Continue exception propagation after finally block
                let updatedExn =
                    { exn with
                        StackTrace =
                            {
                                Method = currentMethodState.ExecutingMethod
                                IlOffset = currentMethodState.IlOpIndex
                            }
                            :: exn.StackTrace
                    }

                // Search for next handler
                // TODO: Need to get exception type from heap object
                failwith "TODO: Exception type lookup from heap address not yet implemented"
            | Some (ExceptionContinuation.ResumeAfterFilter (handlerPC, exn)) ->
                // Filter evaluated, continue propagation or jump to handler based on filter result
                failwith "TODO: ResumeAfterFilter not yet implemented"
        | Rethrow -> failwith "TODO: Rethrow unimplemented"
        | Throw ->
            // Pop exception object from stack and begin exception handling
            let exceptionObject, state = IlMachineState.popEvalStack currentThread state

            let addr =
                match exceptionObject with
                | EvalStackValue.ManagedPointer (ManagedPointerSource.Heap addr)
                | EvalStackValue.ObjectRef addr -> addr
                | existing -> failwith $"Throw instruction requires an object reference on the stack; got %O{existing}"

            let threadState = state.ThreadState.[currentThread]
            let currentMethodState = threadState.MethodStates.[threadState.ActiveMethodState]

            // Get exception type from heap object
            let heapObject =
                match state.ManagedHeap.NonArrayObjects |> Map.tryFind addr with
                | Some obj -> obj
                | None -> failwith "Exception object not found in heap"

            // Build initial stack trace
            let stackFrame =
                {
                    Method = currentMethodState.ExecutingMethod
                    IlOffset = currentMethodState.IlOpIndex
                }

            let cliException =
                {
                    ExceptionObject = addr
                    StackTrace = [ stackFrame ]
                }

            // Search for handler in current method
            match
                ExceptionHandling.findExceptionHandler
                    currentMethodState.IlOpIndex
                    heapObject.Type
                    currentMethodState.ExecutingMethod
                    state._LoadedAssemblies
            with
            | Some (handler, isFinally) ->
                match handler with
                | ExceptionRegion.Catch (_, offset) ->
                    // Jump to catch handler, push exception
                    let newMethodState =
                        currentMethodState
                        |> MethodState.setProgramCounter offset.HandlerOffset
                        |> MethodState.clearEvalStack
                        |> MethodState.pushToEvalStack' exceptionObject

                    let newThreadState =
                        { threadState with
                            MethodStates =
                                threadState.MethodStates.SetItem (threadState.ActiveMethodState, newMethodState)
                        }

                    { state with
                        ThreadState = state.ThreadState |> Map.add currentThread newThreadState
                    }
                    |> Tuple.withRight WhatWeDid.Executed
                    |> ExecutionResult.Stepped
                | ExceptionRegion.Finally offset ->
                    // Jump to finally handler with exception continuation
                    let newMethodState =
                        currentMethodState
                        |> MethodState.setProgramCounter offset.HandlerOffset
                        |> MethodState.clearEvalStack
                        |> MethodState.setExceptionContinuation (PropagatingException cliException)

                    let newThreadState =
                        { threadState with
                            MethodStates =
                                threadState.MethodStates.SetItem (threadState.ActiveMethodState, newMethodState)
                        }

                    { state with
                        ThreadState = state.ThreadState |> Map.add currentThread newThreadState
                    }
                    |> Tuple.withRight WhatWeDid.Executed
                    |> ExecutionResult.Stepped
                | _ -> failwith "TODO: Filter and Fault handlers not yet implemented"
            | None -> failwith "TODO: Implement stack unwinding when no handler in current method"

        | Localloc -> failwith "TODO: Localloc unimplemented"
        | Stind_I ->
            let state =
                stind (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))) currentThread state
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
        | Stind_I1 ->
            let state =
                stind (CliType.Numeric (CliNumericType.Int8 0y)) currentThread state
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
        | Stind_I2 ->
            let state =
                stind (CliType.Numeric (CliNumericType.Int16 0s)) currentThread state
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
        | Stind_I4 ->
            let state =
                stind (CliType.Numeric (CliNumericType.Int32 0)) currentThread state
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
        | Stind_I8 ->
            let state =
                stind (CliType.Numeric (CliNumericType.Int64 0L)) currentThread state
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
        | Stind_R4 -> failwith "TODO: Stind_R4 unimplemented"
        | Stind_R8 -> failwith "TODO: Stind_R8 unimplemented"
        | Ldind_i -> executeLdind LdindTargetType.LdindI currentThread state
        | Ldind_i1 -> executeLdind LdindTargetType.LdindI1 currentThread state
        | Ldind_i2 -> executeLdind LdindTargetType.LdindI2 currentThread state
        | Ldind_i4 -> executeLdind LdindTargetType.LdindI4 currentThread state
        | Ldind_i8 -> executeLdind LdindTargetType.LdindI8 currentThread state
        | Ldind_u1 -> executeLdind LdindTargetType.LdindU1 currentThread state
        | Ldind_u2 -> executeLdind LdindTargetType.LdindU2 currentThread state
        | Ldind_u4 -> executeLdind LdindTargetType.LdindU4 currentThread state
        | Ldind_u8 -> failwith "TODO: Ldind_u8 unimplemented"
        | Ldind_r4 -> executeLdind LdindTargetType.LdindR4 currentThread state
        | Ldind_r8 -> executeLdind LdindTargetType.LdindR8 currentThread state
        | Rem -> failwith "TODO: Rem unimplemented"
        | Rem_un -> failwith "TODO: Rem_un unimplemented"
        | Volatile -> failwith "TODO: Volatile unimplemented"
        | Tail -> failwith "TODO: Tail unimplemented"
        | Conv_ovf_i_un -> failwith "TODO: Conv_ovf_i_un unimplemented"
        | Conv_ovf_u_un -> failwith "TODO: Conv_ovf_u_un unimplemented"
        | Conv_ovf_i1_un -> failwith "TODO: Conv_ovf_i1_un unimplemented"
        | Conv_ovf_u1_un -> failwith "TODO: Conv_ovf_u1_un unimplemented"
        | Conv_ovf_i2_un -> failwith "TODO: Conv_ovf_i2_un unimplemented"
        | Conv_ovf_u2_un -> failwith "TODO: Conv_ovf_u2_un unimplemented"
        | Conv_ovf_i4_un -> failwith "TODO: Conv_ovf_i4_un unimplemented"
        | Conv_ovf_u4_un -> failwith "TODO: Conv_ovf_u4_un unimplemented"
        | Conv_ovf_i8_un -> failwith "TODO: Conv_ovf_i8_un unimplemented"
        | Conv_ovf_u8_un -> failwith "TODO: Conv_ovf_u8_un unimplemented"
        | Conv_ovf_i -> failwith "TODO: Conv_ovf_i unimplemented"
        | Conv_ovf_u -> failwith "TODO: Conv_ovf_u unimplemented"
        | Neg -> failwith "TODO: Neg unimplemented"
        | Not -> failwith "TODO: Not unimplemented"
        | Ldind_ref ->
            let addr, state = IlMachineState.popEvalStack currentThread state

            let referenced =
                match addr with
                | EvalStackValue.ManagedPointer src ->
                    match src with
                    | ManagedPointerSource.Null -> failwith "TODO: throw NRE"
                    | ManagedPointerSource.LocalVariable (sourceThread, methodFrame, whichVar) ->
                        state.ThreadState.[sourceThread].MethodStates.[methodFrame].LocalVariables
                            .[int<uint16> whichVar]
                    | ManagedPointerSource.Argument (sourceThread, methodFrame, whichVar) ->
                        state.ThreadState.[sourceThread].MethodStates.[methodFrame].Arguments.[int<uint16> whichVar]
                    | ManagedPointerSource.Heap managedHeapAddress -> failwith "todo"
                | a -> failwith $"TODO: {a}"

            let state =
                match referenced with
                | CliType.ObjectRef _ -> IlMachineState.pushToEvalStack referenced currentThread state
                | _ -> failwith $"Unexpected non-reference {referenced}"
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
        | Stind_ref -> failwith "TODO: Stind_ref unimplemented"
        | Ldelem_i -> failwith "TODO: Ldelem_i unimplemented"
        | Ldelem_i1 -> failwith "TODO: Ldelem_i1 unimplemented"
        | Ldelem_u1 -> failwith "TODO: Ldelem_u1 unimplemented"
        | Ldelem_i2 -> failwith "TODO: Ldelem_i2 unimplemented"
        | Ldelem_u2 -> failwith "TODO: Ldelem_u2 unimplemented"
        | Ldelem_i4 -> failwith "TODO: Ldelem_i4 unimplemented"
        | Ldelem_u4 -> failwith "TODO: Ldelem_u4 unimplemented"
        | Ldelem_i8 -> failwith "TODO: Ldelem_i8 unimplemented"
        | Ldelem_u8 -> failwith "TODO: Ldelem_u8 unimplemented"
        | Ldelem_r4 -> failwith "TODO: Ldelem_r4 unimplemented"
        | Ldelem_r8 -> failwith "TODO: Ldelem_r8 unimplemented"
        | Ldelem_ref -> failwith "TODO: Ldelem_ref unimplemented"
        | Stelem_i -> failwith "TODO: Stelem_i unimplemented"
        | Stelem_i1 -> failwith "TODO: Stelem_i1 unimplemented"
        | Stelem_u1 -> failwith "TODO: Stelem_u1 unimplemented"
        | Stelem_i2 -> failwith "TODO: Stelem_i2 unimplemented"
        | Stelem_u2 -> failwith "TODO: Stelem_u2 unimplemented"
        | Stelem_i4 -> failwith "TODO: Stelem_i4 unimplemented"
        | Stelem_u4 -> failwith "TODO: Stelem_u4 unimplemented"
        | Stelem_i8 -> failwith "TODO: Stelem_i8 unimplemented"
        | Stelem_u8 -> failwith "TODO: Stelem_u8 unimplemented"
        | Stelem_r4 -> failwith "TODO: Stelem_r4 unimplemented"
        | Stelem_r8 -> failwith "TODO: Stelem_r8 unimplemented"
        | Stelem_ref -> failwith "TODO: Stelem_ref unimplemented"
        | Cpblk -> failwith "TODO: Cpblk unimplemented"
        | Initblk -> failwith "TODO: Initblk unimplemented"
        | Conv_ovf_u1 -> failwith "TODO: Conv_ovf_u1 unimplemented"
        | Conv_ovf_u2 -> failwith "TODO: Conv_ovf_u2 unimplemented"
        | Conv_ovf_u4 -> failwith "TODO: Conv_ovf_u4 unimplemented"
        | Conv_ovf_u8 -> failwith "TODO: Conv_ovf_u8 unimplemented"
        | Conv_ovf_i1 -> failwith "TODO: Conv_ovf_i1 unimplemented"
        | Conv_ovf_i2 -> failwith "TODO: Conv_ovf_i2 unimplemented"
        | Conv_ovf_i4 -> failwith "TODO: Conv_ovf_i4 unimplemented"
        | Conv_ovf_i8 -> failwith "TODO: Conv_ovf_i8 unimplemented"
        | Break -> failwith "TODO: Break unimplemented"
        | Conv_r_un -> failwith "TODO: Conv_r_un unimplemented"
        | Arglist -> failwith "TODO: Arglist unimplemented"
        | Ckfinite -> failwith "TODO: Ckfinite unimplemented"
        | Readonly -> failwith "TODO: Readonly unimplemented"
        | Refanytype -> failwith "TODO: Refanytype unimplemented"
