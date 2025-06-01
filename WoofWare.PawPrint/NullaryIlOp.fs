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
        | EvalStackValue.UserDefinedValueType
        | EvalStackValue.Float _ -> failwith $"unexpectedly tried to store value {valueToStore} in a non-address {addr}"
        | EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
        | EvalStackValue.ManagedPointer src ->
            match src with
            | ManagedPointerSource.Null -> failwith "TODO: throw NullReferenceException"
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
            match IlMachineState.returnStackFrame loggerFactory currentThread state with
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
        | LdcI4_m1 -> failwith "TODO: LdcI4_m1 unimplemented"
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
                    failwith $"TODO (CEQ): nativeint vs nativeint"
                | EvalStackValue.NativeInt var1, EvalStackValue.Int32 var2 -> failwith $"TODO (CEQ): nativeint vs int32"
                | EvalStackValue.NativeInt var1, EvalStackValue.ManagedPointer var2 ->
                    failwith $"TODO (CEQ): nativeint vs managed pointer"
                | EvalStackValue.NativeInt _, _ -> failwith $"bad ceq: NativeInt vs {var2}"
                | EvalStackValue.ObjectRef var1, EvalStackValue.ObjectRef var2 -> if var1 = var2 then 1 else 0
                | EvalStackValue.ObjectRef _, _ -> failwith $"bad ceq: ObjectRef vs {var2}"
                | EvalStackValue.ManagedPointer var1, EvalStackValue.ManagedPointer var2 ->
                    failwith $"TODO (CEQ): managed pointers"
                | EvalStackValue.ManagedPointer var1, EvalStackValue.NativeInt var2 ->
                    failwith $"TODO (CEQ): managed pointer vs nativeint"
                | EvalStackValue.ManagedPointer _, _ -> failwith $"bad ceq: ManagedPointer vs {var2}"
                | EvalStackValue.UserDefinedValueType, _ -> failwith $"bad ceq: UserDefinedValueType vs {var2}"

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
                | EvalStackValue.NativeInt var1, EvalStackValue.NativeInt var2 -> if var1 < var2 then 1 else 0
                | EvalStackValue.NativeInt var1, other -> failwith $"invalid comparison, nativeint {var1} vs %O{other}"
                | EvalStackValue.ManagedPointer managedPointerSource, NativeInt int64 ->
                    failwith "TODO: Clt ManagedPointer vs NativeInt comparison unimplemented"
                | EvalStackValue.ManagedPointer managedPointerSource, ManagedPointer pointerSource ->
                    failwith "TODO: Clt ManagedPointer vs ManagedPointer comparison unimplemented"
                | EvalStackValue.ManagedPointer managedPointerSource, UserDefinedValueType ->
                    failwith "TODO: Clt ManagedPointer vs UserDefinedValueType comparison unimplemented"
                | EvalStackValue.UserDefinedValueType, NativeInt int64 ->
                    failwith "TODO: Clt UserDefinedValueType vs NativeInt comparison unimplemented"
                | EvalStackValue.UserDefinedValueType, ManagedPointer managedPointerSource ->
                    failwith "TODO: Clt UserDefinedValueType vs ManagedPointer comparison unimplemented"
                | EvalStackValue.UserDefinedValueType, UserDefinedValueType ->
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
        | Conv_I -> failwith "TODO: Conv_I unimplemented"
        | Conv_I1 -> failwith "TODO: Conv_I1 unimplemented"
        | Conv_I2 -> failwith "TODO: Conv_I2 unimplemented"
        | Conv_I4 -> failwith "TODO: Conv_I4 unimplemented"
        | Conv_I8 -> failwith "TODO: Conv_I8 unimplemented"
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

                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt conv) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
        | Conv_U1 -> failwith "TODO: Conv_U1 unimplemented"
        | Conv_U2 -> failwith "TODO: Conv_U2 unimplemented"
        | Conv_U4 -> failwith "TODO: Conv_U4 unimplemented"
        | Conv_U8 -> failwith "TODO: Conv_U8 unimplemented"
        | LdLen -> failwith "TODO: LdLen unimplemented"
        | Endfilter -> failwith "TODO: Endfilter unimplemented"
        | Endfinally -> failwith "TODO: Endfinally unimplemented"
        | Rethrow -> failwith "TODO: Rethrow unimplemented"
        | Throw -> failwith "TODO: Throw unimplemented"
        | Localloc -> failwith "TODO: Localloc unimplemented"
        | Stind_I ->
            let state =
                stind (CliType.Numeric (CliNumericType.NativeInt 0L)) currentThread state
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
        | Ldind_i -> failwith "TODO: Ldind_i unimplemented"
        | Ldind_i1 -> failwith "TODO: Ldind_i1 unimplemented"
        | Ldind_i2 -> failwith "TODO: Ldind_i2 unimplemented"
        | Ldind_i4 -> failwith "TODO: Ldind_i4 unimplemented"
        | Ldind_i8 -> failwith "TODO: Ldind_i8 unimplemented"
        | Ldind_u1 ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            let value =
                match popped with
                | EvalStackValue.NativeInt nativeIntSource -> failwith $"TODO: in Ldind_u1, {nativeIntSource}"
                | EvalStackValue.ManagedPointer src ->
                    match src with
                    | ManagedPointerSource.Null -> failwith "unexpected null pointer in Ldind_u1"
                    | ManagedPointerSource.LocalVariable (sourceThread, methodFrame, whichVar) ->
                        let methodState =
                            state.ThreadState.[sourceThread].MethodStates.[methodFrame].LocalVariables
                                .[int<uint16> whichVar]

                        match methodState with
                        | CliType.Bool b -> b
                        | CliType.Numeric numeric -> failwith $"tried to load a Numeric as a u8: {numeric}"
                        | CliType.Char _ -> failwith "tried to load a Char as a u8"
                        | CliType.ObjectRef _ -> failwith "tried to load an ObjectRef as a u8"
                        | CliType.RuntimePointer _ -> failwith "tried to load a RuntimePointer as a u8"
                    | ManagedPointerSource.Heap managedHeapAddress -> failwith "todo"
                | EvalStackValue.ObjectRef managedHeapAddress -> failwith "todo"
                | popped -> failwith $"unexpected Ldind_u1 input: {popped}"

            let state =
                state
                |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.UInt8 value)) currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
        | Ldind_u2 -> failwith "TODO: Ldind_u2 unimplemented"
        | Ldind_u4 -> failwith "TODO: Ldind_u4 unimplemented"
        | Ldind_u8 -> failwith "TODO: Ldind_u8 unimplemented"
        | Ldind_r4 -> failwith "TODO: Ldind_r4 unimplemented"
        | Ldind_r8 -> failwith "TODO: Ldind_r8 unimplemented"
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
        | Ldind_ref -> failwith "TODO: Ldind_ref unimplemented"
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
