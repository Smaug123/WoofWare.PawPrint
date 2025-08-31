namespace WoofWare.PawPrint

open System
open Microsoft.Extensions.Logging

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
            | EvalStackValue.ManagedPointer src -> IlMachineState.dereferencePointer state src
            | EvalStackValue.NativeInt nativeIntSource ->
                failwith $"TODO: Native int pointer dereferencing not implemented for {targetType}"
            | EvalStackValue.ObjectRef managedHeapAddress ->
                IlMachineState.dereferencePointer state (ManagedPointerSource.Heap managedHeapAddress)
            | other -> failwith $"Unexpected eval stack value for Ldind operation: {other}"

        let loadedValue = loadedValue |> EvalStackValue.ofCliType

        let targetCliType = getTargetLdindCliType targetType
        let coercedValue = EvalStackValue.toCliTypeCoerced targetCliType loadedValue

        let state =
            state
            |> IlMachineState.pushToEvalStack coercedValue currentThread
            |> IlMachineState.advanceProgramCounter currentThread

        (state, WhatWeDid.Executed) |> ExecutionResult.Stepped

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
            | ManagedPointerSource.InterpretedAsType (src, ty) -> failwith "TODO"
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
            | ManagedPointerSource.ArrayIndex _ -> failwith "todo"
            | ManagedPointerSource.Field (managedPointerSource, fieldName) -> failwith "todo"
        | EvalStackValue.ObjectRef managedHeapAddress -> failwith "todo"

    let internal ldElem
        (index : EvalStackValue)
        (arr : EvalStackValue)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : ExecutionResult
        =
        let index =
            match index with
            | EvalStackValue.NativeInt src ->
                match src with
                | NativeIntSource.FunctionPointer _
                | NativeIntSource.FieldHandlePtr _
                | NativeIntSource.TypeHandlePtr _
                | NativeIntSource.ManagedPointer _ -> failwith "Refusing to treat a pointer as an array index"
                | NativeIntSource.Verbatim i -> i |> int32
            | EvalStackValue.Int32 i -> i
            | _ -> failwith $"Invalid index: {index}"

        let arrAddr =
            match arr with
            | EvalStackValue.ManagedPointer (ManagedPointerSource.Heap addr)
            | EvalStackValue.ObjectRef addr -> addr
            | EvalStackValue.ManagedPointer ManagedPointerSource.Null -> failwith "TODO: throw NRE"
            | _ -> failwith $"Invalid array: %O{arr}"

        let value = IlMachineState.getArrayValue arrAddr index state

        let state =
            state
            |> IlMachineState.pushToEvalStack value currentThread
            |> IlMachineState.advanceProgramCounter currentThread

        ExecutionResult.Stepped (state, WhatWeDid.Executed)

    let internal stElem
        (targetCliTypeZero : CliType)
        (value : EvalStackValue)
        (index : EvalStackValue)
        (arr : EvalStackValue)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : ExecutionResult
        =
        let index =
            match index with
            | EvalStackValue.NativeInt src ->
                match src with
                | NativeIntSource.FunctionPointer _
                | NativeIntSource.FieldHandlePtr _
                | NativeIntSource.TypeHandlePtr _
                | NativeIntSource.ManagedPointer _ -> failwith "Refusing to treat a pointer as an array index"
                | NativeIntSource.Verbatim i -> i |> int32
            | EvalStackValue.Int32 i -> i
            | _ -> failwith $"Invalid index: {index}"

        let arrAddr =
            match arr with
            | EvalStackValue.ManagedPointer (ManagedPointerSource.Heap addr)
            | EvalStackValue.ObjectRef addr -> addr
            | EvalStackValue.ManagedPointer ManagedPointerSource.Null -> failwith "TODO: throw NRE"
            | _ -> failwith $"Invalid array: %O{arr}"
        // TODO: throw ArrayTypeMismatchException if incorrect types

        let arr = state.ManagedHeap.Arrays.[arrAddr]

        if index < 0 || index >= arr.Length then
            failwith "TODO: throw IndexOutOfRangeException"

        let state =
            state
            |> IlMachineState.setArrayValue arrAddr (EvalStackValue.toCliTypeCoerced targetCliTypeZero value) index
            |> IlMachineState.advanceProgramCounter currentThread

        ExecutionResult.Stepped (state, WhatWeDid.Executed)

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

            let comparisonResult = if EvalStackValueComparisons.ceq var1 var2 then 1 else 0

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 comparisonResult) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Cgt ->
            let var2, state = state |> IlMachineState.popEvalStack currentThread
            let var1, state = state |> IlMachineState.popEvalStack currentThread

            let comparisonResult = if EvalStackValueComparisons.cgt var1 var2 then 1 else 0

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 comparisonResult) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Cgt_un ->
            let var2, state = state |> IlMachineState.popEvalStack currentThread
            let var1, state = state |> IlMachineState.popEvalStack currentThread

            let comparisonResult = if EvalStackValueComparisons.cgtUn var1 var2 then 1 else 0

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 comparisonResult) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Clt ->
            let var2, state = state |> IlMachineState.popEvalStack currentThread
            let var1, state = state |> IlMachineState.popEvalStack currentThread

            let comparisonResult = if EvalStackValueComparisons.clt var1 var2 then 1 else 0

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 comparisonResult) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Clt_un ->
            let var2, state = state |> IlMachineState.popEvalStack currentThread
            let var1, state = state |> IlMachineState.popEvalStack currentThread

            let comparisonResult = if EvalStackValueComparisons.cltUn var1 var2 then 1 else 0

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 comparisonResult) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
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
        | Sub ->
            let val2, state = IlMachineState.popEvalStack currentThread state
            let val1, state = IlMachineState.popEvalStack currentThread state
            let result = BinaryArithmetic.execute ArithmeticOperation.sub state val1 val2

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Sub_ovf -> failwith "TODO: Sub_ovf unimplemented"
        | Sub_ovf_un -> failwith "TODO: Sub_ovf_un unimplemented"
        | Add ->
            let val2, state = IlMachineState.popEvalStack currentThread state
            let val1, state = IlMachineState.popEvalStack currentThread state
            let result = BinaryArithmetic.execute ArithmeticOperation.add state val1 val2

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Add_ovf ->
            let val2, state = IlMachineState.popEvalStack currentThread state
            let val1, state = IlMachineState.popEvalStack currentThread state
            let result =
                try
                    BinaryArithmetic.execute ArithmeticOperation.addOvf state val1 val2
                    |> Ok
                with
                | :? OverflowException as e -> Error e

            let state =
                match result with
                | Ok result -> state |> IlMachineState.pushToEvalStack' result currentThread
                | Error excToThrow -> failwith "TODO: throw OverflowException"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Add_ovf_un -> failwith "TODO: Add_ovf_un unimplemented"
        | Mul ->
            let val2, state = IlMachineState.popEvalStack currentThread state
            let val1, state = IlMachineState.popEvalStack currentThread state
            let result = BinaryArithmetic.execute ArithmeticOperation.mul state val1 val2

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Mul_ovf ->
            let val2, state = IlMachineState.popEvalStack currentThread state
            let val1, state = IlMachineState.popEvalStack currentThread state

            let result =
                try
                    BinaryArithmetic.execute ArithmeticOperation.mulOvf state val1 val2 |> Ok
                with :? OverflowException as e ->
                    Error e

            let state =
                match result with
                | Ok result -> state |> IlMachineState.pushToEvalStack' result currentThread
                | Error excToThrow -> failwith "TODO: throw OverflowException"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Mul_ovf_un -> failwith "TODO: Mul_ovf_un unimplemented"
        | Div ->
            let val2, state = IlMachineState.popEvalStack currentThread state
            let val1, state = IlMachineState.popEvalStack currentThread state

            let result =
                try
                    BinaryArithmetic.execute ArithmeticOperation.div state val1 val2 |> Ok
                with :? OverflowException as e ->
                    Error e

            let state =
                match result with
                | Ok result -> state |> IlMachineState.pushToEvalStack' result currentThread
                | Error excToThrow -> failwith "TODO: throw OverflowException"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Div_un -> failwith "TODO: Div_un unimplemented"
        | Shr ->
            let shift, state = IlMachineState.popEvalStack currentThread state
            let number, state = IlMachineState.popEvalStack currentThread state

            let shift =
                match shift with
                | EvalStackValue.Int32 i -> i
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) -> int<int64> i
                | _ -> failwith $"Not allowed shift of {shift}"

            let result =
                // See table III.6
                match number with
                | EvalStackValue.Int32 i -> i >>> shift |> EvalStackValue.Int32
                | EvalStackValue.Int64 i -> i >>> shift |> EvalStackValue.Int64
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) ->
                    (i >>> shift) |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt
                | _ -> failwith $"Not allowed to shift {number}"

            let state =
                state
                |> IlMachineState.pushToEvalStack' result currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
        | Shr_un -> failwith "TODO: Shr_un unimplemented"
        | Shl ->
            let shift, state = IlMachineState.popEvalStack currentThread state
            let number, state = IlMachineState.popEvalStack currentThread state

            let shift =
                match shift with
                | EvalStackValue.Int32 i -> i
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) -> int<int64> i
                | _ -> failwith $"Not allowed shift of {shift}"

            let result =
                // See table III.6
                match number with
                | EvalStackValue.Int32 i -> i <<< shift |> EvalStackValue.Int32
                | EvalStackValue.Int64 i -> i <<< shift |> EvalStackValue.Int64
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) ->
                    (i <<< shift) |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt
                | _ -> failwith $"Not allowed to shift {number}"

            let state =
                state
                |> IlMachineState.pushToEvalStack' result currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
        | And ->
            let v2, state = IlMachineState.popEvalStack currentThread state
            let v1, state = IlMachineState.popEvalStack currentThread state

            let result =
                match v1, v2 with
                | EvalStackValue.Int32 v1, EvalStackValue.Int32 v2 -> v1 &&& v2 |> EvalStackValue.Int32
                | EvalStackValue.Int32 v1, EvalStackValue.NativeInt (NativeIntSource.Verbatim v2) ->
                    int64<int32> v1 &&& v2 |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt
                | EvalStackValue.Int32 _, EvalStackValue.NativeInt _ ->
                    failwith $"can't do binary operation on non-verbatim native int {v2}"
                | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 -> v1 &&& v2 |> EvalStackValue.Int64
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim v1), EvalStackValue.Int32 v2 ->
                    v1 &&& int64<int32> v2 |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt
                | EvalStackValue.NativeInt _, EvalStackValue.Int32 _ ->
                    failwith $"can't do binary operation on non-verbatim native int {v1}"
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim v1),
                  EvalStackValue.NativeInt (NativeIntSource.Verbatim v2) ->
                    v1 &&& v2 |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim _), EvalStackValue.NativeInt _ ->
                    failwith $"can't do binary operation on non-verbatim native int {v2}"
                | EvalStackValue.NativeInt _, EvalStackValue.NativeInt (NativeIntSource.Verbatim _) ->
                    failwith $"can't do binary operation on non-verbatim native int {v1}"
                | _, _ -> failwith $"refusing to do binary operation on {v1} and {v2}"

            let state =
                state
                |> IlMachineState.pushToEvalStack' result currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
        | Or ->
            let v2, state = IlMachineState.popEvalStack currentThread state
            let v1, state = IlMachineState.popEvalStack currentThread state

            let result =
                match v1, v2 with
                | EvalStackValue.Int32 v1, EvalStackValue.Int32 v2 -> v1 ||| v2 |> EvalStackValue.Int32
                | EvalStackValue.Int32 v1, EvalStackValue.NativeInt (NativeIntSource.Verbatim v2) ->
                    int64<int32> v1 ||| v2 |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt
                | EvalStackValue.Int32 _, EvalStackValue.NativeInt _ ->
                    failwith $"can't do binary operation on non-verbatim native int {v2}"
                | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 -> v1 ||| v2 |> EvalStackValue.Int64
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim v1), EvalStackValue.Int32 v2 ->
                    v1 ||| int64<int32> v2 |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt
                | EvalStackValue.NativeInt _, EvalStackValue.Int32 _ ->
                    failwith $"can't do binary operation on non-verbatim native int {v1}"
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim v1),
                  EvalStackValue.NativeInt (NativeIntSource.Verbatim v2) ->
                    v1 ||| v2 |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim _), EvalStackValue.NativeInt _ ->
                    failwith $"can't do binary operation on non-verbatim native int {v2}"
                | EvalStackValue.NativeInt _, EvalStackValue.NativeInt (NativeIntSource.Verbatim _) ->
                    failwith $"can't do binary operation on non-verbatim native int {v1}"
                | _, _ -> failwith $"refusing to do binary operation on {v1} and {v2}"

            let state =
                state
                |> IlMachineState.pushToEvalStack' result currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
        | Xor ->
            let v2, state = IlMachineState.popEvalStack currentThread state
            let v1, state = IlMachineState.popEvalStack currentThread state

            let result =
                match v1, v2 with
                | EvalStackValue.Int32 v1, EvalStackValue.Int32 v2 -> v1 ^^^ v2 |> EvalStackValue.Int32
                | EvalStackValue.Int32 v1, EvalStackValue.NativeInt (NativeIntSource.Verbatim v2) ->
                    int64<int32> v1 ^^^ v2 |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt
                | EvalStackValue.Int32 _, EvalStackValue.NativeInt _ ->
                    failwith $"can't do binary operation on non-verbatim native int {v2}"
                | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 -> v1 ^^^ v2 |> EvalStackValue.Int64
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim v1), EvalStackValue.Int32 v2 ->
                    v1 ^^^ int64<int32> v2 |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt
                | EvalStackValue.NativeInt _, EvalStackValue.Int32 _ ->
                    failwith $"can't do binary operation on non-verbatim native int {v1}"
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim v1),
                  EvalStackValue.NativeInt (NativeIntSource.Verbatim v2) ->
                    v1 ^^^ v2 |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim _), EvalStackValue.NativeInt _ ->
                    failwith $"can't do binary operation on non-verbatim native int {v2}"
                | EvalStackValue.NativeInt _, EvalStackValue.NativeInt (NativeIntSource.Verbatim _) ->
                    failwith $"can't do binary operation on non-verbatim native int {v1}"
                | _, _ -> failwith $"refusing to do binary operation on {v1} and {v2}"

            let state =
                state
                |> IlMachineState.pushToEvalStack' result currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
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
        | Conv_U1 ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let converted = EvalStackValue.convToUInt8 popped

            let state =
                match converted with
                | None -> failwith "TODO: Conv_U8 conversion failure unimplemented"
                | Some conv ->
                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 conv) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
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
                | EvalStackValue.ObjectRef addr
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
                    heapObject.ConcreteType
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
        | Rem ->
            let val2, state = IlMachineState.popEvalStack currentThread state
            let val1, state = IlMachineState.popEvalStack currentThread state
            let result = BinaryArithmetic.execute ArithmeticOperation.rem state val1 val2

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
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
        | Not ->
            let val1, state = IlMachineState.popEvalStack currentThread state

            let result =
                match val1 with
                | EvalStackValue.Int32 i -> ~~~i |> EvalStackValue.Int32
                | EvalStackValue.Int64 i -> ~~~i |> EvalStackValue.Int64
                | EvalStackValue.ManagedPointer _
                | EvalStackValue.ObjectRef _ -> failwith "refusing to negate a pointer"
                | _ -> failwith "TODO"

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Ldind_ref ->
            let addr, state = IlMachineState.popEvalStack currentThread state

            let referenced =
                match addr with
                | EvalStackValue.ManagedPointer src -> IlMachineState.dereferencePointer state src
                | a -> failwith $"TODO: {a}"

            let state =
                match referenced with
                | CliType.RuntimePointer (CliRuntimePointer.Managed _)
                | CliType.ObjectRef _ -> IlMachineState.pushToEvalStack referenced currentThread state
                | _ -> failwith $"Unexpected non-reference {referenced}"
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
        | Stind_ref ->
            let value, state = IlMachineState.popEvalStack currentThread state
            let addr, state = IlMachineState.popEvalStack currentThread state

            let state =
                match addr with
                | EvalStackValue.ManagedPointer src ->
                    match src with
                    | ManagedPointerSource.Null -> failwith "TODO: throw NRE"
                    | ManagedPointerSource.LocalVariable (sourceThread, methodFrame, whichVar) -> failwith "todo"
                    | ManagedPointerSource.Argument (sourceThread, methodFrame, whichVar) -> failwith "todo"
                    | ManagedPointerSource.Heap managedHeapAddress -> failwith "todo"
                    | ManagedPointerSource.ArrayIndex (arr, index) ->
                        state
                        |> IlMachineState.setArrayValue
                            arr
                            (EvalStackValue.toCliTypeCoerced (CliType.ObjectRef None) value)
                            index
                    | ManagedPointerSource.Field _ -> failwith "TODO"
                    | ManagedPointerSource.InterpretedAsType (src, ty) -> failwith "TODO"
                | addr -> failwith $"TODO: {addr}"

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
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
        | Ldelem_ref ->
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            ldElem index arr currentThread state
        | Stelem_i ->
            let value, state = IlMachineState.popEvalStack currentThread state
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            stElem
                (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)))
                value
                index
                arr
                currentThread
                state
        | Stelem_i1 ->
            let value, state = IlMachineState.popEvalStack currentThread state
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state
            stElem (CliType.Numeric (CliNumericType.Int8 0y)) value index arr currentThread state
        | Stelem_u1 -> failwith "TODO: Stelem_u1 unimplemented"
        | Stelem_i2 ->
            let value, state = IlMachineState.popEvalStack currentThread state
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state
            stElem (CliType.Numeric (CliNumericType.Int16 0s)) value index arr currentThread state
        | Stelem_u2 -> failwith "TODO: Stelem_u2 unimplemented"
        | Stelem_i4 ->
            let value, state = IlMachineState.popEvalStack currentThread state
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state
            stElem (CliType.Numeric (CliNumericType.Int32 0)) value index arr currentThread state
        | Stelem_u4 -> failwith "TODO: Stelem_u4 unimplemented"
        | Stelem_i8 ->
            let value, state = IlMachineState.popEvalStack currentThread state
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state
            stElem (CliType.Numeric (CliNumericType.Int64 0L)) value index arr currentThread state
        | Stelem_u8 -> failwith "TODO: Stelem_u8 unimplemented"
        | Stelem_r4 -> failwith "TODO: Stelem_r4 unimplemented"
        | Stelem_r8 -> failwith "TODO: Stelem_r8 unimplemented"
        | Stelem_ref ->
            let value, state = IlMachineState.popEvalStack currentThread state
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state
            stElem (CliType.ObjectRef None) value index arr currentThread state
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
