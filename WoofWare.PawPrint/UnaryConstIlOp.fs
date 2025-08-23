namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal UnaryConstIlOp =
    let private leave (currentThread : ThreadId) (offset : int) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let threadState = state.ThreadState.[currentThread]
        let currentMethodState = threadState.MethodStates.[threadState.ActiveMethodState]

        let targetPc =
            (MethodState.advanceProgramCounter currentMethodState).IlOpIndex + offset

        let finallyBlocksToRun =
            let currentPC = currentMethodState.IlOpIndex
            ExceptionHandling.findFinallyBlocksToRun currentPC targetPc currentMethodState.ExecutingMethod

        // TODO: check that finallyBlocksToRun are indeed sorted by closeness
        match finallyBlocksToRun with
        | [] ->
            // No finallys to run, just jump and clear eval stack
            let newMethodState =
                currentMethodState
                |> MethodState.clearEvalStack
                |> MethodState.setProgramCounter targetPc

            let newThreadState =
                { threadState with
                    MethodStates = threadState.MethodStates.SetItem (threadState.ActiveMethodState, newMethodState)
                }

            { state with
                ThreadState = state.ThreadState |> Map.add currentThread newThreadState
            },
            WhatWeDid.Executed
        | finallyOffset :: _ ->
            // Jump to first finally, set up continuation, clear eval stack
            let newMethodState =
                currentMethodState
                |> MethodState.clearEvalStack
                |> MethodState.setExceptionContinuation (ExceptionContinuation.ResumeAfterFinally targetPc)
                |> MethodState.setProgramCounter finallyOffset.HandlerOffset

            let newThreadState =
                { threadState with
                    MethodStates = threadState.MethodStates.SetItem (threadState.ActiveMethodState, newMethodState)
                }

            { state with
                ThreadState = state.ThreadState |> Map.add currentThread newThreadState
            },
            WhatWeDid.Executed

    let execute (state : IlMachineState) (currentThread : ThreadId) (op : UnaryConstIlOp) : IlMachineState * WhatWeDid =
        match op with
        | Stloc s ->
            state
            |> IlMachineState.popFromStackToLocalVariable currentThread (int s)
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
        | Stloc_s b ->
            state
            |> IlMachineState.popFromStackToLocalVariable currentThread (int b)
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
        | Ldc_I8 i ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int64 i)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
        | Ldc_I4 i ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 i)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
        | Ldc_R4 f ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Float32 f)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
        | Ldc_R8 f ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Float64 f)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
        | Ldc_I4_s b ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int8 b)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
        | Br i ->
            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> IlMachineState.jumpProgramCounter currentThread i
            |> Tuple.withRight WhatWeDid.Executed
        | Br_s b ->
            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> IlMachineState.jumpProgramCounter currentThread (int b)
            |> Tuple.withRight WhatWeDid.Executed
        | Brfalse_s b ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            let isTrue =
                match popped with
                | EvalStackValue.Int32 i -> i <> 0
                | EvalStackValue.Int64 i -> i <> 0L
                | EvalStackValue.NativeInt i -> not (NativeIntSource.isZero i)
                | EvalStackValue.Float f -> failwith "TODO: Brfalse_s float semantics undocumented"
                | EvalStackValue.ManagedPointer ManagedPointerSource.Null -> false
                | EvalStackValue.ManagedPointer _ -> true
                | EvalStackValue.ObjectRef _ -> failwith "TODO: Brfalse_s ObjectRef comparison unimplemented"
                | EvalStackValue.UserDefinedValueType _ ->
                    failwith "TODO: Brfalse_s UserDefinedValueType comparison unimplemented"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> if isTrue then
                   id
               else
                   IlMachineState.jumpProgramCounter currentThread (int b)
            |> Tuple.withRight WhatWeDid.Executed
        | Brtrue_s b ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            let isTrue =
                match popped with
                | EvalStackValue.Int32 i -> i <> 0
                | EvalStackValue.Int64 i -> i <> 0L
                | EvalStackValue.NativeInt i -> not (NativeIntSource.isZero i)
                | EvalStackValue.Float f -> failwith "TODO: Brtrue_s float semantics undocumented"
                | EvalStackValue.ManagedPointer ManagedPointerSource.Null -> false
                | EvalStackValue.ManagedPointer _
                | EvalStackValue.ObjectRef _ -> true
                | EvalStackValue.UserDefinedValueType _ ->
                    failwith "TODO: Brtrue_s UserDefinedValueType comparison unimplemented"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> if isTrue then
                   IlMachineState.jumpProgramCounter currentThread (int b)
               else
                   id
            |> Tuple.withRight WhatWeDid.Executed
        | Brfalse i ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            let isFalse =
                match popped with
                | EvalStackValue.Int32 i -> i = 0
                | EvalStackValue.Int64 i -> i = 0L
                | EvalStackValue.NativeInt i -> NativeIntSource.isZero i
                | EvalStackValue.Float f -> failwith "TODO: Brfalse float semantics undocumented"
                | EvalStackValue.ManagedPointer ManagedPointerSource.Null -> true
                | EvalStackValue.ManagedPointer _ -> false
                | EvalStackValue.ObjectRef _ -> failwith "TODO: Brfalse ObjectRef comparison unimplemented"
                | EvalStackValue.UserDefinedValueType _ ->
                    failwith "TODO: Brfalse UserDefinedValueType comparison unimplemented"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> if isFalse then
                   IlMachineState.jumpProgramCounter currentThread i
               else
                   id
            |> Tuple.withRight WhatWeDid.Executed
        | Brtrue i ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            let isTrue =
                match popped with
                | EvalStackValue.Int32 i -> i <> 0
                | EvalStackValue.Int64 i -> i <> 0L
                | EvalStackValue.NativeInt i -> not (NativeIntSource.isZero i)
                | EvalStackValue.Float f -> failwith "TODO: Brtrue float semantics undocumented"
                | EvalStackValue.ManagedPointer ManagedPointerSource.Null -> false
                | EvalStackValue.ManagedPointer _ -> true
                | EvalStackValue.ObjectRef _ -> failwith "TODO: Brtrue ObjectRef comparison unimplemented"
                | EvalStackValue.UserDefinedValueType _ ->
                    failwith "TODO: Brtrue UserDefinedValueType comparison unimplemented"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> if isTrue then
                   IlMachineState.jumpProgramCounter currentThread i
               else
                   id
            |> Tuple.withRight WhatWeDid.Executed
        | Beq_s b ->
            let value2, state = IlMachineState.popEvalStack currentThread state
            let value1, state = IlMachineState.popEvalStack currentThread state

            let isEq =
                match value1, value2 with
                | EvalStackValue.Int32 v1, EvalStackValue.Int32 v2 -> v1 = v2
                | EvalStackValue.Int32 i, EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
                | EvalStackValue.Int32 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 -> v1 = v2
                | EvalStackValue.Int64 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.NativeInt nativeIntSource, _ -> failwith "todo"
                | EvalStackValue.Float v1, EvalStackValue.Float v2 -> failwith "todo"
                | EvalStackValue.Float f, _ -> failwith $"invalid comparison, {f} with {value2}"
                | EvalStackValue.ManagedPointer v1, EvalStackValue.ManagedPointer v2 -> failwith "todo"
                | EvalStackValue.ManagedPointer v1, _ -> failwith $"invalid comparison, {v1} with {value2}"
                | EvalStackValue.ObjectRef _, _ -> failwith "todo"
                | EvalStackValue.UserDefinedValueType _, _ ->
                    failwith "unexpectedly tried to compare user-defined value type"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> if isEq then
                   IlMachineState.jumpProgramCounter currentThread (int<int8> b)
               else
                   id
            |> Tuple.withRight WhatWeDid.Executed
        | Blt_s b ->
            let value2, state = IlMachineState.popEvalStack currentThread state
            let value1, state = IlMachineState.popEvalStack currentThread state

            let isLessThan =
                match value1, value2 with
                | EvalStackValue.Int32 v1, EvalStackValue.Int32 v2 -> v1 < v2
                | EvalStackValue.Int32 i, EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
                | EvalStackValue.Int32 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 -> v1 < v2
                | EvalStackValue.Int64 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.NativeInt nativeIntSource, _ -> failwith "todo"
                | EvalStackValue.Float v1, EvalStackValue.Float v2 -> failwith "todo"
                | EvalStackValue.Float f, _ -> failwith $"invalid comparison, {f} with {value2}"
                | EvalStackValue.ManagedPointer v1, EvalStackValue.ManagedPointer v2 -> failwith "todo"
                | EvalStackValue.ManagedPointer v1, _ -> failwith $"invalid comparison, {v1} with {value2}"
                | EvalStackValue.ObjectRef _, _ -> failwith "todo"
                | EvalStackValue.UserDefinedValueType _, _ ->
                    failwith "unexpectedly tried to compare user-defined value type"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> if isLessThan then
                   IlMachineState.jumpProgramCounter currentThread (int<int8> b)
               else
                   id
            |> Tuple.withRight WhatWeDid.Executed
        | Ble_s b ->
            let value2, state = IlMachineState.popEvalStack currentThread state
            let value1, state = IlMachineState.popEvalStack currentThread state

            let isLessEq =
                match value1, value2 with
                | EvalStackValue.Int32 v1, EvalStackValue.Int32 v2 -> v1 <= v2
                | EvalStackValue.Int32 i, EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
                | EvalStackValue.Int32 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 -> v1 <= v2
                | EvalStackValue.Int64 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.NativeInt nativeIntSource, _ -> failwith "todo"
                | EvalStackValue.Float v1, EvalStackValue.Float v2 -> failwith "todo"
                | EvalStackValue.Float f, _ -> failwith $"invalid comparison, {f} with {value2}"
                | EvalStackValue.ManagedPointer v1, EvalStackValue.ManagedPointer v2 -> failwith "todo"
                | EvalStackValue.ManagedPointer v1, _ -> failwith $"invalid comparison, {v1} with {value2}"
                | EvalStackValue.ObjectRef _, _ -> failwith "todo"
                | EvalStackValue.UserDefinedValueType _, _ ->
                    failwith "unexpectedly tried to compare user-defined value type"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> if isLessEq then
                   IlMachineState.jumpProgramCounter currentThread (int<int8> b)
               else
                   id
            |> Tuple.withRight WhatWeDid.Executed
        | Bgt_s b ->
            let value2, state = IlMachineState.popEvalStack currentThread state
            let value1, state = IlMachineState.popEvalStack currentThread state

            let isGreaterThan =
                match value1, value2 with
                | EvalStackValue.Int32 v1, EvalStackValue.Int32 v2 -> v1 > v2
                | EvalStackValue.Int32 i, EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
                | EvalStackValue.Int32 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 -> v1 > v2
                | EvalStackValue.Int64 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.NativeInt nativeIntSource, _ -> failwith "todo"
                | EvalStackValue.Float v1, EvalStackValue.Float v2 -> failwith "todo"
                | EvalStackValue.Float f, _ -> failwith $"invalid comparison, {f} with {value2}"
                | EvalStackValue.ManagedPointer v1, EvalStackValue.ManagedPointer v2 -> failwith "todo"
                | EvalStackValue.ManagedPointer v1, _ -> failwith $"invalid comparison, {v1} with {value2}"
                | EvalStackValue.ObjectRef _, _ -> failwith "todo"
                | EvalStackValue.UserDefinedValueType _, _ ->
                    failwith "unexpectedly tried to compare user-defined value type"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> if isGreaterThan then
                   IlMachineState.jumpProgramCounter currentThread (int<int8> b)
               else
                   id
            |> Tuple.withRight WhatWeDid.Executed
        | Bge_s b ->
            let value2, state = IlMachineState.popEvalStack currentThread state
            let value1, state = IlMachineState.popEvalStack currentThread state

            let isGreaterEq =
                match value1, value2 with
                | EvalStackValue.Int32 v1, EvalStackValue.Int32 v2 -> v1 >= v2
                | EvalStackValue.Int32 i, EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
                | EvalStackValue.Int32 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 -> v1 >= v2
                | EvalStackValue.Int64 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.NativeInt nativeIntSource, _ -> failwith "todo"
                | EvalStackValue.Float v1, EvalStackValue.Float v2 -> failwith "todo"
                | EvalStackValue.Float f, _ -> failwith $"invalid comparison, {f} with {value2}"
                | EvalStackValue.ManagedPointer v1, EvalStackValue.ManagedPointer v2 -> failwith "todo"
                | EvalStackValue.ManagedPointer v1, _ -> failwith $"invalid comparison, {v1} with {value2}"
                | EvalStackValue.ObjectRef _, _ -> failwith "todo"
                | EvalStackValue.UserDefinedValueType _, _ ->
                    failwith "unexpectedly tried to compare user-defined value type"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> if isGreaterEq then
                   IlMachineState.jumpProgramCounter currentThread (int<int8> b)
               else
                   id
            |> Tuple.withRight WhatWeDid.Executed
        | Beq i ->
            let value2, state = IlMachineState.popEvalStack currentThread state
            let value1, state = IlMachineState.popEvalStack currentThread state

            let isEq =
                match value1, value2 with
                | EvalStackValue.Int32 v1, EvalStackValue.Int32 v2 -> v1 = v2
                | EvalStackValue.Int32 i, EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
                | EvalStackValue.Int32 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 -> v1 = v2
                | EvalStackValue.Int64 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.NativeInt nativeIntSource, _ -> failwith "todo"
                | EvalStackValue.Float v1, EvalStackValue.Float v2 -> failwith "todo"
                | EvalStackValue.Float f, _ -> failwith $"invalid comparison, {f} with {value2}"
                | EvalStackValue.ManagedPointer v1, EvalStackValue.ManagedPointer v2 -> failwith "todo"
                | EvalStackValue.ManagedPointer v1, _ -> failwith $"invalid comparison, {v1} with {value2}"
                | EvalStackValue.ObjectRef _, _ -> failwith "todo"
                | EvalStackValue.UserDefinedValueType _, _ ->
                    failwith "unexpectedly tried to compare user-defined value type"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> if isEq then
                   IlMachineState.jumpProgramCounter currentThread i
               else
                   id
            |> Tuple.withRight WhatWeDid.Executed
        | Blt i -> failwith "TODO: Blt unimplemented"
        | Ble i ->
            let value2, state = IlMachineState.popEvalStack currentThread state
            let value1, state = IlMachineState.popEvalStack currentThread state

            let isLessEq =
                match value1, value2 with
                | EvalStackValue.Int32 v1, EvalStackValue.Int32 v2 -> v1 <= v2
                | EvalStackValue.Int32 i, EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
                | EvalStackValue.Int32 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 -> v1 <= v2
                | EvalStackValue.Int64 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.NativeInt nativeIntSource, _ -> failwith "todo"
                | EvalStackValue.Float v1, EvalStackValue.Float v2 -> failwith "todo"
                | EvalStackValue.Float f, _ -> failwith $"invalid comparison, {f} with {value2}"
                | EvalStackValue.ManagedPointer v1, EvalStackValue.ManagedPointer v2 -> failwith "todo"
                | EvalStackValue.ManagedPointer v1, _ -> failwith $"invalid comparison, {v1} with {value2}"
                | EvalStackValue.ObjectRef _, _ -> failwith "todo"
                | EvalStackValue.UserDefinedValueType _, _ ->
                    failwith "unexpectedly tried to compare user-defined value type"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> if isLessEq then
                   IlMachineState.jumpProgramCounter currentThread i
               else
                   id
            |> Tuple.withRight WhatWeDid.Executed
        | Bgt i -> failwith "TODO: Bgt unimplemented"
        | Bge i ->
            let value2, state = IlMachineState.popEvalStack currentThread state
            let value1, state = IlMachineState.popEvalStack currentThread state

            let isGreaterEq =
                match value1, value2 with
                | EvalStackValue.Int32 v1, EvalStackValue.Int32 v2 -> v1 >= v2
                | EvalStackValue.Int32 i, EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
                | EvalStackValue.Int32 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 -> v1 >= v2
                | EvalStackValue.Int64 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.NativeInt nativeIntSource, _ -> failwith "todo"
                | EvalStackValue.Float v1, EvalStackValue.Float v2 -> failwith "todo"
                | EvalStackValue.Float f, _ -> failwith $"invalid comparison, {f} with {value2}"
                | EvalStackValue.ManagedPointer v1, EvalStackValue.ManagedPointer v2 -> failwith "todo"
                | EvalStackValue.ManagedPointer v1, _ -> failwith $"invalid comparison, {v1} with {value2}"
                | EvalStackValue.ObjectRef _, _ -> failwith "todo"
                | EvalStackValue.UserDefinedValueType _, _ ->
                    failwith "unexpectedly tried to compare user-defined value type"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> if isGreaterEq then
                   IlMachineState.jumpProgramCounter currentThread i
               else
                   id
            |> Tuple.withRight WhatWeDid.Executed
        | Bne_un_s b ->
            // Table III.4
            let value2, state = IlMachineState.popEvalStack currentThread state
            let value1, state = IlMachineState.popEvalStack currentThread state

            let isNotEqual =
                match value1, value2 with
                | EvalStackValue.Int32 v1, EvalStackValue.Int32 v2 -> v1 <> v2
                | EvalStackValue.Int32 v1, EvalStackValue.NativeInt v2 -> failwith "TODO"
                | EvalStackValue.Int32 v1, _ -> failwith $"invalid comparison, {v1} with {value2}"
                | _, EvalStackValue.Int32 v2 -> failwith $"invalid comparison, {value1} with {v2}"
                | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 -> v1 <> v2
                | EvalStackValue.Int64 v1, _ -> failwith $"invalid comparison, {v1} with {value2}"
                | _, EvalStackValue.Int64 v2 -> failwith $"invalid comparison, {value1} with {v2}"
                | EvalStackValue.Float v1, EvalStackValue.Float v2 -> v1 <> v2
                | _, EvalStackValue.Float v2 -> failwith $"invalid comparison, {value1} with {v2}"
                | EvalStackValue.Float v1, _ -> failwith $"invalid comparison, {v1} with {value2}"
                | EvalStackValue.NativeInt v1, EvalStackValue.NativeInt v2 -> v1 <> v2
                | EvalStackValue.ManagedPointer ptr1, EvalStackValue.ManagedPointer ptr2 -> ptr1 <> ptr2
                | _, _ -> failwith $"TODO {value1} {value2} (see table III.4)"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> if isNotEqual then
                   IlMachineState.jumpProgramCounter currentThread (int b)
               else
                   id
            |> Tuple.withRight WhatWeDid.Executed
        | Bge_un_s b ->
            let value2, state = IlMachineState.popEvalStack currentThread state
            let value1, state = IlMachineState.popEvalStack currentThread state

            let isGreaterEq =
                match value1, value2 with
                | EvalStackValue.Int32 v1, EvalStackValue.Int32 v2 ->
                    if v1 < 0 || v2 < 0 then
                        failwith "TODO"

                    v1 >= v2
                | EvalStackValue.Int32 i, EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
                | EvalStackValue.Int32 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 ->
                    if v1 < 0L || v2 < 0L then
                        failwith "TODO"

                    v1 >= v2
                | EvalStackValue.Int64 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.NativeInt nativeIntSource, _ -> failwith "todo"
                | EvalStackValue.Float v1, EvalStackValue.Float v2 -> failwith "todo"
                | EvalStackValue.Float f, _ -> failwith $"invalid comparison, {f} with {value2}"
                | EvalStackValue.ManagedPointer v1, EvalStackValue.ManagedPointer v2 -> failwith "todo"
                | EvalStackValue.ManagedPointer v1, _ -> failwith $"invalid comparison, {v1} with {value2}"
                | EvalStackValue.ObjectRef _, _ -> failwith "todo"
                | EvalStackValue.UserDefinedValueType _, _ ->
                    failwith "unexpectedly tried to compare user-defined value type"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> if isGreaterEq then
                   IlMachineState.jumpProgramCounter currentThread (int b)
               else
                   id
            |> Tuple.withRight WhatWeDid.Executed
        | Bgt_un_s b ->
            let value2, state = IlMachineState.popEvalStack currentThread state
            let value1, state = IlMachineState.popEvalStack currentThread state

            let isGreaterThan =
                match value1, value2 with
                | EvalStackValue.Int32 v1, EvalStackValue.Int32 v2 ->
                    if v1 < 0 || v2 < 0 then
                        failwith "TODO"

                    v1 > v2
                | EvalStackValue.Int32 i, EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
                | EvalStackValue.Int32 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 ->
                    if v1 < 0L || v2 < 0L then
                        failwith "TODO"

                    v1 > v2
                | EvalStackValue.Int64 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.NativeInt nativeIntSource, _ -> failwith "todo"
                | EvalStackValue.Float v1, EvalStackValue.Float v2 -> failwith "todo"
                | EvalStackValue.Float f, _ -> failwith $"invalid comparison, {f} with {value2}"
                | EvalStackValue.ManagedPointer v1, EvalStackValue.ManagedPointer v2 -> failwith "todo"
                | EvalStackValue.ManagedPointer v1, _ -> failwith $"invalid comparison, {v1} with {value2}"
                | EvalStackValue.ObjectRef _, _ -> failwith "todo"
                | EvalStackValue.UserDefinedValueType _, _ ->
                    failwith "unexpectedly tried to compare user-defined value type"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> if isGreaterThan then
                   IlMachineState.jumpProgramCounter currentThread (int b)
               else
                   id
            |> Tuple.withRight WhatWeDid.Executed
        | Ble_un_s b ->
            let value2, state = IlMachineState.popEvalStack currentThread state
            let value1, state = IlMachineState.popEvalStack currentThread state

            let isLessEq =
                match value1, value2 with
                | EvalStackValue.Int32 v1, EvalStackValue.Int32 v2 ->
                    if v1 < 0 || v2 < 0 then
                        failwith "TODO"

                    v1 <= v2
                | EvalStackValue.Int32 i, EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
                | EvalStackValue.Int32 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 ->
                    if v1 < 0L || v2 < 0L then
                        failwith "TODO"

                    v1 <= v2
                | EvalStackValue.Int64 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.NativeInt nativeIntSource, _ -> failwith "todo"
                | EvalStackValue.Float v1, EvalStackValue.Float v2 -> failwith "todo"
                | EvalStackValue.Float f, _ -> failwith $"invalid comparison, {f} with {value2}"
                | EvalStackValue.ManagedPointer v1, EvalStackValue.ManagedPointer v2 -> failwith "todo"
                | EvalStackValue.ManagedPointer v1, _ -> failwith $"invalid comparison, {v1} with {value2}"
                | EvalStackValue.ObjectRef _, _ -> failwith "todo"
                | EvalStackValue.UserDefinedValueType _, _ ->
                    failwith "unexpectedly tried to compare user-defined value type"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> if isLessEq then
                   IlMachineState.jumpProgramCounter currentThread (int b)
               else
                   id
            |> Tuple.withRight WhatWeDid.Executed
        | Blt_un_s b ->
            let value2, state = IlMachineState.popEvalStack currentThread state
            let value1, state = IlMachineState.popEvalStack currentThread state

            let isLessThan =
                match value1, value2 with
                | EvalStackValue.Int32 v1, EvalStackValue.Int32 v2 ->
                    if v1 < 0 || v2 < 0 then
                        failwith "TODO"

                    v1 < v2
                | EvalStackValue.Int32 i, EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
                | EvalStackValue.Int32 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 ->
                    if v1 < 0L || v2 < 0L then
                        failwith "TODO"

                    v1 < v2
                | EvalStackValue.Int64 i, _ -> failwith $"invalid comparison, {i} with {value2}"
                | EvalStackValue.NativeInt nativeIntSource, _ -> failwith "todo"
                | EvalStackValue.Float v1, EvalStackValue.Float v2 -> failwith "todo"
                | EvalStackValue.Float f, _ -> failwith $"invalid comparison, {f} with {value2}"
                | EvalStackValue.ManagedPointer v1, EvalStackValue.ManagedPointer v2 -> failwith "todo"
                | EvalStackValue.ManagedPointer v1, _ -> failwith $"invalid comparison, {v1} with {value2}"
                | EvalStackValue.ObjectRef _, _ -> failwith "todo"
                | EvalStackValue.UserDefinedValueType _, _ ->
                    failwith "unexpectedly tried to compare user-defined value type"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> if isLessThan then
                   IlMachineState.jumpProgramCounter currentThread (int b)
               else
                   id
            |> Tuple.withRight WhatWeDid.Executed
        | Bne_un i -> failwith "TODO: Bne_un unimplemented"
        | Bge_un i -> failwith "TODO: Bge_un unimplemented"
        | Bgt_un i -> failwith "TODO: Bgt_un unimplemented"
        | Ble_un i -> failwith "TODO: Ble_un unimplemented"
        | Blt_un i -> failwith "TODO: Blt_un unimplemented"
        | Ldloc_s b ->
            let threadState = state.ThreadState.[currentThread]

            let state =
                state
                |> IlMachineState.pushToEvalStack threadState.MethodState.LocalVariables.[int<uint8> b] currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            state, WhatWeDid.Executed
        | Ldloca_s b ->
            let threadState = state.ThreadState.[currentThread]

            let state =
                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.ManagedPointer (
                        ManagedPointerSource.LocalVariable (
                            currentThread,
                            threadState.ActiveMethodState,
                            uint16<uint8> b
                        )
                    ))
                    currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            state, WhatWeDid.Executed
        | Ldarga s ->
            let executingMethod = state.ThreadState.[currentThread]

            let ptr =
                ManagedPointerSource.Argument (currentThread, executingMethod.ActiveMethodState, s)

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptr) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
        | Ldarga_s b ->
            let executingMethod = state.ThreadState.[currentThread]

            let ptr =
                ManagedPointerSource.Argument (currentThread, executingMethod.ActiveMethodState, uint16<byte> b)

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptr) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
        | Ldarg_s b -> failwith "TODO: Ldarg_s unimplemented"
        | Leave i -> leave currentThread i state
        | Leave_s b -> leave currentThread (int<int8> b) state
        | Starg_s b -> failwith "TODO: Starg_s unimplemented"
        | Starg s -> failwith "TODO: Starg unimplemented"
        | Unaligned b -> failwith "TODO: Unaligned unimplemented"
        | Ldloc s -> failwith "TODO: Ldloc unimplemented"
        | Ldloca s -> failwith "TODO: Ldloca unimplemented"
        | Ldarg s -> failwith "TODO: Ldarg unimplemented"
