namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal UnaryConstIlOp =
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
        | Br i -> failwith "TODO: Br unimplemented"
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
                | EvalStackValue.UserDefinedValueType ->
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
                | EvalStackValue.ManagedPointer _ -> true
                | EvalStackValue.ObjectRef _ -> failwith "TODO: Brtrue_s ObjectRef comparison unimplemented"
                | EvalStackValue.UserDefinedValueType ->
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
                | EvalStackValue.UserDefinedValueType ->
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
                | EvalStackValue.UserDefinedValueType ->
                    failwith "TODO: Brtrue UserDefinedValueType comparison unimplemented"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> if isTrue then
                   IlMachineState.jumpProgramCounter currentThread i
               else
                   id
            |> Tuple.withRight WhatWeDid.Executed
        | Beq_s b -> failwith "TODO: Beq_s unimplemented"
        | Blt_s b -> failwith "TODO: Blt_s unimplemented"
        | Ble_s b -> failwith "TODO: Ble_s unimplemented"
        | Bgt_s b -> failwith "TODO: Bgt_s unimplemented"
        | Bge_s b -> failwith "TODO: Bge_s unimplemented"
        | Beq i -> failwith "TODO: Beq unimplemented"
        | Blt i -> failwith "TODO: Blt unimplemented"
        | Ble i -> failwith "TODO: Ble unimplemented"
        | Bgt i -> failwith "TODO: Bgt unimplemented"
        | Bge i -> failwith "TODO: Bge unimplemented"
        | Bne_un_s b -> failwith "TODO: Bne_un_s unimplemented"
        | Bge_un_s b -> failwith "TODO: Bge_un_s unimplemented"
        | Bgt_un_s b -> failwith "TODO: Bgt_un_s unimplemented"
        | Ble_un_s b -> failwith "TODO: Ble_un_s unimplemented"
        | Blt_un_s b -> failwith "TODO: Blt_un_s unimplemented"
        | Bne_un i -> failwith "TODO: Bne_un unimplemented"
        | Bge_un i -> failwith "TODO: Bge_un unimplemented"
        | Bgt_un i -> failwith "TODO: Bgt_un unimplemented"
        | Ble_un i -> failwith "TODO: Ble_un unimplemented"
        | Blt_un i -> failwith "TODO: Blt_un unimplemented"
        | Ldloc_s b -> failwith "TODO: Ldloc_s unimplemented"
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
        | Ldarga s -> failwith "TODO: Ldarga unimplemented"
        | Ldarg_s b -> failwith "TODO: Ldarg_s unimplemented"
        | Ldarga_s b -> failwith "TODO: Ldarga_s unimplemented"
        | Leave i -> failwith "TODO: Leave unimplemented"
        | Leave_s b -> failwith "TODO: Leave_s unimplemented"
        | Starg_s b -> failwith "TODO: Starg_s unimplemented"
        | Starg s -> failwith "TODO: Starg unimplemented"
        | Unaligned b -> failwith "TODO: Unaligned unimplemented"
        | Ldloc s -> failwith "TODO: Ldloc unimplemented"
        | Ldloca s -> failwith "TODO: Ldloca unimplemented"
        | Ldarg s -> failwith "TODO: Ldarg unimplemented"
