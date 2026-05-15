namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeSystemNative =
    let private trySystemNativeEntryPoint (ctx : NativeCallContext) : string option =
        match ctx.Instruction.ExecutingMethod.NativeImport with
        | Some import when import.ModuleName = "libSystem.Native" -> Some import.EntryPointName
        | _ -> None

    let private pushInt32 (value : int) (ctx : NativeCallContext) : ExecutionResult =
        ctx.State
        |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 value) ctx.Thread
        |> Tuple.withRight WhatWeDid.Executed
        |> ExecutionResult.Stepped

    /// Decode an `nuint`-shaped allocation size argument to an `int` byte count,
    /// failing fast on negative or synthetic (cross-storage subtraction) values
    /// which can never represent a valid C `size_t`.
    let private allocationSizeArgument (operation : string) (arg : CliType) : int =
        let checkedCount (count : int64) : int =
            if count < 0L then
                failwith $"%s{operation}: allocation size %d{count} is negative"

            if count > int64 System.Int32.MaxValue then
                failwith $"%s{operation}: allocation size %d{count} exceeds the interpreter Int32 byte-offset model"

            int count

        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim count)) -> checkedCount count
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.SyntheticCrossArrayOffset count)) ->
            failwith
                $"%s{operation}: allocation size came from synthetic cross-storage pointer subtraction %O{count}, which is not a valid UIntPtr length"
        | CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim count)) -> checkedCount count
        | CliType.Numeric (CliNumericType.Int64 (Int64Source.SyntheticCrossArrayOffset count)) ->
            failwith
                $"%s{operation}: allocation size came from synthetic cross-storage pointer subtraction %O{count}, which is not a valid UIntPtr length"
        | CliType.Numeric (CliNumericType.Int32 count) -> checkedCount (int64 count)
        | other -> failwith $"%s{operation}: expected UIntPtr allocation size, got %O{other}"

    let tryExecute (ctx : NativeCallContext) : ExecutionResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            trySystemNativeEntryPoint ctx,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | Some ("SystemNative_LChflagsCanSetHiddenFlag" | "SystemNative_CanGetHiddenFlag"),
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // PawPrint does not model Unix file flags. Report that hidden flags
            // are unsupported so CoreLib follows the portable attribute path.
            pushInt32 0 ctx |> Some
        | Some "SystemNative_GetErrNo",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            pushInt32 state.LastSystemError ctx |> Some
        | Some "SystemNative_SetErrNo",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void ->
            let error =
                NativeCall.int32Argument "SystemNative_SetErrNo" instruction.Arguments.[0]

            ({ state with
                LastSystemError = error
             },
             WhatWeDid.Executed)
            |> ExecutionResult.Stepped
            |> Some
        | Some "SystemNative_Malloc",
          [ ConcreteUIntPtr state.ConcreteTypes ],
          MethodReturnType.Returns (ConcretePointer _) ->
            // C malloc returns an uninitialised block; mirror that here so guest
            // code that reads before writing is caught by the use-of-uninit
            // detector rather than silently observing zeros.
            let size = allocationSizeArgument "SystemNative_Malloc" instruction.Arguments.[0]

            let ptrSrc, state =
                IlMachineState.allocateNativeMemory MemoryBlockInitialization.Uninitialized size state

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptrSrc) ctx.Thread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
            |> Some
        | Some "SystemNative_Calloc",
          [ ConcreteUIntPtr state.ConcreteTypes ; ConcreteUIntPtr state.ConcreteTypes ],
          MethodReturnType.Returns (ConcretePointer _) ->
            let count =
                allocationSizeArgument "SystemNative_Calloc (num)" instruction.Arguments.[0]

            let elementSize =
                allocationSizeArgument "SystemNative_Calloc (size)" instruction.Arguments.[1]

            // C calloc multiplies and zero-fills; we mirror the multiplication
            // here in int64 and reject overflow rather than truncating silently.
            let total = int64 count * int64 elementSize

            if total > int64 System.Int32.MaxValue then
                failwith
                    $"SystemNative_Calloc: allocation %d{count} * %d{elementSize} = %d{total} exceeds the interpreter Int32 byte-offset model"

            let ptrSrc, state =
                IlMachineState.allocateNativeMemory MemoryBlockInitialization.ZeroInitialized (int total) state

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptrSrc) ctx.Thread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
            |> Some
        | Some "SystemNative_Free", [ ConcretePointer _ ], MethodReturnType.Void ->
            let ptr =
                NativeCall.managedPointerOfPointerArgument "SystemNative_Free" "ptr" instruction.Arguments.[0]

            // C `free(x)` is undefined unless `x` is exactly a pointer returned
            // by `malloc`/`calloc`/`realloc` (or null). Interior pointers like
            // `base + 4` must be rejected — silently freeing the whole block
            // would mask guest memory-corruption bugs.
            let rec projectionByteOffset (acc : int) (ps : ByrefProjection list) : Result<int, ByrefProjection> =
                match ps with
                | [] -> Ok acc
                | ByrefProjection.ReinterpretAs _ :: rest -> projectionByteOffset acc rest
                | ByrefProjection.ByteOffset n :: rest -> projectionByteOffset (acc + n) rest
                | (ByrefProjection.Field _ as field) :: _ -> Error field

            let state =
                match ptr with
                // C `free(NULL)` is documented as a no-op. CoreLib's
                // NativeMemory.Free already filters null before reaching the
                // P/Invoke, but Marshal.FreeHGlobal does not, so honour the
                // C semantics here too.
                | ManagedPointerSource.Null -> state
                | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (block, rootByteOffset), projs) ->
                    match projectionByteOffset rootByteOffset projs with
                    | Ok 0 -> IlMachineState.freeNativeMemory block state
                    | Ok offset ->
                        failwith
                            $"SystemNative_Free: refusing to free interior native-heap pointer at byte offset %d{offset} into %O{block} (only the allocation base address returned by SystemNative_Malloc/Calloc may be freed)"
                    | Error field ->
                        failwith
                            $"SystemNative_Free: refusing to free native-heap pointer with non-byte projection %O{field} into %O{block} (only the allocation base address may be freed)"
                | other ->
                    failwith
                        $"SystemNative_Free: expected null or native-heap pointer, got %O{other} (only pointers from SystemNative_Malloc/Calloc may be freed here)"

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None
