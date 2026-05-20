namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeBuffer =
    let private checkedByteCount (operation : string) (count : int64) : int =
        if count < 0L then
            failwith $"%s{operation}: byte count %d{count} is negative"

        if count > int64 System.Int32.MaxValue then
            failwith $"%s{operation}: byte count %d{count} exceeds the interpreter Int32 byte-offset model"

        int count

    let private byteCountOfArgument (operation : string) (arg : CliType) : int =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim count)) ->
            checkedByteCount operation count
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.SyntheticCrossArrayOffset count)) ->
            failwith
                $"%s{operation}: byte count came from synthetic cross-storage pointer subtraction %O{count}, which is not a valid UIntPtr length"
        | CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim count)) -> checkedByteCount operation count
        | CliType.Numeric (CliNumericType.Int64 (Int64Source.SyntheticCrossArrayOffset count)) ->
            failwith
                $"%s{operation}: byte count came from synthetic cross-storage pointer subtraction %O{count}, which is not a valid UIntPtr length"
        | CliType.Numeric (CliNumericType.Int32 count) -> checkedByteCount operation (int64 count)
        | other -> failwith $"%s{operation}: expected UIntPtr byte count, got %O{other}"

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "Buffer_MemMove",
          "System.Private.CoreLib",
          "System",
          "Buffer",
          "__Memmove",
          [ ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
            ConcreteUIntPtr state.ConcreteTypes ],
          MethodReturnType.Void
        | "Buffer_MemMove",
          "System.Private.CoreLib",
          "System",
          "Buffer",
          "MemmoveInternal",
          [ ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
            ConcreteUIntPtr state.ConcreteTypes ],
          MethodReturnType.Void ->
            let operation = "Buffer_MemMove"

            if instruction.Arguments.Length <> 3 then
                failwith
                    $"%s{operation}: expected three native arguments after matching signature, got %d{instruction.Arguments.Length}"

            let dest =
                NativeCall.managedPointerOfPointerArgument operation "dest" instruction.Arguments.[0]

            let src =
                NativeCall.managedPointerOfPointerArgument operation "src" instruction.Arguments.[1]

            let byteCount = byteCountOfArgument operation instruction.Arguments.[2]

            let state =
                if byteCount = 0 then
                    state
                else
                    CellAwareCopy.copy ctx.BaseClassTypes operation CellAwareCopyPolicy.Memmove state dest src byteCount

            NativeHandlerResult.completed state |> Some
        | _ -> None

    /// Dispatches the InternalCall (FCall) variants of `System.Buffer` that
    /// take byref `byte` endpoints rather than the QCall pointer endpoints.
    ///
    /// This handler wires `BulkMoveWithWriteBarrierInternal` into native
    /// dispatch and implements CoreCLR's FCall short-circuits
    /// (`dst != src && byteCount != 0`, see comutilnative.cpp); the actual
    /// move reuses the shared `CellAwareCopy.copy` helper. The BCL's primary
    /// callers (`Buffer.Memmove<T>` for `T` containing references,
    /// `Array.Copy` of reference-typed arrays, the reflection-cache growth
    /// path, etc.) hand in byrefs that land on non-byte-addressable cells
    /// (object references, value types containing object references);
    /// `CellAwareCopy.copy` detects cell-aligned ranges via
    /// `tryWholeCellMoveAt` and moves whole typed cells through
    /// `readManagedByref` / `writeManagedByrefWithBase` so the dest cell's
    /// CLI shape and the stored ObjectRef provenance are preserved.
    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
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
          "Buffer",
          "BulkMoveWithWriteBarrierInternal",
          [ ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
            ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
            ConcreteUIntPtr state.ConcreteTypes ],
          MethodReturnType.Void ->
            let operation = "Buffer_BulkMoveWithWriteBarrierInternal"

            if instruction.Arguments.Length <> 3 then
                failwith
                    $"%s{operation}: expected three native arguments after matching signature, got %d{instruction.Arguments.Length}"

            let dest =
                NativeCall.managedPointerOfPointerArgument operation "dest" instruction.Arguments.[0]

            let src =
                NativeCall.managedPointerOfPointerArgument operation "src" instruction.Arguments.[1]

            let byteCount = byteCountOfArgument operation instruction.Arguments.[2]

            // CoreCLR's FCall short-circuits both `dst == src` and
            // `byteCount == 0` (see comutilnative.cpp). We honour both
            // explicitly: storage that contains object references is not
            // byte-addressable in PawPrint, so a self-copy of such storage
            // must not fall through to `CellAwareCopy.copy` — the byte-walk
            // fallback would reject it.
            let state =
                if byteCount = 0 || dest = src then
                    state
                else
                    CellAwareCopy.copy ctx.BaseClassTypes operation CellAwareCopyPolicy.Memmove state dest src byteCount

            NativeHandlerResult.completed state |> Some
        | _ -> None
