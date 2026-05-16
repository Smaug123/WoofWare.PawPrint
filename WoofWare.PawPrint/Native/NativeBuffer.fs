namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeBuffer =
    let private byteTemplate : CliType = CliType.Numeric (CliNumericType.UInt8 0uy)

    let private byteType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ConcreteType<ConcreteTypeHandle>
        =
        let handle =
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes baseClassTypes.Byte.Identity
            |> Option.defaultWith (fun () -> failwith "Buffer_MemMove: System.Byte is not concretized")

        AllConcreteTypes.lookup handle state.ConcreteTypes
        |> Option.defaultWith (fun () -> failwith $"Buffer_MemMove: concrete System.Byte handle %O{handle} not found")

    let private readByte
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : byte
        =
        match IlMachineState.readManagedByrefBytesAs baseClassTypes state ptr byteTemplate with
        | CliType.Numeric (CliNumericType.UInt8 b) -> b
        | other -> failwith $"Buffer_MemMove: byte-view read returned non-byte value %O{other}"

    let private writeByte
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        (value : byte)
        : IlMachineState
        =
        IlMachineState.writeManagedByrefBytesOrTypedCell
            baseClassTypes
            state
            ptr
            (CliType.Numeric (CliNumericType.UInt8 value))

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

    let private projectionByteOffset (projs : ByrefProjection list) : int64 option =
        let rec loop (byteOffset : int64) (projs : ByrefProjection list) : int64 option =
            match projs with
            | [] -> Some byteOffset
            | ByrefProjection.ReinterpretAs _ :: rest -> loop byteOffset rest
            | ByrefProjection.ByteOffset offset :: rest -> loop (byteOffset + int64 offset) rest
            | ByrefProjection.Field _ :: _ -> None

        loop 0L projs

    let private byteLocation
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : (ByteStorageIdentity * int64) option
        =
        match ptr with
        | ManagedPointerSource.Null -> None
        | ManagedPointerSource.NativeIntPlaceholder _ -> None
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), projs) ->
            projectionByteOffset projs
            |> Option.map (fun byteOffset ->
                ByteStorageIdentity.Array arr,
                ManagedPointerByteView.arrayBytePosition baseClassTypes state arr index byteOffset
            )
        | ManagedPointerSource.Byref (ByrefRoot.StringCharAt (str, charIndex), projs) ->
            projectionByteOffset projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.String str, int64 charIndex * 2L + byteOffset)
        | ManagedPointerSource.Byref (ByrefRoot.PeByteRange peByteRange, projs) ->
            projectionByteOffset projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.PeByteRange peByteRange, byteOffset)
        | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (thread, frame, block, rootByteOffset), projs) ->
            projectionByteOffset projs
            |> Option.map (fun byteOffset ->
                ByteStorageIdentity.StackMemory (thread, frame, block), int64 rootByteOffset + byteOffset
            )
        | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (block, rootByteOffset), projs) ->
            projectionByteOffset projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.NativeMemory block, int64 rootByteOffset + byteOffset)
        | ManagedPointerSource.Byref (ByrefRoot.LocalVariable (thread, frame, local), projs) ->
            projectionByteOffset projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.StackLocal (thread, frame, local), byteOffset)
        | ManagedPointerSource.Byref (ByrefRoot.Argument (thread, frame, arg), projs) ->
            projectionByteOffset projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.StackArgument (thread, frame, arg), byteOffset)
        | ManagedPointerSource.Byref (ByrefRoot.StaticField (declaringType, field), projs) ->
            projectionByteOffset projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.StaticField (declaringType, field), byteOffset)
        // These roots do not expose a stable flat byte coordinate here. The
        // supported Buffer_MemMove overlap paths are flat byte-storage-backed;
        // if aliased overlap on these roots appears, extend this model rather
        // than guessing a projection.
        | ManagedPointerSource.Byref (ByrefRoot.HeapValue _, _)
        | ManagedPointerSource.Byref (ByrefRoot.HeapObjectField _, _)
        | ManagedPointerSource.Byref (ByrefRoot.MethodTableExposedClassObject _, _) -> None

    let private shouldCopyBackwards
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (dest : ManagedPointerSource)
        (byteCount : int)
        : bool
        =
        match byteLocation baseClassTypes state src, byteLocation baseClassTypes state dest with
        | Some (srcStorage, srcOffset), Some (destStorage, destOffset) when srcStorage = destStorage ->
            srcOffset < destOffset && destOffset < srcOffset + int64 byteCount
        | _ -> false

    /// All residual projections must be plain `Field` projections. The
    /// fast-path uses `readManagedByref` on the residual, which dispatches
    /// through `readProjectedValue`: that helper supports `Field` cleanly,
    /// short-circuits `ReinterpretAs` only for same-width primitive families
    /// (and throws otherwise), and unconditionally throws on `ByteOffset`.
    /// Allowing interior `ReinterpretAs` here would turn the fast path into
    /// a host failure for shapes the byte-walk fallback would otherwise
    /// service (e.g. a byte view built from a non-trailing `ReinterpretAs`
    /// such as `Unsafe.As<int, S>(ref arr[0]).B`), so the fast path
    /// declines and lets the byte-walk peel the projection.
    let private isPlainResidual (projs : ByrefProjection list) : bool =
        projs
        |> List.forall (fun proj ->
            match proj with
            | ByrefProjection.Field _ -> true
            | ByrefProjection.ReinterpretAs _
            | ByrefProjection.ByteOffset _ -> false
        )

    /// Strip a trailing byte-view suffix (`[..., ReinterpretAs _]` or
    /// `[..., ReinterpretAs _; ByteOffset n]`) and return the residual byref
    /// together with the intra-cell byte offset. Returns `None` for non-Byref
    /// pointers, for byrefs whose trailing projections are not a clean
    /// byte-view suffix (e.g. a trailing `Field` or `ByteOffset` without a
    /// preceding `ReinterpretAs` — the latter would already be an
    /// interpreter-bug shape), or for byrefs whose residual after stripping
    /// would still contain an interior `ReinterpretAs` / `ByteOffset`
    /// projection (see `isPlainResidual` for why those must fall back to the
    /// byte-walk).
    let private inCellOffsetAndStripByteView (ptr : ManagedPointerSource) : (ManagedPointerSource * int) option =
        let tryReturn (root : ByrefRoot) (residualRev : ByrefProjection list) (inCellOffset : int) =
            let residual = List.rev residualRev

            if isPlainResidual residual then
                Some (ManagedPointerSource.Byref (root, residual), inCellOffset)
            else
                None

        match ptr with
        | ManagedPointerSource.Null
        | ManagedPointerSource.NativeIntPlaceholder _ -> None
        | ManagedPointerSource.Byref (root, projs) ->
            match List.rev projs with
            | [] -> Some (ptr, 0)
            | ByrefProjection.ByteOffset n :: ByrefProjection.ReinterpretAs _ :: revRest -> tryReturn root revRest n
            | ByrefProjection.ReinterpretAs _ :: revRest -> tryReturn root revRest 0
            | _ -> None

    /// True when copying the src cell wholesale into the dest cell would
    /// preserve the dest cell's CLI shape: same primitive constructor for
    /// primitives/refs/pointers, same declared concrete type for
    /// value-type structs. This matters because the typed write paths
    /// (`writeRootValue` for `ArrayElement`/`HeapObjectField`/`HeapValue`)
    /// overwrite wholesale and would silently rewrite the cell's shape on
    /// a mismatch.
    let private cellsHaveCompatibleShape (a : CliType) (b : CliType) : bool =
        match a, b with
        | CliType.Bool _, CliType.Bool _
        | CliType.Char _, CliType.Char _
        | CliType.ObjectRef _, CliType.ObjectRef _
        | CliType.RuntimePointer _, CliType.RuntimePointer _ -> true
        | CliType.Numeric a, CliType.Numeric b ->
            match a, b with
            | CliNumericType.Int8 _, CliNumericType.Int8 _
            | CliNumericType.UInt8 _, CliNumericType.UInt8 _
            | CliNumericType.Int16 _, CliNumericType.Int16 _
            | CliNumericType.UInt16 _, CliNumericType.UInt16 _
            | CliNumericType.Int32 _, CliNumericType.Int32 _
            | CliNumericType.Int64 _, CliNumericType.Int64 _
            | CliNumericType.NativeInt _, CliNumericType.NativeInt _
            | CliNumericType.NativeFloat _, CliNumericType.NativeFloat _
            | CliNumericType.Float32 _, CliNumericType.Float32 _
            | CliNumericType.Float64 _, CliNumericType.Float64 _ -> true
            | _ -> false
        | CliType.ValueType a, CliType.ValueType b -> a.Declared = b.Declared
        | _ -> false

    /// True for byref roots whose cell shape is non-trivial (each root
    /// designates a typed cell, possibly non-byte-addressable). The byte-walk
    /// path through `readManagedByrefBytesAs` works for byte-addressable
    /// cells under these roots, but fails for non-byte-addressable cells
    /// (ObjectRef, RuntimePointer, value types containing those); the
    /// cell-aware fast path handles both. Byte-storage roots
    /// (`StackMemoryByte`, `NativeMemoryByte`, `PeByteRange`,
    /// `StringCharAt`) and stack-slot roots (`LocalVariable`, `Argument`,
    /// `StaticField`) are not considered here — the byte-walk path is the
    /// modelled access shape for them, and stripping to a `[]` residual
    /// for a `readManagedByref` call into a byte-storage root would fail
    /// because the root carries no typed cell at arbitrary byte offsets.
    let private rootIsCellAware (root : ByrefRoot) : bool =
        match root with
        | ByrefRoot.ArrayElement _
        | ByrefRoot.HeapValue _
        | ByrefRoot.HeapObjectField _
        | ByrefRoot.MethodTableExposedClassObject _ -> true
        | ByrefRoot.LocalVariable _
        | ByrefRoot.Argument _
        | ByrefRoot.StackMemoryByte _
        | ByrefRoot.NativeMemoryByte _
        | ByrefRoot.PeByteRange _
        | ByrefRoot.StaticField _
        | ByrefRoot.StringCharAt _ -> false

    /// True if the residual byref (after stripping its byte-view suffix)
    /// is anchored on a cell-aware root. Used to decide whether to attempt
    /// a whole-cell typed read; for byte-storage roots the byte-walk path
    /// is the modelled access shape and we must not bypass it.
    let private byrefAnchorsCellAwareRoot (ptr : ManagedPointerSource) : bool =
        match ptr with
        | ManagedPointerSource.Byref (root, _) -> rootIsCellAware root
        | _ -> false

    /// Attempt to copy a single whole cell, starting at byte offset `i` in
    /// the buffer. Returns `Some cellSize` when the move succeeded; in that
    /// case `state` has been updated and the caller must advance `i` by
    /// `cellSize` bytes. Returns `None` when the cell-aware path is not
    /// applicable; the caller must then fall back to a single byte step.
    ///
    /// The path is taken iff both src and dest byrefs are anchored on
    /// cell-aware roots (see `rootIsCellAware`), strip to a typed
    /// residual at the same intra-cell byte offset (which must be 0 for a
    /// forward step, or `cellSize - 1` for a backward step), the residual
    /// cells have compatible CLI shape (so the wholesale typed write does
    /// not silently change the dest's shape), and there are at least
    /// `cellSize` bytes remaining in the requested copy. The path is the
    /// only correct option for non-byte-addressable cells (`ObjectRef`,
    /// `RuntimePointer`, value-type cells containing those); for
    /// byte-addressable cells under cell-aware roots it remains correct
    /// (and faster than the byte-by-byte loop), so we take it
    /// unconditionally when the preconditions hold rather than
    /// restricting to non-byte-addressable cells only.
    let private tryWholeCellMoveAt
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (dest : ManagedPointerSource)
        (bytesRemaining : int)
        (backwards : bool)
        : (IlMachineState * int) option
        =
        if not (byrefAnchorsCellAwareRoot src && byrefAnchorsCellAwareRoot dest) then
            None
        else

        match inCellOffsetAndStripByteView src, inCellOffsetAndStripByteView dest with
        | Some (srcPlain, srcInCell), Some (destPlain, destInCell) when srcInCell = destInCell ->
            let srcCell = IlMachineState.readManagedByref baseClassTypes state srcPlain
            let cellSize = CliType.sizeOf srcCell

            let aligned =
                if backwards then
                    srcInCell = cellSize - 1
                else
                    srcInCell = 0

            // Reject `cellSize <= 0`: a zero-sized value-type cell (PawPrint
            // gives fieldless default-layout structs `sizeOf = 0`) would
            // return `Some (newState, 0)`, and the caller would advance `i`
            // by zero — spinning the copy loop forever for any positive
            // requested byte count. Falling back to the byte step is also
            // wrong here (we'd read a byte off a non-existent cell), so the
            // caller's byte step would itself fail loudly — which is the
            // intended behaviour, since a positive byte copy against a
            // zero-sized cell anchor is an interpreter-bug shape.
            if not aligned || cellSize <= 0 || cellSize > bytesRemaining then
                None
            else
                let destCell = IlMachineState.readManagedByref baseClassTypes state destPlain

                if CliType.sizeOf destCell <> cellSize then
                    None
                elif not (cellsHaveCompatibleShape srcCell destCell) then
                    None
                else
                    let newState =
                        IlMachineState.writeManagedByrefWithBase baseClassTypes state destPlain srcCell

                    Some (newState, cellSize)
        | _ -> None

    let private copy
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (dest : ManagedPointerSource)
        (src : ManagedPointerSource)
        (byteCount : int)
        : IlMachineState
        =
        let byteConcreteType = byteType baseClassTypes state
        let mutable state = state

        if shouldCopyBackwards baseClassTypes state src dest byteCount then
            let mutable i = byteCount - 1

            while i >= 0 do
                let srcAtI =
                    ManagedPointerByteView.addByteOffset baseClassTypes state byteConcreteType i src

                let destAtI =
                    ManagedPointerByteView.addByteOffset baseClassTypes state byteConcreteType i dest

                // For a backward step we are at byte `i`, which must be the
                // *last* byte of its cell (intra-cell offset `cellSize - 1`)
                // for a whole-cell move to be safe; the move covers bytes
                // `[i - cellSize + 1, i]` and advances `i` backwards by
                // `cellSize`.
                match tryWholeCellMoveAt baseClassTypes state srcAtI destAtI (i + 1) true with
                | Some (newState, cellSize) ->
                    state <- newState
                    i <- i - cellSize
                | None ->
                    let value = readByte baseClassTypes state srcAtI
                    state <- writeByte baseClassTypes state destAtI value
                    i <- i - 1
        else
            let mutable i = 0

            while i < byteCount do
                let srcAtI =
                    ManagedPointerByteView.addByteOffset baseClassTypes state byteConcreteType i src

                let destAtI =
                    ManagedPointerByteView.addByteOffset baseClassTypes state byteConcreteType i dest

                match tryWholeCellMoveAt baseClassTypes state srcAtI destAtI (byteCount - i) false with
                | Some (newState, cellSize) ->
                    state <- newState
                    i <- i + cellSize
                | None ->
                    let value = readByte baseClassTypes state srcAtI
                    state <- writeByte baseClassTypes state destAtI value
                    i <- i + 1

        state

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
                    copy ctx.BaseClassTypes state dest src byteCount

            NativeHandlerResult.completed state |> Some
        | _ -> None

    /// Dispatches the InternalCall (FCall) variants of `System.Buffer` that
    /// take byref `byte` endpoints rather than the QCall pointer endpoints.
    ///
    /// This handler wires `BulkMoveWithWriteBarrierInternal` into native
    /// dispatch and implements CoreCLR's FCall short-circuits
    /// (`dst != src && byteCount != 0`, see comutilnative.cpp); the actual
    /// move reuses the shared `copy` helper. The BCL's primary callers
    /// (`Buffer.Memmove<T>` for `T` containing references, `Array.Copy` of
    /// reference-typed arrays, the reflection-cache growth path, etc.)
    /// hand in byrefs that land on non-byte-addressable cells (object
    /// references, value types containing object references); `copy`
    /// detects cell-aligned ranges via `tryWholeCellMoveAt` and moves
    /// whole typed cells through `readManagedByref` /
    /// `writeManagedByrefWithBase` so the dest cell's CLI shape and the
    /// stored ObjectRef provenance are preserved.
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
            // must not fall through to `copy` — `validateByteAddressableCell`
            // would reject it.
            let state =
                if byteCount = 0 || dest = src then
                    state
                else
                    copy ctx.BaseClassTypes state dest src byteCount

            NativeHandlerResult.completed state |> Some
        | _ -> None
