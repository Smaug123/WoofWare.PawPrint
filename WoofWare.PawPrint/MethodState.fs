namespace WoofWare.PawPrint

open System.Collections.Immutable

/// Accumulates IL prefix opcodes (ECMA-335 III.2) that have been executed but not yet
/// consumed by their target instruction. Multiple prefixes can stack on a single
/// target instruction (e.g. `volatile. unaligned. ldfld`), so this is a record of
/// independent slots rather than a single-slot DU.
///
/// Prefixes apply to the immediately-following instruction; after the target instruction
/// consumes the state, callers must reset it back to `PrefixState.empty`.
type PrefixState =
    {
        /// `constrained. T` (III.2.1) — applies to the next `callvirt`.
        Constrained : ConcreteTypeHandle option
        /// `volatile.` (III.2.5) — applies to the next ldind/stind/ldfld/stfld/ldobj/stobj/initblk/cpblk.
        Volatile : bool
        /// `tail.` (III.2.4) — applies to the next call/callvirt/calli.
        Tail : bool
        /// `unaligned. alignment` (III.2.3) — applies to the next ldind/stind/ldfld/stfld/ldobj/stobj/initblk/cpblk.
        Unaligned : uint8 option
        /// `readonly.` (III.2.2) — applies to the next ldelema.
        Readonly : bool
    }

[<RequireQualifiedAccess>]
module PrefixState =
    let empty : PrefixState =
        {
            Constrained = None
            Volatile = false
            Tail = false
            Unaligned = None
            Readonly = false
        }

/// Whether a new `localloc` block should expose zeroed bytes immediately.
[<RequireQualifiedAccess>]
type LocalMemoryInitialization =
    | ZeroInitialized
    | Uninitialized

/// Frame-owned typed-cell storage for `localloc`. A block stores typed CliType
/// "cells" at known offsets together with a sparse byte overlay for raw byte
/// writes that don't correspond to a typed cell. Bytes that are not covered by
/// either representation default to the block's `Initialization`.
///
/// Cells preserve provenance — for example, `NativeIntSource.FieldHandlePtr`
/// stays a tagged pointer rather than collapsing to its bit pattern. A byte
/// view of a non-byte-addressable cell intentionally fails through the
/// `CliType` byte helpers; that fail-fast is the seam that catches accidental
/// provenance loss.
///
/// Pointers into a block are valid only while the owning method frame is live;
/// if one escapes, later deref fails visibly because the frame-local pool is
/// gone.
type LocalMemoryBlock =
    {
        Size : int
        Initialization : LocalMemoryInitialization
        /// Offset -> typed cell. Cells must not overlap each other; cell ranges
        /// are also disjoint from `Bytes` keys.
        Cells : Map<int, CliType>
        /// Offset -> raw byte. Keys must not lie inside any cell range.
        Bytes : Map<int, byte>
    }

type LocalMemoryPool =
    {
        NextBlockId : int
        Blocks : Map<LocallocBlockId, LocalMemoryBlock>
    }

/// A primitive byte read from a localloc block, classified by where the byte
/// came from. Callers walking byte ranges use the `Cell` arm to amortise per-
/// cell processing; the byte arms describe a single byte each.
[<RequireQualifiedAccess>]
type LocalMemoryByteSource =
    /// Byte lies inside a typed cell. The cell starts at `cellOffset` and
    /// reading byte `n` of it requires the existing byte helpers
    /// (`CliType.BytesAt`); callers must respect the cell's byte
    /// addressability.
    | Cell of cellOffset : int * cell : CliType
    /// Byte was written through the raw byte overlay.
    | Overlay of byte
    /// Byte was never written but the block was zero-initialised.
    | DefaultZero
    /// Byte was never written and the block is uninitialised. Reads must
    /// fail visibly.
    | Uninitialized

[<RequireQualifiedAccess>]
module LocalMemoryPool =
    let empty : LocalMemoryPool =
        {
            NextBlockId = 0
            Blocks = Map.empty
        }

    let private checkRange
        (operation : string)
        (blockId : LocallocBlockId)
        (blockLength : int)
        (byteOffset : int)
        (byteCount : int)
        : unit
        =
        if byteOffset < 0 then
            failwith $"%s{operation}: negative byte offset %d{byteOffset} in %O{blockId}"

        if byteCount < 0 then
            failwith $"%s{operation}: negative byte count %d{byteCount} in %O{blockId}"

        let rangeEnd = int64 byteOffset + int64 byteCount

        if rangeEnd > int64 blockLength then
            failwith
                $"%s{operation}: byte range [%d{byteOffset}, %d{rangeEnd}) is outside %O{blockId} of length %d{blockLength}"

    let allocate
        (initialization : LocalMemoryInitialization)
        (byteCount : int)
        (pool : LocalMemoryPool)
        : LocallocBlockId * LocalMemoryPool
        =
        if byteCount < 0 then
            failwith $"LocalMemoryPool.allocate: negative byte count %d{byteCount}"

        let blockId = LocallocBlockId pool.NextBlockId

        let block =
            {
                Size = byteCount
                Initialization = initialization
                Cells = Map.empty
                Bytes = Map.empty
            }

        blockId,
        { pool with
            NextBlockId = pool.NextBlockId + 1
            Blocks = pool.Blocks |> Map.add blockId block
        }

    let getBlock (blockId : LocallocBlockId) (pool : LocalMemoryPool) : LocalMemoryBlock =
        match pool.Blocks |> Map.tryFind blockId with
        | Some block -> block
        | None -> failwith $"Local memory block %O{blockId} is not live in this method frame"

    let private setBlock
        (blockId : LocallocBlockId)
        (block : LocalMemoryBlock)
        (pool : LocalMemoryPool)
        : LocalMemoryPool
        =
        { pool with
            Blocks = pool.Blocks |> Map.add blockId block
        }

    let private rangesIntersect (aOffset : int) (aSize : int) (bOffset : int) (bSize : int) : bool =
        aOffset < bOffset + bSize && bOffset < aOffset + aSize

    /// Find the unique cell whose covered range contains `offset`, if any.
    /// Cells don't overlap, so at most one matches. F# `Map` iterates entries
    /// in key order, so we can stop as soon as a cell starts past `offset`
    /// (no later cell can contain `offset`) or as soon as we find a cover.
    let private tryFindCellCovering' (offset : int) (block : LocalMemoryBlock) : (int * CliType) option =
        use enumerator =
            (block.Cells :> System.Collections.Generic.IEnumerable<_>).GetEnumerator ()

        let mutable result = None
        let mutable continueScan = true

        while continueScan && enumerator.MoveNext () do
            let kvp = enumerator.Current
            let cellOffset = kvp.Key
            let cell = kvp.Value

            if cellOffset > offset then
                continueScan <- false
            else
                let cellSize = CliType.sizeOf cell

                if offset < cellOffset + cellSize then
                    result <- Some (cellOffset, cell)
                    continueScan <- false

        result

    /// Find the unique cell whose covered range contains `offset`, if any.
    /// Returns `None` for an in-range offset that no cell covers, and also for
    /// an out-of-range `offset` (consistent with the `try` prefix).
    let tryFindCellCovering
        (blockId : LocallocBlockId)
        (offset : int)
        (pool : LocalMemoryPool)
        : (int * CliType) option
        =
        let block = getBlock blockId pool

        if offset < 0 || offset >= block.Size then
            None
        else
            tryFindCellCovering' offset block

    /// Return the cell that begins at exactly `offset`, if any.
    let tryReadCell (blockId : LocallocBlockId) (offset : int) (pool : LocalMemoryPool) : CliType option =
        let block = getBlock blockId pool
        Map.tryFind offset block.Cells

    /// Classify a single byte position. Callers walking byte ranges use the
    /// `Cell` arm to dispatch through the existing typed-cell byte helpers.
    let private readByteSource
        (blockId : LocallocBlockId)
        (offset : int)
        (pool : LocalMemoryPool)
        : LocalMemoryByteSource
        =
        let block = getBlock blockId pool
        checkRange "LocalMemoryPool.readByteSource" blockId block.Size offset 1

        match tryFindCellCovering' offset block with
        | Some (cellOffset, cell) -> LocalMemoryByteSource.Cell (cellOffset, cell)
        | None ->
            match Map.tryFind offset block.Bytes with
            | Some b -> LocalMemoryByteSource.Overlay b
            | None ->
                match block.Initialization with
                | LocalMemoryInitialization.ZeroInitialized -> LocalMemoryByteSource.DefaultZero
                | LocalMemoryInitialization.Uninitialized -> LocalMemoryByteSource.Uninitialized

    /// Remove any cells or byte-overlay entries intersecting
    /// `[offset, offset + count)`. Cells are removed wholesale even if they
    /// only partially overlap the requested range.
    let private evictRangeInBlock (offset : int) (count : int) (block : LocalMemoryBlock) : LocalMemoryBlock =
        if count <= 0 then
            block
        else
            let cells =
                block.Cells
                |> Map.filter (fun cellOffset cell ->
                    not (rangesIntersect cellOffset (CliType.sizeOf cell) offset count)
                )

            let bytes =
                block.Bytes
                |> Map.filter (fun byteOffset _ -> byteOffset < offset || byteOffset >= offset + count)

            if
                Map.count cells = Map.count block.Cells
                && Map.count bytes = Map.count block.Bytes
            then
                block
            else
                { block with
                    Cells = cells
                    Bytes = bytes
                }

    /// Insert a typed cell at `offset`, evicting any cells/bytes whose range
    /// intersects the new cell. The caller is responsible for ensuring the
    /// value is the intended typed view; provenance carried by the value
    /// (such as `NativeIntSource.FieldHandlePtr`) is preserved.
    let writeCell
        (blockId : LocallocBlockId)
        (offset : int)
        (value : CliType)
        (pool : LocalMemoryPool)
        : LocalMemoryPool
        =
        let block = getBlock blockId pool
        let size = CliType.sizeOf value
        checkRange "LocalMemoryPool.writeCell" blockId block.Size offset size

        let evicted = evictRangeInBlock offset size block

        let updated =
            { evicted with
                Cells = evicted.Cells |> Map.add offset value
            }

        setBlock blockId updated pool

    /// Replace an existing cell at `cellOffset` whose new size does not exceed
    /// the prior size. Used by the byte-write path to install an updated cell
    /// produced by `CliType.WithBytesAtIfChanged`. Throws if no cell exists at
    /// `cellOffset` or the new value's size differs from the existing cell.
    let private replaceCell
        (blockId : LocallocBlockId)
        (cellOffset : int)
        (updated : CliType)
        (pool : LocalMemoryPool)
        : LocalMemoryPool
        =
        let block = getBlock blockId pool

        match Map.tryFind cellOffset block.Cells with
        | None ->
            failwith
                $"LocalMemoryPool.replaceCell: no cell at offset %d{cellOffset} in %O{blockId} (this is an interpreter bug)"
        | Some existing ->
            let existingSize = CliType.sizeOf existing
            let updatedSize = CliType.sizeOf updated

            if existingSize <> updatedSize then
                failwith
                    $"LocalMemoryPool.replaceCell: refusing to change cell size at offset %d{cellOffset} in %O{blockId} (was %d{existingSize}, would be %d{updatedSize})"

            let block =
                { block with
                    Cells = block.Cells |> Map.add cellOffset updated
                }

            setBlock blockId block pool

    /// Write a single byte through the byte overlay. Caller must ensure
    /// `offset` does not lie inside any cell — typically by walking the cell
    /// covering check first and routing cell-resident writes through
    /// `replaceCell` instead.
    let private writeOverlayByte
        (blockId : LocallocBlockId)
        (offset : int)
        (value : byte)
        (pool : LocalMemoryPool)
        : LocalMemoryPool
        =
        let block = getBlock blockId pool
        checkRange "LocalMemoryPool.writeOverlayByte" blockId block.Size offset 1

        match tryFindCellCovering' offset block with
        | Some (cellOffset, _) ->
            failwith
                $"LocalMemoryPool.writeOverlayByte: byte offset %d{offset} lies inside cell at %d{cellOffset} in %O{blockId} (this is an interpreter bug)"
        | None ->
            let block =
                { block with
                    Bytes = block.Bytes |> Map.add offset value
                }

            setBlock blockId block pool

    /// Read `count` bytes starting at `offset`, returning `ValueNone` when any
    /// byte in the range is uninitialised or lies inside a cell whose typed
    /// view is not byte-addressable. Used by writers that want to short-circuit
    /// a write when the bytes already match.
    let tryReadBytes
        (blockId : LocallocBlockId)
        (offset : int)
        (count : int)
        (pool : LocalMemoryPool)
        : byte[] voption
        =
        let block = getBlock blockId pool
        let rangeEnd = int64 offset + int64 count

        if offset < 0 || count < 0 || rangeEnd > int64 block.Size then
            ValueNone
        else
            let result = Array.zeroCreate<byte> count
            let mutable readable = true
            let mutable i = 0

            while readable && i < count do
                match readByteSource blockId (offset + i) pool with
                | LocalMemoryByteSource.Cell (cellOffset, cell) ->
                    match CliType.ByteAddressability cell with
                    | CliByteAddressability.ByteAddressable ->
                        let inCellOffset = offset + i - cellOffset
                        let cellSize = CliType.sizeOf cell
                        let take = min (cellSize - inCellOffset) (count - i)
                        let bytes = CliType.BytesAt inCellOffset take cell
                        Array.blit bytes 0 result i take
                        i <- i + take
                    | CliByteAddressability.Rejected _ -> readable <- false
                | LocalMemoryByteSource.Overlay b ->
                    result.[i] <- b
                    i <- i + 1
                | LocalMemoryByteSource.DefaultZero ->
                    result.[i] <- 0uy
                    i <- i + 1
                | LocalMemoryByteSource.Uninitialized -> readable <- false

            if readable then ValueSome result else ValueNone

    /// Read `count` bytes starting at `offset`. Throws if the range is out of
    /// bounds, contains uninitialised bytes, or crosses a cell whose typed
    /// view is not byte-addressable (a tagged-pointer cell, for instance).
    let readBytes (blockId : LocallocBlockId) (offset : int) (count : int) (pool : LocalMemoryPool) : byte[] =
        let block = getBlock blockId pool
        checkRange "LocalMemoryPool.readBytes" blockId block.Size offset count

        let result = Array.zeroCreate<byte> count
        let mutable i = 0

        while i < count do
            let pos = offset + i

            match readByteSource blockId pos pool with
            | LocalMemoryByteSource.Cell (cellOffset, cell) ->
                match CliType.ByteAddressability cell with
                | CliByteAddressability.ByteAddressable ->
                    let inCellOffset = pos - cellOffset
                    let cellSize = CliType.sizeOf cell
                    let take = min (cellSize - inCellOffset) (count - i)
                    let bytes = CliType.BytesAt inCellOffset take cell
                    Array.blit bytes 0 result i take
                    i <- i + take
                | CliByteAddressability.Rejected rejection ->
                    failwith
                        $"LocalMemoryPool.readBytes: refusing byte view over %s{rejection.Description} at offset %d{cellOffset} in %O{blockId}"
            | LocalMemoryByteSource.Overlay b ->
                result.[i] <- b
                i <- i + 1
            | LocalMemoryByteSource.DefaultZero ->
                result.[i] <- 0uy
                i <- i + 1
            | LocalMemoryByteSource.Uninitialized ->
                failwith $"LocalMemoryPool.readBytes: byte at offset %d{pos} in %O{blockId} is uninitialised"

        result

    /// Scatter `bytes` into the block starting at `offset`. Bytes that fall
    /// inside an existing cell are merged through `CliType.WithBytesAtIfChanged`
    /// (preserving the cell's typed shape) and replace the cell in place via
    /// `replaceCell`. Bytes outside any cell are written through the byte
    /// overlay via `writeOverlayByte`. Throws if the range is out of bounds or
    /// crosses a non-byte-addressable cell.
    let writeBytes
        (blockId : LocallocBlockId)
        (offset : int)
        (bytes : byte[])
        (pool : LocalMemoryPool)
        : LocalMemoryPool
        =
        let block = getBlock blockId pool
        checkRange "LocalMemoryPool.writeBytes" blockId block.Size offset bytes.Length

        let mutable pool = pool
        let mutable filled = 0

        while filled < bytes.Length do
            let pos = offset + filled

            match tryFindCellCovering blockId pos pool with
            | Some (cellOffset, cell) ->
                match CliType.ByteAddressability cell with
                | CliByteAddressability.ByteAddressable ->
                    let inCellOffset = pos - cellOffset
                    let cellSize = CliType.sizeOf cell
                    let canTake = cellSize - inCellOffset
                    let take = min canTake (bytes.Length - filled)
                    let cellBytes = bytes.[filled .. filled + take - 1]

                    match CliType.WithBytesAtIfChanged inCellOffset cellBytes cell with
                    | None -> ()
                    | Some updatedCell -> pool <- replaceCell blockId cellOffset updatedCell pool

                    filled <- filled + take
                | CliByteAddressability.Rejected rejection ->
                    failwith
                        $"LocalMemoryPool.writeBytes: refusing byte view over %s{rejection.Description} at offset %d{cellOffset} in %O{blockId}"
            | None ->
                pool <- writeOverlayByte blockId pos bytes.[filled] pool
                filled <- filled + 1

        pool

type MethodReturnState =
    {
        /// Handle to the caller's frame
        JumpTo : FrameId
        WasInitialisingType : ConcreteTypeHandle option
        /// The Newobj instruction means we need to push a reference immediately after Ret.
        WasConstructingObj : ManagedHeapAddress option
        /// The IL offset of the call/callvirt/newobj instruction in the caller that created
        /// this frame. Exception dispatch must use this (not the caller's resumed IlOpIndex)
        /// so that handler lookup sees the call site inside the protected region, even when
        /// the advanced resume PC falls outside it.
        CallSiteIlOpIndex : int
        /// When true, the constructed object (WasConstructingObj) should be dispatched as a
        /// managed exception on return instead of being pushed onto the caller's eval stack.
        /// Used by raiseRuntimeException to run exception ctors via the dispatch loop.
        DispatchAsExceptionOnReturn : bool
    }

and MethodState =
    {
        // TODO: local variables are initialised to 0 if the localsinit flag is set for the method
        LocalVariables : CliType ImmutableArray
        /// Index into the stream of IL bytes.
        _IlOpIndex : int
        EvaluationStack : EvalStack
        Arguments : CliType ImmutableArray
        ExecutingMethod : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        LocalMemoryPool : LocalMemoryPool
        /// On return, we restore this state. This should be Some almost always; an exception is the entry point.
        ReturnState : MethodReturnState option
        Generics : ImmutableArray<ConcreteTypeHandle>
        /// Track which exception regions are currently active (innermost first)
        ActiveExceptionRegions : ExceptionRegion list
        /// When executing finally/fault/filter bodies, we need to know how to resume.
        /// Nested EH inside those bodies pushes a new continuation over the outer one.
        ExceptionContinuations :
            ExceptionContinuationFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> list
        /// Active catch/filter handler body -> caught exception.
        /// TODO: replace with a push/pop active-catch stack so escaped handlers cannot leave stale entries.
        CatchExceptions : Map<ExceptionOffset, CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>>
        /// Prefix opcodes (constrained./volatile./tail./unaligned./readonly.) executed but
        /// not yet consumed by the following instruction. Reset to `PrefixState.empty` after
        /// consumption.
        PendingPrefix : PrefixState
    }

    member this.IlOpIndex = this._IlOpIndex

    member this.ExceptionContinuation =
        this.ExceptionContinuations |> List.tryHead |> Option.map _.Continuation

    /// Set the program counter to an absolute byte offset from the start of the method.
    static member setProgramCounter (absoluteOffset : int) (state : MethodState) =
        let jumped =
            { state with
                _IlOpIndex = absoluteOffset
            }

        let newActiveRegions =
            ExceptionHandling.getActiveRegionsAtOffset jumped.IlOpIndex state.ExecutingMethod

        { jumped with
            ActiveExceptionRegions = newActiveRegions
        }


    static member jumpProgramCounter (bytes : int) (state : MethodState) =
        MethodState.setProgramCounter (state._IlOpIndex + bytes) state

    static member advanceProgramCounter (state : MethodState) =
        let instruction =
            match state.ExecutingMethod.Body with
            | MethodBody.Il instr -> instr.Locations.[state.IlOpIndex]
            | other ->
                failwith
                    $"advanceProgramCounter: executing method %O{state.ExecutingMethod} has no IL body (Body=%A{other})"

        MethodState.jumpProgramCounter (IlOp.NumberOfBytes instruction) state

    static member peekEvalStack (state : MethodState) : EvalStackValue option = EvalStack.Peek state.EvaluationStack

    static member clearEvalStack (state : MethodState) : MethodState =
        { state with
            EvaluationStack = EvalStack.Empty
        }

    static member pushExceptionContinuation
        (scope : ExceptionContinuationScope)
        (cont : ExceptionContinuation<_, _, _>)
        (state : MethodState)
        : MethodState
        =
        match scope, cont with
        | ExceptionContinuationScope.FilterHandler _, ExceptionContinuation.ResumeAfterFilter _
        | ExceptionContinuationScope.FinallyHandler _, ExceptionContinuation.ResumeAfterFinally _
        | ExceptionContinuationScope.FinallyHandler _, ExceptionContinuation.PropagatingException _
        | ExceptionContinuationScope.FaultHandler _, ExceptionContinuation.PropagatingException _ -> ()
        | _ -> failwith $"Exception continuation scope %O{scope} does not match continuation %O{cont}"

        { state with
            ExceptionContinuations =
                {
                    Scope = scope
                    Continuation = cont
                }
                :: state.ExceptionContinuations
        }

    static member popExceptionContinuation
        (state : MethodState)
        : ExceptionContinuationFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> option * MethodState
        =
        match state.ExceptionContinuations with
        | [] -> None, state
        | head :: tail ->
            Some head,
            { state with
                ExceptionContinuations = tail
            }

    /// Store the full caught exception for `rethrow`, which must preserve the original
    /// stack trace rather than creating a fresh throw record from the eval-stack object.
    static member setCatchException
        (offset : ExceptionOffset)
        (exn : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : MethodState)
        : MethodState
        =
        { state with
            CatchExceptions = state.CatchExceptions |> Map.add offset exn
        }

    static member clearCatchException (offset : ExceptionOffset) (state : MethodState) : MethodState =
        { state with
            CatchExceptions = state.CatchExceptions |> Map.remove offset
        }

    /// Clear any pending prefix opcodes. Must be called whenever the PC is set to a
    /// non-sequential target (exception handler entry, finally entry) so that a prefix
    /// set before the transfer cannot be consumed by an unrelated instruction in the handler.
    static member clearPendingPrefix (state : MethodState) : MethodState =
        { state with
            PendingPrefix = PrefixState.empty
        }

    static member pushToEvalStack' (e : EvalStackValue) (state : MethodState) : MethodState =
        { state with
            EvaluationStack = EvalStack.Push' e state.EvaluationStack
        }

    static member pushToEvalStack (o : CliType) (state : MethodState) : MethodState =
        { state with
            EvaluationStack = EvalStack.Push o state.EvaluationStack
        }

    /// Pop the eval stack into the given argument slot.
    static member popFromStackToArg (index : int) (state : MethodState) : MethodState =
        let popped, state = MethodState.popFromStack state

        let arg =
            if index < state.Arguments.Length then
                state.Arguments.[index]
            else
                failwith
                    $"Tried to get element {index} of the args list for method {state.ExecutingMethod.Name}, which has only {state.Arguments.Length} elements"

        let popped = EvalStackValue.toCliTypeCoerced arg popped

        { state with
            Arguments = state.Arguments.SetItem (index, popped)
        }

    static member loadArgument (index : int) (state : MethodState) : MethodState =
        // Correct CIL guarantees that we are loading an argument from an index that exists.
        MethodState.pushToEvalStack state.Arguments.[index] state

    static member popFromStack (state : MethodState) : EvalStackValue * MethodState =
        let popped, newStack = EvalStack.Pop state.EvaluationStack

        let state =
            { state with
                EvaluationStack = newStack
            }

        popped, state

    static member popFromStackToVariable (localVariableIndex : int) (state : MethodState) : MethodState =
        if localVariableIndex >= state.LocalVariables.Length then
            failwith
                $"Tried to access zero-indexed local variable %i{localVariableIndex} but only %i{state.LocalVariables.Length} exist"

        if localVariableIndex < 0 || localVariableIndex >= 65535 then
            failwith $"Incorrect CIL encountered: local variable index has value %i{localVariableIndex}"

        let popped, state = MethodState.popFromStack state

        let desiredValue =
            EvalStackValue.toCliTypeCoerced state.LocalVariables.[localVariableIndex] popped

        { state with
            LocalVariables = state.LocalVariables.SetItem (localVariableIndex, desiredValue)
        }

    /// `args` must be populated with entries of the right type.
    /// If `method` is an instance method, `args` must be of length 1+numParams.
    /// If `method` is static, `args` must be of length numParams.
    static member Empty
        (concreteTypes : AllConcreteTypes)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (loadedAssemblies : ImmutableDictionary<string, DumpedAssembly>)
        (containingAssembly : DumpedAssembly)
        (method : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (args : ImmutableArray<CliType>)
        (returnState : MethodReturnState option)
        : Result<MethodState, WoofWare.PawPrint.AssemblyReference list>
        =
        do
            if method.IsStatic then
                if args.Length <> method.Parameters.Length then
                    failwith
                        $"Static method {method.Name} should have had %i{method.Parameters.Length} parameters, but was given %i{args.Length}"
            else if args.Length <> method.Parameters.Length + 1 then
                failwith
                    $"Non-static method {method.Name} should have had %i{method.Parameters.Length + 1} parameters, but was given %i{args.Length}"

        let localVariableSig =
            match MethodInfo.tryIlBody method with
            | None -> ImmutableArray.Empty
            | Some instr ->
                match instr.LocalVars with
                | None -> ImmutableArray.Empty
                | Some vars -> vars
        // I think valid code should remain valid if we unconditionally localsInit - it should be undefined
        // to use an uninitialised value? Not checked this; TODO.

        let localVars =
            let result = ImmutableArray.CreateBuilder ()

            for var in localVariableSig do
                // Note: This assumes all types have already been concretized
                // If this fails with "ConcreteTypeHandle not found", it means
                // we need to ensure types are concretized before creating the MethodState
                let zero, _ = CliType.zeroOf concreteTypes loadedAssemblies baseClassTypes var
                result.Add zero

            result.ToImmutable ()

        let activeRegions = ExceptionHandling.getActiveRegionsAtOffset 0 method

        {
            EvaluationStack = EvalStack.Empty
            LocalVariables = localVars
            _IlOpIndex = 0
            Arguments = args
            ExecutingMethod = method
            LocalMemoryPool = LocalMemoryPool.empty
            ReturnState = returnState
            Generics = methodGenerics
            ActiveExceptionRegions = activeRegions
            ExceptionContinuations = []
            CatchExceptions = Map.empty
            PendingPrefix = PrefixState.empty
        }
        |> Ok
