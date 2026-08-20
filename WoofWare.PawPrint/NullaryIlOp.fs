namespace WoofWare.PawPrint

open System
open System.Diagnostics
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NullaryIlOp =
    let private cliCharSizeBytes : int64 = 2L

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

    let private tryManagedPointerAddressBits (state : IlMachineState) (ptr : ManagedPointerSource) : int64 option =
        match ManagedPointerSource.tryStableAddressBits ptr with
        | Some bits -> Some bits
        | None ->
            let projectionByteOffset (projs : ByrefProjection list) : int64 option =
                ((Some 0L), projs)
                ||> List.fold (fun (offset : int64 option) (projection : ByrefProjection) ->
                    match offset with
                    | None -> None
                    | Some offset ->
                        match projection with
                        | ByrefProjection.ReinterpretAs _ -> Some offset
                        | ByrefProjection.ByteOffset n -> Some (offset + int64<int> n)
                        // Field layout does not yet expose stable low address bits.
                        // Returning None keeps struct-field pointer masking explicit.
                        | ByrefProjection.Field _ -> None
                )

            match ptr with
            | ManagedPointerSource.Null -> failwith "unreachable: tryStableAddressBits handles null managed pointers"
            | ManagedPointerSource.NativeIntPlaceholder _ ->
                failwith "unreachable: tryStableAddressBits handles NativeIntPlaceholder managed pointers"
            | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), projs) ->
                // The stride is recorded on the array at allocation, so it is available even
                // for `Array.Empty<T>()`, which has no cell to measure.
                let elementSize = ManagedHeap.getArrayElementStride arr state.ManagedHeap

                projectionByteOffset projs
                |> Option.map (fun byteOffset -> int64<int> index * int64<int> elementSize + byteOffset)
            | ManagedPointerSource.Byref (ByrefRoot.StringCharAt (_, charIndex), projs) ->
                projectionByteOffset projs
                |> Option.map (fun byteOffset -> int64<int> charIndex * cliCharSizeBytes + byteOffset)
            | ManagedPointerSource.Byref _ -> None

    let private andManagedPointerAddressBits
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        (mask : int64)
        : EvalStackValue
        =
        match tryManagedPointerAddressBits state ptr with
        | Some bits -> NativeIntSource.Verbatim (bits &&& mask) |> EvalStackValue.NativeInt
        | None -> failwith $"And: refusing to convert managed pointer %O{ptr} to integer bits"

    /// Mask a type-handle-shaped pointer, if the model can state the answer.
    /// PawPrint models no address for a `MethodTable*` or a `TypeDesc*`, so the
    /// value is `base ||| tag` with `base` unknown and `tag` given by
    /// `TypeHandleTag`; see `TaggedPointerBits` for the decision procedure and
    /// docs/plans/2026-08-06-typehandle-tag-bits.md for the CoreLib IL involved.
    ///
    /// Unlike a GC handle's tag, this tag is a function of the target, so a
    /// changed tag is a change of identity rather than the same pointer retagged.
    /// `untagged` is that other identity where one exists: for a TypeDesc-shaped
    /// `TypeHandlePtr`, clearing the tag is CoreCLR's `TypeHandle.AsTypeDesc`
    /// (`handle & ~2`) and yields the target's `TypeDesc*`. Pointers that carry
    /// no tag to begin with pass `None`; nothing can strip them, so it is never
    /// consulted.
    let private typeHandleMask
        (source : NativeIntSource)
        (tag : int64)
        (untagged : NativeIntSource option)
        (mask : int64)
        : NativeIntSource
        =
        match TaggedPointerBits.bitAnd TypeHandleTag.widthBits tag mask with
        | TaggedPointerBitsResult.TagBitsOnly bits -> NativeIntSource.Verbatim bits
        // The whole unknown base survives and the tag is unchanged, so the result
        // is bit-identical to the input.
        | TaggedPointerBitsResult.Retagged newTag when newTag = tag -> source
        | TaggedPointerBitsResult.Retagged newTag ->
            // `and` can only clear bits, so the only reachable change is clearing
            // the whole tag.
            // Parenthesised because `newTag = 0L` as a bare first argument parses
            // as a named argument, not a comparison.
            Debug.Assert (
                (newTag = 0L),
                $"And on %O{source} produced tag 0x%x{newTag} from 0x%x{tag}; `and` can only clear tag bits"
            )

            match untagged with
            | Some untagged -> untagged
            | None ->
                failwith
                    $"And: refusing to apply 0x%x{mask} to %O{source}; it would clear tag bits 0x%x{tag}, and PawPrint has no untagged identity for this pointer kind"
        | TaggedPointerBitsResult.NotStatable ->
            failwith
                $"And: refusing to apply 0x%x{mask} to %O{source}; the result would depend on the pointer's address, which PawPrint does not model"

    /// Apply a bitwise operation to the low tag region of a GC handle, if the
    /// model can state the answer. See `TaggedPointerBits`: PawPrint does not
    /// model a handle's numeric address, so only operations that either preserve
    /// the whole (unknown) handle or reduce to the known tag bits are answerable.
    let private gcHandleTagOp
        (operation : string)
        (decide : int -> int64 -> int64 -> TaggedPointerBitsResult)
        (handle : GcHandleAddress)
        (tag : int64)
        (operand : int64)
        : NativeIntSource
        =
        match decide TaggedPointerBits.gcHandleTagWidthBits tag operand with
        | TaggedPointerBitsResult.Retagged tag -> NativeIntSource.gcHandlePtrTagged handle tag
        | TaggedPointerBitsResult.TagBitsOnly bits -> NativeIntSource.Verbatim bits
        | TaggedPointerBitsResult.NotStatable ->
            failwith
                $"%s{operation}: refusing to apply 0x%x{operand} to GC handle %O{handle}; the result would depend on the handle's address, which PawPrint does not model"

    /// Mask a byref that `conv.i4` / `conv.u4` has truncated (see
    /// `Int32Source.NarrowedManagedPointer`). The byref is modelled as an unknown
    /// container address with its low `alignmentBits` bits clear, plus a known
    /// in-container offset, so `TaggedPointerBits` decides what the mask can say —
    /// and the truncation does not change the answer, because every bit the mask is
    /// allowed to select lies inside the alignment region, far below the width that
    /// was discarded.
    let private andNarrowedManagedPointerBits
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        (mask : int64)
        : Int32Source
        =
        let refuse (reason : string) : Int32Source =
            failwith $"And: refusing to mask managed pointer %O{ptr}, truncated to 32 bits, with 0x%x{mask}: %s{reason}"

        // A byref whose container has no alignment claim, or no stable in-container
        // offset, is not outside the model: it is an unknown address with an *empty*
        // tag region. `TaggedPointerBits` still answers the two address-independent
        // masks from that — `p & 0` is zero and `p & -1` is the identity, whatever
        // the address — and refuses the rest. Rejecting outright would be a refusal
        // of something answerable, which is the one thing this decision procedure is
        // not allowed to do.
        let alignmentBits, offset =
            match ManagedPointerSource.tryContainerAlignmentBits ptr, tryManagedPointerAddressBits state ptr with
            | Some alignmentBits, Some offset -> alignmentBits, offset
            | _ -> 0, 0L

        match TaggedPointerBits.bitAndOffsetFromAlignedBase alignmentBits offset mask with
        | TaggedPointerBitsResult.TagBitsOnly bits ->
            // `TagBitsOnly` only fires when the mask selects nothing above the
            // container's alignment, so `bits` is a handful of low bits and the
            // narrowing to int32 is exact.
            Debug.Assert (
                (bits &&& ~~~(TaggedPointerBits.tagMask alignmentBits)) = 0L,
                $"masked byref bits 0x%x{bits} escape the %i{alignmentBits}-bit alignment region"
            )

            int32<int64> bits |> Int32Source.Verbatim
        | TaggedPointerBitsResult.Retagged newLowBits when
            newLowBits = (offset &&& TaggedPointerBits.tagMask alignmentBits)
            ->
            // The mask preserved every bit, so the value is unchanged.
            Int32Source.NarrowedManagedPointer ptr
        | TaggedPointerBitsResult.Retagged _ ->
            // Align-down (`p & ~7`). The answer is a *different* byref, which would
            // have to be expressed by walking the offset back; PawPrint has no
            // consumer for that yet, so refuse rather than approximate.
            refuse
                "the result is the same container at a lower offset, which PawPrint does not yet re-express as a byref"
        | TaggedPointerBitsResult.NotStatable ->
            if alignmentBits = 0 then
                refuse
                    "PawPrint claims no alignment for this byref's container, so only masks of 0 and -1 are answerable"
            else
                refuse
                    $"the result would depend on address bits above the container's guaranteed %i{alignmentBits}-bit alignment"

    let private andNativeIntAddressBits
        (state : IlMachineState)
        (source : NativeIntSource)
        (mask : int64)
        : EvalStackValue
        =
        match source with
        | NativeIntSource.Verbatim bits -> NativeIntSource.Verbatim (bits &&& mask) |> EvalStackValue.NativeInt
        | NativeIntSource.ManagedPointer ptr -> andManagedPointerAddressBits state ptr mask
        | NativeIntSource.GcHandlePtr (handle, tag) ->
            gcHandleTagOp "And" TaggedPointerBits.bitAnd handle tag mask
            |> EvalStackValue.NativeInt
        | NativeIntSource.TypeHandlePtr target ->
            let tag = TypeHandleTag.forTarget target

            // A TypeDesc-shaped handle has an untagged identity — the `TypeDesc*`
            // that `AsTypeDesc` produces. A MethodTable-shaped one carries no tag
            // at all, so there is nothing to strip and no second identity.
            let untagged =
                if tag = 0L then
                    None
                else
                    Some (NativeIntSource.TypeDescPtr target)

            typeHandleMask source tag untagged mask |> EvalStackValue.NativeInt
        // A `MethodTable*` and a `TypeDesc*` are both untagged: the alignment that
        // makes the tag region available in the first place still holds, so masking
        // to that region is honestly zero, and any base-preserving mask is the
        // identity.
        | NativeIntSource.MethodTablePtr _
        | NativeIntSource.TypeDescPtr _ -> typeHandleMask source 0L None mask |> EvalStackValue.NativeInt
        | other -> failwith $"can't do binary operation on non-verbatim native int %O{other}"

    /// XOR of two `NativeIntSource` values in the native-int slot. Mirrors
    /// `Int64Source.bitXor`: `Verbatim ^ Verbatim` stays in the `Verbatim`
    /// domain; any other combination routes both operands through
    /// `PointerHashSynthesis.materialiseHashBits` and tags the result with
    /// `OpaqueHashBits` so the synthesised-bits contract propagates (the
    /// result MUST NOT be used as a real pointer). `materialiseHashBits`
    /// fails loudly on `ManagedPointer` (non-null) and
    /// `SyntheticCrossArrayOffset`, preserving byref / cross-storage
    /// provenance.
    let private xorNativeIntSources
        (i1 : NativeIntSource)
        (i2 : NativeIntSource)
        (counters : PointerHashState)
        : NativeIntSource * PointerHashState
        =
        match i1, i2 with
        | NativeIntSource.Verbatim a, NativeIntSource.Verbatim b -> NativeIntSource.Verbatim (a ^^^ b), counters
        // A GC handle's tag region is exactly representable, so flipping bits
        // inside it has an exact answer. Without these arms the pair would fall
        // through to hash synthesis below, which would *silently* replace the
        // handle with opaque bits.
        | NativeIntSource.GcHandlePtr (handle, tag), NativeIntSource.Verbatim operand
        | NativeIntSource.Verbatim operand, NativeIntSource.GcHandlePtr (handle, tag) ->
            gcHandleTagOp "Xor" TaggedPointerBits.bitXor handle tag operand, counters
        | _ ->
            let a, counters = PointerHashSynthesis.materialiseHashBits "Xor" i1 counters
            let b, counters = PointerHashSynthesis.materialiseHashBits "Xor" i2 counters
            NativeIntSource.OpaqueHashBits (a ^^^ b), counters

    /// Bitwise complement of a `NativeIntSource` in the native-int slot, for the
    /// `not` IL instruction (ECMA-335 III.3.35). Mirrors `xorNativeIntSources`,
    /// which is the same operation against an all-ones operand.
    ///
    /// `Verbatim` and the two bit-pattern pointer forms have exact, definitional
    /// bit patterns — PawPrint models `Null` as the bit pattern 0 throughout, and
    /// a `NativeIntPlaceholder` is by construction nothing but the raw bits that
    /// produced it — so their complement is an honest `Verbatim`, which keeps
    /// composing with the verbatim arms of `And` / `Or` / comparisons. Everything else routes
    /// through `PointerHashSynthesis.materialiseHashBits` and is tagged
    /// `OpaqueHashBits`, propagating the synthesised-bits contract: the result is
    /// deterministic but MUST NOT be used as a real pointer. `materialiseHashBits`
    /// fails loudly on any other `ManagedPointer` and on
    /// `SyntheticCrossArrayOffset`, preserving byref / cross-storage provenance.
    ///
    /// A handle-shaped source does not survive a double complement *as itself*:
    /// `~~handle` comes back as `OpaqueHashBits`, not as the handle. Comparing that against
    /// the original handle is nonetheless answered correctly — `equalsForCli` looks the
    /// handle's assigned address up in `PointerHashState` and compares bit patterns — so
    /// `~~handle == handle` and `(handle ^ 0) ^ 0 == handle` are both true, as they are on
    /// real .NET. What is lost is provenance, not the answer: the result can no longer be
    /// dereferenced or narrowed as a pointer, only compared.
    let private notNativeIntSource
        (source : NativeIntSource)
        (counters : PointerHashState)
        : NativeIntSource * PointerHashState
        =
        match source with
        | NativeIntSource.Verbatim i -> NativeIntSource.Verbatim ~~~i, counters
        | NativeIntSource.ManagedPointer ManagedPointerSource.Null -> NativeIntSource.Verbatim ~~~0L, counters
        // The result is an integer, not a pointer, so the placeholder-to-`Null`
        // normalisation that applies when *constructing* byrefs is not needed here.
        | NativeIntSource.ManagedPointer (ManagedPointerSource.NativeIntPlaceholder bits) ->
            NativeIntSource.Verbatim ~~~bits, counters
        | _ ->
            let bits, counters = PointerHashSynthesis.materialiseHashBits "Not" source counters
            NativeIntSource.OpaqueHashBits ~~~bits, counters

    let private locallocSizeBytes (value : EvalStackValue) : int =
        let size =
            match value with
            | EvalStackValue.Int32 int32Source ->
                let i = Int32Source.value "Localloc" int32Source
                int64 i
            | EvalStackValue.Int64 (Int64Source.Verbatim i) -> i
            | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _) ->
                failwith "Localloc: refusing to use synthetic pointer delta as a byte count"
            | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)) ->
                failwith $"Localloc: refusing to use widened native int %O{src} as a byte count"
            | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) ->
                failwith $"Localloc: refusing to use synthesised pointer-hash bits 0x%x{bits} as a byte count"
            | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) -> i
            | EvalStackValue.NativeInt (NativeIntSource.SyntheticCrossArrayOffset _) ->
                failwith "Localloc: refusing to use synthetic pointer delta as a byte count"
            | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer _)
            | EvalStackValue.NativeInt (NativeIntSource.FunctionPointer _)
            | EvalStackValue.NativeInt (NativeIntSource.TypeHandlePtr _)
            | EvalStackValue.NativeInt (NativeIntSource.TypeDescPtr _)
            | EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr _)
            | EvalStackValue.NativeInt (NativeIntSource.MethodTableAuxiliaryDataPtr _)
            | EvalStackValue.NativeInt (NativeIntSource.PerInstInfoPtr _)
            | EvalStackValue.NativeInt (NativeIntSource.PerInstDictPtr _)
            | EvalStackValue.NativeInt (NativeIntSource.FieldHandlePtr _)
            | EvalStackValue.NativeInt (NativeIntSource.MethodHandlePtr _)
            | EvalStackValue.NativeInt (NativeIntSource.GcHandlePtr _)
            | EvalStackValue.NativeInt (NativeIntSource.AssemblyHandle _)
            | EvalStackValue.NativeInt (NativeIntSource.ModuleHandle _)
            | EvalStackValue.NativeInt (NativeIntSource.MetadataImportHandle _)
            | EvalStackValue.NativeInt (NativeIntSource.EventPipeProviderPtr _)
            | EvalStackValue.NativeInt (NativeIntSource.EventPipeEventPtr _)
            | EvalStackValue.NativeInt (NativeIntSource.LowLevelMonitorPtr _)
            | EvalStackValue.NativeInt (NativeIntSource.WaitHandlePtr _) ->
                failwith $"Localloc: refusing to use pointer-like value %O{value} as a byte count"
            | EvalStackValue.NativeInt (NativeIntSource.OpaqueHashBits bits) ->
                failwith $"Localloc: refusing to use synthesised pointer-hash bits 0x%x{bits} as a byte count"
            | EvalStackValue.ManagedPointer _
            | EvalStackValue.NullObjectRef
            | EvalStackValue.ObjectRef _
            | EvalStackValue.UserDefinedValueType _
            | EvalStackValue.Float _ -> failwith $"Localloc: expected integer byte count, got %O{value}"

        if size < 0L then
            failwith "TODO: Localloc with a negative byte count should throw StackOverflowException"

        if size > int64 Int32.MaxValue then
            failwith $"TODO: Localloc byte count %d{size} exceeds PawPrint's int32 allocation model"

        int size

    let private isStackMemoryPointer (src : ManagedPointerSource) : bool =
        match src with
        | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte _, _) -> true
        | ManagedPointerSource.Null
        | ManagedPointerSource.NativeIntPlaceholder _
        | ManagedPointerSource.Byref _ -> false

    let private isNativeMemoryPointer (src : ManagedPointerSource) : bool =
        match src with
        | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte _, _) -> true
        | ManagedPointerSource.Null
        | ManagedPointerSource.NativeIntPlaceholder _
        | ManagedPointerSource.Byref _ -> false

    let private isTrailingByteViewPointer (src : ManagedPointerSource) : bool =
        match src with
        | ManagedPointerSource.Null -> false
        | ManagedPointerSource.NativeIntPlaceholder _ -> false
        | ManagedPointerSource.Byref (_, projs) ->
            match List.rev projs with
            | ByrefProjection.ByteOffset _ :: ByrefProjection.ReinterpretAs _ :: _
            | ByrefProjection.ReinterpretAs _ :: _ -> true
            | ByrefProjection.ByteOffset n :: _ ->
                failwith
                    $"ByteOffset %d{n} without a preceding ReinterpretAs in projection chain: %O{src} (this is an interpreter bug)"
            | _ -> false

    let private isLocallocForbiddenExceptionRegion (ilOffset : int) (region : ExceptionRegion) : bool =
        let isInHandlerBody (offset : ExceptionOffset) : bool =
            ExceptionHandling.isInHandlerBody ilOffset offset

        match region with
        | ExceptionRegion.Catch (_, offset)
        | ExceptionRegion.Finally offset
        | ExceptionRegion.Fault offset -> isInHandlerBody offset
        | ExceptionRegion.Filter (filterOffset, offset) ->
            (ilOffset >= filterOffset && ilOffset < offset.HandlerOffset)
            || isInHandlerBody offset

    /// A zero divisor is a *guest* fault, not an interpreter one. `divUnValues` computes the
    /// unsigned quotient the way the host's own `div.un` would, so it signals the fault the way
    /// the host would too: by raising `System.DivideByZeroException`. `executeFaultingArithmetic`
    /// converts that into the guest's `System.DivideByZeroException` at the opcode boundary — the
    /// same treatment `div` and `rem` get, where the raise comes from the host instruction itself
    /// rather than from a check written out here.
    let private checkDivUnZero (operation : string) (isZero : bool) : unit =
        if isZero then
            raise (DivideByZeroException $"%s{operation}: divisor was zero")

    let internal divUnValues (v1 : EvalStackValue) (v2 : EvalStackValue) : EvalStackValue =
        match v1, v2 with
        | EvalStackValue.Int32 (Int32Source.Verbatim v1), EvalStackValue.Int32 (Int32Source.Verbatim v2) ->
            checkDivUnZero "Div_un" (v2 = 0)

            (uint32<int32> v1 / uint32<int32> v2)
            |> int32<uint32>
            |> Int32Source.Verbatim
            |> EvalStackValue.Int32
        | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 ->
            checkDivUnZero "Div_un" (Int64Source.isZero v2)

            match v1, v2 with
            | Int64Source.Verbatim v1, Int64Source.Verbatim v2 ->
                (uint64<int64> v1 / uint64<int64> v2)
                |> int64<uint64>
                |> Int64Source.Verbatim
                |> EvalStackValue.Int64
            | _, _ -> failwith "TODO"
        | EvalStackValue.Int32 (Int32Source.Verbatim v1), EvalStackValue.NativeInt (NativeIntSource.Verbatim v2) ->
            checkDivUnZero "Div_un" (v2 = 0L)

            (uint64 (uint32<int32> v1) / uint64<int64> v2)
            |> int64<uint64>
            |> NativeIntSource.Verbatim
            |> EvalStackValue.NativeInt
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim v1), EvalStackValue.Int32 (Int32Source.Verbatim v2) ->
            checkDivUnZero "Div_un" (v2 = 0)

            (uint64<int64> v1 / uint64 (uint32<int32> v2))
            |> int64<uint64>
            |> NativeIntSource.Verbatim
            |> EvalStackValue.NativeInt
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim v1), EvalStackValue.NativeInt (NativeIntSource.Verbatim v2) ->
            checkDivUnZero "Div_un" (v2 = 0L)

            (uint64<int64> v1 / uint64<int64> v2)
            |> int64<uint64>
            |> NativeIntSource.Verbatim
            |> EvalStackValue.NativeInt
        | _ -> failwith $"TODO: Div_un for {v1} and {v2}"

    /// Which operand-dependent faults an IL arithmetic instruction is *allowed* to raise.
    /// `executeFaultingArithmetic` converts exactly the listed faults into guest exceptions and
    /// lets anything else escape as an interpreter failure, so a host `DivideByZeroException`
    /// arriving from somewhere that cannot divide still crashes loudly instead of being handed to
    /// the guest as a plausible-looking `System.DivideByZeroException`.
    [<RequireQualifiedAccess>]
    type private ArithmeticFaults =
        /// `div.un`, `rem.un`. Unsigned, so every quotient is representable and only a zero divisor
        /// faults (ECMA-335 III.3.32, III.3.56).
        | DivideByZero
        /// `div`, `rem`. A zero divisor faults, and so does `MinValue op -1`, whose quotient has no
        /// two's-complement representation (ECMA-335 III.3.31, III.3.55).
        | DivideByZeroOrOverflow

    /// Run one arithmetic instruction that can fault on its operands, and either push its result or
    /// hand the guest the exception the CLR would have thrown.
    ///
    /// The fault arrives as a host exception, so `compute` must stay as tight as
    /// possible around the arithmetic itself: anything else caught in here would be silently
    /// reinterpreted as a guest fault.
    ///
    /// On a fault the program counter deliberately does NOT advance: exception dispatch reads the
    /// faulting instruction's offset to decide which handler regions are active and to build the
    /// stack trace.
    let private executeFaultingArithmetic
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (faults : ArithmeticFaults)
        (currentThread : ThreadId)
        (state : IlMachineState)
        (compute : unit -> EvalStackValue * IlMachineState)
        : ExecutionResult
        =
        // The host's own `div`/`rem` instructions are what actually detect these faults —
        // PawPrint delegates the arithmetic to them via `BinaryArithmetic`, so there is no
        // separate table of faulting operand values here that could drift from the semantics
        // the host implements.
        let outcome =
            try
                compute () |> Ok
            with
            | :? DivideByZeroException -> Error corelib.DivideByZeroException
            | :? OverflowException when
                (match faults with
                 | ArithmeticFaults.DivideByZeroOrOverflow -> true
                 | ArithmeticFaults.DivideByZero -> false)
                ->
                Error corelib.OverflowException

        match outcome with
        | Ok (result, state) ->
            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Error exceptionType ->
            IlMachineStateExecution.raiseRuntimeException loggerFactory corelib exceptionType currentThread state
            |> ExecutionResult.stepped

    let private negInt32Unchecked (value : int32) : int32 =
        0u - uint32<int32> value |> int32<uint32>

    let private negInt64Unchecked (value : int64) : int64 =
        0UL - uint64<int64> value |> int64<uint64>

    let private negValue (value : EvalStackValue) (counters : PointerHashState) : EvalStackValue * PointerHashState =
        match value with
        | EvalStackValue.Int32 int32Source ->
            let value = Int32Source.value "Neg" int32Source
            negInt32Unchecked value |> Int32Source.Verbatim |> EvalStackValue.Int32, counters
        | EvalStackValue.Int64 value ->
            match Int64Source.negate "Neg" value counters with
            | Some (v, counters) -> EvalStackValue.Int64 v, counters
            | None -> EvalStackValue.Int64 (Int64Source.Verbatim Int64.MinValue), counters
        | EvalStackValue.NativeInt source ->
            match source with
            | NativeIntSource.Verbatim value ->
                negInt64Unchecked value |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt, counters
            | NativeIntSource.SyntheticCrossArrayOffset value ->
                SyntheticCrossArrayOffset.negate value
                |> NativeIntSource.SyntheticCrossArrayOffset
                |> EvalStackValue.NativeInt,
                counters
            | NativeIntSource.ManagedPointer ManagedPointerSource.Null ->
                NativeIntSource.Verbatim 0L |> EvalStackValue.NativeInt, counters
            | NativeIntSource.ManagedPointer ptr -> failwith $"Neg: refusing to negate managed pointer %O{ptr}"
            | NativeIntSource.FunctionPointer methodInfo ->
                failwith $"Neg: refusing to negate function pointer %O{methodInfo}"
            | NativeIntSource.TypeHandlePtr typeHandle ->
                failwith $"Neg: refusing to negate RuntimeTypeHandle pointer %O{typeHandle}"
            | NativeIntSource.TypeDescPtr typeHandle ->
                failwith $"Neg: refusing to negate TypeDesc pointer %O{typeHandle}"
            | NativeIntSource.MethodTablePtr typeHandle ->
                failwith $"Neg: refusing to negate MethodTable pointer %O{typeHandle}"
            | NativeIntSource.MethodTableAuxiliaryDataPtr typeHandle ->
                failwith $"Neg: refusing to negate MethodTableAuxiliaryData pointer %O{typeHandle}"
            | NativeIntSource.PerInstInfoPtr handle ->
                failwith $"Neg: refusing to negate PerInstInfo pointer %O{handle}"
            | NativeIntSource.PerInstDictPtr handle ->
                failwith $"Neg: refusing to negate PerInstDict pointer %O{handle}"
            | NativeIntSource.MethodHandlePtr handle ->
                failwith $"Neg: refusing to negate RuntimeMethodHandle pointer %d{handle}"
            | NativeIntSource.FieldHandlePtr handle ->
                failwith $"Neg: refusing to negate RuntimeFieldHandle pointer %d{handle}"
            | NativeIntSource.GcHandlePtr (handle, _) ->
                failwith $"Neg: refusing to negate GC handle pointer %O{handle}"
            | NativeIntSource.AssemblyHandle assemblyName ->
                failwith $"Neg: refusing to negate assembly handle %s{assemblyName}"
            | NativeIntSource.ModuleHandle moduleName ->
                failwith $"Neg: refusing to negate module handle %s{moduleName}"
            | NativeIntSource.MetadataImportHandle moduleName ->
                failwith $"Neg: refusing to negate metadata import handle %s{moduleName}"
            | NativeIntSource.EventPipeProviderPtr id ->
                failwith $"Neg: refusing to negate EventPipe provider handle %d{id}"
            | NativeIntSource.EventPipeEventPtr id -> failwith $"Neg: refusing to negate EventPipe event handle %d{id}"
            | NativeIntSource.LowLevelMonitorPtr id ->
                failwith $"Neg: refusing to negate low-level monitor handle %O{id}"
            | NativeIntSource.WaitHandlePtr id -> failwith $"Neg: refusing to negate wait handle %O{id}"
            | NativeIntSource.OpaqueHashBits bits ->
                // Negating synthesised hash bits is a bit-mixing operation
                // that stays in the synthesis domain; the result keeps the
                // OpaqueHashBits tag.
                negInt64Unchecked bits
                |> NativeIntSource.OpaqueHashBits
                |> EvalStackValue.NativeInt,
                counters
        | EvalStackValue.Float value -> -value |> EvalStackValue.Float, counters
        | EvalStackValue.ManagedPointer ptr -> failwith $"Neg: refusing to negate managed pointer %O{ptr}"
        | EvalStackValue.NullObjectRef -> failwith "Neg: refusing to negate null object reference"
        | EvalStackValue.ObjectRef addr -> failwith $"Neg: refusing to negate object reference %O{addr}"
        | EvalStackValue.UserDefinedValueType valueType ->
            failwith $"Neg: refusing to negate user-defined value type %O{valueType}"

    let private convOvfI4Un (value : EvalStackValue) : Result<int32, unit> =
        let fromUnsignedInt64 (value : int64) : Result<int32, unit> =
            if value < 0L || value > int64 Int32.MaxValue then
                Error ()
            else
                int32 value |> Ok

        match value with
        | EvalStackValue.Int32 int32Source ->
            let i = Int32Source.value "Conv_ovf_i4_un" int32Source
            if i < 0 then Error () else Ok i
        | EvalStackValue.Int64 (Int64Source.Verbatim i) -> fromUnsignedInt64 i
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _) ->
            failwith "TODO: Conv_ovf_i4_un from synthetic cross-array offset"
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)) ->
            failwith $"TODO: Conv_ovf_i4_un from widened native int %O{src}"
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) ->
            // Synthesised hash bits truncated to int32 is conv.i4, not
            // conv.ovf.i4.un. If the hash happens to fit in int32 anyway
            // we could allow it, but the overflow contract on this path
            // is a real CLR semantic that hash bits don't model; fail
            // loudly until a call site demonstrates the need.
            failwith $"TODO: Conv_ovf_i4_un from synthesised pointer-hash bits 0x%x{bits}"
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) -> fromUnsignedInt64 i
        | EvalStackValue.NativeInt (NativeIntSource.SyntheticCrossArrayOffset _) ->
            failwith "TODO: Conv_ovf_i4_un from synthetic cross-array offset native int"
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null) -> Ok 0
        | EvalStackValue.NativeInt src -> failwith $"TODO: Conv_ovf_i4_un from non-verbatim native int source %O{src}"
        | EvalStackValue.Float f ->
            // ECMA-335 III.3.27: for a floating-point source, the `_un` suffix has
            // no effect — floats are signed by construction, so there is no source
            // bit-pattern to reinterpret. Behaviour matches `conv.ovf.i4`: truncate
            // toward zero, accept results in `[Int32.MinValue, Int32.MaxValue]`,
            // overflow on NaN or out-of-range.
            if Double.IsNaN f || f >= 2147483648.0 || f <= -2147483649.0 then
                Error ()
            else
                int32<float> (Math.Truncate f) |> Ok
        | EvalStackValue.ManagedPointer ptr -> failwith $"TODO: Conv_ovf_i4_un from managed pointer %O{ptr}"
        | EvalStackValue.NullObjectRef -> failwith "TODO: Conv_ovf_i4_un from null object reference"
        | EvalStackValue.ObjectRef addr -> failwith $"TODO: Conv_ovf_i4_un from object reference %O{addr}"
        | EvalStackValue.UserDefinedValueType valueType ->
            failwith $"TODO: Conv_ovf_i4_un from user-defined value type %O{valueType}"

    /// `conv.ovf.i4`: treats the source as signed and converts it to int32,
    /// returning `Error ()` when the value does not fit in `[Int32.MinValue,
    /// Int32.MaxValue]`. Pointer-shaped native ints reach this opcode only via
    /// patterns we have not yet observed, so they `failwith` until a real call
    /// site demonstrates the right policy.
    let private convOvfI4 (value : EvalStackValue) : Result<int32, unit> =
        let fromSignedInt64 (value : int64) : Result<int32, unit> =
            if value < int64 Int32.MinValue || value > int64 Int32.MaxValue then
                Error ()
            else
                int32 value |> Ok

        match value with
        | EvalStackValue.Int32 int32Source ->
            let i = Int32Source.value "Conv_ovf_i4" int32Source
            Ok i
        | EvalStackValue.Int64 (Int64Source.Verbatim i) -> fromSignedInt64 i
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _) ->
            failwith "TODO: Conv_ovf_i4 from synthetic cross-array offset"
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)) ->
            failwith $"TODO: Conv_ovf_i4 from widened native int %O{src}"
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) ->
            failwith $"TODO: Conv_ovf_i4 from synthesised pointer-hash bits 0x%x{bits}"
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) -> fromSignedInt64 i
        | EvalStackValue.NativeInt (NativeIntSource.SyntheticCrossArrayOffset _) ->
            failwith "TODO: Conv_ovf_i4 from synthetic cross-array offset native int"
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null) -> Ok 0
        | EvalStackValue.NativeInt src -> failwith $"TODO: Conv_ovf_i4 from non-verbatim native int source %O{src}"
        | EvalStackValue.Float f ->
            // Truncate toward zero, then check the truncated integer fits in
            // `[Int32.MinValue, Int32.MaxValue]`. `2147483648.0` (= 2^31) is exactly
            // representable and is the smallest double > Int32.MaxValue;
            // `-2147483649.0` (= -2^31 - 1) is exactly representable and is the
            // largest double < Int32.MinValue. Doubles strictly between
            // `-2147483649.0` and `-2147483648.0` truncate to `-2147483648` which is
            // in range, so use a strict `<` against `-2147483649.0`. NaN compares
            // false to every value, so guard separately.
            if Double.IsNaN f || f >= 2147483648.0 || f <= -2147483649.0 then
                Error ()
            else
                int32<float> (Math.Truncate f) |> Ok
        | EvalStackValue.ManagedPointer ptr -> failwith $"TODO: Conv_ovf_i4 from managed pointer %O{ptr}"
        | EvalStackValue.NullObjectRef -> failwith "TODO: Conv_ovf_i4 from null object reference"
        | EvalStackValue.ObjectRef addr -> failwith $"TODO: Conv_ovf_i4 from object reference %O{addr}"
        | EvalStackValue.UserDefinedValueType valueType ->
            failwith $"TODO: Conv_ovf_i4 from user-defined value type %O{valueType}"

    /// `conv.ovf.u4`: treats the source as signed and converts it to uint32,
    /// returning `Error ()` when the value does not fit in `[0,
    /// UInt32.MaxValue]`. Negative signed sources overflow; positive sources
    /// up to UInt32.MaxValue succeed.
    let private convOvfU4 (value : EvalStackValue) : Result<uint32, unit> =
        let fromSignedInt64 (value : int64) : Result<uint32, unit> =
            if value < 0L || value > int64 UInt32.MaxValue then
                Error ()
            else
                uint32 value |> Ok

        match value with
        | EvalStackValue.Int32 int32Source ->
            let i = Int32Source.value "Conv_ovf_u4" int32Source
            if i < 0 then Error () else uint32 i |> Ok
        | EvalStackValue.Int64 (Int64Source.Verbatim i) -> fromSignedInt64 i
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _) ->
            failwith "TODO: Conv_ovf_u4 from synthetic cross-array offset"
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)) ->
            failwith $"TODO: Conv_ovf_u4 from widened native int %O{src}"
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) ->
            failwith $"TODO: Conv_ovf_u4 from synthesised pointer-hash bits 0x%x{bits}"
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) -> fromSignedInt64 i
        | EvalStackValue.NativeInt (NativeIntSource.SyntheticCrossArrayOffset _) ->
            failwith "TODO: Conv_ovf_u4 from synthetic cross-array offset native int"
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null) -> Ok 0u
        | EvalStackValue.NativeInt src -> failwith $"TODO: Conv_ovf_u4 from non-verbatim native int source %O{src}"
        | EvalStackValue.Float f ->
            // Truncate toward zero, then check the truncated integer fits in
            // `[0, UInt32.MaxValue]`. `4294967296.0` (= 2^32) is exactly
            // representable and is the smallest double > UInt32.MaxValue. Doubles
            // strictly between `-1.0` and `0.0` truncate to `0` which is in range,
            // so use `<=` against `-1.0` for the lower bound. NaN guard separate.
            if Double.IsNaN f || f >= 4294967296.0 || f <= -1.0 then
                Error ()
            else
                uint32<float> (Math.Truncate f) |> Ok
        | EvalStackValue.ManagedPointer ptr -> failwith $"TODO: Conv_ovf_u4 from managed pointer %O{ptr}"
        | EvalStackValue.NullObjectRef -> failwith "TODO: Conv_ovf_u4 from null object reference"
        | EvalStackValue.ObjectRef addr -> failwith $"TODO: Conv_ovf_u4 from object reference %O{addr}"
        | EvalStackValue.UserDefinedValueType valueType ->
            failwith $"TODO: Conv_ovf_u4 from user-defined value type %O{valueType}"

    /// `conv.ovf.i1`: treats the source as signed and converts it to int8,
    /// returning `Error ()` when the value does not fit in `[SByte.MinValue,
    /// SByte.MaxValue]`.
    let private convOvfI1 (value : EvalStackValue) : Result<sbyte, unit> =
        let fromSignedInt32 (value : int32) : Result<sbyte, unit> =
            if value < int32 SByte.MinValue || value > int32 SByte.MaxValue then
                Error ()
            else
                sbyte value |> Ok

        let fromSignedInt64 (value : int64) : Result<sbyte, unit> =
            if value < int64 SByte.MinValue || value > int64 SByte.MaxValue then
                Error ()
            else
                sbyte value |> Ok

        match value with
        | EvalStackValue.Int32 int32Source ->
            let i = Int32Source.value "Conv_ovf_i1" int32Source
            fromSignedInt32 i
        | EvalStackValue.Int64 (Int64Source.Verbatim i) -> fromSignedInt64 i
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _) ->
            failwith "TODO: Conv_ovf_i1 from synthetic cross-array offset"
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)) ->
            failwith $"TODO: Conv_ovf_i1 from widened native int %O{src}"
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) ->
            failwith $"TODO: Conv_ovf_i1 from synthesised pointer-hash bits 0x%x{bits}"
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) -> fromSignedInt64 i
        | EvalStackValue.NativeInt (NativeIntSource.SyntheticCrossArrayOffset _) ->
            failwith "TODO: Conv_ovf_i1 from synthetic cross-array offset native int"
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null) -> Ok 0y
        | EvalStackValue.NativeInt src -> failwith $"TODO: Conv_ovf_i1 from non-verbatim native int source %O{src}"
        | EvalStackValue.Float f ->
            // Truncate toward zero, then check the truncated integer fits in
            // `[SByte.MinValue, SByte.MaxValue]`. Both bounds are exactly
            // representable in double. Doubles strictly between `-129.0` and
            // `-128.0` truncate to `-128` which is in range, so use strict `<`
            // against `-129.0`. NaN guard separate.
            if Double.IsNaN f || f >= 128.0 || f <= -129.0 then
                Error ()
            else
                sbyte<float> (Math.Truncate f) |> Ok
        | EvalStackValue.ManagedPointer ptr -> failwith $"TODO: Conv_ovf_i1 from managed pointer %O{ptr}"
        | EvalStackValue.NullObjectRef -> failwith "TODO: Conv_ovf_i1 from null object reference"
        | EvalStackValue.ObjectRef addr -> failwith $"TODO: Conv_ovf_i1 from object reference %O{addr}"
        | EvalStackValue.UserDefinedValueType valueType ->
            failwith $"TODO: Conv_ovf_i1 from user-defined value type %O{valueType}"

    /// `conv.ovf.u1`: treats the source as signed and converts it to uint8,
    /// returning `Error ()` when the value does not fit in `[0, 255]`. Negative
    /// signed sources overflow.
    let private convOvfU1 (value : EvalStackValue) : Result<byte, unit> =
        let fromSignedInt32 (value : int32) : Result<byte, unit> =
            if value < 0 || value > 255 then
                Error ()
            else
                byte value |> Ok

        let fromSignedInt64 (value : int64) : Result<byte, unit> =
            if value < 0L || value > 255L then
                Error ()
            else
                byte value |> Ok

        match value with
        | EvalStackValue.Int32 int32Source ->
            let i = Int32Source.value "Conv_ovf_u1" int32Source
            fromSignedInt32 i
        | EvalStackValue.Int64 (Int64Source.Verbatim i) -> fromSignedInt64 i
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _) ->
            failwith "TODO: Conv_ovf_u1 from synthetic cross-array offset"
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)) ->
            failwith $"TODO: Conv_ovf_u1 from widened native int %O{src}"
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) ->
            failwith $"TODO: Conv_ovf_u1 from synthesised pointer-hash bits 0x%x{bits}"
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) -> fromSignedInt64 i
        | EvalStackValue.NativeInt (NativeIntSource.SyntheticCrossArrayOffset _) ->
            failwith "TODO: Conv_ovf_u1 from synthetic cross-array offset native int"
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null) -> Ok 0uy
        | EvalStackValue.NativeInt src -> failwith $"TODO: Conv_ovf_u1 from non-verbatim native int source %O{src}"
        | EvalStackValue.Float f ->
            // Truncate toward zero, then check the truncated integer fits in
            // `[0, 255]`. `256.0` is exactly representable. NaN guard separate.
            if Double.IsNaN f || f >= 256.0 || f <= -1.0 then
                Error ()
            else
                byte<float> (Math.Truncate f) |> Ok
        | EvalStackValue.ManagedPointer ptr -> failwith $"TODO: Conv_ovf_u1 from managed pointer %O{ptr}"
        | EvalStackValue.NullObjectRef -> failwith "TODO: Conv_ovf_u1 from null object reference"
        | EvalStackValue.ObjectRef addr -> failwith $"TODO: Conv_ovf_u1 from object reference %O{addr}"
        | EvalStackValue.UserDefinedValueType valueType ->
            failwith $"TODO: Conv_ovf_u1 from user-defined value type %O{valueType}"

    /// `conv.ovf.u2`: treats the source as signed and converts it to uint16,
    /// returning `Error ()` when the value does not fit in `[0, 65535]`. Negative
    /// signed sources overflow, and so does any source whose full signed width
    /// exceeds the range — this is a range check, not a truncation.
    let internal convOvfU2 (value : EvalStackValue) : Result<uint16, unit> =
        let fromSignedInt32 (value : int32) : Result<uint16, unit> =
            if value < 0 || value > int32 UInt16.MaxValue then
                Error ()
            else
                uint16 value |> Ok

        let fromSignedInt64 (value : int64) : Result<uint16, unit> =
            if value < 0L || value > int64 UInt16.MaxValue then
                Error ()
            else
                uint16 value |> Ok

        match value with
        | EvalStackValue.Int32 int32Source ->
            let i = Int32Source.value "Conv_ovf_u2" int32Source
            fromSignedInt32 i
        | EvalStackValue.Int64 (Int64Source.Verbatim i) -> fromSignedInt64 i
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _) ->
            failwith "TODO: Conv_ovf_u2 from synthetic cross-array offset"
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)) ->
            failwith $"TODO: Conv_ovf_u2 from widened native int %O{src}"
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) ->
            failwith $"TODO: Conv_ovf_u2 from synthesised pointer-hash bits 0x%x{bits}"
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) -> fromSignedInt64 i
        | EvalStackValue.NativeInt (NativeIntSource.SyntheticCrossArrayOffset _) ->
            failwith "TODO: Conv_ovf_u2 from synthetic cross-array offset native int"
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null) -> Ok 0us
        | EvalStackValue.NativeInt src -> failwith $"TODO: Conv_ovf_u2 from non-verbatim native int source %O{src}"
        | EvalStackValue.Float f ->
            // Truncate toward zero, then check the truncated integer fits in
            // `[0, 65535]`. `65536.0` is exactly representable and is the smallest
            // double above UInt16.MaxValue. Doubles strictly between `-1.0` and
            // `0.0` truncate to `0`, which is in range, so use `<=` against `-1.0`.
            // NaN compares false to every value, so guard separately.
            if Double.IsNaN f || f >= 65536.0 || f <= -1.0 then
                Error ()
            else
                uint16<float> (Math.Truncate f) |> Ok
        // The narrowing `conv.ovf.*` opcodes refuse every byref that still
        // carries its tag, where the native-width `conv.ovf.i` / `conv.ovf.u`
        // pass one through: a 16-bit destination cannot hold an address, so
        // there is nothing charitable to answer. `NativeInt (ManagedPointer
        // Null)` above is the exception because the guest already asked for that
        // byref as a number.
        | EvalStackValue.ManagedPointer ptr -> failwith $"TODO: Conv_ovf_u2 from managed pointer %O{ptr}"
        | EvalStackValue.NullObjectRef -> failwith "TODO: Conv_ovf_u2 from null object reference"
        | EvalStackValue.ObjectRef addr -> failwith $"TODO: Conv_ovf_u2 from object reference %O{addr}"
        | EvalStackValue.UserDefinedValueType valueType ->
            failwith $"TODO: Conv_ovf_u2 from user-defined value type %O{valueType}"

    /// `conv.ovf.u1.un`: treats the source as unsigned and converts it to
    /// uint8, returning `Error ()` when the value does not fit in `[0, 255]`.
    /// Because the source is interpreted unsigned, an Int32 with the sign bit
    /// set (e.g. -1) is treated as a large positive uint32, which overflows.
    let private convOvfU1Un (value : EvalStackValue) : Result<byte, unit> =
        let fromUnsignedInt32 (value : int32) : Result<byte, unit> =
            let u = uint32 value
            if u > 255u then Error () else byte u |> Ok

        let fromUnsignedInt64 (value : int64) : Result<byte, unit> =
            let u = uint64 value
            if u > 255UL then Error () else byte u |> Ok

        match value with
        | EvalStackValue.Int32 int32Source ->
            let i = Int32Source.value "Conv_ovf_u1_un" int32Source
            fromUnsignedInt32 i
        | EvalStackValue.Int64 (Int64Source.Verbatim i) -> fromUnsignedInt64 i
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _) ->
            failwith "TODO: Conv_ovf_u1_un from synthetic cross-array offset"
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)) ->
            failwith $"TODO: Conv_ovf_u1_un from widened native int %O{src}"
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) ->
            failwith $"TODO: Conv_ovf_u1_un from synthesised pointer-hash bits 0x%x{bits}"
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) -> fromUnsignedInt64 i
        | EvalStackValue.NativeInt (NativeIntSource.SyntheticCrossArrayOffset _) ->
            failwith "TODO: Conv_ovf_u1_un from synthetic cross-array offset native int"
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null) -> Ok 0uy
        | EvalStackValue.NativeInt src -> failwith $"TODO: Conv_ovf_u1_un from non-verbatim native int source %O{src}"
        | EvalStackValue.Float f ->
            // For float sources the `_un` suffix is a no-op: floats are signed by
            // construction. Truncate toward zero, then check the truncated integer
            // fits in `[0, 255]`. `256.0` is exactly representable. NaN guard
            // separate.
            if Double.IsNaN f || f >= 256.0 || f <= -1.0 then
                Error ()
            else
                byte<float> (Math.Truncate f) |> Ok
        | EvalStackValue.ManagedPointer ptr -> failwith $"TODO: Conv_ovf_u1_un from managed pointer %O{ptr}"
        | EvalStackValue.NullObjectRef -> failwith "TODO: Conv_ovf_u1_un from null object reference"
        | EvalStackValue.ObjectRef addr -> failwith $"TODO: Conv_ovf_u1_un from object reference %O{addr}"
        | EvalStackValue.UserDefinedValueType valueType ->
            failwith $"TODO: Conv_ovf_u1_un from user-defined value type %O{valueType}"

    /// The conversion performed by `conv.ovf.u`: treats the source value as
    /// signed and converts it to an unsigned native int, returning `Error ()`
    /// when the value cannot be represented. On a 64-bit interpreter the
    /// destination range is `[0, UInt64.MaxValue]`, so the only signed sources
    /// that overflow are negative ones; floats outside `[0, 2^64)` and `NaN`
    /// also overflow. Pointer-shaped native ints are passed through to keep
    /// pointer provenance intact, matching the `Conv_U` policy. The result is
    /// expressed as `NativeIntSource` (the same slot used by `Conv_U`).
    let private convOvfU (value : EvalStackValue) : Result<NativeIntSource, unit> =
        match value with
        | EvalStackValue.Int32 int32Source ->
            let i = Int32Source.value "Conv_ovf_u" int32Source

            if i < 0 then
                Error ()
            else
                NativeIntSource.Verbatim (int64 i) |> Ok
        | EvalStackValue.Int64 (Int64Source.Verbatim i) ->
            if i < 0L then
                Error ()
            else
                NativeIntSource.Verbatim i |> Ok
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset i) ->
            // Cross-array offsets are non-negative storage indices, not numeric
            // values that can be negative; preserving the tag keeps later
            // arithmetic honest.
            NativeIntSource.SyntheticCrossArrayOffset i |> Ok
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)) ->
            // Inversion of `Conv.U8` / `Conv.I8` followed by `Conv.ovf.u`. On a
            // 64-bit interpreter the widening is bit-preserving, so the
            // truncation back to native int recovers the original
            // NativeIntSource. The overflow check here would be redundant for
            // pointer-shaped sources, and for a `Verbatim` underlying we treat
            // its bits as pointer-domain on round-trip (consistent with
            // `Conv_U`'s charity).
            Ok src
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) ->
            // Synthesised hash bits: preserve the tag so downstream code sees
            // deterministic numeric content rather than a fake pointer.
            NativeIntSource.OpaqueHashBits bits |> Ok
        | EvalStackValue.NativeInt src ->
            match src with
            | NativeIntSource.Verbatim n ->
                if n < 0L then
                    Error ()
                else
                    NativeIntSource.Verbatim n |> Ok
            | NativeIntSource.SyntheticCrossArrayOffset _ -> Ok src
            | NativeIntSource.ManagedPointer _ -> Ok src
            | NativeIntSource.OpaqueHashBits _ -> Ok src
            | NativeIntSource.FunctionPointer methodInfo ->
                failwith $"Conv_ovf_u: refusing to convert function pointer %O{methodInfo} to unsigned native int"
            | NativeIntSource.FieldHandlePtr handle ->
                failwith $"Conv_ovf_u: refusing to convert RuntimeFieldHandle pointer %d{handle} to unsigned native int"
            | NativeIntSource.MethodHandlePtr handle ->
                failwith
                    $"Conv_ovf_u: refusing to convert RuntimeMethodHandle pointer %d{handle} to unsigned native int"
            | NativeIntSource.TypeHandlePtr typeHandle ->
                failwith
                    $"Conv_ovf_u: refusing to convert RuntimeTypeHandle pointer %O{typeHandle} to unsigned native int"
            | NativeIntSource.TypeDescPtr typeHandle ->
                failwith $"Conv_ovf_u: refusing to convert TypeDesc pointer %O{typeHandle} to unsigned native int"
            | NativeIntSource.MethodTablePtr typeHandle ->
                failwith $"Conv_ovf_u: refusing to convert MethodTable pointer %O{typeHandle} to unsigned native int"
            | NativeIntSource.MethodTableAuxiliaryDataPtr typeHandle ->
                failwith
                    $"Conv_ovf_u: refusing to convert MethodTableAuxiliaryData pointer %O{typeHandle} to unsigned native int"
            | NativeIntSource.PerInstInfoPtr handle ->
                failwith $"Conv_ovf_u: refusing to convert PerInstInfo pointer %O{handle} to unsigned native int"
            | NativeIntSource.PerInstDictPtr handle ->
                failwith $"Conv_ovf_u: refusing to convert PerInstDict pointer %O{handle} to unsigned native int"
            | NativeIntSource.GcHandlePtr (handle, _) ->
                failwith $"Conv_ovf_u: refusing to convert GC handle pointer %O{handle} to unsigned native int"
            | NativeIntSource.EventPipeProviderPtr id ->
                failwith $"Conv_ovf_u: refusing to convert EventPipe provider handle #%d{id} to unsigned native int"
            | NativeIntSource.EventPipeEventPtr id ->
                failwith $"Conv_ovf_u: refusing to convert EventPipe event handle #%d{id} to unsigned native int"
            | NativeIntSource.LowLevelMonitorPtr id ->
                failwith $"Conv_ovf_u: refusing to convert low-level monitor handle %O{id} to unsigned native int"
            | NativeIntSource.WaitHandlePtr id ->
                failwith $"Conv_ovf_u: refusing to convert wait handle %O{id} to unsigned native int"
            | NativeIntSource.AssemblyHandle assemblyName ->
                failwith $"Conv_ovf_u: refusing to convert assembly handle %s{assemblyName} to unsigned native int"
            | NativeIntSource.ModuleHandle moduleName ->
                failwith $"Conv_ovf_u: refusing to convert module handle %s{moduleName} to unsigned native int"
            | NativeIntSource.MetadataImportHandle moduleName ->
                failwith $"Conv_ovf_u: refusing to convert metadata import handle %s{moduleName} to unsigned native int"
        | EvalStackValue.Float f ->
            // `conv.ovf.u` truncates the float toward zero and overflows if
            // the truncated integer does not fit in `[0, UInt64.MaxValue]`
            // (on a 64-bit interpreter). So `-0.5` is in range (truncates to
            // 0) but `-1.0` is not (truncates to -1). NaN compares false to
            // every value, so the `IsNaN` guard is required separately.
            // `2.0 ** 64` is the smallest double > UInt64.MaxValue, so use
            // `>=` to reject it.
            if Double.IsNaN f || f <= -1.0 || f >= 18446744073709551616.0 then
                Error ()
            else
                NativeIntSource.Verbatim (int64<uint64> (uint64<float> f)) |> Ok
        | EvalStackValue.ManagedPointer ptr -> NativeIntSource.ManagedPointer ptr |> Ok
        | EvalStackValue.NullObjectRef -> NativeIntSource.ManagedPointer ManagedPointerSource.Null |> Ok
        | EvalStackValue.ObjectRef addr ->
            failwith $"Conv_ovf_u: refusing to convert object reference %O{addr} to unsigned native int"
        | EvalStackValue.UserDefinedValueType valueType ->
            failwith $"Conv_ovf_u: refusing to convert user-defined value type %O{valueType} to unsigned native int"

    /// The conversion performed by `conv.ovf.i`: treats the source value as
    /// signed and converts it to a signed native int, returning `Error ()`
    /// when the value cannot be represented. PawPrint's native int is 64 bits
    /// wide, so every integer source (int32, int64, native int) is exactly
    /// representable and this is the identity on them; only floats can
    /// overflow, when the truncated value falls outside `[-2^63, 2^63)` or the
    /// source is NaN. Pointer-shaped native ints are passed through to keep
    /// pointer provenance intact, matching the `Conv_I` / `Conv_ovf_u` policy.
    /// The result is expressed as `NativeIntSource` (the same slot used by
    /// `Conv_I`).
    let internal convOvfI (value : EvalStackValue) : Result<NativeIntSource, unit> =
        match value with
        | EvalStackValue.Int32 int32Source ->
            let i = Int32Source.value "Conv_ovf_i" int32Source
            NativeIntSource.Verbatim (int64 i) |> Ok
        | EvalStackValue.Int64 (Int64Source.Verbatim i) -> NativeIntSource.Verbatim i |> Ok
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset i) ->
            // Cross-array offsets are byte distances between two storage
            // containers, not numeric values; preserving the tag keeps later
            // arithmetic honest.
            NativeIntSource.SyntheticCrossArrayOffset i |> Ok
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)) ->
            // Inversion of `Conv.I8` / `Conv.U8` followed by `Conv.ovf.i`. On a
            // 64-bit interpreter the widening is bit-preserving, so the
            // truncation back to native int recovers the original
            // NativeIntSource, and the overflow check cannot fire.
            Ok src
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) ->
            // Synthesised hash bits: preserve the tag so downstream code sees
            // deterministic numeric content rather than a fake pointer.
            NativeIntSource.OpaqueHashBits bits |> Ok
        | EvalStackValue.NativeInt src ->
            match src with
            | NativeIntSource.Verbatim _ -> Ok src
            | NativeIntSource.SyntheticCrossArrayOffset _ -> Ok src
            | NativeIntSource.ManagedPointer _ -> Ok src
            | NativeIntSource.OpaqueHashBits _ -> Ok src
            | NativeIntSource.FunctionPointer methodInfo ->
                failwith $"Conv_ovf_i: refusing to convert function pointer %O{methodInfo} to signed native int"
            | NativeIntSource.FieldHandlePtr handle ->
                failwith $"Conv_ovf_i: refusing to convert RuntimeFieldHandle pointer %d{handle} to signed native int"
            | NativeIntSource.MethodHandlePtr handle ->
                failwith $"Conv_ovf_i: refusing to convert RuntimeMethodHandle pointer %d{handle} to signed native int"
            | NativeIntSource.TypeHandlePtr typeHandle ->
                failwith
                    $"Conv_ovf_i: refusing to convert RuntimeTypeHandle pointer %O{typeHandle} to signed native int"
            | NativeIntSource.TypeDescPtr typeHandle ->
                failwith $"Conv_ovf_i: refusing to convert TypeDesc pointer %O{typeHandle} to signed native int"
            | NativeIntSource.MethodTablePtr typeHandle ->
                failwith $"Conv_ovf_i: refusing to convert MethodTable pointer %O{typeHandle} to signed native int"
            | NativeIntSource.MethodTableAuxiliaryDataPtr typeHandle ->
                failwith
                    $"Conv_ovf_i: refusing to convert MethodTableAuxiliaryData pointer %O{typeHandle} to signed native int"
            | NativeIntSource.PerInstInfoPtr handle ->
                failwith $"Conv_ovf_i: refusing to convert PerInstInfo pointer %O{handle} to signed native int"
            | NativeIntSource.PerInstDictPtr handle ->
                failwith $"Conv_ovf_i: refusing to convert PerInstDict pointer %O{handle} to signed native int"
            | NativeIntSource.GcHandlePtr (handle, _) ->
                failwith $"Conv_ovf_i: refusing to convert GC handle pointer %O{handle} to signed native int"
            | NativeIntSource.EventPipeProviderPtr id ->
                failwith $"Conv_ovf_i: refusing to convert EventPipe provider handle #%d{id} to signed native int"
            | NativeIntSource.EventPipeEventPtr id ->
                failwith $"Conv_ovf_i: refusing to convert EventPipe event handle #%d{id} to signed native int"
            | NativeIntSource.LowLevelMonitorPtr id ->
                failwith $"Conv_ovf_i: refusing to convert low-level monitor handle %O{id} to signed native int"
            | NativeIntSource.WaitHandlePtr id ->
                failwith $"Conv_ovf_i: refusing to convert wait handle %O{id} to signed native int"
            | NativeIntSource.AssemblyHandle assemblyName ->
                failwith $"Conv_ovf_i: refusing to convert assembly handle %s{assemblyName} to signed native int"
            | NativeIntSource.ModuleHandle moduleName ->
                failwith $"Conv_ovf_i: refusing to convert module handle %s{moduleName} to signed native int"
            | NativeIntSource.MetadataImportHandle moduleName ->
                failwith $"Conv_ovf_i: refusing to convert metadata import handle %s{moduleName} to signed native int"
        | EvalStackValue.Float f ->
            // `conv.ovf.i` truncates the float toward zero and overflows if the
            // truncated integer does not fit in `[Int64.MinValue,
            // Int64.MaxValue]` (on a 64-bit interpreter). `2^63` is exactly
            // representable and is the smallest double > Int64.MaxValue, so use
            // `>=` to reject it. `-2^63` is exactly representable and is
            // precisely Int64.MinValue; the next double below it is `-2^63 -
            // 2048`, which is out of range, so the lower bound is a strict `<`.
            // NaN compares false to every value, so the `IsNaN` guard is
            // required separately.
            if Double.IsNaN f || f >= 9223372036854775808.0 || f < -9223372036854775808.0 then
                Error ()
            else
                NativeIntSource.Verbatim (int64<float> (Math.Truncate f)) |> Ok
        | EvalStackValue.ManagedPointer ptr -> NativeIntSource.ManagedPointer ptr |> Ok
        | EvalStackValue.NullObjectRef -> NativeIntSource.ManagedPointer ManagedPointerSource.Null |> Ok
        | EvalStackValue.ObjectRef addr ->
            failwith $"Conv_ovf_i: refusing to convert object reference %O{addr} to signed native int"
        | EvalStackValue.UserDefinedValueType valueType ->
            failwith $"Conv_ovf_i: refusing to convert user-defined value type %O{valueType} to signed native int"

    /// The conversion performed by `conv.ovf.i.un`: treats the source value as
    /// *unsigned* and converts it to a signed native int, returning `Error ()`
    /// when the value cannot be represented. PawPrint's native int is 64 bits
    /// wide, so an int32 source is zero-extended and can never overflow (every
    /// uint32 fits), while a 64-bit source overflows exactly when its top bit is
    /// set — read as unsigned it is then at least 2^63, which no signed native
    /// int can hold. That is the whole of the difference from `conv.ovf.i`, and
    /// it points both ways: the int32 `-1` that `conv.ovf.i` passes through
    /// becomes `4294967295` here, and the int64 `-1` it passes through overflows
    /// here. A float has no alternative bit-pattern reading — it is signed by
    /// construction — so for float sources the `.un` suffix is a no-op and the
    /// behaviour is exactly `conv.ovf.i`'s. All three readings are pinned in
    /// `TestNullaryIlOp` against the host's own `conv.ovf.i.un`, emitted through
    /// a `DynamicMethod` rather than restated from this code. Pointer-shaped
    /// native ints are passed through to keep pointer provenance intact,
    /// matching the `Conv_I` / `Conv_ovf_i` policy. The result is expressed as
    /// `NativeIntSource` (the same slot used by `Conv_I`).
    let internal convOvfIUn (value : EvalStackValue) : Result<NativeIntSource, unit> =
        // A 64-bit source read as unsigned is representable in a signed native
        // int exactly when its top bit is clear.
        let fromUnsignedInt64 (i : int64) : Result<NativeIntSource, unit> =
            if i < 0L then
                Error ()
            else
                NativeIntSource.Verbatim i |> Ok

        // A byref read as unsigned. PawPrint does not model addresses, so a real
        // byref has no bits to range-check — but no CLR user-mode address
        // reaches 2^63, so the conversion is the identity and preserving the
        // provenance is the same charity `Conv_I` / `Conv_ovf_i` extend. A
        // `NativeIntPlaceholder` is the exception: it *is* an exact bit pattern
        // the guest handed us via `(void*)bits`, so it gets the real range check
        // (`Int64Source.widenedNativeInt` and `materialiseHashBits` treat
        // placeholders as bit patterns for the same reason).
        let fromManagedPointer (ptr : ManagedPointerSource) : Result<NativeIntSource, unit> =
            match ptr with
            | ManagedPointerSource.NativeIntPlaceholder bits when bits < 0L -> Error ()
            | _ -> NativeIntSource.ManagedPointer ptr |> Ok

        match value with
        | EvalStackValue.Int32 int32Source ->
            let i = Int32Source.value "Conv_ovf_i_un" int32Source
            // Zero-extend: the 32-bit slot is the whole of the source, and every
            // uint32 fits in a signed 64-bit native int, so this cannot fail.
            NativeIntSource.Verbatim (int64<uint32> (uint32<int32> i)) |> Ok
        | EvalStackValue.Int64 (Int64Source.Verbatim i) -> fromUnsignedInt64 i
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset i) ->
            // Cross-array offsets are byte distances between two storage
            // containers whose addresses PawPrint does not model, so there is no
            // sign bit to read. The model places every such delta's unsigned
            // image strictly inside (2^40, 2^64 - 2^40) (see
            // `SyntheticCrossArrayOffset.cltUnVerbatim` / `cgtUnVerbatim`)
            // without committing to either side of 2^63, so whether the real
            // delta would pass this overflow check is exactly the layout
            // question the model declines to answer. Preserving the tag is the
            // same charity `Conv_I` / `Conv_ovf_i` extend to byrefs, and keeps
            // later arithmetic honest.
            NativeIntSource.SyntheticCrossArrayOffset i |> Ok
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)) ->
            // Inversion of `Conv.I8` / `Conv.U8` followed by `Conv.ovf.i.un`. On
            // a 64-bit interpreter the widening is bit-preserving, so the
            // truncation back to native int recovers the original
            // NativeIntSource. `Int64Source.widenedNativeInt` normalises away
            // every numeric underlying source, so what survives here is always
            // pointer-shaped and the overflow check cannot fire.
            Ok src
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) ->
            // Unlike a byref, synthesised hash bits *are* the value: they are
            // what every other numeric use of this tag reads. So the unsigned
            // range check applies to them for real — bit-mixing (`not`, `neg`)
            // can set the top bit, and a real pointer put through the same
            // mixing would overflow in real .NET too. Preserve the tag on the
            // way out so downstream code still sees a non-pointer.
            if bits < 0L then
                Error ()
            else
                NativeIntSource.OpaqueHashBits bits |> Ok
        | EvalStackValue.NativeInt src ->
            match src with
            | NativeIntSource.Verbatim n -> fromUnsignedInt64 n
            | NativeIntSource.SyntheticCrossArrayOffset _ -> Ok src
            | NativeIntSource.ManagedPointer ptr -> fromManagedPointer ptr
            | NativeIntSource.OpaqueHashBits bits -> if bits < 0L then Error () else Ok src
            | NativeIntSource.FunctionPointer methodInfo ->
                failwith $"Conv_ovf_i_un: refusing to convert function pointer %O{methodInfo} to signed native int"
            | NativeIntSource.FieldHandlePtr handle ->
                failwith
                    $"Conv_ovf_i_un: refusing to convert RuntimeFieldHandle pointer %d{handle} to signed native int"
            | NativeIntSource.MethodHandlePtr handle ->
                failwith
                    $"Conv_ovf_i_un: refusing to convert RuntimeMethodHandle pointer %d{handle} to signed native int"
            | NativeIntSource.TypeHandlePtr typeHandle ->
                failwith
                    $"Conv_ovf_i_un: refusing to convert RuntimeTypeHandle pointer %O{typeHandle} to signed native int"
            | NativeIntSource.TypeDescPtr typeHandle ->
                failwith $"Conv_ovf_i_un: refusing to convert TypeDesc pointer %O{typeHandle} to signed native int"
            | NativeIntSource.MethodTablePtr typeHandle ->
                failwith $"Conv_ovf_i_un: refusing to convert MethodTable pointer %O{typeHandle} to signed native int"
            | NativeIntSource.MethodTableAuxiliaryDataPtr typeHandle ->
                failwith
                    $"Conv_ovf_i_un: refusing to convert MethodTableAuxiliaryData pointer %O{typeHandle} to signed native int"
            | NativeIntSource.PerInstInfoPtr handle ->
                failwith $"Conv_ovf_i_un: refusing to convert PerInstInfo pointer %O{handle} to signed native int"
            | NativeIntSource.PerInstDictPtr handle ->
                failwith $"Conv_ovf_i_un: refusing to convert PerInstDict pointer %O{handle} to signed native int"
            | NativeIntSource.GcHandlePtr (handle, _) ->
                failwith $"Conv_ovf_i_un: refusing to convert GC handle pointer %O{handle} to signed native int"
            | NativeIntSource.EventPipeProviderPtr id ->
                failwith $"Conv_ovf_i_un: refusing to convert EventPipe provider handle #%d{id} to signed native int"
            | NativeIntSource.EventPipeEventPtr id ->
                failwith $"Conv_ovf_i_un: refusing to convert EventPipe event handle #%d{id} to signed native int"
            | NativeIntSource.LowLevelMonitorPtr id ->
                failwith $"Conv_ovf_i_un: refusing to convert low-level monitor handle %O{id} to signed native int"
            | NativeIntSource.WaitHandlePtr id ->
                failwith $"Conv_ovf_i_un: refusing to convert wait handle %O{id} to signed native int"
            | NativeIntSource.AssemblyHandle assemblyName ->
                failwith $"Conv_ovf_i_un: refusing to convert assembly handle %s{assemblyName} to signed native int"
            | NativeIntSource.ModuleHandle moduleName ->
                failwith $"Conv_ovf_i_un: refusing to convert module handle %s{moduleName} to signed native int"
            | NativeIntSource.MetadataImportHandle moduleName ->
                failwith
                    $"Conv_ovf_i_un: refusing to convert metadata import handle %s{moduleName} to signed native int"
        | EvalStackValue.Float f ->
            // ECMA-335 Partition III, `conv.ovf.<to type>.un`: the `.un` suffix
            // describes how to read an *integer* source, so it has no effect on
            // a float, which is signed by construction — the host agrees, and
            // the property test checks that against it rather than against this
            // comment. This is therefore `conv.ovf.i`'s float path
            // verbatim: truncate toward zero, then range-check against
            // `[Int64.MinValue, Int64.MaxValue]`. `2^63` is exactly
            // representable and is the smallest double > Int64.MaxValue, so use
            // `>=` to reject it; `-2^63` is exactly Int64.MinValue and in range,
            // so the lower bound is a strict `<`. NaN compares false to every
            // value, so the `IsNaN` guard is required separately.
            if Double.IsNaN f || f >= 9223372036854775808.0 || f < -9223372036854775808.0 then
                Error ()
            else
                NativeIntSource.Verbatim (int64<float> (Math.Truncate f)) |> Ok
        | EvalStackValue.ManagedPointer ptr -> fromManagedPointer ptr
        | EvalStackValue.NullObjectRef -> NativeIntSource.ManagedPointer ManagedPointerSource.Null |> Ok
        | EvalStackValue.ObjectRef addr ->
            failwith $"Conv_ovf_i_un: refusing to convert object reference %O{addr} to signed native int"
        | EvalStackValue.UserDefinedValueType valueType ->
            failwith $"Conv_ovf_i_un: refusing to convert user-defined value type %O{valueType} to signed native int"

    // Helper to get the target CliType for each Ldind variant
    let private getTargetLdindCliType (targetType : LdindTargetType) : CliType =
        match targetType with
        | LdindI -> CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
        | LdindI1 -> CliType.Numeric (CliNumericType.Int8 0y)
        | LdindI2 -> CliType.Numeric (CliNumericType.Int16 0s)
        | LdindI4 -> CliType.Numeric (CliNumericType.Int32 0)
        | LdindI8 -> CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))
        | LdindU1 -> CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy))
        | LdindU2 -> CliType.Numeric (CliNumericType.UInt16 0us)
        | LdindU4 ->
            // This doesn't actually exist as a CLI type
            CliType.Numeric (CliNumericType.Int32 0)
        | LdindU8 ->
            // This doesn't actually exist as a CLI type
            CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))
        | LdindR4 -> CliType.Numeric (CliNumericType.Float32 0.0f)
        | LdindR8 -> CliType.Numeric (CliNumericType.Float64 0.0)

    /// Does a plain-byref `ldind` of `target` over a storage cell of `cell` have to
    /// reinterpret the cell's bytes, rather than read the cell and coerce it?
    ///
    /// Reading the typed cell is right when the cell already holds a value of the requested
    /// kind, and it is the *only* possibility for a cell whose provenance has no byte image —
    /// a `TypeHandlePtr`, a `WidenedNativeInt`, synthesised hash bits — because
    /// `CliType.ToBytes` deliberately refuses those. It is not right when the requested type
    /// is strictly *narrower* than the cell: `*(byte*)&aLong` asks for the byte at that
    /// address, and the cell is eight bytes of something else, so the answer is a
    /// reinterpretation rather than a numeric conversion. Exactly that case routes through
    /// `readManagedByrefBytesAs`, which is what every other byref root already does for it
    /// (`readArrayBytesAs`, `readStackMemoryBytesAs`, `tryReadHeapValueFieldPrecise`).
    ///
    /// The test is on *width* alone, with no same-kind clause. Same-width kind changes
    /// (`ldind.i4` over a `Float32` cell, `ldind.i` over an `Int64` one) therefore keep the
    /// incumbent answer, which is what preserves provenance through `ldind.i8` over a
    /// native-int cell.
    ///
    /// Primitive-like single-field wrappers (`IntPtr`, `RuntimeTypeHandle`, enums) are
    /// unwrapped first, because this predicate's contract is "classify what
    /// `EvalStackValue.ofCliType` will hand to `toCliTypeCoerced`", and that function
    /// flattens them through `CliValueType.PrimitiveLikeField` before the coercion sees
    /// them. Classifying the wrapper itself would answer a different question.
    /// Whether the typed field descent can serve a `size`-byte window at offset 0 of this
    /// struct: a field of exactly that size starts the struct, recursively so when that field
    /// is itself a value type. This mirrors the descent `viewValueTypeAsPrimitive` performs
    /// (an exact `DereferenceFieldAt 0 size` at each level), so `ldindNeedsByteView` routes to
    /// bytes precisely when that descent would have nothing to answer with. The recursion is
    /// unguarded for the same reason `ldindNeedsByteView`'s is, below.
    let rec private hasExactLeadingCell (size : int) (vt : CliValueType) : bool =
        CliValueType.TryFieldsAt 0 vt
        |> List.exists (fun f ->
            f.Size = size
            && (
                match f.Contents with
                | CliType.ValueType inner -> hasExactLeadingCell size inner
                | _ -> true
            )
        )

    let rec private ldindNeedsByteView (target : CliNumericType) (cell : CliType) : bool =
        match cell with
        // The recursion is unguarded for the same reason `ofCliType`'s is: a value type
        // cannot contain itself in well-formed metadata, and both downstream routes walk the
        // identical structure.
        | CliType.ValueType vt when vt.PrimitiveLikeKind.IsSome ->
            ldindNeedsByteView target (CliValueType.PrimitiveLikeField vt).Contents
        // `CliNumericType.SizeOf` is a function of the kind constructor, so a same-kind pair
        // can never be strictly narrower and a `not (SameKind ...)` conjunct would be
        // unfalsifiable.
        | CliType.Numeric c -> CliNumericType.SizeOf target < CliNumericType.SizeOf c
        // A load strictly narrower than the struct is a byte view of its leading bytes,
        // exactly as it is over a numeric cell; route it through the same byte walk that
        // already serves nonzero displacements and `conv`'d pointers — but only when the
        // typed descent has nothing to answer with. A field chain exactly covering the
        // window (an exact-size field at offset 0, recursively so through nested value
        // types, matching `viewValueTypeAsPrimitive`'s own descent) is served by the
        // typed cell route, which preserves provenance — a leading pointer field, say —
        // that a byte rendering cannot carry. Equal-or-wider targets and exactly-covered
        // windows therefore keep their old route, and the byte view fires precisely on
        // the loads that previously crashed.
        | CliType.ValueType vt ->
            let targetSize = CliNumericType.SizeOf target

            targetSize < (CliValueType.SizeOf vt).Size
            && not (hasExactLeadingCell targetSize vt)
        | CliType.Bool _
        | CliType.Char _
        | CliType.ObjectRef _
        | CliType.RuntimePointer _ -> false

    // Unified Ldind implementation
    let private executeLdind
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (targetType : LdindTargetType)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : ExecutionResult
        =
        let popped, state = IlMachineState.popEvalStack currentThread state

        match popped with
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ManagedPointer ManagedPointerSource.Null
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null) ->
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                corelib
                corelib.NullReferenceException
                currentThread
                state
            |> ExecutionResult.stepped
        | EvalStackValue.NativeInt (NativeIntSource.PerInstInfoPtr handle) ->
            // First deref of the `MethodTable*** PerInstInfo` chain: step
            // from MethodTable*** to MethodTable** (the per-instance
            // dictionary pointer). The chain is walked only via `ldind.i`;
            // narrowing widths would betray the synthetic provenance.
            match targetType with
            | LdindI ->
                let state =
                    state
                    |> IlMachineState.pushToEvalStack
                        (CliType.RuntimePointer (CliRuntimePointer.PerInstDictPtr handle))
                        currentThread
                    |> IlMachineState.advanceProgramCounter currentThread

                (state, WhatWeDid.Executed) |> ExecutionResult.stepped
            | _ ->
                failwith
                    $"Ldind %O{targetType} on PerInstInfoPtr %O{handle} is not modelled; only LdindI walks the synthetic PerInstInfo chain"
        | EvalStackValue.NativeInt (NativeIntSource.PerInstDictPtr handle) ->
            // Second deref of the `MethodTable*** PerInstInfo` chain: the
            // first slot of a `System.Nullable\`1` instantiation's
            // single per-instance dictionary holds T's MethodTable*. The
            // projection that minted this synthetic value already gates to
            // Nullable; we re-check here as a defense-in-depth invariant
            // because `Generics.[0]` is only the correct slot when the type
            // has exactly one dictionary (the inherited-dictionary layout
            // for derived generics would put the base's dictionary first).
            match targetType with
            | LdindI ->
                let concreteType =
                    match AllConcreteTypes.lookup handle state.ConcreteTypes with
                    | Some c -> c
                    | None ->
                        failwith $"Ldind on PerInstDictPtr: handle %O{handle} was not registered in AllConcreteTypes"

                let isNullable =
                    InternalTypeKind.kind corelib concreteType = InternalTypeKind.Nullable

                if not isNullable then
                    failwith
                        $"Ldind on PerInstDictPtr %O{handle}: PawPrint only models the synthetic PerInstInfo dictionary chain for System.Nullable`1 today; broader support requires explicit dictionary-index modelling"

                if concreteType.Generics.IsEmpty then
                    failwith
                        $"Ldind on PerInstDictPtr %O{handle}: System.Nullable`1 instantiation unexpectedly has no generic arguments"

                let firstArg = concreteType.Generics.[0]

                let state =
                    state
                    |> IlMachineState.pushToEvalStack
                        (CliType.RuntimePointer (
                            CliRuntimePointer.MethodTablePtr (RuntimeTypeHandleTarget.Closed firstArg)
                        ))
                        currentThread
                    |> IlMachineState.advanceProgramCounter currentThread

                (state, WhatWeDid.Executed) |> ExecutionResult.stepped
            | _ ->
                failwith
                    $"Ldind %O{targetType} on PerInstDictPtr %O{handle} is not modelled; only LdindI walks the synthetic PerInstInfo chain"
        | _ ->

        let targetCliType = getTargetLdindCliType targetType

        let loadedValue =
            match popped with
            | EvalStackValue.ManagedPointer src when
                isStackMemoryPointer src
                || isNativeMemoryPointer src
                || isTrailingByteViewPointer src
                ->
                IlMachineState.readManagedByrefBytesAs corelib state src targetCliType
            // Must stay *after* the guarded arm above, for two reasons. A byte-only root has no
            // typed cell for `readManagedByref` to return, so reading one to ask the routing
            // question would throw before the question could be answered. And
            // `isTrailingByteViewPointer` already claims every chain ending in a `ReinterpretAs`,
            // so what reaches here is a *plain* byref — `[]` or a chain ending in a `Field` — for
            // which `readManagedByref` falls to its `readProjectedValue` branch and hands back
            // the projected cell verbatim, rather than reinterpreting anything itself.
            | EvalStackValue.ManagedPointer src ->
                let target =
                    match targetCliType with
                    | CliType.Numeric target -> target
                    | other ->
                        failwith
                            $"Ldind target type is always numeric (`getTargetLdindCliType`, and `ldind.ref` has its own handler), but %O{targetType} produced %O{other}"

                // The cell has to be read before the routing question can be asked, and is
                // discarded when the answer is "bytes". Reads are pure, so this costs a walk
                // and nothing else.
                let cell = IlMachineState.readManagedByref corelib state src

                if ldindNeedsByteView target cell then
                    IlMachineState.readManagedByrefBytesAs corelib state src targetCliType
                else
                    cell
            | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer src) ->
                IlMachineState.readManagedByrefBytesAs corelib state src targetCliType
            | EvalStackValue.NativeInt nativeIntSource ->
                failwith $"TODO: Native int pointer dereferencing not implemented for {targetType}"
            | EvalStackValue.NullObjectRef -> failwith "unreachable: NullObjectRef handled above"
            | EvalStackValue.ObjectRef _ ->
                failwith "Ldind on an object reference is invalid; expected a managed pointer (byref)"
            | other -> failwith $"Unexpected eval stack value for Ldind operation: {other}"

        let loadedValue = loadedValue |> EvalStackValue.ofCliType

        let coercedValue = EvalStackValue.toCliTypeCoerced targetCliType loadedValue

        let state =
            state
            |> IlMachineState.pushToEvalStack coercedValue currentThread
            |> IlMachineState.advanceProgramCounter currentThread

        (state, WhatWeDid.Executed) |> ExecutionResult.stepped

    let private stind
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (varType : CliType)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : ExecutionResult
        =
        let valueToStore, state = IlMachineState.popEvalStack currentThread state
        let addr, state = IlMachineState.popEvalStack currentThread state

        match addr with
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ManagedPointer ManagedPointerSource.Null
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null) ->
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                corelib
                corelib.NullReferenceException
                currentThread
                state
            |> ExecutionResult.stepped
        | _ ->

        let state =
            match addr with
            | EvalStackValue.Int32 _
            | EvalStackValue.Int64 _
            | EvalStackValue.UserDefinedValueType _
            | EvalStackValue.Float _ ->
                failwith $"unexpectedly tried to store value {valueToStore} in a non-address {addr}"
            | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer src) ->
                IlMachineState.writeIndirectPrimitiveStore
                    corelib
                    state
                    src
                    (EvalStackValue.toCliTypeCoerced varType valueToStore)
            | EvalStackValue.NativeInt nativeIntSource ->
                failwith $"TODO: Native int pointer store not implemented for %O{nativeIntSource}"
            | EvalStackValue.ManagedPointer src ->
                IlMachineState.writeIndirectPrimitiveStore
                    corelib
                    state
                    src
                    (EvalStackValue.toCliTypeCoerced varType valueToStore)
            | EvalStackValue.NullObjectRef -> failwith "unreachable: NullObjectRef handled above"
            | EvalStackValue.ObjectRef _ ->
                failwith "stind on an object reference is invalid; expected a managed pointer"

        state
        |> IlMachineState.advanceProgramCounter currentThread
        |> Tuple.withRight WhatWeDid.Executed
        |> ExecutionResult.stepped

    let internal getArrayElt
        (index : EvalStackValue)
        (arr : EvalStackValue)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : CliType
        =
        let index =
            match index with
            | EvalStackValue.NativeInt src ->
                match src with
                | NativeIntSource.FunctionPointer _
                | NativeIntSource.FieldHandlePtr _
                | NativeIntSource.MethodHandlePtr _
                | NativeIntSource.TypeHandlePtr _
                | NativeIntSource.TypeDescPtr _
                | NativeIntSource.MethodTablePtr _
                | NativeIntSource.MethodTableAuxiliaryDataPtr _
                | NativeIntSource.PerInstInfoPtr _
                | NativeIntSource.PerInstDictPtr _
                | NativeIntSource.GcHandlePtr _
                | NativeIntSource.AssemblyHandle _
                | NativeIntSource.ModuleHandle _
                | NativeIntSource.MetadataImportHandle _
                | NativeIntSource.EventPipeProviderPtr _
                | NativeIntSource.EventPipeEventPtr _
                | NativeIntSource.LowLevelMonitorPtr _
                | NativeIntSource.WaitHandlePtr _
                | NativeIntSource.ManagedPointer _ -> failwith "Refusing to treat a pointer as an array index"
                | NativeIntSource.SyntheticCrossArrayOffset _ ->
                    failwith "Refusing to treat a synthetic cross-storage byte offset as an array index"
                | NativeIntSource.OpaqueHashBits bits ->
                    // Synthesised hash bits narrowing to an array index is the
                    // exact cast-cache load path: `(int)((hash * ...) >> shift)`
                    // yields a bucket index. Truncate via int32 the same way as
                    // Verbatim.
                    bits |> int32
                | NativeIntSource.Verbatim i -> i |> int32
            | EvalStackValue.Int32 int32Source ->
                let i = Int32Source.value "array index" int32Source
                i
            | _ -> failwith $"Invalid index: {index}"

        let arrAddr =
            match arr with
            | EvalStackValue.ObjectRef addr -> addr
            | EvalStackValue.NullObjectRef -> failwith "TODO: throw NRE"
            | _ -> failwith $"Invalid array: %O{arr}"

        IlMachineState.getArrayValue arrAddr index state

    /// Read an array element and project it to the width and signedness that the concrete-width
    /// `ldelem.*` opcode is asking for.
    ///
    /// Array cells hold the element type's *declared* `CliType` — `CliType.Bool` for `bool[]`,
    /// `CliType.Char` for `char[]`, a primitive-like value type for `nint[]`/`nuint[]`/enum
    /// arrays — which is the same representation locals, fields and statics use. The opcode is
    /// asking for a *view* of that cell at a given width.
    let internal ldElem
        (targetCliType : CliType)
        (index : EvalStackValue)
        (arr : EvalStackValue)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : ExecutionResult
        =
        let value = getArrayElt index arr currentThread state

        // The identical two-step projection `executeLdind` performs: `ofCliType` canonically
        // widens the stored form (flattening `Bool`, `Char` and primitive-like wrappers to
        // their underlying primitive), and `toCliTypeCoerced` narrows that to the requested
        // template.
        let coerced =
            value
            |> EvalStackValue.ofCliType
            |> EvalStackValue.toCliTypeCoerced targetCliType

        state
        |> IlMachineState.pushToEvalStack coerced currentThread
        |> IlMachineState.advanceProgramCounter currentThread
        |> Tuple.withRight WhatWeDid.Executed
        |> ExecutionResult.stepped

    let internal endfilterAccepts (filterResult : EvalStackValue) : bool =
        match filterResult with
        | EvalStackValue.Int32 (Int32Source.Verbatim 0) -> false
        | EvalStackValue.Int32 (Int32Source.Verbatim _) -> true
        // A byref that `conv.i4` truncated is not known to be non-zero: the low half
        // of an address PawPrint does not model can perfectly well be zero. Guessing
        // "true" here would pick an exception handler the guest did not select.
        | EvalStackValue.Int32 (Int32Source.NarrowedManagedPointer ptr) ->
            failwith
                $"Endfilter: refusing to decide whether managed pointer %O{ptr}, truncated to 32 bits, is a non-zero filter result; that depends on the container's address, which PawPrint does not model"
        | value -> failwith $"Endfilter requires an int32 result on the stack; got %O{value}"

    /// Store into an array element, coercing to the array's *declared* element type.
    let internal stElem
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
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
                | NativeIntSource.MethodHandlePtr _
                | NativeIntSource.TypeHandlePtr _
                | NativeIntSource.TypeDescPtr _
                | NativeIntSource.MethodTablePtr _
                | NativeIntSource.MethodTableAuxiliaryDataPtr _
                | NativeIntSource.PerInstInfoPtr _
                | NativeIntSource.PerInstDictPtr _
                | NativeIntSource.GcHandlePtr _
                | NativeIntSource.AssemblyHandle _
                | NativeIntSource.ModuleHandle _
                | NativeIntSource.MetadataImportHandle _
                | NativeIntSource.EventPipeProviderPtr _
                | NativeIntSource.EventPipeEventPtr _
                | NativeIntSource.LowLevelMonitorPtr _
                | NativeIntSource.WaitHandlePtr _
                | NativeIntSource.ManagedPointer _ -> failwith "Refusing to treat a pointer as an array index"
                | NativeIntSource.SyntheticCrossArrayOffset _ ->
                    failwith "Refusing to treat a synthetic cross-storage byte offset as an array index"
                | NativeIntSource.OpaqueHashBits bits -> bits |> int32
                | NativeIntSource.Verbatim i -> i |> int32
            | EvalStackValue.Int32 int32Source ->
                let i = Int32Source.value "array index" int32Source
                i
            | _ -> failwith $"Invalid index: {index}"

        let arrAddr =
            match arr with
            | EvalStackValue.ObjectRef addr -> addr
            | EvalStackValue.NullObjectRef -> failwith "TODO: throw NRE"
            | _ -> failwith $"Invalid array: %O{arr}"

        let arr = ManagedHeap.getArrayShape arrAddr state.ManagedHeap

        if index < 0 || index >= arr.Length then
            failwith "TODO: throw IndexOutOfRangeException"

        // ECMA-335 III.4.x runtime-assignment-compatibility gate (see
        // IlMachineStateExecution.checkArrayStoreVariance).
        match
            IlMachineStateExecution.checkArrayStoreVariance
                loggerFactory
                baseClassTypes
                currentThread
                arrAddr
                value
                state
        with
        | IlMachineStateExecution.ArrayStoreVarianceCheck.Raised state ->
            ExecutionResult.stepped (state, WhatWeDid.Executed)
        | IlMachineStateExecution.ArrayStoreVarianceCheck.Allowed state ->

        // Re-read the allocation from the post-check state rather than reusing the `arr` binding
        // above: the variance check returns an updated state, and everything after it should be
        // derived from that one. (An array's `ConcreteType` is fixed at allocation, so this is
        // the same handle either way.)
        let elementHandle =
            match (ManagedHeap.getArrayShape arrAddr state.ManagedHeap).ConcreteType with
            | ConcreteTypeHandle.OneDimArrayZero element -> element
            | other ->
                failwith
                    $"stelem reached an array whose concrete type is not a single-dimension zero-based array (multi-dimensional stores go through Array::Set): %O{other}"

        let targetCliTypeZero, state =
            IlMachineState.cliTypeZeroOfHandle state baseClassTypes elementHandle

        let state =
            state
            |> IlMachineState.setArrayValue arrAddr (EvalStackValue.toCliTypeCoerced targetCliTypeZero value) index
            |> IlMachineState.advanceProgramCounter currentThread

        ExecutionResult.stepped (state, WhatWeDid.Executed)

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
            |> ExecutionResult.stepped
        | LdArg0 ->
            state
            |> IlMachineState.loadArgument currentThread 0
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | LdArg1 ->
            state
            |> IlMachineState.loadArgument currentThread 1
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | LdArg2 ->
            state
            |> IlMachineState.loadArgument currentThread 2
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | LdArg3 ->
            state
            |> IlMachineState.loadArgument currentThread 3
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Ldloc_0 ->
            let localVar = state.ThreadState.[currentThread].MethodState.LocalVariables.[0]

            state
            |> IlMachineState.pushToEvalStack localVar currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Ldloc_1 ->
            let localVar = state.ThreadState.[currentThread].MethodState.LocalVariables.[1]

            state
            |> IlMachineState.pushToEvalStack localVar currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Ldloc_2 ->
            let localVar = state.ThreadState.[currentThread].MethodState.LocalVariables.[2]

            state
            |> IlMachineState.pushToEvalStack localVar currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Ldloc_3 ->
            let localVar = state.ThreadState.[currentThread].MethodState.LocalVariables.[3]

            state
            |> IlMachineState.pushToEvalStack localVar currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Pop ->
            IlMachineState.popEvalStack currentThread state
            |> snd
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Dup ->
            let topValue =
                match IlMachineState.peekEvalStack currentThread state with
                | None -> failwith "tried to Dup when nothing on top of stack"
                | Some v -> v

            state
            |> IlMachineState.pushToEvalStack' topValue currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Ret ->
            match IlMachineState.returnStackFrame loggerFactory corelib currentThread state with
            | ReturnFrameResult.NoFrameToReturn -> ExecutionResult.Terminated (state, currentThread)
            | ReturnFrameResult.NormalReturn state -> (state, WhatWeDid.Executed) |> ExecutionResult.stepped
            | ReturnFrameResult.DispatchException (state, exnAddr, exnType, message) ->
                // The ctor has run; now overwrite _HResult with the CLR's mapped value,
                // matching EEException::CreateThrowable's SetHResult(GetHR()) post-ctor step.
                let state =
                    ExceptionDispatching.overwriteHResultPostCtor corelib exnAddr exnType state

                // The raiser asked for a specific message, i.e. the CLR would have used a
                // message-taking ctor overload here. This has to happen after the ctor, which
                // has just written the type's default resource string into `_message`.
                let state =
                    match message with
                    | None -> state
                    | Some message -> IlMachineState.setExceptionMessage loggerFactory corelib exnAddr message state

                match
                    ExceptionDispatching.throwExceptionObject loggerFactory corelib state currentThread exnAddr exnType
                with
                | ExceptionDispatchResult.Dispatched state -> (state, WhatWeDid.Executed) |> ExecutionResult.stepped
                | ExceptionDispatchResult.ExceptionUnhandled (state, exn) ->
                    ExecutionResult.UnhandledException (state, currentThread, exn)
        | LdcI4_0 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 0)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | LdcI4_1 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 1)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | LdcI4_2 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 2)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | LdcI4_3 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 3)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | LdcI4_4 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 4)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | LdcI4_5 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 5)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | LdcI4_6 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 6)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | LdcI4_7 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 7)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | LdcI4_8 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 8)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | LdcI4_m1 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 -1)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | LdNull ->
            let state =
                state
                |> IlMachineState.pushToEvalStack' EvalStackValue.NullObjectRef currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Ceq ->
            let var2, state = state |> IlMachineState.popEvalStack currentThread
            let var1, state = state |> IlMachineState.popEvalStack currentThread

            let comparisonResult =
                if
                    EvalStackValueComparisons.ceqDeferred state.PointerHashState var1 var2
                    |> StorageLocation.resolveCeq corelib state
                then
                    1
                else
                    0

            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.Int32 (Int32Source.Verbatim comparisonResult))
                currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Cgt ->
            let var2, state = state |> IlMachineState.popEvalStack currentThread
            let var1, state = state |> IlMachineState.popEvalStack currentThread

            let comparisonResult = if EvalStackValueComparisons.cgt var1 var2 then 1 else 0

            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.Int32 (Int32Source.Verbatim comparisonResult))
                currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Cgt_un ->
            let var2, state = state |> IlMachineState.popEvalStack currentThread
            let var1, state = state |> IlMachineState.popEvalStack currentThread

            let comparisonResult = if EvalStackValueComparisons.cgtUn var1 var2 then 1 else 0

            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.Int32 (Int32Source.Verbatim comparisonResult))
                currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Clt ->
            let var2, state = state |> IlMachineState.popEvalStack currentThread
            let var1, state = state |> IlMachineState.popEvalStack currentThread

            let comparisonResult = if EvalStackValueComparisons.clt var1 var2 then 1 else 0

            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.Int32 (Int32Source.Verbatim comparisonResult))
                currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Clt_un ->
            let var2, state = state |> IlMachineState.popEvalStack currentThread
            let var1, state = state |> IlMachineState.popEvalStack currentThread

            let comparisonResult = if EvalStackValueComparisons.cltUn var1 var2 then 1 else 0

            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.Int32 (Int32Source.Verbatim comparisonResult))
                currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Stloc_0 ->
            state
            |> IlMachineState.popFromStackToLocalVariable currentThread 0
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Stloc_1 ->
            state
            |> IlMachineState.popFromStackToLocalVariable currentThread 1
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Stloc_2 ->
            state
            |> IlMachineState.popFromStackToLocalVariable currentThread 2
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Stloc_3 ->
            state
            |> IlMachineState.popFromStackToLocalVariable currentThread 3
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Sub ->
            let val2, state = IlMachineState.popEvalStack currentThread state
            let val1, state = IlMachineState.popEvalStack currentThread state

            let result, state =
                BinaryArithmetic.execute corelib ArithmeticOperation.sub state val1 val2

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Sub_ovf ->
            let val2, state = IlMachineState.popEvalStack currentThread state
            let val1, state = IlMachineState.popEvalStack currentThread state

            match
                try
                    BinaryArithmetic.execute corelib ArithmeticOperation.subOvf state val1 val2
                    |> Ok
                with :? OverflowException as e ->
                    Error e
            with
            | Ok (result, state) ->
                state
                |> IlMachineState.pushToEvalStack' result currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.stepped
            | Error _ ->
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    corelib
                    corelib.OverflowException
                    currentThread
                    state
                |> ExecutionResult.stepped
        | Sub_ovf_un -> failwith "TODO: Sub_ovf_un unimplemented"
        | Add ->
            let val2, state = IlMachineState.popEvalStack currentThread state
            let val1, state = IlMachineState.popEvalStack currentThread state

            let result, state =
                BinaryArithmetic.execute corelib ArithmeticOperation.add state val1 val2

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Add_ovf ->
            let val2, state = IlMachineState.popEvalStack currentThread state
            let val1, state = IlMachineState.popEvalStack currentThread state

            match
                try
                    BinaryArithmetic.execute corelib ArithmeticOperation.addOvf state val1 val2
                    |> Ok
                with :? OverflowException as e ->
                    Error e
            with
            | Ok (result, state) ->
                state
                |> IlMachineState.pushToEvalStack' result currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.stepped
            | Error _ ->
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    corelib
                    corelib.OverflowException
                    currentThread
                    state
                |> ExecutionResult.stepped
        | Add_ovf_un -> failwith "TODO: Add_ovf_un unimplemented"
        | Mul ->
            let val2, state = IlMachineState.popEvalStack currentThread state
            let val1, state = IlMachineState.popEvalStack currentThread state

            let result, state =
                BinaryArithmetic.execute corelib ArithmeticOperation.mul state val1 val2

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Mul_ovf ->
            let val2, state = IlMachineState.popEvalStack currentThread state
            let val1, state = IlMachineState.popEvalStack currentThread state

            match
                try
                    BinaryArithmetic.execute corelib ArithmeticOperation.mulOvf state val1 val2
                    |> Ok
                with :? OverflowException as e ->
                    Error e
            with
            | Ok (result, state) ->
                state
                |> IlMachineState.pushToEvalStack' result currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.stepped
            | Error _ ->
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    corelib
                    corelib.OverflowException
                    currentThread
                    state
                |> ExecutionResult.stepped
        | Mul_ovf_un ->
            let val2, state = IlMachineState.popEvalStack currentThread state
            let val1, state = IlMachineState.popEvalStack currentThread state

            match
                try
                    BinaryArithmetic.execute corelib ArithmeticOperation.mulOvfUn state val1 val2
                    |> Ok
                with :? OverflowException as e ->
                    Error e
            with
            | Ok (result, state) ->
                state
                |> IlMachineState.pushToEvalStack' result currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.stepped
            | Error _ ->
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    corelib
                    corelib.OverflowException
                    currentThread
                    state
                |> ExecutionResult.stepped
        | Div ->
            let val2, state = IlMachineState.popEvalStack currentThread state
            let val1, state = IlMachineState.popEvalStack currentThread state

            executeFaultingArithmetic
                loggerFactory
                corelib
                ArithmeticFaults.DivideByZeroOrOverflow
                currentThread
                state
                (fun () -> BinaryArithmetic.execute corelib ArithmeticOperation.div state val1 val2)
        | Div_un ->
            let v2, state = IlMachineState.popEvalStack currentThread state
            let v1, state = IlMachineState.popEvalStack currentThread state

            executeFaultingArithmetic
                loggerFactory
                corelib
                ArithmeticFaults.DivideByZero
                currentThread
                state
                (fun () -> divUnValues v1 v2, state)
        | Shr ->
            let shift, state = IlMachineState.popEvalStack currentThread state
            let number, state = IlMachineState.popEvalStack currentThread state

            let shift =
                match shift with
                | EvalStackValue.Int32 int32Source ->
                    let i = Int32Source.value "Shr shift count" int32Source
                    i
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) -> int<int64> i
                | _ -> failwith $"Not allowed shift of {shift}"

            let result, state =
                // See table III.6
                match number with
                | EvalStackValue.Int32 int32Source ->
                    let i = Int32Source.value "Shr" int32Source
                    i >>> shift |> Int32Source.Verbatim |> EvalStackValue.Int32, state
                | EvalStackValue.Int64 i ->
                    let r, counters = Int64Source.shr "Shr" i shift state.PointerHashState

                    EvalStackValue.Int64 r,
                    { state with
                        PointerHashState = counters
                    }
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) ->
                    (i >>> shift) |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt, state
                | _ -> failwith $"Not allowed to shift {number}"

            let state =
                state
                |> IlMachineState.pushToEvalStack' result currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Shr_un ->
            let shift, state = IlMachineState.popEvalStack currentThread state
            let number, state = IlMachineState.popEvalStack currentThread state

            let shift =
                match shift with
                | EvalStackValue.Int32 int32Source ->
                    let i = Int32Source.value "Shr_un shift count" int32Source
                    i
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) -> int<int64> i
                | _ -> failwith $"Not allowed shift of {shift}"

            let result, state =
                // See table III.6
                match number with
                | EvalStackValue.Int32 int32Source ->
                    let i = Int32Source.value "Shr_un" int32Source

                    uint32<int> i >>> shift
                    |> int32<uint32>
                    |> Int32Source.Verbatim
                    |> EvalStackValue.Int32,
                    state
                | EvalStackValue.Int64 i ->
                    let r, counters = Int64Source.shrUn "Shr_un" i shift state.PointerHashState

                    EvalStackValue.Int64 r,
                    { state with
                        PointerHashState = counters
                    }
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) ->
                    (uint64<int64> i >>> shift |> int64<uint64>)
                    |> NativeIntSource.Verbatim
                    |> EvalStackValue.NativeInt,
                    state
                | _ -> failwith $"Not allowed to shift {number}"

            let state =
                state
                |> IlMachineState.pushToEvalStack' result currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Shl ->
            let shift, state = IlMachineState.popEvalStack currentThread state
            let number, state = IlMachineState.popEvalStack currentThread state

            let shift =
                match shift with
                | EvalStackValue.Int32 int32Source ->
                    let i = Int32Source.value "Shl shift count" int32Source
                    i
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) -> int<int64> i
                | _ -> failwith $"Not allowed shift of {shift}"

            let result, state =
                // See table III.6
                match number with
                | EvalStackValue.Int32 int32Source ->
                    let i = Int32Source.value "Shl" int32Source
                    i <<< shift |> Int32Source.Verbatim |> EvalStackValue.Int32, state
                | EvalStackValue.Int64 i ->
                    let r, counters = Int64Source.shl "Shl" i shift state.PointerHashState

                    EvalStackValue.Int64 r,
                    { state with
                        PointerHashState = counters
                    }
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) ->
                    (i <<< shift) |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt, state
                | _ -> failwith $"Not allowed to shift {number}"

            let state =
                state
                |> IlMachineState.pushToEvalStack' result currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | And ->
            let v2, state = IlMachineState.popEvalStack currentThread state
            let v1, state = IlMachineState.popEvalStack currentThread state

            let result, state =
                match v1, v2 with
                // The alignment test CoreLib writes as `((int)p & 1) != 0`: `conv.i4`
                // kept the byref alive precisely so this mask could be asked. See
                // `andNarrowedManagedPointerBits`.
                | EvalStackValue.Int32 (Int32Source.NarrowedManagedPointer ptr),
                  EvalStackValue.Int32 (Int32Source.Verbatim mask)
                | EvalStackValue.Int32 (Int32Source.Verbatim mask),
                  EvalStackValue.Int32 (Int32Source.NarrowedManagedPointer ptr) ->
                    andNarrowedManagedPointerBits state ptr (int64<int32> mask)
                    |> EvalStackValue.Int32,
                    state
                // Two truncated byrefs: `p & q` would need both addresses.
                | EvalStackValue.Int32 (Int32Source.NarrowedManagedPointer p1),
                  EvalStackValue.Int32 (Int32Source.NarrowedManagedPointer p2) ->
                    failwith
                        $"And: refusing to mask one truncated managed pointer with another (%O{p1} and %O{p2}); the result would depend on both containers' addresses, which PawPrint does not model"
                | EvalStackValue.Int32 (Int32Source.Verbatim v1), EvalStackValue.Int32 (Int32Source.Verbatim v2) ->
                    v1 &&& v2 |> Int32Source.Verbatim |> EvalStackValue.Int32, state
                | EvalStackValue.Int32 (Int32Source.Verbatim mask),
                  EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr) ->
                    int64<int32> mask |> andManagedPointerAddressBits state ptr, state
                | EvalStackValue.Int32 (Int32Source.Verbatim v1), EvalStackValue.NativeInt (NativeIntSource.Verbatim v2) ->
                    int64<int32> v1 &&& v2 |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt, state
                | EvalStackValue.Int32 (Int32Source.Verbatim mask), EvalStackValue.NativeInt src ->
                    andNativeIntAddressBits state src (int64<int32> mask), state
                | EvalStackValue.Int32 (Int32Source.Verbatim mask), EvalStackValue.ManagedPointer ptr ->
                    int64<int32> mask |> andManagedPointerAddressBits state ptr, state
                | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 ->
                    let r, counters = Int64Source.bitAnd "And" v1 v2 state.PointerHashState

                    EvalStackValue.Int64 r,
                    { state with
                        PointerHashState = counters
                    }
                | EvalStackValue.Int64 mask, EvalStackValue.ManagedPointer ptr -> failwith "TODO"
                // andManagedPointerAddressBits state ptr mask
                | EvalStackValue.Int64 mask, EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr) ->
                    // andManagedPointerAddressBits state ptr mask
                    failwith "TODO"
                | EvalStackValue.ManagedPointer ptr, EvalStackValue.Int32 (Int32Source.Verbatim mask) ->
                    int64<int32> mask |> andManagedPointerAddressBits state ptr, state
                | EvalStackValue.ManagedPointer ptr, EvalStackValue.Int64 mask ->
                    // andManagedPointerAddressBits state ptr mask
                    failwith "TODO"
                | EvalStackValue.ManagedPointer ptr, EvalStackValue.NativeInt (NativeIntSource.Verbatim mask)
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim mask), EvalStackValue.ManagedPointer ptr ->
                    andManagedPointerAddressBits state ptr mask, state
                | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr),
                  EvalStackValue.Int32 (Int32Source.Verbatim mask) ->
                    int64<int32> mask |> andManagedPointerAddressBits state ptr, state
                | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr), EvalStackValue.Int64 mask ->
                    // andManagedPointerAddressBits state ptr mask
                    failwith "TODO"
                | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr),
                  EvalStackValue.NativeInt (NativeIntSource.Verbatim mask)
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim mask),
                  EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr) ->
                    andManagedPointerAddressBits state ptr mask, state
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim v1), EvalStackValue.Int32 (Int32Source.Verbatim v2) ->
                    v1 &&& int64<int32> v2 |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt, state
                | EvalStackValue.NativeInt src, EvalStackValue.Int32 (Int32Source.Verbatim mask) ->
                    andNativeIntAddressBits state src (int64<int32> mask), state
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim v1),
                  EvalStackValue.NativeInt (NativeIntSource.Verbatim v2) ->
                    v1 &&& v2 |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt, state
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim mask), EvalStackValue.NativeInt src ->
                    andNativeIntAddressBits state src mask, state
                | EvalStackValue.NativeInt src, EvalStackValue.NativeInt (NativeIntSource.Verbatim mask) ->
                    andNativeIntAddressBits state src mask, state
                | _, _ -> failwith $"refusing to do binary operation on {v1} and {v2}"

            let state =
                state
                |> IlMachineState.pushToEvalStack' result currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Or ->
            let v2, state = IlMachineState.popEvalStack currentThread state
            let v1, state = IlMachineState.popEvalStack currentThread state

            let result, state =
                match v1, v2 with
                | EvalStackValue.Int32 (Int32Source.Verbatim v1), EvalStackValue.Int32 (Int32Source.Verbatim v2) ->
                    v1 ||| v2 |> Int32Source.Verbatim |> EvalStackValue.Int32, state
                // Managed code tags GC handles by OR-ing bits into the low,
                // known-clear region: `WeakReference.Create` stores
                // `handle | TracksResurrectionBit`, and `GCHandle..ctor` marks a
                // pinned handle with `handle |= 1`.
                | EvalStackValue.Int32 (Int32Source.Verbatim operand),
                  EvalStackValue.NativeInt (NativeIntSource.GcHandlePtr (handle, tag))
                | EvalStackValue.NativeInt (NativeIntSource.GcHandlePtr (handle, tag)),
                  EvalStackValue.Int32 (Int32Source.Verbatim operand) ->
                    gcHandleTagOp "Or" TaggedPointerBits.bitOr handle tag (int64<int32> operand)
                    |> EvalStackValue.NativeInt,
                    state
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim operand),
                  EvalStackValue.NativeInt (NativeIntSource.GcHandlePtr (handle, tag))
                | EvalStackValue.NativeInt (NativeIntSource.GcHandlePtr (handle, tag)),
                  EvalStackValue.NativeInt (NativeIntSource.Verbatim operand) ->
                    gcHandleTagOp "Or" TaggedPointerBits.bitOr handle tag operand
                    |> EvalStackValue.NativeInt,
                    state
                | EvalStackValue.Int32 (Int32Source.Verbatim v1), EvalStackValue.NativeInt (NativeIntSource.Verbatim v2) ->
                    int64<int32> v1 ||| v2 |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt, state
                | EvalStackValue.Int32 _, EvalStackValue.NativeInt _ ->
                    failwith $"can't do binary operation on non-verbatim native int {v2}"
                | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 ->
                    let r, counters = Int64Source.bitOr "Or" v1 v2 state.PointerHashState

                    EvalStackValue.Int64 r,
                    { state with
                        PointerHashState = counters
                    }
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim v1), EvalStackValue.Int32 (Int32Source.Verbatim v2) ->
                    v1 ||| int64<int32> v2 |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt, state
                | EvalStackValue.NativeInt _, EvalStackValue.Int32 _ ->
                    failwith $"can't do binary operation on non-verbatim native int {v1}"
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim v1),
                  EvalStackValue.NativeInt (NativeIntSource.Verbatim v2) ->
                    v1 ||| v2 |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt, state
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim _), EvalStackValue.NativeInt _ ->
                    failwith $"can't do binary operation on non-verbatim native int {v2}"
                | EvalStackValue.NativeInt _, EvalStackValue.NativeInt (NativeIntSource.Verbatim _) ->
                    failwith $"can't do binary operation on non-verbatim native int {v1}"
                | _, _ -> failwith $"refusing to do binary operation on {v1} and {v2}"

            let state =
                state
                |> IlMachineState.pushToEvalStack' result currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Xor ->
            let v2, state = IlMachineState.popEvalStack currentThread state
            let v1, state = IlMachineState.popEvalStack currentThread state

            let result, state =
                match v1, v2 with
                | EvalStackValue.Int32 (Int32Source.Verbatim v1), EvalStackValue.Int32 (Int32Source.Verbatim v2) ->
                    v1 ^^^ v2 |> Int32Source.Verbatim |> EvalStackValue.Int32, state
                | EvalStackValue.Int32 (Int32Source.Verbatim v1), EvalStackValue.NativeInt (NativeIntSource.Verbatim v2) ->
                    int64<int32> v1 ^^^ v2 |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt, state
                | EvalStackValue.Int32 _, EvalStackValue.NativeInt _ ->
                    failwith $"can't do binary operation on non-verbatim native int {v2}"
                | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 ->
                    let r, counters = Int64Source.bitXor "Xor" v1 v2 state.PointerHashState

                    EvalStackValue.Int64 r,
                    { state with
                        PointerHashState = counters
                    }
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim v1), EvalStackValue.Int32 (Int32Source.Verbatim v2) ->
                    v1 ^^^ int64<int32> v2 |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt, state
                | EvalStackValue.NativeInt _, EvalStackValue.Int32 _ ->
                    failwith $"can't do binary operation on non-verbatim native int {v1}"
                | EvalStackValue.NativeInt src1, EvalStackValue.NativeInt src2 ->
                    let r, counters = xorNativeIntSources src1 src2 state.PointerHashState

                    EvalStackValue.NativeInt r,
                    { state with
                        PointerHashState = counters
                    }
                | _, _ -> failwith $"refusing to do binary operation on {v1} and {v2}"

            let state =
                state
                |> IlMachineState.pushToEvalStack' result currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Conv_I ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let converted = EvalStackValue.toNativeInt popped

            // Crossing from byref-world to native-pointer-world: subsequent
            // pointer arithmetic must be byte-stride per ECMA-335 §III.1.5,
            // so anchor a `ReinterpretAs T` projection on plain array
            // byrefs. Plain byrefs (no anchor) keep element-stride
            // arithmetic to match `Unsafe.Add<T>`.
            let conv =
                match converted with
                | NativeIntSource.ManagedPointer ptr ->
                    ManagedPointerByteView.anchorByteViewIfPlainArrayByref corelib state ptr
                    |> NativeIntSource.ManagedPointer
                | other -> other

            let state =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt conv) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Conv_I1 ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let conv, counters = EvalStackValue.convToInt8 popped state.PointerHashState

            let state =
                { state with
                    PointerHashState = counters
                }
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim conv)) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Conv_I2 ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let conv, counters = EvalStackValue.convToInt16 popped state.PointerHashState

            let state =
                { state with
                    PointerHashState = counters
                }
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim conv)) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Conv_I4 ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            let converted, counters = EvalStackValue.convToInt32 popped state.PointerHashState

            let state =
                { state with
                    PointerHashState = counters
                }
                |> IlMachineState.pushToEvalStack' converted currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Conv_I8 ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let conv = EvalStackValue.convToInt64 popped

            let state =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int64 conv) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Conv_R4 ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let conv = EvalStackValue.convToFloat32 popped

            let state =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Float conv) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Conv_R8 ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let conv = EvalStackValue.convToFloat64 popped

            let state =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Float conv) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Conv_U ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let converted = EvalStackValue.toUnsignedNativeInt popped

            // NativeIntSource.Verbatim backs the native-int stack slot with
            // a signed int64, but the bits are what matter: signed and
            // unsigned native-int comparisons reinterpret the slot as
            // needed. `int64 (conv : uint64)` is a bit-exact reinterpret
            // cast in F#, which is what ECMA-335 requires here (truncate
            // high-order bits beyond the native word size).
            let conv =
                match converted with
                | UnsignedNativeIntSource.Verbatim conv -> int64 conv |> NativeIntSource.Verbatim
                | UnsignedNativeIntSource.FromManagedPointer ptr ->
                    // Crossing from byref-world to native-pointer-world: subsequent
                    // pointer arithmetic must be byte-stride per ECMA-335 §III.1.5,
                    // so anchor a `ReinterpretAs T` projection on plain array
                    // byrefs. Plain byrefs (no anchor) keep element-stride
                    // arithmetic to match `Unsafe.Add<T>`.
                    ManagedPointerByteView.anchorByteViewIfPlainArrayByref corelib state ptr
                    |> NativeIntSource.ManagedPointer
                | UnsignedNativeIntSource.FromSyntheticCrossArrayStorage i ->
                    NativeIntSource.SyntheticCrossArrayOffset i
                | UnsignedNativeIntSource.FromOpaqueHashBits bits -> NativeIntSource.OpaqueHashBits bits

            let state =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt conv) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Conv_U1 ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let conv, counters = EvalStackValue.convToUInt8 popped state.PointerHashState

            let state =
                { state with
                    PointerHashState = counters
                }
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim conv)) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Conv_U2 ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let conv, counters = EvalStackValue.convToUInt16 popped state.PointerHashState

            let state =
                { state with
                    PointerHashState = counters
                }
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim conv)) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Conv_U4 ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            let converted, counters = EvalStackValue.convToUInt32 popped state.PointerHashState

            let state =
                { state with
                    PointerHashState = counters
                }
                |> IlMachineState.pushToEvalStack' converted currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Conv_U8 ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let conv = EvalStackValue.convToUInt64 popped

            let state =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int64 conv) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | LdLen ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            let popped =
                match popped with
                | EvalStackValue.NullObjectRef -> failwith "TODO: throw NRE"
                | EvalStackValue.ObjectRef addr -> addr
                | _ -> failwith $"can't get len of {popped}"

            let popped = ManagedHeap.getArrayShape popped state.ManagedHeap

            IlMachineState.pushToEvalStack'
                (EvalStackValue.Int32 (Int32Source.Verbatim popped.Length))
                currentThread
                state
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Endfilter ->
            let filterResult, state = IlMachineState.popEvalStack currentThread state
            let filterAccepted = endfilterAccepts filterResult

            let threadState = state.ThreadState.[currentThread]
            let currentMethodState = threadState.MethodState

            match currentMethodState.EvaluationStack.Values with
            | [] -> ()
            | remaining ->
                failwith
                    $"Endfilter requires the filter evaluation stack to be empty after popping the result; remaining stack was %O{remaining}"

            match MethodState.popExceptionContinuation currentMethodState with
            | Some {
                       Scope = ExceptionContinuationScope.FilterHandler currentFilter
                       Continuation = ExceptionContinuation.ResumeAfterFilter continuation
                   },
              methodStateWithoutFilter ->
                if currentFilter <> continuation.CurrentFilter then
                    failwith
                        $"Endfilter continuation scope %O{currentFilter} did not match continuation %O{continuation.CurrentFilter}"

                // The filter's own scratch stack is gone either way; what remains of the frame is
                // the same whether it accepted or not, and the first pass decides the rest.
                let newMethodState = methodStateWithoutFilter |> MethodState.clearEvalStack

                let newThreadState =
                    ThreadState.setFrame threadState.ActiveMethodState newMethodState threadState

                let state =
                    { state with
                        ThreadState = state.ThreadState |> Map.add currentThread newThreadState
                    }

                // Accepting does *not* enter the handler here. The filter ran in the first pass,
                // with every frame inner to it still live, so acceptance only settles where the
                // exception is going; the second pass still has to unwind to this frame, running
                // the cleanup in between, before the handler body can start.
                match
                    ExceptionDispatching.resumeSearchAfterFilter
                        loggerFactory
                        corelib
                        state
                        currentThread
                        continuation
                        filterAccepted
                with
                | ExceptionDispatchResult.Dispatched state -> (state, WhatWeDid.Executed) |> ExecutionResult.stepped
                | ExceptionDispatchResult.ExceptionUnhandled (state, exn) ->
                    ExecutionResult.UnhandledException (state, currentThread, exn)
            | Some frame, _ ->
                failwith
                    $"Endfilter encountered outside an exception filter; current continuation was scope %O{frame.Scope} with continuation %O{frame.Continuation}"
            | None, _ -> failwith "Endfilter encountered without an exception continuation"
        | Endfinally ->
            let threadState = state.ThreadState.[currentThread]
            let currentMethodState = threadState.MethodState

            let endsWithEndfinally (scope : ExceptionContinuationScope) : bool =
                match scope with
                | ExceptionContinuationScope.FinallyHandler _
                | ExceptionContinuationScope.FaultHandler _ -> true
                | ExceptionContinuationScope.FilterHandler _ -> false

            match MethodState.popExceptionContinuation currentMethodState with
            | None, _ ->
                // Not in a finally block, just advance PC
                state
                |> IlMachineState.advanceProgramCounter currentThread
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.stepped
            | Some {
                       Scope = ExceptionContinuationScope.FinallyHandler justRan
                       Continuation = ExceptionContinuation.ResumeAfterFinally targetPC
                   },
              methodStateWithoutContinuation ->
                // A single `leave` may exit several nested protected regions, and every one of
                // their handlers has to run, innermost first (ECMA-335 III.3.55). `leave`
                // starts the innermost; each handler's `endfinally` asks for its successor
                // here and only resumes at the leave target once the chain is exhausted.
                // Jumping straight to `targetPC` skipped every outer handler — including, for
                // instance, the one in `CancellationTokenSource.ExecuteCallbackHandlers` that
                // clears `ExecutingCallbackId`, whose loss livelocks a cross-thread
                // `CancellationTokenRegistration.Dispose()`.
                let newMethodState =
                    match
                        ExceptionHandling.nextFinallyToRun
                            justRan
                            targetPC
                            methodStateWithoutContinuation.ExecutingMethod
                    with
                    | Some next ->
                        // The eval stack is not cleared again: `leave` emptied it before the
                        // first handler, and a handler must leave it empty at its `endfinally`.
                        methodStateWithoutContinuation
                        |> MethodState.pushExceptionContinuation
                            (ExceptionContinuationScope.FinallyHandler next)
                            (ExceptionContinuation.ResumeAfterFinally targetPC)
                        |> MethodState.setProgramCounter next.HandlerOffset
                    | None -> methodStateWithoutContinuation |> MethodState.setProgramCounter targetPC

                let newThreadState =
                    ThreadState.setFrame threadState.ActiveMethodState newMethodState threadState

                { state with
                    ThreadState = state.ThreadState |> Map.add currentThread newThreadState
                }
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.stepped
            | Some {
                       Scope = scope
                       Continuation = ExceptionContinuation.PropagatingException unwind
                   },
              methodStateWithoutContinuation when endsWithEndfinally scope ->
                // The cleanup clause is done; hand the second pass back the unwind it parked here.
                let threadState =
                    ThreadState.setFrame threadState.ActiveMethodState methodStateWithoutContinuation threadState

                let state =
                    { state with
                        ThreadState = state.ThreadState |> Map.add currentThread threadState
                    }

                // Nothing about the foreign-raise flag happens on this path, and nothing needs to:
                // the raise being resumed appended every frame it will ever append back in the
                // first pass, before this clause began, so a flag the clause itself set belongs to
                // whatever raises next.
                match
                    ExceptionDispatching.resumeUnwindAfterCleanup loggerFactory corelib state currentThread unwind
                with
                | ExceptionDispatchResult.Dispatched state -> (state, WhatWeDid.Executed) |> ExecutionResult.stepped
                | ExceptionDispatchResult.ExceptionUnhandled (state, exn) ->
                    ExecutionResult.UnhandledException (state, currentThread, exn)
            | Some {
                       Scope = ExceptionContinuationScope.FilterHandler _
                       Continuation = ExceptionContinuation.ResumeAfterFilter continuation
                   },
              _ -> failwith $"Endfinally encountered while evaluating exception filter %O{continuation.CurrentFilter}"
            | Some frame, _ ->
                failwith
                    $"Endfinally encountered a non-finally continuation: scope %O{frame.Scope} with continuation %O{frame.Continuation}"
        | Rethrow ->
            let threadState = state.ThreadState.[currentThread]
            let currentMethodState = threadState.MethodState

            match ExceptionDispatching.tryCurrentCatchException currentMethodState with
            | None ->
                failwith
                    $"Rethrow at IL offset %d{currentMethodState.IlOpIndex} of %s{currentMethodState.ExecutingMethod.Name} encountered outside a catch handler"
            | Some cliException ->
                let exceptionType =
                    ExceptionDispatching.exceptionObjectType state cliException.ExceptionObject

                // Nothing about the foreign-raise flag happens here, deliberately. `IL_Rethrow`
                // (jithelpers.cpp:890) never *sets* it, so a rethrow of its own produces no
                // boundary — its frames simply accumulate onto the trace it inherited. It can
                // still *consume* a flag someone else left pending, but CoreCLR's read-and-reset
                // lives in `StackTraceInfo::AppendElement` (excep.cpp:3016), which fires when a
                // frame is appended rather than when a raise begins — so that belongs at
                // `ExceptionDispatching`'s append site, where it is, and a rethrow whose handler
                // lives in this same method leaves the flag pending because it appends nothing at
                // all. See `sourcesPure/ForeignRaiseFlagSurvivesFramelessRethrow.cs`.
                //
                // The raise needs no record of what was pending at this instant either: the
                // first pass appends every frame before any cleanup clause runs, so a `finally`
                // this raise executes on its way out cannot get between the raise and its own
                // appends, and there is no window in which a flag could be mistaken for this
                // raise's to spend.
                //
                // Its `StackTrace` out of `CatchExceptions` *is* stale: that is the snapshot this
                // catch handler was entered with. The trace a rethrow inherits is the exception's
                // own, whatever `_stackTrace` holds now.
                let cliException =
                    { cliException with
                        StackTrace = IlMachineState.frozenStackTraceFrames corelib cliException.ExceptionObject state
                    }

                match
                    ExceptionDispatching.dispatchException
                        loggerFactory
                        corelib
                        state
                        currentThread
                        cliException
                        exceptionType
                with
                | ExceptionDispatchResult.Dispatched state -> (state, WhatWeDid.Executed) |> ExecutionResult.stepped
                | ExceptionDispatchResult.ExceptionUnhandled (state, exn) ->
                    ExecutionResult.UnhandledException (state, currentThread, exn)
        | Throw ->
            // Pop exception object from stack and begin exception handling
            let exceptionObject, state = IlMachineState.popEvalStack currentThread state

            match exceptionObject with
            | EvalStackValue.NullObjectRef ->
                // Per ECMA-335 III.4.31: if the object is null, throw NullReferenceException instead.
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    corelib
                    corelib.NullReferenceException
                    currentThread
                    state
                |> ExecutionResult.stepped
            | _ ->

            let addr =
                match exceptionObject with
                | EvalStackValue.ObjectRef addr -> addr
                | EvalStackValue.NullObjectRef -> failwith "unreachable: NullObjectRef handled above"
                | existing -> failwith $"Throw instruction requires an object reference on the stack; got %O{existing}"

            // Get exception type from heap object
            let heapObject =
                match ManagedHeap.tryGet addr state.ManagedHeap with
                | Some obj -> obj
                | None -> failwith "Exception object not found in heap"

            match
                ExceptionDispatching.throwExceptionObject
                    loggerFactory
                    corelib
                    state
                    currentThread
                    addr
                    heapObject.ConcreteType
            with
            | ExceptionDispatchResult.Dispatched state -> (state, WhatWeDid.Executed) |> ExecutionResult.stepped
            | ExceptionDispatchResult.ExceptionUnhandled (state, exn) ->
                ExecutionResult.UnhandledException (state, currentThread, exn)

        | Localloc ->
            let currentMethodState = state.ThreadState.[currentThread].MethodState

            let stackMemoryInitialization =
                match MethodInfo.tryIlBody currentMethodState.ExecutingMethod with
                | None ->
                    failwith
                        $"Invalid CIL: Localloc reached in method %s{currentMethodState.ExecutingMethod.Name} with no IL body"
                | Some instructions when
                    instructions.ExceptionRegions
                    |> Seq.exists (isLocallocForbiddenExceptionRegion currentMethodState.IlOpIndex)
                    ->
                    failwith
                        $"Invalid CIL: Localloc at IL offset %d{currentMethodState.IlOpIndex} of %s{currentMethodState.ExecutingMethod.Name} is inside an exception handler or filter"
                | Some instructions ->
                    if instructions.LocalsInit then
                        MemoryBlockInitialization.ZeroInitialized
                    else
                        MemoryBlockInitialization.Uninitialized

            let sizeValue, state = IlMachineState.popEvalStack currentThread state

            let remainingStack =
                state.ThreadState.[currentThread].MethodState.EvaluationStack.Values

            if not remainingStack.IsEmpty then
                failwith
                    $"Invalid CIL: Localloc at IL offset %d{currentMethodState.IlOpIndex} of %s{currentMethodState.ExecutingMethod.Name} requires the evaluation stack to be empty after popping the byte count, but found %d{remainingStack.Length} extra value(s)"

            let size = locallocSizeBytes sizeValue

            let ptr, state =
                IlMachineState.allocateStackMemory currentThread stackMemoryInitialization size state

            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr))
                currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Stind_I ->
            stind
                loggerFactory
                corelib
                (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)))
                currentThread
                state
        | Stind_I1 -> stind loggerFactory corelib (CliType.Numeric (CliNumericType.Int8 0y)) currentThread state
        | Stind_I2 -> stind loggerFactory corelib (CliType.Numeric (CliNumericType.Int16 0s)) currentThread state
        | Stind_I4 -> stind loggerFactory corelib (CliType.Numeric (CliNumericType.Int32 0)) currentThread state
        | Stind_I8 ->
            stind
                loggerFactory
                corelib
                (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L)))
                currentThread
                state
        | Stind_R4 -> stind loggerFactory corelib (CliType.Numeric (CliNumericType.Float32 0.0f)) currentThread state
        | Stind_R8 -> stind loggerFactory corelib (CliType.Numeric (CliNumericType.Float64 0.0)) currentThread state
        | Ldind_i -> executeLdind loggerFactory corelib LdindTargetType.LdindI currentThread state
        | Ldind_i1 -> executeLdind loggerFactory corelib LdindTargetType.LdindI1 currentThread state
        | Ldind_i2 -> executeLdind loggerFactory corelib LdindTargetType.LdindI2 currentThread state
        | Ldind_i4 -> executeLdind loggerFactory corelib LdindTargetType.LdindI4 currentThread state
        | Ldind_i8 -> executeLdind loggerFactory corelib LdindTargetType.LdindI8 currentThread state
        | Ldind_u1 -> executeLdind loggerFactory corelib LdindTargetType.LdindU1 currentThread state
        | Ldind_u2 -> executeLdind loggerFactory corelib LdindTargetType.LdindU2 currentThread state
        | Ldind_u4 -> executeLdind loggerFactory corelib LdindTargetType.LdindU4 currentThread state
        | Ldind_u8 -> failwith "TODO: Ldind_u8 unimplemented"
        | Ldind_r4 -> executeLdind loggerFactory corelib LdindTargetType.LdindR4 currentThread state
        | Ldind_r8 -> executeLdind loggerFactory corelib LdindTargetType.LdindR8 currentThread state
        | Rem ->
            let val2, state = IlMachineState.popEvalStack currentThread state
            let val1, state = IlMachineState.popEvalStack currentThread state

            executeFaultingArithmetic
                loggerFactory
                corelib
                ArithmeticFaults.DivideByZeroOrOverflow
                currentThread
                state
                (fun () -> BinaryArithmetic.execute corelib ArithmeticOperation.rem state val1 val2)
        | Rem_un ->
            let val2, state = IlMachineState.popEvalStack currentThread state
            let val1, state = IlMachineState.popEvalStack currentThread state

            executeFaultingArithmetic
                loggerFactory
                corelib
                ArithmeticFaults.DivideByZero
                currentThread
                state
                (fun () -> BinaryArithmetic.execute corelib ArithmeticOperation.remUn state val1 val2)
        | Volatile ->
            // `volatile.` constrains host memory reordering. PawPrint's
            // deterministic execution model has no host reordering to model,
            // so the prefix is an executed no-op for now.
            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Tail ->
            // ECMA-335 III.2.4: `tail.` asks for the caller's frame to be released before
            // control transfers to the following call/callvirt/calli. Declining that request
            // is a behaviour the CLI itself exhibits — the spec has the frame silently
            // retained when the callee is more trusted, and CoreCLR's importer additionally
            // drops explicit tail calls whenever the caller is synchronized, is a reverse
            // P/Invoke, is varargs, the callee is native, or the return types aren't tailcall
            // compatible (see jit/importercalls.cpp `szCanTailCallFailReason`), emitting an
            // ordinary call instead.
            //
            // So PawPrint executes the prefix as a no-op: `tail.` is required to be followed
            // by a call whose result is immediately `ret`urned, so keeping the frame alive
            // for that one extra call changes nothing observable except (a) frame lifetime,
            // which matters only to code that illegally hands out byrefs into the dying
            // frame, and (b) the depth of the frame stack. Two divergences we accept for now:
            // a guest stack trace captured inside the callee names the caller that a real
            // tail call would have erased, and a program that relies on `tail.` for unbounded
            // recursion (FSC emits it for mutual recursion) grows PawPrint's heap-allocated
            // frame stack without bound rather than running in constant space. PawPrint
            // enforces no frame-count limit, so the guest sees no StackOverflowException.
            //
            // Nothing is recorded in `PendingPrefix`: a flag set here and read by nobody
            // would be a lie in the machine state, and the following call has no clearing
            // logic for it.
            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Conv_ovf_i_un ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            match convOvfIUn popped with
            | Ok conv ->
                // Crossing from byref-world to native-pointer-world: subsequent
                // pointer arithmetic must be byte-stride per ECMA-335 §III.1.5,
                // so anchor a `ReinterpretAs T` projection on plain array
                // byrefs. Plain byrefs (no anchor) keep element-stride
                // arithmetic to match `Unsafe.Add<T>`.
                let conv =
                    match conv with
                    | NativeIntSource.ManagedPointer ptr ->
                        ManagedPointerByteView.anchorByteViewIfPlainArrayByref corelib state ptr
                        |> NativeIntSource.ManagedPointer
                    | other -> other

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt conv) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.stepped
            | Error () ->
                // Exception dispatch uses the faulting instruction's PC, so do
                // not advance the program counter on this branch.
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    corelib
                    corelib.OverflowException
                    currentThread
                    state
                |> ExecutionResult.stepped
        | Conv_ovf_u_un -> failwith "TODO: Conv_ovf_u_un unimplemented"
        | Conv_ovf_i1_un -> failwith "TODO: Conv_ovf_i1_un unimplemented"
        | Conv_ovf_u1_un ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            match convOvfU1Un popped with
            | Ok conv ->
                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.Int32 (Int32Source.Verbatim (int32 conv)))
                    currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.stepped
            | Error () ->
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    corelib
                    corelib.OverflowException
                    currentThread
                    state
                |> ExecutionResult.stepped
        | Conv_ovf_i2_un -> failwith "TODO: Conv_ovf_i2_un unimplemented"
        | Conv_ovf_u2_un -> failwith "TODO: Conv_ovf_u2_un unimplemented"
        | Conv_ovf_i4_un ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            match convOvfI4Un popped with
            | Ok conv ->
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim conv)) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.stepped
            | Error () ->
                // Exception dispatch uses the faulting instruction's PC, so do
                // not advance the program counter on this branch.
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    corelib
                    corelib.OverflowException
                    currentThread
                    state
                |> ExecutionResult.stepped
        | Conv_ovf_u4_un -> failwith "TODO: Conv_ovf_u4_un unimplemented"
        | Conv_ovf_i8_un -> failwith "TODO: Conv_ovf_i8_un unimplemented"
        | Conv_ovf_u8_un -> failwith "TODO: Conv_ovf_u8_un unimplemented"
        | Conv_ovf_i ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            match convOvfI popped with
            | Ok conv ->
                // Crossing from byref-world to native-pointer-world: subsequent
                // pointer arithmetic must be byte-stride per ECMA-335 §III.1.5,
                // so anchor a `ReinterpretAs T` projection on plain array
                // byrefs. Plain byrefs (no anchor) keep element-stride
                // arithmetic to match `Unsafe.Add<T>`.
                let conv =
                    match conv with
                    | NativeIntSource.ManagedPointer ptr ->
                        ManagedPointerByteView.anchorByteViewIfPlainArrayByref corelib state ptr
                        |> NativeIntSource.ManagedPointer
                    | other -> other

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt conv) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.stepped
            | Error () ->
                // Exception dispatch uses the faulting instruction's PC, so do
                // not advance the program counter on this branch.
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    corelib
                    corelib.OverflowException
                    currentThread
                    state
                |> ExecutionResult.stepped
        | Conv_ovf_u ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            match convOvfU popped with
            | Ok conv ->
                // Crossing from byref-world to native-pointer-world: subsequent
                // pointer arithmetic must be byte-stride per ECMA-335 §III.1.5,
                // so anchor a `ReinterpretAs T` projection on plain array
                // byrefs. Plain byrefs (no anchor) keep element-stride
                // arithmetic to match `Unsafe.Add<T>`.
                let conv =
                    match conv with
                    | NativeIntSource.ManagedPointer ptr ->
                        ManagedPointerByteView.anchorByteViewIfPlainArrayByref corelib state ptr
                        |> NativeIntSource.ManagedPointer
                    | other -> other

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt conv) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.stepped
            | Error () ->
                // Exception dispatch uses the faulting instruction's PC, so do
                // not advance the program counter on this branch.
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    corelib
                    corelib.OverflowException
                    currentThread
                    state
                |> ExecutionResult.stepped
        | Neg ->
            let val1, state = IlMachineState.popEvalStack currentThread state
            let result, counters = negValue val1 state.PointerHashState

            let state =
                { state with
                    PointerHashState = counters
                }

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Not ->
            let val1, state = IlMachineState.popEvalStack currentThread state

            let result, state =
                match val1 with
                | EvalStackValue.Int32 int32Source ->
                    let i = Int32Source.value "Not" int32Source
                    ~~~i |> Int32Source.Verbatim |> EvalStackValue.Int32, state
                | EvalStackValue.Int64 i ->
                    let r, counters = Int64Source.bitNot "Not" i state.PointerHashState

                    EvalStackValue.Int64 r,
                    { state with
                        PointerHashState = counters
                    }
                | EvalStackValue.NativeInt src ->
                    // ECMA-335 III.3.35: `not` is defined on native ints. The BCL reaches
                    // this through the unrolled loops that round a count down to a multiple
                    // of a power of two, e.g. `numElements & ~(nuint)7` in SpanHelpers.Fill.
                    let r, counters = notNativeIntSource src state.PointerHashState

                    EvalStackValue.NativeInt r,
                    { state with
                        PointerHashState = counters
                    }
                | EvalStackValue.ManagedPointer _
                | EvalStackValue.NullObjectRef
                | EvalStackValue.ObjectRef _ -> failwith "refusing to negate a pointer"
                | EvalStackValue.Float f -> failwith $"Not is not defined on floating-point values; got %f{f}"
                | EvalStackValue.UserDefinedValueType vt -> failwith $"TODO: Not on a user-defined value type: %O{vt}"

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Ldind_ref ->
            let addr, state = IlMachineState.popEvalStack currentThread state

            match addr with
            | EvalStackValue.NullObjectRef
            | EvalStackValue.ManagedPointer ManagedPointerSource.Null
            | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null) ->
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    corelib
                    corelib.NullReferenceException
                    currentThread
                    state
                |> ExecutionResult.stepped
            | _ ->

            let referenced =
                match addr with
                | EvalStackValue.ManagedPointer src
                | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer src) ->
                    // ECMA-335 III.3.44 allows `ldind.ref`'s address operand to
                    // be `native int` as well as `&`: a byref widened by
                    // `conv.i`/`conv.u` (e.g. real CoreLib's
                    // `Task.FromResult<TResult>`, which reinterprets a cached
                    // `Task<bool>`/`Task<int>` as `Task<TResult>` via
                    // `ldloca.s; conv.u; ldind.ref`) is still logically the
                    // same address, so it must dereference identically to the
                    // `&`-typed spelling. `Stind_ref` immediately below
                    // treats both spellings identically for the same reason.
                    //
                    // Both spellings route through `readManagedByref` rather
                    // than the byte-view `readManagedByrefBytesAs` that the
                    // generic `ldind` uses for primitives: `readManagedByref`
                    // contains the correct byte-view-vs-structural dispatch
                    // for object references (including the `ReinterpretAs`
                    // zero-offset elision that the Task<T> pattern above
                    // exercises when a projection chain is present), and
                    // object references are not byte-addressable in
                    // PawPrint's value model (`CliType.OfBytesLike` has no
                    // case for `CliType.ObjectRef`). Pointers that are purely
                    // byte-addressable storage (stack-allocated or native
                    // memory with no typed cell at the target offset) have no
                    // way to hold a real object reference in this model
                    // either, and `readManagedByref` already fails loudly and
                    // specifically for that shape (e.g. "has no typed cell
                    // here; needs a byte-view byref shape") rather than
                    // routing through a byte reconstruction that would
                    // silently do the wrong thing.
                    IlMachineState.readManagedByref corelib state src
                // Release CoreLib's `GCHandle.InternalGet` is literally
                // `*(object*)handle` (GCHandle.CoreCLR.cs), so this is the
                // dereference of the handle table slot.
                | EvalStackValue.NativeInt (NativeIntSource.GcHandlePtr (handle, 0L)) ->
                    GcHandleRegistry.target handle state.GcHandles |> CliType.ObjectRef
                | EvalStackValue.NativeInt (NativeIntSource.GcHandlePtr (handle, tag)) ->
                    // Managed code always strips its tag bits before dereferencing
                    // (`WeakReference.Target` does `_taggedHandle & ~TracksResurrectionBit`
                    // first). Dereferencing a tagged handle would be a misaligned
                    // read in reality, so it must not quietly succeed here.
                    failwith
                        $"Ldind_ref: refusing to dereference GC handle %O{handle} while it carries tag bits 0x%x{tag}; managed code must mask them off first"
                | EvalStackValue.NullObjectRef -> failwith "unreachable: NullObjectRef handled above"
                | a -> failwith $"TODO: Ldind_ref on unsupported eval stack value {a}"

            let state =
                match referenced with
                | CliType.RuntimePointer (CliRuntimePointer.Managed _)
                | CliType.ObjectRef _ -> IlMachineState.pushToEvalStack referenced currentThread state
                | _ -> failwith $"Unexpected non-reference {referenced}"
                |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Stind_ref ->
            let value, state = IlMachineState.popEvalStack currentThread state
            let addr, state = IlMachineState.popEvalStack currentThread state

            match addr with
            | EvalStackValue.NullObjectRef
            | EvalStackValue.ManagedPointer ManagedPointerSource.Null
            | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null) ->
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    corelib
                    corelib.NullReferenceException
                    currentThread
                    state
                |> ExecutionResult.stepped
            | _ ->

            let state =
                match addr with
                | EvalStackValue.ManagedPointer src
                | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer src) ->
                    IlMachineState.writeManagedByrefWithBase
                        corelib
                        state
                        src
                        (EvalStackValue.toCliTypeCoerced (CliType.ObjectRef None) value)
                | EvalStackValue.NullObjectRef -> failwith "unreachable: NullObjectRef handled above"
                | addr -> failwith $"TODO: {addr}"

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Ldelem_i ->
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            ldElem
                (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)))
                index
                arr
                currentThread
                state
        | Ldelem_i1 ->
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            ldElem (CliType.Numeric (CliNumericType.Int8 0y)) index arr currentThread state
        | Ldelem_u1 ->
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            ldElem (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy))) index arr currentThread state
        | Ldelem_i2 ->
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            ldElem (CliType.Numeric (CliNumericType.Int16 0s)) index arr currentThread state
        | Ldelem_u2 ->
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            ldElem (CliType.Numeric (CliNumericType.UInt16 0us)) index arr currentThread state
        | Ldelem_i4 ->
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            ldElem (CliType.Numeric (CliNumericType.Int32 0)) index arr currentThread state
        | Ldelem_u4 ->
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            // unsigned int32 is not a distinct CliType; the spec stores it on the stack as if
            // signed, with two's complement wraparound. Matches `getTargetLdindCliType`'s LdindU4.
            ldElem (CliType.Numeric (CliNumericType.Int32 0)) index arr currentThread state
        | Ldelem_i8 ->
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            ldElem (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))) index arr currentThread state
        | Ldelem_u8 -> failwith "TODO: Ldelem_u8 unimplemented"
        | Ldelem_r4 ->
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            ldElem (CliType.Numeric (CliNumericType.Float32 0.0f)) index arr currentThread state
        | Ldelem_r8 ->
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            ldElem (CliType.Numeric (CliNumericType.Float64 0.0)) index arr currentThread state
        | Ldelem_ref ->
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            let value = getArrayElt index arr currentThread state

            match value with
            | CliType.ObjectRef _
            | CliType.RuntimePointer _ -> ()
            | _ -> failwith "expected object reference in Ldelem.ref"

            let state =
                state
                |> IlMachineState.pushToEvalStack value currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            ExecutionResult.stepped (state, WhatWeDid.Executed)
        | Stelem_i ->
            let value, state = IlMachineState.popEvalStack currentThread state
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            stElem loggerFactory corelib value index arr currentThread state
        | Stelem_i1 ->
            let value, state = IlMachineState.popEvalStack currentThread state
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            stElem loggerFactory corelib value index arr currentThread state
        | Stelem_u1 -> failwith "TODO: Stelem_u1 unimplemented"
        | Stelem_i2 ->
            let value, state = IlMachineState.popEvalStack currentThread state
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            stElem loggerFactory corelib value index arr currentThread state
        | Stelem_u2 -> failwith "TODO: Stelem_u2 unimplemented"
        | Stelem_i4 ->
            let value, state = IlMachineState.popEvalStack currentThread state
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            stElem loggerFactory corelib value index arr currentThread state
        | Stelem_u4 -> failwith "TODO: Stelem_u4 unimplemented"
        | Stelem_i8 ->
            let value, state = IlMachineState.popEvalStack currentThread state
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            stElem loggerFactory corelib value index arr currentThread state
        | Stelem_u8 -> failwith "TODO: Stelem_u8 unimplemented"
        | Stelem_r4 ->
            let value, state = IlMachineState.popEvalStack currentThread state
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            stElem loggerFactory corelib value index arr currentThread state
        | Stelem_r8 ->
            let value, state = IlMachineState.popEvalStack currentThread state
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            stElem loggerFactory corelib value index arr currentThread state
        | Stelem_ref ->
            let value, state = IlMachineState.popEvalStack currentThread state
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            stElem loggerFactory corelib value index arr currentThread state
        | Cpblk -> failwith "TODO: Cpblk unimplemented"
        | Initblk -> failwith "TODO: Initblk unimplemented"
        | Conv_ovf_u1 ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            match convOvfU1 popped with
            | Ok conv ->
                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.Int32 (Int32Source.Verbatim (int32 conv)))
                    currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.stepped
            | Error () ->
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    corelib
                    corelib.OverflowException
                    currentThread
                    state
                |> ExecutionResult.stepped
        | Conv_ovf_u2 ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            match convOvfU2 popped with
            | Ok conv ->
                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.Int32 (Int32Source.Verbatim (int32 conv)))
                    currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.stepped
            | Error () ->
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    corelib
                    corelib.OverflowException
                    currentThread
                    state
                |> ExecutionResult.stepped
        | Conv_ovf_u4 ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            match convOvfU4 popped with
            | Ok conv ->
                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.Int32 (Int32Source.Verbatim (int32 conv)))
                    currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.stepped
            | Error () ->
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    corelib
                    corelib.OverflowException
                    currentThread
                    state
                |> ExecutionResult.stepped
        | Conv_ovf_u8 -> failwith "TODO: Conv_ovf_u8 unimplemented"
        | Conv_ovf_i1 ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            match convOvfI1 popped with
            | Ok conv ->
                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.Int32 (Int32Source.Verbatim (int32 conv)))
                    currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.stepped
            | Error () ->
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    corelib
                    corelib.OverflowException
                    currentThread
                    state
                |> ExecutionResult.stepped
        | Conv_ovf_i2 -> failwith "TODO: Conv_ovf_i2 unimplemented"
        | Conv_ovf_i4 ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            match convOvfI4 popped with
            | Ok conv ->
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim conv)) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.stepped
            | Error () ->
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    corelib
                    corelib.OverflowException
                    currentThread
                    state
                |> ExecutionResult.stepped
        | Conv_ovf_i8 -> failwith "TODO: Conv_ovf_i8 unimplemented"
        | Break -> failwith "TODO: Break unimplemented"
        | Conv_r_un ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let conv = EvalStackValue.convUnsignedToFloat popped

            let state =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Float conv) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Arglist -> failwith "TODO: Arglist unimplemented"
        | Ckfinite -> failwith "TODO: Ckfinite unimplemented"
        | Readonly ->
            // ECMA-335 III.2.2: `readonly.` precedes `ldelema`. The resulting controlled-
            // mutability managed pointer must not be used to write through, nor to call
            // a method taking a writable `this`. The observable runtime effect is to
            // suppress the array covariance check (ArrayTypeMismatchException) on the
            // following ldelema. We record the prefix here; ldelema consumes it.
            let activeFrameId = state.ThreadState.[currentThread].ActiveMethodState

            state
            |> IlMachineState.mapFrame
                currentThread
                activeFrameId
                (fun frame ->
                    { frame with
                        PendingPrefix =
                            { frame.PendingPrefix with
                                Readonly = true
                            }
                    }
                )
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Refanytype -> failwith "TODO: Refanytype unimplemented"
