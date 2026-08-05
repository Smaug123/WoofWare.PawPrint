namespace WoofWare.PawPrint

open System
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
                let arrObj = state.ManagedHeap.Arrays.[arr]

                let elementSize =
                    if arrObj.Length = 0 then
                        // Array.Empty<T>() has no representative element from
                        // which to read the byte stride. The only stable address
                        // we can derive without the element type is index zero.
                        if index = 0 then Some 0 else None
                    else
                        CliType.sizeOf arrObj.Elements.[0] |> Some

                match elementSize, projectionByteOffset projs with
                | Some elementSize, Some byteOffset -> Some (int64<int> index * int64<int> elementSize + byteOffset)
                | _ -> None
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

    let private typeHandleLowAddressBits (target : RuntimeTypeHandleTarget) : int64 =
        match target with
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ -> 0L
        // Generic parameters in CoreCLR are TypeVarTypeDesc, a TypeDesc subclass, so the
        // tagged-pointer encoding sets the second-lowest bit. Reflection paths such as
        // `RuntimeType.get_IsInterface` rely on `TypeHandle.IsTypeDesc` to short-circuit
        // before dereferencing a non-existent MethodTable; honour that contract.
        | RuntimeTypeHandleTarget.GenericParameter _
        | RuntimeTypeHandleTarget.MethodGenericParameter _ -> 2L
        | RuntimeTypeHandleTarget.Closed typeHandle ->
            match typeHandle with
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ ->
                // CoreCLR tags TypeDesc handles by setting the second-lowest bit.
                // PawPrint has no real address, but matching that low-bit contract
                // lets managed CoreLib code run `TypeHandle.IsTypeDesc`.
                2L
            | ConcreteTypeHandle.Concrete _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ -> 0L

    let private andNativeIntAddressBits
        (state : IlMachineState)
        (source : NativeIntSource)
        (mask : int64)
        : EvalStackValue
        =
        match source with
        | NativeIntSource.Verbatim bits -> NativeIntSource.Verbatim (bits &&& mask) |> EvalStackValue.NativeInt
        | NativeIntSource.ManagedPointer ptr -> andManagedPointerAddressBits state ptr mask
        | NativeIntSource.TypeHandlePtr target ->
            NativeIntSource.Verbatim (typeHandleLowAddressBits target &&& mask)
            |> EvalStackValue.NativeInt
        | NativeIntSource.MethodTablePtr _ -> NativeIntSource.Verbatim (0L &&& mask) |> EvalStackValue.NativeInt
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
        (counters : PointerHashCounters)
        : NativeIntSource * PointerHashCounters
        =
        match i1, i2 with
        | NativeIntSource.Verbatim a, NativeIntSource.Verbatim b -> NativeIntSource.Verbatim (a ^^^ b), counters
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
    /// composing with the verbatim arms of `And` / `Or` / comparisons. (The
    /// result is an integer, not a pointer, so the placeholder-to-`Null`
    /// normalisation that applies when *constructing* byrefs is not needed
    /// here.) Everything else routes
    /// through `PointerHashSynthesis.materialiseHashBits` and is tagged
    /// `OpaqueHashBits`, propagating the synthesised-bits contract: the result is
    /// deterministic but MUST NOT be used as a real pointer. `materialiseHashBits`
    /// fails loudly on any other `ManagedPointer` and on
    /// `SyntheticCrossArrayOffset`, preserving byref / cross-storage provenance.
    ///
    /// Note that a handle-shaped source does not survive a double complement as
    /// itself: `~~handle` comes back as `OpaqueHashBits`, so comparing that against
    /// the original handle hits `equalsForCli`'s "synthesised hash bits vs handle
    /// pointer" refusal (CEQ has no `PointerHashCounters` with which to materialise
    /// the handle). That is a property of the hash-synthesis design rather than of
    /// `not`: `xorNativeIntSources` loses handle provenance the same way, so
    /// `(handle ^ 0) ^ 0 == handle` already fails identically. The failure is loud,
    /// and the complemented bits themselves are correct and deterministic.
    let private notNativeIntSource
        (source : NativeIntSource)
        (counters : PointerHashCounters)
        : NativeIntSource * PointerHashCounters
        =
        match source with
        | NativeIntSource.Verbatim i -> NativeIntSource.Verbatim ~~~i, counters
        | NativeIntSource.ManagedPointer ManagedPointerSource.Null -> NativeIntSource.Verbatim ~~~0L, counters
        | NativeIntSource.ManagedPointer (ManagedPointerSource.NativeIntPlaceholder bits) ->
            NativeIntSource.Verbatim ~~~bits, counters
        | _ ->
            let bits, counters = PointerHashSynthesis.materialiseHashBits "Not" source counters
            NativeIntSource.OpaqueHashBits ~~~bits, counters

    let private locallocSizeBytes (value : EvalStackValue) : int =
        let size =
            match value with
            | EvalStackValue.Int32 i -> int64 i
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

    let private checkDivUnZero (operation : string) (isZero : bool) : unit =
        if isZero then
            failwith $"TODO: throw DivideByZeroException for %s{operation} by zero"

    let internal divUnValues (v1 : EvalStackValue) (v2 : EvalStackValue) : EvalStackValue =
        match v1, v2 with
        | EvalStackValue.Int32 v1, EvalStackValue.Int32 v2 ->
            checkDivUnZero "Div_un" (v2 = 0)
            (uint32<int32> v1 / uint32<int32> v2) |> int32<uint32> |> EvalStackValue.Int32
        | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 ->
            checkDivUnZero "Div_un" (Int64Source.isZero v2)

            match v1, v2 with
            | Int64Source.Verbatim v1, Int64Source.Verbatim v2 ->
                (uint64<int64> v1 / uint64<int64> v2)
                |> int64<uint64>
                |> Int64Source.Verbatim
                |> EvalStackValue.Int64
            | _, _ -> failwith "TODO"
        | EvalStackValue.Int32 v1, EvalStackValue.NativeInt (NativeIntSource.Verbatim v2) ->
            checkDivUnZero "Div_un" (v2 = 0L)

            (uint64 (uint32<int32> v1) / uint64<int64> v2)
            |> int64<uint64>
            |> NativeIntSource.Verbatim
            |> EvalStackValue.NativeInt
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim v1), EvalStackValue.Int32 v2 ->
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

    let private negInt32Unchecked (value : int32) : int32 =
        0u - uint32<int32> value |> int32<uint32>

    let private negInt64Unchecked (value : int64) : int64 =
        0UL - uint64<int64> value |> int64<uint64>

    let private negValue
        (value : EvalStackValue)
        (counters : PointerHashCounters)
        : EvalStackValue * PointerHashCounters
        =
        match value with
        | EvalStackValue.Int32 value -> negInt32Unchecked value |> EvalStackValue.Int32, counters
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
            | NativeIntSource.GcHandlePtr handle -> failwith $"Neg: refusing to negate GC handle pointer %O{handle}"
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
        | EvalStackValue.Int32 i -> if i < 0 then Error () else Ok i
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
        | EvalStackValue.Int32 i -> Ok i
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
        | EvalStackValue.Int32 i -> if i < 0 then Error () else uint32 i |> Ok
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
        | EvalStackValue.Int32 i -> fromSignedInt32 i
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
        | EvalStackValue.Int32 i -> fromSignedInt32 i
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
        | EvalStackValue.Int32 i -> fromUnsignedInt32 i
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
        | EvalStackValue.Int32 i ->
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
            | NativeIntSource.MethodTablePtr typeHandle ->
                failwith $"Conv_ovf_u: refusing to convert MethodTable pointer %O{typeHandle} to unsigned native int"
            | NativeIntSource.MethodTableAuxiliaryDataPtr typeHandle ->
                failwith
                    $"Conv_ovf_u: refusing to convert MethodTableAuxiliaryData pointer %O{typeHandle} to unsigned native int"
            | NativeIntSource.PerInstInfoPtr handle ->
                failwith $"Conv_ovf_u: refusing to convert PerInstInfo pointer %O{handle} to unsigned native int"
            | NativeIntSource.PerInstDictPtr handle ->
                failwith $"Conv_ovf_u: refusing to convert PerInstDict pointer %O{handle} to unsigned native int"
            | NativeIntSource.GcHandlePtr handle ->
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

    // Helper to get the target CliType for each Ldind variant
    let private getTargetLdindCliType (targetType : LdindTargetType) : CliType =
        match targetType with
        | LdindI -> CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
        | LdindI1 -> CliType.Numeric (CliNumericType.Int8 0y)
        | LdindI2 -> CliType.Numeric (CliNumericType.Int16 0s)
        | LdindI4 -> CliType.Numeric (CliNumericType.Int32 0)
        | LdindI8 -> CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))
        | LdindU1 -> CliType.Numeric (CliNumericType.UInt8 0uy)
        | LdindU2 -> CliType.Numeric (CliNumericType.UInt16 0us)
        | LdindU4 ->
            // This doesn't actually exist as a CLI type
            CliType.Numeric (CliNumericType.Int32 0)
        | LdindU8 ->
            // This doesn't actually exist as a CLI type
            CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))
        | LdindR4 -> CliType.Numeric (CliNumericType.Float32 0.0f)
        | LdindR8 -> CliType.Numeric (CliNumericType.Float64 0.0)

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
            | EvalStackValue.ManagedPointer src -> IlMachineState.readManagedByref corelib state src
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
            | EvalStackValue.Int32 i -> i
            | _ -> failwith $"Invalid index: {index}"

        let arrAddr =
            match arr with
            | EvalStackValue.ObjectRef addr -> addr
            | EvalStackValue.NullObjectRef -> failwith "TODO: throw NRE"
            | _ -> failwith $"Invalid array: %O{arr}"

        IlMachineState.getArrayValue arrAddr index state

    let internal endfilterAccepts (filterResult : EvalStackValue) : bool =
        match filterResult with
        | EvalStackValue.Int32 0 -> false
        | EvalStackValue.Int32 _ -> true
        | value -> failwith $"Endfilter requires an int32 result on the stack; got %O{value}"

    let internal stElem
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
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
                | NativeIntSource.MethodHandlePtr _
                | NativeIntSource.TypeHandlePtr _
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
            | EvalStackValue.Int32 i -> i
            | _ -> failwith $"Invalid index: {index}"

        let arrAddr =
            match arr with
            | EvalStackValue.ObjectRef addr -> addr
            | EvalStackValue.NullObjectRef -> failwith "TODO: throw NRE"
            | _ -> failwith $"Invalid array: %O{arr}"

        let arr = state.ManagedHeap.Arrays.[arrAddr]

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
                | ExceptionDispatchResult.HandlerFound state -> (state, WhatWeDid.Executed) |> ExecutionResult.stepped
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

            let comparisonResult = if EvalStackValueComparisons.ceq var1 var2 then 1 else 0

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 comparisonResult) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Cgt ->
            let var2, state = state |> IlMachineState.popEvalStack currentThread
            let var1, state = state |> IlMachineState.popEvalStack currentThread

            let comparisonResult = if EvalStackValueComparisons.cgt var1 var2 then 1 else 0

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 comparisonResult) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Cgt_un ->
            let var2, state = state |> IlMachineState.popEvalStack currentThread
            let var1, state = state |> IlMachineState.popEvalStack currentThread

            let comparisonResult = if EvalStackValueComparisons.cgtUn var1 var2 then 1 else 0

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 comparisonResult) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Clt ->
            let var2, state = state |> IlMachineState.popEvalStack currentThread
            let var1, state = state |> IlMachineState.popEvalStack currentThread

            let comparisonResult = if EvalStackValueComparisons.clt var1 var2 then 1 else 0

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 comparisonResult) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Clt_un ->
            let var2, state = state |> IlMachineState.popEvalStack currentThread
            let var1, state = state |> IlMachineState.popEvalStack currentThread

            let comparisonResult = if EvalStackValueComparisons.cltUn var1 var2 then 1 else 0

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 comparisonResult) currentThread
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

            match
                try
                    BinaryArithmetic.execute corelib ArithmeticOperation.div state val1 val2 |> Ok
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
        | Div_un ->
            let v2, state = IlMachineState.popEvalStack currentThread state
            let v1, state = IlMachineState.popEvalStack currentThread state

            let result = divUnValues v1 v2

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Shr ->
            let shift, state = IlMachineState.popEvalStack currentThread state
            let number, state = IlMachineState.popEvalStack currentThread state

            let shift =
                match shift with
                | EvalStackValue.Int32 i -> i
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) -> int<int64> i
                | _ -> failwith $"Not allowed shift of {shift}"

            let result, state =
                // See table III.6
                match number with
                | EvalStackValue.Int32 i -> i >>> shift |> EvalStackValue.Int32, state
                | EvalStackValue.Int64 i ->
                    let r, counters = Int64Source.shr "Shr" i shift state.PointerHashCounters

                    EvalStackValue.Int64 r,
                    { state with
                        PointerHashCounters = counters
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
                | EvalStackValue.Int32 i -> i
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) -> int<int64> i
                | _ -> failwith $"Not allowed shift of {shift}"

            let result, state =
                // See table III.6
                match number with
                | EvalStackValue.Int32 i -> uint32<int> i >>> shift |> int32<uint32> |> EvalStackValue.Int32, state
                | EvalStackValue.Int64 i ->
                    let r, counters = Int64Source.shrUn "Shr_un" i shift state.PointerHashCounters

                    EvalStackValue.Int64 r,
                    { state with
                        PointerHashCounters = counters
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
                | EvalStackValue.Int32 i -> i
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) -> int<int64> i
                | _ -> failwith $"Not allowed shift of {shift}"

            let result, state =
                // See table III.6
                match number with
                | EvalStackValue.Int32 i -> i <<< shift |> EvalStackValue.Int32, state
                | EvalStackValue.Int64 i ->
                    let r, counters = Int64Source.shl "Shl" i shift state.PointerHashCounters

                    EvalStackValue.Int64 r,
                    { state with
                        PointerHashCounters = counters
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
                | EvalStackValue.Int32 v1, EvalStackValue.Int32 v2 -> v1 &&& v2 |> EvalStackValue.Int32, state
                | EvalStackValue.Int32 mask, EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr) ->
                    int64<int32> mask |> andManagedPointerAddressBits state ptr, state
                | EvalStackValue.Int32 v1, EvalStackValue.NativeInt (NativeIntSource.Verbatim v2) ->
                    int64<int32> v1 &&& v2 |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt, state
                | EvalStackValue.Int32 mask, EvalStackValue.NativeInt src ->
                    andNativeIntAddressBits state src (int64<int32> mask), state
                | EvalStackValue.Int32 mask, EvalStackValue.ManagedPointer ptr ->
                    int64<int32> mask |> andManagedPointerAddressBits state ptr, state
                | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 ->
                    let r, counters = Int64Source.bitAnd "And" v1 v2 state.PointerHashCounters

                    EvalStackValue.Int64 r,
                    { state with
                        PointerHashCounters = counters
                    }
                | EvalStackValue.Int64 mask, EvalStackValue.ManagedPointer ptr -> failwith "TODO"
                // andManagedPointerAddressBits state ptr mask
                | EvalStackValue.Int64 mask, EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr) ->
                    // andManagedPointerAddressBits state ptr mask
                    failwith "TODO"
                | EvalStackValue.ManagedPointer ptr, EvalStackValue.Int32 mask ->
                    int64<int32> mask |> andManagedPointerAddressBits state ptr, state
                | EvalStackValue.ManagedPointer ptr, EvalStackValue.Int64 mask ->
                    // andManagedPointerAddressBits state ptr mask
                    failwith "TODO"
                | EvalStackValue.ManagedPointer ptr, EvalStackValue.NativeInt (NativeIntSource.Verbatim mask)
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim mask), EvalStackValue.ManagedPointer ptr ->
                    andManagedPointerAddressBits state ptr mask, state
                | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr), EvalStackValue.Int32 mask ->
                    int64<int32> mask |> andManagedPointerAddressBits state ptr, state
                | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr), EvalStackValue.Int64 mask ->
                    // andManagedPointerAddressBits state ptr mask
                    failwith "TODO"
                | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr),
                  EvalStackValue.NativeInt (NativeIntSource.Verbatim mask)
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim mask),
                  EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr) ->
                    andManagedPointerAddressBits state ptr mask, state
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim v1), EvalStackValue.Int32 v2 ->
                    v1 &&& int64<int32> v2 |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt, state
                | EvalStackValue.NativeInt src, EvalStackValue.Int32 mask ->
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
                | EvalStackValue.Int32 v1, EvalStackValue.Int32 v2 -> v1 ||| v2 |> EvalStackValue.Int32, state
                | EvalStackValue.Int32 v1, EvalStackValue.NativeInt (NativeIntSource.Verbatim v2) ->
                    int64<int32> v1 ||| v2 |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt, state
                | EvalStackValue.Int32 _, EvalStackValue.NativeInt _ ->
                    failwith $"can't do binary operation on non-verbatim native int {v2}"
                | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 ->
                    let r, counters = Int64Source.bitOr "Or" v1 v2 state.PointerHashCounters

                    EvalStackValue.Int64 r,
                    { state with
                        PointerHashCounters = counters
                    }
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim v1), EvalStackValue.Int32 v2 ->
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
                | EvalStackValue.Int32 v1, EvalStackValue.Int32 v2 -> v1 ^^^ v2 |> EvalStackValue.Int32, state
                | EvalStackValue.Int32 v1, EvalStackValue.NativeInt (NativeIntSource.Verbatim v2) ->
                    int64<int32> v1 ^^^ v2 |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt, state
                | EvalStackValue.Int32 _, EvalStackValue.NativeInt _ ->
                    failwith $"can't do binary operation on non-verbatim native int {v2}"
                | EvalStackValue.Int64 v1, EvalStackValue.Int64 v2 ->
                    let r, counters = Int64Source.bitXor "Xor" v1 v2 state.PointerHashCounters

                    EvalStackValue.Int64 r,
                    { state with
                        PointerHashCounters = counters
                    }
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim v1), EvalStackValue.Int32 v2 ->
                    v1 ^^^ int64<int32> v2 |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt, state
                | EvalStackValue.NativeInt _, EvalStackValue.Int32 _ ->
                    failwith $"can't do binary operation on non-verbatim native int {v1}"
                | EvalStackValue.NativeInt src1, EvalStackValue.NativeInt src2 ->
                    let r, counters = xorNativeIntSources src1 src2 state.PointerHashCounters

                    EvalStackValue.NativeInt r,
                    { state with
                        PointerHashCounters = counters
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

            let state =
                match converted with
                | None -> failwith "TODO: Conv_I conversion failure unimplemented"
                | Some conv ->
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

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Conv_I1 ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let converted = EvalStackValue.convToInt8 popped

            let state =
                match converted with
                | None -> failwith "TODO: Conv_I1 conversion failure unimplemented"
                | Some conv ->
                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 conv) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Conv_I2 ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let converted = EvalStackValue.convToInt16 popped

            let state =
                match converted with
                | None -> failwith "TODO: Conv_I2 conversion failure unimplemented"
                | Some conv ->
                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 conv) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
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

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
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

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Conv_R4 ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let converted = EvalStackValue.convToFloat32 popped

            let state =
                match converted with
                | None -> failwith "TODO: Conv_R4 conversion failure unimplemented"
                | Some conv ->
                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.Float conv) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Conv_R8 ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let converted = EvalStackValue.convToFloat64 popped

            let state =
                match converted with
                | None -> failwith "TODO: Conv_R8 conversion failure unimplemented"
                | Some conv ->
                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.Float conv) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Conv_U ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let converted = EvalStackValue.toUnsignedNativeInt popped

            let state =
                match converted with
                | None -> failwith "TODO: Conv_U conversion failure unimplemented"
                | Some conv ->
                    // NativeIntSource.Verbatim backs the native-int stack slot with
                    // a signed int64, but the bits are what matter: signed and
                    // unsigned native-int comparisons reinterpret the slot as
                    // needed. `int64 (conv : uint64)` is a bit-exact reinterpret
                    // cast in F#, which is what ECMA-335 requires here (truncate
                    // high-order bits beyond the native word size).
                    let conv =
                        match conv with
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

                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt conv) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Conv_U1 ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let converted = EvalStackValue.convToUInt8 popped

            let state =
                match converted with
                | None -> failwith "TODO: Conv_U1 conversion failure unimplemented"
                | Some conv ->
                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 conv) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Conv_U2 ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let converted = EvalStackValue.convToUInt16 popped

            let state =
                match converted with
                | None -> failwith "TODO: Conv_U2 conversion failure unimplemented"
                | Some conv ->
                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 conv) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | Conv_U4 ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let converted = EvalStackValue.convToUInt32 popped

            let state =
                match converted with
                | None -> failwith "TODO: Conv_U4 conversion failure unimplemented"
                | Some conv ->
                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 conv) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
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

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped
        | LdLen ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            let popped =
                match popped with
                | EvalStackValue.NullObjectRef -> failwith "TODO: throw NRE"
                | EvalStackValue.ObjectRef addr -> addr
                | _ -> failwith $"can't get len of {popped}"

            let popped = state.ManagedHeap.Arrays.[popped]

            IlMachineState.pushToEvalStack' (EvalStackValue.Int32 popped.Length) currentThread state
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

                if filterAccepted then
                    let threadState =
                        ThreadState.setFrame threadState.ActiveMethodState methodStateWithoutFilter threadState

                    let state =
                        { state with
                            ThreadState = state.ThreadState |> Map.add currentThread threadState
                        }

                    ExceptionDispatching.enterCatchHandler
                        currentThread
                        methodStateWithoutFilter
                        threadState
                        state
                        continuation.CurrentFilter.HandlerOffset
                        continuation.CliException
                    |> Tuple.withRight WhatWeDid.Executed
                    |> ExecutionResult.stepped
                else
                    let newMethodState = methodStateWithoutFilter |> MethodState.clearEvalStack

                    let newThreadState =
                        ThreadState.setFrame threadState.ActiveMethodState newMethodState threadState

                    let state =
                        { state with
                            ThreadState = state.ThreadState |> Map.add currentThread newThreadState
                        }

                    let exceptionType =
                        ExceptionDispatching.exceptionObjectType state continuation.CliException.ExceptionObject

                    let skippedFilters = continuation.CurrentFilter :: continuation.SkippedFilters

                    match
                        ExceptionDispatching.dispatchExceptionFromSearchPC
                            loggerFactory
                            corelib
                            state
                            currentThread
                            continuation.CliException
                            exceptionType
                            continuation.SearchPC
                            skippedFilters
                    with
                    | ExceptionDispatchResult.HandlerFound state ->
                        (state, WhatWeDid.Executed) |> ExecutionResult.stepped
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
                       Scope = ExceptionContinuationScope.FinallyHandler _
                       Continuation = ExceptionContinuation.ResumeAfterFinally targetPC
                   },
              methodStateWithoutContinuation ->
                // Resume at the leave target
                let newMethodState =
                    methodStateWithoutContinuation |> MethodState.setProgramCounter targetPC

                let newThreadState =
                    ThreadState.setFrame threadState.ActiveMethodState newMethodState threadState

                { state with
                    ThreadState = state.ThreadState |> Map.add currentThread newThreadState
                }
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.stepped
            | Some {
                       Scope = scope
                       Continuation = ExceptionContinuation.PropagatingException exn
                   },
              methodStateWithoutContinuation when endsWithEndfinally scope ->
                // Continue exception propagation after finally block.
                // Get exception type from heap object.
                let heapObject =
                    match state.ManagedHeap.NonArrayObjects |> Map.tryFind exn.ExceptionObject with
                    | Some obj -> obj
                    | None -> failwith "Exception object not found in heap during endfinally propagation"

                let threadState =
                    ThreadState.setFrame threadState.ActiveMethodState methodStateWithoutContinuation threadState

                let state =
                    { state with
                        ThreadState = state.ThreadState |> Map.add currentThread threadState
                    }

                match
                    ExceptionDispatching.dispatchException
                        loggerFactory
                        corelib
                        state
                        currentThread
                        exn
                        heapObject.ConcreteType
                with
                | ExceptionDispatchResult.HandlerFound state -> (state, WhatWeDid.Executed) |> ExecutionResult.stepped
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

                // TODO: when stack traces are formatted, record the rethrow site as a boundary
                // so rendered traces can distinguish it from the original throw frame.
                match
                    ExceptionDispatching.dispatchException
                        loggerFactory
                        corelib
                        state
                        currentThread
                        cliException
                        exceptionType
                with
                | ExceptionDispatchResult.HandlerFound state -> (state, WhatWeDid.Executed) |> ExecutionResult.stepped
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
                match state.ManagedHeap.NonArrayObjects |> Map.tryFind addr with
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
            | ExceptionDispatchResult.HandlerFound state -> (state, WhatWeDid.Executed) |> ExecutionResult.stepped
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

            let result, state =
                BinaryArithmetic.execute corelib ArithmeticOperation.rem state val1 val2

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
        | Rem_un ->
            let val2, state = IlMachineState.popEvalStack currentThread state
            let val1, state = IlMachineState.popEvalStack currentThread state

            let result, state =
                BinaryArithmetic.execute corelib ArithmeticOperation.remUn state val1 val2

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
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
        | Conv_ovf_i_un -> failwith "TODO: Conv_ovf_i_un unimplemented"
        | Conv_ovf_u_un -> failwith "TODO: Conv_ovf_u_un unimplemented"
        | Conv_ovf_i1_un -> failwith "TODO: Conv_ovf_i1_un unimplemented"
        | Conv_ovf_u1_un ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            match convOvfU1Un popped with
            | Ok conv ->
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (int32 conv)) currentThread
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
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 conv) currentThread
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
        | Conv_ovf_i -> failwith "TODO: Conv_ovf_i unimplemented"
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
            let result, counters = negValue val1 state.PointerHashCounters

            let state =
                { state with
                    PointerHashCounters = counters
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
                | EvalStackValue.Int32 i -> ~~~i |> EvalStackValue.Int32, state
                | EvalStackValue.Int64 i ->
                    let r, counters = Int64Source.bitNot "Not" i state.PointerHashCounters

                    EvalStackValue.Int64 r,
                    { state with
                        PointerHashCounters = counters
                    }
                | EvalStackValue.NativeInt src ->
                    // ECMA-335 III.3.35: `not` is defined on native ints. The BCL reaches
                    // this through the unrolled loops that round a count down to a multiple
                    // of a power of two, e.g. `numElements & ~(nuint)7` in SpanHelpers.Fill.
                    let r, counters = notNativeIntSource src state.PointerHashCounters

                    EvalStackValue.NativeInt r,
                    { state with
                        PointerHashCounters = counters
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
                    // already treats both spellings identically for exactly
                    // this reason; this mirrors it.
                    //
                    // Both spellings route through `readManagedByref` rather
                    // than the byte-view `readManagedByrefBytesAs` that the
                    // generic `ldind` uses for primitives. That split is
                    // deliberate, not an oversight: `readManagedByref` already
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
                | EvalStackValue.NativeInt (NativeIntSource.GcHandlePtr handle) ->
                    GcHandleRegistry.target handle state.GcHandles |> CliType.ObjectRef
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

            let value = getArrayElt index arr currentThread state

            match value with
            | CliType.Numeric (CliNumericType.NativeInt _) -> ()
            | _ -> failwith "expected native int in Ldelem.i"

            let state =
                state
                |> IlMachineState.pushToEvalStack value currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            ExecutionResult.stepped (state, WhatWeDid.Executed)
        | Ldelem_i1 ->
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            let value = getArrayElt index arr currentThread state

            let value =
                match value with
                | CliType.Numeric (CliNumericType.Int8 i) -> int i
                | CliType.Numeric (CliNumericType.UInt8 i) -> int (sbyte i)
                | _ -> failwith $"expected one-byte integer in Ldelem.i1, got: %O{value}"

            let state =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 value) currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            ExecutionResult.stepped (state, WhatWeDid.Executed)
        | Ldelem_u1 ->
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            let value = getArrayElt index arr currentThread state

            let value =
                match value with
                | CliType.Numeric (CliNumericType.UInt8 i) -> int i
                | CliType.Numeric (CliNumericType.Int8 i) -> int (byte (int i &&& 0xFF))
                | _ -> failwith $"expected one-byte integer in Ldelem.u1, got: %O{value}"

            let state =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 value) currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            ExecutionResult.stepped (state, WhatWeDid.Executed)
        | Ldelem_i2 ->
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            let value = getArrayElt index arr currentThread state

            let value =
                match value with
                | CliType.Numeric (CliNumericType.Int16 i) -> int i
                | CliType.Numeric (CliNumericType.UInt16 i) -> int (int16 i)
                | _ -> failwith $"expected two-byte integer in Ldelem.i2, got: %O{value}"

            let state =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 value) currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            ExecutionResult.stepped (state, WhatWeDid.Executed)
        | Ldelem_u2 ->
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            let value = getArrayElt index arr currentThread state

            let value =
                match value with
                | CliType.Numeric (CliNumericType.UInt16 i) -> int i
                | CliType.Numeric (CliNumericType.Int16 i) -> int (uint16 i)
                | CliType.Char (high, low) -> (int high <<< 8) ||| int low
                | _ -> failwith $"expected two-byte integer in Ldelem.u2, got: %O{value}"

            let state =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 value) currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            ExecutionResult.stepped (state, WhatWeDid.Executed)
        | Ldelem_i4 ->
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            let value = getArrayElt index arr currentThread state

            match value with
            | CliType.Numeric (CliNumericType.Int32 _) -> ()
            | _ -> failwith "expected int32 in Ldelem.i4"

            let state =
                state
                |> IlMachineState.pushToEvalStack value currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            ExecutionResult.stepped (state, WhatWeDid.Executed)
        | Ldelem_u4 ->
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            let value = getArrayElt index arr currentThread state

            match value with
            | CliType.Numeric (CliNumericType.Int32 _) -> ()
            | _ -> failwith $"expected four-byte integer in Ldelem.u4, got: %O{value}"

            let state =
                state
                |> IlMachineState.pushToEvalStack value currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            ExecutionResult.stepped (state, WhatWeDid.Executed)
        | Ldelem_i8 ->
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            let value = getArrayElt index arr currentThread state

            match value with
            | CliType.Numeric (CliNumericType.Int64 _) -> ()
            | _ -> failwith "expected int64 in Ldelem.i8"

            let state =
                state
                |> IlMachineState.pushToEvalStack value currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            ExecutionResult.stepped (state, WhatWeDid.Executed)
        | Ldelem_u8 -> failwith "TODO: Ldelem_u8 unimplemented"
        | Ldelem_r4 ->
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            let value = getArrayElt index arr currentThread state

            match value with
            | CliType.Numeric (CliNumericType.Float32 _) -> ()
            | _ -> failwith $"expected float32 in Ldelem.r4, got: %O{value}"

            let state =
                state
                |> IlMachineState.pushToEvalStack value currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            ExecutionResult.stepped (state, WhatWeDid.Executed)
        | Ldelem_r8 ->
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            let value = getArrayElt index arr currentThread state

            match value with
            | CliType.Numeric (CliNumericType.Float64 _) -> ()
            | _ -> failwith $"expected float64 in Ldelem.r8, got: %O{value}"

            let state =
                state
                |> IlMachineState.pushToEvalStack value currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            ExecutionResult.stepped (state, WhatWeDid.Executed)
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

            stElem
                loggerFactory
                corelib
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

            stElem loggerFactory corelib (CliType.Numeric (CliNumericType.Int8 0y)) value index arr currentThread state
        | Stelem_u1 -> failwith "TODO: Stelem_u1 unimplemented"
        | Stelem_i2 ->
            let value, state = IlMachineState.popEvalStack currentThread state
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            stElem loggerFactory corelib (CliType.Numeric (CliNumericType.Int16 0s)) value index arr currentThread state
        | Stelem_u2 -> failwith "TODO: Stelem_u2 unimplemented"
        | Stelem_i4 ->
            let value, state = IlMachineState.popEvalStack currentThread state
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            stElem loggerFactory corelib (CliType.Numeric (CliNumericType.Int32 0)) value index arr currentThread state
        | Stelem_u4 -> failwith "TODO: Stelem_u4 unimplemented"
        | Stelem_i8 ->
            let value, state = IlMachineState.popEvalStack currentThread state
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            stElem
                loggerFactory
                corelib
                (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L)))
                value
                index
                arr
                currentThread
                state
        | Stelem_u8 -> failwith "TODO: Stelem_u8 unimplemented"
        | Stelem_r4 ->
            let value, state = IlMachineState.popEvalStack currentThread state
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            stElem
                loggerFactory
                corelib
                (CliType.Numeric (CliNumericType.Float32 0.0f))
                value
                index
                arr
                currentThread
                state
        | Stelem_r8 ->
            let value, state = IlMachineState.popEvalStack currentThread state
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state

            stElem
                loggerFactory
                corelib
                (CliType.Numeric (CliNumericType.Float64 0.0))
                value
                index
                arr
                currentThread
                state
        | Stelem_ref ->
            let value, state = IlMachineState.popEvalStack currentThread state
            let index, state = IlMachineState.popEvalStack currentThread state
            let arr, state = IlMachineState.popEvalStack currentThread state
            stElem loggerFactory corelib (CliType.ObjectRef None) value index arr currentThread state
        | Cpblk -> failwith "TODO: Cpblk unimplemented"
        | Initblk -> failwith "TODO: Initblk unimplemented"
        | Conv_ovf_u1 ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            match convOvfU1 popped with
            | Ok conv ->
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (int32 conv)) currentThread
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
        | Conv_ovf_u2 -> failwith "TODO: Conv_ovf_u2 unimplemented"
        | Conv_ovf_u4 ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            match convOvfU4 popped with
            | Ok conv ->
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (int32 conv)) currentThread
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
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (int32 conv)) currentThread
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
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 conv) currentThread
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
            let converted = EvalStackValue.convUnsignedToFloat popped

            let state =
                match converted with
                | None -> failwith "TODO: Conv_r_un conversion failure unimplemented"
                | Some conv ->
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
