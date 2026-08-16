namespace WoofWare.PawPrint

#nowarn "42"

/// See I.12.3.2.1 for definition
type EvalStackValue =
    /// An int32 slot. The payload carries provenance because `conv.i4` / `conv.u4`
    /// can put a truncated byref here; see `Int32Source`.
    | Int32 of Int32Source
    | Int64 of Int64Source
    | NativeInt of NativeIntSource
    | Float of float
    | ManagedPointer of ManagedPointerSource
    | NullObjectRef
    | ObjectRef of ManagedHeapAddress
    /// This doesn't match what the CLR does in reality, but we can work out whatever we need from it.
    | UserDefinedValueType of CliValueType

    override this.ToString () =
        match this with
        | EvalStackValue.Int32 i -> $"Int32(%O{i})"
        | EvalStackValue.Int64 i -> $"Int64(%O{i})"
        | EvalStackValue.NativeInt src -> $"NativeInt(%O{src})"
        | EvalStackValue.Float f -> $"Float(%f{f})"
        | EvalStackValue.ManagedPointer managedPointerSource -> $"Pointer(%O{managedPointerSource})"
        | EvalStackValue.NullObjectRef -> "NullObjectRef"
        | EvalStackValue.ObjectRef managedHeapAddress -> $"ObjectRef(%O{managedHeapAddress})"
        | EvalStackValue.UserDefinedValueType evalStackValues -> $"Struct(%O{evalStackValues})"

[<RequireQualifiedAccess>]
module EvalStackValue =
    /// Decode a `MethodTable*` argument to the closed type it describes, or fail loudly.
    /// Shared by every consumer of a MethodTable-shaped native argument — the QCall/InternalCall
    /// boundary (`NativeCall.methodTableOfEvalStackValue`) and `calli` through the runtime's
    /// allocation helper — so that all of them agree on what counts as a MethodTable pointer.
    ///
    /// Deliberately narrow: only a *closed* type has a MethodTable that can be allocated from
    /// or reflected over here. An open generic definition and a generic parameter both have
    /// non-`Closed` targets, and CoreCLR's TypeDescs have no MethodTable at all, so each is
    /// refused with its own message rather than coerced into a closed handle.
    let requireMethodTable (operation : string) (arg : EvalStackValue) : ConcreteTypeHandle =
        match arg with
        | EvalStackValue.NativeInt (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed typeHandle))
        | EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed typeHandle)) ->
            typeHandle
        | EvalStackValue.NativeInt (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity))
        | EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity)) ->
            failwith $"%s{operation}: expected closed MethodTable pointer argument, got open generic %O{identity}"
        | EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.GenericParameter _ as target))
        | EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.MethodGenericParameter _ as target)) ->
            failwith
                $"%s{operation}: expected closed MethodTable pointer argument, got generic parameter %O{target} (TypeDescs have no MethodTable)"
        | other -> failwith $"%s{operation}: expected MethodTable pointer argument, got %O{other}"

    /// Bits of a native-int-shaped value for `conv.r4` / `conv.r8` / `conv.r.un`.
    ///
    /// A float destination refuses every shape whose bits PawPrint does not model,
    /// rather than synthesising them the way the narrowing integer conversions do.
    /// The difference is that a narrowing conversion keeps its result in the integer
    /// domain, where `Int64Source.OpaqueHashBits` and the pointer-shaped
    /// `NativeIntSource` cases can still recognise a synthesised value and refuse to
    /// let it become a pointer again; a float carries no such tag, so bits laundered
    /// into it are indistinguishable from a measurement of a real address.
    let private nativeIntBitsForFloatConversion (operation : string) (src : NativeIntSource) : int64 =
        match src with
        | NativeIntSource.Verbatim i -> i
        | NativeIntSource.SyntheticCrossArrayOffset _ ->
            failwith $"%s{operation}: refusing to convert cross-array offset to a float"
        | NativeIntSource.ManagedPointer ManagedPointerSource.Null -> 0L
        | NativeIntSource.ManagedPointer (ManagedPointerSource.NativeIntPlaceholder bits) ->
            // `Unsafe.AsRef<T>((void*)bits)` placeholders ARE bit patterns, not
            // addresses, so converting one to a float is ordinary arithmetic on a
            // number the guest itself supplied.
            bits
        | NativeIntSource.ManagedPointer ptr ->
            failwith $"%s{operation}: refusing to convert managed pointer %O{ptr} to a float"
        | NativeIntSource.FunctionPointer methodInfo ->
            failwith $"%s{operation}: refusing to convert function pointer %O{methodInfo} to a float"
        | NativeIntSource.TypeHandlePtr typeHandle ->
            failwith $"%s{operation}: refusing to convert RuntimeTypeHandle pointer %O{typeHandle} to a float"
        | NativeIntSource.TypeDescPtr typeHandle ->
            failwith $"%s{operation}: refusing to convert TypeDesc pointer %O{typeHandle} to a float"
        | NativeIntSource.MethodTablePtr typeHandle ->
            failwith $"%s{operation}: refusing to convert MethodTable pointer %O{typeHandle} to a float"
        | NativeIntSource.MethodTableAuxiliaryDataPtr typeHandle ->
            failwith $"%s{operation}: refusing to convert MethodTableAuxiliaryData pointer %O{typeHandle} to a float"
        | NativeIntSource.PerInstInfoPtr handle ->
            failwith $"%s{operation}: refusing to convert PerInstInfo pointer %O{handle} to a float"
        | NativeIntSource.PerInstDictPtr handle ->
            failwith $"%s{operation}: refusing to convert PerInstDict pointer %O{handle} to a float"
        | NativeIntSource.FieldHandlePtr handle ->
            failwith $"%s{operation}: refusing to convert RuntimeFieldHandle pointer %d{handle} to a float"
        | NativeIntSource.MethodHandlePtr handle ->
            failwith $"%s{operation}: refusing to convert RuntimeMethodHandle pointer %d{handle} to a float"
        | NativeIntSource.GcHandlePtr (handle, _) ->
            failwith $"%s{operation}: refusing to convert GC handle pointer %O{handle} to a float"
        | NativeIntSource.EventPipeProviderPtr id ->
            failwith $"%s{operation}: refusing to convert EventPipe provider handle #%d{id} to a float"
        | NativeIntSource.EventPipeEventPtr id ->
            failwith $"%s{operation}: refusing to convert EventPipe event handle #%d{id} to a float"
        | NativeIntSource.LowLevelMonitorPtr id ->
            failwith $"%s{operation}: refusing to convert low-level monitor handle %O{id} to a float"
        | NativeIntSource.WaitHandlePtr id ->
            failwith $"%s{operation}: refusing to convert wait handle %O{id} to a float"
        | NativeIntSource.AssemblyHandle assemblyName ->
            failwith $"%s{operation}: refusing to convert assembly handle %s{assemblyName} to a float"
        | NativeIntSource.ModuleHandle moduleName ->
            failwith $"%s{operation}: refusing to convert module handle %s{moduleName} to a float"
        | NativeIntSource.MetadataImportHandle moduleName ->
            failwith $"%s{operation}: refusing to convert metadata import handle %s{moduleName} to a float"
        | NativeIntSource.OpaqueHashBits bits ->
            // The three float conversions each refuse this shape before reaching
            // here, so that their diagnostic can name the native-int slot. Refuse
            // again rather than fall through to a permissive answer: a float is the
            // one destination from which a synthesised value can never be
            // recognised again.
            failwith $"%s{operation}: refusing to convert synthesised pointer-hash bits 0x%x{bits} to a float"

    let private failReferenceConversion (operation : string) (value : EvalStackValue) : 'a =
        match value with
        | EvalStackValue.ManagedPointer ptr -> failwith $"%s{operation}: refusing to convert managed pointer %O{ptr}"
        | EvalStackValue.NullObjectRef -> failwith $"%s{operation}: refusing to convert null object reference"
        | EvalStackValue.ObjectRef addr -> failwith $"%s{operation}: refusing to convert object reference %O{addr}"
        | EvalStackValue.UserDefinedValueType valueType ->
            failwith $"%s{operation}: refusing to convert user-defined value type %O{valueType}"
        | _ -> failwith $"%s{operation}: unexpected non-reference value %O{value}"

    let private convIFromInt32 (value : int32) : int64 =
        let converted = (# "conv.i" value : nativeint #)
        int64<nativeint> converted

    let private convIFromInt64 (value : int64) : int64 =
        let converted = (# "conv.i" value : nativeint #)
        int64<nativeint> converted

    let private convIFromFloat (value : float) : int64 =
        let converted = (# "conv.i" value : nativeint #)
        int64<nativeint> converted

    let private convUFromFloat (value : float) : uint64 =
        let converted = (# "conv.u" value : unativeint #)
        uint64<unativeint> converted

    let private convI1FromInt64 (value : int64) : int32 =
        let converted = (# "conv.i1" value : int8 #)
        int32<int8> converted

    let private convI1FromInt32 (value : int32) : int32 =
        let converted = (# "conv.i1" value : int8 #)
        int32<int8> converted

    let private convI1FromFloat (value : float) : int32 =
        let converted = (# "conv.i1" value : int8 #)
        int32<int8> converted

    let private convI2FromInt64 (value : int64) : int32 =
        let converted = (# "conv.i2" value : int16 #)
        int32<int16> converted

    let private convI2FromInt32 (value : int32) : int32 =
        let converted = (# "conv.i2" value : int16 #)
        int32<int16> converted

    let private convI2FromFloat (value : float) : int32 =
        let converted = (# "conv.i2" value : int16 #)
        int32<int16> converted

    let private convI4FromInt64 (value : int64) : int32 = (# "conv.i4" value : int32 #)

    let private convI4FromFloat (value : float) : int32 = (# "conv.i4" value : int32 #)

    let private convI8FromFloat (value : float) : int64 = (# "conv.i8" value : int64 #)

    let private convU1FromInt64 (value : int64) : int32 =
        let converted = (# "conv.u1" value : uint8 #)
        int32<uint8> converted

    let private convU1FromInt32 (value : int32) : int32 =
        let converted = (# "conv.u1" value : uint8 #)
        int32<uint8> converted

    let private convU1FromFloat (value : float) : int32 =
        let converted = (# "conv.u1" value : uint8 #)
        int32<uint8> converted

    let private convU2FromInt64 (value : int64) : int32 =
        let converted = (# "conv.u2" value : uint16 #)
        int32<uint16> converted

    let private convU2FromInt32 (value : int32) : int32 =
        let converted = (# "conv.u2" value : uint16 #)
        int32<uint16> converted

    let private convU2FromFloat (value : float) : int32 =
        let converted = (# "conv.u2" value : uint16 #)
        int32<uint16> converted

    let private convU4FromInt64 (value : int64) : int32 =
        let converted = (# "conv.u4" value : uint32 #)
        int32<uint32> converted

    let private convU4FromInt32 (value : int32) : int32 =
        let converted = (# "conv.u4" value : uint32 #)
        int32<uint32> converted

    let private convU4FromFloat (value : float) : int32 =
        let converted = (# "conv.u4" value : uint32 #)
        int32<uint32> converted

    let private convU8FromFloat (value : float) : int64 =
        let converted = (# "conv.u8" value : uint64 #)
        int64<uint64> converted

    let private convR4FromInt32 (value : int32) : float =
        let converted = (# "conv.r4" value : float32 #)
        float<float32> converted

    let private convR4FromInt64 (value : int64) : float =
        let converted = (# "conv.r4" value : float32 #)
        float<float32> converted

    let private convR4FromFloat (value : float) : float =
        let converted = (# "conv.r4" value : float32 #)
        float<float32> converted

    let private convR8FromInt32 (value : int32) : float = (# "conv.r8" value : float #)

    let private convR8FromInt64 (value : int64) : float = (# "conv.r8" value : float #)

    let private convR8FromFloat (value : float) : float = (# "conv.r8" value : float #)

    let private convRUnFromInt32 (value : int32) : float = (# "conv.r.un" value : float #)

    let private convRUnFromInt64 (value : int64) : float = (# "conv.r.un" value : float #)

    /// The conversion performed by Conv_u.
    let toUnsignedNativeInt (value : EvalStackValue) : UnsignedNativeIntSource =
        // Table III.8. Negative inputs are bit-reinterpreted (zero-extended
        // for Int32, same bits for Int64/NativeInt); the F# `uint32`/`uint64`
        // conversions from signed already do this.
        match value with
        | EvalStackValue.Int32 int32Source ->
            let i = Int32Source.value "Conv_U" int32Source
            uint64 (uint32 i) |> UnsignedNativeIntSource.Verbatim
        | EvalStackValue.Int64 (Int64Source.Verbatim i) -> uint64 i |> UnsignedNativeIntSource.Verbatim
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset i) ->
            UnsignedNativeIntSource.FromSyntheticCrossArrayStorage i
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)) ->
            // Inversion of `Conv.U8` / `Conv.I8` followed by `Conv.U`. On a
            // 64-bit interpreter the widening is bit-preserving, so the
            // truncation back to native int recovers the original
            // NativeIntSource. On a 32-bit interpreter this would lose the
            // high 32 bits, which is exactly the wraparound that the
            // `UnmanagedMemoryStream.Initialize` idiom checks for; modelling
            // that would require revisiting `NATIVE_INT_SIZE`.
            match src with
            | NativeIntSource.ManagedPointer ptr -> UnsignedNativeIntSource.FromManagedPointer ptr
            | NativeIntSource.SyntheticCrossArrayOffset s -> UnsignedNativeIntSource.FromSyntheticCrossArrayStorage s
            | NativeIntSource.Verbatim n -> UnsignedNativeIntSource.Verbatim (uint64 n)
            | _ -> failwith $"TODO: Conv_U from widened native int with non-pointer source %O{src}"
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) ->
            // `conv.u` narrows synthesised hash bits from int64 width back to
            // native-int width. Preserve the synthesis tag so downstream code
            // (e.g. `BitOperations.RotateLeft`'s `(nuint)` cast) sees the same
            // contract: deterministic numeric content, not a real pointer.
            UnsignedNativeIntSource.FromOpaqueHashBits bits
        | EvalStackValue.NativeInt i ->
            match i with
            | NativeIntSource.Verbatim i -> uint64 i |> UnsignedNativeIntSource.Verbatim
            | NativeIntSource.SyntheticCrossArrayOffset i -> UnsignedNativeIntSource.FromSyntheticCrossArrayStorage i
            | NativeIntSource.ManagedPointer ptr -> UnsignedNativeIntSource.FromManagedPointer ptr
            | NativeIntSource.FunctionPointer methodInfo ->
                failwith $"Conv_U: refusing to convert function pointer %O{methodInfo} to unsigned native int"
            | NativeIntSource.FieldHandlePtr handle ->
                failwith $"Conv_U: refusing to convert RuntimeFieldHandle pointer %d{handle} to unsigned native int"
            | NativeIntSource.MethodHandlePtr handle ->
                failwith $"Conv_U: refusing to convert RuntimeMethodHandle pointer %d{handle} to unsigned native int"
            | NativeIntSource.TypeHandlePtr typeHandle ->
                failwith $"Conv_U: refusing to convert RuntimeTypeHandle pointer %O{typeHandle} to unsigned native int"
            | NativeIntSource.TypeDescPtr typeHandle ->
                failwith $"Conv_U: refusing to convert TypeDesc pointer %O{typeHandle} to unsigned native int"
            | NativeIntSource.MethodTablePtr typeHandle ->
                failwith $"Conv_U: refusing to convert MethodTable pointer %O{typeHandle} to unsigned native int"
            | NativeIntSource.MethodTableAuxiliaryDataPtr typeHandle ->
                failwith
                    $"Conv_U: refusing to convert MethodTableAuxiliaryData pointer %O{typeHandle} to unsigned native int"
            | NativeIntSource.PerInstInfoPtr handle ->
                failwith $"Conv_U: refusing to convert PerInstInfo pointer %O{handle} to unsigned native int"
            | NativeIntSource.PerInstDictPtr handle ->
                failwith $"Conv_U: refusing to convert PerInstDict pointer %O{handle} to unsigned native int"
            | NativeIntSource.GcHandlePtr (handle, _) ->
                failwith $"Conv_U: refusing to convert GC handle pointer %O{handle} to unsigned native int"
            | NativeIntSource.EventPipeProviderPtr id ->
                failwith $"Conv_U: refusing to convert EventPipe provider handle #%d{id} to unsigned native int"
            | NativeIntSource.EventPipeEventPtr id ->
                failwith $"Conv_U: refusing to convert EventPipe event handle #%d{id} to unsigned native int"
            | NativeIntSource.LowLevelMonitorPtr id ->
                failwith $"Conv_U: refusing to convert low-level monitor handle %O{id} to unsigned native int"
            | NativeIntSource.WaitHandlePtr id ->
                failwith $"Conv_U: refusing to convert wait handle %O{id} to unsigned native int"
            | NativeIntSource.AssemblyHandle assemblyName ->
                failwith $"Conv_U: refusing to convert assembly handle %s{assemblyName} to unsigned native int"
            | NativeIntSource.ModuleHandle moduleName ->
                failwith $"Conv_U: refusing to convert module handle %s{moduleName} to unsigned native int"
            | NativeIntSource.MetadataImportHandle moduleName ->
                failwith $"Conv_U: refusing to convert metadata import handle %s{moduleName} to unsigned native int"
            | NativeIntSource.OpaqueHashBits bits -> UnsignedNativeIntSource.FromOpaqueHashBits bits
        | EvalStackValue.Float f -> convUFromFloat f |> UnsignedNativeIntSource.Verbatim
        | EvalStackValue.ManagedPointer managedPointerSource ->
            UnsignedNativeIntSource.FromManagedPointer managedPointerSource
        | EvalStackValue.NullObjectRef -> ManagedPointerSource.Null |> UnsignedNativeIntSource.FromManagedPointer
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_U" value

    /// The conversion performed by Conv_i.
    let toNativeInt (value : EvalStackValue) : NativeIntSource =
        match value with
        | EvalStackValue.Int64 (Int64Source.Verbatim i) -> i |> convIFromInt64 |> NativeIntSource.Verbatim
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset i) -> NativeIntSource.SyntheticCrossArrayOffset i
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)) ->
            // Inversion of `Conv.U8` / `Conv.I8` followed by `Conv.I`. See
            // the matching arm in `toUnsignedNativeInt` for the architecture
            // assumption.
            src
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) ->
            // `conv.i` narrows synthesised hash bits from int64 width back to
            // native-int width. The tag is preserved so the bits remain
            // distinguishable from real-pointer NativeInt sources.
            NativeIntSource.OpaqueHashBits bits
        | EvalStackValue.Int32 int32Source ->
            let i = Int32Source.value "Conv_I" int32Source
            i |> convIFromInt32 |> NativeIntSource.Verbatim
        | EvalStackValue.NativeInt src -> src
        | EvalStackValue.Float f -> f |> convIFromFloat |> NativeIntSource.Verbatim
        | EvalStackValue.ManagedPointer ptr -> NativeIntSource.ManagedPointer ptr
        | EvalStackValue.NullObjectRef -> ManagedPointerSource.Null |> NativeIntSource.ManagedPointer
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_I" value

    /// `conv.i1`.
    ///
    /// The narrowing integer conversions (`conv.i1` / `conv.i2` / `conv.i4` and
    /// their unsigned counterparts) take `PointerHashState` because a
    /// pointer-shaped source must be materialised into bits before it can be
    /// truncated. The destination is narrower than a pointer, so the result cannot
    /// be a pointer and the honest answer is the source's bits;
    /// `PointerHashSynthesis.materialiseHashBits` supplies them from the pointer's
    /// identity, memoised so that one pointer always truncates to one number, and
    /// refuses exactly the shapes a narrowing must refuse — a real byref, whose
    /// address PawPrint does not model, and a cross-array offset, which is a
    /// difference of two such addresses.
    let convToInt8 (value : EvalStackValue) (counters : PointerHashState) : int32 * PointerHashState =
        match value with
        | EvalStackValue.Int32 int32Source ->
            let i = Int32Source.value "Conv_I1" int32Source
            convI1FromInt32 i, counters
        | EvalStackValue.Int64 (Int64Source.Verbatim i) -> convI1FromInt64 i, counters
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _) -> failwith "TODO: SyntheticCrossArrayOffset"
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) -> convI1FromInt64 bits, counters
        // The two spellings of the operation share this arm, so `(sbyte)ptr` and
        // `(sbyte)(long)ptr` agree by construction rather than by coincidence.
        // CoreLib chooses between those spellings with `#if TARGET_64BIT` inside
        // `IntPtr.GetHashCode` (IntPtr.cs:90-97), so they have to.
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _))
        | EvalStackValue.NativeInt src ->
            let bits, counters = PointerHashSynthesis.materialiseHashBits "Conv_I1" src counters
            convI1FromInt64 bits, counters
        | EvalStackValue.Float f -> convI1FromFloat f, counters
        | EvalStackValue.ManagedPointer _
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_I1" value

    /// `conv.i2`. See `convToInt8` for why this takes `PointerHashState`.
    let convToInt16 (value : EvalStackValue) (counters : PointerHashState) : int32 * PointerHashState =
        match value with
        | EvalStackValue.Int32 int32Source ->
            let i = Int32Source.value "Conv_I2" int32Source
            convI2FromInt32 i, counters
        | EvalStackValue.Int64 (Int64Source.Verbatim i) -> convI2FromInt64 i, counters
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _) -> failwith "TODO: SyntheticCrossArrayOffset"
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) -> convI2FromInt64 bits, counters
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _))
        | EvalStackValue.NativeInt src ->
            let bits, counters = PointerHashSynthesis.materialiseHashBits "Conv_I2" src counters
            convI2FromInt64 bits, counters
        | EvalStackValue.Float f -> convI2FromFloat f, counters
        | EvalStackValue.ManagedPointer _
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_I2" value

    /// `conv.i4` / `conv.u4` on a byref, given that conversion's own truncation.
    /// See `Int32Source.narrowManagedPointer`.
    let private narrowByrefTo32 (truncate : int64 -> int32) (ptr : ManagedPointerSource) : EvalStackValue =
        Int32Source.narrowManagedPointer truncate ptr |> EvalStackValue.Int32

    /// `conv.i4`. See `convToInt8` for why this takes `PointerHashState`.
    let convToInt32 (value : EvalStackValue) (counters : PointerHashState) : EvalStackValue * PointerHashState =
        match value with
        // Identity, narrowed byrefs included: re-truncating a value that is
        // already 32 bits wide changes nothing.
        | EvalStackValue.Int32 i -> EvalStackValue.Int32 i, counters
        | EvalStackValue.Int64 (Int64Source.Verbatim i) ->
            convI4FromInt64 i |> Int32Source.Verbatim |> EvalStackValue.Int32, counters
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _) -> failwith "TODO: SyntheticCrossArrayOffset"
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) ->
            // Truncating synthesised hash bits to int32 is the path
            // `CastCache.KeyToBucket` takes: it ends in `(int)((hash * c) >> shift)`
            // to produce an array index. The result has no provenance, but
            // an array index doesn't need one.
            convI4FromInt64 bits |> Int32Source.Verbatim |> EvalStackValue.Int32, counters
        // A byref keeps more than hash bits would: `NarrowedManagedPointer` still
        // answers a mask, which is what managed code narrowing an address is usually
        // about, and `materialiseHashBits` refuses byrefs precisely so that a
        // synthesised number can never stand in for an address PawPrint doesn't model.
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (NativeIntSource.ManagedPointer ptr, _))
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr) -> narrowByrefTo32 convI4FromInt64 ptr, counters
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _))
        | EvalStackValue.NativeInt src ->
            let bits, counters = PointerHashSynthesis.materialiseHashBits "Conv_I4" src counters
            convI4FromInt64 bits |> Int32Source.Verbatim |> EvalStackValue.Int32, counters
        | EvalStackValue.Float f -> convI4FromFloat f |> Int32Source.Verbatim |> EvalStackValue.Int32, counters
        | EvalStackValue.ManagedPointer ptr -> narrowByrefTo32 convI4FromInt64 ptr, counters
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_I4" value

    let convToInt64 (value : EvalStackValue) : Int64Source =
        match value with
        | EvalStackValue.Int32 int32Source ->
            let i = Int32Source.value "Conv_I8" int32Source
            int64<int> i |> Int64Source.Verbatim
        | EvalStackValue.Int64 i -> i
        | EvalStackValue.NativeInt src ->
            // `widenedNativeInt` normalises the Verbatim/SyntheticCrossArrayOffset/Null
            // cases back to canonical Int64Source variants. Non-numeric
            // sources (managed pointers, function pointers, type handles)
            // get wrapped so their provenance survives the
            // `Conv.I8 → … → Conv.I` round-trip.
            Int64Source.widenedNativeInt src true
        | EvalStackValue.Float f -> convI8FromFloat f |> Int64Source.Verbatim
        | EvalStackValue.ManagedPointer ptr ->
            // Same rationale as the NativeInt arm: keep the pointer's provenance
            // as a widened-native-int so a subsequent `Conv.U` / `Conv.I`
            // recovers the original ManagedPointer.
            Int64Source.widenedNativeInt (NativeIntSource.ManagedPointer ptr) true
        | EvalStackValue.NullObjectRef ->
            Int64Source.widenedNativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null) true
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_I8" value

    /// `conv.u8`, then truncates to int64.
    let convToUInt64 (value : EvalStackValue) : Int64Source =
        match value with
        | EvalStackValue.Int32 int32Source ->
            let i = Int32Source.value "Conv_U8" int32Source
            int64 (uint32 i) |> Int64Source.Verbatim
        | EvalStackValue.Int64 i -> i
        | EvalStackValue.NativeInt src -> Int64Source.widenedNativeInt src false
        | EvalStackValue.Float f -> convU8FromFloat f |> Int64Source.Verbatim
        | EvalStackValue.ManagedPointer ptr -> Int64Source.widenedNativeInt (NativeIntSource.ManagedPointer ptr) false
        | EvalStackValue.NullObjectRef ->
            Int64Source.widenedNativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null) false
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_U8" value

    /// `conv.u1`, then truncates to int32. See `convToInt8` for why this takes
    /// `PointerHashState`.
    let convToUInt8 (value : EvalStackValue) (counters : PointerHashState) : int32 * PointerHashState =
        match value with
        | EvalStackValue.Int32 int32Source ->
            let i = Int32Source.value "Conv_U1" int32Source
            convU1FromInt32 i, counters
        | EvalStackValue.Int64 (Int64Source.Verbatim i) -> convU1FromInt64 i, counters
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _) -> failwith "TODO: SyntheticCrossArrayOffset"
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) -> convU1FromInt64 bits, counters
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _))
        | EvalStackValue.NativeInt src ->
            let bits, counters = PointerHashSynthesis.materialiseHashBits "Conv_U1" src counters
            convU1FromInt64 bits, counters
        | EvalStackValue.Float f -> convU1FromFloat f, counters
        | EvalStackValue.ManagedPointer _
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_U1" value

    /// `conv.u2`, then truncates to int32. See `convToInt8` for why this takes
    /// `PointerHashState`.
    let convToUInt16 (value : EvalStackValue) (counters : PointerHashState) : int32 * PointerHashState =
        match value with
        | EvalStackValue.Int32 int32Source ->
            let i = Int32Source.value "Conv_U2" int32Source
            convU2FromInt32 i, counters
        | EvalStackValue.Int64 (Int64Source.Verbatim i) -> convU2FromInt64 i, counters
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _) -> failwith "TODO: SyntheticCrossArrayOffset"
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) -> convU2FromInt64 bits, counters
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _))
        | EvalStackValue.NativeInt src ->
            let bits, counters = PointerHashSynthesis.materialiseHashBits "Conv_U2" src counters
            convU2FromInt64 bits, counters
        | EvalStackValue.Float f -> convU2FromFloat f, counters
        | EvalStackValue.ManagedPointer _
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_U2" value

    /// `conv.u4`, then truncates to int32. See `convToInt8` for why this takes
    /// `PointerHashState`.
    let convToUInt32 (value : EvalStackValue) (counters : PointerHashState) : EvalStackValue * PointerHashState =
        match value with
        // A narrowed byref is already 32 bits wide, and `conv.u4` only reinterprets
        // those bits; it neither discards any nor makes the value knowable.
        | EvalStackValue.Int32 (Int32Source.NarrowedManagedPointer _ as i) -> EvalStackValue.Int32 i, counters
        | EvalStackValue.Int32 (Int32Source.Verbatim i) ->
            convU4FromInt32 i |> Int32Source.Verbatim |> EvalStackValue.Int32, counters
        | EvalStackValue.Int64 (Int64Source.Verbatim i) ->
            convU4FromInt64 i |> Int32Source.Verbatim |> EvalStackValue.Int32, counters
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _) -> failwith "TODO: SyntheticCrossArrayOffset"
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) ->
            convU4FromInt64 bits |> Int32Source.Verbatim |> EvalStackValue.Int32, counters
        // Same rationale as `convToInt32`: the byref survives the narrowing so that
        // the mask managed code is about to apply stays answerable.
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (NativeIntSource.ManagedPointer ptr, _))
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr) -> narrowByrefTo32 convU4FromInt64 ptr, counters
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _))
        | EvalStackValue.NativeInt src ->
            let bits, counters = PointerHashSynthesis.materialiseHashBits "Conv_U4" src counters
            convU4FromInt64 bits |> Int32Source.Verbatim |> EvalStackValue.Int32, counters
        | EvalStackValue.Float f -> convU4FromFloat f |> Int32Source.Verbatim |> EvalStackValue.Int32, counters
        | EvalStackValue.ManagedPointer ptr -> narrowByrefTo32 convU4FromInt64 ptr, counters
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_U4" value

    let convToFloat32 (value : EvalStackValue) : float =
        match value with
        | EvalStackValue.Int32 int32Source ->
            let i = Int32Source.value "Conv_R4" int32Source
            convR4FromInt32 i
        | EvalStackValue.Int64 (Int64Source.Verbatim i) -> convR4FromInt64 i
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _) ->
            failwith "Refusing to convert byte offset to float"
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)) ->
            failwith $"Refusing to convert widened native int %O{src} to float"
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) ->
            failwith $"Refusing to convert synthesised pointer-hash bits 0x%x{bits} to float"
        | EvalStackValue.NativeInt (NativeIntSource.OpaqueHashBits bits) ->
            // Matches the Int64 OpaqueHashBits refusal above. The helper below
            // would let these bits become a float, materialising synthesised
            // pointer provenance into the float domain.
            failwith $"Refusing to convert synthesised pointer-hash bits 0x%x{bits} (native int) to float"
        | EvalStackValue.NativeInt src -> nativeIntBitsForFloatConversion "Conv_R4" src |> convR4FromInt64
        | EvalStackValue.Float f -> convR4FromFloat f
        | EvalStackValue.ManagedPointer _
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_R4" value

    let convToFloat64 (value : EvalStackValue) : float =
        match value with
        | EvalStackValue.Int32 int32Source ->
            let i = Int32Source.value "Conv_R8" int32Source
            convR8FromInt32 i
        | EvalStackValue.Int64 (Int64Source.Verbatim i) -> convR8FromInt64 i
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _) ->
            failwith "Refusing to convert byte offset to float"
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)) ->
            failwith $"Refusing to convert widened native int %O{src} to float"
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) ->
            failwith $"Refusing to convert synthesised pointer-hash bits 0x%x{bits} to float"
        | EvalStackValue.NativeInt (NativeIntSource.OpaqueHashBits bits) ->
            failwith $"Refusing to convert synthesised pointer-hash bits 0x%x{bits} (native int) to float"
        | EvalStackValue.NativeInt src -> nativeIntBitsForFloatConversion "Conv_R8" src |> convR8FromInt64
        | EvalStackValue.Float f -> convR8FromFloat f
        | EvalStackValue.ManagedPointer _
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_R8" value

    let convUnsignedToFloat (value : EvalStackValue) : float =
        match value with
        | EvalStackValue.Int32 int32Source ->
            let i = Int32Source.value "Conv_R_un" int32Source
            convRUnFromInt32 i
        | EvalStackValue.Int64 (Int64Source.Verbatim i) -> convRUnFromInt64 i
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _) ->
            failwith "Refusing to convert byte offset to float"
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)) ->
            failwith $"Refusing to convert widened native int %O{src} to float"
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) ->
            failwith $"Refusing to convert synthesised pointer-hash bits 0x%x{bits} to float"
        | EvalStackValue.NativeInt (NativeIntSource.OpaqueHashBits bits) ->
            failwith $"Refusing to convert synthesised pointer-hash bits 0x%x{bits} (native int) to float"
        | EvalStackValue.NativeInt src -> nativeIntBitsForFloatConversion "Conv_R_Un" src |> convRUnFromInt64
        | EvalStackValue.Float _ -> failwith "Conv_R_Un: refusing to convert an existing float as unsigned integer"
        | EvalStackValue.ManagedPointer _
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_R_Un" value

    /// The integer bits of a stack value, but only when PawPrint already knows them —
    /// never by synthesising an address for a pointer it does not model.
    ///
    /// This is the honest half of what the narrowing conversions do. Verbatim numbers, the
    /// two exactly-known pointer bit patterns (`Null`, and the `NativeIntPlaceholder`
    /// produced by `Unsafe.AsRef<T>((void*)bits)`), and already-synthesised hash bits all
    /// have bits to report. A real pointer or handle does not: reporting one means
    /// assigning it a `PointerHashState` identity, which is a side effect a caller may
    /// have no business causing — so those, and cross-array offsets, return `ValueNone`
    /// and leave the caller to refuse with its own diagnostic.
    let rec tryExactIntegerBits (value : EvalStackValue) : int64 voption =
        match value with
        | EvalStackValue.Int32 (Int32Source.Verbatim i) -> ValueSome (int64<int32> i)
        | EvalStackValue.Int32 (Int32Source.NarrowedManagedPointer _) -> ValueNone
        | EvalStackValue.Int64 (Int64Source.Verbatim i) -> ValueSome i
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) -> ValueSome bits
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _) -> ValueNone
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)) ->
            tryExactIntegerBits (EvalStackValue.NativeInt src)
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) -> ValueSome i
        | EvalStackValue.NativeInt (NativeIntSource.OpaqueHashBits bits) -> ValueSome bits
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr)
        | EvalStackValue.ManagedPointer ptr -> ManagedPointerSource.tryBitPatternBits ptr
        | EvalStackValue.NullObjectRef -> ValueSome 0L
        | EvalStackValue.NativeInt _
        | EvalStackValue.Float _
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> ValueNone

    let rec ofCliType (v : CliType) : EvalStackValue =
        match v with
        | CliType.Numeric numeric ->
            match numeric with
            | CliNumericType.Int32 i -> EvalStackValue.Int32 (Int32Source.Verbatim i)
            | CliNumericType.Int64 i -> EvalStackValue.Int64 i
            | CliNumericType.NativeInt i -> EvalStackValue.NativeInt i
            // Sign-extend types int8 and int16
            // Zero-extend unsigned int8/unsigned int16
            | CliNumericType.Int8 b -> int32<int8> b |> Int32Source.Verbatim |> EvalStackValue.Int32
            | CliNumericType.UInt8 b -> int32<uint8> b |> Int32Source.Verbatim |> EvalStackValue.Int32
            | CliNumericType.Int16 s -> int32<int16> s |> Int32Source.Verbatim |> EvalStackValue.Int32
            | CliNumericType.UInt16 s -> int32<uint16> s |> Int32Source.Verbatim |> EvalStackValue.Int32
            | CliNumericType.Float32 f -> EvalStackValue.Float (float<float32> f)
            | CliNumericType.Float64 f -> EvalStackValue.Float f
            | CliNumericType.NativeFloat f -> EvalStackValue.Float f
        | CliType.ObjectRef None -> EvalStackValue.NullObjectRef
        | CliType.ObjectRef (Some addr) -> EvalStackValue.ObjectRef addr
        // Zero-extend bool/char
        | CliType.Bool b -> int32 b |> Int32Source.Verbatim |> EvalStackValue.Int32
        | CliType.Char (high, low) -> int32 high * 256 + int32 low |> Int32Source.Verbatim |> EvalStackValue.Int32
        | CliType.RuntimePointer ptr ->
            match ptr with
            | CliRuntimePointer.Verbatim ptrInt -> NativeIntSource.Verbatim ptrInt |> EvalStackValue.NativeInt
            | CliRuntimePointer.TypeHandlePtr typeHandle ->
                NativeIntSource.TypeHandlePtr typeHandle |> EvalStackValue.NativeInt
            | CliRuntimePointer.TypeDescPtr typeHandle ->
                NativeIntSource.TypeDescPtr typeHandle |> EvalStackValue.NativeInt
            | CliRuntimePointer.FieldRegistryHandle ptrInt ->
                NativeIntSource.FieldHandlePtr ptrInt |> EvalStackValue.NativeInt
            | CliRuntimePointer.MethodRegistryHandle ptrInt ->
                NativeIntSource.MethodHandlePtr ptrInt |> EvalStackValue.NativeInt
            | CliRuntimePointer.MethodTablePtr typeHandle ->
                NativeIntSource.MethodTablePtr typeHandle |> EvalStackValue.NativeInt
            | CliRuntimePointer.MethodTableAuxiliaryDataPtr typeHandle ->
                NativeIntSource.MethodTableAuxiliaryDataPtr typeHandle
                |> EvalStackValue.NativeInt
            | CliRuntimePointer.PerInstInfoPtr handle ->
                NativeIntSource.PerInstInfoPtr handle |> EvalStackValue.NativeInt
            | CliRuntimePointer.PerInstDictPtr handle ->
                NativeIntSource.PerInstDictPtr handle |> EvalStackValue.NativeInt
            | CliRuntimePointer.Managed ptr -> ptr |> EvalStackValue.ManagedPointer
            | CliRuntimePointer.GcHandlePtr (addr, tag) ->
                NativeIntSource.GcHandlePtr (addr, tag) |> EvalStackValue.NativeInt
        | CliType.ValueType vt ->
            // Primitive-like single-field wrappers (IntPtr, RuntimeTypeHandle, enums, ...) all get
            // flattened to their underlying primitive on the stack. ECMA III.1.8 treats enums as
            // their underlying integer for every numeric/comparison opcode; flattening here means
            // cgt.un/clt.un/add/etc. don't need enum-specific arms. Storage stays wrapped;
            // `toCliTypeCoerced` re-wraps on the pop side when the target slot is primitive-like.
            if vt.PrimitiveLikeKind.IsSome then
                ofCliType (CliValueType.PrimitiveLikeField vt).Contents
            else
                EvalStackValue.UserDefinedValueType vt

    let rec toCliTypeCoerced (target : CliType) (popped : EvalStackValue) : CliType =
        match target with
        | CliType.Numeric numeric ->
            match numeric with
            | CliNumericType.Int32 _ ->
                match popped with
                | EvalStackValue.Int32 int32Source ->
                    let i = Int32Source.value "storing to an int32 location" int32Source
                    CliType.Numeric (CliNumericType.Int32 i)
                | EvalStackValue.UserDefinedValueType vt -> viewValueTypeAsPrimitive target vt
                | i -> failwith $"TODO: %O{i}"
            | CliNumericType.Int64 _ ->
                match popped with
                | EvalStackValue.Int64 i -> CliType.Numeric (CliNumericType.Int64 i)
                | EvalStackValue.NativeInt src ->
                    match src with
                    | NativeIntSource.Verbatim i ->
                        CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim i))
                    | NativeIntSource.SyntheticCrossArrayOffset _ -> failwith "TODO"
                    | NativeIntSource.ManagedPointer ptr -> failwith "TODO"
                    | NativeIntSource.FunctionPointer f -> failwith $"TODO: {f}"
                    | NativeIntSource.FieldHandlePtr f -> failwith $"TODO: {f}"
                    | NativeIntSource.MethodHandlePtr f -> failwith $"TODO: {f}"
                    | NativeIntSource.TypeHandlePtr f -> failwith $"TODO: {f}"
                    | NativeIntSource.TypeDescPtr f -> failwith $"TODO: {f}"
                    | NativeIntSource.MethodTablePtr f -> failwith $"TODO: {f}"
                    | NativeIntSource.MethodTableAuxiliaryDataPtr f -> failwith $"TODO: {f}"
                    | NativeIntSource.PerInstInfoPtr f ->
                        failwith $"refusing to coerce PerInstInfo pointer %O{f} to int64"
                    | NativeIntSource.PerInstDictPtr f ->
                        failwith $"refusing to coerce PerInstDict pointer %O{f} to int64"
                    | NativeIntSource.GcHandlePtr (f, tag) -> failwith $"TODO: {f} (tag 0x%x{tag})"
                    | NativeIntSource.EventPipeProviderPtr id ->
                        failwith $"refusing to coerce EventPipe provider handle #%d{id} to int64"
                    | NativeIntSource.EventPipeEventPtr id ->
                        failwith $"refusing to coerce EventPipe event handle #%d{id} to int64"
                    | NativeIntSource.LowLevelMonitorPtr id ->
                        failwith $"refusing to coerce low-level monitor handle %O{id} to int64"
                    | NativeIntSource.WaitHandlePtr id -> failwith $"refusing to coerce wait handle %O{id} to int64"
                    | NativeIntSource.AssemblyHandle f -> failwith $"TODO: {f}"
                    | NativeIntSource.ModuleHandle f -> failwith $"TODO: {f}"
                    | NativeIntSource.MetadataImportHandle f ->
                        failwith $"refusing to coerce metadata import handle %s{f} to int64"
                    | NativeIntSource.OpaqueHashBits bits ->
                        // Widening synthesised pointer-hash bits to an int64 slot
                        // is the inverse of `conv.u` from `Int64Source.OpaqueHashBits`.
                        CliType.Numeric (CliNumericType.Int64 (Int64Source.OpaqueHashBits bits))
                // CliType.Numeric (CliNumericType.TypeHandlePtr f)
                | EvalStackValue.UserDefinedValueType vt -> viewValueTypeAsPrimitive target vt
                | i -> failwith $"TODO: %O{i}"
            | CliNumericType.NativeInt _ ->
                match popped with
                | EvalStackValue.NativeInt s -> CliNumericType.NativeInt s |> CliType.Numeric
                | EvalStackValue.ManagedPointer ptrSrc ->
                    CliNumericType.NativeInt (NativeIntSource.ManagedPointer ptrSrc)
                    |> CliType.Numeric
                | EvalStackValue.UserDefinedValueType vt ->
                    // Deliberately *not* `viewValueTypeAsPrimitive`, unlike every other primitive
                    // width. The native-int slot is the one that carries pointer provenance, so
                    // the conversions below are lossless where a byte-level reinterpretation could
                    // not be: `CliType.ToBytes` refuses to express a pointer, a handle or a
                    // widened native int as bytes, and rightly so. Routing this arm through the
                    // shared projector would turn those reads into refusals.
                    let popped = CliValueType.DereferenceFieldAt 0 NATIVE_INT_SIZE vt

                    match popped with
                    | CliType.Numeric (CliNumericType.NativeInt i) -> CliType.Numeric (CliNumericType.NativeInt i)
                    | CliType.Numeric (CliNumericType.Int64 i) ->
                        match i with
                        | Int64Source.Verbatim i ->
                            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim i))
                        | Int64Source.SyntheticCrossArrayOffset i ->
                            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.SyntheticCrossArrayOffset i))
                        | Int64Source.WidenedNativeInt (src, _) ->
                            // The int64 carries a widened NativeIntSource; truncating
                            // back to native int recovers the original source on
                            // 64-bit (the widening is bit-preserving).
                            CliType.Numeric (CliNumericType.NativeInt src)
                        | Int64Source.OpaqueHashBits bits ->
                            failwith
                                $"refusing to coerce synthesised pointer-hash bits 0x%x{bits} into a native int (would forge pointer provenance)"
                    | CliType.RuntimePointer ptr ->
                        match ptr with
                        | CliRuntimePointer.Verbatim i ->
                            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim i))
                        | CliRuntimePointer.TypeHandlePtr typeHandle ->
                            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr typeHandle))
                        | CliRuntimePointer.TypeDescPtr typeHandle ->
                            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeDescPtr typeHandle))
                        | CliRuntimePointer.FieldRegistryHandle ptr ->
                            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr ptr))
                        | CliRuntimePointer.MethodRegistryHandle ptr ->
                            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.MethodHandlePtr ptr))
                        | CliRuntimePointer.MethodTablePtr typeHandle ->
                            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.MethodTablePtr typeHandle))
                        | CliRuntimePointer.MethodTableAuxiliaryDataPtr typeHandle ->
                            CliType.Numeric (
                                CliNumericType.NativeInt (NativeIntSource.MethodTableAuxiliaryDataPtr typeHandle)
                            )
                        | CliRuntimePointer.PerInstInfoPtr handle ->
                            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.PerInstInfoPtr handle))
                        | CliRuntimePointer.PerInstDictPtr handle ->
                            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.PerInstDictPtr handle))
                        | CliRuntimePointer.Managed src ->
                            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer src))
                        | CliRuntimePointer.GcHandlePtr (addr, tag) ->
                            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.GcHandlePtr (addr, tag)))
                    | _ -> failwith $"TODO: {popped}"
                | _ -> failwith $"TODO: {popped}"
            | CliNumericType.NativeFloat f -> failwith "todo"
            | CliNumericType.Int8 _ ->
                match popped with
                | EvalStackValue.Int32 int32Source ->
                    let i = Int32Source.value "storing to an int8 location" int32Source
                    CliType.Numeric (CliNumericType.Int8 (i % 256 |> int8))
                | EvalStackValue.UserDefinedValueType vt -> viewValueTypeAsPrimitive target vt
                | i -> failwith $"TODO: %O{i}"
            | CliNumericType.Int16 _ ->
                match popped with
                | EvalStackValue.Int32 int32Source ->
                    let popped = Int32Source.value "storing to an int16 location" int32Source
                    CliType.Numeric (CliNumericType.Int16 (popped % 65536 |> int16<int>))
                | EvalStackValue.UserDefinedValueType vt -> viewValueTypeAsPrimitive target vt
                | _ -> failwith $"TODO: {popped}"
            | CliNumericType.UInt8 _ ->
                match popped with
                | EvalStackValue.Int32 int32Source ->
                    let i = Int32Source.value "storing to a uint8 location" int32Source
                    CliType.Numeric (CliNumericType.UInt8 (i % 256 |> uint8))
                | EvalStackValue.UserDefinedValueType vt -> viewValueTypeAsPrimitive target vt
                | i -> failwith $"todo: {i} to uint8"
            | CliNumericType.UInt16 _ ->
                match popped with
                | EvalStackValue.Int32 int32Source ->
                    let popped = Int32Source.value "storing to a uint16 location" int32Source
                    CliType.Numeric (CliNumericType.UInt16 (uint16<int32> popped))
                | EvalStackValue.UserDefinedValueType vt -> viewValueTypeAsPrimitive target vt
                | i -> failwith $"todo: {i} to uint16"
            | CliNumericType.Float32 _ ->
                match popped with
                | EvalStackValue.Float f -> CliType.Numeric (CliNumericType.Float32 (float32<float> f))
                | EvalStackValue.UserDefinedValueType vt -> viewValueTypeAsPrimitive target vt
                | i -> failwith $"todo: {i} to float32"
            | CliNumericType.Float64 _ ->
                match popped with
                | EvalStackValue.Float f -> CliType.Numeric (CliNumericType.Float64 f)
                | EvalStackValue.UserDefinedValueType vt -> viewValueTypeAsPrimitive target vt
                | _ -> failwith $"todo: {popped} to float64"
        | CliType.ObjectRef _ ->
            match popped with
            | EvalStackValue.NullObjectRef -> CliType.ObjectRef None
            | EvalStackValue.ObjectRef addr -> CliType.ObjectRef (Some addr)
            | EvalStackValue.NativeInt nativeIntSource ->
                match nativeIntSource with
                | NativeIntSource.Verbatim 0L -> CliType.ObjectRef None
                | NativeIntSource.Verbatim i -> failwith $"refusing to interpret verbatim native int {i} as a pointer"
                | NativeIntSource.SyntheticCrossArrayOffset _ ->
                    failwith "refusing to interpret synthetic cross-storage byte offset as a pointer"
                | NativeIntSource.FunctionPointer _ -> failwith "TODO"
                | NativeIntSource.TypeHandlePtr _ -> failwith "refusing to interpret type handle ID as an object ref"
                | NativeIntSource.TypeDescPtr _ -> failwith "refusing to interpret TypeDesc pointer as an object ref"
                | NativeIntSource.MethodTablePtr _ ->
                    failwith "refusing to interpret method table pointer as an object ref"
                | NativeIntSource.MethodTableAuxiliaryDataPtr _ ->
                    failwith "refusing to interpret method table auxiliary-data pointer as an object ref"
                | NativeIntSource.PerInstInfoPtr _ ->
                    failwith "refusing to interpret PerInstInfo pointer as an object ref"
                | NativeIntSource.PerInstDictPtr _ ->
                    failwith "refusing to interpret PerInstDict pointer as an object ref"
                | NativeIntSource.MethodHandlePtr _ ->
                    failwith "refusing to interpret method handle ID as an object ref"
                | NativeIntSource.FieldHandlePtr _ -> failwith "refusing to interpret field handle ID as an object ref"
                | NativeIntSource.GcHandlePtr _ -> failwith "refusing to interpret GC handle ID as an object ref"
                | NativeIntSource.EventPipeProviderPtr _ ->
                    failwith "refusing to interpret EventPipe provider handle as an object ref"
                | NativeIntSource.EventPipeEventPtr _ ->
                    failwith "refusing to interpret EventPipe event handle as an object ref"
                | NativeIntSource.LowLevelMonitorPtr _ ->
                    failwith "refusing to interpret low-level monitor handle as an object ref"
                | NativeIntSource.WaitHandlePtr _ -> failwith "refusing to interpret wait handle as an object ref"
                | NativeIntSource.AssemblyHandle _ -> failwith "refusing to interpret assembly handle as an object ref"
                | NativeIntSource.ModuleHandle _ -> failwith "refusing to interpret module handle as an object ref"
                | NativeIntSource.MetadataImportHandle _ ->
                    failwith "refusing to interpret metadata import handle as an object ref"
                | NativeIntSource.OpaqueHashBits bits ->
                    failwith
                        $"refusing to interpret synthesised pointer-hash bits 0x%x{bits} as an object ref (would forge a heap address)"
                | NativeIntSource.ManagedPointer ptr ->
                    match ptr with
                    | ManagedPointerSource.Null -> CliType.ObjectRef None
                    | _ -> failwith "TODO: non-null managed pointer in NativeIntSource coerced to ObjectRef"
            | EvalStackValue.UserDefinedValueType obj ->
                let popped = CliValueType.DereferenceFieldAt 0 NATIVE_INT_SIZE obj

                match popped with
                | CliType.ObjectRef r -> CliType.ObjectRef r
                | _ -> failwith "TODO"
            | EvalStackValue.ManagedPointer _ -> failwith "cannot coerce managed pointer to object reference"
            | _ -> failwith $"TODO: {popped}"
        | CliType.Bool _ ->
            match popped with
            | EvalStackValue.Int32 int32Source ->
                let i = Int32Source.value "storing to a bool location" int32Source
                // Bools are zero-extended
                CliType.Bool (i % 256 |> byte)
            | EvalStackValue.ManagedPointer src ->
                failwith $"unexpectedly tried to convert a managed pointer (%O{src}) into a bool"
            | EvalStackValue.UserDefinedValueType vt -> viewValueTypeAsPrimitive target vt
            | i -> failwith $"TODO: %O{i}"
        | CliType.RuntimePointer _ ->
            match popped with
            | EvalStackValue.ManagedPointer src -> src |> CliRuntimePointer.Managed |> CliType.RuntimePointer
            | EvalStackValue.NativeInt intSrc ->
                match intSrc with
                | NativeIntSource.Verbatim i -> CliType.RuntimePointer (CliRuntimePointer.Verbatim i)
                | NativeIntSource.SyntheticCrossArrayOffset _ ->
                    failwith
                        "refusing to interpret synthetic cross-storage byte offset as a runtime pointer: the value is a deterministic sentinel, not a real address"
                | NativeIntSource.ManagedPointer src -> src |> CliRuntimePointer.Managed |> CliType.RuntimePointer
                | NativeIntSource.FunctionPointer methodInfo ->
                    CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FunctionPointer methodInfo))
                | NativeIntSource.TypeHandlePtr typeHandle ->
                    CliType.RuntimePointer (CliRuntimePointer.TypeHandlePtr typeHandle)
                | NativeIntSource.TypeDescPtr typeHandle ->
                    CliType.RuntimePointer (CliRuntimePointer.TypeDescPtr typeHandle)
                | NativeIntSource.MethodTablePtr typeHandle ->
                    CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr typeHandle)
                | NativeIntSource.MethodTableAuxiliaryDataPtr typeHandle ->
                    CliType.RuntimePointer (CliRuntimePointer.MethodTableAuxiliaryDataPtr typeHandle)
                | NativeIntSource.PerInstInfoPtr handle ->
                    CliType.RuntimePointer (CliRuntimePointer.PerInstInfoPtr handle)
                | NativeIntSource.PerInstDictPtr handle ->
                    CliType.RuntimePointer (CliRuntimePointer.PerInstDictPtr handle)
                | NativeIntSource.FieldHandlePtr ptr ->
                    CliType.RuntimePointer (CliRuntimePointer.FieldRegistryHandle ptr)
                | NativeIntSource.MethodHandlePtr ptr ->
                    CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle ptr)
                | NativeIntSource.GcHandlePtr (addr, tag) ->
                    CliType.RuntimePointer (CliRuntimePointer.GcHandlePtr (addr, tag))
                | NativeIntSource.EventPipeProviderPtr id ->
                    failwith
                        $"refusing to coerce EventPipe provider handle #%d{id} to runtime pointer: tracing handles are opaque, not addresses"
                | NativeIntSource.EventPipeEventPtr id ->
                    failwith
                        $"refusing to coerce EventPipe event handle #%d{id} to runtime pointer: tracing handles are opaque, not addresses"
                | NativeIntSource.LowLevelMonitorPtr id ->
                    failwith
                        $"refusing to coerce low-level monitor handle %O{id} to runtime pointer: monitor handles are opaque, not addresses"
                | NativeIntSource.WaitHandlePtr id ->
                    failwith
                        $"refusing to coerce wait handle %O{id} to runtime pointer: wait handles are opaque, not addresses"
                | NativeIntSource.AssemblyHandle _ -> failwith "todo: AssemblyHandle into CliType.RuntimePointer"
                | NativeIntSource.ModuleHandle _ -> failwith "todo: ModuleHandle into CliType.RuntimePointer"
                | NativeIntSource.MetadataImportHandle _ ->
                    failwith "refusing to coerce metadata import handle to runtime pointer"
                | NativeIntSource.OpaqueHashBits bits ->
                    failwith
                        $"refusing to coerce synthesised pointer-hash bits 0x%x{bits} to runtime pointer (would forge a dereferenceable address)"
            | EvalStackValue.NullObjectRef -> failwith "cannot coerce null object reference to runtime pointer"
            | EvalStackValue.ObjectRef addr -> failwith $"cannot coerce object reference %O{addr} to runtime pointer"
            | _ -> failwith $"TODO: %O{popped}"
        | CliType.Char _ ->
            match popped with
            | EvalStackValue.Int32 int32Source ->
                let i = Int32Source.value "storing to a char location" int32Source
                // Char is a 16-bit unsigned slot. The int32 on the stack may
                // carry a sign-extended negative value (e.g. from coercing a
                // negative Int16 through a `Unsafe.As<ushort, short>` write);
                // narrow via `uint16` so the reinterpret preserves the low
                // 16 bits bit-for-bit instead of splitting signed/ arithmetic
                // into the wrong high byte.
                let truncated = uint16<int> i
                let high = byte<uint16> (truncated >>> 8)
                let low = byte<uint16> (truncated &&& 0xFFus)
                CliType.Char (high, low)
            | EvalStackValue.UserDefinedValueType vt -> viewValueTypeAsPrimitive target vt
            | popped -> failwith $"Unexpectedly wanted a char from {popped}"
        | CliType.ValueType vt ->
            match popped with
            | EvalStackValue.UserDefinedValueType popped' ->
                let coerceContents (targetContents : CliType) (sourceContents : CliType) : CliType =
                    toCliTypeCoerced targetContents (ofCliType sourceContents)

                CliValueType.CoerceFrom coerceContents vt popped' |> CliType.ValueType
            | popped ->
                // A bare primitive popped into a ValueType slot is only legal for primitive-like
                // wrappers: the BCL handles (IntPtr, RuntimeTypeHandle, ...) flattened on push,
                // and enums, where CIL freely coerces between the underlying integer on the stack
                // and the enum slot. Both cases share the same rewrap: clone the target's single-
                // field skeleton and store the coerced primitive into `value__`/`_value`. A
                // single-field user-defined struct receiving a bare primitive is invalid IL; fail
                // loud so the misfire surfaces instead of silently degrading the storage shape.
                if vt.PrimitiveLikeKind.IsSome then
                    let field = CliValueType.PrimitiveLikeField vt
                    let newContents = toCliTypeCoerced field.Contents popped

                    let newField =
                        { field with
                            Contents = newContents
                        }

                    [ newField ] |> CliValueType.OfFieldsLike vt vt.Layout |> CliType.ValueType
                else
                    failwith $"TODO: {popped} into value type {target}"

    /// A value type popped into a primitive slot.
    ///
    /// Opcodes that name a width (`ldind.<width>`, `ldelem.<width>`, ...) ask for a *view* of
    /// storage at that width, and a byref to a boxed primitive addresses a value type: `box` of a
    /// bare primitive stores it inside the boxed type's own single instance field
    /// (`System.Int64::m_value`, `System.Boolean::m_value`, ...), so the `this` byref the runtime
    /// synthesises for a virtual call on a boxed receiver points at that wrapper rather than at
    /// the primitive inside it. Every primitive's instance methods open with
    /// `ldarg.0; ldind.<width>`, so this is the shape `((object) 1L).ToString()` reaches.
    ///
    /// The requested view is the field covering the leading `SizeOf target` bytes, and the
    /// *target* governs the result's shape: these opcodes reinterpret memory, they do not convert
    /// values, so an int32 cell read at `ldind.r4` yields the float those bits spell and a
    /// native-int-backed cell read at `ldind.i8` lands in the int64 slot rather than staying a
    /// native int. Storage whose flavour already matches the target is handed back as-is, because
    /// only the cell carries the provenance the byte image cannot (managed pointers, handle-valued
    /// native ints, widened native ints); everything else goes through the bytes, which refuse
    /// rather than forge when the value has provenance to lose.
    ///
    /// Primitive-like wrappers (IntPtr, RuntimeTypeHandle, enums, ...) never arrive here as
    /// `popped`: `ofCliType` flattens them on push, and `EvalStack.Push'` enforces that invariant.
    /// They can still appear *inside* the storage — a `nint` field is stored as the `System.IntPtr`
    /// wrapper — so nested value types are stepped through rather than flattened to bytes, keeping
    /// the innermost cell's provenance available to the shape test.
    and private viewValueTypeAsPrimitive (target : CliType) (popped : CliValueType) : CliType =
        viewValueTypeAsPrimitiveWithVisited target popped Set.empty

    /// `visited` carries the declared types already stepped through. The CLI forbids a value type
    /// from containing itself, so a repeat means malformed metadata; crash on it rather than
    /// unwrap forever. Mirrors `CliType.zeroOfWithVisited`, which guards the same shape of walk.
    and private viewValueTypeAsPrimitiveWithVisited
        (target : CliType)
        (popped : CliValueType)
        (visited : Set<ConcreteTypeHandle>)
        : CliType
        =
        let size = (CliType.SizeOf target).Size

        if Set.contains popped.Declared visited then
            failwith
                $"refusing to view %O{popped.Declared} as a %d{size}-byte value: its storage nests through itself, so unwrapping would not terminate"
        else

        let visited = Set.add popped.Declared visited

        match CliValueType.DereferenceFieldAt 0 size popped with
        | CliType.ValueType inner -> viewValueTypeAsPrimitiveWithVisited target inner visited
        | contents when CliType.ZeroLike contents = CliType.ZeroLike target ->
            // Already the requested shape, so this cell *is* the view; returning it directly is
            // what keeps provenance alive across the projection.
            contents
        | CliType.ObjectRef _
        | CliType.RuntimePointer _ ->
            failwith
                $"refusing to view the leading %d{size} bytes of %O{popped.Declared} as %O{target}: the storage there is a reference, and reinterpreting it would forge address bits"
        | contents -> CliType.OfBytesLike target (CliType.ToBytes contents)

type EvalStack =
    {
        Values : EvalStackValue list
    }

    static member Empty : EvalStack =
        {
            Values = []
        }

    static member Pop (stack : EvalStack) : EvalStackValue * EvalStack =
        match stack.Values with
        | [] -> failwith "eval stack was empty on pop instruction"
        | v :: rest ->
            let stack =
                {
                    Values = rest
                }

            v, stack

    static member Peek (stack : EvalStack) : EvalStackValue option = stack.Values |> List.tryHead

    static member Push' (v : EvalStackValue) (stack : EvalStack) : EvalStack =
        // Invariant: primitive-like wrapper structs (IntPtr, RuntimeTypeHandle, enums, ...) must
        // never appear on the eval stack as UserDefinedValueType; EvalStackValue.ofCliType flattens
        // them on push. A caller using Push' directly must respect this too.
        match v with
        | EvalStackValue.UserDefinedValueType vt when vt.PrimitiveLikeKind.IsSome ->
            failwith
                $"eval-stack invariant violated: primitive-like struct %O{vt.Declared} pushed as UserDefinedValueType (kind = %O{vt.PrimitiveLikeKind})"
        | _ -> ()

        {
            Values = v :: stack.Values
        }

    static member Push (v : CliType) (stack : EvalStack) : EvalStack =
        let v = EvalStackValue.ofCliType v

        EvalStack.Push' v stack

    static member PeekNthFromTop (n : int) (stack : EvalStack) : EvalStackValue option = stack.Values |> List.tryItem n
